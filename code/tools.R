library(tidyverse)
library(ipumsr)
library(sqldf)
library(RSQLite)

source("code/oes.R")

# Read IPUMS CPS data and write to SQLite
write_ipums_to_sql <- function() {
  # READ IPUMS DATA ---------------------------------------------------------
  
  cps_ddi_file <- "data/CPS/cps_00005.xml"
  cps_data_file <- "data/CPS/cps_00005.dat"
  
  cps_ddi <- read_ipums_ddi(cps_ddi_file)
  cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)
  
  
  # SQLite ------------------------------------------------------------------
  
  # connection
  db <- dbConnect(SQLite(), dbname="data_out/cps_data.sqlite")
  
  # export
  dbWriteTable(db, "cps", cps_data, overwrite = TRUE)
  
  # disconnect
  dbDisconnect(db)
}

# Read CPS March data and construct the main dataset
load_df <- function() {
  # CPS March from IPUMS
  cps_data <- sqldf("select * from cps", dbname = "data_out/cps_data.sqlite")
  
  # GDP Deflators from https://fred.stlouisfed.org/series/GDPDEF
  gdp_deflator <- read.csv("data/GDPDEF.csv", stringsAsFactors = F) %>%
    separate(DATE, c("y", "m", "d")) %>%
    filter(m == "01") %>%
    column_to_rownames('y')
  
  # STEM, STEM-related
  occ10 <- read.csv("data/SOC/occ10.csv", stringsAsFactors = F) %>%
    mutate(is_stem = ifelse(stem %in% 2, 1, 0),
           is_stem_related = ifelse(stem %in% c(1, 2), 1, 0)
    )
  
  df <- cps_data %>%
    # limit to full-time workers with non-zero salary income
    filter(
      FULLPART == 1, INCWAGE != 0
      ) %>%
    
    # weight
    group_by(YEAR) %>%
    mutate(weight = ASECWT / sum(ASECWT)) %>%
    ungroup() %>%
    
    # wage, lwage: inflation-adjusted
    mutate(
      wage = INCWAGE / WKSWORK1 / UHRSWORKLY * 100 / gdp_deflator[as.character(YEAR), "GDPDEF"],
      lwage = log(wage)
      ) %>%
    
    # schooling
    mutate(
      schooling = recode(unclass(EDUC),
                         `2` = 0,
                         `10` = 2,
                         `20` = 5.5,
                         `30` = 7.5,
                         `40` = 9,
                         `50` = 10,
                         `60` = 11,
                         `71` = 11.5,
                         `73` = 12,
                         `81` = 13,
                         `91` = 14,
                         `92` = 14,
                         `111` = 16,
                         `123` = 18,
                         `124` = 18,
                         `125` = 20
                         ),
      experience = ifelse(AGE - schooling - 6 >= 0, AGE - schooling - 6, 0),
      experience_sq = experience^2,
      RACE = recode(unclass(RACE),
                    `100` = "White",
                    `200` = "Black",
                    `300` = "American Indian",
                    `651` = "Asian",
                    `652` = "Asian",
                    .default = "Other"
                    ),
      married = ifelse(MARST == 1, 1, 0),
      METAREA = factor(METAREA)
      ) %>%
    
    # occupations: stem, stem-related
    left_join(occ10 %>% select(OCC10LY, is_stem, is_stem_related), by = "OCC10LY")

  return(df)
}

# create mapping from INDLY to OES industry codes,
# and calculate economy average and sd using the given set of occupations
prepare_INDLY_conversion_census_to_naics <- function(years, tech_occupations, df_oes = NULL) {
  # fetch empl, tech from oes using the census_code
  fetch_oes_empl_tech <- function(census_code, df) {
    # census_code: number
    # df: OES data, already filter by year
    return(as.matrix(
      df %>%
        filter(startsWith(NAICS, census_code)) %>%
        summarise(
          empl = sum(empl),
          tech = sum(tech)
          ) %>%
        select(empl, tech)
    ))
  }
  
  # construct df_oes only once
  if (is.null(df_oes)) {
    df_oes <- CompileOES(years, tech_occupations)
  }
  
  if (length(years) > 1) {
    # Multiple Years
    df <- prepare_INDLY_conversion_census_to_naics(years[1], tech_occupations, df_oes)
    
    for (year in tail(years, -1)) {
      df <- rbind(df, prepare_INDLY_conversion_census_to_naics(year, tech_occupations, df_oes))
    }
    
    return(df)
  } else {
    # Single year
    year <- years
    
    # Convert census industry code to sic/naics as in OES
    if (year <= 2001) {
      # TODO: create the file
      filename <- "data/NAICS/1990_census_to_sic.csv"
    } else if (year == 2002) {
      filename <- "data/NAICS/1990_census_to_2002_naics.csv"
    } else if (year <= 2007) {
      filename <- "data/NAICS/2002_census_to_2002_naics.csv"
    } else if (year == 2008) {
      filename <- "data/NAICS/2002_census_to_2007_naics.csv"
    } else if (year <= 2011) {
      filename <- "data/NAICS/2007_census_to_2007_naics.csv"
    } else if (year <= 2013) {
      filename <- "data/NAICS/2007_census_to_2012_naics.csv"
    } else if (year <= 2016) {
      filename <- "data/NAICS/2012_census_to_2012_naics.csv"
    } else {
      # TODO: create the file
      filename <- "data/NAICS/2012_census_to_2017_naics.csv"
    }
    
    # long format, remove duplicates
    df_conversion <- read.csv(filename, stringsAsFactors = F) %>%
      setNames(c("INDLY", "naics")) %>%
      separate(naics, paste0("naics", 1:15), ", ") %>%
      mutate_at(vars(starts_with("naics")), funs(substr(., 1, 4))) %>%
      gather("var", "naics", 2:16) %>%
      filter(!is.na(naics)) %>%
      group_by(INDLY, naics) %>%
      summarise(var = first(var)) %>%
      ungroup()
    
    # tech.pct.economy, tech.pct.sd
    this_year_tech_pct_economy <- as.numeric(
      df_oes %>% 
        filter(year == !!year) %>%
        summarise(tech.pct.economy = first(tech.pct.economy))
    )
    
    this_year_tech_pct_sd <- as.numeric(
      df_oes %>% 
        filter(year == !!year) %>%
        summarise(tech.pct.sd = first(tech.pct.sd))
    ) 
    
    df_conversion <- df_conversion %>%
      # link to oes
      cbind(
        as.data.frame(
          t(sapply(as.character(df_conversion$naics),
                   fetch_oes_empl_tech,
                   df_oes %>% filter(year == !!year),
                   USE.NAMES = F
                   )
            )
          ) %>%
          setNames(c("empl", "tech"))
        ) %>%
      
      # compute fractions
      group_by(INDLY) %>%
      summarise(empl = sum(empl),
                tech = sum(tech)) %>%
      filter(empl > 0) %>%
      mutate(tech.pct = tech / empl) %>%
      
      # create columns for year-specific values
      mutate(
        year = !!year,
        tech.pct.economy = this_year_tech_pct_economy,
        tech.pct.sd = this_year_tech_pct_sd
        ) %>%
      
      # reorder columns
      select(year, everything())
    
    return(df_conversion)
  }
}  

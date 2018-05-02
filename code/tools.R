library(tidyverse)
library(ipumsr)
library(sqldf)
library(RSQLite)

source("code/oes.R")

# logistic function
logistic <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

weighted_colMeans <- function(df, w) {
  sapply(1:ncol(df),
         function(i) weighted.mean(df[, i], w = w))
}

# Read IPUMS CPS data and write to SQLite
write_ipums_to_sql <- function(cps_data_file, cps_ddi_file, out, tablename = "cps", append = F) {
  # READ IPUMS DATA ---------------------------------------------------------
  
  cps_ddi <- read_ipums_ddi(cps_ddi_file)
  cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)
  
  
  # SQLite ------------------------------------------------------------------
  
  # connection
  db <- dbConnect(SQLite(), dbname = out)
  
  # export
  dbWriteTable(db, tablename, cps_data, overwrite = !append, append = append)
  
  # disconnect
  dbDisconnect(db)
}

# Read CPS March data and construct the main dataset
load_df_aces <- function(query = NULL) {
  # query
  if (is.null(query)) {
    query <- ""
  } else {
    query <- paste0(" where ", query)
  }

  # Read from SQLite
  cps_data <- sqldf(paste0("select * from cps", query), dbname = paste0("data_out/cps_aces.sqlite"))

  # GDP Deflators from https://fred.stlouisfed.org/series/DPCERD3Q086SBEA
  gdp_deflator <- read.csv("data/GDPDEF.csv", stringsAsFactors = F) %>%
    separate(DATE, c("y", "m", "d")) %>%
    filter(m == "01") %>%
    column_to_rownames('y')

  # STEM, STEM-related
  occ10_tech <- read.csv("data/SOC/occ10_tech.csv", stringsAsFactors = F, fileEncoding = "UTF-8") %>%
    rename(OCC10LY = OCC2010)

  # federal minimum wage from https://www.dol.gov/whd/minwage/chart.htm
  df_minwage <- read.csv('data/min_wage.csv', stringsAsFactors = F, fileEncoding = "UTF-8")

  df <- cps_data %>%
    filter(
      # keep age between 16 and 65
      AGE %in% 16:65,
      
      # drop if wage income is 0, missing (9999998), or NIU (9999999)
      INCWAGE != 0, INCWAGE != 9999999, INCWAGE != 9999998,

      # keep private workers
      CLASSWLY == 22,
      
      # drop if wages were allocated
      QOINCWAGE == 0, (SRCEARN != 1 | QINCLONG == 0)
      ) %>%

    # minimum wage: drop if lower than minimum wage
    left_join(df_minwage, by = "YEAR") %>%
    
    mutate(
      # nominal wage: yearly wage and salary in come divided by number of hours worked
      wage_nominal = INCWAGE / WKSWORK1 / UHRSWORKLY,

      # wage, lwage: inflation-adjusted
      gdp_def = gdp_deflator[as.character(YEAR), "GDPDEF"],
      wage = wage_nominal * 100 / gdp_def,
      lwage = log(wage)
      ) %>%
    
    # weight: cps weight multiplied by number of hours worked
    mutate(weight = ASECWT * WKSWORK1 * UHRSWORKLY / 52) %>%

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
      
      # 5 education groups: below_hs, hs, college_some, college, graduate
      schooling_group = ifelse(schooling < 12, "below_hs",
                               ifelse(schooling == 12, "hs",
                                      ifelse(schooling < 16, "college_some", # "college"))),
                                             ifelse(schooling == 16, "college", "graduate")))),
      college = ifelse(schooling > 12, 1, 0),
      # college = factor(college, labels("HS", "College")),
      
      # experience, quartic
      experience = ifelse(AGE - schooling - 5 >= 0, AGE - schooling - 5, 0),
      experience_2 = experience^2,
      experience_3 = experience^3,
      experience_4 = experience^4,
      
      # 4 experience groups: 0-9, 10-19, 20-29, 30+
      experience_group = ifelse(experience < 10, "experience9",
                                ifelse(experience < 20, "experience19",
                                       ifelse(experience < 30, "experience29", "experience30"))),
      
      # 20 education-experience groups
      educ_exp_group = paste(schooling_group, experience_group, sep = "_"),
      educ_exp_group = factor(educ_exp_group),
      
      # Race
      RACE = recode(unclass(RACE),
                    `100` = "White",
                    `200` = "Black",
                    .default = "Other"
                    ),
      RACE = factor(RACE),
      
      # marital status
      married = ifelse(MARST == 1, 1, 0),
      
      # metropolitan area
      METAREA = factor(METAREA)
      ) %>%
    
    # additional restriction: wage between $1 and $350 (in 2009 dollar)
    filter(wage >= 1, wage <= 350) %>%
    
    # occupations: stem, stem_related, high_tech, tech_group
    left_join(occ10_tech %>% 
                dplyr::select(OCC10LY, stem, stem_related, high_tech, tech_group),
              by = "OCC10LY") %>%
    mutate_at(vars(stem, stem_related, high_tech, tech_group),
              funs(replace(., is.na(.), 0))
    )

  return(df)
}

# # Read CPS ORG data and construct the main dataset
# load_df_org <- function(filename, query = NULL) {
#   # query
#   query_default <- " where EARNWT != 0"
#   if (is.null(query)) {
#     query <- query_default
#   } else {
#     query <- paste0(query_default, " and ", query)
#   }
#   
#   cps_data <- sqldf(paste0("select * from cps", query), dbname = paste0("data_out/", filename, ".sqlite"))
#   
#   # GDP Deflators from https://fred.stlouisfed.org/series/GDPDEF
#   gdp_deflator <- read.csv("data/GDPDEF.csv", stringsAsFactors = F) %>%
#     separate(DATE, c("y", "m", "d")) %>%
#     filter(m == "01") %>%
#     column_to_rownames("y")
#   
#   # STEM, STEM-related
#   occ10_tech <- read.csv("data/SOC/occ10_tech.csv", stringsAsFactors = F, fileEncoding = "UTF-8")
#   
#   # minimum wage
#   df_minwage <- read.csv('data/min_wage.csv', stringsAsFactors = F, fileEncoding = "UTF-8")
#   
#   df <- cps_data %>%
#     # limit to full-time workers with non-zero salary income
#     filter(
#       AGE %in% 16:65,
#       HOURWAGE != 99.99, HOURWAGE > 0, EARNWT != 0, UHRSWORKORG != 999,
#       
#       # keep private workers
#       CLASSWKR == 22
#     ) %>%
#     
#     # minimum wage: drop if lower than minimum wage
#     left_join(df_minwage, by = "YEAR") %>%
#     
#     # weight
#     mutate(weight = EARNWT * UHRSWORKORG) %>%
#     group_by(YEAR) %>%
#     mutate(weight = weight / sum(weight)) %>%
#     ungroup() %>%
#     
#     # wage, lwage: inflation-adjusted
#     mutate(
#       wage = HOURWAGE * 100 / gdp_deflator[as.character(YEAR), "GDPDEF"],
#       lwage = log(wage)
#     ) %>%
#     
#     # schooling
#     mutate(
#       schooling = recode(unclass(EDUC),
#                          `2` = 0,
#                          `10` = 2,
#                          `20` = 5.5,
#                          `30` = 7.5,
#                          `40` = 9,
#                          `50` = 10,
#                          `60` = 11,
#                          `71` = 11.5,
#                          `73` = 12,
#                          `81` = 13,
#                          `91` = 14,
#                          `92` = 14,
#                          `111` = 16,
#                          `123` = 18,
#                          `124` = 18,
#                          `125` = 20
#       ),
#       experience = ifelse(AGE - schooling - 5 >= 0, AGE - schooling - 5, 0),
#       experience_2 = experience^2,
#       experience_3 = experience^3,
#       experience_4 = experience^4,
#       RACE = recode(unclass(RACE),
#                     `100` = "White",
#                     `200` = "Black",
#                     .default = "Other"
#       ),
#       RACE = factor(RACE),
#       married = ifelse(MARST == 1, 1, 0),
#       METAREA = factor(METAREA),
#       union_covered = UNION %in% c(2, 3)
#     ) %>%
#     
#     # occupations: stem, stem-related
#     left_join(occ10_tech %>% dplyr::select(OCC2010, stem, stem_related), by = "OCC2010")
#   
#   return(df)
# }

# create mapping from INDLY to OES industry codes,
# and calculate economy average and sd using the given set of occupations
prepare_INDLY_conversion_census_to_naics <- function(years, tech_occupations, df_oes = NULL, cached = F, save = F) {
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
        dplyr::select(empl, tech)
    ))
  }
  
  # use cached?
  if (cached) {
    return(read.csv("data_out/INDLY_to_naics.csv", stringsAsFactors = F, fileEncoding = "UTF-8"))  
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
  
    # save?
    if (save) {
      write.csv(df, "data_out/INDLY_to_naics.csv", fileEncoding = "UTF-8")
    }
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
    
    this_year_tech_pct_median <- as.numeric(
      df_oes %>% 
        filter(year == !!year) %>%
        summarise(tech.pct.median = first(tech.pct.median))
    ) 
    
    df <- df_conversion %>%
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
        YEAR = !!year,
        tech.pct.economy = this_year_tech_pct_economy,
        tech.pct.sd = this_year_tech_pct_sd,
        tech.pct.median = this_year_tech_pct_median
        ) %>%
      
      # reorder columns
      dplyr::select(YEAR, everything())
  }
  
  return(df)
}  

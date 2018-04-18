library(tidyverse)
library(ipumsr)
library(sqldf)
library(RSQLite)

# Read IPUMS CPS data and write to SQLite
write_ipums_to_sql <- function() {
  # READ IPUMS DATA ---------------------------------------------------------
  
  cps_ddi_file <- "data/CPS/cps_00004.xml"
  cps_data_file <- "data/CPS/cps_00004.dat"
  
  cps_ddi <- read_ipums_ddi(cps_ddi_file)
  cps_data <- read_ipums_micro(cps_ddi_file, data_file = cps_data_file)
  
  
  # SQLite ------------------------------------------------------------------
  
  # connection
  db <- dbConnect(SQLite(), dbname="data_out/cps_data.sqlite")
  
  # export
  dbWriteTable(db, "cps", cps_data)
  
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
    mutate(is_stem = ifelse(stem %in% 2, TRUE, FALSE),
           is_stem_related = ifelse(stem %in% c(1, 2), TRUE, FALSE)
    )
  
  df <- cps_data %>%
    # limit to full-time workers with non-zero salary income
    filter(FULLPART == 1, INCWAGE != 0) %>%
    
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
                    )
      ) %>%
    
    # occupations: stem, stem-related
    left_join(occ10 %>% select(OCC10LY, is_stem, is_stem_related), by = "OCC10LY")

  return(df)
}

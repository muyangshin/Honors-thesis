library(tidyverse)
library(sqldf)


# LOAD DATA ---------------------------------------------------------------

cps_data <- sqldf("select * from cps", dbname = "data_out/cps_data.sqlite")

df <- cps_data %>%
  # limit to full-time workers with non-zero salary income
  filter(FULLPART == 1, INCWAGE != 0) %>%
  
  # weight
  group_by(YEAR) %>%
  mutate(weight = ASECWT / sum(ASECWT)) %>%
  ungroup() %>%
  
  # wage, lwage
  mutate(wage = INCWAGE / WKSWORK1 / UHRSWORKLY,
         lwage = log(wage)) %>%
  
  # schooling
  mutate(schooling = recode(unclass(EDUC),
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
         experience = ifelse(AGE - schooling >= 0, AGE - schooling, 0),
         RACE = recode(unclass(RACE),
                       `100` = "White",
                       `200` = "Black",
                       `300` = "American Indian",
                       `651` = "Asian",
                       `652` = "Asian",
                       .default = "Other"
                       )
         )



# PROCESS DATA ------------------------------------------------------------

# STEM, STEM-related
occ10 <- read.csv("data/SOC/occ10.csv", stringsAsFactors = F) %>%
  mutate(is_stem = ifelse(stem %in% 2, TRUE, FALSE),
         is_stem_related = ifelse(stem %in% c(1, 2), TRUE, FALSE)
  )

df <- df %>%
  left_join(occ10 %>% select(OCC10LY, is_stem, is_stem_related), by = "OCC10LY")


# ANALYSIS ----------------------------------------------------------------
# TODO: weight

df2 <- df %>% 
  spread(RACE, RACE) %>%
  select(YEAR, schooling, experience, SEX, "American Indian", Asian, Black, Other, White, is_stem_related) %>%
  mutate_at(c("American Indian", "Asian", "Black", "Other", "White"),
            funs(ifelse(is.na(.), FALSE, TRUE)))

df_results <- data.frame()
for (year in 2002:2017) {
  # regression on age (or working experience), education, race, gender + tech
  model <- lm(lwage ~ schooling + experience + SEX + RACE + is_stem_related - 1,
              weights = ASECWT, data = df %>% filter(YEAR == year))
  
  df2_sum <- df2 %>%
    filter(YEAR == year) %>%
    summarize_all(mean)
  
  df_results <- df_results %>%
    bind_rows(df2_sum * c(1, coef(model)))
}

rownames(df_results) <- df_results$YEAR
df_results <- df_results %>% select(-YEAR) 

df_results2 <- df_results %>%
  bind_cols(df %>% group_by(YEAR) %>% summarise(lwage = mean(lwage))) %>%
  select(-YEAR) %>%
  mutate_all(funs(. / lwage))

  

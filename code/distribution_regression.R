library(tidyverse)
library(Hmisc)  # weighted

source("code/tools.R")


# LOAD DATA ---------------------------------------------------------------

# df_aces <- load_df_aces()
# df_aces_0 <- load_df()
# df_org_2016 <- load_df_org("cps_org", "YEAR=2016")
# df_org_1994 <- load_df_org("cps_org_94")
# df_org_2002 <- load_df_org("cps_org", "YEAR=2002")

# df_org <- load_df_org("cps_org")

# start year, end year
years <- c(1992, 2016)

# IND to naics
df_IND_naics <- prepare_INDLY_conversion_census_to_naics(years, occs_bls_stem, cached = T)
  
df <- df_aces %>%
  mutate(cons = 1) %>%
  
  # restrict to men for now
  filter(SEX == 1)

# create views for period 1 and 2
df_t1 <- function() {
  return(df %>% filter(YEAR == years[1]))
}

df_t2 <- function() {
  return(df %>% filter(YEAR == years[2]))
}

# list of METAREAs which appears only once
METAREAs_appearing_once <- df %>% 
  dplyr::select(YEAR, METAREA) %>% 
  group_by(METAREA) %>% 
  summarise(n = n_distinct(YEAR)) %>%
  filter(n == 1) %>%
  pull(METAREA)

df <- df %>%
  # if METAREA appears only once, code it as 10000
  mutate(METAREA = ifelse(METAREA %in% get("METAREAs_appearing_once"), 10000, METAREA))  %>%
  
  # reset factor levels
  mutate(METAREA = factor(METAREA)) %>%
  
  # tech using industry sector
  left_join(df_IND_naics, by = c("YEAR", "INDLY")) %>%
  filter(!is.na(tech.pct)) %>%
  
  filter(
    # censor those below minimum wage
    INCWAGE / WKSWORK1 / UHRSWORKLY >= min_wage
    )
  
# recalculate weight
df <- df %>%
  group_by(YEAR) %>%
  mutate(weight = weight / sum(weight)) %>%
  ungroup()


# ANALYSIS ----------------------------------------------------------------

# set thresholds for lwage
lwage_thresholds <- wtd.quantile(df_t2()$lwage, df_t2()$weight, seq(0, 1, by = 0.001), normwt = T)


# tech only ---------------------------------------------------------------

tech <- "high_tech"

# construct empty data frame to save regression coefficients
df_reg_tech_only <- data.frame(matrix(ncol = 4, 
                            nrow = length(lwage_thresholds))) %>%
  setNames(c("year", "lwage", "cons", tech))

# distribution regression: using time 2
for (j in seq_along(lwage_thresholds)) {
  threshold <- lwage_thresholds[j]
  
  y <- df_t2()$lwage <= threshold
  reg_formula <- paste0("y ~ ", tech)
  
  # # linear
  # reg <- lm(reg_formula, data = df_t2(), weights = weight)
  
  # logistic
  reg <- glm(reg_formula, data = df_t2(), family = "binomial", weights = weight)
  
  df_reg_tech_only[j, ] <- c(years[2], threshold, reg$coefficients)
}

# empty data frame to store cdfs
df_cdf_tech_only <- data.frame(
  year = numeric(length(lwage_thresholds)),
  lwage = numeric(length(lwage_thresholds)),
  cdf = numeric(length(lwage_thresholds)),
  cdf_tech = numeric(length(lwage_thresholds))
)

# year
df_cdf_tech_only$year <- years[2]

# lwage
df_cdf_tech_only$lwage <- lwage_thresholds

# cdf of period 1 and 2
df_cdf_tech_only$cdf <- seq(0, 1, by = 0.001)

# covariate distributions
# cdf_tech
covariates <- as.matrix(df_t1() %>% dplyr::select(cons, !!tech))
df_cdf_tech_only$cdf_tech <- colMeans(logistic(covariates %*% t(as.matrix(df_reg_tech_only[, c("cons", tech)]))))

if (is.nan(df_cdf_tech_only$cdf_tech[length(df_cdf_tech_only$cdf_tech)])) {
  df_cdf_tech_only$cdf_Tech[length(df_cdf_tech_only$cdf_tech)] <- 1
}

# construct variance
df_cdf_tech_only <- df_cdf_tech_only %>%
  mutate(
    lwage_mid = lwage - (lwage - lag(lwage)) / 2,
    d_cdf = cdf - lag(cdf),
    d_cdf_tech = cdf_tech - lag(cdf_tech)
  ) %>%
  filter(!is.na(d_cdf))

avg_empr <- sum(df_cdf_tech_only$lwage * df_cdf_tech_only$d_cdf) / sum(df_cdf_tech_only$d_cdf)
sd_empr <- sqrt(sum(df_cdf_tech_only$lwage^2 * df_cdf_tech_only$d_cdf) / sum(df_cdf_tech_only$d_cdf) - avg_empr^2)

avg_tech <- sum(df_cdf_tech_only$lwage * df_cdf_tech_only$d_cdf_tech) / sum(df_cdf_tech_only$d_cdf_tech)
sd_tech <- sqrt(sum(df_cdf_tech_only$lwage^2 * df_cdf_tech_only$d_cdf_tech) / sum(df_cdf_tech_only$d_cdf_tech) - avg_tech^2)

lwage_t1_mean <- wtd.mean(df_t1()$lwage, df_t1()$weight)
lwage_t1_sd <- sqrt(wtd.var(df_t1()$lwage, df_t1()$weight, normwt = T))

lwage_t2_mean <- wtd.mean(df_t2()$lwage, df_t2()$weight)
lwage_t2_sd <- sqrt(wtd.var(df_t2()$lwage, df_t2()$weight, normwt = T))


# results data frame
df_decompose_tech_only <- data.frame(matrix(ncol = 4, nrow = 2))
colnames(df_decompose_tech_only) <- c("name", "total", "tech", "unexplained")

df_decompose_tech_only[1, ] <- list("Std Dev", 
                                    lwage_t2_sd - lwage_t1_sd,
                                    lwage_t2_sd - sd_tech,
                                    sd_tech - lwage_t1_sd
)

df_decompose_tech_only[2, ] <- list("pct", 
                                    100,
                                    100 * (lwage_t2_sd - sd_tech) / (lwage_t2_sd - lwage_t1_sd),
                                    100 * (sd_tech - lwage_t1_sd) / (lwage_t2_sd - lwage_t1_sd)
)






# tech + other covariates -------------------------------------------------

# regression specification
tech <- "high_tech"
xs <- c("schooling", "experience", "experience_2", "experience_3", "experience_4", "married", "RACE")

# fill na with 0
# df[, tech] <- as.vector(ifelse(is.na(df[, tech]), 0, 1))

xs_full <- c()  # vector of control variable names, including dummy variables
for (x in xs) {
  x_vector <- df[[x]]
  if (!is.factor(x_vector)) {
    # non-categorical
    xs_full <- c(xs_full, x)
  } else {
    # categorical
    x_new_vectors <- model.matrix(as.formula(paste0("~", x)), df)
    x_new_vectors <- x_new_vectors[, -1]  # exclude constant
    
    xs_full <- c(xs_full, colnames(x_new_vectors))
    df <- cbind(df, x_new_vectors)
    rm(x_new_vectors)
  }
}


# Counterfactual Y1 given X1 ----------------------------------------------

# construct empty data frame to save regression coefficients
df_reg <- data.frame(matrix(ncol = 4 + length(xs_full), 
                            nrow = length(lwage_thresholds))) %>%
  setNames(c("year", "lwage", "cons", tech, xs_full))

# distribution regression: using time 2
for (j in seq_along(lwage_thresholds)) {
  threshold <- lwage_thresholds[j]
  
  y <- df_t2()$lwage <= threshold
  reg_formula <- paste0("y ~ ", paste0(c(tech, xs_full), collapse = " + "))
  
  # # linear
  # reg <- lm(reg_formula, data = df_t2(), weights = weight)
  
  # logistic
  reg <- glm(reg_formula, data = df_t2(), family = "binomial", weights = weight)
  
  df_reg[j, ] <- c(years[2], threshold, reg$coefficients)
}


# conditional distribution u0 given c0 ------------------------------------

reg_formula <- paste0(tech, " ~ ", paste0(xs_full, collapse = " + "))

reg_u0_c0 <- glm(reg_formula, 
                 data = df_t1(), 
                 family = "binomial",
                 weights = weight)


# Construct distributions ---------------------------------------

# empty data frame to store cdfs
df_cdf <- data.frame(
  year = numeric(length(lwage_thresholds)),
  lwage = numeric(length(lwage_thresholds)),
  cdf = numeric(length(lwage_thresholds)),
  cdf_tech = numeric(length(lwage_thresholds)),
  cdf_cont = numeric(length(lwage_thresholds))
)

# year
df_cdf$year <- years[2]

# lwage
df_cdf$lwage <- lwage_thresholds

# cdf of period 1 and 2
df_cdf$cdf <- seq(0, 1, by = 0.001)


# covariate distributions
# logistic
# cdf_tech
# Pr_0(tech | c_1)
prob_tech <- logistic(
  as.matrix(df_t2() %>% dplyr::select(cons, !!xs_full)) %*% reg_u0_c0$coefficients
  )

prob_tech_sum <- sum(prob_tech)

covariates <- as.matrix(df_t2() %>% dplyr::select(cons, !!tech, !!xs_full))

temp <- logistic(covariates %*% t(as.matrix(df_reg[, c("cons", tech, xs_full)])))

df_cdf$cdf_tech <- sapply(1:nrow(df_reg),
                          function(i) weighted.mean(temp[, i], w = prob_tech / prob_tech_sum))

if (is.nan(df_cdf$cdf_tech[length(df_cdf$cdf_tech)])) {
  df_cdf$cdf_tech[length(df_cdf$cdf_tech)] <- 1
}

# cdf_cont
covariates <- as.matrix(df_t1() %>% dplyr::select(cons, !!tech, !!xs_full))
df_cdf$cdf_cont <- colMeans(logistic(covariates %*% t(as.matrix(df_reg[, c("cons", tech, xs_full)]))))

if (is.nan(df_cdf$cdf_cont[length(df_cdf$cdf_cont)])) {
  df_cdf$cdf_cont[length(df_cdf$cdf_cont)] <- 1
}

# # linear
# # cdf_tech
# # Pr_0(tech | c_1)
# covariates_weighted_mean <- sapply(1:ncol(covariates),
#                                    function (i) weighted.mean(covariates[, i], w = prob_tech / prob_tech_sum))
# 
# df_cdf$cdf_tech <- as.vector(
#   as.matrix(df_reg[, c("cons", tech, xs_full)]) %*% covariates_weighted_mean
#   )

# # cdf_cont
# df_cdf$cdf_cont <- as.vector(
#   as.matrix(df_reg[, c("cons", tech, xs_full)]) %*% colMeans(df_t1() %>% dplyr::select(cons, !!tech, !!xs_full))
# )


# calculate variance ------------------------------------------------------

df_cdf <- df_cdf %>%
  mutate(
    lwage_mid = lwage - (lwage - lag(lwage)) / 2,
    d_cdf = cdf - lag(cdf),
    d_cdf_tech = cdf_tech - lag(cdf_tech),
    d_cdf_cont = cdf_cont - lag(cdf_cont)
    ) %>%
  filter(!is.na(d_cdf))

avg_empr <- sum(df_cdf$lwage * df_cdf$d_cdf) / sum(df_cdf$d_cdf)
sd_empr <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf) / sum(df_cdf$d_cdf) - avg_empr^2)

avg_empr_mid <- sum(df_cdf$lwage_mid * df_cdf$d_cdf) / sum(df_cdf$d_cdf)
sd_empr_mid <- sqrt(sum(df_cdf$lwage_mid^2 * df_cdf$d_cdf) / sum(df_cdf$d_cdf) - avg_empr^2)

avg_tech <- sum(df_cdf$lwage * df_cdf$d_cdf_tech) / sum(df_cdf$d_cdf_tech)
sd_tech <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf_tech) / sum(df_cdf$d_cdf_tech) - avg_tech^2)

avg_cont <- sum(df_cdf$lwage * df_cdf$d_cdf_cont) / sum(df_cdf$d_cdf_cont)
sd_cont <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf_cont) / sum(df_cdf$d_cdf_cont) - avg_cont^2)

lwage_t1_mean <- wtd.mean(df_t1()$lwage, df_t1()$weight)
lwage_t1_sd <- sqrt(wtd.var(df_t1()$lwage, df_t1()$weight, normwt = T))

lwage_t2_mean <- wtd.mean(df_t2()$lwage, df_t2()$weight)
lwage_t2_sd <- sqrt(wtd.var(df_t2()$lwage, df_t2()$weight, normwt = T))


# results data frame
df_decompose <- data.frame(matrix(ncol = 5, nrow = 2))
colnames(df_decompose) <- c("name", "total", "tech", "control", "unexplained")

df_decompose[1, ] <- list("Std Dev", 
                        lwage_t2_sd - lwage_t1_sd,
                        lwage_t2_sd - sd_tech,
                        sd_tech - sd_cont,
                        sd_cont - lwage_t1_sd
)

df_decompose[2, ] <- list("pct", 
                        100,
                        100 * (lwage_t2_sd - sd_tech) / (lwage_t2_sd - lwage_t1_sd),
                        100 * (sd_tech - sd_cont) / (lwage_t2_sd - lwage_t1_sd),
                        100 * (sd_cont - lwage_t1_sd) / (lwage_t2_sd - lwage_t1_sd)
)

View(df_decompose)

df_summary <- df %>% 
  filter(YEAR %in% years) %>%
  group_by(YEAR) %>%
  dplyr::select(weight, !!tech, !!xs_full) %>%
  mutate_at(c(tech, xs_full), funs(. * weight)) %>%
  summarise_all(sum)

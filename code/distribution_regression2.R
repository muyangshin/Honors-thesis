library(tidyverse)
# library(Hmisc)  # weighted
library(survey)  # survey analysis

source("code/tools.R")


# LOAD DATA ---------------------------------------------------------------

# start year, end year
years <- c(1992, 2017)

# read data from sql
df <- load_df_aces(paste0("YEAR = ", years[1], " or YEAR = ", years[2]))

# IND to naics
df_IND_naics <- prepare_INDLY_conversion_census_to_naics(years, occs_high_tech, cached = T)

df <- df %>%
  mutate(cons = 1) %>%
  
  # restrict to men for now
  filter(SEX == 1)

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
  
  # # tech using industry sector
  # left_join(df_IND_naics, by = c("YEAR", "INDLY")) %>%
  # filter(!is.na(tech.pct)) %>%
  
  filter(
    # censor those below minimum wage
    wage_nominal >= min_wage,
    
    # college-educated only
    college == 1
  )


# analysis setup -------------------------------------------------

# regression specification
tech <- "high_tech"
xs <- c("schooling", "experience", "experience_2", "experience_3", "experience_4", 
        "married", "RACE", "FULLPART")

# vector of tech variable names, including dummy variables
tech_full <- c()  # exclude first
tech_full2 <- c()  # everything

tech_vector <- df[[tech]]
if (!is.factor(tech_vector)) {
  # non-categorical
  tech_full <- tech
  tech_full2 <- tech
} else {
  # categorical
  tech_new_vectors <- model.matrix(as.formula(paste0("~", tech)), df)
  colnames(tech_new_vectors) <- paste0(tech, levels(tech_vector))
  
  tech_full <- c(tech_full, colnames(tech_new_vectors)[-1])
  tech_full2 <- c(tech_full2, colnames(tech_new_vectors))
  df <- cbind(df, tech_new_vectors)
  rm(tech_new_vectors)
}


# vector of control variable names, including dummy variables
xs_full <- c()
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

# construct survey design object
dfw <- svydesign(ids = ~1, data = df, weights = ~weight)
dfw_t1 <- subset(dfw, YEAR == years[1])
dfw_t2 <- subset(dfw, YEAR == years[2])

dfw_t <- function(i) {
  if (i == 1) {
    return(dfw_t1)
  } else {
    return(dfw_t2)
  }
}

# set thresholds for lwage
quantile_interval <- 0.01
lwage_thresholds <- t(svyquantile(~lwage, dfw, seq(0, 1, by = quantile_interval)))


# wage structure ----------------------------------------------------------

# * Counterfactual Y1 given X1 ----------------------------------------------

df_regs <- vector("list", 2)

# construct empty data frame to save regression coefficients
for (i in 1:2) {
  df_regs[[i]] <- data.frame(matrix(ncol = 3 + length(tech_full) + length(xs_full), 
                                    nrow = length(lwage_thresholds))) %>%
    setNames(c("year", "lwage", "cons", tech_full, xs_full))

  # distribution regression: using time 2
  for (j in seq_along(lwage_thresholds)) {
    threshold <- lwage_thresholds[j]
    
    y <- ifelse(dfw_t(i)$variables$lwage <= threshold, 1, 0)
    reg_formula <- paste0("y ~ ", paste0(c(tech_full, xs_full), collapse = " + "))
    
    # # linear
    # reg <- lm(reg_formula, data = df_t(2), weights = weight)
    
    # logistic
    reg <- svyglm(reg_formula, design = dfw_t(i), family = "binomial")
    
    df_regs[[i]][j, ] <- c(years[i], threshold, reg$coefficients)
  }
}


# * Construct distributions ---------------------------------------

# empty data frame to store cdfs
df_cdf <- data.frame(
  # year = numeric(length(lwage_thresholds)),
  lwage = numeric(length(lwage_thresholds)),
  cdf_tech_ws = numeric(length(lwage_thresholds)),
  cdf_cont_ws = numeric(length(lwage_thresholds)),
  cdf_tech_comp = numeric(length(lwage_thresholds))
)

# # year
# df_cdf$year <- years[2]

# lwage
df_cdf$lwage <- as.vector(lwage_thresholds)

# cdf of period 1 and 2
# df_cdf$cdf <- seq(0, 1, by = quantile_interval)


# ** cdf_tech_ws --------------------

covariates <- as.matrix(dfw_t2$variables %>% dplyr::select(cons, !!tech_full, !!xs_full))

coef_matrix <- as.matrix(cbind(df_regs[[2]][, c("cons")], 
                               df_regs[[1]][, tech_full],
                               df_regs[[2]][, xs_full]
                               ))

df_cdf$cdf_tech_ws <- weighted_colMeans(
  logistic(
    covariates %*% t(coef_matrix)
    ),
  weights(dfw_t2)
  )

# last entry
if (is.nan(df_cdf$cdf_cont_ws[length(df_cdf$cdf_cont_ws)])) {
  df_cdf$cdf_cont_ws[length(df_cdf$cdf_cont_ws)] <- 1
}


# *** cdf_cont_ws -------------------------------------------------------------

coef_matrix <- as.matrix(cbind(df_regs[[1]][, c("cons")], 
                               df_regs[[1]][, tech_full],
                               df_regs[[1]][, xs_full]
))

df_cdf$cdf_cont_ws <- weighted_colMeans(
  logistic(
    covariates %*% t(coef_matrix)
    ),
  weights(dfw_t2)
  )

# last entry
if (is.nan(df_cdf$cdf_cont_ws[length(df_cdf$cdf_cont_ws)])) {
  df_cdf$cdf_cont_ws[length(df_cdf$cdf_cont_ws)] <- 1
}


# # ** linear ---------------------------------------------------------------

# # *** cdf_tech_ws ------------------------------------------------------------
# # Pr_0(tech | c_1)
# covariates_weighted_mean <- sapply(1:ncol(covariates),
#                                    function (i) weighted.mean(covariates[, i], w = prob_tech / prob_tech_sum))
# 
# df_cdf$cdf_tech_ws <- as.vector(
#   as.matrix(df_reg[, c("cons", tech, xs_full)]) %*% covariates_weighted_mean
#   )

# # *** cdf_cont_ws ------------------------------------------------------------

# df_cdf$cdf_cont_ws <- as.vector(
#   as.matrix(df_reg[, c("cons", tech, xs_full)]) %*% colMeans(df_t(1) %>% dplyr::select(cons, !!tech, !!xs_full))
# )


# composition -------------------------------------------------------------

# * conditional distribution u0 given c0 ------------------------------------

reg_formula <- paste0(tech, " ~ ", paste0(xs_full, collapse = " + "))

# binomial
reg_u0_c0 <- svyglm(reg_formula,
                 design = dfw_t1,
                 family = "binomial")

# # multinomial
# reg_u0_c0 <- multinom(reg_formula,
#                       data = df_t(1),
#                       weights = weight)


# * cdf_tech_comp: Pr_0(tech | c_1) ------------------------------------------

# binomial
prob_tech <- logistic(predict(reg_u0_c0, dfw_t2$variables %>% dplyr::select(cons, !!xs_full)))
prob_tech_sum <- sum(prob_tech)

covariates <- as.matrix(dfw_t2$variables %>% dplyr::select(cons, !!tech_full, !!xs_full))

temp <- logistic(
  covariates %*% t(as.matrix(
    df_regs[[1]][, c("cons", tech_full, xs_full)]
    ))
  )

df_cdf$cdf_tech_comp <- weighted_colMeans(temp, weights(dfw_t2) * prob_tech / prob_tech_sum)

# last entry
if (is.nan(df_cdf$cdf_tech_comp[length(df_cdf$cdf_tech_comp)])) {
  df_cdf$cdf_tech_comp[length(df_cdf$cdf_tech_comp)] <- 1
}


# results ------------------------------------------------------

# create d_cdf
df_cdf <- df_cdf %>%
  mutate(
    lwage_mid = lwage - (lwage - lag(lwage)) / 2,
    d_cdf_tech_ws = cdf_tech_ws - lag(cdf_tech_ws),
    d_cdf_cont_ws = cdf_cont_ws - lag(cdf_cont_ws),
    d_cdf_tech_comp = cdf_tech_comp - lag(cdf_tech_comp)
  ) %>%
  filter(!is.na(d_cdf_tech_ws))

# summary statistics
df_summary <- df %>% 
  filter(YEAR %in% years) %>%
  group_by(YEAR) %>%
  dplyr::select(weight, !!tech_full2, !!xs_full) %>%
  mutate_at(c(tech_full2, xs_full), funs(. * weight)) %>%
  summarise_all(sum)

# decomposition
df_decompose <- data.frame(matrix(ncol = 6, nrow = 12))
colnames(df_decompose) <- c("name", "total", "tech_ws", "control_ws", "tech_comp", "control_comp")


# * variance ------------------------------------------------------

# t1 and t2's observed mean and sd 
lwage_t1_mean <- svymean(~lwage, dfw_t1)
lwage_t1_sd <- sqrt(svyvar(~lwage, dfw_t1))

lwage_t2_mean <- svymean(~lwage, dfw_t2)
lwage_t2_sd <- sqrt(svyvar(~lwage, dfw_t2))

# tech ws
avg_tech_ws <- sum(df_cdf$lwage * df_cdf$d_cdf_tech_ws) / sum(df_cdf$d_cdf_tech_ws)
sd_tech_ws <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf_tech_ws) / sum(df_cdf$d_cdf_tech_ws) - avg_tech_ws^2)

# other control variables ws
avg_cont_ws <- sum(df_cdf$lwage * df_cdf$d_cdf_cont_ws) / sum(df_cdf$d_cdf_cont_ws)
sd_cont_ws <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf_cont_ws) / sum(df_cdf$d_cdf_cont_ws) - avg_cont_ws^2)

# tech comp
avg_tech_comp <- sum(df_cdf$lwage * df_cdf$d_cdf_tech_comp) / sum(df_cdf$d_cdf_tech_comp)
sd_tech_comp <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf_tech_comp) / sum(df_cdf$d_cdf_tech_comp) - avg_tech_comp^2)

# write to df
df_decompose[1, ] <- list("Std Dev", 
                          sprintf("%.3f", lwage_t2_sd - lwage_t1_sd),
                          sprintf("%.3f", lwage_t2_sd - sd_tech_ws),
                          sprintf("%.3f", sd_tech_ws - sd_cont_ws),
                          sprintf("%.3f", sd_cont_ws - sd_tech_comp),
                          sprintf("%.3f", sd_tech_comp - lwage_t1_sd)
)

df_decompose[2, ] <- list("pct", 
                          100,
                          sprintf("%.1f%%", 100 * (lwage_t2_sd - sd_tech_ws) / (lwage_t2_sd - lwage_t1_sd)),
                          sprintf("%.1f%%", 100 * (sd_tech_ws - sd_cont_ws) / (lwage_t2_sd - lwage_t1_sd)),
                          sprintf("%.1f%%", 100 * (sd_cont_ws - sd_tech_comp) / (lwage_t2_sd - lwage_t1_sd)),
                          sprintf("%.1f%%", 100 * (sd_tech_comp - lwage_t1_sd) / (lwage_t2_sd - lwage_t1_sd))
)


# * percentiles ---------------------------------------------------

percents <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

# observed
t1_percentiles <- t(svyquantile(~lwage, dfw_t1, percents))
names(t1_percentiles) <- percents

t2_percentiles <- t(svyquantile(~lwage, dfw_t2, percents))
names(t2_percentiles) <- percents

# for constructed cdfs
cdfs <- c("tech_ws", "cont_ws", "tech_comp")
cdf_percentiles <- vector("list", length(cdfs))
names(cdf_percentiles) <- cdfs

# calculate percentiles
for (cdf in cdfs) {
  cdf_percentiles[[cdf]] <- numeric(length(percents))
  vector_name <- paste0("cdf_", cdf)
  j <- 1
  for (i in 1:length(lwage_thresholds)) {
    if (df_cdf[i, vector_name] > percents[j]) {
      # intrapolate
      cdf_percentiles[[cdf]][j] <- (df_cdf[i, vector_name] - percents[j]) /
        (df_cdf[i, vector_name] - df_cdf[i - 1, vector_name]) *
        (lwage_thresholds[i] - lwage_thresholds[i - 1]) + lwage_thresholds[i - 1]
      
      j <- j + 1
      if (j > length(percents)) {
        break
      }
    }
  }
  names(cdf_percentiles[[cdf]]) <- percents
}

# store in the data frame
i <- 3
for (pair in list(c(0.9, 0.1), c(0.5, 0.1), c(0.9, 0.5), c(0.75, 0.25), c(0.95, 0.05))) {
  perc1 <- pair[1]
  perc2 <- pair[2]
  
  t2_t1_diff <- (t2_percentiles[[as.character(perc1)]] - t2_percentiles[[as.character(perc2)]]) - (t1_percentiles[[as.character(perc1)]] - t1_percentiles[[as.character(perc2)]])
  
  df_decompose[i, ] <- list(
    paste0(100 * perc1, "-", 100 * perc2),
    sprintf("%.3f", t2_t1_diff),
    sprintf("%.3f", (t2_percentiles[[as.character(perc1)]] - t2_percentiles[[as.character(perc2)]]) - (cdf_percentiles[["tech_ws"]][[as.character(perc1)]] - cdf_percentiles[["tech_ws"]][[as.character(perc2)]])),
    sprintf("%.3f", (cdf_percentiles[["tech_ws"]][[as.character(perc1)]] - cdf_percentiles[["tech_ws"]][[as.character(perc2)]]) - (cdf_percentiles[["cont_ws"]][[as.character(perc1)]] - cdf_percentiles[["cont_ws"]][[as.character(perc2)]])),
    sprintf("%.3f", (cdf_percentiles[["cont_ws"]][[as.character(perc1)]] - cdf_percentiles[["cont_ws"]][[as.character(perc2)]]) - (cdf_percentiles[["tech_comp"]][[as.character(perc1)]] - cdf_percentiles[["tech_comp"]][[as.character(perc2)]])),
    sprintf("%.3f", (cdf_percentiles[["tech_comp"]][[as.character(perc1)]] - cdf_percentiles[["tech_comp"]][[as.character(perc2)]]) - (t1_percentiles[[as.character(perc1)]] - t1_percentiles[[as.character(perc2)]]))
  )
  i <- i + 1
  
  df_decompose[i, ] <- list(
    "pct",
    100,
    sprintf("%.1f%%", 100 * ((t2_percentiles[[as.character(perc1)]] - t2_percentiles[[as.character(perc2)]]) - (cdf_percentiles[["tech_ws"]][[as.character(perc1)]] - cdf_percentiles[["tech_ws"]][[as.character(perc2)]])) / t2_t1_diff),
    sprintf("%.1f%%", 100 * ((cdf_percentiles[["tech_ws"]][[as.character(perc1)]] - cdf_percentiles[["tech_ws"]][[as.character(perc2)]]) - (cdf_percentiles[["cont_ws"]][[as.character(perc1)]] - cdf_percentiles[["cont_ws"]][[as.character(perc2)]])) / t2_t1_diff),
    sprintf("%.1f%%", 100 * ((cdf_percentiles[["cont_ws"]][[as.character(perc1)]] - cdf_percentiles[["cont_ws"]][[as.character(perc2)]]) - (cdf_percentiles[["tech_comp"]][[as.character(perc1)]] - cdf_percentiles[["tech_comp"]][[as.character(perc2)]])) / t2_t1_diff),
    sprintf("%.1f%%", 100 * ((cdf_percentiles[["tech_comp"]][[as.character(perc1)]] - cdf_percentiles[["tech_comp"]][[as.character(perc2)]]) - (t1_percentiles[[as.character(perc1)]] - t1_percentiles[[as.character(perc2)]])) / t2_t1_diff)
  )  
  i <- i + 1
}


View(df_decompose)


library(tidyverse)
library(sqldf)  # SQL
library(Emcdf)

source("code/tools.R")


# LOAD DATA ---------------------------------------------------------------

# df_0 <- load_df()

# start year, end year
years <- c(2002, 2016)

# IND to naics
df_IND_naics <- prepare_INDLY_conversion_census_to_naics(years, occs_bls_stem, cached = T)
  
df <- df_org_0 %>%
  filter(YEAR %in% years) %>%
  
  mutate(cons = 1) %>%
  
  # restrict to men for now
  filter(SEX == 1)

# list of METAREAs which appears only once
METAREAs_appearing_once <- df %>% 
  select(YEAR, METAREA) %>% 
  group_by(METAREA) %>% 
  summarise(n = n_distinct(YEAR)) %>%
  filter(n == 1) %>%
  pull(METAREA)

df <- df %>%
  # if METAREA appears only once, code it as 10000
  mutate(METAREA = ifelse(METAREA %in% get("METAREAs_appearing_once"), 10000, METAREA))  %>%
  
  # reset factor levels
  mutate(METAREA = factor(METAREA)) %>%
  
  # tech using indsutry sector
  left_join(df_IND_naics, by = c("YEAR", "IND")) %>%
  filter(!is.na(tech.pct)) %>%
  mutate(is_high_tech_ind = ifelse(tech.pct >= tech.pct.economy + tech.pct.sd, 1, 0))
  
# recalculate weight
df <- df %>%
  group_by(YEAR) %>%
  mutate(weight = weight / sum(weight)) %>%
  ungroup()

# ANALYSIS ----------------------------------------------------------------

# set thresholds for lwage: approx from -8 to 8
# lwage_thresholds <- seq(min(df$lwage), max(df$lwage), by = 0.8)
lwage_thresholds <- quantile(df$lwage, seq(0.02, 0.98, by = 0.001))

# regression specification
tech <- "is_stem"
xs <- c("schooling", "experience", "experience_sq", "married")

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


# Construct covariate distributions ---------------------------------------

quantile_interval <- 0.1

construct_empirical_pdf <- function(vars, vecs, data) {
  # number of cells
  num <- 1
  for (vec in vecs) {
    num <- num * length(vec)
  }
  
  # construct df_max, df_min
  df_max <- as.data.frame(matrix(ncol = length(vecs),
                                 nrow = num))
  df_min <- as.data.frame(matrix(ncol = length(vecs),
                                 nrow = num))
  for (i in 1:num) {
    row_max <- vector("list", length = length(vecs))
    row_min <- vector("list", length = length(vecs))
    prevs <- 1
    for (j in seq_along(vecs)) {
      vec <- vecs[[j]]
      
      row_max[[j]] <- vec[((i - 1) %/% prevs) %% length(vec) + 1]
      
      if (((i - 1) %/% prevs) %% length(vec) + 1 == 1) {
        row_min[[j]] <- -Inf
      } else {
        row_min[[j]] <- vec[((i - 1) %/% prevs) %% length(vec)]
      }
      
      prevs <- prevs * length(vec)
    }
    
    df_max[i, ] <- row_max
    df_min[i, ] <- row_min
  }
  
  # construct df
  result <- numeric(num)
  
  for (i in 1:num) {
    queries <- c()
    for (j in seq_along(vecs)) {
      queries <- c(queries, 
                   paste0(vars[[j]], " <= ", df_max[i, j]),
                   paste0(vars[[j]], " > ", df_min[i, j])
                   )
    }
    
    queries_string <- paste0(queries, collapse = " & ")
    
    result[i] <- nrow(data %>% filter_(queries_string))
  }
  
  return(as.matrix(cbind(1, df_max, result / nrow(data))))
}

# f_x0
f_x0 <- construct_empirical_pdf(
  c(tech, xs_full),
  list(
    c(0, 1),
    quantile(df$schooling, seq(0, 1, by = quantile_interval))[-1],
    quantile(df$experience, seq(0, 1, by = quantile_interval))[-1],
    quantile(df$experience_sq, seq(0, 1, by = quantile_interval))[-1],
    c(0, 1)
  ),
  data = df %>% filter(YEAR == years[1])
)

# f_c1
f_c1 <- construct_empirical_pdf(
  c(xs_full),
  list(
    quantile(df$schooling, seq(0, 1, by = quantile_interval))[-1],
    quantile(df$experience, seq(0, 1, by = quantile_interval))[-1],
    quantile(df$experience_sq, seq(0, 1, by = quantile_interval))[-1],
    c(0, 1)
  ),
  data = df %>% filter(YEAR == years[2])
)

# # F_X0, F_X1, F_C1
# # TODO: automated
# a <- as.matrix(expand.grid(
#   c(0, 1),
#   quantile(df$schooling, seq(0, 1, by = quantile_interval))[-1],
#   quantile(df$experience, seq(0, 1, by = quantile_interval))[-1],
#   quantile(df$experience_sq, seq(0, 1, by = quantile_interval))[-1],
#   c(0, 1)
# ))
# 
# a_min <- as.matrix(expand.grid(
#   c(-1, 0),
#   c(-Inf, quantile(df$schooling, seq(0, 1, by = quantile_interval))[-c(1, 1 / quantile_interval + 1)]),
#   c(-Inf, quantile(df$experience, seq(0, 1, by = quantile_interval))[-c(1, 1 / quantile_interval + 1)]),
#   c(-Inf, quantile(df$experience_sq, seq(0, 1, by = quantile_interval))[-c(1, 1 / quantile_interval + 1)]),
#   c(-1, 0)
# ))
# 
# # without tech
# a_C <- as.matrix(expand.grid(
#   quantile(df$schooling, seq(0, 1, by = quantile_interval))[-1],
#   quantile(df$experience, seq(0, 1, by = quantile_interval))[-1],
#   quantile(df$experience_sq, seq(0, 1, by = quantile_interval))[-1],
#   c(0, 1)
# ))
# 
# a_C_min <- as.matrix(expand.grid(
#   c(-Inf, quantile(df$schooling, seq(0, 1, by = quantile_interval))[-c(1, 1 / quantile_interval + 1)]),
#   c(-Inf, quantile(df$experience, seq(0, 1, by = quantile_interval))[-c(1, 1 / quantile_interval + 1)]),
#   c(-Inf, quantile(df$experience_sq, seq(0, 1, by = quantile_interval))[-c(1, 1 / quantile_interval + 1)]),
#   c(-1, 0)
# ))
# 
# df_t1 <- df %>%
#   filter(YEAR == years[1]) %>%
#   select(!!tech, !!xs_full)
# 
# obj <- initF(as.matrix(df_t1), 2)
# 
# f_x0 <- emcdf(obj, a) - emcdf(obj, a_min)
# 
# # X0, X1
# for (i in seq_along(years)) {
#   t <- years[i]
#   df_t <- df %>%
#     filter(YEAR == t) %>%
#     select(!!tech, !!xs_full)
# 
#   obj <- initF(as.matrix(df_t), 2)
# 
#   cov_distributions[[i]] <- emcdf(obj, a)
# }
# 
# # C1
# t <- years[2]
# df_t <- df %>%
#   filter(YEAR == t) %>%
#   select(!!xs_full)
# obj <- initF(as.matrix(df_t), 2)
# cov_distributions[[3]] <- emcdf(obj, a_C)


# Counterfactual Y1 given X1 ----------------------------------------------

# construct empty data frame to save regression coefficients
df_reg <- data.frame(matrix(ncol = 4 + length(xs_full), 
                            nrow = length(lwage_thresholds))) %>%
  setNames(c("year", "lwage", "cons", tech, xs_full))

# distribution regression
t <- years[2]  # use the last year
df_t <- df %>% filter(YEAR == t)

for (j in seq_along(lwage_thresholds)) {
  threshold <- lwage_thresholds[j]
  
  y <- df_t$lwage <= threshold
  reg_formula <- paste0("y ~ ", paste0(c(tech, xs_full), collapse = " + "))
  reg <- lm(reg_formula, data = df_t, weights = weight)
  
  df_reg[j, ] <- c(t, threshold, reg$coefficients)
}


# conditional distribution u0 given c0 ------------------------------------

reg_formula <- paste0(tech, " ~ ", paste0(xs_full, collapse = " + "))

reg_u0_c0 <- glm(reg_formula, 
                 data = df %>% filter(YEAR == years[1]), 
                 family = "binomial",
                 weights = weight)


# construct distributions -------------------------------------------------

# empty data frame to store cdfs
df_cdf <- data.frame(
  year = numeric(length(lwage_thresholds)),
  lwage = numeric(length(lwage_thresholds)),
  cdf = numeric(length(lwage_thresholds)),
  cdf_tech = numeric(length(lwage_thresholds)),
  cdf_cont = numeric(length(lwage_thresholds))
)

df_cdf$year <- years[2]
df_cdf$lwage <- lwage_thresholds
df_cdf$cdf <- seq(0.02, 0.98, by = 0.96 / (length(lwage_thresholds) - 1))

# fitted values of logistic regression
reg_u0_c0_fitted <- f_x0[, -c(2, ncol(f_x0))] %*% reg_u0_c0$coefficients
reg_u0_c0_fitted <- exp(reg_u0_c0_fitted) / (1 + exp(reg_u0_c0_fitted))
reg_u0_c0_fitted <- ifelse(f_x0[, 2] == 0, 1 - reg_u0_c0_fitted, reg_u0_c0_fitted)

for (i in 1:nrow(df_reg)) {
  # counterfactual
  vec_coeffs <- t(df_reg[i, -(1:2)])
  
  df_cdf[i, "cdf_tech"] <- sum((f_x0[, -ncol(f_x0)] %*% vec_coeffs) * reg_u0_c0_fitted * rep(f_c1[, ncol(f_c1)], each = 2))
  df_cdf[i, "cdf_cont"] <- sum((f_x0[, -ncol(f_x0)] %*% vec_coeffs) * f_x0[, ncol(f_x0)])
}


# # construct cdfs
# for (i in seq_along(years)) {
#   t <- years[i]
#   df_t <- df %>% filter(YEAR == t)
#   N_t <- nrow(df_t)
#   
#   A_t <- df_t %>% 
#     mutate_at(c("cons", tech, xs_full), funs(. * weight)) %>%
#     select(cons, !!tech, !!xs_full) %>% 
#     summarise_all(sum)
#   
#   A_tech <- A_t %>% 
#     mutate(
#       !!tech := t(as.matrix(df[df$YEAR == years[1], tech])) %*% as.matrix(df[df$YEAR == years[1], "weight"])
#       )
#   A_cont <- as.matrix(A_tech)
#   A_cont[3:(2 + length(xs_full))] <- as.matrix(df %>% 
#                                             filter(YEAR == years[1]) %>%
#                                             mutate_at(xs_full, funs(. * weight)) %>%
#                                             select(!!xs_full) %>% 
#                                             summarise_all(funs(sum)))
#   
#   A_t <- as.matrix(A_t)
#   A_tech <- as.matrix(A_tech)
#   A_cont <- as.matrix(A_cont)
#   
#   for (j in seq_along(lwage_thresholds)) {
#     threshold <- lwage_thresholds[j]
# 
#     B <- t(as.matrix(df_reg[(i - 1) * length(lwage_thresholds) + j, c("cons", tech, xs_full)]))
#     
#     df_cdf[(i - 1) * length(lwage_thresholds) + j, ] <- c(t,
#                                                           threshold,
#                                                           A_t %*% B,
#                                                           A_tech %*% B,
#                                                           A_cont %*% B
#                                                           )
#   }
# }

# calculate variance ------------------------------------------------------

df_cdf <- df_cdf %>%
  mutate(
    d_cdf = cdf - lag(cdf),
    d_cdf_tech = cdf_tech - lag(cdf_tech),
    d_cdf_cont = cdf_cont - lag(cdf_cont)
    ) %>%
  filter(!is.na(d_cdf))

avg_empr <- sum(df_cdf$lwage * df_cdf$d_cdf) / sum(df_cdf$d_cdf)
sd_empr <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf) / sum(df_cdf$d_cdf) - avg_empr^2)

avg_tech <- sum(df_cdf$lwage * df_cdf$d_cdf_tech) / sum(df_cdf$d_cdf_tech)
sd_tech <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf_tech) / sum(df_cdf$d_cdf_tech) - avg_tech^2)

avg_cont <- sum(df_cdf$lwage * df_cdf$d_cdf_cont) / sum(df_cdf$d_cdf_cont)
sd_cont <- sqrt(sum(df_cdf$lwage^2 * df_cdf$d_cdf_cont) / sum(df_cdf$d_cdf_cont) - avg_cont^2)

lwage_t1 <- df %>% filter(YEAR == years[1]) %>% pull(lwage)
weight_t1 <- df %>% filter(YEAR == years[1]) %>% pull(weight)
lwage_t1_mean <- weighted.mean(lwage_t1, weight_t1)
lwage_t1_sd <- sqrt(sum(weight_t1 * (lwage_t1 - lwage_t1_mean)^2))

lwage_t2 <- df %>% filter(YEAR == years[2]) %>% pull(lwage)
weight_t2 <- df %>% filter(YEAR == years[2]) %>% pull(weight)
lwage_t2_mean <- weighted.mean(lwage_t2, weight_t2)
lwage_t2_sd <- sqrt(sum(weight_t2 * (lwage_t2 - lwage_t2_mean)^2))


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
  select(weight, !!tech, !!xs_full) %>%
  mutate_at(c(tech, xs_full), funs(. * weight)) %>%
  summarise_all(sum)
library(tidyverse)
library(sqldf)

source("code/tools.R")


# LOAD DATA ---------------------------------------------------------------

# df_0 <- load_df()
rm(list = setdiff(ls(), "df_0"))

# start year, end year
years <- c(2002, 2017)

df <- df_0 %>%
  filter(YEAR %in% years) %>%
  
  mutate(cons = 1) %>%
  
  # restrict to men for now
  filter(SEX == 1) %>%
  
  # recalculate weight
  group_by(YEAR) %>%
  mutate(weight = weight / sum(weight)) %>%
  ungroup()

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
  mutate(METAREA = factor(METAREA))
  
# ANALYSIS ----------------------------------------------------------------

# set thresholds for lwage: approx from -8 to 8
lwage_thresholds <- seq(min(df$lwage), max(df$lwage), by = 1.6)

# regression specification
tech <- "is_stem_related"
xs <- c("schooling", "experience", "experience_sq", "married", "METAREA")

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

# construct empty data frame to save regression coefficients
df_reg <- data.frame(matrix(ncol = 4 + length(xs_full), 
                            nrow = length(years) * length(lwage_thresholds))) %>%
  setNames(c("year", "lwage", "cons", tech, xs_full))

# distribution regression
for (i in seq_along(years)) {
  t <- years[i]
  df_t <- df %>% filter(YEAR == t)
  
  for (j in seq_along(lwage_thresholds)) {
    threshold <- lwage_thresholds[j]
    
    y <- df_t$lwage <= threshold
    reg_formula <- paste0("y ~ ", paste0(c(tech, xs_full), collapse = " + "))
    reg <- lm(reg_formula, data = df_t, weights = weight)
    
    df_reg[(i - 1) * length(lwage_thresholds) + j, ] <- c(t, threshold, reg$coefficients)
  }
}

# empty data frame to store cdfs
df_cdf <- data.frame(
  year = numeric(length(years) * length(lwage_thresholds)),
  lwage = numeric(length(years) * length(lwage_thresholds)),
  cdf = numeric(length(years) * length(lwage_thresholds)),
  cdf_tech = numeric(length(years) * length(lwage_thresholds)),
  cdf_cont = numeric(length(years) * length(lwage_thresholds))
)

# construct cdfs
for (i in seq_along(years)) {
  t <- years[i]
  df_t <- df %>% filter(YEAR == t)
  N_t <- nrow(df_t)
  
  A_t <- df_t %>% 
    mutate_at(c("cons", tech, xs_full), funs(. * weight)) %>%
    select(cons, !!tech, !!xs_full) %>% 
    summarise_all(sum)
  
  A_tech <- A_t %>% 
    mutate(
      !!tech := t(as.matrix(df[df$YEAR == years[1], tech])) %*% as.matrix(df[df$YEAR == years[1], "weight"])
      )
  A_cont <- as.matrix(A_tech)
  A_cont[3:(2 + length(xs_full))] <- as.matrix(df %>% 
                                            filter(YEAR == years[1]) %>%
                                            mutate_at(xs_full, funs(. * weight)) %>%
                                            select(!!xs_full) %>% 
                                            summarise_all(funs(sum)))
  
  A_t <- as.matrix(A_t)
  A_tech <- as.matrix(A_tech)
  A_cont <- as.matrix(A_cont)
  
  for (j in seq_along(lwage_thresholds)) {
    threshold <- lwage_thresholds[j]

    B <- t(as.matrix(df_reg[(i - 1) * length(lwage_thresholds) + j, c("cons", tech, xs_full)]))
    
    df_cdf[(i - 1) * length(lwage_thresholds) + j, ] <- c(t,
                                                          threshold,
                                                          A_t %*% B,
                                                          A_tech %*% B,
                                                          A_cont %*% B
                                                          )
  }
}

# calculate variance
df_results <- df_cdf %>%
  group_by(year) %>%
  mutate(
    d_cdf = cdf - lag(cdf),
    d_cdf_tech = cdf_tech - lag(cdf_tech),
    d_cdf_cont = cdf_cont - lag(cdf_cont)
    ) %>%
  filter(!is.na(d_cdf)) %>%
  summarise(
    e = sum(lwage * d_cdf),
    sd = sqrt(sum(lwage^2 * d_cdf) - sum(lwage * d_cdf)^2),
    sd_tech = sqrt(sum(lwage^2 * d_cdf_tech) - sum(lwage * d_cdf_tech)^2),
    sd_cont = sqrt(sum(lwage^2 * d_cdf_cont) - sum(lwage * d_cdf_cont)^2)
    )
  
# results data frame
df_decompose <- data.frame(matrix(ncol = 5, nrow = 2))
colnames(df_decompose) <- c("name", "total", "tech", "control", "unexplained")

df_decompose[1, ] <- list("Std Dev", 
                        df_results[2, "sd"] - df_results[1, "sd"],
                        df_results[2, "sd"] - df_results[2, "sd_tech"],
                        df_results[2, "sd_tech"] - df_results[2, "sd_cont"],
                        df_results[2, "sd_cont"] - df_results[1, "sd"]
)

df_decompose[2, ] <- list("pct", 
                        100,
                        100 * (df_results[2, "sd"] - df_results[2, "sd_tech"]) / (df_results[2, "sd"] - df_results[1, "sd"]),
                        100 * (df_results[2, "sd_tech"] - df_results[2, "sd_cont"]) / (df_results[2, "sd"] - df_results[1, "sd"]),
                        100 * (df_results[2, "sd_cont"] - df_results[1, "sd"]) / (df_results[2, "sd"] - df_results[1, "sd"])
)

View(df_decompose)

df_summary <- df %>% 
  filter(YEAR %in% years) %>%
  group_by(YEAR) %>%
  select(weight, !!tech, !!xs_full) %>%
  mutate_at(c(tech, xs_full), funs(. * weight)) %>%
  summarise_all(sum)
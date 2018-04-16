library(tidyverse)
library(sqldf)

source("code/tools.R")


# LOAD DATA ---------------------------------------------------------------

# df_0 <- load_df()
df <- df_0 %>%
  mutate(cons = 1)


# ANALYSIS ----------------------------------------------------------------

# years <- unique(df$YEAR)
years <- c(2002, 2017)
lwage_thresholds <- seq(min(df$lwage), max(df$lwage), by = 0.2)[-1]

df_reg <- data.frame(
  year = numeric(length(years) * length(wage_thresholds)),
  wage = numeric(length(years) * length(wage_thresholds)),
  alpha = numeric(length(years) * length(wage_thresholds)),
  beta = numeric(length(years) * length(wage_thresholds))
  )

for (i in seq_along(years)) {
  year <- years[i]
  df_year <- df %>% filter(YEAR == year)
  
  for (j in seq_along(lwage_thresholds)) {
    threshold <- lwage_thresholds[j]
    
    y <- df_year$lwage <= threshold
    reg <- lm(y ~ is_stem, data = df_year)
    
    df_reg[(i - 1) * length(lwage_thresholds) + j, ] <- c(year, threshold, reg$coefficients[1], reg$coefficients[2])
  }
}

years <- c(2002, 2017)
df_cdf <- data.frame(
  year = numeric(length(years) * length(wage_thresholds)),
  wage = numeric(length(years) * length(wage_thresholds)),
  cdf = numeric(length(years) * length(wage_thresholds))
)

for (i in seq_along(years)) {
  year <- years[i]
  df_year <- df %>% filter(YEAR == year)
  N_t <- nrow(df_year)
  
  for (j in seq_along(lwage_thresholds)) {
    threshold <- lwage_thresholds[j]
    
    A <- as.matrix(df_year[, c("cons", "is_stem")])
    B <- t(as.matrix(df_reg[(i - 1) * length(lwage_thresholds) + j, c("alpha", "beta")]))
    
    df_cdf[(i - 1) * length(lwage_thresholds) + j, ] <- c(year, 
                                                          threshold, 
                                                          1 / N_t * sum(A %*% B))
  }
}
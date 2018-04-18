library(tidyverse)
library(sqldf)

source("code/tools.R")


# LOAD DATA ---------------------------------------------------------------

df_0 <- load_df()
df <- df_0 %>%
  mutate(cons = 1)


# ANALYSIS ----------------------------------------------------------------

years <- c(2002, 2017)
lwage_thresholds <- seq(min(df$lwage), max(df$lwage), by = 0.2)

tech <- "is_stem"
xs <- c("schooling", "experience", "experience_sq")

# construct empty data frame to save regression coefficients
df_reg <- data.frame(
  year = numeric(length(years) * length(lwage_thresholds)),
  lwage = numeric(length(years) * length(lwage_thresholds))
  )

df_reg <- df_reg %>%
  bind_cols(
    cons = numeric(length(years) * length(lwage_thresholds)),
    !!tech := numeric(length(years) * length(lwage_thresholds))
    )

for (x in xs) {
  df_reg <- df_reg %>%
    bind_cols(!!x := numeric(length(years) * length(lwage_thresholds)))
}

# distribution regression
for (i in seq_along(years)) {
  t <- years[i]
  df_t <- df %>% filter(YEAR == t)
  
  for (j in seq_along(lwage_thresholds)) {
    threshold <- lwage_thresholds[j]
    
    y <- df_t$lwage <= threshold
    reg_formula <- paste0("y ~ ", paste0(c(tech, xs), collapse = " + "))
    reg <- lm(reg_formula, data = df_t, weights = weight)
    
    df_reg[(i - 1) * length(lwage_thresholds) + j, ] <- c(t, threshold, reg$coefficients)
  }
}

# estimate cdf
df_cdf <- data.frame(
  year = numeric(length(years) * length(lwage_thresholds)),
  lwage = numeric(length(years) * length(lwage_thresholds)),
  cdf = numeric(length(years) * length(lwage_thresholds)),
  cdf_tilda = numeric(length(years) * length(lwage_thresholds))
)

# vector t0: use t0's tech, use mean for other control variables
A_0 <- as.matrix(df %>% 
                   filter(YEAR == years[1]) %>% 
                   select(cons, !!tech, !!xs)
                 )

# length of vector t0
N_0 <- nrow(df %>% 
              filter(YEAR == years[1])
            )

# construct counterfactual using A_0
for (i in seq_along(years)) {
  t <- years[i]
  df_t <- df %>% filter(YEAR == t)
  N_t <- nrow(df_t)
  
  for (j in seq_along(lwage_thresholds)) {
    threshold <- lwage_thresholds[j]

    A_t <- as.matrix(df_t %>% select(cons, !!tech, !!xs))    
    B <- t(as.matrix(df_reg[(i - 1) * length(lwage_thresholds) + j, c("cons", tech, xs)]))

    # for control variables, use mean
    df_t_mean <- df %>% 
      filter(YEAR == t) %>%
      summarise_at(xs, mean)
    
    for (x in xs) {
      A_0[, x] <- as.numeric(df_t_mean[1, x])
    }
        
    df_cdf[(i - 1) * length(lwage_thresholds) + j, ] <- c(t, 
                                                          threshold, 
                                                          1 / N_t * sum(A_t %*% B),
                                                          1 / N_0 * sum(A_0 %*% B)
                                                          )
  }
}

# calculate variance
df_cdf2 <- df_cdf %>%
  group_by(year) %>%
  mutate(
    d_cdf = cdf - lag(cdf),
    d_cdf_tilda = cdf_tilda - lag(cdf_tilda)
    ) %>%
  filter(!is.na(d_cdf)) %>%
  summarise(
    e = sum(lwage * d_cdf),
    var = sum(lwage^2 * d_cdf) - sum(lwage * d_cdf)^2,
    var_tilda = sum(lwage^2 * d_cdf_tilda) - sum(lwage * d_cdf_tilda)^2
    )
  
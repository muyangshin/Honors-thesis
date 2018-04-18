library(tidyverse)
library(sqldf)

source("code/tools.R")


# LOAD DATA ---------------------------------------------------------------

# df_0 <- load_df()
df <- df_0 %>%
  mutate(cons = 1) %>%
  
  # restrict to men for now
  filter(SEX == 1) %>%
  
  # recalculate weight
  group_by(YEAR) %>%
  mutate(weight = weight / sum(weight)) %>%
  ungroup()


# ANALYSIS ----------------------------------------------------------------

years <- c(2002, 2017)
lwage_thresholds <- seq(min(df$lwage), max(df$lwage), by = 2)

tech <- "is_stem_related"
xs <- c("schooling", "experience", "experience_sq", "married")

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
    mutate_at(c("cons", tech, xs), funs(. * weight)) %>%
    select(cons, !!tech, !!xs) %>% 
    summarise_all(sum)
  
  A_tech <- A_t %>% 
    mutate(
      !!tech := t(as.matrix(df[df$YEAR == years[1], tech])) %*% as.matrix(df[df$YEAR == years[1], "weight"])
      )
  A_cont <- as.matrix(A_tech)
  A_cont[3:(2 + length(xs))] <- as.matrix(df %>% 
                                            filter(YEAR == years[1]) %>%
                                            mutate_at(xs, funs(. * weight)) %>%
                                            select(!!xs) %>% 
                                            summarise_all(funs(sum)))
  
  A_t <- as.matrix(A_t)
  A_tech <- as.matrix(A_tech)
  A_cont <- as.matrix(A_cont)
  
  for (j in seq_along(lwage_thresholds)) {
    threshold <- lwage_thresholds[j]

    B <- t(as.matrix(df_reg[(i - 1) * length(lwage_thresholds) + j, c("cons", tech, xs)]))
    
    df_cdf[(i - 1) * length(lwage_thresholds) + j, ] <- c(t,
                                                          threshold,
                                                          A_t %*% B,
                                                          A_tech %*% B,
                                                          A_cont %*% B
                                                          )
  }
}

# calculate variance
df_cdf2 <- df_cdf %>%
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
df_results <- data.frame(matrix(ncol = 5, nrow = 2))
colnames(df_results) <- c("name", "total", "tech", "control", "unexplained")

df_results[1, ] <- list("Std Dev", 
                        df_cdf2[2, "sd"] - df_cdf2[1, "sd"],
                        df_cdf2[2, "sd"] - df_cdf2[2, "sd_tech"],
                        df_cdf2[2, "sd_tech"] - df_cdf2[2, "sd_cont"],
                        df_cdf2[2, "sd_cont"] - df_cdf2[1, "sd"]
)

df_results[2, ] <- list("pct", 
                        100,
                        100 * (df_cdf2[2, "sd"] - df_cdf2[2, "sd_tech"]) / (df_cdf2[2, "sd"] - df_cdf2[1, "sd"]),
                        100 * (df_cdf2[2, "sd_tech"] - df_cdf2[2, "sd_cont"]) / (df_cdf2[2, "sd"] - df_cdf2[1, "sd"]),
                        100 * (df_cdf2[2, "sd_cont"] - df_cdf2[1, "sd"]) / (df_cdf2[2, "sd"] - df_cdf2[1, "sd"])
)

View(df_results)

df_summary <- df %>% 
  filter(YEAR %in% years) %>%
  group_by(YEAR) %>%
  select(weight, !!tech, !!xs) %>%
  mutate_at(c(tech, xs), funs(. * weight)) %>%
  summarise_all(sum)
library(tidyverse)
library(sqldf)

source("code/tools.R")


# LOAD DATA ---------------------------------------------------------------

df_0 <- load_df()
df <- df_0


# ANALYSIS ----------------------------------------------------------------

wage_thresholds <- seq(min(df$lwage), max(df$lwage), by = 0.2)
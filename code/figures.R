library(acid)  # weighted inequality measures
library(tidyverse)
library(stargazer)
library(cowplot)
library(Hmisc)  # weighted mean, var

source("code/tools.R")


# Load data ---------------------------------------------------------------

df_aces <- load_df_aces()


# 2002 vs 2016 ------------------------------------------------------------
# cdf
df_cdf_observed <- as.data.frame(wtd.Ecdf(df_t1$lwage, df_t1$weight, normwt = T)) %>%
  mutate(year = years[1]) %>%
  bind_rows(as.data.frame(wtd.Ecdf(df_t2$lwage, df_t2$weight, normwt = T)) %>%
              mutate(year = years[2])) %>%
  mutate(year = factor(year))

ggplot(df_cdf_observed, aes(x, ecdf, color = year)) +
  geom_line()


# CPS summary statistics --------------------------------------------------
df_aces_summary <- df_aces %>%
  group_by(YEAR, SEX) %>%
  mutate(
    min_wage = sprintf("%.3f", log(min_wage * 100 / gdp_def)),
    nonwhite = RACE != "White"
  ) %>%
  summarise(
    min_wage = first(min_wage),
    lwage = sprintf("%.3f", weighted.mean(lwage, weight)),
    high_tech = sprintf("%.3f", weighted.mean(high_tech, weight)),
    nonwhite = sprintf("%.3f", weighted.mean(nonwhite, weight)),
    age = sprintf("%.2f", weighted.mean(AGE, weight)),
    schooling = sprintf("%.2f", weighted.mean(schooling, weight)),
    experience = sprintf("%.2f", weighted.mean(experience, weight)),
    n = n()
  ) %>%
  arrange(SEX, YEAR)

stargazer(df_aces_summary %>%
            select(YEAR, min_wage, lwage, high_tech, nonwhite, schooling, experience, n) %>%
            setNames(c("Year", "Minimum Real Log Wage", "Real Log Wage", "Tech Worker", "Nonwhite", "Age", "Education", 
                       "Experience", "Number of Observations")),
          title = "Sample Means from the CPS ACES 2002-2016",
          label = "tab:tab_aces_summary",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/aces_summary.tex"
)


# Measures of Wage Inequality ---------------------------------------------
df_wage_ineq_over_time <- df_aces %>%
  # recalculate weights
  group_by(YEAR, SEX) %>%
  mutate(weight = weight / sum(weight)) %>%
  ungroup() %>%
  
  # measures
  group_by(YEAR, SEX) %>%
  summarise(
    sd = sqrt(wtd.var(lwage, weight, normwt = T)),
    q5 = wtd.quantile(lwage, weight, probs = c(0.05), normwt = T),
    q10 = wtd.quantile(lwage, weight, probs = c(0.1), normwt = T),
    q25 = wtd.quantile(lwage, weight, probs = c(0.25), normwt = T),
    q50 = wtd.quantile(lwage, weight, probs = c(0.5), normwt = T),
    q75 = wtd.quantile(lwage, weight, probs = c(0.75), normwt = T),
    q90 = wtd.quantile(lwage, weight, probs = c(0.90), normwt = T),
    q95 = wtd.quantile(lwage, weight, probs = c(0.95), normwt = T),
    gini = weighted.gini(wage, weight)[[1]]
  ) %>%
  
  # diff btwn percentiles
  mutate(
    "10-90" = q90 - q10,
    "10-50" = q50 - q10,
    "50-90" = q90 - q50,
    "25-75" = q75 - q25,
    "5-95" = q95 - q5
  ) %>%
  arrange(YEAR, SEX) %>%
  dplyr::select(YEAR, sd, "10-90", "10-50", "50-90", "25-75", "5-95", gini)

stargazer(df_wage_ineq_over_time %>%
            mutate_at(vars(-YEAR), sprintf, fmt = "%.3f") %>%
            rename(Year = YEAR, 
                   "Standard Deviation of Log Wage" = sd,
                   "Gini Coefficient" = gini),
          title = "Measures of Wage Inequality",
          label = "tab:tab_wage_ineq_measures",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/wage_ineq_measures.tex"
)


# List of Tech occupations --------------------------------------------------------

df_tech_occupations <- read.csv("data/SOC/soc2010_tech.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

stargazer(df_tech_occupations %>%
            setNames(c("Type", "SOC", "Occupation Title")),
          title = "Technology Occupations",
          label = "tab:tab_tech_occ",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/tech_occ.tex"
)



# List of top/bottom tech industry ------------------------------------------------

naics.def.2012 <- read.csv("data/NAICS/naics2012_titles.csv", stringsAsFactors = F, fileEncoding = "UTF-8") %>%
  setNames(c("no", "NAICS", "title")) %>%
  dplyr::select(-no) %>%
  mutate(NAICS = ifelse(nchar(NAICS) == 3, paste0(NAICS, "000"),
                        ifelse(nchar(NAICS) == 4, paste0(NAICS, "00"),
                               NAICS)))

df_industry <- read.csv("data_out/df_industry.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

df_industry_top_tech_2016 <- df_industry %>%
  filter(year == 2016) %>%
  top_n(10, tech) %>%
  arrange(-tech) %>%
  dplyr::select(NAICS, tech) %>%
  mutate(NAICS = as.character(NAICS)) %>%
  left_join(naics.def.2012, by = "NAICS") %>%
  dplyr::select(NAICS, title, tech) %>%
  mutate(tech = sprintf("%.3f", tech)) %>%
  setNames(c("NAICS", "Title", "Tech"))

df_industry_bottom_tech_2016 <- df_industry %>%
  filter(year == 2016) %>%
  top_n(5, desc(tech)) %>%
  arrange(tech) %>%
  dplyr::select(NAICS, tech) %>%
  mutate(NAICS = as.character(NAICS)) %>%
  left_join(naics.def.2012, by = "NAICS") %>%
  dplyr::select(NAICS, title, tech) %>%
  mutate(tech = sprintf("%.3f", tech)) %>%
  setNames(c("NAICS", "Title", "Tech"))

stargazer(df_industry_top_tech_2016 %>% bind_rows(df_industry_bottom_tech_2016),
          title = "Top and Bottom Technology Industry Sectors in 2016",
          label = "tab:tab_top_bottom_tech_ind",
          type = "latex", summary = F, rownames = F, header = F,
          out = "tex/top_bottom_tech_ind.tex")


# share of tech workers - industry approach -------------------------------

stargazer(df_industry %>%
            group_by(year) %>%
            mutate(
              employment_tech = employment * tech,
              wage_total = wage * employment,
              wage_total_tech = wage_total * tech
              ) %>%
            summarise(
              employment_share = sprintf("%.2f", sum(employment_tech) / sum(employment)),
              wage_share = sprintf("%.2f", sum(wage_total_tech) / sum(wage_total))
              ) %>%
            setNames(c("Year", "Employment Share", "Wage Share")),
          title = "Employment and Wage Share of Technology Workers",
          label = "tab:tab_industry_tech_share",
          type = "latex", summary = F, rownames = F, header = F,
          out = "tex/industry_tech_share.tex"
)


# industry: by wage group -------------------------------------------------

# number of sectors in 2016
num_sectors <- nrow(df_industry %>% filter(year == 2016))

df_industry_by_year_group <- df_industry %>%
  group_by(year) %>%
  mutate(rank = rank(wage)) %>%
  mutate(group = ifelse(rank > num_sectors - num_sectors / 10, "Top 10%", 
                        ifelse(rank <= num_sectors / 2, "Bottom 50%", "Middle 40%"))) %>%
  group_by(year, group) %>%
  summarise(wage = mean(wage), tech = mean(tech), employment = mean(employment))

df_industry_by_year_group$group <- factor(df_industry_by_year_group$group)
df_industry_by_year_group$group <- factor(df_industry_by_year_group$group, 
                                               levels = rev(levels(df_industry_by_year_group$group)))

df_industry_by_year_group_2002_2016 <- df_industry_by_year_group %>%
  filter(year %in% c(2002, 2016)) %>%
  group_by(group) %>%
  summarise(
    wage.growth = last(wage) / first(wage) - 1,
    tech.growth = last(tech) - first(tech)
    ) %>%
  bind_rows(., df_industry %>%
              filter(year %in% c(2002, 2016)) %>%
              group_by(year) %>%
              summarise(wage = mean(wage),
                        tech = mean(tech)) %>%
              ungroup() %>%
              summarise(wage.growth = last(wage) / first(wage) - 1,
                        tech.growth = last(tech) - first(tech)
                        ) %>%
              mutate(group = "Full Sectors")
  ) %>%
  arrange(-row_number())

stargazer(df_industry_by_year_group_2002_2016 %>%
            mutate(wage.growth = sprintf("%.2f%%", wage.growth * 100),
                   tech.growth = sprintf("%.2f", tech.growth * 100)
            ) %>%
            setNames(c("Wage group", "Wage (%)", "Technology Workers (pp)")),
          title = "The Growth of Wage and Share of Technology Workers in 2002-2016",
          label = "tab:tab_industry_trends_by_group",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/industry_trends_by_group.tex")


# plot
theme_set(theme_cowplot(font_size = 10)) # reduce default font size

plot.top.bottom.wage <- ggplot(df_industry_by_year_group, aes(x = year, y = wage, colour = group, shape = group)) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Year") +
  ylab("Average Wage") +
  scale_x_continuous(limits = c(2002, 2016), breaks = seq(2002, 2016, by = 2)) +
  scale_y_continuous(limits = c(0, 150000), breaks = seq(0, 150000, by = 30000), expand = c(0, 0)) +
  theme(legend.title = element_blank(), panel.grid.major = element_line(colour = "grey"), legend.position = "bottom")

plot.top.bottom.tech <- ggplot(df_industry_by_year_group, aes(x = year, y = tech, colour = group, shape = group)) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Year") +
  ylab("Share of Technology Workers") +
  scale_x_continuous(limits = c(2002, 2016), breaks = seq(2002, 2016, by = 2)) +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.05), expand = c(0, 0)) +
  theme(legend.title = element_blank(), panel.grid.major = element_line(colour = "grey"), legend.position = "bottom")

plot_grid(plot.top.bottom.wage, plot.top.bottom.tech, ncol = 2, align = "v")

ggsave("png/industry_trends_by_year_group.png", height = 4)


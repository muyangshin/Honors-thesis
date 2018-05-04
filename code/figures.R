library(acid)  # weighted inequality measures
library(tidyverse)
library(stargazer)
library(cowplot)
library(Hmisc)  # weighted mean, var
library(survey)

source("code/tools.R")


# Load data ---------------------------------------------------------------

years <- c(1996, 2017)

dfw <- svydesign(~1, data = load_df_org(paste0("YEAR >=", years[1])), weights = ~weight)

dfw_t1 <- subset(dfw, YEAR == years[1])
dfw_t2 <- subset(dfw, YEAR == years[2])
dfw_m <- subset(dfw, SEX == 1)

theme_set(theme_cowplot(font_size = 10)) # reduce default font size




# CPS summary statistics --------------------------------------------------
df_org_summary <- dfw$variables %>%
  group_by(YEAR, SEX) %>%
  mutate(
    nonwhite = RACE != "White"
  ) %>%
  summarise(
    lwage = sprintf("%.3f", weighted.mean(lwage, weight)),
    high_tech = sprintf("%.3f", weighted.mean(high_tech, weight)),
    nonwhite = sprintf("%.3f", weighted.mean(nonwhite, weight)),
    age = sprintf("%.2f", weighted.mean(AGE, weight)),
    schooling = sprintf("%.2f", weighted.mean(schooling, weight)),
    experience = sprintf("%.2f", weighted.mean(experience, weight)),
    n = n()
  ) %>%
  arrange(SEX, YEAR)

stargazer(df_org_summary %>%
            filter(SEX == 1) %>%
            select(YEAR, lwage, high_tech, nonwhite, age, schooling, experience, n) %>%
            setNames(c("Year", "Real Log Wage", "Tech Worker", "Nonwhite", "Age", "Education", 
                       "Experience", "Number of Observations")),
          title = "Sample Means from the CPS ORG, 1996-2017, Men",
          label = "tab:tab_org_summary_men",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/org_summary_men.tex"
)

stargazer(df_org_summary %>%
            filter(SEX == 2) %>%
            select(YEAR, lwage, high_tech, nonwhite, age, schooling, experience, n) %>%
            setNames(c("Year", "Real Log Wage", "Tech Worker", "Nonwhite", "Age", "Education", 
                       "Experience", "Number of Observations")),
          title = "Sample Means from the CPS ORG, 1996-2017, Women",
          label = "tab:tab_org_summary_women",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/org_summary_women.tex"
)


# CPS: Measures of Wage Inequality ---------------------------------------------

# * construct measures ------------------------------------------------------

# variance
df_wage_ineq_over_time_var <- svyby(~lwage, ~YEAR+SEX, dfw, svyvar, estimate.only = T) %>%
  mutate(sd = sqrt(lwage)) %>%
  select(YEAR, SEX, sd)

# quantiles
df_wage_ineq_over_time_quantiles <- svyby(~lwage, ~YEAR+SEX, dfw, svyquantile, keep.var = F,
                                          quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) %>%
  setNames(c("YEAR", "SEX", paste0("q", 100 * c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))))

# gini
df_wage_ineq_over_time_gini <- dfw$variables %>%
  group_by(YEAR, SEX) %>%
  summarise(gini = weighted.gini(wage, weight)[[1]])

# merge into one df
df_wage_ineq_over_time <- df_wage_ineq_over_time_var %>%
  left_join(df_wage_ineq_over_time_quantiles, by = c("YEAR", "SEX")) %>%
  left_join(df_wage_ineq_over_time_gini, by = c("YEAR", "SEX")) %>%
  
  # diff btwn percentiles
  mutate(
    "d10_90" = q90 - q10,
    "d10_50" = q50 - q10,
    "d50_90" = q90 - q50,
    "d25_75" = q75 - q25,
    "d5_95" = q95 - q5
  ) %>%
  arrange(YEAR, SEX) %>%
  dplyr::select(YEAR, SEX, sd, "d10_90", "d10_50", "d50_90", "d25_75", "d5_95", gini)

# remove intermediary dfs
rm(df_wage_ineq_over_time_var, df_wage_ineq_over_time_quantiles, df_wage_ineq_over_time_gini)


# * write as table --------------------------------------------------------

stargazer(df_wage_ineq_over_time %>%
            filter(SEX == 1) %>%
            select(-SEX) %>%
            mutate_at(vars(-YEAR), sprintf, fmt = "%.3f") %>%
            mutate(YEAR = as.character(YEAR)) %>%
            setNames(c("Year", "Standard Deviation of Log Wage", 
                       "90-10", "50-10", "90-50", "75-25", "95-5", "Gini Coefficient")),
          title = "Measures of Wage Inequality, 1992-2017, Men",
          label = "tab:tab_wage_ineq_measures_men",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/wage_ineq_measures_men.tex"
)

stargazer(df_wage_ineq_over_time %>%
            filter(SEX == 2) %>%
            select(-SEX) %>%
            mutate_at(vars(-YEAR), sprintf, fmt = "%.3f") %>%
            mutate(YEAR = as.character(YEAR)) %>%
            setNames(c("Year", "Standard Deviation of Log Wage", 
                       "90-10", "50-10", "90-50", "75-25", "95-5", "Gini Coefficient")),
          title = "Measures of Wage Inequality, 1992-2017, Women",
          label = "tab:tab_wage_ineq_measures_women",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/wage_ineq_measures_women.tex"
)


# * Figure ----------------------------------------------------------------

wanted_vars <- c("sd", "d10_90", "d10_50", "d50_90", "d25_75", "gini")
wanted_vars_names <- c("Standard Deviation", "90-10", "50-10", "90-50", "75-25", "Gini Coefficient")
plots <- vector("list", length(wanted_vars))

for (i in 1:length(wanted_vars)) {
  plots[[i]] <- df_wage_ineq_over_time %>%
    filter(SEX == 1) %>%
    ggplot(aes(YEAR)) +
    geom_line(aes_string(y = wanted_vars[i])) +
    xlab("") + ylab("") + ggtitle(wanted_vars_names[i]) +
    scale_x_continuous(limits = c(1996, 2017), breaks = seq(1996, 2017, by = 5)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_grid(plotlist = plots, ncol = 2, align = "v")
ggsave("png/ineq_measures_by_year.png")



# CPS: Tech vs Non-tech ---------------------------------------------------

# Fraction of tech occupations
df_fraction_tech <- svyby(~high_tech, ~YEAR+SEX, dfw, svymean, estimate.only = T)
df_fraction_tech_total <- svyby(~high_tech, ~YEAR, dfw, svymean, estimate.only = T)

# mean wage: tech vs non-tech occupations
df_mean_tech_nontech <- svyby(~wage, ~high_tech+YEAR+SEX, dfw, svymean, estimate.only = T)
df_mean_tech_nontech_total <- svyby(~wage, ~high_tech+YEAR, dfw, svymean, estimate.only = T)

# plots
plot_fraction_tech <- df_fraction_tech %>%
  bind_rows(df_fraction_tech_total %>%
              mutate(SEX = -1)) %>%
  mutate(SEX = factor(SEX, levels = c(-1, 1, 2), labels = c("Both", "Male", "Female"))) %>%
  ggplot(aes(YEAR, high_tech, color = SEX)) +
  geom_line() +
  geom_point(aes(shape = SEX), size = 3) +
  theme_bw() +
  xlab("Year") + ylab("") + #ylab("Share of Technology Workers") +
  ggtitle("Share of Technology Workers") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom")

plot_wage_gap_tech_nontech <- df_mean_tech_nontech %>%
  bind_rows(df_mean_tech_nontech_total %>%
              mutate(SEX = -1)) %>%
  mutate(SEX = factor(SEX, levels = c(-1, 1, 2), labels = c("Both", "Male", "Female"))) %>%
  group_by(YEAR, SEX) %>%
  summarise(diff = last(wage) - first(wage)) %>%
  ggplot(aes(YEAR, diff, color = SEX)) +
  geom_line() +
  geom_point(aes(shape = SEX), size = 3) +
  theme_bw() +
  xlab("Year") + ylab("") + #ylab("Difference in Average Hourly Wage")
  ggtitle("Difference in Average Hourly Wage") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "bottom")

plot_grid(plot_fraction_tech, plot_wage_gap_tech_nontech, cols = 2)
ggsave("png/cps_tech_trend.png", height = 4)  


# CPS: by wage group ------------------------------------------------------

# determine wage group
df_cps_wage_quantiles <- svyby(~lwage, ~YEAR, dfw_m, svyquantile, keep.var = F,
                               quantiles = c(0.5, 0.9))

df_cps_wage_quantiles2 <- df_cps_wage_quantiles %>%
  setNames(c("YEAR", "q0.5", "q0.9")) %>%
  right_join(dfw_m$variables %>% select(YEAR),
            by = "YEAR")

dfw_m <- update(dfw_m,
                wage_group = ifelse(lwage <= df_cps_wage_quantiles2$q0.5, "Bottom 50%", 
                                    ifelse(lwage <= df_cps_wage_quantiles2$q0.9, "Middle 40%", "Top 10%")))

df_cps_wage_group_mean_wage <- svyby(~wage, ~YEAR+wage_group, dfw_m, svymean, estimate.only = T)
df_cps_wage_group_mean_high_tech <- svyby(~high_tech, ~YEAR+wage_group, dfw_m, svymean, estimate.only = T)

plot_cps_wage_group_mean_wage <- df_cps_wage_group_mean_wage %>%
  mutate(wage_group = factor(wage_group, levels = c("Top 10%", "Middle 40%", "Bottom 50%"))) %>%
  ggplot(aes(YEAR, wage, color = wage_group)) +
  geom_line() +
  geom_point(aes(shape = wage_group), size = 3) +
  theme_bw() +
  xlab("Year") + ylab("") + ggtitle("Average Hourly Wage") +
  theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))

plot_cps_wage_group_mean_high_tech <- df_cps_wage_group_mean_high_tech %>%
  mutate(wage_group = factor(wage_group, levels = c("Top 10%", "Middle 40%", "Bottom 50%"))) %>%
  ggplot(aes(YEAR, high_tech, color = wage_group)) +
  geom_line() +
  geom_point(aes(shape = wage_group), size = 3) +
  theme_bw() +
  xlab("Year") + ylab("") + ggtitle("Share of Technology Workers") +
  theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))

plot_grid(plot_cps_wage_group_mean_wage, plot_cps_wage_group_mean_high_tech)
ggsave("png/cps_wage_group.png", height = 4)


# CPS: by tech educ group -------------------------------------------------

dfw_m <- update(dfw_m, 
                tech_educ_group = ifelse(schooling <= 12, ifelse(high_tech == 1, "HS-Tech", "HS-NonTech"),
                                         ifelse(high_tech == 1, "College-Tech", "College-NonTech")),
                is_HS_Tech = ifelse(tech_educ_group == "HS-Tech", 1, 0),
                is_HS_NonTech = ifelse(tech_educ_group == "HS-NonTech", 1, 0),
                is_College_Tech = ifelse(tech_educ_group == "College-Tech", 1, 0),
                is_College_NonTech = ifelse(tech_educ_group == "College-NonTech", 1, 0)
)

df_cps_tech_educ_group_mean_wage <- svyby(~wage, ~tech_educ_group + YEAR + SEX, dfw_m, svymean, estimate.only = T)
df_cps_tech_educ_group_fraction <- svyby(~is_HS_Tech + is_HS_NonTech + is_College_Tech + is_College_NonTech,
                                         ~YEAR + SEX, dfw_m, svymean, estimate.only = T)

plot_cps_tech_educ_group_mean_wage <- df_cps_tech_educ_group_mean_wage %>%
  mutate(tech_educ_group = factor(tech_educ_group,
                                  levels = c("College-Tech", "HS-Tech", "College-NonTech", "HS-NonTech"))) %>%
  ggplot(aes(YEAR, wage, color = tech_educ_group)) +
  geom_line() +
  geom_point(aes(shape = tech_educ_group), size = 3) +
  theme_bw() +
  xlab("Year") + ylab("") + ggtitle("Average Hourly Wage") +
  theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

plot_cps_tech_educ_group_fraction <- df_cps_tech_educ_group_fraction %>%
  select(YEAR, is_HS_Tech, is_HS_NonTech, is_College_Tech, is_College_NonTech) %>%
  gather("name", "value", 2:5) %>%
  mutate(name = factor(name, levels = c("is_College_Tech", "is_HS_Tech", "is_College_NonTech", "is_HS_NonTech"),
                       labels = c("College-Tech", "HS-Tech", "College-NonTech", "HS-NonTech"))) %>%
  ggplot(aes(YEAR, value, color = name)) +
  geom_line() +
  geom_point(aes(shape = name), size = 3) +
  theme_bw() +
  xlab("Year") + ylab("") + ggtitle("Composition") +
  theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

plot_grid(plot_cps_tech_educ_group_mean_wage, plot_cps_tech_educ_group_fraction, ncol = 2)
ggsave("png/cps_tech_educ_group.png", height = 4)


# List of Tech occupations --------------------------------------------------------

df_tech_occupations <- read.csv("data/SOC/soc2010_tech.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

stargazer(df_tech_occupations %>%
            setNames(c("Type", "SOC", "Occupation Title")),
          title = "Technology Occupations",
          label = "tab:tab_tech_occ",
          type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
          out = "tex/tech_occ.tex"
)



# OES: List of top/bottom tech industry ------------------------------------------------

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


# QCEW: share of tech workers -------------------------------

df_industry %>%
  group_by(year) %>%
  mutate(
    employment_tech = employment * tech,
    wage_total = wage * employment,
    wage_total_tech = wage_total * tech
  ) %>%
  summarise(
    employment_share = sum(employment_tech) / sum(employment),
    wage_share = sum(wage_total_tech) / sum(wage_total)
  ) %>%
  setNames(c("Year", "Employment Share", "Wage Share")) %>%
  gather("name", "value", 2:3) %>%
  mutate(name = factor(name,
                       levels(factor(name)) [c(2, 1)])) %>%
  ggplot(aes(Year, value, color = name)) +
  geom_line() +
  geom_point(aes(shape = name), size = 3) +
  xlab("Year") + ylab("Share") +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave("png/industry_tech_wage_employment_share.png")


# QCEW: by wage group -------------------------------------------------

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

# stargazer(df_industry_by_year_group_2002_2016 %>%
#             mutate(wage.growth = sprintf("%.1f%%", wage.growth * 100),
#                    tech.growth = sprintf("%.3f", tech.growth * 100)
#             ) %>%
#             setNames(c("Wage group", "Wage (%)", "Share of Technology Workers (pp)")),
#           title = "The Growth of Wage and Share of Technology Workers, 2002-2016",
#           label = "tab:tab_industry_trends_by_group",
#           type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
#           out = "tex/industry_trends_by_group.tex")


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


# ANALYSIS: decomposition result ------------------------------------------

tech_vars <- c("high_tech", "tech_group", "stem", "stem_related")
tech_vars_name <- c("High-Tech", "Tech Group", "STEM", "STEM-related")

for (i in 1:length(tech_vars)) {
  tech <- tech_vars[i]
  df_decompose <- read.csv(paste0("data_out/analysis/", tech, "_small/df_decompose.csv"), stringsAsFactors = F, fileEncoding = "UTF-8") %>%
    mutate(
      name = ifelse(name == "pct", "", name),
      total = ifelse(total == 100, NA, total)
    ) %>%
    setNames(c("Statistic", "Total Change", "Wage Structure: Technology", "Wage Structure: Others", 
               "Composition: Technology", "Composition: Others"))
  
  stargazer(df_decompose,
            title = paste0("Decomposing Changes in Measures of Wage Dispersion, Using ", tech_vars_name[i], " Occupations"),
            label = paste0("tab:tab_decompose_", tech),
            type = "latex", summary = FALSE,  rownames = FALSE, header = FALSE,
            out = paste0("tex/decompose_", tech, ".tex")
  )
}



# ANALYSIS: figures -------------------------------------------------------

# empirical cdf
cdf_t1 <- svycdf(~lwage, dfw_t1)
cdf_t2 <- svycdf(~lwage, dfw_t2)

# load df_cdf
df_cdf <- read.csv("data_out/analysis/high_tech_small/df_cdf.csv", fileEncoding = "UTF-8") %>%
  mutate(cdf_t1 = cdf_t1$lwage(lwage),
         cdf_t2 = cdf_t2$lwage(lwage)) %>%
  filter(lwage > 0.5, lwage < 5)

plot_analysis_observed <- df_cdf %>%
  select(lwage, cdf_t1, cdf_t2) %>%
  gather("name", "value", 2:3) %>%
  mutate(name = factor(ifelse(name == "cdf_t1", 1996, 2017))) %>%
  ggplot(aes(lwage, value, color = name)) +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("CDFs") + ggtitle("Observed CDFs") +
  theme(legend.title = element_blank(),
        legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)
)

plot_analysis_diff_t2_t1 <- df_cdf %>%
  mutate(diff = cdf_t2 - cdf_t1) %>%
  ggplot(aes(lwage, diff)) +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("") + ggtitle("Observed differences") +
  scale_y_continuous(limits = c(-0.3, 0.15), breaks = seq(-0.3, 0.15, by = 0.07), expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

plot_analysis_tech_ws <- df_cdf %>%
  mutate(diff = cdf_t2 - cdf_tech_ws) %>%
  ggplot(aes(lwage, diff)) +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("") + ggtitle("Wage Structure: Technology") +
  scale_y_continuous(limits = c(-0.3, 0.15), breaks = seq(-0.3, 0.15, by = 0.07), expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

plot_analysis_cont_ws <- df_cdf %>%
  mutate(diff = cdf_tech_ws - cdf_cont_ws) %>%
  ggplot(aes(lwage, diff)) +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("") + ggtitle("Wage Structure: Others") +
  scale_y_continuous(limits = c(-0.3, 0.15), breaks = seq(-0.3, 0.15, by = 0.07), expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

plot_analysis_tech_comp <- df_cdf %>%
  mutate(diff = cdf_cont_ws - cdf_tech_comp) %>%
  ggplot(aes(lwage, diff)) +
  geom_line() +
  theme_bw() +
  xlab("Log real wage") + ylab("") + ggtitle("Composition: Technology") +
  scale_y_continuous(limits = c(-0.3, 0.15), breaks = seq(-0.3, 0.15, by = 0.07), expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

plot_analysis_cont_comp <- df_cdf %>%
  mutate(diff = cdf_tech_comp - cdf_t1) %>%
  ggplot(aes(lwage, diff)) +
  geom_line() +
  theme_bw() +
  xlab("Log real wage") + ylab("") + ggtitle("Composition: Others") +
  scale_y_continuous(limits = c(-0.3, 0.15), breaks = seq(-0.3, 0.15, by = 0.07), expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5))

plot_grid(plot_analysis_observed, plot_analysis_diff_t2_t1, plot_analysis_tech_ws, plot_analysis_cont_ws,
          plot_analysis_tech_comp, plot_analysis_cont_comp,
          ncol = 2)
ggsave("png/analysis.png")


# miscs -----------------------------------------------------------------

# compare observed vs tech cdfs
df_cdf_tech_only %>% 
  dplyr::select(lwage, cdf, cdf_tech) %>% 
  gather("name", "value", 2:3) %>% 
  ggplot(aes(lwage, value, color = name)) +
  geom_line()



# between educated, wage distribution by high_tech
ggplot(df_t1(), aes(x = lwage, color = high_tech)) + geom_histogram(alpha = .3)
ggplot(df_t2(), aes(x = lwage, color = high_tech)) + geom_density()

# by wage group
df_by_wage_group <- df_org %>%
  group_by(YEAR) %>%
  mutate(rank = rank(lwage),
         n = n()) %>%
  mutate(group = ifelse(rank > n - n / 10, "Top 10%", 
                        ifelse(rank <= n / 2, "Bottom 50%", "Middle 40%"))) %>%
  group_by(YEAR, group) %>%
  summarise(wage = mean(wage), high_tech = mean(high_tech))

# plot
theme_set(theme_cowplot(font_size = 10)) # reduce default font size

plot.top.bottom.wage <- ggplot(df_by_wage_group, aes(x = YEAR, y = wage, colour = group, shape = group)) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Year") +
  ylab("Average Wage") +
  scale_x_continuous(limits = c(1992, 2017), breaks = seq(1992, 2017, by = 2)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10), expand = c(0, 0)) +
  theme(legend.title = element_blank(), panel.grid.major = element_line(colour = "grey"), legend.position = "bottom")

plot.top.bottom.tech <- ggplot(df_by_wage_group, aes(x = YEAR, y = high_tech, colour = group, shape = group)) +
  geom_line() +
  geom_point(size = 3) +
  xlab("Year") +
  ylab("Share of Technology Workers") +
  scale_x_continuous(limits = c(1992, 2017), breaks = seq(1992, 2017, by = 2)) +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.05), expand = c(0, 0)) +
  theme(legend.title = element_blank(), panel.grid.major = element_line(colour = "grey"), legend.position = "bottom")

plot_grid(plot.top.bottom.wage, plot.top.bottom.tech, ncol = 2, align = "v")

ggsave("png/wage_tech_trends_by_wage_group.png", height = 4)
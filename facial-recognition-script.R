
rm(list = ls())

library(readxl)
library(here)
library(lubridate)
library(ggplot2)
library(pscl)
library(sjPlot)
library(ggpubr)
library(dplyr)

# Read deployments sheet, skipping the first 3 lines
df <- read_excel(here("data/Live Facial Recognition Deployments.xlsx"),
                 sheet = "Deployment Data",
                 skip = 3)

# Remove empty rows
df <- df %>%
  filter(!is.na(Year))

# Convert to total minutes within dplyr
df <- df %>%
  mutate(
    duration_format = ymd_hms(Duration),
    total_minutes = hour(duration_format) * 60 + minute(duration_format)
  )

# Count deployments in wards
wards <- df %>%
  group_by(`Ward Code`) %>%
  mutate(`Faces seen` = as.numeric(`Faces seen`)) %>%
  summarise(
    total_deployments = n(),
    total_minutes = sum(total_minutes, na.rm = TRUE),
    total_faces = sum(`Faces seen`, na.rm = TRUE),
    .groups = "drop"
  )

wards2023 <- df %>%
  filter(Year < 2024) %>%
  group_by(`Ward Code`) %>%
  mutate(`Faces seen` = as.numeric(`Faces seen`)) %>%
  summarise(
    total_deployments = n(),
    total_minutes = sum(total_minutes, na.rm = TRUE),
    total_faces = sum(`Faces seen`, na.rm = TRUE),
    .groups = "drop"
  )

wards2024 <- df %>%
  filter(Year == 2024) %>%
  group_by(`Ward Code`, `Ward (approx)`) %>%
  mutate(`Faces seen` = as.numeric(`Faces seen`)) %>%
  summarise(
    total_deployments = n(),
    total_minutes = sum(total_minutes, na.rm = TRUE),
    total_faces = sum(`Faces seen`, na.rm = TRUE),
    .groups = "drop"
  )

wards2025 <- df %>%
  filter(Year == 2025) %>%
  group_by(`Ward Code`, `Ward (approx)`) %>%
  mutate(`Faces seen` = as.numeric(`Faces seen`)) %>%
  summarise(
    total_deployments = n(),
    total_minutes = sum(total_minutes, na.rm = TRUE),
    total_faces = sum(`Faces seen`, na.rm = TRUE),
    .groups = "drop"
  )

# Read population groups sheet, skipping the first 3 lines
census <- read_excel(here("data/Live Facial Recognition Deployments.xlsx"),
                 sheet = "Census data",
                 skip = 1)

# Create database with Black percentage
wards_black <- census %>%
  mutate(nonwhite_percentage = 1 - (`White British` / `All usual residents`)) %>%
  dplyr::select(`ward code`, `ward name`, `Black Percentage`, nonwhite_percentage)

# Read crime data
crime <- read.csv(here('data/MPS Ward Level Crime (most recent 24 months).csv'))

# Create valid month column names
years <- 2023:2025
months <- sprintf("%02d", 1:12)
valid_ym <- as.vector(outer(years, months, paste0))
valid_ym <- valid_ym[valid_ym >= "202307" & valid_ym <= "202506"]
month_cols <- paste0("X", valid_ym)

# Summarise by Ward
crime_totals <- crime %>%
  group_by(WardCode, WardName) %>%
  summarise(
    total_crimes = sum(across(all_of(month_cols), as.numeric), na.rm = TRUE),
    .groups = "drop"
  )


# Load workday population
#workday <- read.csv(here('data/WD002_oa.csv'))
#workday <- workday %>%
#  group_by(Output.Areas.Code) %>%
#  summarise(wd_pop = sum(Count))
#oa_to_ward <- read.csv(here('data/Output_Area_(2021)_to_Ward_(2024)_to_LAD_(May_2024)_Best_Fit_Lookup_in_EW.csv'))
#workday <- workday %>%
#  left_join(oa_to_ward, by = c("Output.Areas.Code" = "OA21CD"))
#workday <- workday %>%
#  group_by(WD24CD) %>%
#  summarise(wd_pop = sum(wd_pop)) 
#write.csv(workday, here("data/workday.csv"))
workday <- read.csv(here('data/workday.csv'))
workday <- workday %>%
  dplyr::select(-X)

# Create data of Wards
#wards_all <- crime_totals %>%
#  left_join(wards, by = c("WardName" = "Ward (approx)")) %>%
#  left_join(wards_black, by = c("WardName" = "ward name"))

wards_all <- crime_totals %>%
  left_join(wards, by = c("WardCode" = "Ward Code")) %>%
  left_join(workday, by = c("WardCode" = "WD24CD")) %>%
  left_join(wards_black, by = c("WardCode" = "ward code"))

wards2023 <- crime_totals %>%
  left_join(wards2023, by = c("WardCode" = "Ward Code")) %>%
  left_join(workday, by = c("WardCode" = "WD24CD")) %>%
  left_join(wards_black, by = c("WardCode" = "ward code"))

wards2024 <- crime_totals %>%
  left_join(wards2024, by = c("WardCode" = "Ward Code")) %>%
  left_join(workday, by = c("WardCode" = "WD24CD")) %>%
  left_join(wards_black, by = c("WardCode" = "ward code"))

wards2025 <- crime_totals %>%
  left_join(wards2025, by = c("WardCode" = "Ward Code")) %>%
  left_join(workday, by = c("WardCode" = "WD24CD")) %>%
  left_join(wards_black, by = c("WardCode" = "ward code"))

# Fill NAs with 0s in facial recognition deployment
wards_all <- wards_all %>%
  mutate(total_deployments = ifelse(is.na(total_deployments), 0, total_deployments),
         total_minutes = ifelse(is.na(total_minutes), 0, total_minutes),
         total_faces = ifelse(is.na(total_faces), 0 , total_faces))

wards2023 <- wards2023 %>%
  mutate(total_deployments = ifelse(is.na(total_deployments), 0, total_deployments),
         total_minutes = ifelse(is.na(total_minutes), 0, total_minutes),
         total_faces = ifelse(is.na(total_faces), 0 , total_faces))

wards2024 <- wards2024 %>%
  mutate(total_deployments = ifelse(is.na(total_deployments), 0, total_deployments),
         total_minutes = ifelse(is.na(total_minutes), 0, total_minutes),
         total_faces = ifelse(is.na(total_faces), 0 , total_faces))

wards2025 <- wards2025 %>%
  mutate(total_deployments = ifelse(is.na(total_deployments), 0, total_deployments),
         total_minutes = ifelse(is.na(total_minutes), 0, total_minutes),
         total_faces = ifelse(is.na(total_faces), 0 , total_faces))

# Bivariate correlations
cor.test(wards_all$`Black Percentage`, wards_all$total_deployments, method = "spearman")
cor.test(wards_all$total_crimes, wards_all$total_deployments, method = "spearman")

cor.test(wards_all$`Black Percentage`, wards_all$total_minutes, method = "spearman")
cor.test(wards_all$total_crimes, wards_all$total_minutes, method = "spearman")

cor.test(wards_all$`Black Percentage`, wards_all$total_faces, method = "spearman")
cor.test(wards_all$total_crimes, wards_all$total_faces, method = "spearman")

# Scatter plots

# Common plot style function
make_plot <- function(x, y, xlab, ylab, title) {
  ggplot(wards_all, aes(x = {{ x }}, y = {{ y }})) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, colour = "blue") +
    stat_cor(method = "spearman", label.x = 0, label.y = max(wards_all[[deparse(substitute(y))]], na.rm = TRUE)) +
    labs(x = xlab, y = ylab, title = title) +
    theme_classic()
}

# Create all six plots
p1 <- make_plot(`Black Percentage`, total_deployments, "Black %", "Deployments", "Black % vs Deployments")
p2 <- make_plot(total_crimes, total_deployments, "Total Crimes", "Deployments", "Crimes vs Deployments")
p3 <- make_plot(`Black Percentage`, total_minutes, "Black %", "Minutes", "Black % vs Minutes")
p4 <- make_plot(total_crimes, total_minutes, "Total Crimes", "Minutes", "Crimes vs Minutes")
p5 <- make_plot(`Black Percentage`, total_faces, "Black %", "Faces seen", "Black % vs Faces seen")
p6 <- make_plot(total_crimes, total_faces, "Total Crimes", "Faces seen", "Crimes vs Faces seen")

# Arrange plots in a 3x2 grid
ggarrange(p1, p2, p3, p4, p5, p6,
          ncol = 2, nrow = 3,
          labels = LETTERS[1:6])

ggsave(here('scatterplots.png'))

# Standardise variables of interest
wards_all <- wards_all %>%
  mutate(
    black_std = scale(`Black Percentage`),
    nonwhite_std = scale(nonwhite_percentage),
    crime_std = scale(total_crimes),
    wd_pop_std = scale(wd_pop)
  )

wards2023 <- wards2023 %>%
  mutate(
    black_std = scale(`Black Percentage`),
    nonwhite_std = scale(nonwhite_percentage),
    crime_std = scale(total_crimes),
    wd_pop_std = scale(wd_pop)
  )

wards2024 <- wards2024 %>%
  mutate(
    black_std = scale(`Black Percentage`),
    nonwhite_std = scale(nonwhite_percentage),
    crime_std = scale(total_crimes),
    wd_pop_std = scale(wd_pop)
  )

wards2025 <- wards2025 %>%
  mutate(
    black_std = scale(`Black Percentage`),
    nonwhite_std = scale(nonwhite_percentage),
    crime_std = scale(total_crimes),
    wd_pop_std = scale(wd_pop)
  )

# Log transform dependent variables
#wards_all <- wards_all %>%
#  mutate(
#    log_deployments = as.integer(log(total_deployments + 1)),
#    log_minutes = as.integer(log(total_minutes + 1))
#  )

# Check overdispersion
mean(wards_all$total_deployments)
var(wards_all$total_deployments)

mean(wards_all$total_minutes)
var(wards_all$total_minutes)

mean(wards_all$total_faces)
var(wards_all$total_faces)

# Hurdle models for deployments each year
hurdle2023 <- hurdle(
  total_deployments ~ black_std + crime_std + wd_pop_std,
  data = wards2023,
  dist = "negbin"
)

hurdle2024 <- hurdle(
  total_deployments ~ black_std + crime_std + wd_pop_std,
  data = wards2024,
  dist = "negbin"
)

hurdle2025 <- hurdle(
  total_deployments ~ black_std + crime_std + wd_pop_std,
  data = wards2025,
  dist = "negbin"
)

summary(hurdle2023)
summary(hurdle2024)
summary(hurdle2025)

tab_model(hurdle2023, hurdle2024, hurdle2025,
          show.ci = FALSE,
          show.p = TRUE,
          dv.labels = c("2023", "2024", "2025"),
          title = "Hurdle Model Results: Live Facial Recognition Use",
          file = here("LFR_Hurdle_Models_Years.doc"))

# Hurdle models for outcomes
hurdle_deployments <- hurdle(
  total_deployments ~ black_std + crime_std + wd_pop_std,
  data = wards_all,
  dist = "negbin"
)

hurdle_minutes <- hurdle(
  total_minutes ~ black_std + crime_std + wd_pop_std,
  data = wards_all,
  dist = "negbin"
)

hurdle_faces <- hurdle(
  total_faces ~ black_std + crime_std + wd_pop_std,
  data = wards_all,
  dist = "negbin"
)

# Summaries

summary(hurdle_deployments)
summary(hurdle_minutes)
summary(hurdle_faces)

tab_model(hurdle_deployments, hurdle_minutes, hurdle_faces,
          show.ci = FALSE,
          show.p = TRUE,
          dv.labels = c("Deployments", "Minutes", "Faces"),
          title = "Hurdle Model Results: Live Facial Recognition Use",
          file = here("LFR_Hurdle_Models.doc"))

#Summary:
#In this analysis of live facial recognition (LFR) use, we explored whether areas with a higher proportion of Black residents were more likely to be targeted by deployments. 
#We also took into account other local characteristics such as crime levels and working-day population. 
#The findings suggest that LFR was somewhat more likely to be used in areas with more Black residents, but we cannot say with confidence that this pattern is real rather than due to chance. 
#In the areas where LFR was used, there was no clear link between the ethnic makeup of the population and how intensively the technology was deployed—whether measured by the number of operations, their duration, or the number of faces scanned. 
#In contrast, higher crime levels were clearly linked to both the likelihood of LFR being used and the number of faces scanned. 
#Overall, while the analysis points to a possible association between ethnicity and the use of LFR, the evidence is not strong enough to draw firm conclusions.

# Analysis for non whites as sensitivity

# Hurdle models
hurdle_deployments_nw <- hurdle(
  total_deployments ~ nonwhite_std + crime_std + wd_pop_std,
  data = wards_all,
  dist = "negbin"
)

hurdle_minutes_nw <- hurdle(
  total_minutes ~ nonwhite_std + crime_std + wd_pop_std,
  data = wards_all,
  dist = "negbin"
)

hurdle_faces_nw <- hurdle(
  total_faces ~ nonwhite_std + crime_std + wd_pop_std,
  data = wards_all,
  dist = "negbin"
)

# Summaries

summary(hurdle_deployments_nw)
summary(hurdle_minutes_nw)
summary(hurdle_faces_nw)

tab_model(hurdle_deployments_nw, hurdle_minutes_nw, hurdle_faces_nw,
          show.ci = FALSE,
          show.p = TRUE,
          dv.labels = c("Deployments", "Minutes", "Faces"),
          title = "Hurdle Model Results: Live Facial Recognition Use",
          file = here("LFR_Hurdle_Models_nw.doc"))

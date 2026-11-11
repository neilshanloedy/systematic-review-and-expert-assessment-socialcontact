rm(list = ls())

library(rstudioapi)
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

library(dplyr)
library(data.table)
library(ggplot2)
library(rstudioapi)
library(countrycode)
library(tidyverse)
library(janitor)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(forcats)
library(stringr)

# 0. Functions ---------------------------------------------------------------
continent7_lookup <- list(
  "Africa" = c(
    "Cameroon", "Democratic Republic of Congo", "Egypt", "Ethiopia", "Gambia",
    "Ghana", "Guinea", "Ivory Coast", "Kenya", "Liberia", "Malawi", "Morocco",
    "Mozambique", "Nigeria", "Senegal", "Somaliland",
    "South Africa", "Sudan", "Tunisia", "Uganda", "Zambia", "Zimbabwe"
  ),
  "Asia" = c(
    "Cambodia", "China", "Hong Kong", "India", "Japan",
    "Pakistan", "Singapore",
    "South Korea", "Thailand", "Vietnam", "Turkey"
  ),
  "Europe" = c(
    "Austria", "Belgium", "Croatia", "Denmark", "Estonia", "Finland",
    "France", "Germany", "Greece", "Hungary", "Italy",
    "Lithuania", "Luxembourg", "Malta",
    "Netherlands", "Norway", "Poland", "Portugal", "Russia",
    "Slovenia", "Slovakia", "Spain", "Switzerland",
    "United Kingdom"
  ),
  "North America" = c(
    "Canada", "United States", "United States"
  ),
  "South America" = c(
    "Brazil"
  ),
  "Australia and Oceania" = c(
    "Australia", "Fiji"
  ),
  "Antarctica" = c(
    # none in dataset
  )
)


# 1. Load data ---------------------------------------------------------------

df <- read_csv("data/data_systematic_review.csv")

n_of_studies <- n_distinct(df$No)
print(n_of_studies)

n_of_articles <- n_distinct(df$Title)
print(n_of_articles)

n_of_distinct_countries <- n_distinct(df$Country)
print(n_of_distinct_countries)

colnames(df)

# 2. Clean column names ------------------------------------------------------
df <- df %>%
  clean_names()  # snake_case column names

df <- df %>%
  clean_names() %>%
  mutate(country_raw = country) %>%
  mutate(
    country = str_squish(country) # trim whitespaces
)

df <- df %>%
  mutate(
    continent7 = case_when(
      country %in% continent7_lookup$Africa        ~ "Africa",
      country %in% continent7_lookup$Asia          ~ "Asia",
      country %in% continent7_lookup$Europe        ~ "Europe",
      country %in% continent7_lookup$`North America` ~ "North America",
      country %in% continent7_lookup$`South America` ~ "South America",
      country %in% continent7_lookup$`Australia and Oceania` ~ "Oceania",
      country %in% continent7_lookup$Antarctica    ~ "Antarctica",
      TRUE ~ NA_character_
    )
  )

range(df[df$study_design == "Repeated cross sectional",]$year_of_publication)
range(df[df$study_design == "Longitudinal",]$year_of_publication)
range(df[df$study_design == "Cross sectional",]$year_of_publication)

# 3. Visualization
# ....................
continent_colors <- c(
  "Africa"        = "#005f73",
  "Asia"          = "#ae2012",
  "Europe"        = "#94d2bd",
  "North America" = "#e9d8a6",
  "South America" = "#ee9b00",
  "Oceania"       = "#ff0054"
)

df_continent <- df %>%
  dplyr::filter(!is.na(continent7)) %>%
  group_by(continent7, year_of_publication) %>%
  summarise(
    n_studies = n(),
    .groups = "drop"
  ) %>%
  mutate(
    continent7 = factor(continent7,
                        levels = c("Africa", "Asia", "Oceania",
                                   "Europe", "North America", "South America"))
  )

df %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

df %>%
  group_by(year_of_publication) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

df_continent %>%
  group_by(continent7) %>%
  summarise(sum = sum(n_studies))

df %>%
  group_by(year_of_publication, study_design, study_type) %>%
  summarise(n = n()) %>% print(n = 35)

table(df$continent7, df$survey_mode)
xtabs(~ continent7 + survey_mode + year_of_publication, data = df)

p_year_stacked <- df_continent %>%
  ggplot(aes(x = year_of_publication, y = n_studies, fill = continent7)) +
  geom_col(color = "black", width = 0.8) +
  scale_fill_manual(values = continent_colors) +
  scale_x_continuous(
    breaks = seq(min(df_continent$year_of_publication),
                 max(df_continent$year_of_publication),
                 by = 1),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Year of publication",
    y = paste0("Number of surveys (N = ", n_of_studies, ")"),
    fill = "Continent"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14, hjust = 0),
    axis.title      = element_text(size = 13),
    axis.text       = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(color = "black", linewidth = 0.5),
    legend.position    = "top",
    legend.title = element_blank(),
    plot.margin        = margin(10, 10, 10, 10)
  )

p_year_stacked

# .......
# Count studies per country and add ISO3 + continent
df_country <- df %>%
  count(country, name = "n_studies") %>%
  mutate(
    country_clean = case_when(
      country == "Somaliland" ~ "Somalia",
      TRUE ~ country
    ),
    iso3c     = countrycode(country_clean, "country.name", "iso3c")
  )

df_country <- merge(df_country,
                    df %>% select(country, continent7) %>%
                      unique(),
                    by = "country")

# Get world map as sf object
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

world <- world %>%
  mutate(
    iso_a3 = case_when(
      name == "France"     ~ "FRA",
      name == "Norway"     ~ "NOR",
      name == "Somaliland" ~ "SOM",
      TRUE ~ iso_a3
    )
  )

# France = FRA, Norway = NOR, Somaliland = SOM are missing.
# FRANCE = -99, Norway = -99, Somaliland = -99
# Join counts to spatial data
world_data <- world %>%
  left_join(df_country, by = c("iso_a3" = "iso3c"))

# Choropleth map
p_world <- ggplot(world_data) +
  geom_sf(aes(fill = n_studies), color = "black", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "grey95",
    name = "Number of studies"
  ) +
  labs(
    title = "Number of studies per country included in the review"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.1),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin  = margin(10, 10, 10, 10)
  )

p_world

df_continent <- df_country %>%
  filter(!is.na(continent7)) %>%
  group_by(continent7) %>%
  summarise(
    n_studies = sum(n_studies, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    continent7 = factor(continent7,
                        levels = c("Africa", "Asia", "Australia and Oceania",
                                   "Europe", "North America", "South America"))
  )

p_continent <- ggplot(df_continent, aes(x = continent7, y = n_studies)) +
  geom_col(fill = "dodgerblue3", color = "black", linewidth = 0.3, width = 0.7) +
  coord_flip() +
  geom_text(aes(label = n_studies), vjust = 0.0, hjust = -0.2, size = 3) +
  labs(
    x = "Continent",
    y = paste0("Number of studies (N = ", n_of_studies, ")")
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", size = 13, hjust = 0),
    axis.text    = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(color = "black", linewidth = 0.5),
    plot.margin        = margin(10, 3, 10, 10)
  )

p_continent

# Definition of contacts
contacts_colors <- c(
  "Conversational contacts"        = "#005f73",
  "Physical contacts"              = "#ff0054",
  "Physical contacts or proximty-based"        = "#94d2bd",
  "Physical or conversational contacts"        = "#ae2012",
  "Physical or conversational contacts or proximity-based" = "#ee9b00",
  "Proximity-based"                = "#e9d8a6"
)

df_def_contacts <- df %>%
  group_by(definition_of_contacts, year_of_publication) %>%
  summarise(
    n_studies = n(),
    .groups = "drop"
  )

p_year_cnt_stacked <- df_def_contacts %>%
  ggplot(aes(x = year_of_publication, y = n_studies, fill = definition_of_contacts)) +
  geom_col(color = "black", width = 0.8) +
  scale_fill_manual(values = contacts_colors) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Year of publication",
    y = paste0("Number of studies (N = ", n_of_studies, ")"),
    fill = "Continent"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(color = "black", linewidth = 0.5),
    legend.position    = "top",
    legend.title = element_blank(),
    plot.margin        = margin(10, 10, 10, 10)
  )

p_year_cnt_stacked

df_plot <- df %>%
  group_by(year_of_publication) %>%
  count(study_design, study_type) %>%
  mutate(
    text_color = ifelse(n < 10, "white", "black"),
    study_design = factor(study_design,
                          levels = c("Cross sectional", "Longitudinal",
                                     "Repeated cross sectional",
                                     "Longitudinal and repeated cross sectional"))) %>%
  ungroup()

ggplot(df_plot, aes(study_type, study_design, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, color = text_color), size = 4.5) +
  facet_wrap(~ year_of_publication) +
  scale_fill_viridis_c() +
  scale_color_identity() +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 16)) +
  theme_bw(base_size = 14) +
  theme(
    # axis.text.x = element_text(hjust = 1),
    legend.position = "right"
  ) +
  xlab("Reporting approach") +
  ylab("Study design") +
  labs(fill = "# of surveys")

df_plot2 <- df %>%
  group_by(year_of_publication) %>%
  count(survey_mode, survey_tools) %>%
  mutate(
    text_color = ifelse(n < 15, "white", "black")
  ) %>%
  mutate(survey_tools = factor(survey_tools,
                               levels = c("Paper-based", "Non-paper based",
                                          "Combined", "Not reported"),
                               labels = c("Paper", "Non-paper", "Combined",
                                          "Not reported")))

ggplot(df_plot2, aes(survey_tools, survey_mode, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, color = text_color), size = 4.5) +
  facet_wrap(~ year_of_publication) +
  scale_fill_viridis_c() +
  scale_color_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(color = "black", linewidth = 0.5),
    legend.position    = "right",
    plot.margin        = margin(10, 10, 10, 10)
  ) +
  ylab("Survey mode") +
  xlab("Survey tools") +
  labs(fill = "# of surveys")

df <- df %>%
  mutate(survey_tools = factor(survey_tools,
                               levels = c("Paper-based", "Non-paper based",
                                          "Combined", "Not reported")))

# Survey tools and sample size
df %>%
  ggplot(aes(x = survey_tools, y = sample_size)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  stat_summary(
    fun.data = mean_cl_boot,     # bootstrap 95% CI
    geom = "pointrange",
    color = "red",
    size = 0.9
  ) +
  scale_y_log10(labels = scales::comma_format()) +
  theme_bw(base_size = 14) +
  labs(
    y = "Sample size (log scale)",
    x = "Survey tools"
  )

df <- df %>%
  mutate(sampling_design = factor(sampling_design,
                               levels = c("Non-probability sampling",
                                          "Probability sampling",
                                          "Probability and non-probability sampling")))

# probability
survey_colors <- c(
  "Non-probability sampling"        = "#1d3557",
  "Probability sampling"          = "#a8dadc",
  "Probability and non-probability sampling"        = "#e63946"
)

df %>%
  count(year_of_publication, sampling_design) %>%
  ggplot(aes(x = year_of_publication, y = n, fill = sampling_design)) +
  geom_col(color = "black", width = 0.8) +
  scale_fill_manual(values = survey_colors) +
  theme_bw() +
  labs(
  x = "Year of publication",
  y = paste0("Number of studies (N = ", n_of_studies, ")"),
  fill = "sampling_design"
) +
  theme_bw(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13, hjust = 0),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(color = "black", linewidth = 0.5),
    legend.position    = "top",
    legend.title = element_blank(),
    plot.margin        = margin(10, 10, 10, 10)
  )

table(df$survey_level)
table(df$incentives)

df_plot3 <- df %>%
  count(study_setting, representativeness) %>%
  mutate(
    text_color = ifelse(n < 10, "white", "black")) %>%
  ungroup()

ggplot(df_plot3, aes(study_setting, representativeness, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, color = text_color), size = 4.5) +
  scale_fill_viridis_c() +
  scale_color_identity() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(color = "black", linewidth = 0.5),
    legend.position    = "top",
    legend.title = element_blank(),
    plot.margin        = margin(10, 10, 10, 10)
  ) +
  ylab("Representativeness") +
  xlab("Study setting")

table(df$representativeness, df$sampling_design)/sum(table(df$representativeness, df$sampling_design))

# SENSITIVITY ANALYSIS
# ------------------------

get_initiative_distribution <- function(data, variable) {

  data |>
    distinct(initiatives, {{ variable }}) |>
    count({{ variable }}) |>
    mutate(
      N = sum(n),
      percentage = 100 * n / N
    )
}

df <- df |>
  mutate(
    initiatives = case_when(
      title == "Characterising social contacts under COVID-19 control measures in Africa" ~
        "Dobreva initiative",

      title == "Differential impact of physical distancing strategies on social contacts relevant for the spread of SARS-CoV-2: evidence from a cross-national online survey, March-April 2020" ~
        "Del fava initiative",

      str_detect(remarks, regex("CoMix", ignore_case = TRUE)) ~
        "CoMix initiative",

      TRUE ~ title
    )
  )

initiative_df <- df |>
  distinct(
    initiatives,
    continent7,
    survey_level,
    study_design,
    survey_mode,
    sampling_design,
    sampling_stage,
    survey_level,
    study_type,
    definition_of_contacts
  )

get_initiative_distribution(df, continent7)
get_initiative_distribution(df, survey_level)
get_initiative_distribution(df, study_design)
get_initiative_distribution(df, survey_mode)
get_initiative_distribution(df, sampling_design)
get_initiative_distribution(df, sampling_stage)
get_initiative_distribution(df, definition_of_contacts)
get_initiative_distribution(df, study_type)


# Dummy data
dummy_data <- tribble(
  ~item,                 ~category,        ~importance, ~accuracy, ~burden,
  "Age of respondent",   "Demographics",    3,           3,         1,
  "Sex of contact",      "Contact-level",   2,           3,         1,
  "Duration of contact", "Contact-level",   2,           2,         2,
  "Setting of contact",  "Contact-level",   3,           3,         2,
  "Mask use",            "Behavior",        2,           2,         2,
  "Vaccination status",  "Health",          1,           3,         1,
  "Vaccine follow-up",   "Health",          1,           2,         3,
  "Risk perception",     "Psychosocial",    1,           1,         2,
  "Number of contacts",  "Diary-level",     3,           2,         2,
  "Household size",      "Demographics",    3,           3,         1
)

dummy_data <- dummy_data %>%
  mutate(
    x_adj = accuracy + (as.numeric(factor(category)) * 0.05),
    y_adj = burden + (as.numeric(factor(category)) * 0.05)
  )

ggplot(dummy_data, aes(x = x_adj, y = y_adj, color = category)) +
  geom_point(size = 4, alpha = 0.85) +
  geom_text(aes(label = item), hjust = -0.1, vjust = 1, size = 3) +
  scale_x_continuous(breaks = 1:3) +
  scale_y_continuous(breaks = 1:3) +
  facet_wrap(~ importance, nrow = 1) +
  theme_bw() +
  labs(
    x = "Accuracy",
    y = "Burden",
    color = "Category"
  ) +
  theme(legend.position = "top")

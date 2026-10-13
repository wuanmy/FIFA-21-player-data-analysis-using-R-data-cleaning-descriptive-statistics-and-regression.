
# ============================================================
# FIFA 21 DATA ANALYSIS PROJECT
# Data cleaning, descriptive statistics, visualization,
# regression analysis, and model evaluation
# ============================================================

# SECTION 1. LOAD DATA ----------------------------------------
install.packages(c(
  "tidyverse",
  "readxl",
  "janitor",
  "lubridate",
  "broom",
  "scales",
  "dplyr"
))
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(broom)
library(scales)
library(dplyr)

# 1.1 Load the original FIFA 21 dataset
fifa_raw<-read.csv("Data Cleaning Exercise fifa21_raw_data-2 - Copy.csv")


# 1.2 Create a copy for data cleaning

data <- fifa_raw


# SECTION 2. DATA CLEANING -----------------------------------

# 2.1 Remove unwanted whitespace, newlines, and tabs
data <- data %>%
  mutate(
    across(
      where(is.character),
      ~ str_squish(str_replace_all(.x, "[\r\n\t]+", " "))
    )
  )

# 2.2  Convert Height and Weight to numerical forms
data$Weight <- as.numeric(str_remove(data$Weight, "lbs"))

data$Height <- str_remove(data$Height, '\"')

data$Height <- as.numeric(sub(".*'", "", data$Height)) +
  12 * as.numeric(sub("'.*", "", data$Height))

# 2.3 Convert Weight from pounds (lbs) to kilograms (kg)
data$Weight <- data$Weight / 2.20462


# 2.4 Convert Value, Wage, and Release.Clause to numbers

library(readr)

money_to_number <- function(x) {
  x <- str_remove_all(x, "[€,$£]")
  
  multiplier <- case_when(
    str_detect(x, "M") ~ 1000000,
    str_detect(x, "K") ~ 1000,
    TRUE ~ 1
  )
  
  number <- parse_number(x)
  
  number * multiplier
}

data <- data %>%
  mutate(
    across(
      c(Value, Wage, Release.Clause),
      money_to_number
    )
  )

# 2.5 Convert star columns to numbers

data <- data %>%
  mutate(
    across(
      c(W.F, SM, IR),
      ~ as.numeric(str_remove_all(
        as.character(.x),
        "[★☆[:space:]]"
      ))
    )
  )

# 2.6 Convert Joined to date format

data <- data %>%
  mutate(
    Joined = parse_date_time(
      Joined,
      orders = c("b d, Y", "dmy", "mdy")
    ) %>%
      as.Date()
  )


# 2.7 Calculate years at the club

library(lubridate)
library(dplyr)

data <- data %>%
  mutate(
    years_at_club = time_length(
      interval(Joined, Sys.Date()),
      "years"
    )
  )

# SECTION 3. DATA QUALITY CHECKS -----------------------------

# 3.1 Check dataset structure

glimpse(data)

# 3.2 Check missing values

colSums(is.na(data))

# 3.3 Check whether newline characters remain

data %>%
  filter(
    if_any(
      where(is.character),
      ~ str_detect(.x, "[\n\r\t]")
    )
  )

# 3.4 Check whether unnecessary whitespace remains

data %>%
  filter(
    if_any(
      where(is.character),
      ~ .x != str_squish(.x)
    )
  )

# SECTION 4. DESCRIPTIVE ANALYSIS ----------------------------

# 4.1 Calculate average height by position (inches)

height_by_position <- data %>%
  filter(!is.na(BP), BP != "GK") %>%
  group_by(BP) %>%
  summarise(
    average_height = mean(Height, na.rm = TRUE),
    number_of_players = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(average_height))

height_by_position

# 4.2 Calculate average weight by position (kg)

weight_by_position <- data %>%
  filter(!is.na(BP), BP != "GK") %>%
  group_by(BP) %>%
  summarise(
    average_weight = mean(Weight, na.rm = TRUE),
    number_of_players = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(average_weight))

weight_by_position
# 4.3 Find players who have been at their club for over 10 years

long_term_players <- data %>%
  filter(years_at_club > 10) %>%
  select(Name, BP, Joined, years_at_club) %>%
  arrange(desc(years_at_club))

long_term_players
glimpse(data)

# SECTION 5. IDENTIFYING HIGH-VALUE, LOW-WAGE PLAYERS -----------------------------------

# 5.1 Players are highly valuable but still underpaid (on low wages)
install.packages("ggrepel")
library(tidyverse)
library(ggrepel)
library(scales)

# Calculate the 75th percentile of Value
value_high <- quantile(data$Value, 0.75, na.rm = TRUE)

# Calculate the 25th percentile of Wage
wage_low <- quantile(data$Wage, 0.25, na.rm = TRUE)

# Find high-value, low-wage players
undervalue_players <- data %>%
  filter(
    Value >= value_high,
    Wage <= wage_low
  ) %>%
  mutate(
    Value_Wage_Gap = Value - Wage,
    Wage_to_Value = Wage / Value
  ) %>%
  select(
    Name, Positions, Value, Wage,
    Value_Wage_Gap, Wage_to_Value
  ) %>%
  arrange(desc(Value))

# 5.2 Select the top 10 players
top10_players <- undervalue_players %>%
  slice_max(
    order_by = Value,
    n = 10,
    with_ties = FALSE
  )

top10_players

# 5.3 Top 10 players_Bar chart
ggplot(
  top10_players,
  aes(
    x = reorder(Name, Value),
    y = Value
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(
    aes(label = paste0(Positions, " | Wage: ", dollar(Wage))),
    hjust = -0.1,
    size = 3
  ) +
  scale_y_continuous(
    labels = label_dollar(),
    expand = expansion(mult = c(0, 0.35))
  ) +
  labs(
    title = "Top 10 High-Value, Low-Wage Players",
    x = "Player",
    y = "Market Value"
  ) +
  theme_minimal()
# 5.4 Top 10 players_Scatter Plot
ggplot(data, aes(x = Wage, y = Value)) +
  # All players
  geom_point(
    color = "gray75",
    alpha = 0.3,
    size = 1.5
  ) +
  # Highlight high-value, low-wage players
  geom_point(
    data = undervalue_players,
    aes(x = Wage, y = Value),
    color = "red",
    size = 2.5
  ) +
  # Label the top 10 players
  geom_text_repel(
    data = top10_players,
    aes(
      label = paste0(Name, " (", Positions, ")")
    ),
    size = 3.5,
    color = "darkred",
    max.overlaps = Inf
  ) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "High-Value, Low-Wage Players",
    subtitle = "Red points highlight players in the high-value, low-wage group",
    x = "Weekly Wage",
    y = "Market Value"
  ) +
  theme_minimal()

# SECTION 6. DUMMY VARIABLES AND POSITION ANALYSIS -----------------------------------
# 6.1. Create Dummy Variables

data <- data %>%
  mutate(
    Left = if_else(foot == "Left", 1L, 0L),
    Striker = if_else(BP == "ST", 1L, 0L),
    Goalie = if_else(BP == "GK", 1L, 0L)
  )
# Create dummy variables for all positions
position_dummies <- as.data.frame(
  model.matrix(~ 0 + BP, data = data)
)

head(position_dummies)

# 6.2. Remove Goalkeepers

field_players <- data %>%
  filter(BP != "GK")

# 6.3. Analyze Height and Weight by Position

# 6.3.1: Average height in inches
average_height <- mean(field_players$Height, na.rm = TRUE)
average_height

height_table <- field_players %>%
  group_by(BP) %>%
  summarise(
    Average_Height = mean(Height, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Height))

height_table

# 6.3.2: Average weight in kilograms
average_weight <- mean(field_players$Weight, na.rm = TRUE)
average_weight

weight_table <- field_players %>%
  group_by(BP) %>%
  summarise(
    Average_Weight = mean(Weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Weight))

weight_table

# 6.4. Analyze Skills and Wage by Position
# 6.4.1: Average Dribbling by Position
dribbling_table <- field_players %>%
  group_by(BP) %>%
  summarise(
    Average_Dribbling = mean(Dribbling, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Dribbling))

dribbling_table

# 6.4.2: Average Attacking by Position
attacking_table <- field_players %>%
  group_by(BP) %>%
  summarise(
    Average_Attacking = mean(Attacking, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Attacking))

attacking_table

# 6.4.3: Average Agility by Position
agility_table <- field_players %>%
  group_by(BP) %>%
  summarise(
    Average_Agility = mean(Agility, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Agility))

agility_table

# 6.4.4: Average Wage by Position
wage_table <- field_players %>%
  group_by(BP) %>%
  summarise(
    Average_Wage = mean(Wage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Average_Wage))

wage_table

# 6.5. Combine All Results into One Summary Table

position_summary <- field_players %>%
  group_by(BP) %>%
  summarise(
    Players = n(),
    Average_Height = mean(Height, na.rm = TRUE),
    Average_Weight = mean(Weight, na.rm = TRUE),
    Average_Dribbling = mean(Dribbling, na.rm = TRUE),
    Average_Attacking = mean(Attacking, na.rm = TRUE),
    Average_Agility = mean(Agility, na.rm = TRUE),
    Average_Wage = mean(Wage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(BP)

position_summary

#position_summary_barchart
library(dplyr)
library(tidyr)
library(ggplot2)

position_summary %>%
  select(
    BP,
    Average_Dribbling,
    Average_Attacking,
    Average_Agility
  ) %>%
  pivot_longer(
    cols = c(
      Average_Dribbling,
      Average_Attacking,
      Average_Agility
    ),
    names_to = "Attribute",
    values_to = "Average"
  ) %>%
  ggplot(aes(x = BP, y = Average, fill = Attribute)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Player Attributes by Position",
    x = "Player Position",
    y = "Average Score",
    fill = "Attribute"
  ) +
  theme_minimal()

# 6.6. Identify Best and Worst Positions

comparison_table <- tibble(
  Metric = c("Dribbling", "Attacking", "Agility", "Wage"),
  
  Best_Position = c(
    position_summary$BP[which.max(position_summary$Average_Dribbling)],
    position_summary$BP[which.max(position_summary$Average_Attacking)],
    position_summary$BP[which.max(position_summary$Average_Agility)],
    position_summary$BP[which.max(position_summary$Average_Wage)]
  ),
  
  Best_Average = c(
    max(position_summary$Average_Dribbling, na.rm = TRUE),
    max(position_summary$Average_Attacking, na.rm = TRUE),
    max(position_summary$Average_Agility, na.rm = TRUE),
    max(position_summary$Average_Wage, na.rm = TRUE)
  ),
  
  Worst_Position = c(
    position_summary$BP[which.min(position_summary$Average_Dribbling)],
    position_summary$BP[which.min(position_summary$Average_Attacking)],
    position_summary$BP[which.min(position_summary$Average_Agility)],
    position_summary$BP[which.min(position_summary$Average_Wage)]
  ),
  
  Worst_Average = c(
    min(position_summary$Average_Dribbling, na.rm = TRUE),
    min(position_summary$Average_Attacking, na.rm = TRUE),
    min(position_summary$Average_Agility, na.rm = TRUE),
    min(position_summary$Average_Wage, na.rm = TRUE)
  )
)

comparison_table

# SECTION 7. SIMPLE AND MULTIPLE REGRESSION -----------------------------------
# 7.1. Simple Regression: Height and Interceptions

model_height <- lm(
  Interceptions ~ Height,
  data = data
)

summary(model_height)

# Visualize the relationship
ggplot(data, aes(x = Height, y = Interceptions)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Relationship Between Height and Interceptions",
    x = "Height (inches)",
    y = "Interceptions"
  ) +
  theme_minimal()

# Combine regression coefficients and model statistics
height_results <- tidy(model_height) %>%
  mutate(
    R_squared = glance(model_height)$r.squared,
    Adjusted_R_squared = glance(model_height)$adj.r.squared,
    Model_p_value = glance(model_height)$p.value,
    Observations = glance(model_height)$nobs
  ) %>%
  mutate(
    Relationship = "Height -> Interceptions"
  ) %>%
  select(
    Relationship,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    R_squared,
    Adjusted_R_squared,
    Model_p_value,
    Observations
  )

height_results

# 7.2. Simple Regression: Weight and Agility

model_weight <- lm(
  Agility ~ Weight,
  data = data
)

summary(model_weight)

# Visualize the relationship
ggplot(data, aes(x = Weight, y = Agility)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship Between Weight and Agility",
    x = "Weight (kg)",
    y = "Agility"
  ) +
  theme_minimal()

# Create a regression results table
weight_results <- tidy(model_weight) %>%
  mutate(
    R_squared = glance(model_weight)$r.squared,
    Adjusted_R_squared = glance(model_weight)$adj.r.squared,
    Model_p_value = glance(model_weight)$p.value,
    Observations = glance(model_weight)$nobs
  ) %>%
  mutate(
    Relationship = "Weight -> Agility"
  ) %>%
  select(
    Relationship,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    R_squared,
    Adjusted_R_squared,
    Model_p_value,
    Observations
  )

weight_results

# 7.3. Multiple Regression: Factors Associated with Agility

model_agility <- lm(
  Agility ~ Acceleration +
    Sprint.Speed +
    Balance +
    Reactions +
    Stamina +
    Strength,
  data = data
)

summary(model_agility)

# Create a regression results table
agility_results <- tidy(model_agility) %>%
  mutate(
    R_squared = glance(model_agility)$r.squared,
    Adjusted_R_squared = glance(model_agility)$adj.r.squared,
    Model_p_value = glance(model_agility)$p.value,
    Observations = glance(model_agility)$nobs
  ) %>%
  mutate(
    Relationship = "Multiple Regression -> Agility"
  ) %>%
  select(
    Relationship,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    R_squared,
    Adjusted_R_squared,
    Model_p_value,
    Observations
  )

agility_results

# Visualize multiple regression coefficients
install.packages("broom")  
library(broom)

agility_plot_data <- tidy(model_agility) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Significant = if_else(p.value < 0.05,
                          "Significant",
                          "Not Significant")
  )

ggplot(agility_plot_data,
       aes(x = estimate,
           y = reorder(term, estimate),
           color = Significant)) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(
      xmin = estimate - 1.96 * std.error,
      xmax = estimate + 1.96 * std.error
    ),
    height = 0.2
  ) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  labs(
    title = "Factors Associated with Agility",
    x = "Regression Coefficient (95% CI)",
    y = "Explanatory Variables",
    color = "Statistical Significance"
  ) +
  theme_minimal()

agility_results %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate, p.value)

glance(model_agility) %>%
  select(r.squared, adj.r.squared, p.value, nobs)
#download table
install.packages("writexl")  # Chỉ chạy lần đầu
library(writexl)

# Automatically find result tables
all_tables <- mget(
  Filter(
    function(x) is.data.frame(get(x)),
    ls()
  )
)

# Export all tables to one Excel file
write_xlsx(
  all_tables,
  "All_Results.xlsx"
)
# SECTION 8. CODE FOR DO PROJECT -----------------------------------
#Create dataset_overview
library(tibble)

dataset_overview <- tibble(
  Metric = c(
    "Number of rows",
    "Number of columns",
    "Key variables"
  ),
  Description = c(
    as.character(nrow(data)),
    as.character(ncol(data)),
    paste(
      c("Name", "Age", "Height", "Weight",
        "Value", "Wage", "BP", "Dribbling",
        "Agility", "Interceptions"),
      collapse = ", "
    )
  )
)

dataset_overview
#Table 2: Missing Values Summary
library(dplyr)
library(tibble)

missing_summary <- tibble(
  Variable = names(data),
  Missing_Count = colSums(is.na(data)),
  Missing_Percentage = round(
    colSums(is.na(data)) / nrow(data) * 100,
    2
  )
) %>%
  filter(Missing_Count > 0) %>%
  arrange(desc(Missing_Count))

missing_summary

#Table 3. Results of Residual Whitespace Checks
library(dplyr)
library(stringr)
library(tibble)

whitespace_check <- tibble(
  Variable = names(data),
  Residual_Newlines_Tabs = sapply(data, function(x) {
    if (is.character(x)) {
      sum(str_detect(x, "[\r\n\t]+"), na.rm = TRUE)
    } else {
      0
    }
  }),
  Residual_Extra_Whitespace = sapply(data, function(x) {
    if (is.character(x)) {
      sum(x != str_trim(str_squish(x)), na.rm = TRUE)
    } else {
      0
    }
  })
)

whitespace_check

table3_summary <- whitespace_check %>%
  summarise(
    `Newline/Tab Issues` = sum(Residual_Newlines_Tabs),
    `Extra Whitespace Issues` = sum(Residual_Extra_Whitespace)
  )

table3_summary
#
summary(agility_results)

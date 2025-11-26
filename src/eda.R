# eda.R
# ================ script for Exploratory Data Analysis =====================

# import necessary libraries
library(pacman)
p_load(ggplot2, readr, tidyr, tidyverse)

# import data
df <- read_delim("data/CBC_Wine_data.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

# summarize data
summary(df)
# 6300 questions
# also exactly 6300 1's in choice so we know no participant inadvertently selected
# more than one option while taking the survey.

# price, brands, type, alcohol, age, sweetness, label, and choice 
# are all encoded as characters. Better to turn them into factors.

# convert specified variables to factors
df <- df %>% 
  mutate(across(Price:choice, as.factor))

# number of unique participants
length(unique(df$resp.id)) # 350 participants

# check for missing data
sum(is.na(df)) # complete dataset!

# filter for positive choices
df_chosen <- df %>% 
  filter(choice == 1)

# check distribution of choices across different attributes
brands <- as_tibble(xtabs(as.numeric(choice) ~ Brands, df_chosen))
type <- as_tibble(xtabs(as.numeric(choice) ~ Type, df_chosen))
alcohol <- as_tibble(xtabs(as.numeric(choice) ~ Alcohol, df_chosen))
age <- as_tibble(xtabs(as.numeric(choice) ~ Age, df_chosen))
sweetness <- as_tibble(xtabs(as.numeric(choice) ~ Sweetness, df_chosen))
labels <- as_tibble(xtabs(as.numeric(choice) ~ label, df_chosen))

# plots (run as a single block)
par(mfrow=c(3,2))
par(mar=c(5,4,3,0))
barplot(brands$n, main = "Distribution of choice ~ brands", names.arg = brands$Brands)
barplot(type$n, main = "Distribution of choice ~ type", names.arg = type$Type)
barplot(alcohol$n, main = "Distribution of choice ~ alcohol", names.arg = alcohol$Alcohol)
barplot(age$n, main = "Distribution of choice ~ age", names.arg = age$Age)
barplot(sweetness$n, main = "Distribution of choice ~ sweetness", names.arg = sweetness$Sweetness)
barplot(labels$n, main = "Distribution of choice ~ labels", names.arg = labels$label)

# eda.R
# ================ script for Exploratory Data Analysis =====================

# import necessary libraries
library(pacman)
p_load(ggplot2, readr, tidyr, tidyverse, latex2exp, mlogit)

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
  mutate(
    across(Price:label, as.factor),
    choice = choice == 1
  )

any(duplicated(df[c("resp.id", "ques", "alt")]))

# number of unique participants
length(unique(df$resp.id)) # 350 participants

# check for missing data
sum(is.na(df)) # complete dataset!

# filter for positive choices
df_chosen <- df %>% 
  filter(choice == 1)

# check distribution of choices across different attributes via cross-tabulation
brands <- as_tibble(xtabs(as.numeric(choice) ~ Brands, df_chosen))
type <- as_tibble(xtabs(as.numeric(choice) ~ Type, df_chosen))
alcohol <- as_tibble(xtabs(as.numeric(choice) ~ Alcohol, df_chosen))
age <- as_tibble(xtabs(as.numeric(choice) ~ Age, df_chosen))
sweetness <- as_tibble(xtabs(as.numeric(choice) ~ Sweetness, df_chosen))
labels <- as_tibble(xtabs(as.numeric(choice) ~ label, df_chosen))

# plots (run as a single block)
{
  # distribute plots across 3 rows and 2 columns
  par(mfrow=c(3,2))
  # decrease the right margin to 0
  par(mar=c(5,4,3,0))
  # sequence of barplots to make
  barplot(brands$n, main = "Distribution of choice ~ brands",
          names.arg = brands$Brands, ylim = c(0,5000))
  barplot(type$n, main = "Distribution of choice ~ type",
          names.arg = type$Type, ylim = c(0,5000))
  barplot(alcohol$n, main = "Distribution of choice ~ alcohol",
          names.arg = alcohol$Alcohol, ylim = c(0,5000))
  barplot(age$n, main = "Distribution of choice ~ age",
          names.arg = age$Age, ylim = c(0,5000))
  barplot(sweetness$n, main = "Distribution of choice ~ sweetness",
          names.arg = sweetness$Sweetness, ylim = c(0,5000))
  barplot(labels$n, main = "Distribution of choice ~ labels", 
          names.arg = labels$label, ylim = c(0,5000))
  #reset graphical parameters to default
  par(mfrow=c(1,1))
  par(mar=c(5,4,4,2) + 0.1) # values taken from documentation (see `?par`)
}

# check how many respondents showed signs of survey fatigue or how many
# chose always the same option among those available

# check sd of alt variable for each resp.id
check_attention <- df_chosen %>% 
  group_by(resp.id) %>% 
  summarize(alt_sd = sd(alt))

quantile(check_attention$alt_sd)
(box <- boxplot(check_attention$alt_sd,
        main = TeX("Boxplot of \\sigma(alt) for chosen alternatives")))

# as we can see from the boxplot there are four outliers. People with a very low
# variability in their choices migth be subject to survey fatigue or might have
# not considered at all the other options carefully.
# Considered the very low number of people who exhibited this behavior we
# can either drop them and exclude from further analysis or leave them in the
# sample.
######################### MODEL FITTING ################################

df_mlogit <- mlogit.data(
  df,
  choice  = "choice",     # logical variable: TRUE if the alternative was chosen
  shape   = "long",       # data are in long format (one row per alternative)
  alt.var = "alt",        # alternative identifier within each choice set
  chid.var = "ques",      # choice situation (question ID)
  id.var   = "resp.id"    # respondent ID
)

m1 <- mlogit(choice ~ Type, data = df_mlogit)
summary(m1)


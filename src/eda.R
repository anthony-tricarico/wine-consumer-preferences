# eda.R
# ================ script for Exploratory Data Analysis =====================

# import necessary libraries
library(pacman)
p_load(ggplot2, readr, tidyr, tidyverse, latex2exp, mlogit, MASS, lattice)

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

# add a column (Price_eur) to represent price as a quantitative variable
# the values are the upper-bound of those specified when carrying out the survey
df <- df %>% 
  mutate(Price_eur = case_when(
    Price == "Cheap" ~ 15,
    Price == "Popular" ~ 20,
    Price == "Premium" ~ 50,
    Price == "Luxury" ~ 100,
  ))

df_mlogit <- mlogit.data(
  df,
  choice  = "choice",     # logical variable: TRUE if the alternative was chosen
  shape   = "long",       # data are in long format (one row per alternative)
  alt.var = "alt",        # alternative identifier within each choice set
  chid.var = "ques",      # choice situation (question ID)
  id.var   = "resp.id"    # respondent ID
)

# explicitly assign reference level to each factor
df_mlogit$Price <- relevel(df_mlogit$Price, ref = "Cheap")
df_mlogit$Brands <- relevel(df_mlogit$Brands, ref = "Cavit")
df_mlogit$Type <- relevel(df_mlogit$Type, ref = "red")
df_mlogit$Alcohol <- relevel(df_mlogit$Alcohol, ref = "5.5 Perc")
df_mlogit$Age <- relevel(df_mlogit$Age, ref = "1 year")
df_mlogit$Sweetness <- relevel(df_mlogit$Sweetness, ref = "dry")
df_mlogit$label <- relevel(df_mlogit$label, ref = "No designation")

# fit complete model with intercept
m1 <- mlogit(choice ~ Price + Brands + Type + Alcohol + Age + Sweetness + label,
             data = df_mlogit)
# get summary of the fitted model
summary(m1)
# we notice that the alternative-specific intercepts are not significant. We can
# therefore test how a model without such parameters perform compared to the
# fully-specified model

# fit restricted model 
m2 <- mlogit(choice ~ Price + Brands + Type + Alcohol + Age + Sweetness + label | -1,
             data = df_mlogit)
# check if the fully-specified model has more explanatory power than the restricted model
# we can do this by performing a likelihood ratio test of the two models
# H0: the simpler (restricted) model fits the data better
# H1: the more complicated model fits the data better
lrtest(m2, m1)
# p > 0.05, so we conclude that the simpler restricted model is better
# since we cannot reject H0
summary(m2)

# now we can fit the simpler model with the addition of price as a proper quantitative variable
m3 <- mlogit(choice ~ Price_eur + Brands + Type + Alcohol + Age + Sweetness + label | -1,
             data = df_mlogit)
summary(m3)
# as expected, the estimated coefficient for Price_eur is negative.

# we compare the fit of this model to that of the previous restricted model
# which included Price as a qualitative factorial variable
lrtest(m3, m2)
# in this case we see that the model considering price as a qualitative variable
# explains a significant amount of variation in consumer choice compared to the
# model which considers price a quantitative variable.

# compute willingness to pay (?)

# simulate preference shares
predict.mnl <- function(model, data) {
  # Function for predicting preference shares from a MNL model 
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  logitUtility <- data.model%*%model$coef
  share <- exp(logitUtility)/sum(exp(logitUtility))
  cbind(share, data)
}

# In order to use "predict.mnl", you need to define a data frame containing the set of designs 
# for which you want to predict the preference shares. 
# One way to do this is to create the full set of possible designs
# using expand.grid() and select the designs we want by row number
names(df_mlogit)
attributes <- list(
  Price=names(table(df_mlogit$Price)),
  Brands=names(table(df_mlogit$Brands)),
  Type=names(table(df_mlogit$Type)),
  Alcohol=names(table(df_mlogit$Alcohol)),
  Age=names(table(df_mlogit$Age)),
  Sweetness=names(table(df_mlogit$Sweetness)),
  label=names(table(df_mlogit$label))#,
 # Price_eur = names(table(df_mlogit$Price_eur))
)
all_designs <- expand.grid(attributes) 
all_designs #all possible designs

# sample 10% of total
# TODO: understand if this is the right way to do this.
# alternatively we could just decide on a number of different combinations
# and select those manually instead of relying on pseudo-random sampling.

{
  # set seed for replication purposes
  set.seed(124)
  subset_data <- slice_sample(all_designs, n = 20)
}

# check that there is at least one 
summary(subset_data)

preds_ml2 <- predict.mnl(m2, subset_data)
# as expected the total shares sum up to one
sum(preds_ml2$share)

# view choices in descending projected market share order
preds_ml2 %>% 
  arrange(desc(share))

sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a preference share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  # competitor.data: data frame contining design of competitive set
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}

base_data <- subset_data[1,]
competitor_data <- subset_data[-1,]

(tradeoff <- sensitivity.mnl(m2, attributes, base_data, competitor_data))

barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$level,
        ylab="Change in Share for the Planned Product Design", 
        ylim=c(-0.1,0.11))
grid(nx=NA, ny=NULL)

# =============== FITTING MIXED MODELS ====================
# fit sample model just to extract the name for random coefficients
tmp <- mlogit(choice ~ Price + Brands + Type + Alcohol + Age + Sweetness + label | -1,
              data = df_mlogit,
              rpar = NULL)

# specify that all coefficients are assumed to follow a normal distribution
rpar <- rep("n", length(tmp$coef))
# assign to each element in the vector a name coming from the coefficients 
names(rpar) <- names(tmp$coef)

m2_mixed <- mlogit(choice ~ Price + Brands + Type + Alcohol + Age + Sweetness + label | -1,
                   data = df_mlogit,
                   panel = TRUE,
                   rpar = rpar,
                   correlation = FALSE)
# visual summary of distribution of random effects
plot(m2_mixed)

for (i in 1:length(rpar)) {
  var <- names(rpar[i])
  print(paste("=======", var, "======="))
  dist <- rpar(m2_mixed, var)
  print(dist)
  summary(dist)
  mean(dist)
  med(dist)
  plot(dist)
}

# this model considers random coefficients to be correlated
m2_mixed_corr <- mlogit(choice ~ Price + Brands + Type + Alcohol + Age + Sweetness + label | -1,
                   data = df_mlogit,
                   panel = TRUE,
                   rpar = rpar,
                   correlation = TRUE)

# looking at the covariance matrix of coefficients to understand the strength of
# the correlation
cov2cor(cov.mlogit(m2_mixed_corr))

# we can test significant relationships as well by estimating standard errors
summary(vcov(m2_mixed_corr, what = "rpar", type = "cor"))


# TODO: understand if it is useful to restrict only to a subset of significant
# random coefficients

# The significant presence of random coefficients and their correlation 
# can be further investigated using the ML tests, such as the ML ratio test
lrtest(m2, m2_mixed) #Fixed effects vs. uncorrelated random effects
lrtest(m2_mixed, m2_mixed_corr) #Uncorrelated random effects vs. all correlated random effects

predict.mixed.mnl <- function(model, data, nresp=1000) {
  # Function for predicting shares from a mixed MNL model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares. Same format at the data used to estimate model. 
  # Note that this code assumes all model parameters are random
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  cbind(colMeans(shares), data)
}

{
  set.seed(1111)
  predict.mixed.mnl(m2_mixed_corr, data=subset_data)
}

# ============== Assessing the effects of individual-level predictors ==========
# To assess if consumer heterogeneity can be explained by their individual characteristics
# we can study the relationship between the individual part worth and the 
# individual-level variables. Individual part worth can be extracted using
# fitted(), with the "type" argument set to "parameters". 
PW_ind <- fitted(m2_mixed_corr, type = "parameters")
head(PW_ind)


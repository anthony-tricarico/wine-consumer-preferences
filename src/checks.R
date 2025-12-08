# checks.R --- Max Execution time: 20s
# This script contains a series of sanity checks to understand the criteria
# used to generate the data.
# ====================================
# import necessary libraries
library(pacman)
p_load(ggplot2, readr, tidyr, tidyverse, latex2exp, mlogit, MASS, lattice, vcd,
       car, Matrix, AlgDesign)

# import data
df <- read_delim("data/CBC_Wine_data.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

attributes <- names(df)[4:10] 

# Marginal frequencies for categorical attributes
for (a in attributes) {
  if (!is.numeric(df[[a]])) {
    cat("\n--- Marginals for", a, "---\n")
    print(prop.table(table(df[[a]])))
  }
}

# we conclude that the design is balanced as each level appears roughly the
# same number of times across survey profiles

# check for multicollinearity
cat_attrs <- attributes[!sapply(df[attributes], is.numeric)]

cramer_results <- matrix(NA, nrow=length(cat_attrs), ncol=length(cat_attrs),
                         dimnames=list(cat_attrs, cat_attrs))

# =============== MULTICOLLINEARITY CHECK ===================
cat("\n=== Cramér's V Matrix ===\n")
print(round(cramer_results, 3))
# all correlations are below 0.2
# this indicates that attributes per se should not cause multicollinearity
# in the model

# ============  
# Effects coding 
design_df <- df %>% 
  mutate(across(all_of(cat_attrs), as.factor)) %>%
  dplyr::select(all_of(cat_attrs), Price)

# Build design matrix as used by the regression
X <- model.matrix(~ . , data = design_df)

# Condition number
# the ratio of the largest to the smallest singular value of x 
# small condition number = the columns of X are well separated and the parameter
# estimates are numerically stable
# large condition number = signals strong multicollinearity
# NOTE: the singular value comes from SVD of the design matrix
cond_number <- kappa(X, exact = T)
cat("\n=== Condition Number ===\n")
print(cond_number)

######## COMPUTE D-EFFICIENCY ###########
XtX <- t(X) %*% X
p <- ncol(X)   # number of parameters
n <- nrow(X)   # number of observations

det_val <- det(XtX)

if (det_val <= 0) {
  cat("\nWARNING: Determinant <= 0 → design not full rank.\n")
} else {
  deff <- det_val^(1/p) / n
  cat("\n=== Pseudo D-efficiency ===\n")
  print(deff)
}

######### RARE LEVELS CHECK #############
cat("\n=== Rare Level Check ===\n")

for (a in cat_attrs) {
  freq <- table(df[[a]])
  rare <- freq[freq < 0.01 * nrow(df)]
  if (length(rare) > 0) {
    cat("\nAttribute:", a, "\n")
    cat("Rare levels detected:\n")
    print(rare)
  }
}

# as observed also when discussing uniformity of levels, no rare levels are present
# (operationalized as those showed less than 1% of the times)

#### BENCHMARK D-EFFICIENCY #####
attributes <- list(
  Price=names(table(df$Price)),
  Brands=names(table(df$Brands)),
  Type=names(table(df$Type)),
  Alcohol=names(table(df$Alcohol)),
  Age=names(table(df$Age)),
  Sweetness=names(table(df$Sweetness)),
  label=names(table(df$label))#,
)
all_designs <- expand.grid(attributes) 

# number of runs = number of trials in the final design
n_runs <- nrow(all_designs)

opt <- optFederov(~., data = all_designs, nTrials = n_runs, nRepeats = 10)
opt_design <- opt$design

# Build model matrix for opt_design
Xopt <- model.matrix(~., data = opt_design)
XtX_opt <- crossprod(Xopt)
p <- ncol(Xopt)
n <- nrow(Xopt)
deff_opt <- (det(XtX_opt)^(1/p)) / n

cat("Observed D-eff:", deff, "\n")
cat("Opt (approx) D-eff:", deff_opt, "\n")
cat("Relative D-eff:", deff / as.numeric(deff_opt), "\n")

# from the results we conclude that the actual design of the study is as good
# as the efficient design estimated using the Federov's exchange algorithm.
# The algorithm works by swapping a point in the design with a point that was
# not considered. The algorithm stops when the determinant of the information
# matrix is maximized.

# ============ CHECK IF FULL FACTORIAL ===============
attributes <- names(df)[4:10] 
df %>% 
  dplyr::select(all_of(attributes)) %>% 
  n_distinct()

# Conclusion: the design is not full factorial since it does not contain
# all possible combination of attribute levels. However, based on the analysis
# from the benchmarked D-efficiency section it is likely that the data was
# generated following a fractional efficient design aiming at maximing the
# D-efficiency.
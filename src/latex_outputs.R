# Run this script after running the entire eda.R script from top to bottom
# and having the objects referenced in `tables_names` loaded in the environment.
# Produce LaTeX outputs (tables)
# =========================================================================
library(pacman)
p_load(stargazer, dplyr, knitr)

significant_correlations_coefficients <- stats_df %>% 
  filter(Significant == T,
         Iscorrelation == T) %>% 
  dplyr::select(P_value) %>% 
  rownames_to_column(var = "Parameter")

# object names
tables_names <- list(m1, m2, m3,
                     m2_mixed, m2_mixed_corr,
                     m2_mixed_significant_corrs)

# descriptions attached to each table
descriptions <- c("Regression Results for Model with Intercepts",
                  "Regression Results for Model without Intercepts",
                  "Regression Results for Model treating Price as a Quantitative Variable",
                  "Mixed Multinomial Logit Model (no correlated coefficients)",
                  "Mixed Multinomial Logit Model (with correlated coefficients)",
                  "Mixed Multinomial Logit Model (only statistically significant correlated coefficients)")

# output names (file names)
output_names <- c("latex/model_summary_intercepts.tex",
                  "latex/model_summary_no_intercepts.tex",
                  "latex/model_price_quantitative.tex",
                  "latex/mixed_multinomial_no_corr.tex",
                  "latex/mixed_multinomial_with_corr.tex",
                  "latex/mixed_multinomial_significant_corrs.tex",
                  "latex/random_un_vs_corr.tex")

# generate LaTeX outputs for models
for (i in 1:length(tables_names)) {
  stargazer(
    tables_names[[i]], 
    type = "latex", 
    title = descriptions[i],
    align = TRUE,
    out = output_names[i]
  )
}

tables_no_models <- list(lr_restricted, sorted_shares,
                     base_data, competitor_data,
                     significant_correlations_coefficients,
                     m3_vs_m2,
                     m2_vs_m2_mixed,
                     m2_mixed_vs_m2_mixed_corr,
                     m2_mixed_corr_vs_m2_mixed_significant_corrs,
                     df_wtp)

# descriptions attached to each table
descriptions_no_models <- c(
                  "Testing the Restricted vs Full model Fit",
                  "Projected Market Shares",
                  "Base Profile (Sensitivity Analysis)",
                  "Competitor Profile (Sensitivity Analysis)",
                  "Coefficients with Statistically Significant Correlations from Mixed Model",
                  "Difference in models treating price as a continuous or categorical variable",
                  "Fixed vs Random Coefficients",
                  "Random uncorrelated vs Random correlated",
                  "All random correlated Coefficients vs Significant correlated Coefficients",
                  "WTP associated to each attribute level")

# output names (file names)
output_names_no_models <- c(
                  "latex/intercept_vs_nointercept.tex",
                  "latex/projected_market_shares.tex",
                  "latex/base_profile.tex",
                  "latex/competitor_profile.tex",
                  "latex/significant_coefficient_corrs.tex",
                  "latex/price_quantitative_vs_qualitative.tex",
                  "latex/fixed_vs_random_coeffs.tex",
                  "latex/random_un_vs_corr.tex",
                  "latex/all_corr_coeffs_vs_significant_corr_coefs.tex",
                  "latex/wtp_by_attribute_level.tex")


for (i in 1:length(tables_no_models)) {
  stargazer(
    tables_no_models[[i]], 
    type = "latex", 
    caption = descriptions_no_models[i],
    summary = FALSE,
    header = FALSE,
    out = output_names_no_models[i]
  )
}

sink(file.path(cdir, "statistics.txt"))

df = clean_dataset

# Descriptives total

cat("\n \n Descriptives \n \n")

desc = df %>% 
  select(reading_time, evaluation_time, aim, wept, wept_cor, wept_inc, donation, cPEB) %>%
  describe()

desc_sum = df %>% 
  select(wept, wept_cor, wept_inc, donation) %>% 
  colSums() %>% 
  as.data.frame() %>% 
  rename(sum = ".")

desc_total = merge(desc, desc_sum, by="row.names", all.x = T) %>% arrange(vars)

print(desc_total)

desc_by_group = describeBy(reading_time + evaluation_time + aim + wept + wept_cor 
                           + wept_inc + donation + cPEB ~ category, data = df)

print(desc_by_group)

cat("\n \n Correlation between PEB measures (WEPT and donations) \n \n")
correlation = cor(df$wept, df$donation)
output(correlation)

cat("\n \n Story length descriptives \n \n")

desc_stories = stories %>% 
  select(len_PL, len_NO) %>% 
  describe()

print(desc_stories)

# Manipulation checks — inferential statistics (Reviewer request)

cat("\n\n")
cat("Manipulation checks — inferential statistics\n")
cat("\n\n")


df = df %>%
  mutate(category = factor(category, levels = c("NEU", "ANG", "COM", "HOP")))

# Helper: run ANOVA + planned contrasts + effect sizes and 95% CI
run_manipcheck_anova = function(data, outcome, contrasts_list, label) {
  
  cat("\n\n--- ", label, " (DV = ", outcome, ") ---\n", sep = "")
  
  fit = aov(as.formula(paste0(outcome, " ~ category")), data = data)
  
  cat("\nOmnibus test:\n")
  print(summary(fit))
  
  eta = eta_squared(fit, partial = TRUE, ci = 0.95)
  cat("\nOmnibus effect size (partial eta^2) with 95% CI:\n")
  print(eta)
  
  emm = emmeans(fit, ~ category)
  contr = contrast(emm, method = contrasts_list, adjust = "none")
  contr_sum = summary(contr, infer = c(TRUE, TRUE))
  
  cat("\nPlanned contrasts (estimate + 95% CI):\n")
  print(contr_sum)
  
  d_df = contrast_d_from_model(contr_sum, fit)
  cat("\nEffect sizes (standardized d) with 95% CI:\n")
  print(d_df %>% dplyr::select(contrast, d, d_low, d_high, sigma))
  
  invisible(list(fit = fit, eta = eta, contrasts = contr_sum, d = d_df))
}

# Planned contrasts

# Valence
contrasts_valence = list(
  HOP_vs_NEU = c(-1, 0, 0, 1),
  ANG_vs_NEU = c(-1, 1, 0, 0),
  COM_vs_NEU = c(-1, 0, 1, 0))

# Arousal
contrasts_arousal = list(
  EMO_vs_NEU = c(-1, 1/3, 1/3, 1/3))

# Specificity
contrasts_anger = list(
  ANG_vs_others = c(-1/3, 1, -1/3, -1/3))
contrasts_compassion = list(
  COM_vs_others = c(-1/3, -1/3, 1, -1/3))
contrasts_hope = list(
  HOP_vs_others = c(-1/3, -1/3, -1/3, 1))

if ("valence" %in% names(df)) {
  res_valence = run_manipcheck_anova(
    data = df,
    outcome = "valence",
    contrasts_list = contrasts_valence,
    label = "Manipulation check: Valence")
}

if ("arousal" %in% names(df)) {
  res_arousal = run_manipcheck_anova(
    data = df,
    outcome = "arousal",
    contrasts_list = contrasts_arousal,
    label = "Manipulation check: Arousal")
}

if ("anger" %in% names(df)) {
  res_anger = run_manipcheck_anova(
    data = df,
    outcome = "anger",
    contrasts_list = contrasts_anger,
    label = "Manipulation check: Anger specificity")
}

if ("compassion" %in% names(df)) {
  res_comp = run_manipcheck_anova(
    data = df,
    outcome = "compassion",
    contrasts_list = contrasts_compassion,
    label = "Manipulation check: Compassion specificity" )
}

if ("hope" %in% names(df)) {
  res_hope = run_manipcheck_anova(
    data = df,
    outcome = "hope",
    contrasts_list = contrasts_hope,
    label = "Manipulation check: Hope specificity")
}

# Descriptives for the exploratory analyses
cat("\n\nDecsriptives for exploratory analyses: comparing groups sensitive to manipulation (included) and not sensitive (excluded)\n\n")

# attach participant-level soft_check group to your analysis dataset
soft_group = subjects %>%
  distinct(sid, soft_check) %>%
  mutate(soft_group = ifelse(soft_check == 1, "included", "excluded")) %>%
  select(sid, soft_group)

df_soft = df %>% left_join(soft_group, by = "sid")

make_desc_tables_by_category = function(dat, label, cdir) {
  
  # Continuous variables
  cont_vars = c(
    "reading_time", "evaluation_time",
    "valence", "arousal", "anger", "compassion", "hope",
    "aim", "wept", "wept_cor", "wept_inc", "donation", "cPEB",
    "PCAE", "PD", "WTS", "age"
  )
  cont_vars = cont_vars[cont_vars %in% names(dat)]
  
  desc_cont_by_category = dat %>%
    select(category, any_of(cont_vars)) %>%
    pivot_longer(cols = -category, names_to = "variable", values_to = "value") %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      iqr = IQR(value, na.rm = TRUE),
      .by = c(category, variable)
    ) %>%
    arrange(variable, category)
  
  cat("\n\nDescriptives (continuous) by category — ", label, ":\n", sep = "")
  print(desc_cont_by_category)
  
  write.csv(
    desc_cont_by_category,
    file.path(cdir, paste0("reviewer_", label, "_descriptives_continuous_by_category.csv")),
    row.names = FALSE
  )
  
  # Categorical variables
  cat_vars = c("sex", "gen", "res", "edu", "kid", "ses", "bcc", "ccc", "country")
  cat_vars = cat_vars[cat_vars %in% names(dat)]
  
  desc_cat_by_category = purrr::map_dfr(cat_vars, function(v) {
    dat %>%
      filter(!is.na(category)) %>%
      mutate(level = as.character(.data[[v]])) %>%
      filter(!is.na(level)) %>%
      count(category, level, name = "n") %>%
      group_by(category) %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        variable = v
      ) %>%
      ungroup() %>%
      select(category, variable, level, n, pct)
  }) %>%
    arrange(variable, category, desc(n))
  
  cat("\n\nDescriptives (categorical) by category — ", label, ":\n", sep = "")
  print(desc_cat_by_category, n = 200)
  
  write.csv(
    desc_cat_by_category,
    file.path(cdir, paste0("reviewer_", label, "_descriptives_categorical_by_category.csv")),
    row.names = FALSE
  )
  
  invisible(list(desc_cont_by_category = desc_cont_by_category,
                 desc_cat_by_category = desc_cat_by_category))
}

df_included = df_soft %>% filter(soft_group == "included")
df_excluded = df_soft %>% filter(soft_group == "excluded")

# Output tables

get_desc_cont_by_category = function(dat) {
  cont_vars = c(
    "reading_time", "evaluation_time",
    "valence", "arousal", "anger", "compassion", "hope",
    "aim", "wept", "wept_cor", "wept_inc", "donation", "cPEB",
    "PCAE", "PD", "WTS", "age"
  )
  cont_vars = cont_vars[cont_vars %in% names(dat)]
  
  dat %>%
    select(category, any_of(cont_vars)) %>%
    pivot_longer(cols = -category, names_to = "variable", values_to = "value") %>%
    summarise(
      n = sum(!is.na(value)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .by = c(category, variable)
    ) %>%
    arrange(variable, category)
}

get_desc_cat_by_category = function(dat) {
  cat_vars = c("sex", "gen", "res", "edu", "kid", "ses", "bcc", "ccc", "country")
  cat_vars = cat_vars[cat_vars %in% names(dat)]
  
  purrr::map_dfr(cat_vars, function(v) {
    dat %>%
      filter(!is.na(category)) %>%
      mutate(level = as.character(.data[[v]])) %>%
      filter(!is.na(level)) %>%
      count(category, level, name = "n") %>%
      group_by(category) %>%
      mutate(
        pct = round(100 * n / sum(n), 1),
        variable = v
      ) %>%
      ungroup() %>%
      select(category, variable, level, n, pct)
  }) %>%
    arrange(variable, category, desc(n))
}

cont_in = get_desc_cont_by_category(df_included)
cont_ex = get_desc_cont_by_category(df_excluded)

cont_comp =
  full_join(
    cont_in, cont_ex,
    by = c("category", "variable"),
    suffix = c("_incl", "_excl")
  ) %>%
  mutate(
    diff_mean = mean_incl - mean_excl
  ) %>%
  select(
    category, variable,
    n_incl, mean_incl, sd_incl,
    n_excl, mean_excl, sd_excl,
    diff_mean
  ) %>%
  arrange(variable, category)

cont_comp_to_save = cont_comp %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

write.csv(
  cont_comp_to_save,file.path(cdir, "review_softcheck_comparison_continuous_by_category.csv"), row.names = FALSE)

write.csv(
  cont_comp,file.path(cdir, "review_softcheck_comparison_continuous_by_category.csv"),row.names = FALSE)

cat_in = get_desc_cat_by_category(df_included)
cat_ex = get_desc_cat_by_category(df_excluded)

cat_comp =
  full_join(
    cat_in, cat_ex,
    by = c("category", "variable", "level"),
    suffix = c("_incl", "_excl")
  ) %>%
  mutate(
    n_incl = tidyr::replace_na(n_incl, 0),
    pct_incl = tidyr::replace_na(pct_incl, 0),
    n_excl = tidyr::replace_na(n_excl, 0),
    pct_excl = tidyr::replace_na(pct_excl, 0),
    diff_pct = pct_incl - pct_excl
  ) %>%
  select(category, variable, level, n_incl, pct_incl, n_excl, pct_excl, diff_pct) %>%
  arrange(variable, category, desc(abs(diff_pct)), desc(n_incl + n_excl))

write.csv(
  cat_comp, file.path(cdir, "review_softcheck_comparison_categorical_by_category.csv"),row.names = FALSE)

cat("\nSaved comparison tables:\n",
    "- review_softcheck_comparison_continuous_by_category.csv\n",
    "- review_softcheck_comparison_categorical_by_category.csv\n", sep = "")

## Hypothesis 1: UNREGISTERED

cat("\n \n Hypothesis 1: MANOVA (UNREGISTERED) \n \n")

manova_result = manova(cbind(wept, donation) ~ emo, data = df)
output(summary.aov(manova_result))
output(report(manova_result))

# Hypothesis 2

cat("\n \n Hypothesis 2: ANOVA \n \n")

cat("\n \n ANOVA cPEB \n \n")
anova_result = aov(cPEB ~ category, data = df)
output(summary(anova_result))
output(report(anova_result))

cat("\n \n Posthocs cPEB \n \n")
posthoc_result = TukeyHSD(anova_result)
print(posthoc_result)

cat("\n \n ANOVA WEPT \n \n")
anova_result = aov(wept ~ category, data = df)
output(summary(anova_result))
output(report(anova_result))

cat("\n \n Posthocs WEPT \n \n")
posthoc_result = TukeyHSD(anova_result)
print(posthoc_result)

cat("\n \n ANOVA Donation \n \n")
anova_result = aov(donation ~ category, data = df)
output(summary(anova_result))
output(report(anova_result))

cat("\n \n Posthocs Donation \n \n")
posthoc_result = TukeyHSD(anova_result)
print(posthoc_result)

# Hypothesis 3

cat("\n \n Hypothesis 3: Chi-square test \n \n")

chi2 = chisq.test(df$aim, df$category)

observed = chi2$observed
probabilities = proportions(observed, margin = 2)

output(probabilities*100)
output(chi2)

## Hypothesis 3: UNREGISTERED

cat("\n \n Hypothesis 3: Chi-square test with NEU as baseline (UNREGISTERED) \n \n")

baseline = probabilities[,"NEU"]

output(chisq.test(observed[,"ANG"], p = baseline))
output(chisq.test(observed[,"COM"], p = baseline))
output(chisq.test(observed[,"HOP"], p = baseline))
output(chisq.test(observed[,"NEU"], p = baseline)) # Just a sanity check...

# Hypothesis 4

check_assumptions = function(model) {
  dw_test = durbinWatsonTest(model)
  cat("Durbin-Watson Test for Independence:\n")
  print(dw_test)
  cat("\n")
  ncv_test = ncvTest(model)
  cat("Non-constant Variance Test for Homoscedasticity:\n")
  print(ncv_test)
  cat("\n")
  shapiro_test = shapiro.test(residuals(model))
  cat("Shapiro-Wilk Test for Normality of Residuals:\n")
  print(shapiro_test)
  cat("\n")
}

tidy.up = function(model) {
  tidy_model = tidy(model)
  tidy_model$stats = ifelse(tidy_model$p.value > 0.05, "ns", 
                            ifelse(tidy_model$p.value < 0.001, "p<0.001", 
                                   formatC(tidy_model$p.value, format = "f", digits = 2)))
  tidy_model$estimate = round(tidy_model$estimate, 3)
  tidy_model = tidy_model[, c("term", "estimate", "stats")]
  return(tidy_model)
}

cat("\n \n Hypothesis 4: Regression models \n \n")

# cPEB

cat("\n \n cPEB model: without interactions \n \n")
model1 = lm(cPEB ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS 
            + sex + age + res + edu + kid + ses, data = df)

output(summary(model1))
tidy_model = tidy.up(model1)
output_file = file.path(cdir, "model_summary_cPEB.csv")
write.csv(tidy_model, output_file, row.names = FALSE)

cat("\n \n Assumptions tests \n \n")

check_assumptions(model1)

cat("\n \n cPEB model: with interactions \n \n")
model2 = lm(cPEB ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS 
            + sex + age + res + edu + kid + ses + category:valence + category:arousal + category:bcc 
            + category:ccc + category:PCAE + category:PD + category:WTS 
            + category:sex + category:age + category:res + category:edu 
            + category:kid + category:ses, data = df) 

output(summary(model2))
tidy_model = tidy.up(model2)
output_file = file.path(cdir, "model_summary_cPEB_int.csv")
write.csv(tidy_model, output_file, row.names = FALSE)

cat("\n \n Assumptions tests \n \n")

check_assumptions(model2)

cat("\n \n cPEB models: differences in explained variance\n \n")
int_improvement = anova(model1,model2)
output(int_improvement)


# WEPT

cat("\n \n WEPT model: without interactions \n \n")
model1 = lm(wept ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS 
            + sex + age + res + edu + kid + ses, data = df)

output(summary(model1))
tidy_model = tidy.up(model1)
output_file = file.path(cdir, "model_summary_WEPT.csv")
write.csv(tidy_model, output_file, row.names = FALSE)

cat("\n \n Assumptions tests \n \n")

check_assumptions(model1)

cat("\n \n WEPT model: with interactions \n \n")
model2 = lm(wept ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS 
            + sex + age + res + edu + kid + ses + category:valence + category:arousal + category:bcc 
            + category:ccc + category:PCAE + category:PD + category:WTS 
            + category:sex + category:age + category:res + category:edu 
            + category:kid + category:ses, data = df) 

output(summary(model2))
tidy_model = tidy.up(model2)
output_file = file.path(cdir, "model_summary_WEPT_int.csv")
write.csv(tidy_model, output_file, row.names = FALSE)

cat("\n \n Assumptions tests \n \n")

check_assumptions(model2)

cat("\n \n WEPT models: differences in explained variance\n \n")
int_improvement = anova(model1,model2)
output(int_improvement)

# donation

cat("\n \n Donation model: without interactions \n \n")
model1 = lm(donation ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS 
            + sex + age + res + edu + kid + ses, data = df)

output(summary(model1))
tidy_model = tidy.up(model1)
output_file = file.path(cdir, "model_summary_donation.csv")
write.csv(tidy_model, output_file, row.names = FALSE)

cat("\n \n Assumptions tests \n \n")
check_assumptions(model1)

cat("\n \n Donation model: with interactions \n \n")
model2 = lm(donation ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS 
            + sex + age + res + edu + kid + ses + category:valence + category:arousal + category:bcc 
            + category:ccc + category:PCAE + category:PD + category:WTS 
            + category:sex + category:age + category:res + category:edu 
            + category:kid + category:ses, data = df) 

output(summary(model2))
tidy_model = tidy.up(model2)
output_file = file.path(cdir, "model_summary_donation_int.csv")
write.csv(tidy_model, output_file, row.names = FALSE)

cat("\n \n Assumptions tests \n \n")
check_assumptions(model2)

cat("\n \n Donation models: differences in explained variance\n \n")
int_improvement = anova(model1,model2)
output(int_improvement)

sink()
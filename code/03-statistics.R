sink(file.path(cdir, "statistics.txt"))

df = clean_dataset

# Descriptives

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

# Hypothesis 1

cat("\n \n Hypothesis 1: t-tests \n \n")

cat("\n \n T-test cPEB \n \n")
var_test_result = var.test(cPEB ~ emo, data = df)
t_test_result = t.test(cPEB ~ emo, data = df)
output(t_test_result)
output(report(t_test_result))

cat("\n \n T-test WEPT \n \n")
var_test_result = var.test(wept ~ emo, data = df)
t_test_result = t.test(wept ~ emo, data = df)
output(t_test_result)
output(report(t_test_result))

cat("\n \n T-test Donation \n \n")
var_test_result = var.test(donation ~ emo, data = df)
t_test_result = t.test(donation ~ emo, data = df)
output(t_test_result)
output(report(t_test_result))

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

cat("\n \n Hypothesis 4: Regression models \n \n")

# cPEB

cat("\n \n cPEB model: without interactions \n \n")
model1 = lm(cPEB ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS 
           + sex + age + res + edu + kid + ses, data = df)

output(summary(model1))
tidy_model = tidy(model1)
tidy_model$stats = ifelse(tidy_model$p.value > 0.05, "ns", 
                          ifelse(tidy_model$p.value < 0.001, "p<0.001", 
                                 formatC(tidy_model$p.value, format = "f", digits = 2)))
tidy_model$estimate <- round(tidy_model$estimate, 3)
tidy_model = tidy_model[, c("term", "estimate", "stats")]
output_file = file.path(cdir, "model_summary_cPEB.csv")
write.csv(tidy_model, output_file, row.names = FALSE)

cat("\n \n Assumptions tests \n \n")

check_assumptions(model1)

cat("\n \n cPEB model: with interactions \n \n")
model2 = lm(cPEB ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS 
            + sex + age + res + edu + kid + ses+ category:valence + category:arousal + category:bcc 
           + category:ccc + category:PCAE + category:PD + category:WTS 
           + category:sex + category:age + category:res + category:edu 
           + category:kid + category:ses, data = df) 

output(summary(model2))
tidy_model = tidy(model2)
tidy_model$stats = ifelse(tidy_model$p.value > 0.05, "ns", 
                          ifelse(tidy_model$p.value < 0.001, "p<0.001", 
                                 formatC(tidy_model$p.value, format = "f", digits = 2)))
tidy_model$estimate <- round(tidy_model$estimate, 3)
tidy_model = tidy_model[, c("term", "estimate", "stats")]
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
tidy_model = tidy(model1)
tidy_model$stats = ifelse(tidy_model$p.value > 0.05, "ns", 
                          ifelse(tidy_model$p.value < 0.001, "p<0.001", 
                                 formatC(tidy_model$p.value, format = "f", digits = 2)))
tidy_model$estimate <- round(tidy_model$estimate, 3)
tidy_model = tidy_model[, c("term", "estimate", "stats")]
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
tidy_model = tidy(model2)
tidy_model$stats = ifelse(tidy_model$p.value > 0.05, "ns", 
                          ifelse(tidy_model$p.value < 0.001, "p<0.001", 
                                 formatC(tidy_model$p.value, format = "f", digits = 2)))
tidy_model$estimate <- round(tidy_model$estimate, 3)
tidy_model = tidy_model[, c("term", "estimate", "stats")]
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
tidy_model = tidy(model1)
tidy_model$stats = ifelse(tidy_model$p.value > 0.05, "ns", 
                          ifelse(tidy_model$p.value < 0.001, "p<0.001", 
                                 formatC(tidy_model$p.value, format = "f", digits = 2)))
tidy_model$estimate <- round(tidy_model$estimate, 3)
tidy_model = tidy_model[, c("term", "estimate", "stats")]
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
tidy_model = tidy(model2)
tidy_model$stats = ifelse(tidy_model$p.value > 0.05, "ns", 
                           ifelse(tidy_model$p.value < 0.001, "p<0.001", 
                                  formatC(tidy_model$p.value, format = "f", digits = 2)))
tidy_model$estimate <- round(tidy_model$estimate, 3)
tidy_model = tidy_model[, c("term", "estimate", "stats")]
output_file = file.path(cdir, "model_summary_donation_int.csv")
write.csv(tidy_model, output_file, row.names = FALSE)

cat("\n \n Assumptions tests \n \n")
check_assumptions(model2)

cat("\n \n Donation models: differences in explained variance\n \n")
int_improvement = anova(model1,model2)
output(int_improvement)

sink()
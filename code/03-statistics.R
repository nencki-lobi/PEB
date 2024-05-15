sink(file.path(cdir, "statistics.txt"))

df = clean_dataset

# Descriptives

cat("\n \n Descriptives \n \n")

desc_total = df %>%
  summarise(across(where(is.numeric), list(
    Mean = mean,
    Median = median,
    Sum = sum,
    SD = sd
  ), na.rm = TRUE))

print(desc_total)

desc = describeBy(aim + wept + donation + cPEB ~ category, data = df)
print(desc)

cat("\n \n Correlation between PEB measures (WEPT and donations) \n \n")
correlation = cor(df$wept, df$donation)
output(correlation)

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

cat("\n \n Hypothesis 4: Regression models \n \n")

# cPEB
cat("\n \n cPEB model: with interactions \n \n")
model = lm(cPEB ~ category + category:valence + category:arousal + category:bcc 
           + category:ccc + category:PCAE + category:PD + category:WTS 
           + category:sex + category:age + category:res + category:edu 
           + category:kid + category:ses, data = df) 
output(summary(model))

cat("\n \n cPEB model: without interactions \n \n")
model = lm(cPEB ~ valence + arousal + bcc + ccc + PCAE + PD + WTS 
           + sex + age + res + edu + kid + ses, data = df)
output(summary(model))

# WEPT
cat("\n \n WEPT model: with interactions \n \n")
model = lm(wept ~ category + category:valence + category:arousal + category:bcc 
           + category:ccc + category:PCAE + category:PD + category:WTS 
           + category:sex + category:age + category:res + category:edu 
           + category:kid + category:ses, data = df) 
output(summary(model))

cat("\n \n WEPT model: without interactions \n \n")
model = lm(wept ~ valence + arousal + bcc + ccc + PCAE + PD + WTS 
           + sex + age + res + edu + kid + ses, data = df)
output(summary(model))

# donation
cat("\n \n Donation model: with interactions \n \n")
model = lm(donation ~ category + category:valence + category:arousal + category:bcc 
           + category:ccc + category:PCAE + category:PD + category:WTS 
           + category:sex + category:age + category:res + category:edu 
           + category:kid + category:ses, data = df) 
output(summary(model))

cat("\n \n Donation model: without interactions \n \n")
model = lm(donation ~ valence + arousal + bcc + ccc + PCAE + PD + WTS 
           + sex + age + res + edu + kid + ses, data = df)
output(summary(model))

## Hypothesis 4: UNREGISTERED

cat("\n \n Hypothesis 4: SEM models (UNREGISTERED) \n \n")

df2 = clean_dataset # use a fresh dataframe, as `lavaan` requires some of the vars changed

cat("\n \n SEM model: with interactions \n \n")

#' This model includes interaction terms in the equations for both dependent 
#' variables (donation and WEPT). Thus, it investigates moderators between PEBs 
#' and category.

model = '
donation ~ category*valence + category*arousal + category*bcc + category*ccc + category*PCAE + category*PD + category*WTS + category*sex + category*age + category*res + category*edu + category*kid + category*ses
wept ~ category*valence + category*arousal + category*bcc + category*ccc + category*PCAE + category*PD + category*WTS + category*sex + category*age + category*res + category*edu + category*kid + category*ses
donation ~~ wept
#category ~ valence + arousal + bcc + ccc + PCAE + PD + WTS + sex + age + res + edu + kid + ses
'

fit = sem(model, data = df2)
output(summary(fit, standardized = TRUE))

cat("\n \n SEM model: without interactions \n \n")

#' This model assumes a linear relationship between the dependent variables 
#' (donation and WEPT) and the independent variables, without considering any 
#' interaction effects.

model = '
donation ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS + sex + age + res + edu + kid + ses
wept  ~ category + valence + arousal + bcc + ccc + PCAE + PD + WTS + sex + age + res + edu + kid + ses
donation ~~ wept
category ~ valence + arousal + bcc + ccc + PCAE + PD + WTS + sex + age + res + edu + kid + ses
'

df2$category = ordered(df2$category) # `lavaan` requires `category` to be either numeric or ordered

fit = sem(model, data = df2)
output(summary(fit, standardized = TRUE))

sink()
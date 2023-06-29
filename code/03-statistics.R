odir = "./output"
if (!dir.exists(odir)) {dir.create(odir)}

fdir.create = function(name) {
  fdir = file.path(odir, name)
  if (!dir.exists(fdir)) {dir.create(fdir)}
  fdir}

fdir = fdir.create("Statistics")

sink("./output/Statistics/statistics.txt")

#Hypothesis 1
cat("\n \n Hypothesis 1: t-test \n \n")
var_test_result = var.test(cPEB ~ emo, data = df)
t_test_result = t.test(cPEB ~ emo, data = df)
t_test_result
report(t_test_result)

## unregistered analysis

cat("\n \n Hypothesis 1: unregistered MANOVA \n \n")
manova_result = manova(cbind(wept, donation) ~ emo, data = df)
summary.aov(manova_result)
report(manova_result)

#Hypothesis 2
cat("\n \n Hypothesis 2: ANOVA \n \n")

cat("\n \n ANOVA Donation \n \n")
anova_result = aov(donation ~ category, data = df)
summary(anova_result)
report(anova_result)

cat("\n \n Posthoc Donation \n \n")
posthoc_result = TukeyHSD(anova_result)
print(posthoc_result)

cat("\n \n ANOVA WEPT \n \n")
anova_result = aov(wept ~ category, data = df)
summary(anova_result)
report(anova_result)

cat("\n \n Posthoc WEPT \n \n")
posthoc_result = TukeyHSD(anova_result)
print(posthoc_result)

cat("\n \n ANOVA cPEB \n \n")
anova_result = aov(cPEB ~ category, data = df)
summary(anova_result)
report(anova_result)

cat("\n \n Posthoc cPEB \n \n")
posthoc_result = TukeyHSD(anova_result)
print(posthoc_result)

#Hypothesis 3
cat("\n \n Hypothesis 3: chi-square \n \n")
freq_aim = table(df$category, df$aim)
freq_aim
percentages_aim = prop.table(freq_aim, margin = 1) * 100
percentages_aim
chi_square_test = chisq.test(df$aim, df$category)
chi_square_test

#Donation aim preference in comparison to NEU as baseline
cat("\n \n Donation aim preference in comparison to NEU as baseline \n \n")
ANG_freq = c(28.488372, 28.488372, 39.534884, 3.488372)
COM_freq = c(20.689655, 38.505747, 33.908046, 6.896552)
HOP_freq = c(14.285714, 36.904762, 41.666667, 7.142857)
NEU_freq = c(20, 34.285714, 40, 5.714286)
expected = NEU_freq / sum(NEU_freq)

observed_list = list(ANG_freq, COM_freq, HOP_freq)
results = list()

for (observed in observed_list) {
  result = chisq.test(observed, p = expected)
  results = c(results, list(result))
}

for (i in seq_along(results)) {
  cat("Test", i, ":\n")
  print(results[[i]])
  cat("\n")
}
#Hypothesis 4

cat("\n \n Hypothesis 4: regression models \n \n")

#cPEB
cat("\n \n Model for cPEB \n \n")
model = lm(cPEB ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
category:bcc + category:ccc + category:valence + category:arousal + category:PCAE + category:PD + category:WTS, data = df) 
report(model)
summary(model)

#wept
cat("\n \n Model for WEPT \n \n")
model = lm(wept ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
             category:bcc + category:ccc + category:valence + category:arousal + category:PCAE + category:PD + category:WTS, data = df) 
#report(model)
summary(model)

#donation
cat("\n \n Model for donation \n \n")
model = lm(donation ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
             category:bcc + category:ccc + category:valence + category:arousal + category:PCAE + category:PD + category:WTS, data = df) 
#report(model)
summary(model)

##unregistered analyses
cat("\n \n Hypothesis 4: unregisterd models \n \n")
##no interaction effects
model = lm(cPEB ~ sex + age + res + edu + kid + ses +
             bcc + ccc + valence + arousal + PCAE + PD + WTS, data = df)

## only age and interation with age
model = lm(cPEB ~ category + age + category:age, data = df) 

## SEM no interactions
cat("\n \n  SEM 1: This model assumes a linear relationship between the dependent variable (donation and wept) and the independent variables (category, sex, age, etc.), without considering any interaction effects. \n \n")
model = '
donation ~ category + sex + age + res + edu + kid + ses +
             bcc + ccc + valence + arousal + PCAE + PD + WTS
wept ~ category + sex + age + res + edu + kid + ses +
             bcc + ccc + valence + arousal + PCAE + PD + WTS

donation ~~ wept

category ~ sex + age + res + edu + kid + ses +
             bcc + ccc + valence + arousal + PCAE + PD + WTS
'

fit = sem(model, data = df)
summary(fit, standardized = TRUE)

## SEM interactions
cat("\n \n  SEM 2: This model includes interaction terms (category * sex, category * age, etc.) in the equations for both donation and wept. This means that it investigates moderators between PEBs and category. \n \n")

model = '
donation ~ category * sex + category * age + category * res + category * edu +
             category * kid + category * ses + category * bcc + category * ccc +
             category * valence + category * arousal + category * PCAE + category * PD + category * WTS
wept ~ category * sex + category * age + category * res + category * edu +
             category * kid + category * ses + category * bcc + category * ccc +
             category * valence + category * arousal + category * PCAE + category * PD + category * WTS

donation ~~ wept

category ~ sex + age + res + edu + kid + ses +
             bcc + ccc + valence + arousal + PCAE + PD + WTS
'

fit = sem(model, data = df)
summary(fit, standardized = TRUE)

#Other analyses
cat("\n \n Other unregistered analyses \n")

## Correlation between WEPT and donations
cat("\n \n Correlation between WEPT and donations \n \n")
correlation = df %>%
  group_by(wept) %>%
  na.omit() %>%
  summarise(mean_donation = mean(donation), mean_wept = mean(wept))

cor(correlation$mean_wept, correlation$mean_donation)

## Correlation of gen/age and cPEB

cat("\n \n Number of participants from each generation \n \n")
counts = df %>%
  group_by(gen) %>%
  summarize(count = n())
counts

### Gen
cat("\n \n Correlation between generation and cPEB \n \n")
test = aov(donation ~ gen, data=df)
summary(test)
TukeyHSD(test)
report(test)

### Age
cat("\n \n Correlation between age and cPEB \n \n")
test= lm(cPEB ~ age, data = df)
summary(test)
report(test)

sink()

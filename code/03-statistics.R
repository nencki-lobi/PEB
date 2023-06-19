odir = "./output"
if (!dir.exists(odir)) {dir.create(odir)}

fdir.create = function(name) {
  fdir = file.path(odir, name)
  if (!dir.exists(fdir)) {dir.create(fdir)}
  fdir}

fdir = fdir.create("Statistics")

sink("./output/Statistics/statistics.txt")

df = full_join(conditions_filtered, pebs, by = "sid") %>%
  full_join(demo_transposed, by = 'sid') %>%
  full_join(ratings_transposed, by = 'sid') %>%
  full_join(questionnaires_transposed, by = 'sid') %>%
  select(sid, stid, category.x, int, aim, wept, donation, cPEB, sex, age, res, edu, kid, ses, bcc, ccc, val, aro, hop, com, ang, PCAE_i, PCAE_c, PCAE, PD, WTS)  %>%
  rename(category = category.x) %>%
  mutate(emo = ifelse(category == " NEU", 0, 1)) %>%
  na.omit()

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

anova_result = aov(donation ~ category, data = df)
summary(anova_result)
report(anova_result)

#posthoc_results = TukeyHSD(anova_result)

anova_result = aov(wept ~ category, data = df)
summary(anova_result)
report(anova_result)

#posthoc_results = TukeyHSD(anova_result)

anova_result = aov(cPEB ~ category, data = df)
summary(anova_result)
report(anova_result)

#posthoc_results = TukeyHSD(anova_result)

#Hypothesis 3
cat("\n \n Hypothesis 3: chi-square \n \n")
freq_aim = table(df$category, df$aim)
freq_aim
percentages_aim = prop.table(freq_aim, margin = 1) * 100
percentages_aim
chi_square_test = chisq.test(df$aim, df$category)
chi_square_test

#Hypothesis 4

cat("\n \n Hypothesis 4: regression models \n \n")

#cPEB
model = lm(cPEB ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
category:bcc + category:ccc + category:val + category:aro + category:PCAE + category:PD + category:WTS, data = df) 
report(model)
summary(model)

#wept
model = lm(wept ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
             category:bcc + category:ccc + category:val + category:aro + category:PCAE + category:PD + category:WTS, data = df) 
report(model)
summary(model)

#donation
model = lm(donation ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
             category:bcc + category:ccc + category:val + category:aro + category:PCAE + category:PD + category:WTS, data = df) 
report(model)
summary(model)

##unregistered analyses
cat("\n \n Hypothesis 4: unregisterd models \n \n")
model = lm(cPEB ~ sex + age + res + edu + kid + ses +
             bcc + ccc + val + aro + PCAE + PD + WTS, data = df)

model = lm(cPEB ~ category + age + category:age, data = df) 

model = lm(donation ~ category + category:int + category:aim + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
             category:bcc + category:ccc + category:val + category:aro + category:ang + category:hop + category:com + category:PCAE + category:PCAE_i + category:PCAE_c  + category:PD + category:WTS, data = df) 

model = lm(donation ~ category + category:int + category:aim, data = df)

  
model = '
donation ~ category + sex + age + res + edu + kid + ses +
             bcc + ccc + val + aro + PCAE + PD + WTS
wept ~ category + sex + age + res + edu + kid + ses +
             bcc + ccc + val + aro + PCAE + PD + WTS

donation ~~ wept

category ~ sex + age + res + edu + kid + ses +
             bcc + ccc + val + aro + PCAE + PD + WTS
'

fit = sem(model, data = df)
summary(fit, standardized = TRUE)

model = '
donation ~ category * sex + category * age + category * res + category * edu +
             category * kid + category * ses + category * bcc + category * ccc +
             category * val + category * aro + category * PCAE + category * PD + category * WTS
wept ~ category * sex + category * age + category * res + category * edu +
             category * kid + category * ses + category * bcc + category * ccc +
             category * val + category * aro + category * PCAE + category * PD + category * WTS

donation ~~ wept

category ~ sex + age + res + edu + kid + ses +
             bcc + ccc + val + aro + PCAE + PD + WTS
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

sink()
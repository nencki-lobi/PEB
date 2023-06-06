library('ggplot2')
library('report')

#Hypothesis 1

df =  full_join(conditions_filtered, pebs, by = "sid") %>%
  select(sid, stid, category.x, int, aim, wept, donation, cPEB) %>%
  mutate(emo = ifelse(category.x == " NEU", 0, 1)) %>%
  na.omit()

var_test_result <- var.test(cPEB ~ emo, data = df)
t_test_result = t.test(cPEB ~ emo, data = df)



## unregistered analysis
manova_result = manova(cbind(wept, donation) ~ emo, data = df)
summary.aov(manova_result)

#Hypothesis 2

anova_result = aov(donation ~ category, data = pebs)
summary(anova_result)
report(anova_result)

anova_result = aov(wept ~ category, data = pebs)
summary(anova_result)
report(anova_result)

anova_result = aov(cPEB ~ category, data = pebs)
summary(anova_result)
report(anova_result)
ggplot(pebs, aes(x=category, y=donation, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Donation")

ggplot(pebs, aes(x=category, y=wept, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Completed WEPT pages")

ggplot(pebs, aes(x=category, y=cPEB, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Summary cPEB score")

#Hypothesis 3

freq_aim = table(df$category, df$aim)
print(freq_aim)
percentages_aim = prop.table(freq_aim, margin = 1) * 100
print(percentages_aim)
chi_square_test = chisq.test(df$aim, df$category)
print(chi_square_test)

#Hypothesis 4

df = full_join(conditions_filtered, pebs, by = "sid") %>%
  full_join(demo_transposed, by = 'sid') %>%
  full_join(ratings_transposed, by = 'sid') %>%
  full_join(questionnaires_transposed, by = 'sid') %>%
  select(sid, stid, category.x, int, aim, wept, donation, cPEB, sex, age, res, edu, kid, ses, bcc, ccc, val, aro, hop, com, ang, PCAE_i, PCAE_c, PCAE, PD, WTS)  %>%
  rename(category = category.x) %>%
  na.omit()


model = lm(cPEB ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
category:bcc + category:ccc + category:val + category:aro + category:PCAE + category:PD + category:WTS, data = df) 
report(model)
summary(model)
model = lm(wept ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
             category:bcc + category:ccc + category:val + category:aro + category:PCAE + category:PD + category:WTS, data = df) 
report(model)
summary(model)
model = lm(donation ~ category + category:sex + category:age + category:res + category:edu + category:kid + category:ses +
             category:bcc + category:ccc + category:val + category:aro + category:PCAE + category:PD + category:WTS, data = df) 
report(model)
summary(model)

##unregistered analysis
model = lm(cPEB ~ sex + age + res + edu + kid + ses +
             bcc + ccc + val + aro + PCAE + PD + WTS, data = df)

report(model)
summary(model)

#Other analyses

## Correlation between WEPT and donations
correlation = pebs %>%
  group_by(wept) %>%
  na.omit() %>%
  summarise(mean_donation = mean(donation))
  
##bar
correlation_plot = ggplot(correlation, aes(x = wept, y = mean_donation)) +
  geom_bar(stat = "identity") +
  labs(x = "Wept", y = "Mean Donation") +
  ggtitle("Mean Donation for Each Wept Level")

print(correlation_plot)

##point
correlation_plot = ggplot(correlation, aes(x = wept, y = donation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Wept", y = "Donation") +
  ggtitle("Correlation between Wept and Donation")

## stat
print(correlation_plot)
cor(correlation$wept, correlation$donation)

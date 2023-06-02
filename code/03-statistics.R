library('ggplot2')

#Hypothesis 1

df =  full_join(conditions_filtered, pebs, by = "sid") %>%
  select(sid, stid, category.x, int, aim, wept, donation, cPEB) %>%
  mutate(emo = ifelse(category.x == " NEU", 0, 1)) %>%
  na.omit()

t_test_result = t.test(cPEB ~ emo, data = df)

# unregistered analysis
manova_result = manova(cbind(wept, donation, cPEB) ~ emo, data = df)
summary.aov(manova_result)

#Hypothesis 2

anova_result = aov(donation ~ category, data = pebs)
summary(anova_result)

anova_result = aov(wept ~ category, data = pebs)
summary(anova_result)

anova_result = aov(cPEB ~ category, data = pebs)
summary(anova_result)

ggplot(pebs, aes(x=category, y=donation, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Donation")

ggplot(pebs, aes(x=category, y=wept, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Completed WEPT pages")

ggplot(pebs, aes(x=category, y=cPEB, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Summary cPEB score")
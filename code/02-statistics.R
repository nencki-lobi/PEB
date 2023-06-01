library('ggplot2')

#Hypothesis 1

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
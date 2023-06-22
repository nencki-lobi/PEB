fdir = fdir.create("H1")

#Hypothesis 1

summary_df = aggregate(donation ~ emo, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
  
p = ggplot(summary_df, aes(x = factor(emo), y = donation[,"mean"])) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = donation[,"mean"] - donation[,"sd"], ymax = donation[,"mean"] + donation[,"sd"]),
                width = 0.2, color = "black", size = 0.7) +
  xlab("Emo") + ylab("Mean Donation")
ggsave("H1 - M-effect-donation.png", p, width = 10, height = 10, path = fdir)

summary_df = aggregate(wept ~ emo, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x = factor(emo), y = wept[,"mean"])) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = wept[,"mean"] - wept[,"sd"], ymax = wept[,"mean"] + wept[,"sd"]),
                width = 0.2, color = "black", size = 0.7) +
  xlab("Emo") + ylab("Mean wept")
ggsave("H1 - M-effect-wept.png", p, width = 10, height = 10, path = fdir)


summary_df = aggregate(cPEB ~ emo, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x = factor(emo), y = cPEB[,"mean"])) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = cPEB[,"mean"] - cPEB[,"sd"], ymax = cPEB[,"mean"] + cPEB[,"sd"]),
                width = 0.2, color = "black", size = 0.7) +
  xlab("Emo") + ylab("Mean cPEB")
ggsave("H1 - M-effect-cPEB.png", p, width = 10, height = 10, path = fdir)


fdir = fdir.create("H2")

#Hypothesis 2

##median
p = ggplot(df, aes(x=category, y=donation, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Median donation")
ggsave("H2 - Me-effect-donation.png", p, width = 10, height = 10, path = fdir)

p = ggplot(df, aes(x=category, y=wept, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Completed WEPT pages")
ggsave("H2 - Me-effect-WEPT.png", p, width = 10, height = 10, path = fdir)


p = ggplot(df, aes(x=category, y=cPEB, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Summary cPEB score")
ggsave("H2 - Me-effect-cPEB.png", p, width = 10, height = 10, path = fdir)


##mean

summary_df = aggregate(donation ~ category, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x=category, y=donation[,"mean"], fill=category)) +
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Mean donation")
ggsave("H2 - M-effect-donation.png", p, width = 10, height = 10, path = fdir)

summary_df = aggregate(wept ~ category, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x=category, y=wept[,"mean"], fill=category)) +
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Completed WEPT pages")
ggsave("H2 - M-effect-WEPT.png", p, width = 10, height = 10, path = fdir)

summary_df = aggregate(cPEB ~ category, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x=category, y=cPEB[,"mean"], fill=category)) +
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Summary cPEB score")
ggsave("H2 - M-effect-cPEB.png", p, width = 10, height = 10, path = fdir)

fdir = fdir.create("Unregistered")

# Correlation between WEPT and donations

## bar
p = correlation_plot = ggplot(correlation, aes(x = wept, y = mean_donation)) +
  geom_bar(stat = "identity") +
  labs(x = "Wept", y = "Mean Donation") +
  ggtitle("Mean Donation for Each Wept Level")
ggsave("Correlation-WEPT-donation-bar.png", p, width = 10, height = 10, path = fdir)

## point
p = correlation_plot = ggplot(correlation, aes(x = wept, y = mean_donation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Wept", y = "Donation") +
  ggtitle("Correlation between Wept and Donation")
ggsave("Correlation-WEPT-donation-point.png", p, width = 10, height = 10, path = fdir)

# Manipulation check - unregistered

##All participants

p = ggplot(manipulation_check, aes(x=sid, y=opt, colour=category)) + 
  geom_point() +
  xlab("Participant") + ylab("Ratings") +
  facet_grid(category ~ part, labeller = as_labeller(part_to_scale))
ggsave("Manipulation_check_1.png", p, width = 20, height = 10, path = fdir)

p = ggplot(manipulation_check, aes(x=opt)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  xlab("Rating on emotion rating scale") + ylab("Frequency") +
  xlim(c(-1,100)) + ylim(c(-1,60)) +
  facet_grid(category ~ part, labeller = labeller(part = as_labeller(part_to_scale), 
                                                  category = as_labeller(labels_conditions)))
ggsave("Manipulation_check_2.png", p, width = 20, height = 10, path = fdir)

##soft filter manipulation check

p = ggplot(soft_check, aes(x=sid, y=opt, colour=category)) + 
  geom_point() +
  xlab("Participant") + ylab("Ratings") +
  facet_grid(category ~ part, labeller = as_labeller(part_to_scale))
ggsave("soft_filter_group_1.png", p, width = 20, height = 10, path = fdir)

p = ggplot(soft_check, aes(x=opt)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  xlab("Rating on emotion rating scale") + ylab("Frequency") +
  xlim(c(-1,100)) + ylim(c(-1,60)) +
  facet_grid(category ~ part, labeller = labeller(part = as_labeller(part_to_scale), 
                                                  category = as_labeller(labels_conditions)))

##hard filter manipulation check
p = ggplot(hard_check, aes(x=sid, y=opt, colour=category)) + 
  geom_point() +
  xlab("Participant") + ylab("Ratings") +
  facet_grid(category ~ part, labeller = as_labeller(part_to_scale))
ggsave("hard_filter_group_1.png", p, width = 20, height = 10, path = fdir)

p = ggplot(hard_check, aes(x=opt)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  xlab("Rating on emotion rating scale") + ylab("Frequency") +
  xlim(c(-1,100)) + ylim(c(-1,60)) +
  facet_grid(category ~ part, labeller = labeller(part = as_labeller(part_to_scale), 
                                                  category = as_labeller(labels_conditions)))
ggsave("hard_filter_group_2.png", p, width = 20, height = 10, path = fdir)

# Descriptives

belief = df%>%
  ggplot(aes(x = as.numeric(bcc))) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(title = "Histogram of bcc", x = "bcc", y = "Frequency")
ggsave("belief.png", belief, width = 10, height = 10, path = "./output/Descriptives")

concern = df %>%
  ggplot(aes(x = as.numeric(ccc))) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(title = "Histogram of ccc", x = "ccc", y = "Frequency")
ggsave("concern.png", concern, width = 10, height = 10, path = "./output/Descriptives")

socio_status = df %>%
  ggplot(aes(x = as.numeric(ses))) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(title = "Histogram of ses", x = "ses", y = "Frequency")
ggsave("sociostatus.png", socio_status, width = 10, height = 10, path = "./output/Descriptives")



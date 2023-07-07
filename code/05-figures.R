fdir = fdir.create("Figures")

#Hypothesis 1
df$category = as.factor(df$category)


summary_df = aggregate(donation ~ emo, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
  
p = ggplot(summary_df, aes(x = factor(emo), y = donation[,"mean"])) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = donation[,"mean"] - donation[,"sd"], ymax = donation[,"mean"] + donation[,"sd"]),
                width = 0.2, color = "black", size = 0.7) +
  xlab("Emo") + ylab("Mean Donation")
ggsave("H1-M-donation.png", p, width = 10, height = 10, path = fdir)

summary_df = aggregate(wept ~ emo, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x = factor(emo), y = wept[,"mean"])) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = wept[,"mean"] - wept[,"sd"], ymax = wept[,"mean"] + wept[,"sd"]),
                width = 0.2, color = "black", size = 0.7) +
  xlab("Emo") + ylab("Mean wept")
ggsave("H1-M-wept.png", p, width = 10, height = 10, path = fdir)


summary_df = aggregate(cPEB ~ emo, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x = factor(emo), y = cPEB[,"mean"])) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = cPEB[,"mean"] - cPEB[,"sd"], ymax = cPEB[,"mean"] + cPEB[,"sd"]),
                width = 0.2, color = "black", size = 0.7) +
  xlab("Emo") + ylab("Mean cPEB")
ggsave("H1-M-cPEB.png", p, width = 10, height = 10, path = fdir)

#Hypothesis 2

##median
p = ggplot(df, aes(x=category, y=donation, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Median donation")
ggsave("H2-Me-donation.png", p, width = 10, height = 10, path = fdir)

p = ggplot(df, aes(x=category, y=wept, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Completed WEPT pages")
ggsave("H2-Me-WEPT.png", p, width = 10, height = 10, path = fdir)

p = ggplot(df, aes(x=category, y=cPEB, fill=category)) +
  geom_boxplot(notch = TRUE) +
  xlab("Category") + ylab("Summary cPEB score")
ggsave("H2-Me-cPEB.png", p, width = 10, height = 10, path = fdir)

##mean

summary_df = aggregate(donation ~ category, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x=category, y=donation[,"mean"], fill=category)) +
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Mean donation")
ggsave("H2-M-donation.png", p, width = 10, height = 10, path = fdir)

summary_df = aggregate(wept ~ category, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x=category, y=wept[,"mean"], fill=category)) +
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Completed WEPT pages")
ggsave("H2-M-WEPT.png", p, width = 10, height = 10, path = fdir)

summary_df = aggregate(cPEB ~ category, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

p = ggplot(summary_df, aes(x=category, y=cPEB[,"mean"], fill=category)) +
  geom_bar(stat = "identity") +
  xlab("Category") + ylab("Summary cPEB score")
ggsave("H2-M-cPEB.png", p, width = 10, height = 10, path = fdir)

fdir = fdir.create("Unregistered")

# Correlation between WEPT and MEAN donations

## bar
p = ggplot(correlation, aes(x = wept, y = mean_donation)) +
  geom_bar(stat = "identity") +
  labs(x = "Wept", y = "Mean Donation") +
  ggtitle("Mean Donation for Each Wept Level")
ggsave("F1-Cor-WEPT-donation-bar.png", p, width = 10, height = 10, path = fdir)

## point
p = ggplot(correlation, aes(x = wept, y = mean_donation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Wept", y = "Donation") +
  ggtitle("Correlation between Wept and Donation")
ggsave("F2-Cor-WEPT-donation-point.png", p, width = 10, height = 10, path = fdir)

## Correlation between WEPT and donations

p = ggplot(df, aes(x = wept, y = donation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Wept", y = "Donation") +
  ggtitle("F3-Cor-WEPT-donation")

## Correlation between Gen and cPEB

means = aggregate(df$cPEB, by=list(df$gen), FUN=mean) %>%
  filter(Group.1 !='Silent')
colnames(means) = c("gen", "mean_cPEB")

p = ggplot(data = means, aes(x = gen, y = mean_cPEB)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Group Differences",
       x = "Gen",
       y = "Mean cPEB")
ggsave("F4-gen_cPEB.png", p, width = 10, height = 10, path = fdir)

## Correlation between Age and cPEB

p = ggplot(data = df, aes(x = age, y = cPEB)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between cPEB and Age",
       x = "Age",
       y = "cPEB")
ggsave("F5-age_cPEB.png", p, width = 10, height = 10, path = fdir)

# Manipulation check - unregistered

##All participants

p = ggplot(manipulation_check, aes(x=sid, y=opt, colour=category)) + 
  geom_point() +
  xlab("Participant") + ylab("Ratings") +
  facet_grid(category ~ part, labeller = as_labeller(part_to_scale))
ggsave("C1-Manipulation_check_point.png", p, width = 20, height = 10, path = fdir)

p = ggplot(manipulation_check, aes(x=opt)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  xlab("Rating on emotion rating scale") + ylab("Frequency") +
  xlim(c(-1,100)) + ylim(c(-1,60)) +
  facet_grid(category ~ part, labeller = labeller(part = as_labeller(part_to_scale), 
                                                  category = as_labeller(labels_conditions)))
ggsave("C2-Manipulation_check_bar.png", p, width = 20, height = 10, path = fdir)

##soft filter manipulation check

p = ggplot(soft_check, aes(x=sid, y=opt, colour=category)) + 
  geom_point() +
  xlab("Participant") + ylab("Ratings") +
  facet_grid(category ~ part, labeller = as_labeller(part_to_scale))
ggsave("C3-Soft_check_point.png", p, width = 20, height = 10, path = fdir)

p = ggplot(soft_check, aes(x=opt)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  xlab("Rating on emotion rating scale") + ylab("Frequency") +
  xlim(c(-1,100)) + ylim(c(-1,60)) +
  facet_grid(category ~ part, labeller = labeller(part = as_labeller(part_to_scale), 
                                                  category = as_labeller(labels_conditions)))
ggsave("C4-Soft_check_bar.png", p, width = 20, height = 10, path = fdir)

##hard filter manipulation check
p = ggplot(hard_check, aes(x=sid, y=opt, colour=category)) + 
  geom_point() +
  xlab("Participant") + ylab("Ratings") +
  facet_grid(category ~ part, labeller = as_labeller(part_to_scale))
ggsave("C5-Hard_check_point.png", p, width = 20, height = 10, path = fdir)

p = ggplot(hard_check, aes(x=opt)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  xlab("Rating on emotion rating scale") + ylab("Frequency") +
  xlim(c(-1,100)) + ylim(c(-1,60)) +
  facet_grid(category ~ part, labeller = labeller(part = as_labeller(part_to_scale), 
                                                  category = as_labeller(labels_conditions)))
ggsave("C6-Hard_check_bar.png", p, width = 20, height = 10, path = fdir)

##excluded by soft_filter

p = ggplot(inverse_check, aes(x=sid, y=opt, colour=category)) + 
  geom_point() +
  xlab("Participant") + ylab("Ratings") +
  facet_grid(category ~ part, labeller = as_labeller(part_to_scale))
ggsave("C7-Inverse_check_point.png", p, width = 20, height = 10, path = fdir)

p = ggplot(inverse_check, aes(x=opt)) + 
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  xlab("Rating on emotion rating scale") + ylab("Frequency") +
  xlim(c(-1,100)) + ylim(c(-1,60)) +
  facet_grid(category ~ part, labeller = labeller(part = as_labeller(part_to_scale), 
                                                  category = as_labeller(labels_conditions)))
ggsave("C8-Inverse_check_bar.png", p, width = 20, height = 10, path = fdir)

# Reading time check - unregisterd

# Create bins for observations with at least 100 bins
num_bins = max(ceiling((max(time_check$time) - min(time_check$time)) / 100), 1)

# Plot histogram
p = time_check %>%
  ggplot(aes(x = time)) +
  geom_histogram(binwidth = (max(time_check$time) - min(time_check$time)) / num_bins,
                 color = "steelblue") +
  facet_wrap(~ category, ncol = 2) +
  labs(title = "Histogram of rating times", x = "time in miliseconds", y = "Frequency")
ggsave("C9-times.png", p, width = 10, height = 10, path = "./output/Unregistered")



# Descriptives

belief = df%>%
  ggplot(aes(x = as.numeric(bcc))) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(title = "Histogram of bcc", x = "bcc", y = "Frequency")
ggsave("D1-belief.png", belief, width = 10, height = 10, path = "./output/Descriptives")

concern = df %>%
  ggplot(aes(x = as.numeric(ccc))) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(title = "Histogram of ccc", x = "ccc", y = "Frequency")
ggsave("D2-concern.png", concern, width = 10, height = 10, path = "./output/Descriptives")

socio_status = df %>%
  ggplot(aes(x = as.numeric(ses))) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(title = "Histogram of ses", x = "ses", y = "Frequency")
ggsave("D3-sociostatus.png", socio_status, width = 10, height = 10, path = "./output/Descriptives")



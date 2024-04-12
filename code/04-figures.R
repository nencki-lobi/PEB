sdir = sdir.create("Figures")

df = clean_dataset

# Descriptives

p = ggplot(data = df, aes(x = bcc)) +
  geom_histogram(binwidth = 1, color="black", fill = "gray", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(x = "Climate change belief", y = "Frequency") + 
  beauty
ggsave("D1-belief.png", p, width = 4, height = 4, path = sdir)

p = ggplot(data = df, aes(x = ccc)) +
  geom_histogram(binwidth = 1, color="black", fill = "gray", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(x = "Climate change concern", y = "Frequency") + 
  beauty
ggsave("D2-concern.png", p, width = 4, height = 4, path = sdir)

# Hypothesis 1

## Visual inspection of WEPT & donation distribution

summary_df = df %>%
  group_by(donation, wept) %>%
  dplyr::summarise(n = n())

p = ggplot(data = summary_df, aes(x = wept, y = donation, fill = n)) + 
  geom_tile(color = "white") +
  scale_fill_gradient(low = "gray", high = "#E38421") +
  geom_text(aes(label = sprintf("%i", n)), vjust = 0.5) +
  labs(x = "Completed WEPT pages", y = "Donation") +
  guides(fill=guide_colorbar(ticks.colour = NA, title.hjust=0.2)) +
  beauty + theme(aspect.ratio = case_when(country == "PL" ~ 1.2, country == "NO" ~ 10))
ggsave("H1-WEPT-by-donation.png", p, 
       width = case_when(country == "PL" ~ 4, country == "NO" ~ 4),
       height = case_when(country == "PL" ~ 4, country == "NO" ~ 28),
       path = sdir)

## Mean plots

summary_df = summarySE(df, measurevar="cPEB", groupvars=c("emo"))

p = ggplot(summary_df, aes(x=emo, y=cPEB)) +
  geom_bar(stat = "identity", color="black", fill = colors_simple, width = 0.6) +
  geom_errorbar(aes(ymin=cPEB-ci, ymax=cPEB+ci), width = 0.4) +
  labs(x = "Condition", y = "cPEB score") + beauty
ggsave("H1-M-cPEB.png", p, width = 4, height = 4, path = sdir)

summary_df = summarySE(df, measurevar="wept", groupvars=c("emo"))

p = ggplot(summary_df, aes(x=emo, y=wept)) +
  geom_bar(stat = "identity", color="black", fill = colors_simple, width = 0.6) +
  geom_errorbar(aes(ymin=wept-ci, ymax=wept+ci), width = 0.4) +
  labs(x = "Condition", y = "Completed WEPT pages") + beauty
ggsave("H1-M-WEPT.png", p, width = 4, height = 4, path = sdir)

summary_df = summarySE(df, measurevar="donation", groupvars=c("emo"))

p = ggplot(summary_df, aes(x=emo, y=donation)) +
  geom_bar(stat = "identity", color="black", fill = colors_simple, width = 0.6) +
  geom_errorbar(aes(ymin=donation-ci, ymax=donation+ci), width = 0.4) +
  labs(x = "Condition", y = "Donation") + beauty
ggsave("H1-M-donation.png", p, width = 4, height = 4, path = sdir)

# Hypothesis 2

## Median plots

p = ggplot(df, aes(x=category, y=donation)) +
  geom_boxplot(fill=colors, notch = TRUE) +
  xlab("Condition") + ylab("Donation") + beauty
ggsave("H2-Me-donation.png", p, width = 4, height = 4, path = sdir)

p = ggplot(df, aes(x=category, y=wept)) +
  geom_boxplot(fill=colors, notch = TRUE) +
  xlab("Condition") + ylab("Completed WEPT pages") + beauty
ggsave("H2-Me-WEPT.png", p, width = 4, height = 4, path = sdir)

p = ggplot(df, aes(x=category, y=cPEB)) +
  geom_boxplot(fill=colors, notch = TRUE) +
  xlab("Condition") + ylab("cPEB score") + beauty
ggsave("H2-Me-cPEB.png", p, width = 4, height = 4, path = sdir)

## Mean plots

summary_df = summarySE(df, measurevar="cPEB", groupvars=c("category"))

p = ggplot(summary_df, aes(x=category, y=cPEB)) +
  geom_bar(stat = "identity", color="black", fill = colors, width = 0.6) +
  geom_errorbar(aes(ymin=cPEB-ci, ymax=cPEB+ci), width = 0.4) +
  labs(x = "Condition", y = "cPEB score") + beauty
ggsave("H2-M-cPEB.png", p, width = 4, height = 4, path = sdir)

summary_df = summarySE(df, measurevar="wept", groupvars=c("category"))

p = ggplot(summary_df, aes(x=category, y=wept)) +
  geom_bar(stat = "identity", color="black", fill = colors, width = 0.6) +
  geom_errorbar(aes(ymin=wept-ci, ymax=wept+ci), width = 0.4) +
  labs(x = "Condition", y = "Completed WEPT pages") + beauty
ggsave("H2-M-WEPT.png", p, width = 4, height = 4, path = sdir)

summary_df = summarySE(df, measurevar="donation", groupvars=c("category"))

p = ggplot(summary_df, aes(x=category, y=donation)) +
  geom_bar(stat = "identity", color="black", fill = colors, width = 0.6) +
  geom_errorbar(aes(ymin=donation-ci, ymax=donation+ci), width = 0.4) +
  labs(x = "Condition", y = "Donation") + beauty
ggsave("H2-M-donation.png", p, width = 4, height = 4, path = sdir)

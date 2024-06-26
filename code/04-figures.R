sdir = sdir.create("Figures")

df = clean_dataset

# Descriptives

p = ggplot(data = df, aes(x = bcc)) +
  geom_bar(color="black", fill = "gray") +
  facet_wrap(~category, ncol = 2) +
  labs(x = "Climate change belief", y = "Frequency") +
  scale_x_discrete(labels = str_wrap(levels(df$bcc), width = 10)) +
  beauty + theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
ggsave("D1-belief.png", p, width = 8, height = 8, path = sdir)

p = ggplot(data = df, aes(x = ccc)) +
  geom_bar(color="black", fill = "gray") +
  facet_wrap(~category, ncol = 2) +
  labs(x = "Climate change concern", y = "Frequency") + 
  scale_x_discrete(labels = str_wrap(levels(df$ccc), width = 10)) +
  beauty + theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
ggsave("D2-concern.png", p, width = 8, height = 8, path = sdir)

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


# Hypothesis 4 

plot.interactions.categorical = function(df, measure, predictor) {
  
  # NOTE: This function uses tidy evaluation pronoun .data
  
  thumb = 5 # rule of thumb to discard predictor levels with too little observations
  
  summary_df = summarySE(df, measurevar = measure, groupvars = c(predictor, "category"), .drop = F) %>%
    group_by(.data[[predictor]]) %>%
    filter(!any(N < thumb)) %>%
    ungroup()
  
  print(summary_df)
  
  pd = position_dodge(0.1) # move data points .05 to the left and right
  lab = levels(df[[predictor]])
  
  ggplot(summary_df, aes(x = .data[[predictor]], 
                         y = .data[[measure]], 
                         colour = .data[["category"]], 
                         group = .data[["category"]])) + 
    # geom_errorbar(aes(ymin = .data[[measure]] - ci, ymax = .data[[measure]] + ci), width =.1, position = pd) +
    geom_line(position = pd, linewidth = 0.7) +
    geom_point(position = pd) +
    scale_color_manual(guide = guide_legend(title = "Category"), values = colors) +
    scale_x_discrete(labels = str_wrap(lab, width = 10)) +
    beauty + theme(axis.title.x = element_blank(), 
                   axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))
}  

plot.interactions.continuous = function(df, measure, predictor) {
  
  # NOTE: This function uses tidy evaluation pronoun .data
  
  ggplot(df, aes(x = .data[[predictor]], 
                 y = .data[[measure]], 
                 colour = .data[["category"]], 
                 group = .data[["category"]])) +
    # geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    scale_color_manual(guide = guide_legend(title = "Category"), values = colors) +
    beauty + theme(axis.title.x = element_blank())
}

predictors_categorical = c("bcc", "ccc", "sex", "res", "edu", "kid", "ses")
predictors_continuous = c("valence", "arousal", "PCAE", "PD", "WTS", "age")

titles_categorical = c("CC belief", "CC concern", "Gender", "Place of residence", "Education", "Parenthood", "SES")
titles_continuous = c("Valence", "Arousal", "PCAE", "PD", "WTS", "Age")

npred = length(predictors_categorical)
for (pred in 1:npred) {
  
  predictor = predictors_categorical[pred]
  title = titles_categorical[pred]
  
  p1 = plot.interactions.categorical(df, "cPEB", predictor)
  p2 = plot.interactions.categorical(df, "wept", predictor)
  p3 = plot.interactions.categorical(df, "donation", predictor)
  
  p = p1 + p2 + p3 + 
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(title = paste(title, "-", country_full))
  
  file_name = paste0("H4-int-", predictor, ".png")
  ggsave(file_name, plot = p, width = 10, height = 4, path = sdir)
}

npred = length(predictors_continuous)
for (pred in 1:npred) {
  
  predictor = predictors_continuous[pred]
  title = titles_continuous[pred]
  
  p1 = plot.interactions.continuous(df, "cPEB", predictor)
  p2 = plot.interactions.continuous(df, "wept", predictor)
  p3 = plot.interactions.continuous(df, "donation", predictor)
  
  p = p1 + p2 + p3 + 
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(title = paste(title, "-", country_full))
  
  file_name = paste0("H4-int-", predictor, ".png")
  ggsave(file_name, plot = p, width = 10, height = 4, path = sdir)
}
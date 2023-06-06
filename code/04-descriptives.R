library(dplyr)
library(ggplot2)

summary_stats = demo_transposed %>%
  group_by(category)%>%
  summarise(
    bcc_mean = mean(bcc),
    bcc_sd = sd(bcc),
    ccc_mean = mean(ccc),
    ccc_sd = sd(ccc)
  )

frequencies = demo_transposed %>%
  group_by(category) %>%
  summarise(
    sex_freq = list(table(sex)),
    gen_freq = list(table(gen)),
    res_freq = list(table(res)),
    edu_freq = list(table(edu)),
    kid_freq = list(table(kid)),
    ses_freq = list(table(ses))
  )

sink("./data/S1/descriptives.txt")

print(summary_stats)

for (col in names(frequencies)[-1]) {
  cat("\n")
  cat("#", col, "\n")
  lapply(frequencies[[col]], print)
}

sink()


# Create histogram plots for the variables
belief = demo_transposed %>%
  ggplot(aes(x = bcc)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(title = "Histogram of bcc", x = "bcc", y = "Frequency")

concern = demo_transposed %>%
  ggplot(aes(x = ccc)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  facet_wrap(~category, ncol = 2) +
  labs(title = "Histogram of bcc", x = "bcc", y = "Frequency")



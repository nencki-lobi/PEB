df = full_join(conditions_filtered, pebs, by = "sid") %>%
  full_join(demo_transposed, by = 'sid') %>%
  full_join(ratings_transposed, by = 'sid') %>%
  full_join(questionnaires_transposed, by = 'sid') %>%
  select(sid, stid, category.x, int, aim, wept, donation, cPEB, sex, age, gen, res, edu, kid, ses, bcc, ccc, val, aro, hop, com, ang, PCAE_i, PCAE_c, PCAE, PD, WTS)  %>%
  rename(category = category.x) %>%
  mutate(emo = ifelse(category == " NEU", 0, 1)) %>%
  na.omit()

summary_stats = df %>%
  group_by(category)%>%
  na.omit()%>%
  summarise(
    bcc_mean = mean(as.numeric(bcc)),
    bcc_sd = sd(as.numeric(bcc)),
    ccc_mean = mean(as.numeric(ccc)),
    ccc_sd = sd(as.numeric(ccc))
  )

frequencies = df %>%
  group_by(category) %>%
  summarise(
    sex_freq = list(table(sex)),
    gen_freq = list(table(gen)),
    res_freq = list(table(res)),
    edu_freq = list(table(edu)),
    kid_freq = list(table(kid)),
    ses_freq = list(table(ses))
  )

fdir = fdir.create("Descriptives")

sink("./output/Descriptives/descriptives.txt")

print(summary_stats)
report(summary_stats)

for (col in names(frequencies)[-1]) {
  cat("\n")
  cat("#", col, "\n")
  lapply(frequencies[[col]], print)
}

sink()
conditions = read.table("./data/conditions.tsv", header = F, skip = 2, sep = "|", encoding = "UTF-8")
colnames(conditions) = c("sid", "stid", "category")

demo = read.table("./data/demographic.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(demo) = c("sid", "stid", "name", "ord", "val")

questionnaires = read.table("./data/questionnaires.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(questionnaires) = c("sid", "stid", "name", "ord", "opt")

ratings = read.table("./data/story-ratings.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(ratings) = c("sid", "stid", "name", "ord", "part", "opt")

intentions = read.table("./data/peb-intentions.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(intentions) = c("sid", "stid", "name", "ord", "opt")

weptings = read.table("./data/peb-weptings.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(weptings) = c("sid", "stid", "name", "ord", "mh", "fh")

donations = read.table("./data/peb-donations.tsv", header = F, skip = 2, sep = "|", encoding = "UTF-8")
colnames(donations) = c("sid", "stid", "name", "ord", "val")

demo_transposed = demo %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = "ord", 
              values_from = "val") %>%
  rename(sex = "0", birth = "2", res = "3", edu = "4", kid = "6", ses = "7", bcc = "8", ccc = "9") %>%
  mutate(birth = as.numeric(birth),  # Convert birth to numeric
         age = 2023 - birth,
         gen = case_when(
           between(birth, 1997, 2012) ~ "Z",
           between(birth, 1981, 1996) ~ "Y",
           between(birth, 1965, 1980) ~ "X",
           between(birth, 1946, 1964) ~ "Boomer",
           between(birth, 1928, 1945) ~ "Silent",
           TRUE ~ NA_character_
         )) %>%
  select(sid, stid, sex, age, gen, res, edu, kid, ses, bcc, ccc)

questionnaires_transposed = questionnaires %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = c("name","ord"), 
              values_from = "opt", 
              names_sep = ".") %>%
  mutate (PCAE_i = `PCAE-pl.0` + `PCAE-pl.1`, 
          PCAE_c = `PCAE-pl.2` + `PCAE-pl.3`, 
          PCAE = PCAE_i + PCAE_c, 
          PD = `PD-pl.0` + `PD-pl.1`, 
          WTS = `WTS-pl.0` + `WTS-pl.1` + `WTS-pl.2` + `WTS-pl.3` + `WTS-pl.4`) %>%
  select(sid, stid, PCAE_i, PCAE_c, PCAE, PD, WTS)

ratings_transposed = ratings %>%
  mutate(scale = factor(part_to_scale[as.character(part)], 
                        levels = labels_scales)) %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = "scale",
              values_from = "opt")
  
intentions_transposed = intentions %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = "ord", 
              values_from = "opt") %>%
  rename(int = "0", aim = "1") %>%
  select(sid, stid, int, aim)

weptings_transposed = weptings  %>%
  mutate(correct = mh + fh < 5) %>%
  group_by(sid, stid, correct) %>%
  summarise(pages = n()) %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = "correct", 
              values_from = "pages", 
              values_fill = 0) %>%
  rename(wept_cor = "TRUE", wept_inc = "FALSE") %>%
  mutate(wept = wept_cor + wept_inc)

donations_transposed = donations %>%
  rename(donation = val) %>%
  select(sid, stid, donation)

pebs = conditions %>%
  full_join(intentions_transposed, by = c("sid","stid")) %>%
  full_join(weptings_transposed, by = c("sid","stid")) %>%
  full_join(donations_transposed, by = c("sid","stid")) %>%
  mutate(cPEB = (wept/15 + donation/20) /2)
  #mutate(cPEB = (wept/15 + donation/100) /2)

df = full_join(conditions, pebs, by = "sid") %>%
  full_join(demo_transposed, by = 'sid') %>%
  full_join(ratings_transposed, by = 'sid') %>%
  full_join(questionnaires_transposed, by = 'sid') %>%
  select(sid, stid, category.x, int, aim, wept, donation, cPEB, sex, age, gen, res, edu, kid, ses, bcc, ccc, valence, arousal, anger, compassion, hope, PCAE_i, PCAE_c, PCAE, PD, WTS)  %>%
  rename(category = category.x) %>%
  mutate(emo = ifelse(category == " NEU", 0, 1)) %>%
  na.omit()

s_df = df %>%
  mutate(category = trimws(category)) %>%
  filter(category == "ANG" & as.numeric(anger) > 50 |
           category == "COM" & as.numeric(compassion) > 50 |
           category == "HOP" & as.numeric(hope) > 50 |
           category == "NEU" & as.numeric(arousal) < 50)

h_df = df %>%
  mutate(category = trimws(category)) %>%
  filter(category == "ANG" & as.numeric(anger) > 50 & as.numeric(arousal) > 50 |
           category == "COM" & as.numeric(compassion) > 50 & as.numeric(arousal) > 50 |
           category == "HOP" & as.numeric(hope) > 50 & as.numeric(arousal) > 50|
           category == "NEU" & as.numeric(arousal) < 50)

manipulation_check = conditions %>%
  full_join(ratings, by = c("sid","stid")) %>%
  select(sid, category, part, opt) %>%
  group_by(category)

soft_check = manipulation_check %>%
  filter(sid %in% s_df$sid)

hard_check = manipulation_check %>%
  filter(sid %in% h_df$sid)

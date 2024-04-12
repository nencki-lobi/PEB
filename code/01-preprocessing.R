# Load data

conditions = read.table("./data/conditions.csv", header = T, sep = ",", encoding = "UTF-8")
questionnaires = read.table("./data/questionnaires.csv", header = T, sep = ",", encoding = "UTF-8")
ratings = read.table("./data/story-ratings.csv", header = T, sep = ",", encoding = "UTF-8")
times = read.table("./data/story-times.csv", header = T, sep = ",", encoding = "UTF-8")
intentions = read.table("./data/peb-intentions.csv", header = T, sep = ",", encoding = "UTF-8")
weptings = read.table("./data/peb-weptings.csv", header = T, sep = ",", encoding = "UTF-8")
donations = read.table("./data/peb-donations.csv", header = T, sep = ",", encoding = "UTF-8")
demo = read.table("./data/demographic.csv", header = T, sep = ",", encoding = "UTF-8")

# Generate derivatives

conditions_transposed = conditions %>%
  mutate(category = factor(category, levels = c("NEU", "ANG", "COM", "HOP"))) %>%
  mutate(emo = recode_factor(category,
                             `NEU` = "NEU",
                             `ANG` = "EMO",
                             `COM` = "EMO",
                             `HOP` = "EMO"))

questionnaires_transposed = questionnaires %>%
  mutate(name = factor(name)) %>%
  mutate(name = recode_factor(name,
                       `PCAE-pl` = "PCAE",
                       `PCAE-no` = "PCAE",
                       `PD-pl` = "PD",
                       `PD-no` = "PD",
                       `WTS-pl` = "WTS",
                       `WTS-no` = "WTS")) %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = c("name","ord"), 
              values_from = "opt", 
              names_sep = ".") %>%
  mutate (PCAE_i = `PCAE.0` + `PCAE.1`, 
          PCAE_c = `PCAE.2` + `PCAE.3`, 
          PCAE = PCAE_i + PCAE_c, 
          PD = `PD.0` + `PD.1`, 
          WTS = `WTS.0` + `WTS.1` + `WTS.2` + `WTS.3` + `WTS.4`) %>%
  select(sid, stid, PCAE_i, PCAE_c, PCAE, PD, WTS)

ratings_transposed = ratings %>%
  mutate(scale = factor(part_to_scale[as.character(part)], 
                        levels = labels_scales)) %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = "scale",
              values_from = "opt")

times_transposed = times %>%
  select(sid, stid, reading_time, evaluation_time)
  
intentions_transposed = intentions %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = "ord", 
              values_from = "opt") %>%
  rename(int = "0", aim = "1")

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

pebs = intentions_transposed %>%
  full_join(weptings_transposed, by = c("sid","stid")) %>%
  full_join(donations_transposed, by = c("sid","stid")) %>%
  mutate(cPEB = case_when(stid == 23 ~ (wept/15 + donation/20) /2,
                          stid == 26 ~ (wept/15 + donation/200) /2))

demo_transposed = demo %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = "ord", 
              values_from = "val") %>%
  rename(sex = "0", sexother = "1", birth = "2", res = "3", edu = "4", kid = "6", ses = "7", bcc = "8", ccc = "9") %>%
  select(-c(sexother)) %>% # removing non-numeric column of no interest for convenience
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(age = case_when(stid == 23 ~ 2023 - birth,
                         stid == 26 ~ 2024 - birth),
         gen = factor(case_when(
           between(birth, 1997, 2012) ~ "Z",
           between(birth, 1981, 1996) ~ "Y",
           between(birth, 1965, 1980) ~ "X",
           between(birth, 1946, 1964) ~ "Boomer",
           between(birth, 1928, 1945) ~ "Silent",
           TRUE ~ NA_character_
         ), levels = c("Z","Y","X","Boomer","Silent"))) %>%
  mutate(res = ifelse(res == 4, 3, res)) %>% # pooling data due to small number of observations
  select(sid, stid, sex, age, gen, res, edu, kid, ses, bcc, ccc)


# Joined dataset

dataset = conditions_transposed  %>%
  full_join(questionnaires_transposed, by = c("sid","stid")) %>%
  full_join(ratings_transposed, by = c("sid","stid")) %>%
  full_join(times_transposed, by = c("sid","stid")) %>%
  full_join(pebs, by = c("sid","stid")) %>%
  full_join(demo_transposed, by = c("sid","stid")) %>% 
  na.omit()

## Select country

dataset = dataset %>%
  filter(case_when(country == "PL" ~ stid == 23,
                   country == "NO" ~ stid == 26))

# Define exclusion criteria

subjects = dataset %>%
  mutate(soft_check = ifelse(category == "ANG" & anger > 50 |
                             category == "COM" & compassion > 50 |
                             category == "HOP" & hope > 50 |
                             category == "NEU" & arousal < 50, 1, 0)) %>% # emotional stories must be rated as emotional, neutral stories as not arousing
  mutate(hard_check = ifelse(category == "ANG" & anger > 50 & arousal > 50 |
                             category == "COM" & compassion > 50 & arousal > 50 |
                             category == "HOP" & hope > 50 & arousal > 50 |
                             category == "NEU" & arousal < 50, 1, 0)) %>% # emotional stories must be rated as emotional & arousing, neutral stories as not arousing
  mutate(time_check = ifelse(reading_time > 5000 & # reading time per story must be at least 5 seconds
                             abs(reading_time - mean(reading_time)) <= 2*sd(reading_time), 1, 0)) %>% # reading time per story must be +/- 2SD around the mean
  select(sid, stid, category, soft_check, hard_check, time_check)


# Check how many participants get excluded per category (0 - excluded, 1 - included)

## soft check
subjects %>%
  group_by(soft_check, category) %>%
  summarize(count = n())

## hard check
subjects %>%
  group_by(hard_check, category) %>%
  summarize(count = n())

## soft check & time check
subjects %>%
  mutate(check = soft_check & time_check) %>%
  group_by(check, category) %>%
  summarize(count = n())

## hard check & time check
subjects %>%
  mutate(check = hard_check & time_check) %>%
  group_by(check, category) %>%
  summarize(count = n())


# Visual inspection of the manipulation check

sdir = sdir.create("Figures")

plot.check = function(ptitle, ftitle, selected) {
  df = dataset %>%
    filter(sid %in% selected$sid) %>%
    select(sid, category, valence, arousal, anger, compassion, hope) %>%
    pivot_longer(cols = c(valence, arousal, anger, compassion, hope), 
                 names_to = "scale", 
                 values_to = "rating") %>%
    mutate(scale = factor(scale, levels = c("anger", "compassion", "hope", "valence", "arousal"))) 
  
  p = ggplot(df, aes(x=category, y=rating, fill=category)) +
    geom_boxplot() +
    xlab("Rating scale") + ylab("Mean rating") +
    labs(title = ptitle) +
    facet_wrap(~scale) +
    scale_fill_manual(values = colors, name = "Condition") + 
    beauty
  ggsave(paste0(ftitle, ".png"), p, path = sdir)
}

ptitle = "Manipulation check: all subjects"
ftitle = "C1-all-subjects"
selected = subjects
plot.check(ptitle, ftitle, selected)

ptitle = "Manipulation check: soft filter"
ftitle = "C2-soft-filter"
selected = subset(subjects, soft_check == 1)
plot.check(ptitle, ftitle, selected)

ptitle = "Manipulation check: hard filter"
ftitle = "C3-hard-filter"
selected = subset(subjects, hard_check == 1)
plot.check(ptitle, ftitle, selected)

ptitle = "Manipulation check: soft & time filter"
ftitle = "C4-soft-&-time-filter"
selected = subset(subjects, soft_check == 1 & time_check == 1)
plot.check(ptitle, ftitle, selected)

ptitle = "Manipulation check: hard & time filter"
ftitle = "C5-hard-&-time-filter"
selected = subset(subjects, hard_check == 1 & time_check == 1)
plot.check(ptitle, ftitle, selected)

ptitle = "Manipulation check: inverse soft filter"
ftitle = "C6-inverse-soft-filter"
selected = subset(subjects, soft_check == 0)
plot.check(ptitle, ftitle, selected)

ptitle = "Manipulation check: inverse hard filter"
ftitle = "C7-inverse-hard-filter"
selected = subset(subjects, hard_check == 0)
plot.check(ptitle, ftitle, selected)


# Select data that will be used for subsequent analysis

## To use the whole dataset (as preregistered), use the following:

# selected = subjects # use whole dataset (as preregistered)

## To filter data based on selected exclusion criteria, use one of the following:

selected = subset(subjects, soft_check == 1)
# selected = subset(subjects, hard_check == 1)
# selected = subset(subjects, soft_check == 1 & time_check == 1)
# selected = subset(subjects, hard_check == 1 & time_check == 1)

## To specifically inspect excluded data, use one of the following:

# selected = subset(subjects, soft_check == 0)
# selected = subset(subjects, hard_check == 0)


# Final, cleaned dataset that will be used for subsequent analysis

clean_dataset = dataset %>% filter(sid %in% selected$sid)
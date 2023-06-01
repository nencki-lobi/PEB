library('dplyr')

conditions = read.table("./data/S1/conditions.tsv", header = F, skip = 2, sep = "|", encoding = "UTF-8")
colnames(conditions) = c("qid", "sid", "stid", "category")

demo = read.table("./data/S1/demographic.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(demo) = c("sid","code", "stid", "name", "ord", "val")

questionnaires = read.table("./data/S1/questionnaires.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(questionnaires) = c("sid", "stid", "name", "ord", "opt")

ratings = read.table("./data/S1/story-ratings.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(ratings) = c("sid", "stid", "name", "ord", "part", "opt")

intentions = read.table("./data/S1/peb-intentions.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(intentions) = c("sid", "stid", "name", "ord", "opt")

donations = read.table("./data/S1/peb-donations.tsv", header = F, skip = 2, sep = "|", encoding = "UTF-8")
colnames(donations) = c("sid", "stid", "name", "ord","val")

weptings = read.table("./data/S1/peb-weptings.tsv", header = F, skip = 2, sep = "|", strip.white = T, encoding = "UTF-8")
colnames(weptings) = c("sid", "stid", "ord", "mh", "fh", "name")

weptings_transposed = weptings  %>%
  mutate(wept = mh + fh < 5) %>%
  group_by(sid, stid) %>%
  summarise(wept = n())

pebs = full_join(weptings_transposed, donations, by = "sid") %>%
  full_join(conditions, by = "sid") %>%
  select("sid", "category", "wept", "val") %>%
  rename("donation" = "val") %>%
  mutate(cPEB = (as.integer(wept)/15 + as.integer(donation)/20) /2)
# mutate(cPEB = (as.integer(wept)/15 + as.integer(donation)/100) /2) %>%


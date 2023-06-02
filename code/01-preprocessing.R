library('dplyr')
library('tidyverse')

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

conditions_filtered = conditions %>%
filter(category != " other")

demo_transposed = demo %>%
  pivot_wider(names_from = ord, values_from = val) %>%
  full_join(conditions_filtered, by = "sid") %>%
  rename(sex = "0", age = "2", res = "3", edu = "4", kid = "6", ses = "7", bcc = "8", ccc = "9") %>%
  select(sid, category, sex, age, res, edu, kid, ses, bcc, ccc)

questionnaires_transposed = questionnaires %>%
  group_by(sid) %>%
  mutate(item = paste(sub("(.*?)-.*", "\\1", name), ord, sep = "_"))%>%
  select (sid, stid, opt, item) %>%
  pivot_wider(names_from = item, values_from = opt) %>%
  mutate (PCAE_i = PCAE_0+PCAE_1, PCAE_c = PCAE_2+PCAE_3, PCAE = PCAE_i+PCAE_c, 
          PD = PD_0 + PD_1, WTS = WTS_0 + WTS_1 + WTS_2 + WTS_3 + WTS_4) %>%
  select(sid, stid, PCAE_i, PCAE_c, PCAE, PD, WTS)

ratings_transposed = ratings %>%
  group_by(sid) %>%
  mutate(condition = factor(recode(ord, "0"="ANG", "1"="COM", "2"="HOP", "3"="NEU")),
         scale = factor(recode(part, "0"="val", "1"="aro", "2"="ang", "3"="com", "4"="hop"))) %>%
  select(sid, stid, condition, scale, opt) %>%
  pivot_wider(id_cols = c("sid", "stid"),
              names_from = c("condition", "scale"),
              names_sep = "_",
              names_sort = T,
              values_from = "opt") %>%
  select(sid, stid, ANG_val, ANG_aro, ANG_ang, ANG_com, ANG_hop,
         COM_val, COM_aro, COM_ang, COM_com, COM_hop,
         HOP_val, HOP_aro, HOP_ang, HOP_com, HOP_hop,
         NEU_val, NEU_aro, NEU_ang, NEU_com, NEU_hop)
  
intentions_transposed = intentions %>%
  select(sid, ord, opt) %>%
  pivot_wider(names_from = ord, values_from = opt) %>%
  rename(int = "0", aim = "1") %>%
  relocate(sid, int, aim)

weptings_transposed = weptings  %>%
  mutate(wept = mh + fh < 5) %>%
  group_by(sid, stid) %>%
  summarise(wept = n())

pebs = full_join(weptings_transposed, donations, by = "sid") %>%
  full_join(conditions_filtered, by = "sid") %>%
  full_join(intentions_transposed, by = "sid") %>%
  select("sid", "category", "int", "aim", "wept", "val") %>%
  rename("donation" = "val") %>%
  mutate(cPEB = (as.integer(wept)/15 + as.integer(donation)/20) /2)
# mutate(cPEB = (as.integer(wept)/15 + as.integer(donation)/100) /2) %>%



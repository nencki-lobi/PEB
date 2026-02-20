odir = "./output"
if (!dir.exists(odir)) {dir.create(odir)}

cdir = file.path(odir, params$country)
if (!dir.exists(cdir)) {dir.create(cdir)}

sdir.create = function(name) {
  sdir = file.path(cdir, name)
  if (!dir.exists(sdir)) {dir.create(sdir)}
  sdir}

output = function(content) {
  output = capture.output(content)
  cat(output, sep = "\n")
} 

country = params$country
country_full = if_else(country == "PL", "Poland", "Norway")

parts = 0:4

labels_scales = c("valence", "arousal", "anger", "compassion", "hope")
labels_conditions = c("ANG", "COM", "HOP", "NEU")

colors = c('gray', '#CD1212', '#0EA25A', '#3499CB')
colors_simple = c('gray','#E38421')

part_to_scale = labels_scales
names(part_to_scale) = parts

options(scipen = 999)

beauty = theme_linedraw() + theme(panel.grid = element_blank(),
                                  strip.background = element_rect(fill = "white", color = "white"),
                                  strip.text = element_text(colour = "black", size = 12),
                                  aspect.ratio = 1)

contrast_d_from_model = function(contr_sum, fit) {
  # contr_sum = summary(contr, infer=c(TRUE, TRUE))
  sig = sigma(fit)
  tcrit = qt(0.975, df = df.residual(fit))
  
  contr_sum %>%
    as.data.frame() %>%
    mutate(
      d = estimate / sig,
      d_low = (estimate - tcrit * SE) / sig,
      d_high = (estimate + tcrit * SE) / sig,
      sigma = sig,
      df_resid = df.residual(fit)
    )
}
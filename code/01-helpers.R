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

parts = 0:4

labels_scales = c("valence", "arousal", "anger", "compassion", "hope")
labels_conditions = c("ANG", "COM", "HOP", "NEU")

colors = c('gray', '#CD1212', '#0EA25A', '#3499CB')
colors_simple = c('gray','#E38421')

part_to_scale = labels_scales
names(part_to_scale) = parts


beauty = theme_linedraw() + theme(panel.grid = element_blank(),
                                  strip.background = element_rect(fill = "white", color = "white"),
                                  strip.text = element_text(colour = "black", size = 12),
                                  aspect.ratio = 1)

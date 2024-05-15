library(car)
library(ggplot2)
library(gtsummary)
library(lavaan)
library(psych)
library(report)
library(Rmisc)
library(tidyverse)

odir = "./output"
if (!dir.exists(odir)) {dir.create(odir)}

for (i in c("PL", "NO")) {
  
  cdir = file.path(odir, i)
  if (!dir.exists(cdir)) {dir.create(cdir)}
  
  rmarkdown::render("PEB.Rmd", 
                    params = list(country = i),
                    output_file=paste0("./output/", i, "/descriptives.html"))}

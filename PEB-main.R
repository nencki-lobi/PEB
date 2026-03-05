library(car)
library(ggplot2)
library(gtsummary)
library(lavaan)
library(psych)
library(report)
library(Rmisc)
library(tidyverse)
library(broom)
library(patchwork)
library(effectsize)
library(emmeans)

odir = "./output"
if (!dir.exists(odir)) {dir.create(odir)}

for (i in c("PL", "NO")) {
  
  cdir = file.path(odir, i)
  if (!dir.exists(cdir)) {dir.create(cdir)}
  
  for (j in c("registered", "non-registered")) {
  
    tdir = file.path(cdir, j)
    if (!dir.exists(tdir)) {dir.create(tdir)}
    
    rmarkdown::render("PEB.Rmd", 
                      params = list(country = i, type = j),
                      output_file=paste0(tdir, "/descriptives.html"))}}

##Folder Generator##
library(tidyverse)

d <- read.csv(file.choose())

sapply(d$CameraTrapFiles, dir.create, recursive = TRUE, showWarnings = FALSE)

sapply(d$AudioMothFiles, dir.create, recursive = TRUE, showWarnings = FALSE)

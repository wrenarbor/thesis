speaker_1 <- read.csv("../speaker 1.tsv", sep = "\t")
speaker_a <- read.csv("../speaker_a_rec_1.tsv", sep = "\t")
speaker_b <- read.csv("../speaker_b_rec_1.tsv", sep = "\t")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("png")
install.packages("tikzDevice")
library(ggplot2)
library(dplyr)

library(tikzDevice)
options(tikzDefaultEngine = 'pdftex')

tikz(file = "./Rplots.tex", width=3, onefile = TRUE)

tokens <- bind_rows(list(speaker_1 = speaker_1, speaker_a = speaker_a, speaker_b = speaker_b), .id = "speaker")

ggplot(tokens, aes(x = paste(label, speaker), y = cog)) + geom_boxplot()
t.test(cog ~ label, speaker_1)



dev.off()

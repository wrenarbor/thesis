speaker_1 <- read.csv("../[s] Measurements/speaker_1_s_measurements.tsv", sep = "\t")
speaker_2 <- read.csv("../[s] Measurements/speaker_2_s_measurements.tsv", sep = "\t")
speaker_3 <- read.csv("../[s] Measurements/speaker_3_s_measurements.tsv", sep = "\t")
speaker_4 <- read.csv("../[s] Measurements/speaker_4_s_measurements.tsv", sep = "\t")
int_a_with_spkr1 <- read.csv("../[s] Measurements/interlocutor_a_with_spkr1_s_measurements.tsv", sep = "\t")
int_b_with_spkr1 <- read.csv("../[s] Measurements/interlocutor_a_with_spkr1_s_measurements.tsv", sep = "\t")
ints_with_spkr2 <- read.csv("../[s] Measurements/interlocutors_with_spkr2_s_measurements.tsv", sep = "\t")
ints_with_spkr3 <- read.csv("../[s] Measurements/interlocutors_with_spkr3_s_measurements.tsv", sep = "\t")
ints_with_spkr4 <- read.csv("../[s] Measurements/interlocutors_with_spkr4_s_measurements.tsv", sep = "\t")

interlocutor_s_measurements <- bind_rows(int_a_with_spkr1, 
                                         int_b_with_spkr1, 
                                         ints_with_spkr2, 
                                         ints_with_spkr3, 
                                         ints_with_spkr4)

speaker_s_measurements <- bind_rows(
  speaker_1,
  speaker_2,
  speaker_3,
  speaker_4,
)

all_s_measurements <- bind_rows(
  list(speaker = speaker_s_measurements, interlocutor = interlocutor_s_measurements),
  .id = "participant_type"
)

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

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("png")
#install.packages("tikzDevice")
#install.packages("fillpattern")
#install.packages("Cairo")
library(ggplot2)
library(dplyr)
library(fillpattern)

assemble_measurements <- function() {
speaker_1 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/speaker_1_s_measurements.tsv", sep = "\t")
speaker_2 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/speaker_2_s_measurements.tsv", sep = "\t")
speaker_3 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/speaker_3_s_measurements.tsv", sep = "\t")
speaker_4 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/speaker_4_s_measurements.tsv", sep = "\t")
int_a_with_spkr1 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/interlocutor_a_with_spkr1_s_measurements.tsv", sep = "\t")
int_b_with_spkr1 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/interlocutor_b_with_spkr1_s_measurements.tsv", sep = "\t")
ints_with_spkr2 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/interlocutors_with_spkr2_s_measurements.tsv", sep = "\t")
ints_with_spkr3 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/interlocutors_with_spkr3_s_measurements.tsv", sep = "\t")
ints_with_spkr4 <- read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/interlocutors_with_spkr4_s_measurements.tsv", sep = "\t")

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
  .id = "role_of_participant_measured"
)

all_s_measurements |> filter(cog >= 2500) -> sensible_s_measurements

write.csv(sensible_s_measurements, "~/Documents/Reed/Thesis/Data/[s] Measurements/sensible_s_measurements.csv")

#sensible_s_measurements |> mutate(
#  speaker_in_recording = factor(strtoi(substr(label, 1, 1)), 
#                                levels = 1:4, 
#                                labels = c(
#                                  "Mantis", "Alpaca", "Squirrel", "Jellyfish")),
#  interlocutor_in_recording = factor(substr(label, 2, 2),
#                                            levels = c("a", "b"),
#                                     labels = c("Trans Conv. Partner", 
#                                                "Cis Conv.  Partner")),
#  participant = if_else(role_of_participant_measured == "speaker", 
#                        speaker_in_recording,
#                       interlocutor_in_recording),
#  ) -> recoded_s_measurements
#  write.csv(recoded_s_measurements, "~/Documents/[s] Measurements/recoded_s_measurements.csv")
}

load_s_measurements <- function() {
  read.csv("~/Documents/Reed/Thesis/Data/[s] Measurements/sensible_s_measurements.csv") |> mutate(
    speaker_in_recording = factor(strtoi(substr(label, 1, 1)), 
                                  levels = 1:4, 
                                  labels = c(
                                    "Mantis", "Alpaca", "Squirrel", "Jellyfish")),
    interlocutor_in_recording = factor(substr(label, 2, 2),
                                       levels = c("a", "b"),
                                       labels = c("Trans CP", 
                                                  "Cis CP")),
    participant = if_else(role_of_participant_measured == "speaker", 
                          speaker_in_recording,
                          interlocutor_in_recording),
  )
}

s_measurements <- load_s_measurements()

overall_plot <- ggplot(s_measurements, 
       aes(x = interlocutor_in_recording, 
           y = cog, color = participant, fill = participant)
       ) + geom_boxplot() + 
        facet_wrap(
          vars(speaker_in_recording)) +
        scale_fill_pattern() + 
        theme_minimal() +
        theme(legend.key.size = unit(2,"line"), text=element_text(size=16,  family="Charis")) +
        labs(title = "Spectral Center of Gravity (CoG) of /s/ Across Recordings and Participants",
             y = "/s/ CoG (Hz)", 
             x = "Conversation Partner in Recording",
             fill = "Participant", color = "Participant")

ggsave(file="~/Documents/Reed/Thesis/Document/thesistemplate_2020-04-24/images/overall_plot.png", plot=overall_plot, width=10, height=8, )

speaker_specific_boxplot <- function(speaker) {
  speaker_s_measurements <- 
    s_measurements |> filter(speaker_in_recording == speaker)
  ggplot(
    speaker_s_measurements, 
    aes(
      x = interlocutor_in_recording, 
      y = cog, color = participant, fill = participant
    )
  ) + geom_boxplot() +
    scale_fill_pattern(patterns = c(1, 5, 6)) +
    scale_color_discrete(
      limits=levels(s_measurements$participant),
      breaks=unique(speaker_s_measurements$participant)) +
    theme_minimal() +
    theme(legend.key.size = unit(2,"line"), text=element_text(size=16,  family="Charis")) +
    labs(title = paste("Spectral Center of Gravity (CoG) of /s/ by", speaker, "and CPs"),
         y = "/s/ CoG (Hz)", 
         x = "Conversation Partner in Recording",
         fill = "Participant", color = "Participant")
  
}

for (speaker in levels(s_measurements$speaker_in_recording)) {
  ggsave(file=paste("~/Documents/Reed/Thesis/Document/thesistemplate_2020-04-24/images/", speaker,"_plot.png", sep=""), plot=speaker_specific_boxplot(speaker), width=10, height=8)
}

speaker_specific_boxplot("Mantis")
print(mantis_plot)
alpaca_plot <- speaker_specific_boxplot("Alpaca")
print(alpaca_plot)

ggsave(file="overall_plot.svg", plot=overall_plot, width=5, height=4)



dev.off()

ggplot(s_measurements |> filter(role_of_participant_measured == "interlocutor"), 
       aes(x = speaker_in_recording, 
           y = cog, color = participant, fill = participant)
) + geom_boxplot() + 
  scale_fill_pattern() + 
  theme_minimal()

ggplot(s_measurements, aes(x = start, y = cog, color = participant)) + 
  facet_wrap(vars(label)) +
  # geom_point() + 
  geom_smooth()

ggplot(s_measurements, aes(x = duration, y = cog, color = participant)) + 
  facet_wrap(vars(label)) +
  geom_point() + 
  geom_smooth()


t.test(cog ~ interlocutor_in_recording, filter(s_measurements, participant == "Mantis"))
t.test(cog ~ interlocutor_in_recording, filter(s_measurements, participant == "Alpaca"))
t.test(cog ~ interlocutor_in_recording, filter(s_measurements, participant == "Squirrel"))
t.test(cog ~ interlocutor_in_recording, filter(s_measurements, participant == "Jellyfish"))


s_measurements |> group_by(label, participant) |> summarise(cog = mean(cog))
#s_measurements |> 
#  filter(role_of_participant_measured == "interlocutor") |> 
#  group_by(participant, speaker_in_recording) |>
#  summarise(interlocutor_cog = mean(cog))

#s_measurements |>
#  filter(role_of_participant_measured == "interlocutor") |>
#  group_by(participant, speaker_in_recording) %||% 
  
#aov(cog ~ speaker_in_recording)
  



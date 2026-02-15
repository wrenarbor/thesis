test_recording <- read.csv("../Audiologfile.csv", sep = "\t")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("png")
install.packages("tikzDevice")
library(ggplot2)

library(tikzDevice)
options(tikzDefaultEngine = 'pdftex')

tikz(file = "./Rplots.tex", width=3, onefile = TRUE)

ggplot(test_recording, aes(x = intensity, y = cog)) + geom_point()
ggplot(test_recording, aes(y = cog)) + geom_boxplot()

dev.off()

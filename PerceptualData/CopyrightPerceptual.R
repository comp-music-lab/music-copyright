setwd("/Users/yyc/PerceptualData")

install.packages(c("tidyr", "ggplot2"))
library(tidyr)
library(ggplot2)

accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
t.test(accuracy_byParti$full, accuracy_byParti$melody, paired = TRUE)
t.test(accuracy_byParti$melody, accuracy_byParti$lyrics, paired = TRUE)
t.test(accuracy_byParti$full, accuracy_byParti$lyrics, paired = TRUE)



# Violin plot for accuracy by participant
# Basic violin plot
accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
data_long <- gather(accuracy_byParti, condition, accuracy, full:lyrics, factor_key = TRUE)
data_long_random <- gather(accuracy_byParti, condition, accuracy, full_random:lyrics_random, factor_key = TRUE)

p <- ggplot(data_long, aes(x = condition, y = accuracy, color = condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  geom_violin() + aes(ymin = 0) + aes(ymax = 100) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1)

# random control
g <- ggplot(data_long_random, aes(x = condition, y = accuracy, fill = condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1) + 
  scale_fill_manual(values = c("gray", "gray", "gray"))



# Violin plot for accuracy by case
# Basic violin plot
accuracy_byCase <- read.csv("accuracy_by_case.csv", header = TRUE)
data_long <- gather(accuracy_byCase, condition, accuracy, full:lyrics, factor_key = TRUE)

p_byCase <- ggplot(data_long, aes(x = condition, y = accuracy, color = condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  geom_violin() + aes(ymin = 0) + aes(ymax = 100) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1)



# Perceptual similarity vs PMI
similarity <- read.csv("perceptual_simi_vs_pmi_long.csv", header = TRUE)
pmi_p <- ggplot(similarity, aes(x = SIMILARITY, y = PMI)) + 
  scale_x_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  geom_point(aes(colour = GROUP)) + 
  geom_smooth(method = "lm") + facet_grid(GROUP~.)

# random control
pmi_g <- ggplot(similarity, aes(x = SIMI_RANDOM, y = PMI)) + 
  scale_x_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  geom_point(aes(colour = GROUP_RANDOM)) + 
  geom_smooth(method = "lm") + facet_grid(GROUP_RANDOM~.)



# Perceptual similarity vs Musly similarity
perceptual_musly <- read.csv("perceptual_simi_vs_musly_simi_long.csv", header = TRUE)
musly_p <- ggplot(perceptual_musly, aes(x = SIMILARITY, y = Musly_simi)) + 
  scale_x_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  geom_point(aes(colour = GROUP)) + 
  geom_smooth(method = "lm") + facet_grid(GROUP~.)


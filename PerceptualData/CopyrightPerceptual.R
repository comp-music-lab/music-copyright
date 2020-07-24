setwd("/Users/username/PerceptualData")

install.packages(c("tidyr", "ggplot2"))
install.packages("ggpubr")
library(tidyr)
library(ggplot2)
library(ggpubr)

accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
t.test(accuracy_byParti$full, accuracy_byParti$melody, paired = TRUE, alternative="less") #updated to make one-tailed in accordance with our a priori predictions
t.test(accuracy_byParti$melody, accuracy_byParti$lyrics, paired = TRUE)
t.test(accuracy_byParti$full, accuracy_byParti$lyrics, paired = TRUE)

accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
subset_musician <- subset(accuracy_byParti, MusicExperience==1)
subset_nonmusician <- subset(accuracy_byParti, MusicExperience==0)
t.test(subset_musician$full, subset_nonmusician$full, paired = FALSE, alternative="greater")
t.test(subset_musician$melody, subset_nonmusician$melody, paired = FALSE, alternative="greater")
t.test(subset_musician$lyrics, subset_nonmusician$lyrics, paired = FALSE, alternative="greater")



## The following code of function summarySE() is extracted from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



# Violin plot for accuracy by participant
# Basic violin plot
accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
data_long <- gather(accuracy_byParti, key = "Condition", value = "Accuracy", full:lyrics, factor_key = TRUE)
data_long_random <- gather(accuracy_byParti, key = "Condition", value = "Accuracy", full_random:lyrics_random, factor_key = TRUE)

# combine
data_mix <- gather(accuracy_byParti, key = "Condition", value = "Accuracy", full:lyrics_random, factor_key = TRUE)
data_mix$Condition <- relevel(relevel(relevel(relevel(relevel(relevel(data_mix$Condition,"lyrics_random"),"lyrics"),"melody_random"),"melody"),"full_random"),"full")
subset_nonrandom <- subset(data_mix, Condition=="full" | Condition=="melody" | Condition=="lyrics")
subset_random <- subset(data_mix, Condition=="full_random" | Condition=="melody_random" | Condition=="lyrics_random")
summarySE_nonrandom <- summarySE(subset_nonrandom, measurevar = "Accuracy", groupvars = "Condition")
summarySE_random <- summarySE(subset_random, measurevar = "Accuracy", groupvars = "Condition")

mix <- ggplot(data = data_mix, mapping = aes(x = Condition, y = Accuracy)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  aes(ymin = 0) + aes(ymax = 100) + 
  geom_point(color = "white", size = 0.1) + 
  geom_violin(data = subset_random, 
              mapping = aes(x = Condition, y = Accuracy, color = subset_nonrandom$Condition, fill = "random")) + 
  geom_violin(data = subset_nonrandom, 
              mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = "nonrandom")) + 
  geom_dotplot(data = subset_random, 
               mapping = aes(x = Condition, y = Accuracy, color = subset_nonrandom$Condition, fill = factor(MusicExperience)), 
               position = position_dodge(width=1.3), binwidth = 2, binaxis = "y", stackdir = "center", dotsize = 0.65) + 
  geom_dotplot(data = subset_nonrandom, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = factor(MusicExperience)), 
               position = position_dodge(width=1.1), binwidth = 3, binaxis = "y", stackdir = "center", dotsize = 0.9) + 
  geom_errorbar(data = summarySE_random, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  geom_errorbar(data = summarySE_nonrandom, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  stat_summary(fun = "mean", mapping = aes(shape = "mean"), 
               geom = "point", color = "yellow", fill = "purple", alpha = 0.8, size = 4) + 
  scale_x_discrete(labels = c("full" = "Full-audio", 
                              "full_random" = "Full-audio (randomized)", 
                              "melody" = "Melody-only", 
                              "melody_random" = "Melody-only (randomized)", 
                              "lyrics" = "Lyrics-only", 
                              "lyrics_random" = "Lyrics-only (randomized)")) + 
  scale_color_discrete(name = "Condition", labels = c("full" = "Full-audio", 
                                                      "melody" = "Melody-only", 
                                                      "lyrics" = "Lyrics-only")) + 
  scale_fill_manual(name = "Randomization \n& Music Experience", 
                    values = c("random" = "lightgray", "nonrandom" = "white", 
                               "0" = "green", "1" = "blue"), 
                    labels = c("random" = "Randomized", "nonrandom" = "Non-randomized", 
                               "0" = "Non-experienced", "1" = "Experienced")) + 
  scale_shape_manual(name = "", values = c("mean" = 23), 
                     labels = c("mean" = "Mean value with \n95% confidence \ninterval")) + 
  theme(axis.title = element_text(size = 15), 
        axis.text.x = element_text(angle = 35, size = 15, hjust = 1), 
        axis.text.y = element_text(size = 15), 
        legend.title = element_text(size = 13), 
        legend.text = element_text(size = 13)) + 
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2, reverse = TRUE), 
         shape = guide_legend(order = 3)) + 
  labs(x = "Condition", y = "Accuracy by Participant")



# Violin plot for accuracy by case
# Basic violin plot
accuracy_byCase <- read.csv("accuracy_by_case.csv", header = TRUE)
data_long <- gather(accuracy_byCase, key = "Condition", value = "Accuracy", full:lyrics, factor_key = TRUE)
subset_removeInstrumentalForLyrics <- subset(data_long, !((Condition=="lyrics" & Case==4) | (Condition=="lyrics" & Case==5) | (Condition=="lyrics" & Case==9)))
subset_normal <- subset(data_long, Case!=14 & Case!=15 & Case!=16 & Case!=4 & Case!=5 & Case!=9)
subset_JP <- subset(data_long, Case>=14)
subset_instrumental <- subset(data_long, Case==4 | Case==5 | Case==9)
summarySE_byCase <- summarySE(subset_removeInstrumentalForLyrics, measurevar = "Accuracy", groupvars = "Condition")

p_byCase <- ggplot(data = subset_removeInstrumentalForLyrics, mapping = aes(x = Condition, y = Accuracy, color = Condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  aes(ymin = 0) + aes(ymax = 100) + 
  geom_violin(fill = "white", bw = 12) + 
  geom_dotplot(data = subset_normal, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = factor(CourtDecision)), 
               position = position_dodge(width=0.3), binwidth = 3, binaxis = "y", stackdir = "center", dotsize = 1) + 
  #  geom_dotplot(binwidth = 3, binaxis = "y", binpositions = "all", stackdir = "center", dotsize = 1, stackgroups = TRUE)
  geom_point(data = subset_JP, 
             mapping = aes(x = Condition, y = Accuracy, 
                           fill = factor(CourtDecision), group = factor(CourtDecision), 
                           shape = "rectangle"), #JP cases shown by rectangles
             position = position_jitterdodge(jitter.width = 0.55, dodge.width = 0.6, seed = 2), 
             #position = position_dodge(width=1), 
             color = "black", alpha = 0.5, size = 4, stroke = 1.5) + 
  geom_point(data = subset_instrumental, 
             mapping = aes(x = Condition, y = Accuracy, 
                           fill = factor(CourtDecision), group = factor(CourtDecision), 
                           shape = "triangle"), #Instrumental cases shown by triangles
             #position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.3, seed = 2), 
             position = position_dodge(width=0.8), 
             color = "black", alpha = 0.5, size = 4, stroke = 1.5) + 
  geom_errorbar(data = summarySE_byCase, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  stat_summary(fun = "mean", mapping = aes(size = "mean"), 
               geom = "point", color = "yellow", fill = "purple", 
               shape = 23, alpha = 0.8) + #Mean values shown by diamond dots
  scale_x_discrete(labels = c("full" = "Full-audio", 
                              "melody" = "Melody-only", 
                              "lyrics" = "Lyrics-only")) + 
  scale_color_discrete(name = "Condition", labels = c("full" = "Full-audio", 
                                                      "melody" = "Melody-only", 
                                                      "lyrics" = "Lyrics-only")) + 
  scale_fill_manual(name = "Court Decision", 
                    values = c("0" = "green", "1" = "red"), 
                    labels = c("0" = "No infringement", "1" = "Infringement")) + 
  scale_shape_manual(name = "Special Case", values = c("rectangle" = 22, "triangle" = 24), 
                     labels = c("rectangle" = "Case in Japan", 
                                "triangle" = "Case including \ninstrumental work(s)")) + 
  scale_size_manual(name = "", values = c("mean" = 5), 
                    labels = c("mean" = "Mean value with \n95% confidence \ninterval")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 13)) + 
  guides(color = guide_legend(order = 1), 
         fill = guide_legend(order = 2, override.aes = list(shape = NA)), 
         shape = guide_legend(order = 3), 
         size = guide_legend(order = 4)) + 
  labs(x = "Condition", y = "Accuracy by Case")



# Perceptual similarity vs PMI
similarity <- read.csv("perceptual_simi_vs_pmi_long.csv", header = TRUE)
similarity$GROUP <- relevel(relevel(relevel(similarity$GROUP,"LYRICS_SIMI"),"MELODY_SIMI"),"FULL_SIMI")
GROUP.labs <- c("Full-audio", "Melody-only", "Lyrics-only")
names(GROUP.labs) <- c("FULL_SIMI", "MELODY_SIMI", "LYRICS_SIMI")

pmi_p <- ggplot(data = similarity, mapping = aes(x = SIMILARITY, y = PMI)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5), limits = c(1,5)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  geom_point(mapping = aes(color = GROUP)) + 
  geom_smooth(method = "lm") + 
  facet_grid(GROUP~., labeller = labeller(GROUP = GROUP.labs)) + 
  scale_color_discrete(name = "Condition", labels = c("FULL_SIMI" = "Full-audio", 
                                                      "MELODY_SIMI" = "Melody-only", 
                                                      "LYRICS_SIMI" = "Lyrics-only")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 13), 
        strip.text.y = element_text(size = 13)) + 
  labs(x = "Mean Perceptual Similarity", y = "Automatically Calculated Melodic Similarity (PMI)") + 
  #  stat_regline_equation(label.x = 4, label.y = 15) + 
  stat_cor(label.x = 3.5, label.y = 3, size = 5)



# Perceptual similarity vs Musly similarity
perceptual_musly <- read.csv("perceptual_simi_vs_musly_simi_long.csv", header = TRUE)
perceptual_musly$GROUP <- relevel(relevel(relevel(perceptual_musly$GROUP,"LYRICS_SIMI"),"MELODY_SIMI"),"FULL_SIMI")
GROUP.labs <- c("Full-audio", "Melody-only", "Lyrics-only")
names(GROUP.labs) <- c("FULL_SIMI", "MELODY_SIMI", "LYRICS_SIMI")

musly_p <- ggplot(data = perceptual_musly, mapping = aes(x = SIMILARITY, y = Musly_simi)) + 
  scale_x_continuous(breaks = c(1,2,3,4,5), limits = c(1,5)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  geom_point(mapping = aes(color = GROUP)) + 
  geom_smooth(method = "lm") + 
  facet_grid(GROUP~., labeller = labeller(GROUP = GROUP.labs)) + 
  scale_color_discrete(name = "Condition", labels = c("FULL_SIMI" = "Full-audio", 
                                                      "MELODY_SIMI" = "Melody-only", 
                                                      "LYRICS_SIMI" = "Lyrics-only")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 13), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 13), 
        strip.text.y = element_text(size = 13)) + 
  labs(x = "Mean Perceptual Similarity", y = "Automatically Calculated Audio Similarity (Musly)") + 
  #  stat_regline_equation(label.x = 0, label.y = 98) + 
  stat_cor(label.x = 1, label.y = 90, size = 5)

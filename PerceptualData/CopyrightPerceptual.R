setwd("/Users/yyc/PerceptualData")

install.packages(c("tidyr", "ggplot2"))
install.packages("ggpubr")
library(tidyr)
library(ggplot2)
library(ggpubr)

accuracy_byParti <- read.csv("accuracy_by_participant.csv", header = TRUE)
t.test(accuracy_byParti$full, accuracy_byParti$melody, paired = TRUE, alternative="less") #updated to make one-tailed in accordance with our a priori predictions
t.test(accuracy_byParti$melody, accuracy_byParti$lyrics, paired = TRUE)
t.test(accuracy_byParti$full, accuracy_byParti$lyrics, paired = TRUE)



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

p <- ggplot(data_long, aes(x = Condition, y = Accuracy, color = Condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  geom_violin() + aes(ymin = 0) + aes(ymax = 100) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1)

# random control
g <- ggplot(data_long_random, aes(x = Condition, y = Accuracy, fill = Condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.3) + 
  scale_fill_manual(values = c("gray", "gray", "gray"))


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
              mapping = aes(x = Condition, y = Accuracy, color = subset_nonrandom$Condition), 
              fill = "lightgray") + 
  geom_violin(data = subset_nonrandom, 
              mapping = aes(x = Condition, y = Accuracy, color = Condition), 
              fill = "white") + 
  geom_dotplot(data = subset_random, 
               mapping = aes(x = Condition, y = Accuracy, color = subset_nonrandom$Condition), 
               binwidth = 2, binaxis = "y", stackdir = "center", fill = "gray", dotsize = 0.7) + 
  geom_dotplot(data = subset_nonrandom, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition), 
               binwidth = 3, binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.8) + 
  geom_errorbar(data = summarySE_random, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  geom_errorbar(data = summarySE_nonrandom, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  stat_summary(fun = "mean", geom = "point", color = "yellow", fill = "purple", shape = 23, alpha = 0.8, size = 4) + 
  scale_x_discrete(labels = c("full" = "Full-audio", 
                              "full_random" = "Full-audio (randomized)", 
                              "melody" = "Melody-only", 
                              "melody_random" = "Melody-only (randomized)", 
                              "lyrics" = "Lyrics-only", 
                              "lyrics_random" = "Lyrics-only (randomized)")) + 
  scale_color_discrete(name = "Condition", labels = c("full" = "Full-audio", 
                                                      "melody" = "Melody-only", 
                                                      "lyrics" = "Lyrics-only")) + 
  theme(axis.title = element_text(size = 15), 
        axis.text.x = element_text(angle = 45, size = 15, hjust = 1), 
        axis.text.y = element_text(size = 15), 
        legend.title = element_text(size = 13), 
        legend.text = element_text(size = 13)) + 
  labs(x = "Condition", y = "Accuracy by Participant")


# # non-randomized data points overlap randomized data points (discarded)
# temp <- read.csv("accuracy_by_participant_long.csv", header = TRUE)
# temp$Condition <- relevel(relevel(relevel(temp$Condition,"Lyrics only"),"Melody only"),"Full audio")
# mix <- ggplot(data = temp, mapping = aes(x = Condition, y = Accuracy, color = Condition)) + 
#   scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
#   aes(ymin = 0) + aes(ymax = 100) + 
#   geom_violin(data = subset(temp, Random=="Non-randomized"), 
#               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = Random), 
#               fill = "white") + 
#   geom_violin(data = subset(temp, Random=="Randomized"), 
#               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = Random), 
#               fill = "lightgray") + 
#   geom_dotplot(data = subset(temp, Random=="Non-randomized"), 
#                mapping = aes(x = Condition, y = Accuracy, color = Condition), 
#                binwidth = 3, binaxis = "y", stackdir = "center", fill = "black", dotsize = 0.7) + 
#   geom_dotplot(data = subset(temp, Random=="Randomized"), 
#                mapping = aes(x = Condition, y = Accuracy), color = "yellow", 
#                binwidth = 2, binaxis = "y", stackdir = "center", fill = "gray", dotsize = 0.6) + 
#   labs(x = "Condition", y = "Accuracy by Participant")



# Violin plot for accuracy by case
# Basic violin plot
accuracy_byCase <- read.csv("accuracy_by_case.csv", header = TRUE)
data_long <- gather(accuracy_byCase, key = "Condition", value = "Accuracy", full:lyrics, factor_key = TRUE)
subset_normal <- subset(data_long, Case!=14 & Case!=15 & Case!=16 & Case!=4 & Case!=5 & Case!=9)
subset_JP <- subset(data_long, Case>=14)
subset_instrumental <- subset(data_long, Case==4 | Case==5 | Case==9)
summarySE <- summarySE(data_long, measurevar = "Accuracy", groupvars = "Condition")

p_byCase <- ggplot(data = data_long, mapping = aes(x = Condition, y = Accuracy, color = Condition)) + 
  scale_y_continuous(breaks = seq(0,100,by=10), limits = c(0,100)) + 
  aes(ymin = 0) + aes(ymax = 100) + 
  geom_violin(fill = "white", bw = 12) + 
  geom_dotplot(data = subset_normal, 
               mapping = aes(x = Condition, y = Accuracy, color = Condition, fill = factor(CourtDecision)), 
               position = position_dodge(width=0.3), binwidth = 3, binaxis = "y", stackdir = "center", dotsize = 1) + 
  #  geom_dotplot(binwidth = 3, binaxis = "y", binpositions = "all", stackdir = "center", dotsize = 1, stackgroups = TRUE)
  geom_point(data = subset_JP, 
             mapping = aes(x = Condition, y = Accuracy, fill = factor(CourtDecision)), 
             position = position_jitterdodge(jitter.width = 0.55, dodge.width = 0.55, seed = 2), 
             color = "black", shape = 22, alpha = 0.5, size = 4, stroke = 1.5) + #JP cases shown by rectangles
  geom_point(data = subset_instrumental, 
             mapping = aes(x = Condition, y = Accuracy, fill = factor(CourtDecision)), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.3, seed = 2), 
             color = "black", shape = 24, alpha = 0.5, size = 4, stroke = 1.5) + #Instrumental cases shown by triangles
  geom_errorbar(data = summarySE, 
                mapping = aes(x = Condition, ymin = Accuracy-ci, ymax = Accuracy+ci), 
                color = "purple", width = 0.5, size = 1) + 
  stat_summary(fun = "mean", geom = "point", color = "yellow", fill = "purple", shape = 23, alpha = 0.8, size = 4) + 
  scale_x_discrete(labels = c("full" = "Full-audio", 
                              "melody" = "Melody-only", 
                              "lyrics" = "Lyrics-only")) + 
  scale_color_discrete(name = "Condition", labels = c("full" = "Full-audio", 
                                                      "melody" = "Melody-only", 
                                                      "lyrics" = "Lyrics-only")) + 
  scale_fill_manual(name = "Court Decision", 
                    values = c("0" = "green", "1" = "red"), 
                    labels = c("0" = "No infringement", "1" = "Infringement")) + 
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15), 
        legend.title = element_text(size = 13), legend.text = element_text(size = 13)) + 
  guides(color = guide_legend(order = 1), fill = guide_legend(order = 2)) + 
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
  labs(x = "Perceptual Similarity", y = "Automatically Calculated Melodic Similarity (PMI)") + 
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
  labs(x = "Perceptual Similarity", y = "Automatically Calculated Audio Similarity (Musly)") + 
  #  stat_regline_equation(label.x = 0, label.y = 98) + 
  stat_cor(label.x = 1, label.y = 90, size = 5)

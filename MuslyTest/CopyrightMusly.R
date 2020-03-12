setwd("/Users/yyc/MuslyTest")

# Install and load packages
install.packages("ROCR")
install.packages("ggplot2")
library(ROCR)
library(ggplot2)



# ROC analysis on Musly
# Adapted from https://www.r-bloggers.com/a-small-introduction-to-the-rocr-package/

roc <- read.csv("ROC_Musly.csv", header = F)
simi <- read.csv("Musly_similarity.csv", header = T)
pred <- prediction(simi[,2], roc[,2])

roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a = 0, b = 1)

opt.cut = function(perf, pred) {
	cut.ind = mapply(FUN = function(x, y, p) {
	  d = (x-0)^2 + (y-1)^2
	  ind = which(d == min(d))
	  c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
	}, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

auc.perf = performance(pred, measure = "auc")
auc.perf@y.values



# PMI vs Musly similarity
pmi_musly <- read.csv("pmi_vs_musly_simi.csv", header = TRUE)
p <- ggplot(pmi_musly, aes(x = PMI, y = Musly_simi)) + 
  scale_x_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) + 
  geom_point() + geom_smooth(method = "lm")

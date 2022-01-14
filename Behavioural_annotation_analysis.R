###########
#
#check the analysis route as outlined in https://docs.google.com/document/d/1jxa8D5hHv12OnhDZBANQGnI4pwNfRtQt/edit#heading=h.s0n4rczfptco
#
##########



setwd("/Users/cf19810/Google Drive/Vasudha_Summer_Project/analysis")
getwd()

library("ggplot2")
library("RColorBrewer")
library("gridExtra")

basedata <-read.csv("Vasudha_annotation_fast-calcs_graphs.csv",sep = ",")
summary(basedata)
head(basedata)
basedata$Actor <- as.factor(basedata$Actor)
basedata$Behaviour <- basedata$ï..Behaviour
basedata$frequency <- basedata$occurrence

data<-basedata[!(basedata$Behaviour=="CB" | basedata$Behaviour=="TQ" | basedata$Behaviour=="GQ" | basedata$Behaviour=="FR" | basedata$Behaviour=="CR"),]

#install.packages("forcats")
library("forcats")

############################################
#######DATA PREPARATION#####################
############################################

#we should prepare the data:
#1. Remove double observations: grooming is both made and received, our data should include only one of the two occurrences (i,e. grooming received). Same for trophallaxis
#3. to ease calculations, times  should be normalised as starting from the observation start time (may be an extra column of the dataset) 
#   to help calcluations if making timed observation blocks (see below)
#2. possibly summarise to make ready for analyisis as needed (read below)


#important to keep in mind when deciding/ performing the tests:
#1. each subject (ant) may have repeated measures if performs the behaviour more than once
#2. the N of measures (both total that by individual) will certainly not match pre and post treatment as the N of individuals performing behaviours is going to be different.
#3. will this interfere with tests? Our factor of interest is more properly the nest, not the indiviual ants. 
#   If the observations are summarised by calculating the mean (total per nest-treatment or by time block per nest-treatment, as 5 mins block, 6 blocks per nest-treatment),
#   of time in seconds there would be perfectly paired repeated measures!
#Sum by time could be, for example: blocks of 5 minutes, where in 0-5min go all observations with start time <5:00

#SO, as A paired t-test (or Wilcoxon test) with sample sizes does not make any sense,
# thus if that is the analysis route taken, it will be needed sum data up but the N of groups should not decrease over a threshold as 
# the statistical power will be affected! see https://www.blopig.com/blog/2013/10/wilcoxon-mann-whitney-test-and-a-small-sample-size/


#It may be the case to use bootstrapping on these data, but the topic should be investigated more.
# For example, not wanting to sum up or make the mean of all individual ants, bootstrapping could help!
# Reading from "Practical 3 Programming in R(1).pdf":
# "The main point here is that these are skewed data that only take positive values. They might, for example, be the absolute asymmetry in
# the tail streamers of some bird species with elongated display plumage. With such data you:
# . Can't do a t-test on the mean because they data are so skewed.
# . Can't do a one-sample Wilcoxon test on the median, because it assumes a symmetrical
# distribution.
# . Can't do a permutation test for the median being zero, because no permutation of these data 
#   can have a median equal to or less than zero (because no values are less than zero)."

############################################
#######SOME PLOTTING########################
############################################

#Regarding plots, the consensus is to use boxplots when residuals cannot be normalised and non-parametric tests
#are used, and mean +- standard error plots (either barplots or interval plots) when parametric tests are used.


#MEAN BEHAVIOUR DURATION
mean_dur_plot<- ggplot(data = data , aes(x=Behaviour, y=time_sec, fill=fct_rev(treatment))) + 
  geom_boxplot() + 
  #geom_jitter() +
  scale_fill_manual(values=c("skyblue2","tomato1")) +
  theme(legend.position = c(0.85, 0.85),legend.title = element_blank()) +
  ggtitle("Mean behaviour duration \n before and after pathogen exposure")


data_agg <- aggregate(frequency~Behaviour+treatment, data, sum)

#BEHAVIOUR FREQUENCY
#
#to this graph should be added the Standard deviation by nest! 
freq_plot <- ggplot(data = data_agg , aes(x=Behaviour, y=frequency, fill=treatment)) + 
  geom_bar(
    aes(fill = fct_rev(treatment)), stat = "identity", position = position_dodge()
  ) + 
  scale_fill_manual(values=c("skyblue2","tomato1")) +
  theme(legend.position = c(0.85, 0.85),legend.title = element_blank()) +
  ggtitle("Behaviour frequency \n before and after pathogen exposure")

grid.arrange(mean_dur_plot, freq_plot,
             ncol = 2, nrow = 1)



############################################
#######DATA SUMMARY#########################
############################################

###summary with different tools:

###FREQUENCY summary with tapply
with(data, tapply(occurrence, list(Behaviour,treatment), sum))

###DURATION summary with aggregate (nice but doesn't save correctly, needs correction)
duration_summary <- with(data, aggregate(time_sec ~ Behaviour + treatment, FUN =  function(x) c( SD = sd(x), MN= mean(x), MAX= max(x) )))
duration_summary #the function of aggregate works but the object doesn't save correctly!
#check for outliers, may be not relevant to select them if the test is roust for outliers
with(data, aggregate(time_sec ~ Behaviour + treatment, FUN =  function(x) c( out = boxplot.stats(x)$out )))

###FREQUENCY summary with plyr
library(plyr)

freq_summary <- ddply(data, c("Behaviour","treatment"), summarise, grp.tot=sum(occurrence))
freq_summary

###DURATION summary with plyr, used for the following histograms
#all values
dur_summary <- ddply(data, c("Behaviour","treatment"), summarise, grp.mean=mean(time_sec))
dur_summary

#values excluding those 6 times larger than the mean (value chosen to exclude the larger ones and
#visualise graphs more easily. outliers should not be excluded, especially in non-parametric tests).
#If dropping them in parametric tests helps, it may be done if reasonably executed (truncate them instead of elimination?)
data_without_outliers <- data[!data$time_sec>6*mean(data$time_sec),]
dur_summary_nolarge <- ddply(data_without_outliers, c("Behaviour","treatment"), summarise, grp.mean=mean(time_sec))
dur_summary_nolarge

############################################
#######CHECKING NORMALITY###################
############################################

#check for normality visually first
#look at the data histograms, with and without extreme values
#all values
ggplot(data,aes(x=time_sec))+geom_histogram()+facet_grid(~Behaviour)+theme_bw()
#without very large values
ggplot(data_without_outliers,aes(x=time_sec))+geom_histogram()+facet_grid(~Behaviour)+theme_bw()


###plot divided by behaviour and showing treatment with the mean per group 
#with large values
ggplot(data,aes(x=time_sec, color=treatment))+
  geom_histogram(fill="white",alpha=0.5, position="identity")+
  facet_grid(~Behaviour)+
  geom_vline(data=dur_summary, aes(xintercept=grp.mean, color=treatment),
             linetype="dashed")+
  theme_bw() + theme(legend.position="top")


#large values removed
ggplot(data_without_outliers,aes(x=time_sec, color=treatment))+
  geom_histogram(fill="white",alpha=0.5, position="identity")+
  facet_grid(~Behaviour)+
  geom_vline(data=dur_summary_nolarge, aes(xintercept=grp.mean, color=treatment),
             linetype="dashed")+
  theme_bw() + theme(legend.position="top")


###check for normality with glm (see TAGS_test_Constance/summary_data_movement_131219.R from line 133 on)
# by seeing if the residuals have a normal distribution


############################################
#######TESTING##############################
############################################

# If we sum up observations by ant (I think it is the most sensible choice):
#per each behaviour, we will need to compare two paired groups,
#If data is parametric (normal residuals), use	Paired t test; if data is non-parametric, use	Wilcoxon signed rank test on paired samples




# WILCOXON TEST ASSUMPTIONS:
#1. dependent variable should be measured at the ordinal or continuous level: true for both occurrence and time_sec
#2. Your independent variable should consist of two categorical, "related groups" or "matched pairs": yes (pre-post)
#3. The distribution of the differences between the two related groups needs to be symmetrical in shape
#to check the symmetry assumption of the wilcoxon test we should probably follow https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#signed-rank-test-on-paired-samples


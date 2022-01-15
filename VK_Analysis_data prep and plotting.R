############
#
#Behavioural annotation analysis - Vasudha (Oct 2021)
#Work done with AW (Ant Epidemiology Lab)
#Final data and plots sans statistical tests
#See analysis route as outlined in __
#
############

setwd("D:\\Courses and Projects\\Summer 2021\\Analysis")
getwd()

library(ggplot2)
library(vroom)
library(RColorBrewer)
library(gridExtra)

#basedata has columns for time-sec of each behaviour by some manipulations in excel
#AW's R script has 

basedata <- vroom("D:\\Courses and Projects\\Summer 2021\\Analysis\\R3SP_R9SP_All_data.csv")
basedata$Actor <- as.factor(basedata$Actor)
basedata$frequency <- basedata$occurrence

#Keeping only the most relevant behaviour - agression(A), trophallaxis(T), 
#self-grooming(SG), grooming(G), tending brood(TB) and carrying brood(CB)
#Excluded behaviour - cross rest(CR), front rest(FR), grooming queen(GQ),
#trophallaxis queen(TQ)

#Required data
req_data <- basedata[(basedata$Behaviour=="A" | basedata$Behaviour=="T" | basedata$Behaviour=="SG" | basedata$Behaviour=="G" | basedata$Behaviour=="TB" | basedata$Behaviour=="CB"),]

library(forcats) #what's this for?

#######
#
#Preparing data 
#1. Removing double annotation
#2. Removing outliers
#3. Statistical tests (TBD)
#
#######

#######
#
#Removing double annotation###
#
#Any interactive behaviour (G, T, A) has been annotated twice - once when the 
#actor is the focal ant, and again when the receiver is the focal ant
#This would skew the number of annotations pre and post-treatment
#Important note: In G, A, and CR the actor and receiver ants are the same
#But in T and FR, the actor switches with the focal ant
#So we'll use a different method for removing double annotation in each behavior
#
#######

library(tidyverse)

#To remove the double annotations of A and G
#data1 without double annotations of A and G

data1 <- subset(req_data, req_data$Actor == req_data$Focal)

#To remove double annotations of Trophallaxis
#When I came across a higher-numbered ant interacting with a lower-numbered ant,
#I checked the annotation I'd made earlier (of lower interacting with higher ant),
#and I copied the same row to keep the timing consistent
#So, we construct a function which for the same two interacting ants, if their 
#T_start and T_stop matches, then one of the rows should be removed

#Isolating just the trophollaxis interactions

data_troph <- subset(data1, data1$Behaviour=="T")
n <- nrow(data_troph)

one_troph <- function(data_troph){
  #create an empty vector
  l <- vector("logical", length = 0) 
  for(i in 1:n){
    for(j in i:n){
      if(data_troph$Actor[i] == data_troph$Receiver[j] & data_troph$Actor[j] == data_troph$Receiver[i]){
        if(data_troph$T_start[i] == data_troph$T_start[j]){
          #append j to l
          l <- c(l, j)
        }
      }         
    }
  }
  #print(l)
  data_troph_done <- data_troph[-c(l),]
  return(data_troph_done)
}
data_troph_done <- one_troph(data_troph)

#data2 without T double annotations

data2 <- data1[!(data1$Behaviour=="T"),]
data2 <- rbind(data2, data_troph_done)

#data3 without outliers to get a better representation of mean durations

data3 <- data2[!data2$time_sec>6*mean(data2$time_sec),]

#######
#
#PLOTTING
#Use the data without double annotation or outliers
#plot the data for replicates together and separately
#
#######

#Mean duration plot

mean_dur_plot <- ggplot(data = data2 , aes(x = Behaviour, y = time_sec, fill = fct_rev(treatment))) + 
  #geom_point(aes(colour = treatment)) + 
  scale_fill_manual(values = c("darkturquoise","coral2")) +
  theme(legend.position = c(0.85, 0.85),legend.title = element_blank()) +
  ggtitle("Mean behaviour duration \n before and after pathogen exposure")

#Mean duration Boxplot for all data
p1 <- mean_dur_plot + geom_boxplot()

#Mean duration boxplot for R3SP and R9SP
data_r3 <- data2[(data2$treatment_rep=="R3SP"),]
p2 <- ggplot(data = data_r3, aes(x = Behaviour, y = time_sec, fill = fct_rev(treatment))) + 
  geom_boxplot() + scale_fill_manual(values = c("darkturquoise","coral2")) +
  theme(legend.position = c(0.85, 0.85),legend.title = element_blank()) +
  ggtitle("Mean behaviour duration before and after \n pathogen exposure - R3SP")


data_r9 <- data2[(data2$treatment_rep=="R9SP"),]
p3 <- ggplot(data = data_r9, aes(x = Behaviour, y = time_sec, fill = fct_rev(treatment))) + 
  geom_boxplot() + scale_fill_manual(values = c("darkturquoise","coral2")) +
  theme(legend.position = c(0.85, 0.85),legend.title = element_blank()) +
  ggtitle("Mean behaviour duration before and after \n pathogen exposure - R9SP")

#Behaviour frequency plots
data2_agg <- aggregate(frequency~Behaviour+treatment, data2, sum)

fp1 <- ggplot(data = data2_agg , aes(x=Behaviour, y=frequency, fill=treatment)) + 
  geom_bar(
    aes(fill = fct_rev(treatment)), stat = "identity", position = position_dodge()
  ) + 
  scale_fill_manual(values=c("darkturquoise","coral2")) +
  theme(legend.position = c(0.85, 0.85),legend.title = element_blank()) +
  ggtitle("Behaviour frequency \n before and after pathogen exposure")

#Behaviour frequency for R3SP
data_r3_agg <- aggregate(frequency~Behaviour+treatment, data_r3, sum)

fp2 <- ggplot(data = data_r3_agg , aes(x=Behaviour, y=frequency, fill=treatment)) + 
  geom_bar(
    aes(fill = fct_rev(treatment)), stat = "identity", position = position_dodge()
  ) + 
  scale_fill_manual(values=c("darkturquoise","coral2")) +
  theme(legend.position = c(0.85, 0.85),legend.title = element_blank()) +
  ggtitle("Behaviour frequency before and after \n pathogen exposure - R3SP")

#Behaviour frequency for R9SP
data_r9_agg <- aggregate(frequency~Behaviour+treatment, data_r9, sum)

fp3 <- ggplot(data = data_r9_agg , aes(x=Behaviour, y=frequency, fill=treatment)) + 
  geom_bar(
    aes(fill = fct_rev(treatment)), stat = "identity", position = position_dodge()
  ) + 
  scale_fill_manual(values=c("darkturquoise","coral2")) +
  theme(legend.position = c(0.85, 0.85),legend.title = element_blank()) +
  ggtitle("Behaviour frequency before and after \n pathogen exposure - R9SP")

grid.arrange(p1, fp1, ncol=2, nrow=1)   #mean dur and freq plots of all data
grid.arrange(p2, p3, ncol=2, nrow=1)    #mean dur plot comparison
grid.arrange(fp2, fp3, ncol=2, nrow=1)  #freq plot comparison

#######
#
#Data summary
#
#######

###FREQUENCY summary with plyr
library(plyr)

freq_summary <- ddply(data2, c("Behaviour","treatment"), summarise, grp.tot=sum(occurrence))
freq_summary

###DURATION summary with plyr, used for the following histograms
#all values
dur_summary <- ddply(data2, c("Behaviour","treatment"), summarise, grp.mean=mean(time_sec))
dur_summary


#Checking for Normality - AW's script
#Statistical tests (TBD)






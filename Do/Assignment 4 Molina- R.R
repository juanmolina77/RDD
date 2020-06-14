
# Assignment 4, summer school 
install.packages("readstata13")
library(readstata13)
library(dplyr)
library(ggplot2)
library(rdd)
library(stargazer)

setwd("~/Juan Esteban Molina A/Universidad/Cursos/Summer School/Causal Inference and Research Design/Git/Titanic/RDD/RDD/Data")
Data <- read.dta13("hansen_dwi.dta")

# Exercise 3 dummy (D) if BAC1>=0.08
Data <- Data %>% 
  mutate(
    D  = if_else(bac1 >=0.08, 1, 0))

# Exercise 4 Histogram of Bac 1

ggplot(Data, aes(x=bac1)) + geom_histogram(binwidth = 0.001) +
labs(x="Bac", y="frequency", title="Bac Histogram")+
geom_vline(xintercept=0.08, col="black", lwd=0.02)

#McCrary densiry test
DCdensity(Data$bac1, cutpoint = 0.08)

#running placebos at different cutpoints
DCdensity(Data$bac1, cutpoint = 0.06)
DCdensity(Data$bac1, cutpoint = 0.07)
DCdensity(Data$bac1, cutpoint = 0.09)
DCdensity(Data$bac1, cutpoint = 0.1)

#Exercise 5 bac1 is the independent variable


reg2 <- lm(male~bac1+D+bac1*D, data=Data)
summary(reg2)
reg3 <- lm(white~bac1+D+bac1*D, data=Data)
summary(reg3)
reg4 <- lm(aged~bac1+D+bac1*D, data=Data)
summary(reg4)
reg5 <- lm(acc~bac1+D+bac1*D, data=Data)
summary(reg5)

stargazer(reg2,reg3,reg4,reg5, type = "latex")


#Exercise 6 

Data$contador <- 1 

participation_male <- Data%>%
  group_by(bac1)%>%
  summarize(part_male=sum(male)/sum(contador))

participation_white <- Data%>%
  group_by(bac1)%>%
  summarize(part_white=sum(white)/sum(contador))

participation_acc <- Data%>%
  group_by(bac1)%>%
  summarize(part_acc=sum(acc)/sum(contador))

average_age <- Data%>%
  group_by(bac1)%>%
  summarize(part_age=sum(aged)/sum(contador))

Participation <- cbind(participation_male,participation_white,participation_acc,average_age)
Participation <- Participation[,c(1,2,4,6,8)]

Participation <- Participation %>% 
  mutate(
    D  = if_else(bac1 >=0.08, 1, 0))

# Male participation
ggplot(aes(bac1, part_male, colour = factor(D)), data = Participation) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "BAC", y = "average male per bac level", title="Male")+
  scale_y_continuous(limits=c(0.6,0.9))

# quadratic
ggplot(aes(bac1, part_male, colour = factor(D)), data = Participation) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "loess", se = F) +
  labs(x = "BAC", y = "average male per bac level", title="Male")+
  scale_y_continuous(limits=c(0.6,0.9))

#White participation
ggplot(aes(bac1, part_white, colour = factor(D)), data = Participation) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "BAC", y = "average white per bac level", title="White")+
  scale_y_continuous(limits=c(0.7,1))

#quadratic
ggplot(aes(bac1, part_white, colour = factor(D)), data = Participation) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "loess", se = F) +
  labs(x = "BAC", y = "average white per bac level", title="White")+
  scale_y_continuous(limits=c(0.7,1))


# Acc participation
ggplot(aes(bac1, part_acc, colour = factor(D)), data = Participation) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "BAC", y = "average acc per bac level", title="Accident")+
  scale_y_continuous(limits=c(0,0.5))


ggplot(aes(bac1, part_acc, colour = factor(D)), data = Participation) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "loess", se = F) +
  labs(x = "BAC", y = "average acc per bac level", title="Accident")+
  scale_y_continuous(limits=c(0,0.5))

# average age per bac level
ggplot(aes(bac1, part_age, colour = factor(D)), data = Participation) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "BAC", y = "average age per bac level", title="Age")+
  scale_y_continuous(limits=c(30,45))
 
ggplot(aes(bac1, part_age, colour = factor(D)), data = Participation) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "loess", se = F) +
  labs(x = "BAC", y = "average age per bac level", title="Age")+
  scale_y_continuous(limits=c(30,45))

#Exercise 7

reg6 <- lm(recidivism~bac1+D, data=Data)
summary(reg6)
reg7 <- lm(recidivism~bac1+D+bac1*D, data=Data)
summary(reg7)
reg8 <- lm(recidivism~I(bac1^2)+D+bac1*D, data=Data)
summary(reg8)

stargazer(reg6,reg7,reg8, type = "latex")


# Exercise 8
Data2 <- Data[Data$bac1<=0.15,] # Data with observations of bac level lower or equal to 0.15

Data2$contador <- 1 

participation_recidivism <- Data2%>%
  group_by(bac1)%>%
  summarize(part_recividism=sum(recidivism)/sum(contador))

participation_recidivism <- participation_recidivism %>% 
  mutate(
    D  = if_else(bac1 >=0.08, 1, 0))

ggplot(aes(bac1, part_recividism, colour = factor(D)), data = participation_recidivism) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "BAC", y = "average recividism per bac level", title="Recividism")+
  scale_y_continuous(limits=c(0,0.2))

ggplot(aes(bac1, part_recividism, colour = factor(D)), data = participation_recidivism) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0.08, colour = "grey", linetype = 2)+
  stat_smooth(method = "loess", se = F) +
  labs(x = "BAC", y = "average recidivism per bac level", title="Recividism")+
  scale_y_continuous(limits=c(0,0.2))

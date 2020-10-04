library(pander)
library(knitr)
library(ggplot2)
library(psych)
library(lsr)
library(corrplot)


student_survey <-read.csv("student-survey.csv")
cov(student_survey)
#we would use cov() because it measures how the two are linearly related. A positive covariance would indicate a positive linear relationship between the variables, and a negative covariance would indicate the opposite.There is a negative correlation between time reading and time watching TV
#Time reading: measurment of how much time is spent on reading
#TimeTV is a measurement of how much time is spent watching TV
#Measurement of students happiness
#Gender: Reveals what gender the person is
cor(student_survey$TimeReading, student_survey$TimeTV, method = "pearson")
cor(student_survey$TimeReading, student_survey$TimeTV, method = "kendall")
cor(student_survey$TimeReading, student_survey$TimeTV, method = "spearman")
#All of these show a strong correlation, correlation is one of the first things you run your data through although it is very helpfull in picking out some obvious relationships it is not the end game method you want to use
#Here is how it looks graphed
ggplot(data = student_survey, aes(x = TimeReading, y = TimeTV)) + 
  geom_point() + 
  geom_smooth(method='lm', formula= y~x, se = FALSE, color = "blue") + 
  ylab("Time watching TV") +
  xlab("Time reading") + 
  ggtitle("Student Survey: Time watching tv vs time reading") +
  theme_bw()
cor.table = cor(student_survey)
corrplot(cor.table)
cor.test(formula = ~ student_survey$TimeReading + student_survey$TimeTV,
         data = student_survey)
ggplot(data = student_survey, 
       aes(x = TimeReading, 
           y = TimeTV)) + 
  geom_smooth(method='lm', 
              formula= y~x, 
              se = TRUE, 
              color = "blue", 
              fill = "red", 
              alpha = 0.5) + 
  geom_point() + 
  ylab("Time Watching TV") +
  xlab("Time Reading") + 
  ggtitle("TV vs Reading") +
  theme_bw()
cor.test(formula = ~ student_survey$TimeReading + student_survey$TimeTV,
         data = student_survey, conf.level = 0.99)
ggplot(data = student_survey, 
       aes(x = TimeReading, 
           y = TimeTV)) + 
  geom_point() + 
  geom_smooth(method='lm', 
              formula= y~x, 
              se = TRUE, 
              level = 0.99, 
              color = "red", 
              fill = "blue", 
              alpha = 0.5) + 
  ylab("Time watching TV") +
  xlab("Time reading") + 
  ggtitle("reading vs TV") +
  theme_bw()
#We have a neagtive correlation with TimeTV and TimeReading it is a negative correlation of -.88 which is close to -1 which means it is a very strong correlation. Gender doesn't reveal anything in partiular and there is a small correlation between happines and TimeTV
cor(student_survey$TimeReading, student_survey$TimeTV)
fit <- lm(TimeReading ~ TimeTV , data = student_survey)
summary(fit)$r.squared
#Our rsquare value is .77 which means that this falls into our expected variance
#From out tests we have run it is quite obvious that people who spend more time watching tv spend less time reading

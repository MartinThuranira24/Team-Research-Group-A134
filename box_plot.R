install.packages("tidyverse")
library(readr)
toyota <- read_csv("toyota.csv")
df<-toyota



#is this normal data?  
hist(df2$gbpfood)

#if it were normal data you would do this
t.test(df2$gbpfood ~ df2$isAsian)# for normal data  

#if its not normal data you would do this
wilcox.test(df2$gbpfood ~ df2$isAsian)# for non parametric data  

#here is the box plot
boxplot(df2$gbpfood ~ df2$isAsian,  xlab = "Provenance", ylab = "£s weekly spent on food", main = "Money spent on food ")  

#new exercise - lets see if people from asia spend more on transport coming to college
#get rid of people who say longer than 2 hours
df2<-subset(df,gbptransport<120)

hist(df2$gbptransport)
t.test(df2$gbptransport ~ df2$isAsian)# for normal data  
wilcox.test(df2$gbptransport ~ df2$isAsian)# for non parametric data  
boxplot(df2$gbptransport ~ df2$isAsian,  xlab = "Provenance", ylab = "£s weekly spent on transport", main = "Money spent on transport p/w ")  

#new exercise - do women or men spend more time in the gym
#removing non-binary and "rather not say" because too few (not because of bigotry!)
#also get rid of people who say longer than 2.5 hours per week
#however, that is extremely contestable - see what results you get if you change the minutes cut-off

df2<-subset(df,gender=="Man" | gender=="Woman")
df2<-subset(df2,pe_minutes<150)

#histogram of time doing exercise
hist(df2$pe_minutes)

#t.test(df2$pe_minutes~  df2$gender)# for normal data  - but this is clearly not normal data
wilcox.test(df2$pe_minutes ~ df2$gender)# for non parametric data  
boxplot(df2$pe_minutes ~ df2$gender,  xlab = "Gender", ylab = "Time in Gym Minutes", main = "Physical Exercise Men vs Women") 

#challenges - get a histogram of male height and female height
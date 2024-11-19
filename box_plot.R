install.packages("tidyverse")
library(readr)
toyota <- read_csv("toyota.csv")
# View the toyota dataset
View(toyota)
# get summary of dataset
summary(toyota)
# view unique values for toyota model
unique(toyota$model)
# view unique values for toyota transmission
unique(toyota$transmission)
# means of different transmissions
mean(toyota$transmission == "Manual")

mean(toyota$transmission == "Semi-Auto")

mean(toyota$transmission == "Other")

mean(toyota$transmission == "Automatic")

hist(toyota$transmission)
# histogram of toyota prices
hist(toyota$price, xlab = "Price")
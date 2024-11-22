# ------------------------------------------------------------------------------
## Importing the dataset
toyota <- read.csv('toyota.csv')
#Selecting the dependent and independent variable
df <- subset(toyota, select = c(price, model))

# ------------------------------------------------------------------------------
## Visualization plots




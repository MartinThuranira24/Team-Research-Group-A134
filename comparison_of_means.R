# ------------------------------------------------------------------------------
## Importing the dataset
toyota <- read.csv('toyota.csv')
#Selecting the dependent and independent variable
df <- subset(toyota, select = c(price, model))

# ------------------------------------------------------------------------------
## Visualization plots


# ------------------------------------------------------------------------------
## Splitting the data into training and test set
#install.packages('caTools')
library(caTools)
set.seed(123)

split <- sample.split(df$tax, SplitRatio = 0.8)
training_set <- subset(df, split == TRUE)
test_set <- subset(df, split == FALSE)

## Regressor
model <- lm(
    formula = price ~ model,
    data = training_set
)
summary(model)
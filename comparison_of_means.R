# ------------------------------------------------------------------------------
## Importing the dataset
toyota <- read.csv("toyota.csv")
# Selecting the dependent and independent variable
data <- subset(toyota, select = c(price, model))


# ------------------------------------------------------------------------------
## Data Cleaning
# Removing the outliers using the Inter Quartile Range(IQR) Method
Q1 <- quantile(data$price, 0.25, na.rm = TRUE)
Q3 <- quantile(data$price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

df <- data[data$price >= lower_bound & data$price <= upper_bound, ]

# Filtering data with less than 10 observations
model_count <- table(df$model)
selected_models <- names(model_count[model_count >= 10])
df <- df[df$model %in% selected_models, ]

# Mean prices for each model
mean_prices <- aggregate(df$price ~ df$model, FUN = mean)


# ------------------------------------------------------------------------------
## Statistical Tests
# Kruskal Test(global test)
kruskal_test <- kruskal.test(df$price ~ df$model, data = df)
# Pairwise Wilcox Test(car model vs car model)
pairwise_results <- pairwise.wilcox.test(
    df$price, df$model,
    p.adjust.method = "BH" # Benjamini-Hochberg method
)


# ------------------------------------------------------------------------------
## Visualization plots
# Box Plots
boxplot(
    df$price ~ df$model,
    data = df,
    main = "Boxplot of Price by Toyota Car Model",
    xlab = "Car Model",
    ylab = "Price in Sterling Pounds(£)",
    col = "lightblue",
    las = 2,
    border = "darkblue"
)

# Histogram
hist(
    df$price,
    main = "Histogram of Price Distribution among the Toyota Car Models",
    xlab = "Price in Sterling Pounds(£)",
    ylab = "Density",
    col = "lightgreen",
    border = "black",
    breaks = 20,
    freq = FALSE
)
lines(
    density(df$price, na.rm = TRUE),
    col = "blue", lwd = 2
)

# Checking for skewness
install.packages("e1071")
library(e1071)

skew_value <- skewness(df$price, na.rm = TRUE)


# ------------------------------------------------------------------------------
## Splitting the data into training and test set
install.packages("caTools")
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

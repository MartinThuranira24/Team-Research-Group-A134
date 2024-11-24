# ------------------------------------------------------------------------------
## Preparing the data set
toyota_df <- read.csv("toyota.csv")
toyota_df <- toyota_df[toyota_df$year >= 2010, ]
data <- subset(toyota_df, select = c(mileage, tax))

# ------------------------------------------------------------------------------
## Tax Histogram
hist_data <- hist(
  data$tax,
  breaks = 7,
  col = "lightblue",
  main = "Histogram of Road Tax for used Toyota Cars in the UK (2010 - 2020).",
  xlab = "Road Tax (£)",
  border = "black"
)
# Calculating the bell curve
tax_mean <- mean(data$tax, na.rm = TRUE)
tax_sd <- sd(data$tax, na.rm = TRUE)
bin_width <- diff(hist_data$breaks)[1]
x_vals <- seq(
  min(data$tax, na.rm = TRUE), max(data$tax, na.rm = TRUE),
  length = 1000
)
y_vals <- dnorm(
  x_vals,
  mean = tax_mean, sd = tax_sd
) * bin_width * length(data$tax)
lines(x_vals, y_vals, col = "red", lwd = 2)

# ------------------------------------------------------------------------------
## Correlation test between tax and mileage
cor.test(data$mileage, data$tax, method = "kendall")

# ------------------------------------------------------------------------------
## Scatter Plot of Tax vs Mileage
plot(
  data$mileage, data$tax,
  main = "Scatterplot of Tax vs Mileage for used Toyota cars in the UK (2010 - 2020).",
  xlab = "Mileage",
  ylab = "Road Tax (£)",
  pch = 19,
  col = "blue"
)
abline(lm(data$tax ~ data$mileage), col = "red")

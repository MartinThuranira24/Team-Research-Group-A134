hist(toyota$mileage, main = "Histogram of mileage of toyota cars", xlab = "Car mileage")

# filtered_toyota = subset(toyota,tax>5)

hist(toyota$tax, main = "Histogram of road tax of toyota cars", xlab = "Road Tax")

# cor.test(toyota$mileage,toyota$tax, method = "spearman")

#genertae scatterplot

plot(toyota$mileage, toyota$tax,
     xlab = "Road Tax in £", ylab =
       "Mileage of vehicle", main
     = "Scatterplot of Road Tax vs Mileage of used Toyota car")
abline(lm(toyota$tax ~
            toyota$mileage), col = "red")




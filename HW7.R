data <- read.csv("/cloud/project/Earnings_and_Height.csv")
cat("Alexis Leclerc\n")
options(digits = 5)
colnames(data)
height <- data$height
earnings <- data$earnings

cat("Question 4a\n")
med <- median(height)
cat("The median of height is,", med,"inches\n")

cat("\n Question 4b.i\n")
aheless67 <- data[data$height <= 67, "earnings"]
meanaheless67 <- mean(aheless67)
cat("The average yearly earnings in dollars whose height is at most 67 inches is,",meanaheless67,"\n")

cat("\nQuestion 4b.ii\n")
ahemore67 <- data[data$height > 67, "earnings"]
meanahemore67 <- mean(ahemore67)
cat("The average yearly earnings in dollars whose height is more than 67 inches is,",meanahemore67,"\n")

cat("\nQuestion 4b.iii\n")
mean_diff67 <- (meanahemore67 - meanaheless67)
cat("The Difference in average yearly earnings is,", mean_diff67)
ttest1 <- t.test(ahemore67,aheless67,conf.level = .95)
ttest1conf <- ttest1$conf.int
cat("The 95% confidence interval is from", ttest1conf[1],"to",ttest1conf[2],"\n")
cat("at 5% level of significance the data shows that tall workers do earn more on average\n")

cat("\nQuestion 4c\n")
plot(height, earnings, main = "Scatterplot of earnings on height", xlab = "Height", ylab = "Earnings", pch = 19, col = "blue")
cat("Horizontal lines in the scatter plot suggest that there is very little to no variation in the Dependent variable (Earnings) for different values of the Independent variable (Height).")
cat("We can say that there is a very weak to no linear relationship between X and Y\n")

cat("\nQuestion 4d\n")
model <- lm(earnings~height)
summary(model)
cat("The Intercept B0_hat is -512.7\n")
cat("The slope B1_hat is 707.7\n")
cat("R-Squared is 0.0109\n")
cat("The Residual Standard Error (RSE) is 26,800\n")
cat("According to this result, a one-inch increase in height is associated with a $",707.7, " increase in earnings.\n")

predict_earnings <- function(height) {
  B0_hat <- -512.7
  B1_hat <- 707.7
  Yhat <- B0_hat + B1_hat * height
  return(Yhat)
}
height_1 <- 67
earnings_1 <- predict_earnings(height_1)
height_2 <- 70
earnings_2 <- predict_earnings(height_2)
height_3 <- 65
earnings_3 <- predict_earnings(height_3)
cat("Predicted earnings for worker with height 67 inches: $", earnings_1, "\n")
cat("Predicted earnings for worker with height 70 inches: $", earnings_2, "\n")
cat("Predicted earnings for worker with height 65 inches: $", earnings_3, "\n")

cat("\nQuestion 2\n")
covar <- cov(height,earnings)
cat("The sample covariance between height and earnings is", covar,"\n")
varx <- var(height)
cat("the sample variance of height is", varx,"\n")
b1hat <- covar/varx
cat("The sample covariance of x,y divided by the variance of x is", b1hat,"\n")
cat("This result agrees with the value of B1_hat from the lm output\n")
cat("This is due to the fact that B1hat equals sample covariance of x,y divided by variance of x\n")
cat("R is doing the same calculations as the ones I did manually\n")

##############################################################################################################
#                                           Assignment 8                                                     #
##############################################################################################################
cat("\nQuestion 4d\n")
model <- lm(earnings~height)
summary(model)
cat("The Intercept B0_hat is -512.7\n")
cat("The slope B1_hat is 707.7\n")
cat("R-Squared is 0.0109\n")
cat("The Residual Standard Error (RSE) is 26,800\n")
cat("According to this result, a one-inch increase in height is associated with a $",707.7, " increase in earnings.\n")

cat("\nQuestion 4.2E\n")
height_cm <- data$height * 2.54
earnings <- data$earnings
female <- data[data$sex==0, ]
male <- data[data$sex==1,]
female$heightcm <- female$height*2.54
male$heightcm <- male$height*2.54

model_cm <- lm(earnings~height_cm)
summary(model_cm)
cat("The Intercept B0_hat is -512.7 dollars\n")
cat("The slope B1_hat is 278.6 $/cm\n")
cat("R-Squared is 0.0109\n")
cat("The Residual Standard Error (RSE) is 26,800\n")
cat("According to this result, a one-cm increase in height is associated with a $",278.60, " increase in earnings.\n")
cat("\nSER is in dollars while R^2 is unitless\n")

cat("\nQuestion 4.2F\n")
model_female <- lm(female$earnings~female$heightcm)
summary(model_female)
cat("\nFemale Height and Earnings\n")
cat("The Intercept B0_hat is 12,650.90 dollars\n")
cat("The slope B1_hat is 201.30 $/cm\n")
cat("Unadjusted R-Squared is 0.00267\n")
cat("Adjusted R-Squared is 0.00257\n")
cat("The Residual Standard Error (RSE) is 26,800\n")
cat("According to this result, a one-cm increase in height is associated with a $",201.30, " increase in earnings.\n")


cat("\nQuestion 4.2G\n")
model_male <- lm(male$earnings~male$heightcm)
summary(model_male)
cat("\nMale Height and Earnings\n")
cat("The Intercept B0_hat is -43,130.30 dollars\n")
cat("The slope B1_hat is 514.50 $/cm\n")
cat("Unadjusted R-Squared is 0.0209\n")
cat("Adjusted R-Squared is 0.0207\n")
cat("The Residual Standard Error (RSE) is 26,700\n")
cat("According to this result, a one-cm increase in height is associated with a $",514.50, " increase in earnings.\n")

cat("\nQuestion 4.2H\n")
cat("\nThe exogenity condition cov(x,e)=0 is more general than the zero conditional mean condition\n")
cat("\nWhen Sex is added to the model exogenity is present. Sex is an important variable that has significance when explaining earnings. Specifically male vs female earnings.\n")
cat("\nSex is correlated to Height and a determinant of earnings\n")
cat("When Sex is not in the model for earnings~height, exogenity is not present.")

plot(height, earnings, main = "Scatterplot of Earnings on Height", xlab = "Height", ylab = "Earnings", pch = 19, col = "blue")
abline(model, col = "red")
cat("\nNo the line does not fit the data very vell\n")


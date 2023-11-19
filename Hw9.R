data <- read.csv("/cloud/project/Earnings_and_Height.csv")
library(sandwich)
cat("Alexis Leclerc\n")
options(digits = 5)
colnames(data)

female <- data[data$sex==0, ]
male <- data[data$sex==1,]
height <- data$height
earnings <- data$earnings

cat("\nQuestion 5.1a\n")
model <- lm(earnings~height)
summary(model)
cat("\nYes the estimated slope is statistically significant.\n")

robust_se <- vcovHC(model,type="HC1")
coeftest(model, vcov = robust_se)
SEB11 <- 50.4
B11<- 707.7
CIP1 <- B11 + (1.96*SEB11)
CIN1 <- B11 - (1.96*SEB11)
cat("\nThe Confidence Interval for B1 is,",CIN1, "to",CIP1,"\n")

cat("\nQuestion 5.1b\n")
model_female <- lm(female$earnings~female$height)
summary(model_female)
cat("\nYes the estimated slope is statistically significant.\n")

robust_se1 <- vcovHC(model_female,type="HC1")
coeftest(model_female, vcov = robust_se1)
SEB12 <- 97.6
B12<- 511.2
CIP2 <- B12 + (1.96*SEB12)
CIN2 <- B12 - (1.96*SEB12)
cat("\nThe Confidence Interval for B1 is,",CIN2, "to",CIP2,"\n")


cat("\nQuestion 5.1c\n")
model_male <- lm(male$earnings~male$height)
summary(model_male)
cat("\nYes the estimated slope is statistically significant.\n")

robust_se2 <- vcovHC(model_male,type="HC1")
coeftest(model_male, vcov = robust_se2)
SEB13 <- 98.9
B13<- 1306.9
CIP3 <- B13 + (1.96*SEB13)
CIN3 <- B13 - (1.96*SEB13)
cat("\nThe Confidence Interval for B1 is,",CIN3, "to",CIP3,"\n")

cat("\nQuestion 5.1d\n")
model_interaction <- lm(earnings ~ height * sex, data = data)
summary(model_interaction)
robust_se3 <- vcovHC(model_interaction,type="HC1")
coeftest(model_interaction, vcov = robust_se3)

B1_diff <- B13 - B12
SETest <- sqrt((SEB12^2)+(SEB13^2))
tstat <- B1_diff/SETest
cat("\nThe tstat",tstat,"is larger than 1.96 so we reject the null hypothesis\n")
cat("\nthere is a statistical difference between the effect of height on earnings for men and women \n")

cat("\nQuestion 5.1e\n")
cat("Using a .05 level of significance")
cat("\nWhen examining the effect of height on earnings restricted to occupations that require no strength, we see that height is still statistically significant in the model but sex is no longer significant\n")
cat("\nWhen examining the effect for occupations that require strength, we see that both height and sex are statistically significant in the model.\n")

occ1 <- c(1,2,3,4,5)
occ <- c(9,10,11,13,12,14,15)
occupation <- data[data$occupation==occ, ]

occ_model <- lm(occupation$earnings~occupation$height+occupation$sex)
summary(occ_model)






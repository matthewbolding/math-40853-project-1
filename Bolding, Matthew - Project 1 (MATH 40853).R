install.packages("tidyverse")
library(tidyverse)
?txhousing

head(txhousing)

fortWorthHousing <- txhousing[txhousing$city == "Fort Worth", ]
austinHousing <- txhousing[txhousing$city == "Austin", ]
houstonHousing <- txhousing[txhousing$city == "Houston", ]


################
## FORT WORTH ##
################

# Use log(sales) to predict log(adjusted volume)
fortWorthModel <- lm(log(volume) ~ log(sales), data = fortWorthHousing)
summary(fortWorthModel0)

plot(log(fortWorthHousing$volume) ~ log(fortWorthHousing$sales),
     xlab = "Sales (Log Scaled)",
     ylab = "Volume (Log Scaled)") # plot the log-transformed variables, not in log-scale  
abline(fortWorthModel0, untf=F)

names(summary(fortWorthModel0))

fwM0e <- fortWorthModel0$residuals #### Should these residuals be scaled?
summary(fortWorthModel0)
fwM0.std.e <- fwM0e/summary(fortWorthModel0)$sigma

plot(log(fortWorthHousing$sales), fwM0.std.e,## we need to find the std. res.
     xlab = "Sales (Log Scaled)",
     ylab = "Residuals",
     main = "Fort WorthSales (Log Scaled) vs. Residuals",
     ylim = c(-3.5, 3.5))
abline(a = -2, b = 0, lty = 2, col = "orange")
abline(a = 2, b = 0, lty = 2, col = "orange")
abline(a = -3, b = 0, col = "red")
abline(a = 3, b = 0, col = "red")

fwM0.std.e[fwM0.std.e >= 2 | fwM0.std.e <= -2]

plot(fwM0.std.e) #### There appears to be a relationship here!

drop <- c("city")
fwNumData <- fortWorthHousing[,!(names(fortWorthHousing) %in% c("city"))]
round(cor(log(fwNumData)), 4)

View(txhousing[txhousing$city == "Fort Worth", ])
qqnorm(fwM0e)
qqline(fwM0e)

## Confidence Interval

confidenceInterval95 <- function(model, housingData) {
  b1 <- model$coefficients[[2]]
  std.err.b1 <- sqrt(summary(model)$sigma^2
                     /((nrow(housingData) - 1) * sd(log(housingData$sales))^2))
  t.value <- qt(0.975, nrow(housingData) - 2)
  
  upperBound <- b1 + (std.err.b1 * t.value)
  lowerBound <- b1 - (std.err.b1 * t.value)
  return(c(lowerBound, upperBound))
}

fortWorthCI <- confidenceInterval95(fortWorthModel, fortWorthHousing)

austinCI <- confidenceInterval95(austinModel, austinHousing)

houstonCI <- confidenceInterval95(houstonModel, houstonHousing)

fortWorthModel0$coefficients[[2]]
std.err.b1 <- sqrt(summary(fortWorthModel0)$sigma^2
                   /((nrow(fortWorthHousing) - 1) * sd(log(fortWorthHousing$sales))^2))
std.err.b1
qt(0.975, nrow(fortWorthHousing) - 2)

confint(fortWorthModel, 'log(sales)', level = 0.95)

## Prediction Interval

predict(fortWorthModel, newdata = data.frame(sales = log(500)))[[1]]
summary(fortWorthModel)$sigma^2


predictionInterval95 <- function(model, housingData, value) {
  prediction <- predict(model, newdata = data.frame(sales = value))[[1]]
  mse <- summary(model)$sigma^2
  variance <- sd(log(housingData$sales))^2
  size <- nrow(housingData)
  mean <- mean(log(housingData$sales))
  
  t.value <- qt(0.975, size - 2)
  
  # cat("prediction: ", prediction, "\n")
  # cat("mse: ", mse, "\n")
  # cat("variance: ", variance, "\n")
  # cat("size: ", size, "\n")
  # cat("mean: ", mean, "\n")
  
  se.pred <- sqrt(mse * (1 + (1/size) + ((prediction - mean)^2/((size - 1) * variance))))
  # print(se.pred)
  
  upperBound <- prediction + (se.pred * t.value)
  lowerBound <- prediction - (se.pred * t.value)
  return(c(exp(lowerBound), exp(upperBound)))
}


fortWorthPI <- predictionInterval95(fortWorthModel, fortWorthHousing, 500)
fortWorthPI

austinPI <- predictionInterval95(austinModel, austinHousing, 500)
houstonPI <- predictionInterval95(houstonModel, houstonHousing, 500)


fortWorthModel0MSE <- (1/(nrow(fortWorthHousing) - 2)) * sum(fwM0e^2)
x.bar = mean(log(fortWorthHousing$sales))
x.var = sd(log(fortWorthHousing$sales))^2

se.pred <- sqrt(fortWorthModel0MSE * (1 + (1/nrow(fortWorthHousing)) + ((500 - x.bar)^2)/(nrow(fortWorthHousing) - 1) * x.var))
se.pred

################
##   AUSTIN   ##
################

# Use log(sales) to predict log(adjusted volume)
austinModel <- lm(log(volume) ~ log(sales), data = austinHousing)
summary(austinModel0)

plot(log(austinHousing$volume) ~ log(austinHousing$sales)) # plot the log-transformed variables, not in log-scale  
abline(austinModel0, untf=F)

aM0e <- austinModel0$residuals
aM0.std.e <- aM0e/0.133
plot(log(austinHousing$sales), aM0.std.e,## we need to find the std. res.
     xlab = "Sales (Log Scaled)",
     ylab = "Residuals",
     main = "Austin Sales (Log Scaled) vs. Residuals",
     ylim = c(-3.5, 3.5))
abline(a = -2, b = 0, lty = 2, col = "orange")
abline(a = 2, b = 0, lty = 2, col = "orange")
abline(a = -3, b = 0, col = "red")
abline(a = 3, b = 0, col = "red")
plot(aM0.std.e) #### There appears to be a relationship here, again!

aM0.std.e[aM0.std.e >= 2 | aM0.std.e <= -2]


qqnorm(aM0e)
qqline(aM0e)

################
##   HOUSTON  ##
################

# Use log(sales) to predict log(adjusted volume)
houstonModel <- lm(log(volume) ~ log(sales), data = houstonHousing)

# Regression Line Plot
plot(log(houstonHousing$volume) ~ log(houstonHousing$sales),
     xlab = "Sales (Log Scaled)",
     ylab = "Volume (Log Scaled)",
     main = "houston Regression Line")
abline(houstonModel, untf=F)

# Calculate the Standardized Residuals
houstonResiduals <- houstonModel$residuals
houstonStandardizedResiduals <- houstonResiduals/summary(houstonModel)$sigma

# Plot scaled Sales vs. Standardized Residuals
plot(log(houstonHousing$sales), houstonStandardizedResiduals,
     xlab = "Sales (Log Scaled)",
     ylab = "Standardized Residuals",
     main = "houston Sales vs. Standardized Residuals",
     ylim = c(-3.5, 3.5))

abline(a = -2, b = 0, lty = 2, col = "orange")
abline(a = 2, b = 0, lty = 2, col = "orange")
abline(a = -3, b = 0, col = "red")
abline(a = 3, b = 0, col = "red")

# Plot Index vs. Standardized Residuals
plot(houstonStandardizedResiduals,
     ylab = "Standardized Residuals",
     main = "houston Index vs. Standardized Residuals")

length(houstonStandardizedResiduals[houstonStandardizedResiduals >= 2 | houstonStandardizedResiduals <= -2])

qqnorm(hM0e)
qqline(hM0e)

createPlots <- function(city, model, housingData) {
  # Regression Line Plot
  plot(log(housingData$volume) ~ log(housingData$sales),
       xlab = "Sales (Log Scaled)",
       ylab = "Volume (Log Scaled)",
       main = paste(city, "Regression Line"))
  abline(model, untf=F)
  
  # Calculate the Standardized Residuals
  residuals <- model$residuals
  standardizedResiduals <- residuals/summary(model)$sigma
  
  # QQ Plot
  qqnorm(standardizedResiduals,
         main = paste(city, "Normal Q-Q Plot"))
  qqline(standardizedResiduals)
  
  # Plot scaled Sales vs. Standardized Residuals
  plot(log(housingData$sales), standardizedResiduals,
       xlab = "Sales (Log Scaled)",
       ylab = "Standardized Residuals",
       main =  paste(city, "Sales vs. Standardized Residuals"),
       ylim = c(-3.5, 3.5))
  
  abline(a = -2, b = 0, lty = 2, col = "orange")
  abline(a = 2, b = 0, lty = 2, col = "orange")
  abline(a = -3, b = 0, col = "red")
  abline(a = 3, b = 0, col = "red")
  
  # Plot Index vs. Standardized Residuals
  plot(standardizedResiduals,
       ylab = "Standardized Residuals",
       main = paste(city, "Index vs. Standardized Residuals"))
}

createPlots("Fort Worth", fortWorthModel, fortWorthHousing)

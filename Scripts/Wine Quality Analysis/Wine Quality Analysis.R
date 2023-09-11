setwd("C:/Users/Omary/OneDrive/Desktop/Assignments/CL2603/Assignment/Excel Data Sheets")
Wine <- read.csv ("C:/Users/Omary/Desktop/Wine Quality Project/Data Sheets/wine2157047.csv")
corr <- cor(Wine)
corr_coeffs <- corr["quality",]
barplot(corr_coeffs,xlab="variable",ylab="Correlation Coefficient", main="Correlation Coefficients with Wine Quality")
par(mfrow=c(2,2)) # Produce a 2 by 2 for the plots.
  for (i in 1:11){boxplot(Wine[,i]~Wine$quality, xlab = "Quality", ylab = names(Wine)[i], main = names(Wine)[i])} # All 11 Explanatory Variables Affecting the Quality of Wine.
boxplot(Wine[,11]~Wine$quality, xlab = "Quality", ylab = names(Wine)[11], main = names(Wine)[11])$stats # Stats to show for Alcohol.
boxplot(Wine[,2]~Wine$quality, xlab = "Quality", ylab = names(Wine)[2], main = names(Wine)[2])$stats # Stats to show Volatile Acidity.
pairs(Wine[,-12], col = Wine$quality, pch = 19) # A Scatter Plot Matrix Between the Explanatory Variables and Quality.
#Second Part
fit <- lm(quality~.,Wine) 
step <- step(fit)# Using Step-wise Regression 
par(mfrow=c(2,2))
plot(step,which=1:4) # Producing Residual Plots to see if the assumptions are met and this model is sufficient enough for it.
par(mfrow=c(1,1))
plot(step,which=5)
log.model <- lm(log(quality)~.,Wine) # Transforming Model
par(mfrow=c(2,2))
plot(log.model) # Explained in my Document
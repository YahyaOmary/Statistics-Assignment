install.packages("lmtest")
library(lmtest)
Cola = read.csv("C:/Users/Omary/Desktop/Wine-Quality-Analysis/Data Sheets/yield2157047.csv")
Graph1 = plot(Cola$Temp,Cola$Yield,xlab = "Temperature (Celsius)",ylab = "Yield (Thousands of Litres)",main = "Scatterplot of Yield vs. Temperature")
  model <- lm(Yield~Temp, Cola)
  abline(model,col="red") # Using a linear Regression Model
  summary(model)
  par(mfrow=c(2,2))
  plot(model) # Plotting Residual Plots
model2 <- lm(Yield~Temp, Cola, weights = 1/(fitted(lm(Yield~Temp,Cola))^2)) # Using a Weighted Model
  Cola.2<- data.frame(Temp=25)
  pred <- predict(model2,Cola.2,interval="prediction")

install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
petrol = read.csv("C:/Users/Omary/Desktop/Wine-Quality-Analysis/Data Sheets/petrol2157047.csv")
data.frame(petrol)
Boxplots =boxplot(energy~method,data=petrol,main="Difference in Specific Energy between the 2 Methods",xlab ='Method',ylab ='Specific Energy')# Produced a Box plot.
petrol.t.test <-t.test(energy~method,petrol,var.equal=TRUE) # Using t.test to test at 5% significance if the mean specific energy level differs for the 2 processes.
Graph1 = ggplot(petrol,aes(y=energy,x=method))
  stat_summary(fun.y=mean,geom = "bar",color="black",fill = "white")
  stat_summary(fun.data =mean_se, geom ="errorbar",width=.2)
  geom_bar(stat = "identity",position="fill")
  scale_y_continuous(name="Specific Energy")
  scale_x_discrete(name = "Methods")
  coord_cartesian(ylim=c(45,48)) # Produced a boxplot.
petrol.t.test2 <- t.test(energy~method, petrol, alternative = "greater") # Using a One tailed test.
petrol.t.test3 <- t.test(energy~method, petrol,conf.level=.95, var.equal = TRUE)$conf.int # Using a 95% confidence interval.



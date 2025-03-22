# Object
Calculation = 5 + 5
# Calling object
Calculation
# Vector
KPIExample = c(1,2,3)
# Installing a package
install.packages("gcookbook")
library(gcookbook)
# Show data
data()
# Object
baseball = tophitters2001
# Summary function
summary(baseball)
# Mean function
mean(baseball$g)
# Sorting function
sortedNumbers = sort(c(5, 11, 56, 18, 78))
# Getting the object median
median(sortedNumbers)
# Plot command
plot(baseball$rbi, baseball$r)
# Correlation command
cor(baseball$rbi, baseball$r)
# Making a chart with tags
plot(baseball$team, baseball$rbi, xlab = "Team", ylab = "RBI")
# Outputting a table
table(baseball$lg, baseball$team)
# Welch's Two Sample t-test
t.test(rbi~lg, baseball)
# Two Sample t-test
t.test(rbi~lg, var.equal = T, baseball)
# Evaluating before and after a season
sleep
# Paired t-test
t.test(sleep$extra ~ sleep$group, paired = T)
# Summary of a csv file
Basketball <- read.csv('BasketballDatasetAnalysis.csv')
summary(Basketball)
# Correlation function
BasketballNumerics = Basketball[ ,6:21]
cor(BasketballNumerics)
# Installing tidyverse
install.packages("tidyverse")
# Installing ggplot2
install.packages("ggplot2")
library(ggplot2)
# Histogram showing the distribution
ggplot(Basketball, aes(BLK)+
  geom_histogram()+
  ylab("Frequency")
)
# Examining unusual values
ggplot(Basketball, aes(x = Position, y = BLK))+
  geom_boxplot(aes(fill = Position))
# T-test
sleepdataframe = sleep
sleeptest = t.test(extra ~ group, sleepdataframe)
sleeptest
# T-test for independent samples
sleeptest = t.test(extra ~ group, sleepdataframe, var.equal = T)
sleeptest
# T-test for dependent samples
sleeptest = t.test(extra ~ group, sleepdataframe, paired = T)
sleeptest
# Independent variable is a data factor type
Basketball$Position = as.factor(Basketball$Position)
# ANOVA
STLmodel = aov(STL ~ Position, data = Basketball)
summary(STLmodel)
# Tukey HSD post hoc analysis
TukeyHSD(STLmodel)
# Percentage of steals (STL) by player position
ggplot(Basketball, aes(Position, STL))+
  geom_boxplot(aes(fill = Position))
# Installing car
install.packages("car")
library(car)
# leveneTest for variance homogeneity
leveneTest(STL ~ Position, data = Basketball)
# Extracting residuals
aov_residuals <- residuals(STLmodel)
# Running Shapiro-Wilk test
shapiro.test(x = aov_residuals)
# Nonparametric evaluation
kruskal.test(STL ~ Position, data = Basketball)
# ANCOVA - DRB by player age and position
DRBmodel = aov(DRB ~ Position + Age , data = Basketball)
summary(DRBmodel)
# Check if residuals follow normal distribution
qqnorm(DRBmodel$residuals)
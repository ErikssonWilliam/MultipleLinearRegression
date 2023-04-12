# Project code
# Model 1

# Use the package to import data
install.packages("readxl")
library("readxl")

#Choose the data file from your computer
Data1 <- read_excel(file.choose())
Data1

#Some times, you need to Convert "values" chr(characteristic)
#into (number)
Data1$x1 <- as.numeric(Data1$x1)
Data1$x2 <- as.numeric(Data1$x2)
Data1$y <- as.numeric(Data1$y)

#Define x1, x2 and y
y=Data1$y
x1=Data1$x1
x2=Data1$x2

#Plot x1 against x2 and y, and all
plot(x1,y, pch=20, col = "cyan")
plot(x2,y, pch=20, col = "cyan")
scatter3d(x1,y,x2, surface = FALSE)

DataFrame=data.frame(y, x1, x2)

#Regression analysis
Regression=lm(y~x1+x2)
summary(Regression)

#Install the package to sketch the estimated regression plane
install.packages("alr4")
library("alr4")

#Plot the estimated plane together with the observations
scatter3d(x1, y, x2, surface.col = "green" )

# Consider model 2 with more explanatory variables
x3=x1*x1
x4=x2*x2
x5=x1*x2

#Regression analysis
Regression=lm(y~x1+x2+x3+x4+x5)
summary(Regression)

#Plot the estimated plane together with the observations
scatter3d(x1, y, x2, fit = "quadratic", surface.col = "green" )

Regression1 = lm(y~x1+x2)
Regression2 = lm(y~x1+x2+x3+x4+x5)

#Use ANOVA to compare two linear regression models
anova(Regression1, Regression2)

#Find SSE1
SSE1 <- sum((fitted(Regression1) - DataFrame$y)^2)
SSE1

#Find SSE
SSE2 <- sum((fitted(Regression2) - DataFrame$y)^2)
SSE2

#Correlations y and x1, x2, x3, x4, x5
DataFrame=data.frame(y, x1, x2, x3, x4, x5)

#Install the package to run different model selection methods
install.packages("olsrr")
library(olsrr)

#Forward selection
model.forward <- lm(y ~ ., data = DataFrame)
m=ols_step_forward_p(model.forward, penter=0.05)
m

#Backward selection
model.backward <- lm(y ~ ., data = DataFrame)
n=ols_step_backward_p(model.backward)
n

#Stepwise selection
model.both <- lm(y ~ ., data = DataFrame)
q=ols_step_both_p(model.backward)
q

#All possible subsets regression
options(max.print=1000)
AllSubsetModel <- lm(y ~ ., data = DataFrame)
ols_step_all_possible(AllSubsetModel)

#Get all adjusted-R-square for 2^4=16 models
k=ols_step_all_possible(AllSubsetModel)
View(k)

#Find the maximum adjusted R-square
max(k$adjr)

#The best model for 1 variable, 2 variables, ...
ols_step_best_subset(AllSubsetModel)

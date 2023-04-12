#Lecture 12

#Residual analysis
y = c(268.2, 303.3, 319.2, 270.7, 277.9, 282.5, 297.9, 300.1, 269.0, 282.4, 293.2, 296.5, 300.0, 275.7, 288.4, 293.8, 315.4, 332.1, 280.0, 
      288.9, 291.3, 291.5, 310.0, 311.2, 314.3, 332.5, 309.7, 318.0, 318.7, 325.3, 330.7, 278.8, 294.0, 305.2, 310.1, 312.5, 320.6, 330.4,
      332.2, 332.7, 340.1, 324.0, 338.8, 311.5, 325.1, 293.3, 325.5, 337.7, 340.9, 347.0)
x = c(1, 5, 5, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 12, 12, 12, 13, 13, 13, 13, 13, 14,
      14, 14, 14, 15, 16, 16, 17, 17, 18, 18, 18, 18, 18)
Regression=lm(y~x)

#Residual plot against x
Residual <- resid(Regression)
plot(y,Residual)

#Add horizontal line 0 to residual plot
abline(0,0, col="red")

#Histogram 
hist(Residual) 

#Residual vs. fitted
plot(Regression, 1)

#Scale-Location
plot(Regression, 3)

#Q-Q plot
plot(Regression, 2)

#Example 1
y = c(4, 24, 23, 12, 20, 45, 60, 18, 5, 15, 18, 88, 50, 24, 
      12, 8, 14, 16, 32, 25)
x1 = c(0, 2, 1, 1, 2, 4, 5, 1, 0, 1, 1, 8, 5, 1, 1, 0, 0, 1, 2, 0)
x2 = c(1, 3, 0, 0, 1, 0, 8, 1, 1, 0, 3, 3, 0, 3, 1, 3, 6, 4, 0, 8)

Regression1 = lm(y~x1)
Regression2 = lm(y~x1+x2)

#Use ANOVA to compare two linear regression models
anova(Regression1, Regression2)

#Find SSE for model with less variables
DataFrame=data.frame(y, x1, x2)
SSE1 <- sum((fitted(Regression1) - DataFrame$y)^2)
SSE1

#Find SSE for model with extra variables
SSE2 <- sum((fitted(Regression2) - DataFrame$y)^2)
SSE2


#Example 2
y=c(15, 20, 13, 21, 12, 12, 12, 11, 12, 20, 11, 17, 20, 23, 17, 
    16, 10, 11, 11, 9, 5, 5, 4, 7, 18, 17, 23, 23, 24, 25)
w=c(50, 47, 57, 38, 52, 57, 53, 62, 52, 42, 47, 40, 42, 40, 48,
    50, 55, 52, 48, 52, 52, 48, 65, 53, 36, 45, 43, 42, 35, 43)
t=c(77, 80, 75, 72, 71, 74, 78, 82, 82, 82, 82, 80, 81, 85, 82, 
    79, 72, 72, 76, 77, 73, 68, 67, 71, 75, 81, 84, 83, 87, 92)
h=c(67, 66, 77, 73, 75, 75, 64, 59, 60, 62, 59, 66, 68, 62, 70, 
    66, 63, 61, 60, 59, 58, 63, 65, 53, 54, 44, 46, 43, 44, 35)
i=c(78, 77, 73, 69, 78, 80, 75, 78, 75, 58, 76, 76, 71, 74, 73, 
    72, 69, 57, 74, 72, 67, 30, 23, 72, 78, 81, 78, 78, 77, 79)

DataFrame=data.frame(y, w, t, h, i)

#Install the package to run different model selection methods
install.packages("olsrr")
library(olsrr)

#The below one stpe code will not be given next year

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

# we can search max(k$adjr) to find the model


#The best model for 1 variable, 2 variables, ...
ols_step_best_subset(AllSubsetModel)


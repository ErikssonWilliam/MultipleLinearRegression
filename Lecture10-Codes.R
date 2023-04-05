#Lecture 10, 
#Simple linear regression
x=c(3.4, 1.8, 4.6, 2.3, 3.1, 5.5, 0.7, 3.0, 2.6, 4.3, 2.1, 1.1, 6.1, 4.8, 3.8)
y=c(26.2, 17.8, 31.3, 23.1, 27.5, 36.0, 14.1, 22.3, 19.6, 31.3, 24.0, 17.3, 43.2, 36.4, 26.1)

#Calculate Covariance
covariance=cov(x,y)
covariance

#Plot y against x 
plot(x,y, pch=20, col = "cyan")


#The line which can fit the observations
abline(lm(y~x), col="blue")


#Multiple linear regression y against x4, x7

y = c(268.2, 303.3, 319.2, 270.7, 277.9, 282.5, 297.9, 300.1, 269.0, 282.4, 293.2, 296.5, 300.0, 275.7, 288.4, 293.8, 315.4, 332.1, 280.0, 
      288.9, 291.3, 291.5, 310.0, 311.2, 314.3, 332.5, 309.7, 318.0, 318.7, 325.3, 330.7, 278.8, 294.0, 305.2, 310.1, 312.5, 320.6, 330.4,
      332.2, 332.7, 340.1, 324.0, 338.8, 311.5, 325.1, 293.3, 325.5, 337.7, 340.9, 347.0)

x4 = c(1, 5, 5, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 12, 12, 12, 13, 13, 13, 13, 13, 14,
       14, 14, 14, 15, 16, 16, 17, 17, 18, 18, 18, 18, 18)

x7 = c(16, 23, 32, 17, 15, 15, 23, 24, 9, 17, 22, 24, 20, 17, 19, 18, 31, 39, 15, 17, 18, 16, 29, 28, 31, 38, 29, 33, 35, 37, 40, 14, 22,
       27, 30, 28, 35, 38, 38, 41, 43, 36, 40, 29, 38, 22, 37, 42, 45, 50)

#Correlations y and x4, x7
DataFrame=data.frame(y, x4, x7)
cor(DataFrame[, c('x4', 'x7')], y)

#Regression analysis
Regression=lm(y~x4+x7)
summary(Regression)


#Install the package to sketch the estimated regression plane
install.packages("alr4")
library("alr4")

#Plot the estimated plane together with the observations
y =248.69242+0.11692*x4+2.0603*x7
scatter3d(x4, y, x7, fit = "quadratic", surface.col = "green" )

#Find SSE
SSE <- sum((fitted(Regression) - DataFrame$y)^2)
SSE

#Find SSR
SSR <- sum((fitted(Regression) - mean(DataFrame$y))^2)
SSR

#Find SST
SST <- SSR + SSE
SST

#Get 95% confidence interval for beta_j
confint(Regression)





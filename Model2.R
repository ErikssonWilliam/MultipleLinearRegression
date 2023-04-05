# Model 2

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

# Consider model 2 with more explanatory variables
x3=x1*x1
x4=x2*x2
x5=x1*x2

#Install the package to sketch the estimated regression plane
install.packages("alr4")
library("alr4")

#Plot the estimated plane together with the observations
y =79.8225+1.0675*x1+0.4161*x2-1.1999*x3-0.4544*x4+0.7492*x5
scatter3d(x1, y, x2, fit = "quadratic", surface.col = "green" )

#Find SSE
SSE <- sum((fitted(Regression) - DataFrame$y)^2)
SSE

#Find SSR
SSR <- sum((fitted(Regression) - mean(DataFrame$y))^2)
SSR

#Find SST
SST <- SSR + SSE
SST


#Correlations y and x1, x2, x3, x4, x5
DataFrame=data.frame(y, x1, x2, x3, x4, x5)
cor(DataFrame[, c('x1', 'x2', 'x3', 'x4', 'x5')], y)

#Regression analysis 2
Regression=lm(y~x1+x2+x3+x4+x5)
summary(Regression)

#Get 95% confidence interval for beta_j
confint(Regression)
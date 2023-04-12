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



#Regression analysis 2
Regression=lm(y~x1+x2+x3+x4+x5)
summary(Regression)

#Get 95% confidence interval for beta_j
confint(Regression)



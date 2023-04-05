#Lecture 9

# Use the package to import data
install.packages("readxl")
library("readxl")

#Choose the data file from your computer
Data1 <- read_excel(file.choose())
Data1

#Define x1, y1
y1=Data1$y1
x1=Data1$x1

#Some times, you need to Convert "values" chr(characteristic)
#into (number)
Data$x1 <- as.numeric(Data$x1)
Data$y1 <- as.numeric(Data$y1)

#Plot y1 against x1 
plot(x1,y1, pch=20, col = "cyan")

#Calculate correlation
correlation1=cor(x1,y1)
correlation1


#Sketch Bivariate random vector
install.packages("mnormt")
library(mnormt)

x     <- seq(-5, 5, 0.25) 
y     <- seq(-5, 5, 0.25)
mu    <- c(0, 0)
sigma <- matrix(c(2, -1, -1, 2), nrow = 2)
sigma
f     <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)

persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "green", border="pink", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")







# Reinstalling R packages after installation (Linux/MAC) ----------------------------
packages <- as.data.frame(installed.packages())
install.packages(packages$Package)

#For Windows users, please look at the package "installr"

############## Updating Packages ###############

update.packages(ask = FALSE)

############## Basic R #####################
setwd("~/Documents/bios611/lab1/")
getwd()
library(tidyverse)

1+1
x <- 1
print(x)
x
v1 <- c(1,2,3)
v1[1]
length(v1)

help(read.csv)
help("read.csv")
?lapply

############## Basic Types ###################
# Numbers
a <- 5
b <- sqrt(3)
v1 <- c(1,2,3)
mean(v1)
# Strings
s1 <- "hello world"
s <- c("hello", "world")
#Factors
blp <- factor("high", levels = c("low", "high"))
#Logical
t <- TRUE
f <- FALSE
1 != 2
1 == 1
1 <= 2
#Matrix
X = matrix(c(1,3,4,5,2,7),
           nrow=2,
           ncol=3,
           byrow = TRUE)
X
X[1,2]
#Data.frame
c1 <- c(1,2,3)
c2 <- c(2,3,4)
c3 <- factor(c("a","b","c"))
df <- data.frame(col1 = c1,
                 col2 = c2,
                 col3 = c3)
df
df$col3
#Missing values
NA #missing value, "Not Available"
is.na(c(3,NA))
#NaN
0 / 0 
typeof(NaN)
NULL #object of length zero, mean "nothing"
is.null(NULL)
#checking the type
typeof(a)
typeof(t)
typeof(s1)

############## Environment #############
ls()
rm(s1)

############### Functions ###################
y <- 2
SumTwoNumbers <- function(a,b){
  return(a+b)
}

# R functions pass arguement by value
Double <- function(x){
  x <- 2*x
  return(x)
}
z <- 3
Double(z)
print(z) #is z = 3 or z = 6

############# Useful Functions #############
rep(1,3)
seq(from = 1,to = 10,by = 2)
1:5
1 %in% c(1,2,3)

############ Matrix operation vs Elementwise Operation ##########
c(1,2,3) * 2
X = matrix(c(1,3,4,5,2,7),
           nrow=2,
           ncol=3,
           byrow = TRUE)
Y = matrix(c(1,1,4,3,2,8),
           nrow=3,
           ncol=2,
           byrow = TRUE)
Z <- X %*% Y

############# Saving and Loading ################
save(X, file = "./matrix_X.RData")
load(file = "matrix_X.RData")

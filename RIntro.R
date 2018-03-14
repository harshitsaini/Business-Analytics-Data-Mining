library(xlsx)

df= read.xlsx(file.choose(),1,header = T)

df1= read.xlsx("C:/Users/Harshit/Desktop/Business Analytics in R/SedanCar.xlsx",
               1, header = T)

setwd("C:/Users/Harshit/Desktop/Business Analytics in R/")

df2= read.xlsx("SedanCar.xlsx",1, header = T)

library(matrixcalc)
#matrix.inverse(Mat2)

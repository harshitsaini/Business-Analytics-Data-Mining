df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]

#Normalization:
dfb= df
df[,1:2]=scale(df[,1:2],center = T, scale = T)

partidx= sample(1:nrow(df),15, replace = F)
dftrain=df[partidx,]
dftest=df[-partidx,]

#Modeling 
library(class)
# Building '4NN'
mod= knn(train= dftrain[,1:2],test= dftest[,1:2],
         cl=dftrain$Ownership, k=4)
summary(mod)
  
#Classification Matrix 
table("Actual value"=mod, "Predicted value"=dftest$Ownership)

mean(mod!=dftest$Ownership)

#choosing K
modtrain = NULL
modtest = NULL
errtrain = NULL
errtest = NULL

dftrain= as.data.frame(dftrain)
dftest= as.data.frame(dftest)

for(i in 1:15) {
  modtrain= knn(train = dftrain[,1:2],test=dftrain[,1:2],
                cl= dftrain[,3], k=i)
  modtest= knn(train = dftrain[,1:2],test=dftest[,1:2],
                cl= dftrain[,3], k=i)
  errtrain[i]= 100*mean(modtrain!=dftrain$Ownership)
  errtest[i]= 100*mean(modtest!=dftest$Ownership)
  }

dfp = data.frame("valueofk"=1:15,"ErrorTraining"=errtrain, 
                 "ErrorValidation"=errtest)
round(dfp,digits = 2)
range(dfp$ErrorValidation)
plot(dfp$valueofk, dfp$ErrorValidation, las=1, type="l",
     xlab="value of k", ylab= "Validation Error",
     xlim= c(0,16), ylim=c(0,65))
lines(dfp$valueofk,dfp$ErrorTraining)

#BEST K
min(errtest)
bestk= dfp[which(errtest==min(errtest)),1]
#or
bestk= dfp[which.min(errtest),1]

#Predicting class of new observation
#Annual_Income=6 lpa, Household_area= 20
modnew1= knn(train = dftrain[,1:2], test = c(6,20),
             cl= dftrain$Ownership, k=bestk)
modnew2= knn(train = dftrain[,1:2], test = c(5,15),
             cl= dftrain$Ownership, k=bestk)

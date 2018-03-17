library(xlsx)

# FlightDetails.xlsx
df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]
df= df[!apply(is.na(df),1,all),]
head(df)
str(df)

dfb= df
df= dfb
# Correct arrival and departure times
df$STD= format(df$STD, "%H:%M:%S")
df$STD= as.POSIXlt(paste(df$Date,df$STD),format = "%Y-%m-%d %H:%M:%S")
df$ATD= format(df$ATD, "%H:%M:%S")
df$ATD= as.POSIXlt(paste(df$Date,df$ATD),format = "%Y-%m-%d %H:%M:%S")
df$STA= format(df$STA, "%H:%M:%S")
df$STA= as.POSIXlt(paste(df$Date,df$STA),format = "%Y-%m-%d %H:%M:%S")
df$ATA= format(df$ATA, "%H:%M:%S")
df$ATA= as.POSIXlt(paste(df$Date,df$ATA),format = "%Y-%m-%d %H:%M:%S")

head(df)
str(df)

dfb2= df


df=dfb
df$STD=strptime(format(df$STD, "%H:%M:%S"),"%H:%M:%S")
df$ATD=strptime(format(df$ATD, "%H:%M:%S"),"%H:%M:%S")
df$STA=strptime(format(df$STA, "%H:%M:%S"),"%H:%M:%S")
df$ATA=strptime(format(df$ATA, "%H:%M:%S"),"%H:%M:%S")

head(df)
str(df)


#Break departure time into approprinttermal
range(df$ATD)
breaks = seq(strptime("00:00:00", "%H:%M:%S"),strptime("24:00:00","%H:%M:%S"),
             by = "6 hours")
labelsv= c("0-6","6-12","12-18","18-24")
DEPT= cut(df$ATD, breaks= breaks, right= F, labels = labelsv)

df= cbind(df, DEPT)

df$Day= as.factor(df$Day)
levels(df$Day)
levels(df$Day)= c("Sunday","Monday")

head(df)
str(df)

dfb3= df
df= df[,-c(1,3,5,8)]
str(df)
head(df)

partidx= sample(1:nrow(df),0.6*nrow(df), replace = F)
dftrain=df[partidx,]
dftest=df[-partidx,]

library(e1071)
mod= naiveBayes(df$Flight.Status ~ .,dftrain)
attributes(mod)

mod$apariori
mod$tables
path=""
write.xlsx(dftrain, path+"FlightDetails.xlsx")

mod$tables$Flight.Carrier
mod$tables$Flight.Carrier[1,3]
mod$tables$Flight.Carrier["ontime","Indigo"]


# Find exact matches for complete for Exact Bayes

dftrain[which(dftrain$Flight.Carrier=="Indigo"&
                dftrain$SRC=="MAA"&
                dftrain$DEST=="IXC"&
                dftrain$Day=="Monday"&
              dftrain$DEPT=="0-6"),]

#NAIVE BAYES formulae (NUMERATOR):

p1= (mod$apriori[["delayed"]]/nrow(dftrain))
      *(mod$tables$Flight.Carrier["delayed","Indigo"])
      *(mod$tables$SRC["delayed","MAA"])
        *(mod$tables$DEST["delayed","IXC"])
          *(mod$tables$DEPT["delayed","0-6"])*
    (mod$table$Day["delayed","Monday"])
print(p1,digits=4)

# P(ontime|Example)
p2= (mod$apriori[["ontime"]]/nrow(dftrain))*
  (mod$tables$Flight.Carrier["ontime","Indigo"])
*(mod$tables$SRC["ontime","MAA"])
*(mod$tables$DEST["ontime","IXC"])
*(mod$tables$DEPT["ontime","0-6"])*
  (mod$table$Day["ontime","Monday"])
print(p1,digits=5)

# Actual Probablities
# P(delyed|Example)
p1/(p1+p2)
# P(ontime|Example)
p2/(p1+p2)


#SCoring test partition
modtest= predict(mod, dftest[,5], type="class")
modtestp= predict(mod, dftest[,5], type="raw")

table("Actual class"=dftest$Flight.Status, "Predicted class"=modtest)
head(data.frame("Predicted class"=modtest,
                "Actual class"=dftest$Flight.Status,
                "Prob for 1(success)"=modtestp[,"delayed"],
                dftest[-5]))

#Classification accuracy 
mean(modtest==dftest$Flight.Status)
#MisClassification accuracy 
mean(modtest!=dftest$Flight.Status)

#Scoring Training Partition
modtrain= predict(mod, dftrain[,-5])
table("Actual class"=dftrain$Flight.Status, "Predicted class"=modtrain)

#Classification accuracy 
mean(modtrain==dftest$Flight.Status)
#MisClassification accuracy 
mean(modtrain!=dftest$Flight.Status)

#Cumlulative Lift Curve
cases = 1:nrow(dftest)
modtestn= dftest$Flight.Status
levels(modtestn)= c(1,0) #c("delayed","ontime")
modtestn= as.numeric(as.character(modtestn))
dfl= data.frame("prob"=modtestp[,"delayed"],"actual class" =modtestn)
dfl= dfl[order(-dfl$prob),]

cumAC= NULL
cumAC[1]= dfl$actual.class[1]
for(i in 2:nrow(dfl)) {
  cumAC[i]= cumAC[i-1]+ dfl$Actual.class[i]
}

plot(cases, cumAC, type="l",
     xlab="# cases", ylab= "Cumulative", xlim=c(0,50), ylim = c(0,20))
segments(0,0,nrow(dfl),cumAC[nrow(dfl)],lty= 3)
legend(25,5,inset= 0.005,
       c("Cumulative 1's sorted by predicted values",
         "Cumulative 1's using average"),
       lty= c(1,2),cex= 0.7, x.intersp = 0,3, y.intersp = 0.3)


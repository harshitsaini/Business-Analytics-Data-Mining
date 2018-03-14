library(xlsx)

df= read.xlsx(file.choose(),1, header= T)
df= df[,!apply(is.na(df), 2,all)]

plot(df$Annual_Income,df$Household_Area, las=1 , xlab= "Annual Income", ylab= "Household Area",
     xlim= c(2,12), ylim = c(13,25), pch= c(21,19)[as.numeric(df$Ownership)])

legend("bottomright", inset= 0.005, c("owner", "Non-Owner"), pch= c(19,21), cex= 0.7,
       x.intersp = 0.5, y.intersp = 0.5)

#Promoffers.xlsx
df1= read.xlsx(file.choose(),1, header= T)
df1= df1[,!apply(is.na(df1), 2,all)]

palette()
palette(c("gray","black"))

plot(df1$Income, df1$Spending, xlim=c(0,225), ylim=c(0,11),
     xlab="Income", ylab="Spending", col= as.factor(df1$Promoffer), 
     pch=19, cex=0.8, panel.first = grid())

plot(jitter(df1$Income,1), df1$Spending, xlim=c(0,225), ylim=c(0,11),
     xlab="Income", ylab="Spending", col= as.factor(df1$Promoffer), 
     pch=20, cex=0.8, panel.first = grid())
par(mar=c(4,4,1,1), oma=c(1,1,1,1))

plot(jitter(df1$Income,1), df1$Spending, log= "xy",
     xlab="Income", ylab="Spending", col= as.factor(df1$Promoffer), 
     pch=20, cex=0.7, panel.first = grid())
palette("default")

#Classification Marix 
cm= matrix(c(400,50,25,2525), 2,2,T, list(c("1","0"),c("1","0")))

err= (cm['0','1']+cm['1','0'])/sum(cm)
accuracy= (cm['1','1']+cm['0','0'])/sum(cm)

#ROC Curve
df2= read.xlsx(file.choose(),1, header= T)
df2= df2[,!apply(is.na(df2), 2,all)]
df2= df2[!apply(is.na(df2),1,all),]

data.frame("cutoffvalue"=df2$cutoff,"OneMinusSpecificity"=1-df2$specificity,
           "Senstivity"=df2$senstivity)[order(df2$cutoff, decreasing = T)]
plot(1-df2$specificity, df2$senstivity, type= "p", 
     xlab = "1-Specifity", ylab= "Senstivity", pch=19)

plot(1-df2$specificity, df2$senstivity, type= "s", 
     xlab = "1-Specifity", ylab= "Senstivity", pch=19)
segments(0,0,1,1,lty= 3)
legend("right", inset= 0.005, c("Random", "ROC"), lty= c(2,1),bty="n", cex= 0.7,
       x.intersp = 0.3, y.intersp = 0.3)

#Cumulative Lifts Curve / Gains Chart
df3= read.xlsx(file.choose(),1, header= T)
df3= df3[,!apply(is.na(df3), 2,all)]
df3= df3[!apply(is.na(df3),1,all),]

range(df3$Cumulative.Actual.Class)
range(df3$Serial.no.)
plot(df3$Serial.no., df3$Cumulative.Actual.Class, type = "l",
     xlab= "# Cases", ylab= "Cumulative", xlim= c(0,30), ylim= c(0,14))
segments(0,0,24,12,lty= 3)
segments(1,1,12,12,lty= 4, col= "red")
segments(12,12,24,12,lty= 4, col= "red")
legend(22,10, inset= 0.005, c("Cumlative 1's sorted by predicted values",
                              "Cumlative 1's using random selection"),
        lty= c(1,2),bty="n", cex= 0.7,x.intersp = 0.3, y.intersp = 0.3)

#Decile Chart
decilecases= round(seq(0.1,1,0,1)*length(df3$serial.no.))
decile= NULL
decilemean= NULL
globalmean= length(which(df3$Actual.Class==1))/length(df3$Actual.Class)
j=0
for(i in decilecases) {
  j=j+1
  decilemean[j]= df3Cumulative.Actual.Class[i]/i
  decile[j]= decilemean[j]/globalmean
}
barplot(decile, names.arg = as.factor(seq(1,10,1)),xlab="Deciles",
        ylab= "Decile mean/Global Mean", ylim = c(0,2.5))

#Cumulative lift curve(gains chart) incorporating costs
#cutoffdata.xlsx
df4= read.xlsx(file.choose(), 5, colIndex = 1:5, T)
df4= df4[,!apply(is.na(df4), 2,all)]
head(df4)

range(df4$Cumulative.Cost.)
range(df4$Serial.no.)
plot(df4$Serial.no., df4$Cumulative.Cost.,type= "l", xlab= xlab = "# cases",
     ylab="Cumulative costs", xlim= c(0,25), ylim=c(5,140))
segments(0,0,24,132, lty = 3)
legend(22,10, inset= 0.005, c("Cumlative costs sorted by predicted values",
                              "reference line"),
       lty= c(1,2),bty="n", cex= 0.7,x.intersp = 0.3, y.intersp = 0.3)

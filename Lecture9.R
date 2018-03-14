library(xlsx)

df1= read.xlsx(file.choose(),1, header= T)
df1= df1[,!apply(is.na(df1), 2,all)]

Age= 2017- df1$Mfg_Year
df1= cbind(df1,Age)
dfb= df1
df1= df1[,-c(1,2,3)]

head(df1)
str(df1)
df1$Transmission= as.factor(df1$Transmission)
df1$C_Price= as.factor(df1$C_Price)
str(df1)
summary(df1)

df1= df1[df1$Price<70,]
dfb= df1
df1= df1[-23,]

head(df1)
range(df1$KM)
boxplot(df1$Price~df1$Transmission, ylim= c(0,15),xlab="Transmission",
        ylab="Price")
means= by(df1$Price, df1$Transmission,mean)
points(1:2,means,pch=3)

boxplot(df1$KM~df1$C_Price, ylim= c(25,180),xlab="C_Price",
        ylab="KM")
means1= by(df1$KM, df1$C_Price,mean)
points(1:2,means1,pch=3)

boxplot(df1$Age~df1$C_Price, ylim= c(0,12),xlab="C_Price",
        ylab="Age")
means2= by(df1$Age, df1$C_Price,mean)
points(1:2,means2,pch=3)

boxplot(df1$SR_Price~df1$C_Price, ylim= c(0,25),xlab="C_Price",
        ylab="SR_Price")
means3= by(df1$SR_Price, df1$C_Price,mean)
points(1:2,means3,pch=3)

#Heatmaps
#correlation matrix
M= cor(df1[,-c(1,5,8)])
symnum(M)
M[upper.tri(M)]=NA;

#correlation table heatmap
heatmap(M,Rowv = NA, symm= T, col= grey.colors(100, start= 0.8, end=0.2),scale= "none", margins= c(8,4))

#Missing value heatmap
heatmap(head(as.matrix(df1[,-c(1,5,8)])),Rowv = NA,Colv = NA,
        col= grey.colors(1000, start= 0.8, end=0.0),scale= "column", margins= c(8,4))

heatmap(as.matrix(df1[,-c(1,5,8)]),Rowv = NA,Colv = NA, 
        col= grey.colors(1000, start= 0.8, end=0.0),scale= "column", margins= c(8,4))

#Multidimensional Visualization
palette()
palette(rainbow(6))
palette("default")

range(df1$Age)
plot(df1$Age, df1$KM, xlim=c(0,12),xlab= "Age", ylab= "KM", col= df1$C_Price)

#separate panel for each group
Age_groups= levels(as.factor(df1$Age))
Age_groups2= as.numeric(Age_groups)
avgPrice1=NULL
avgPrice2=NULL
for(x in Age_groups2){
  avgPrice1= c(avgPrice1,mean(df1[which(df1$Age == x & df1$Transmission == 0),]$Price))
  avgPrice2= c(avgPrice2,mean(df1[which(df1$Age == x & df1$Transmission == 1),]$Price))
}
avgPrice1[which(avgPrice1=="Nan")]=0
avgPrice2[which(avgPrice2=="Nan")]=0
avgPrice2[is.nan(avgPrice2)]<-0

par(mfrow=c(2,1),cex= 0.6, mar= c(3,3,0,0),oma= c(1,1,1,1))  

range(avgPrice1)
range(avgPrice2)

#code incomplete
barplot(avgPrice1,names.arg = Age_groups, xlab="",ylab="",xaxt="n",ylim=c(0,9))
box("plot")
legend("topright",inset=0.005, c("Trans=0"),bty = "n", cex = 1)
mtext("Avg(Price)",side=2, line= 2.2, cex= 0.7, adj=0)


barplot(avgPrice2, names.arg = Age_groups, xlab = "",ylab="", ylim= c(0,9))
box("plot")
legend("topright",inset=0.005, c("Trans=1"),bty = "n", cex = 1)
mtext("Age",side=1, line= 2.2, cex= 0.7)

# MULTIPANEL PLOT VISUALIZATION
pairs(~ SR_Price + KM + Price + Age, data= df1)

par(mfrow=c(2,2),cex= 0.6, mar= c(3,3,0,0),oma= c(1,1,1,1))  

plot(df1$KM,df1$Price, xlim=c(0,180), ylim= c(0,15),xlab="",ylab="")
mtext("KM",side=1, line= 2.2, cex= 0.7)
mtext("Price",side=2, line= 2.2, cex= 0.7)

plot(df1$KM,df1$Price,log="xy" ,xlim=c(10,1000), ylim= c(0.1,100),
     xaxt="n",yaxt="n",xlab="",ylab="")
axis(1,at=c(10,100,1000),labels=c("10","100","1000"))
axis(2,at=c(0.1,1,10,100),labels=c("0.1","1","10","100"))
mtext("KM",side=1, line=2, cex=0.6)
mtext("Price",side=2, line=2, cex=0.6)

range(dfb$Price)
boxplot(dfb$Price~dfb$Transmission, ylim=c(0,75),xlab="",ylab="")
mtext("Trans",side=1, line=2, cex=0.6)
mtext("Price",side=2, line=2, cex=0.6)

boxplot(dfb$Price~dfb$Transmission, log="y",ylim=c(0.1,100),xlab="",ylab="")
mtext("Trans",side=1, line=2, cex=0.6)
mtext("Price",side=2, line=2, cex=0.6)

#AGGREGATIONS , ATTACHING A CURVE , ZOOMING IN 
par(mfrow=c(2,2),cex= 0.6, mar= c(2.7,2.5,1,0.5),oma= c(0,0,0,0))  
df= read.xlsx(file.choose(), 1 ,header = T)
df= df[,!apply(is.na(df), 2,all)]

at1= seq(as.Date("2004-01-01"), as.Date("2017-03-01"),by="2 years")
labels1=format(at1,"%b-%Y")
at2=format(at1,"%Y")
plot(tsv,xaxt="n",yaxt="n")
axis(1,at=at2, labels=format(at1, "%d/%m/%Y"),cex.axis=0.8)
axis(2,cex.axis=0.8)
mtext(side=1,text="Month" ,line=2, cex=0.6)
mtext(side=2,text="Riders", line=2, cex=0.6)
title(main="Overlaying a quadratic curve on Raw Series",adj=0, cex.main=0.9)

lines(lowess(tsv), col="red")

t=seq(1,length(df$Month), by=1)
tsq= t*t
points(time(tsv), predict(lm(df$Riders~t+tsq)), col="green")
abline(v=at2, h=axTicks(2), col="gray", lty=3)

rideBym=NULL
for(it in 1:12){
  rideBym[it]=0
}

i=1
while(i<=145){
  rideBym[1]=rideBym[1]+df$Riders[i]
  rideBym[2]=rideBym[2]+df$Riders[i+1]
  rideBym[3]=rideBym[3]+df$Riders[i+2]
  rideBym[4]=rideBym[4]+df$Riders[i+3]
  rideBym[5]=rideBym[5]+df$Riders[i+4]
  rideBym[6]=rideBym[6]+df$Riders[i+5]
  rideBym[7]=rideBym[7]+df$Riders[i+6]
  rideBym[8]=rideBym[8]+df$Riders[i+7]
  rideBym[9]=rideBym[9]+df$Riders[i+8]
  rideBym[10]=rideBym[10]+df$Riders[i+9]
  rideBym[11]=rideBym[11]+df$Riders[i+10]
  rideBym[12]=rideBym[12]+df$Riders[i+11]
  i=i+12
}
rideBym[1]=rideBym[1]+df$Riders[i]
rideBym[2]=rideBym[2]+df$Riders[i+1]
rideBym[3]=rideBym[3]+df$Riders[i+2]

avgBym=  c(rideBym[1]/14, rideBym[2]/14,rideBym[3]/14,rideBym[4]/13,rideBym[5]/13,
  rideBym[6]/13, rideBym[7]/13,rideBym[8]/13,rideBym[9]/13,rideBym[10]/13,
  rideBym[11]/13, rideBym[12]/13)

tsv1= ts(avgBym, start=1 , end=12, frequency = 1)
plot(tsv1, xaxt="n", yaxt="n")
at3= seq(as.Date("1jan","%d%b"),as.Date("1dec","%d%b"),by="1 month")
at4= seq(as.Date("1","%d"),as.Date("12","%d"),by="1 day")
axis(1,at= format(at4,"%d"), labels = format(at3,"%b"),las=3,cex.axis=0.8)
axis(2,cex.axis=0.8)
mtext(side=1,text="Month" ,line=2, cex=0.6)
mtext(side=2,text="AvgRiders", line=2, cex=0.6)
title(main="Aggregation by Month",adj=0, cex.main=0.9)

abline(v= format(at4,"%d"), h=axTicks(2), col="gray", lty=3)
 
tsvz= window(tsv, start=c(2004,1), end=c(2005,12))
plot(tsvz, xaxt="n", yaxt="n")
at5=NULL
i=1
while(i<=24){
  at5=c(at5,time(tsvz)[i])
  i=i+4
}
at6= seq(as.Date("2004-01-01"),as.Date("2005-12-01"),by="4 months")
axis(1,at= at5, labels = format(at6,"%d/%m/%Y"),cex.axis=0.8)
axis(2,cex.axis=0.8)
mtext(side=1,text="Month" ,line=2, cex=0.6)
mtext(side=2,text="AvgRiders", line=2, cex=0.6)
title(main="Zooming into first 2 years",adj=0, cex.main=0.9)

abline(v=at5, h=axTicks(2), col="gray", lty=3)

plot(aggregate(tsv, FUN=mean),xaxt="n",yaxy="n",cex.axis=0.6)
axis(1,cex.axis=0.8)
axis(2,cex.axis=0.8)
mtext(side=1,text="Year" ,line=2, cex=0.6)
mtext(side=2,text="AvgRiders", line=2, cex=0.6)
title(main="Aggregation for year",adj=0, cex.main=0.9)
grid()

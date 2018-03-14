library(xlsx)

#NaughtyBicycle.xlsx
df= read.xlsx(file.choose(), 1 ,header = T)
df= df[,!apply(is.na(df), 2,all)]
df= df[,1:2]
head(df)

#Line Graph
tsv= ts(df$Riders, start= c(2004,1),end=c(2017,3),frequency = 12)
plot(tsv, xlab= "year", ylab="Riders",las=1) # las= styling for axis labels

at1= seq(as.Date("2004-01-01"), as.Date("2017-03-01"),by="2 years")
labels1=format(at1,"%b-%Y")
at2=format(at1,"%Y")

par()$mar
par(mar=c(8,4,4,2)+0.1)

plot(tsv,xlab="",ylab="",xaxt="n",yaxt="n")
axis(1,at=at2,labels = labels1, las=2)
axis(2,las=2)
mtext(side=1,text="Month-Year", line= 5.0)
mtext(side=2,text="Riders", line= 3.3)

graphics.off()
par()$mar

#Bar charts
df1= read.xlsx(file.choose(),1, header= T)
df1= df1[,!apply(is.na(df1), 2,all)]

Age= 2017- df1$Mfg_Year
df1= cbind(df1,Age)
df1= df1[,-c(1,2,3)]

head(df1)
str(df1)
df1$Transmission= as.factor(df1$Transmission)
df1$C_Price= as.factor(df1$C_Price)
str(df1)
summary(df1)

#Scatter plots
range(df1$KM)
range(df1$Price)
plot(df1$KM,df1$Price,xlim= c(18,180),ylim= c(1,75),xlab= "KM",ylab = "Price" )

df1= df1[df1$Price<70,]
dfb= df1
df1= df1[-23,]

range(df1$KM)
range(df1$Price)
plot(df1$KM,df1$Price,xlim= c(18,180),ylim= c(1,15),xlab= "KM",ylab = "Price" )


#Bar Chart 
avgPrice= c(mean(df1[which(df1$Transmission=='0'),]$Price),
            mean(df1[which(df1$Transmission=='1'),]$Price))
Trans= c("0","1")

range(avgPrice)

barplot(avgPrice, names.arg = Trans, xlab= "Transmission",
        ylab="Average-Price", ylim= c(0,6))

pAll= c((length(which(df1$Transmission=='0'))/length(df1$Transmission))*100,
        (length(which(df1$Transmission=='1'))/length(df1$Transmission))*100)
    
barplot(pAll, names.arg = Trans, xlab= "Transmission",
        ylab="% of all records", ylim= c(0,100))

#Histograms

range(df1$KM)
range(df1$Price)
hist(df1$Price, main="", xlim=c(-5,20),ylim= c(0,50),xlab="Price")

#boxplot

boxplot(df1$Price~df1$Transmission, ylim= c(0,15),xlab="Transmission",
        ylab="Price")
means= by(df1$Price, df1$Transmission,mean)
points(1:2,means,pch=19)

range(df1$KM)


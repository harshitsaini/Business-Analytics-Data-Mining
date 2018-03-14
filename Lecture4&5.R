library(xlsx)

df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]
head(df)
summary(df)

cov(df$Annual_Income, df$Household_Area)

cor(df$Annual_Income, df$Household_Area)

mean(df$Annual_Income)

median(df$Annual_Income)

IQR(df$Annual_Income)

sd(df$Annual_Income)

var(df$Annual_Income)

apply(df[,c(1,2)],MARGIN = 2,FUN = sd)

mmdiff= function(df){
  apply(df,MARGIN = 2,function(x){max(x)-min(x)})
}
mmdiff(df[,c(1,2)])

x= rnorm(100)
y=x+ rnorm(100, mean=0, sd=0.6)

df1= as.data.frame(cbind(x,y))
head(df1)
summary(df1)
  
plot(df1$x, df1$y, las=1, main= "Scatterplot of x and y",
     xlab= "x", ylab="y",
     xlim=c(-3,3),ylim=c(-4,4))

x1= rnorm(20,mean=50,5)
y1= rnorm(30,mean=60,5)

t.test(x1,y1,var.equal = T)

qt(p=0.05/2,df= 48, lower.tail = F)

#welch's t-test
t.test(x1,y1,var.equal = F)

Ads= sample(c("AD1","AD2","NoAD"),size=100,replace=T)
purchase= ifelse(Ads=='AD1', rnorm(100,mean=500,sd=80),
          ifelse(Ads=='AD2', rnorm(100,mean=600,sd=80),
                 rnorm(100,mean=200,sd=80)))
df2= data.frame(Ads= as.factor(Ads),purchase) 
head(df2)
summary(df2$Ads)
summary(df2[df2$Ads=='AD1',2])
summary(df2[df2$Ads=='AD2',2])
summary(df2[df2$Ads=='NoAD',2])

mod= aov(purchase~Ads, data= df2)
summary(mod)

dfh= data.frame("Promotions"=c(2.00,3.50,6.00,6.50,7.50,8.00,9.00),
                "sales"=c(5.00,8.00,5.50,14.00,13.50,14.50,13.50)); dfh

summary(dfh)

plot(dfh$Promotions, dfh$sales,las=1,
     xlab= "Primotions(in crores)", ylab="Sales(in in crores)",
     xlim= c(0,10), ylim= c(0,16))

lines(spline(dfh$Promotions, dfh$sales, method= "fmm"))
lines(smooth.spline(dfh$Promotions, dfh$sales))

library(xlsx)     

df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]


names(df)

df[1:9,]

# sorting for outlier detection
head(data.frame("KM"=df$KM,"SR_Price"=df$SR_Price,
                "Mfg_year"=df$Mfg_Year)[order(-df$KM),])

Age= 2017- df$Mfg_Year
df= cbind(df,Age)
df1= df[,-c(1,2,3)]
head(df1)

set.seed(12345)

partidx= sample(1:nrow(df1),0.5*nrow(df1),replace = F)
df1train= df1[partidx,]
df1test= df1[-partidx,]

mod= lm(Price ~ ., df1train)
summary(mod)
Residualtrain= df1train$Price- mod$fitted.values
head(data.frame("Actual value"= df1train$Price,
                "Predicted value"= mod$fitted.values,
                Residualtrain))
modtest = predict(mod, df1test[,-c(4)])
Residualtest= df1test$Price- modtest
head(data.frame("Actual value"=df1test$Price,"Predicted value"=modtest,
                Residualtest))

install.packages("rminer", dependencies = T) 
library(rminer)
mmetric(df1train$Price, mod$fitted.values,c("SSE","RMSE","ME"))
mmetric(df1test$Price, modtest,c("SSE","RMSE","ME"))


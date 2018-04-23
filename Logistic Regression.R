#### Probabilty odds, and logit
## odd= p/(1-p)
curve(p/(1-p), from=0 , to=1, type= "l", xname = "p", las= 1,
      xlab= "Probability of success", ylab= "logit", xaxt= "n")
## logit= log(odd)= log(p/(1-p))
curve(log(p/(1-p)), from=0 , to=1, type= "l", xname = "p", las= 1,
      xlab= "Probability of success", ylab= "logit", xaxt= "n")
axis(1,pos=0)

df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]
str(df)

dfb= df
df= df[,-5]
df$Promoffer= as.factor(df$Promoffer)
df$Online= as.factor(df$Online)

## Partitioning (60%:40%)
partidx= sample(1:nrow(df),0.6*nrow(df),replace= F)
dftrain= df[partidx,]
dftest= df[-partidx,]

mod= glm(Promoffer ~ Income, family = binomial(link="logit"),data= dftrain)
summary(mod)

b0= unname(mod$coefficients[1])
b1= unname(mod$coefficients[2])

# "P(Prmoffer= Yes| Income= X)" = 1/(1+e^ -(b0+b1*x))
 
range(dftrain$Income)
plot(dftrain$Income, as.numeric(as.character(dftrain$Promoffer)),
     type="p",xlab = "Income", ylab= "Promoffer")
curve(1/(1+exp(-(mod$coefficients[[1]]+mod$coefficients[[2]]*x))),
      xlim= c(0,250), type= "l", xname= "x", add = T)

mod1= glm(Promoffer ~ ., family = binomial(link="logit"),data= dftrain)
summary(mod1)

#P=odds/(1+odds)
curve(odds/(1+odds), from =0, to=100, type="l", xname= "odds",
      xlab= "Odds", ylab= "Probability of Success")

#P= exp(logit)/(1+exp(logit))
curve(exp(logit)/(1+exp(logit)), from =-100, to=100, type="l", xname= "logit",
      xlab= "logit", ylab= "Probability of Success")

modtest= predict(mod1, dftest[,-c(3)],type= "response")
### response returns probabilities

modtestl= predict(mod1, dftest[,-c(3)],type= "link")
### return logit values

modtestc= ifelse(modtest>0.5,1,0)

table("Actual value"=dftest$Promoffer, "Predicted"=modtestc)

mean(modtestc == df$Promoffer)
mean(modtestc != df$Promoffer)

head(data.frame(
  "Predicted class"= modtestc,
  "ACtual class"=dftest$Promoffer,
  "Prob for 1(success)"= modtest,
  "Log odds"= modtestl,
  dftest[,-3], check.names = F
)) 

#Cumulative Lift Curve
dflift= data.frame("Probabilty of class 1"=modtest,"Actual class"= as.numeric(as.character(dftest$Promoffer)),check.names = F)

dflift= dflift[order(dflift[,1],decreasing = T),]
CumACtualClass= cumsum(dflift[,2])
dflift= cbind(dflift, CumACtualClass)
head(dflift)

plot(1:nrow(dflift), dflift$CumACtualClass, "l",
     xlab = "# cases", ylab="cumulative", xlim= c(0,2100),
     ylim = c(0,210))
legend(800,70,inset=0.005,
       c("Cumulative Personal Loan when sorted using predicted values",
         "Cumulative Personal Loan using average"),
       lty= c(1,2), bty= "n", cex= 0.7, x.intersp=0.3, y.intersp= 0.5)

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
modtestl= ifelse(modtest>0.5,1,0)

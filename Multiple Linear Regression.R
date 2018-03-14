library(xlsx)
df= read.xlsx(file.choose(),1,T)
df= df[,!apply(is.na(df),2,all)]
head(df)

Age= 2017- df$Mfg_Year
df= cbind(df, Age)

dfb= df
df= df[,-c(1,2,3,11)]
df$Transmission= as.factor(df$Transmission)

#Partitioning (60%:40%)
partidx= sample(1:nrow(df), 0.6*nrow(df), replace=F)
dftrain= df[partidx,]
dftest= df[-partidx,]

mod= lm(Price ~ ., dftrain)
summary(mod)
#anova(mod)

#Goodness of fit
gf= c(mod$df.residual, summary(mod)$r.squared, summary(mod)$sigma,
      anova(mod)["Residuals","Sum Sq"])
gf= as.data.frame(gf,optional = T)
rownames(gf)=c("Residual df","Multiple R-Squared","Std. Dev. Estimate",
              "Residual SS")

modtest= predict(mod,dftest[,-4])
Residuals= dftest$Price-modtest
head(data.frame(c("Actual Value"=dftest$Price,"Predicted Value"=modtest, Residuals)))

library(rminer)
M= mmetric(dftest$Price, modtest, c("SSE","RMSE","ME"))

boxplot(Residuals, main= "Box Plot of residuals", ylab= "Residual",
        ylim= c(-6,7),las= 1)
quantile(Residuals, probs=c(0.25,0.75))

hist(df$Price, main= "", xlab="Price")

#Normal Probabilty Plot
qqnorm(df$Price)
qqline(df$Price)



#########
library(xlsx)
df= read.xlsx(file.choose(),1,T)
df= df[,!apply(is.na(df),2,all)]
head(df)

Age= 2017- df$Mfg_Year
df= cbind(df, Age)

dfb= df
df= df[,-c(1,2,3,11)]
df$Transmission= as.factor(df$Transmission)

plot(df$KM, df$Price, xlim=c(18,180),  ylim = c(1,75),
     xlab= "KM", ylab="Price")
df= df[-c(13,23,29,73),]

plot(df$KM, df$Price, xlim=c(25,115),  ylim = c(1,14),
     xlab= "KM", ylab="Price")

#Partitioning (60%:40%)
partidx= sample(1:nrow(df), 0.6*nrow(df), replace=F)
dftrain= df[partidx,]
dftest= df[-partidx,]

#Variable Selection 
#Exhaustive Search
library(leaps)
mod3= regsubsets(Price ~., data= dftrain, nbest = 1,nvmax = NULL,
                 force.in = NULL, force.out = NULL,
                 method = "exhaustive", intercept = T)
mod3summ= summary(mod3)

countspch= function(x) sum(x=="*")
om= as.integer(apply(mod3summ$outmat,2,countspch))
data.frame("Coeff"=as.integer(apply(mod3summ$outmat,1,countspch)),
           "RSS"=mod3summ$rss,
           "Cp"=round(mod3summ$cp,digits = 2),
           "R-sq"=round(mod3summ$rsq, digits = 2),
           "Adj.R-sq"=round(mod3summ$adjr2, digits = 2),
           mod3summ$outmat[,order(-om)])


#Coefficients of subset models
coef(mod3,1:8)

#Partial Iterative Searching:
#Forward Selection

mod4= regsubsets(Price ~., data= dftrain, nbest = 1,nvmax = NULL,
                 force.in = NULL, force.out = NULL,
                 method = "forward", intercept = T)
mod4summ= summary(mod4)

countspch= function(x) sum(x=="*")
om1= as.integer(apply(mod4summ$outmat,2,countspch))
data.frame("Coeff"=as.integer(apply(mod4summ$outmat,1,countspch)),
           "RSS"=mod4summ$rss,
           "Cp"=round(mod4summ$cp,digits = 2),
           "R-sq"=round(mod4summ$rsq, digits = 2),
           "Adj.R-sq"=round(mod4summ$adjr2, digits = 2),
           mod4summ$outmat[,order(-om1)])

coef(mod4,1:8)

#Backward elimination:

mod5= regsubsets(Price ~., data= dftrain, nbest = 1,nvmax = NULL,
                 force.in = NULL, force.out = NULL,
                 method = "backward", intercept = T)
mod5summ= summary(mod5)

countspch= function(x) sum(x=="*")
om2= as.integer(apply(mod5summ$outmat,2,countspch))
data.frame("Coeff"=as.integer(apply(mod5summ$outmat,1,countspch)),
           "RSS"=mod5summ$rss,
           "Cp"=round(mod5summ$cp,digits = 2),
           "R-sq"=round(mod5summ$rsq, digits = 2),
           "Adj.R-sq"=round(mod5summ$adjr2, digits = 2),
           mod5summ$outmat[,order(-om2)])

coef(mod5,1:8)

#Sequential Replacement

mod6= regsubsets(Price ~., data= dftrain, nbest = 1,nvmax = NULL,
                 force.in = NULL, force.out = NULL,
                 method = "seqrep", intercept = T)
mod6summ= summary(mod6)

countspch= function(x) sum(x=="*")
om3= as.integer(apply(mod6summ$outmat,2,countspch))
data.frame("Coeff"=as.integer(apply(mod6summ$outmat,1,countspch)),
           "RSS"=mod6summ$rss,
           "Cp"=round(mod6summ$cp,digits = 2),
           "R-sq"=round(mod6summ$rsq, digits = 2),
           "Adj.R-sq"=round(mod6summ$adjr2, digits = 2),
           mod6summ$outmat[,order(-om3)])

coef(mod6,1:8)

#Stepwise Regression

mod7= step(lm(Price~., data = dftrain), direction = "both")
#options(op)

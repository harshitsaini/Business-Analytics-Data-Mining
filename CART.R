library(xlsx)

#Classification Trees
#Sedancar.xlsx
df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]
str(df)

data.frame("Household Number"= 1:20,"Annual Income (in lakhs)"= df$Annual_Income,
           "House Area (in fts)"= df$Household_Area,
           "Ownership of Sedan Car"= df$Ownership,
           check.names = F)

par(mar=c(5.1,5.1,5.1,5.1))
plot(x=df$Annual_Income,y= df$Household_Area, las=1,
     xlab= "Annual Income", ylab= "Household Area",
     xlim = c(2,12), ylim= c(13,25), pch= c(21,19)[as.numeric(df$Ownership)])
legend("bottomright", inset= 0.005, c("owner","Non-owner"),
       pch= c(19,21), cex= 0.7, x.intersp = 0.5, y.intersp = 0.5)

#First split
abline(h=18.8)

#Possible set of split values
# For numerical variables
# Midpoints between pairs of consecutive values for a variable,
#which are ranked as per the impurity (heterogeneity) reduction
#in the resulting rectangular parts

sort(df$Annual_Income)
head(sort(df$Annual_Income),-1)+ diff(sort(df$Annual_Income)/2)
sort(df$Household_Area)
head(sort(df$Household_Area),-1)+ diff(sort(df$Household_Area)/2)

#For Categorical variablles
# set of categories are divided into two subsets
p1= seq(0,1,0.1)
gini= NULL
for(i in 1:length(p1)) {
  gini[i]=1-(p1[i]^2 + (1-p1[i])^2)
}
plot(p1,gini, ylab= "Gini index",type= "l")

entropy= NULL
for(i in 1:length(p1)) {
  entropy[i]= -(p1[i]*log2(p1[i])+ (1-p1[i])*log2(1-p1[i]))
}
plot(spline(p1,entropy), type= "l", xlab= "p1", ylab= "Entropy Measure")

#First split in sedanCar example
summary(df$Ownership)
giorg= 1-(10/20)^2-(10/20)^2
emorg= -(10/20)*log2(10/20)- (10/20)*log2(10/20)

#upper rectangle
giniurec= 1- (7/10)^2- (3/10)^2
emurec= -(7/10)*log2(7/10)- (3/10)*log2(3/10)
ginilrec= giniurec # as upper rectangle and lower rectangle have symmetric proportions
emlrec= emurec

ginisplit1= (10/20)*giniurec + (10/20)*ginilrec
emsplit1= (10/20)*emlrec + (10/20)*emurec

ginidelta = ginisplit1- giorg
emdelta= emsplit1- emorg

#Second Split
segments(7,0,7,18.8)

#Final Stage
segments(5.8,18.8,5.8,26)
segments(5.8,19.5,13,19.5)
segments(0,18.2,7,18.2)

library(rpart)
#method = "class" for a classification tree
#method = "anova" for a regression tree

mod = rpart(Ownership~. , method= "class", data= df,
            control= rpart.control(cp= 0, minsplit = 2, minbucket= 1,
           maxcompete = 0, maxsurrogate = 0 ,xval= 0),parms= list(split= "gini"))
par(mar= c(0,0,0,0), oma= c(0,0,0,0), xpd= NA)
plot(mod, uniform=T, branch= 0.3, compress = T,
     margin = 0.1, nspace=1)
text(mod,splits= T, use.n = T, all= F, minlength = 0,
     cex= 0.8)

install.packages("rpart.plot")
library(rpart.plot)
prp(mod,type=1, extra=1 , under= T, varlen= 0, cex= 0.7,compress = T, Margin = 0 , digits = 0 ,
    split.cex = 0.8, under.cex = 0.8)

#Node numbering 
prp(mod,type=1, extra=1 , under= T, varlen= 0, cex= 0.7,
    compress = T, Margin = 0 , digits = 0 ,
    split.cex = 0.8, under.cex = 0.8, nn=T, nn.cex= 0.6)

#First split
modsub= snip.rpart(mod,toss=c(6:7, 12:13, 24:25))
prp(modsub,type=1, extra=1 , under= T, varlen= 0, cex= 0.7,
    compress = T, Margin = 0 , digits = 0 ,
    split.cex = 0.8, under.cex = 0.8, nn=T, nn.cex= 0.6)
#First 3 splits
modsub1= snip.rpart(mod,toss=c(3,6:7,12:13, 24:25))
prp(modsub1,type=1, extra=1 , under= T, varlen= 0, cex= 0.7,
    compress = T, Margin = 0 , digits = 0 ,
    split.cex = 0.8, under.cex = 0.8, nn=T, nn.cex= 0.6)


summary(mod)


#################
###promooffers###

df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]
str(df)

tPIN= table(as.factor(df$Pin.Code))
PINnames= dimnames(tPIN)[[1]]

C_PINcode= NULL
for(x in PINnames) {
  C_PINcode= c(C_PINcode, length(which(as.character(df$Pin.Code)==x & df$Promoffer==1)))
}
barplot(C_PINcode, names.arg = PINnames, xlab= "PIN Code", las=3, ylab= "Promotional offers Accepted",
        ylim=c(0,20), cex.names= 0.6)

table(as.factor(C_PINcode))

for(x in PINnames) {
  index= which(as.character(df$Pin.Code)==x)
  df[index,]$Pin.Code=rep(C_PINcode[which(PINnames==x)],length(index))
}

df$Pin.Code= as.factor(df$Pin.Code)
df$Education= as.factor(df$Education)
df$Promoffer =as.factor(df$Promoffer)
df$Online= as.factor(df$Online)

str(df)

mod = rpart(Promoffer~. , method= "class", data= df,
            control= rpart.control(cp= 0, minsplit = 2, minbucket= 1,
                                   maxcompete = 0, maxsurrogate = 0 ,xval= 0),parms= list(split= "gini"))

mod_predict= predict(mod, df, type= "class")
table("Actual value"=df$Promoffer, "Predicted value"=mod_predict)
mean(mod_predict==df$Promoffer)

toss1= as.integer(row.names(mod$frame))
x= mod$frame$var




### REGRESSION TREES ###
###usedcars dataset###

df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]
str(df)

Age= 2017-df$Mfg_Year
df= cbind(df,Age)

dfb= df
df= df[,-c(1,2,3,11)]

str(df)
df$Transmission= as.factor(df$Transmission)
str(df)

## Partitioning (60%:40%)
partidx= sample(1:nrow(df),0.6*nrow(df),replace= F)
dftrain= df[partidx,]
dftest= df[-partidx,]

library(rpart)
mod = rpart(Price~. , method= "anova", data= dftrain,
            control= rpart.control(cp= 0, minsplit = 2, minbucket= 1,
                                   maxcompete = 0, maxsurrogate = 0 ,xval= 0),parms= list(split= "gini"))

# No of decision nodes
nrow(mod$splits)

# No of terminal nodes
nrow(mod$frame)-nrow(mod$splits)

toss1= as.integer(row.names(mod$frame)); toss1

DFP= data.frame("toss"= toss1, "Svar"=mod$frame$var,
                "CP"=mod$frame$complexity); DFP

DFP1= DFP[DFP$Svar!="<leaf>",] ;DFP1

DFP2= DFP1[order(DFP1$CP, -DFP1$toss, decreasing = T),] ; DFP2

rownames(DFP2)= 1:nrow(DFP2); DFP2

toss2= DFP2$toss

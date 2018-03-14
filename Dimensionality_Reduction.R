library(xlsx)

df1= read.xlsx(file.choose(),1, header= T)
df1= df1[,!apply(is.na(df1), 2,all)]

Age= 2017- df1$Mfg_Year
df1= cbind(df1,Age)
dfb= df1
df1= df1[,-c(1,2,3)]

head(df1)
str(df1)

#Summary Statistics
countblank= function(x) sum(x=="")

dfsum= data.frame(Average= sapply(df1[,-1],mean),Median= sapply(df1[,-1],median),
                  Min= sapply(df1[,-1],min),Max= sapply(df1[,-1],max),
                  Std= sapply(df1[,-1],sd),Count= sapply(df1[,-1],length),
                  Countblank=sapply(df1[,-1],countblank))            
round(dfsum,digits = 2)                  

M= cor(df1[,-c(1,5,8)]);M
M[upper.tri(M)]=NA;M
print(round(M,digits = 2),na.print = "")
symnum(M)

#Reducing Categories
Age_groups= levels(as.factor(df1$Age))
Age_groups2= as.numeric(Age_groups)
C_PricebyAge1= NULL
C_PricebyAge2= NULL
#Group1 has less than Rs400000 cost
# Rest lies in Group2
for(x in Age_groups2) {
  C_PricebyAge1= c(C_PricebyAge1,
                   100* sum(df1$Age==x & df1$C_Price==0)/sum(df1$Age==x))
  C_PricebyAge2= c(C_PricebyAge2,
                   100* sum(df1$Age==x & df1$C_Price==1)/sum(df1$Age==x))
}
C_PricebyAge= matrix(c(C_PricebyAge1, C_PricebyAge2),nrow = 2,
                     ncol= length(Age_groups), byrow= T)
#palette(c("purple","green"))
barplot(C_PricebyAge, names.arg = Age_groups, xlab= "Age", 
        legend.text = c("0","1"), args.legend = list(x="topright"),
        main= "Distribution of C_Price by Age",col = c("blue","green"),
        ylim = c(0,100), xlim = c(0,12))

Sales= c(45,50, 55,100,51,56,61,125,60,65,70,145,68,74,79,165)

tsv = ts(Sales, start = c(2012,1),end= c(2015,4), frequency = 4)

plot(tsv, xlab= "Quarter", ylab= "Sales(in crores)", las=2 , ylim=c(0,180))

#BreakfastCereals.xlsx
df2= read.xlsx(file.choose(),1,header = T)
df2= df2[, !apply(is.na(df2), 2,all)]

df2=cereal
dim(df2)
df2$vitamins= as.factor(df2$vitamins)
df2$vitamins= as.numeric(df2$vitamins)

df2$mfr= as.factor(df2$mfr)
df2$mfr= as.numeric(df2$mfr)

df2$mfr= as.factor(df2$mfr)
df2$mfr= as.numeric(df2$mfr)

sum=NULL
for(x in 1:dim(df2)[1]) {
  csum=0
  for(y in df2[x,-c(1,9,11)]) {
    csum=csum+y
  }
  sum=c(sum,csum)
}
df2$weight= sum

df3= as.data.frame(lapply(df2[,-c(1,9,11,12)], function(x){x=100*(x/df2$weight)}))
df3= cbind(df3,df2[,c(1,9,11)])

range(df3$potassium)
range(df3$fibre)

plot(df3$potassium, df3$fibre, xlab="POTASSIUM", ylab="FIBRE")

v1= var(df3$potassium)
v2= var(df3$fibre)
c12= cov(df3$potassium,df3$fibre)
matrix(c(v1,c12,c12,v2),2,2,T)

cor(df3$potassium,df3$fibre)

v1+v2
100*v1/(v1+v2)
100*v2/(v1+v2)

#Principal Component Analysis
dfpca= df3[,c(8,5)]
mod= prcomp(dfpca)

#adding PC directions to the plot
slp= with(mod, rotation[2,1]/rotation[1,1])
int= with(mod,center[2]-slp*center[1])

#First principal component
abline(coef= c(int,slp))
mod$rotation

slp1= -1/slp
int1= with(mod,center[2]-slp1*center[1])

#Second principal component
abline(coef= c(int1,slp1))
mod$rotation

head(mod$x)
dfpca[1,]
First= mod$rotation[1,1]*(dfpca[1,1]-mean(dfpca[,1]))+
  mod$rotation[1,2]*(dfpca[1,2]-mean(dfpca[,2])); First

vz1= var(mod$x[,1])
vz2= var(mod$x[,2])
vz1+vz2
100*vz1/(vz1+vz2)
100*vz2/(vz1+vz2)

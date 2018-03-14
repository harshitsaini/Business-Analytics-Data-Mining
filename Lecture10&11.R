library(xlsx)

df1= read.xlsx(file.choose(),1, header= T)
df1= df1[,!apply(is.na(df1), 2,all)]

Age= 2017- df1$Mfg_Year
df1= cbind(df1,Age)
dffb= df1
df1= df1[,-c(1,2,3)]

head(df1)
str(df1)
df1$Transmission= as.factor(df1$Transmission)
df1$C_Price= as.factor(df1$C_Price)
str(df1)
summary(df1)
dfb=df1
df1=df1[-23,]

dffb[dffb$Price>70,]
dffb[dffb$Price>12,]
dffb[dffb$KM>150,]

dffb= dffb[-c(13,23,29,65,73),]
range(dffb$KM)
range(dffb$Price)
plot(dffb$KM,dffb$Price, xlim= c(25,120), ylim=c(1,9),xlab="KM",ylab="Price", panel.first = grid())
#dffb$Model= as.factor(dffb$Model)
#dffb$Model= as.numeric(dffb$Model)
text(dffb$KM,dffb$Price , dffb$Model, adj= c(-0.4,-0.4), cex= 0.5)
#dffb$Model= as.factor(dffb$Model)

df3= read.xlsx(file.choose(),1, header= T)
df3= df3[,!apply(is.na(df3), 2,all)]

palette()
palette(c("gray","black"))

plot(df3$Income, df3$Spending, xlim=c(0,225), ylim=c(0,11),
     xlab="Income", ylab="Spending", col= as.factor(df3$Promoffer), 
     pch=19, cex=0.8, panel.first = grid())

plot(jitter(df3$Income,1), df3$Spending, xlim=c(0,225), ylim=c(0,11),
     xlab="Income", ylab="Spending", col= as.factor(df3$Promoffer), 
     pch=20, cex=0.8, panel.first = grid())
par(mar=c(4,4,1,1), oma=c(1,1,1,1))

plot(jitter(df3$Income,1), df3$Spending, log= "xy",
     xlab="Income", ylab="Spending", col= as.factor(df3$Promoffer), 
     pch=20, cex=0.7, panel.first = grid())
palette("default")

#MULTIVARIATE PLOT
#PARALLEL COORDINATES PLOT
library(MASS)
par(mfrow=c(2,1), cex=0.6, mar= c(3,3,0,0), oma=c(1,1,1,1))
df4= df1
levels(df4$Fuel_type)=1:length(levels(df4$Fuel_type))
df4=as.data.frame(lapply(df4,FUN=as.numeric))

parcoord(df4[which(df4$C_Price=='1'),-c(4,8)])
axis(2,at=axTicks(2), labels=c("0%","20%","40%","60%","80%","100%"))
grid()
parcoord(df4[which(df4$C_Price=='2'),-c(4,8)],col="gray")
axis(2,at=axTicks(2), labels=c("0%","20%","40%","60%","80%","100%"))
grid()

#Specialized Visualization
#Network Data
#Network Graph
#Two mode or bipartite graphs
#Example for association rules

item1= sample(LETTERS[1:10], size=50 ,T)
pool= letters[1:10]
item2=NULL
for(i in 1:50) {
  item2=c(item2,sample(pool[-which(pool==tolower(item1[i]))],size=1,replace=T))
}
df5= data.frame(item1,item2)

library(igraph)
g= graph_from_data_frame(df5,directed = F)

V(g)$label= V(g)$name
V(g)[1:10]$type=1
V(g)[11:20]$type=2

V(g)$color= "gray"
E(g)$color= "black"
V(g)$shape= "circle"

V(g)$x= c(runif(10,0,5),runif(10,10,15))
V(g)$y= c(seq(10,1,by=-1),seq(10,1,by=-1))



E(g)$weight= count.multiple(g)
g1= simplify(g, remove.multiple = T)
E(g1)$width= 0.5*E(g1)$weight


size= NULL
for(i in V(g1)$name){
  size=c(size,length(E(g1)[from(V(g1)[i])]))
}
V(g1)$size= 4*size
par(mar= rep(.1,4))

V(g1)$color= "gray"
E(g1)$color= "black"

plot(g1)

#Heirarchical Data
#Treemaps
df6= read.xlsx(file.choose(),1, header= T)
df6= df6[,!apply(is.na(df6), 2,all)]

library(treemap)
rec.size= ifelse(df6$price>=5000+df6$price/10, df6$price)
df6= cbind(df6,rec.size)

par(mar= rep(.1,4))

treemap(df6,index= c("item.category","subcategory","brand"),
        vsize= "rec..size", vColor="rating",
        type= "value", fun.aggregate = "mean",
        palette = gray(0:4/4), fontsize.labels = c(11,9,6),
        title= "", position.legend = "none")

#Geographical data
#Map chart 
df7= read.xlsx(file.choose(),1, header= T)
df7= df7[,!apply(is.na(df7), 2,all)]
library(rworldmap)

mapDevice(rows= 2, columns= 1)
datamap= joinCountryData2Map(df7, nameJoinColumn = "Country", joinCode = "Name")
mapCountryData(datamap, nameColumnToPlot = "Inclusive.Internet.Index",
               catMethod = "pretty", colourPalette = gray(7:0/7),
               addLegend = F)
mapCountryData(datamap, nameColumnToPlot = "Corruptions.Perceptions.Index",
               catMethod = "pretty", colourPalette = gray(7:0/7),
               addLegend = F)

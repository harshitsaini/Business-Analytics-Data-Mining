library(xlsx)

df= read.xlsx(file.choose(),1,header = T)
df= df[,!apply(is.na(df),2,all)]
head(df)
summary(df)

plot(df$Annual_Income, df$Household_Area, las= 1,
     xlab= "Annual Income (a.'lakhs)", ylab= "Houshold Area (00s fts)",
     xlim= c(2,12), ylim= c(13,25), pch=c(21,19)[as.numeric(df$Ownership)])
legend("bottomright",inset= 0.005,c("Owner","Nonowner"),
       pch=c(19,21),cex= 0.7,x.intersp = 0.5, y.intersp = 0.5)
df[df$Annual_Income>5 & df$Annual_Income<8.5 & df$Household_Area>18
   & df$Household_Area<20, c(1,2)]
abline(h=18.8, col=3)
segments(7,0,7,18.8,col = 3)
segments(5.8,18.8,5.8,26,col = 3)

df[df$Annual_Income>6 & df$Annual_Income<8.5 & df$Household_Area>18 
   & df$Household_Area<21, c(1,2)]
segments(5.8,19.5,13,19.5,col = 3)

df[df$Annual_Income<7 & df$Household_Area>17 & df$Household_Area<19, c(1,2)]
segments(0,18.2,7,18.2,col = 3)

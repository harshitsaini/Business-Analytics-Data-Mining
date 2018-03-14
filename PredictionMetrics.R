library(xlsx)
df= read.xlsx(file.choose(),1,T)
df= df[,!apply(is.na(df),2,all)]
head(df)

plot(df$Serial.No, df$Cumulative.value, type = "l",
     xlab= "# cases", ylab= "Cumulative value",
     xlim= c(0,25), ylim= c(40 ,550))
segments(0,0,20,544,lty=3)
legend(12,200, inset=0.005,
       c("Cumulative value sorted by predicted value",
         "reference line"),
       lty= c(1,2), bty= "n", cex= 0.7, x.intersp= 0.3, y.intersp= 0.3)

# PRISTOP REŠEVANJA Z RISANJEM DVEH PREMIC - 
getwd()
setwd("C:/Users/aanja/OneDrive/Dokumenti/fmf/magisterski študij/matematika z raèunalnikom/Nelinearne-transakcije/projekt")
#________________________________________________________________________________
#Knjižnice
library(nlme)
#________________________________________________________________________________
# preberi csv datoteko
df <- read.csv("podatki.csv", sep=';', header = FALSE, stringsAsFactors=FALSE)
colnames(df) <- c('Price','Profit')

df[order(df$Price),]

# pretvori niz v vektor stevilk
price <- as.numeric(gsub(",", ".", df$Price))
profit <- as.numeric(gsub(",", ".", df$Profit))
profit <- profit / 1000 # profit / 1000 za lažjo predstavo, izbriši kasneje

#________________________________________________________________________________
# ugotovi za kakšno opcijo gre - najprej loèimo med put in call
# 
# for (i in 1:length(price)/2){
#   if 
# }





#________________________________________________________________________________
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Cena vs Profit",
     pch = 1, cex=1.5)
abline(h = 0, lty='dashed') #meja med poz in neg profiti


# #NEUSPELI POSKUS
# # RAZDELIMO NA 2 PREMICI, KJE JE MEJNA TOÈKA? 
# # Uporabimo lin, regresijo na dveh loèenih podatkih
# 
# df1 <- data.frame(df$Price[1:round((length(price)/2),0)], df$Profit[1:round((length(price)/2),0)])
# colnames(df1) =c('Price1','Profit1')
# 
# df2 <- data.frame(df$Price[round((length(price)/2),0)+1:(length(price))], df$Profit[round((length(price)/2),0)+1:(length(price))])
# colnames(df2)=c('Price2','Profit2')
# 
# profit1 <- as.numeric(gsub(",", ".", df1$Profit1))
# profit2 <- as.numeric(gsub(",", ".", df2$Profit2))
# price1 <- as.numeric(gsub(",", ".", df1$Price1))
# price2 <- as.numeric(gsub(",", ".", df2$Price2))
# 
# 
# profit1 <- (profit1[ profit1 != 0 ])
# price1 <- (price1[ price1 != 0 ])
# profit2 <- (profit2[ profit2 != 0 ])
# price2 <- (price2[ price2 != 0 ])
# 
# fit1 <- lm(profit1 ~ price1)
# abline(fit1$coefficients[1],fit1$coefficients[2])
# 
# fit2 <- lm(profit2 ~ price2)
# abline(fit2$coefficients[1],fit2$coefficients[2])



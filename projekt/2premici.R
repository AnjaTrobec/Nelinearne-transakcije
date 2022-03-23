# PRISTOP REŠEVANJA Z RISANJEM DVEH PREMIC - 
getwd()
setwd("C:/Users/aanja/OneDrive/Dokumenti/fmf/magisterski študij/matematika z raèunalnikom/Nelinearne-transakcije/projekt")
#________________________________________________________________________________
# preberi csv datoteko
df <- read.csv("podatki.csv", sep=';', header = FALSE, stringsAsFactors=FALSE)
colnames(df) <- c('Price','Profit')

# pretvori niz v vektor stevilk
price <- as.numeric(gsub(",", ".", df$Price))
profit <- as.numeric(gsub(",", ".", df$Profit))
profit <- profit / 1000 # profit / 1000 za lažjo predstavo, izbriši kasneje
#________________________________________________________________________________
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Cena vs Profit",
     pch = 1, cex=1.5)
abline(h = 0, lty='dashed') #meja med poz in neg profiti

#1. VODORAVNA PREMICA
#ideja, razdelim vrednosti iz stolpca profit na izgubo in dobièek, vzamem povpreèje izgube, da izraèunam povpreèno premijo 
# ali morda še boljši naèin - vzamem modus, ker prièakujemo izravnavo in išèemo vrednosti, ki se najveèkrat pojavi

indexi_neg_profit <- which(profit < 0)
vrednosti_neg_profiti <- round(sort(profit[indexi_neg_profit]),0)

#oceni s povpreèjem negativnih profitov
abline(h=mean(vrednosti_neg_profiti),col='darksalmon', lwd=2)

# #oceni z modusom - najbolj pogosta vrednost
# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# modus <- getmode(vrednosti_neg_profiti)
# abline(h=modus, col='red')

#2. POŠEVNA PREMICA
indexi_poz_profit <- which(profit > 0)
vrednosti_poz_profit <- round(sort(profit[indexi_poz_profit]),0)
cene_poz <- price[indexi_poz_profit]

fit <- lm(vrednosti_poz_profit ~ cene_poz)
abline(fit$coefficients[1],fit$coefficients[2], col='cadetblue2', lwd=2)
legend(x='topright',legend=c('podatki','y = premija','fit 2. premica'),col=c('black','darksalmon','cadetblue2'), lty=c(NA,1,1),pch=c(1,NA,NA),lwd=c(1,2,2))

# POIŠÈI K IN PREMIJO
#premijo ocenim s povpreèjem neg. profitov
premija = mean(vrednosti_neg_profiti)
K = (premija-fit$coefficients[1])/fit$coefficients[2]




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


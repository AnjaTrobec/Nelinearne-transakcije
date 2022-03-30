# Delo na razliènih podatkih

#Kako jih loèiti?
Po korelaciji med podatki!!

#_______________________________________________________________________________
#1. podatki, nakup prodajne opcije
df <- read.csv("podatki.csv", sep=';', header = FALSE, stringsAsFactors=FALSE)
colnames(df) <- c('Price','Profit')
df[order(df$Price),]
price <- as.numeric(gsub(",", ".", df$Price))
profit <- as.numeric(gsub(",", ".", df$Profit))

korelacija1 <- cor(price, profit)
korelacija1
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Cena vs Profit",
     pch = 1, cex=1.5)
abline(h = 0, lty='dashed') #meja med poz in neg profiti

nakup_put_opcije(price,profit)

#NARIŠI NAJBOLJŠO APROKSIMACIJO
najboljsi_K <- which(min(odstopanja)==odstopanja)
premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
abline(h = profit[najboljsi_K], col = 'red')
premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')
najboljsi_K

#________________________________________________________________________________
#2. podatki, nakup call opcije
dfA <- read.csv("Naloga_A.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfA) <- c('Price','Profit')
price <- as.numeric(gsub(",", ".", dfA$Price))
profit <- as.numeric(gsub(",", ".", dfA$Profit))

korelacija2 <- cor(price, profit)
korelacija2
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Cena vs Profit",
     pch = 1, cex=1.5)
abline(h = 0, lty='dashed')
#NARIŠI NAJBOLJŠO APROKSIMACIJO
najboljsi_K <- which(min(odstopanja)==odstopanja)
premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
abline(h = profit[najboljsi_K], col = 'red')
premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')
najboljsi_K

#________________________________________________________________________________
#3. podatki, prodaja nakupne opcije
dfB <- read.csv("Naloga_B.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfB) <- c('Price','Profit')
price <- as.numeric(gsub(",", ".", dfB$Price))
profit <- as.numeric(gsub(",", ".", dfB$Profit))

korelacija3 <- cor(price, profit)
korelacija3
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Cena vs Profit",
     pch = 1, cex=1.5)
abline(h = 0, lty='dashed')

#NARIŠI NAJBOLJŠO APROKSIMACIJO
najboljsi_K <- which(min(odstopanja)==odstopanja)
premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
abline(h = profit[najboljsi_K], col = 'red')
premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')
najboljsi_K
#________________________________________________________________________________
#4. podatki, nakup call opcije
dfC <- read.csv("Naloga_C.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfC) <- c('Price','Profit')
price <- as.numeric(gsub(",", ".", dfC$Price))
profit <- as.numeric(gsub(",", ".", dfC$Profit))

korelacija4 <- cor(price, profit)
korelacija4
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Cena vs Profit",
     pch = 1, cex=1.5)
abline(h = 0, lty='dashed')

#NARIŠI NAJBOLJŠO APROKSIMACIJO
najboljsi_K <- which(min(odstopanja)==odstopanja)
premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
abline(h = profit[najboljsi_K], col = 'red')
premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')
najboljsi_K


# Delo na razliènih podatkih
opt_fit <- function(price, profit) {
  
  povprecna_napaka <- function(sm){mean(sm$residuals^2)}
  
  odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
  odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
  odstopanja <- rep(0,length(price)) #skupno odstopanje
  najboljsi_K = 0
  
  for (K in 1:(length(price))){
    
    # test[K] = K
    
    #VODORAVNA PREMICA
    premica1 <- profit[K]
    #abline(h = premica1, col = 'green')
    pomozni_seznam = rep(0,length(profit[1:K]))
    for (i in 1:length(profit[1:K])){
      pomozni_seznam[i] = sqrt(sum((premica1 - profit[i])^2))
    }
    for (j in 1:length(pomozni_seznam)){
      odstopanje1[K] = odstopanje1[K] + pomozni_seznam[j]
    }
    odstopanje1[K]
    
    #POŠEVNA PREMICA
    premica2 <- lm(profit[K:length(price)] ~ price[K:length(price)])
    #abline(premica2$coefficients[1],premica2$coefficients[2], col = 'pink')
    odstopanje2[K] <- povprecna_napaka(premica2)
    odstopanje2[K]
    
    odstopanja[K] <- odstopanje1[K] + odstopanje2[K]
    
    if(odstopanje1[K] == 0){
      odstopanje1[K]= 5*10^10
      odstopanja[K] = 5*10^10
    }
    if(odstopanje2[K] == 0){
      odstopanje2[K]= 5*10^10
      odstopanja[K] = 5*10^10
    }
    odstopanja[K]
  }
  
  
  najboljsi_K <- which(min(odstopanja) == odstopanja)
  premica1 <- profit[najboljsi_K]
  abline(h = profit[najboljsi_K], col = 'red')
  premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
  abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')
  najboljsi_K
  
  strike_price = price[najboljsi_K]
  premija = profit[najboljsi_K]
  komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), ", za premijo pa ", as.character(premija), ".",sep="")
  print(komentar)
  
}

#_______________________________________________________________________________
#1. podatki, nakup prodajne opcije
df <- read.csv("podatki.csv", sep=';', header = FALSE, stringsAsFactors=FALSE)
colnames(df) <- c('Price','Profit')
price <- as.numeric(gsub(",", ".", df$Price))
profit <- as.numeric(gsub(",", ".", df$Profit))
profit <- profit / 1000

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


opt_fit(price,profit)

#________________________________________________________________________________
#2. podatki, nakup call opcije
dfA <- read.csv("Naloga_A.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfA) <- c('Price','Profit')
price <- as.numeric(gsub(",", ".", dfA$Price))
profit <- as.numeric(gsub(",", ".", dfA$Profit))
profit <- profit / 1000

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

opt_fit(price,profit)


#________________________________________________________________________________
#3. podatki, prodaja nakupne opcije
dfB <- read.csv("Naloga_B.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfB) <- c('Price','Profit')
price <- as.numeric(gsub(",", ".", dfB$Price))
profit <- as.numeric(gsub(",", ".", dfB$Profit))
profit <- profit / 1000

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

opt_fit(price,profit)
#________________________________________________________________________________
#4. podatki, nakup call opcije
dfC <- read.csv("Naloga_C.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfC) <- c('Price','Profit')
price <- as.numeric(gsub(",", ".", dfC$Price))
profit <- as.numeric(gsub(",", ".", dfC$Profit))
profit <- profit / 1000

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
opt_fit(price,profit)

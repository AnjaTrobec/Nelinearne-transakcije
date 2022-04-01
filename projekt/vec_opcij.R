# Delo na razliènih podatkih
opt_fit <- function(price, profit){
  if (cor(price, profit) < 0){
    #nakup put opcije (3) ali prodaja nakupne opcije (2)
    
    meja <- length(price)/4
    povpr1 <- mean(profit[1:meja])
    er1 <- 0
    
    for (i in 1:meja){
      er1 <- er1 + (profit[i] - povpr1)^2}
    
    povpr1 <- mean(profit[(length(price)-meja):length(price)])
    er2 <- 0
    for (i in (length(price)-meja):length(price)){
      er2 <- er2 + (profit[i] - povpr1)^2}

    
    #NAKUP PUT OPCIJE
    if (er1 > er2){
      komentar <- paste("Gre za nakup put opcije.")
      print(komentar)
      
      odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
      #odstopanja <- rep(0,length(price)) #skupno odstopanje
      najboljsi_K = 0
      
      
      for (K in 1:length(price)){
        #VODORAVNA PREMICA
        premica1 <- profit[K]
        napaka1 <- rep(0,length(price[K:length(profit)]))
        profiti <- profit[K:length(profit)]
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profiti[i])^2))
        }
        odstopanje1[K] <- sum(napaka1)
        
        
        #POŠEVNA PREMICA
        premica2 <- lm(profit[1:K] ~ price[1:K])
        odstopanje2[K] <- deviance(premica2)
        odstopanje2[K]
        
        
        # #èe pokaže kako vrednost 0 
        # if(odstopanje1[K] == 0){
        #   odstopanje1[K] = 5*10^10
        # }
        # if(odstopanje2[K] == 0){
        #   odstopanje2[K] = 5*10^10
        # }
      }
      
      odstopanja <- odstopanje1 + odstopanje2
      
      najboljsi_K <- which(min(odstopanja) == odstopanja)
      premica1 <- mean(profit[najboljsi_K:length(profit)])
      abline(h = profit[najboljsi_K], col = 'red', lwd=2)
      premica2 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
      abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue',lwd=2)
      najboljsi_K
      
      strike_price = price[najboljsi_K]
      premija = profit[najboljsi_K]
      komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), ", za premijo pa ", as.character(premija), ".",sep="")
      print(komentar)
        
      }
      
  

    #PRODAJA CALL OPCIJE
    if (er2 > er1){
      komentar <- paste("Gre za prodajo call opcije.")
      print(komentar)

      for (K in 1:(length(price))){
        odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
        odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
        #odstopanja <- rep(0,length(price)) #skupno odstopanje
        najboljsi_K = 0

        #VODORAVNA PREMICA
        premica1 <- profit[K]
        napaka1 <- rep(0,length(price[1:K]))
        profiti <- profit[1:K]
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profiti[i])^2))
        }
        odstopanje1[K] <- sum(napaka1)
        
        
        #POŠEVNA PREMICA
        premica2 <- lm(profit[K:length(price)] ~ price[K:length(price)])
        odstopanje2[K] <- deviance(premica2)
        odstopanje2[K]

      }

      odstopanja <- odstopanje1 + odstopanje2
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

    

  }
  if (cor(price, profit) > 0){
    #nakup call opcije ali prodaja put opcije
    #Ugotovi za katero gre:
    meja <- length(price)/4
    povpr1 <- mean(profit[1:meja])
    er1 <- 0
    
    for (i in 1:meja){
      er1 <- er1 + (profit[i] - povpr1)^2
    }
    
    povpr1 <- mean(profit[(length(price)-meja):length(price)])
    er2 <- 0
    for (i in (length(price)-meja):length(price)){
      er2 <- er2 + (profit[i] - povpr1)^2
    }
    
    
    #PRODAJA PUT OPCIJE
    if (er1 > er2){
      komentar <- paste("Gre za prodajo put opcije.")
      print(komentar)
      
      odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
      #odstopanja <- rep(0,length(price)) #skupno odstopanje
      najboljsi_K = 0
      
      
      for (K in 1:length(price)){
        #VODORAVNA PREMICA
        premica1 <- mean(profit[K:length(profit)])
        napaka1 <- rep(0,length(price[K:length(profit)]))
        profiti <- profit[K:length(profit)]
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profiti[i])^2))
        }
        odstopanje1[K] <- sum(napaka1)
        
        
        #POŠEVNA PREMICA
        premica2 <- lm(profit[1:K] ~ price[1:K])
        odstopanje2[K] <- deviance(premica2)
        odstopanje2[K]
        
        
        # #èe pokaže kako vrednost 0 
        # if(odstopanje1[K] == 0){
        #   odstopanje1[K] = 5*10^10
        # }
        # if(odstopanje2[K] == 0){
        #   odstopanje2[K] = 5*10^10
        # }
      }
      
      odstopanja <- odstopanje1 + odstopanje2
      
      najboljsi_K <- which(min(odstopanja) == odstopanja)
      premica1 <- mean(profit[najboljsi_K:length(profit)])
      abline(h = profit[najboljsi_K], col = 'red')
      premica2 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
      abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')
      najboljsi_K
      
      strike_price = price[najboljsi_K]
      premija = profit[najboljsi_K]
      komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), ", za premijo pa ", as.character(premija), ".",sep="")
      print(komentar)
      
    }
    
    
    
    #NAKUP CALL OPCIJE
    if (er2 > er1){
      komentar <- paste("Gre za nakup call opcije.")
      print(komentar)
      
      for (K in 1:(length(price))){
        odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
        odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
        # odstopanja <- rep(0,length(price)) #skupno odstopanje
        najboljsi_K = 0
        
        #VODORAVNA PREMICA
        premica1 <- mean(profit[1:K])
        napaka1 <- rep(0,length(price[1:K]))
        profiti <- profit[1:K]
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profiti[i])^2))
        }
        odstopanje1[K] <- sum(napaka1)
        
        
        #POŠEVNA PREMICA
        premica2 <- lm(profit[K:length(price)] ~ price[K:length(price)])
        odstopanje2[K] <- deviance(premica2)
        odstopanje2[K]
        
      }
      
      odstopanja <- odstopanje1 + odstopanje2
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
    
    
    
  }
}

#_______________________________________________________________________________
#1. podatki, nakup prodajne opcije
df <- read.csv("podatki.csv", sep=';', header = FALSE, stringsAsFactors=FALSE)
colnames(df) <- c('Price','Profit')
if (df$Price[2] > df$Price[3]){
  df <- df[order(df$Price),]} #podatke uredim po cene padajoèe
df
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
     pch = 20, cex=1.5)
abline(h = 0, lty='dashed') #meja med poz in neg profiti

opt_fit(price,profit)

#________________________________________________________________________________
#2. podatki, nakup call opcije
dfA <- read.csv("Naloga_A.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfA) <- c('Price','Profit')
df[order(df$Price),]
price <- as.numeric(gsub(",", ".", dfA$Price))
profit <- as.numeric(gsub(",", ".", dfA$Profit))
profit <- profit / 1000

korelacija2 <- cor(price, profit)


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


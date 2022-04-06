# Delo na razliènih podatkih
opt_fit <- function(price, profit){
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
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

    
    # #1. NAKUP PUT OPCIJE__________________________________________________________
    # if (er1 > er2){
    #   komentar <- paste("Gre za nakup put opcije.")
    #   print(komentar)
    # 
    #   odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
    #   odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
    #   najboljsi_K = 0
    # 
    # 
    #   #GLAVNA ZANKA
    #   for (K in 1:length(price)){
    #     #VODORAVNA PREMICA
    #     premica1 <- profit[K]
    #     napaka1 <- rep(0,length(price[K:length(profit)]))
    #     profiti <- profit[K:length(profit)]
    #     for (i in 1:length(napaka1)){
    #       napaka1[i] <- (((premica1 - profiti[i])^2))
    #     }
    #     odstopanje1[K] <- sum(napaka1)
    # 
    # 
    #     #POŠEVNA PREMICA
    #     premica2 <- lm(profit[1:K] ~ price[1:K])
    #     odstopanje2[K] <- deviance(premica2)
    #     odstopanje2[K]
    #   }
    # 
    #   odstopanja <- odstopanje1 + odstopanje2
    # 
    #   najboljsi_K <- which(min(odstopanja) == odstopanja)
    #   premica1 <- mean(profit[najboljsi_K:length(profit)])
    #   abline(h = profit[najboljsi_K], col = 'red', lwd=2)
    #   
    #   premica2 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
    #   abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue',lwd=2)
    #   najboljsi_K
    # 
    #   points(price[najboljsi_K], profit[najboljsi_K],type = "p", col = "green", pch=19)
    #   strike_price = price[najboljsi_K]
    #   premija = profit[najboljsi_K]
    #   komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), ", za premijo pa ", as.character(premija), ".",sep="")
    #   print(komentar)
    # 
    #   }

  

    #PRODAJA CALL OPCIJE__________________________________________________________
    
    if (er2 > er1){
      komentar <- paste("Gre za prodajo call opcije.")
      print(komentar)
      
      odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
      
      zaokrozeni <- round(profit,0)
      modus <- getmode(zaokrozeni)
      
      premica1 <- modus
      abline(h=modus, col='red',lwd=2)
      
      profit2 <- rep(0,length(profit))
      cena2 <- rep(0,length(profit))
        
      S <- sd(profit)
      L <- modus - 0.25*S/sqrt(length(profit))
      U <- modus + 0.25*S/sqrt(length(profit))
      
      zaporedje <- round(seq(L,U,0.001),2)
      
      '%ni%' <- Negate(`%in%`)
      
      for (i in 1:length(profit)) {
        if (zaokrozeni[i] %ni% zaporedje){
          profit2[i] <- profit[i]
          cena2[i] <- price[i]
        }
      }
      
      profit2 <- if(length(which(profit2==0)!=0)) profit2[-which(profit2==0)]
      cena2 <- if(length(which(cena2==0)!=0)) cena2[-which(cena2==0)]
    
      koraki <- 101
      k = rep(0,koraki)
      while (i < koraki){
      k[i] = (sample(profit2,1)-sample(profit2,1))/(sample(cena2,1)-sample(cena2,1))
      }
      #linearna regresija ne naredi najboljše
      premica2 <- lm(profit2 ~ cena2)
      abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue', lwd=2)
      
     #_________________________________________ 
      
      
      for (K in 1:length(price)){
        #VODORAVNA PREMICA
        premica1 <- mean(profit[1:K])
        napaka1 <- rep(0,length(price[1:K]))
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profit[i])^2))
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
      abline(h = profit[najboljsi_K], col = 'red', lwd=2)
      
      
      premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
      abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue', lwd=2)
      najboljsi_K
      
      points(price[najboljsi_K],type = "p", col = "green", pch=19)
      
      strike_price = price[najboljsi_K]
      premija = profit[najboljsi_K]
      komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), ", za premijo pa ", as.character(premija), ".",sep="")
      print(komentar)
    }
  }
  
  if (cor(price, profit) > 0){
    #nakup call opcije ali prodaja put opcije

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
    
    #PRODAJA PUT OPCIJE__________________________________________________________
    if (er1 > er2){
      komentar <- paste("Gre za prodajo put opcije.")
      print(komentar)
      
      #poiscimo optimalni fit
      odstopanje1 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
      najboljsi_K = 0
      
      
      for (K in 1:round(length(price)/2,0)){
        #VODORAVNA PREMICA
        premica1 <- mean(profit[(round(length(price)/2,0)+K):length(profit)])
        napaka1 <- rep(0,length(price[(round(length(price)/2,0)+K):length(profit)]))
        profiti <- profit[(round(length(price)/2,0)+K):length(profit)]
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profiti[i])^2))
        }
        odstopanje1[K] <- sum(napaka1)
        
        
        #POŠEVNA PREMICA
        premica2 <- lm(profit[1:K] ~ price[1:K])
        odstopanje2[K] <- deviance(premica2)
        odstopanje2[K]
      }
      
      odstopanja <- odstopanje1 + odstopanje2
      
      najboljsi_K <- which(min(odstopanja) == odstopanja)
      premica1 <- mean(profit[najboljsi_K:length(profit)])
      abline(h = profit[najboljsi_K], col = 'red', lwd=2)
      
      premica2 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
      abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue', lwd=2)
      najboljsi_K
      
      points(x = price[najboljsi_K], y = profit[najboljsi_K],type = "p", col = "green", pch=19)
      
      
      tocka1 <- c(price[najboljsi_K], profit[najboljsi_K])
      strike_price = price[najboljsi_K]
      premija = profit[najboljsi_K]
      komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), ", za premijo pa ", as.character(premija), ".",sep="")
      print(komentar)
      
    }
    
    #NAKUP CALL OPCIJE__________________________________________________________
    if (er2 > er1){
      komentar <- paste("Gre za nakup call opcije.")
      print(komentar)
      
      odstopanje1 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
      najboljsi_K = 0
      
      for (K in 1:round(length(price)/2,0)){
        
        #VODORAVNA PREMICA
        premica1 <- mean(profit[1:K])
        napaka1 <- rep(0,length(price[1:K]))
        profiti <- profit[1:K]
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profiti[i])^2))
        }
        odstopanje1[K] <- sum(napaka1)
        
        
        #POŠEVNA PREMICA
        premica2 <- lm(profit[(round(length(price)/2,0)+K):length(price)] ~ price[(round(length(price)/2,0)+K):length(price)])
        odstopanje2[K] <- deviance(premica2)
        odstopanje2[K]
        
      }
      
      odstopanja <- odstopanje1 + odstopanje2
      najboljsi_K <- which(min(odstopanja) == odstopanja)
      premica1 <- profit[najboljsi_K]
      abline(h = profit[najboljsi_K], col = 'red', lwd=2)
      
      
      profit_pomozni <- round(profit,0)
      odstrani <- round(profit[najboljsi_K],0)
      profiti_za_drugo <- if(length(which(profit_pomozni==odstrani)!=odstrani)) profit_pomozni[-which(profit_pomozni==odstrani)]
      cene_za_drugo <- if(length(which(profit_pomozni==odstrani)!=odstrani)) price[-which(profit_pomozni==odstrani)]
        
      premica2 <- lm(profiti_za_drugo ~ cene_za_drugo)
      abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue', lwd=2)
      najboljsi_K
      points(x = price[najboljsi_K], y = profit[najboljsi_K],type = "p", col = "green", pch = 19)
      
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
     main = "Nakup put opcije",
     pch = 20, cex=1.5)
abline(h = 0, lty='dashed') #meja med poz in neg profiti

opt_fit(price,profit)

#________________________________________________________________________________
#2. podatki, nakup call opcije
dfA <- read.csv("Naloga_A.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfA) <- c('Price','Profit')
dfA[order(dfA$Price),]
price <- as.numeric(gsub(",", ".", dfA$Price))
profit <- as.numeric(gsub(",", ".", dfA$Profit))
profit <- profit / 1000

korelacija2 <- cor(price, profit)


#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Nakup call opcije",
     pch = 20, cex=1.5)
abline(h = 0, lty='dashed')

opt_fit(price,profit)


#________________________________________________________________________________
#3. podatki, prodaja nakupne opcije
par(mfrow=c(1,1))

dfB <- read.csv("Naloga_B.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfB) <- c('Price','Profit')

price <- as.numeric(gsub(",", ".", dfB$Price))
profit <- as.numeric(gsub(",", ".", dfB$Profit))

dfB <- data.frame(price,profit)
dfB <- dfB[order(dfB$price, decreasing = FALSE),]

price <- dfB$price
profit <- dfB$profit
profit <- profit/1000

korelacija3 <- cor(price, profit)
korelacija3
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Prodaja call opcije",
     pch = 20, cex=1.5)
abline(h = 0, lty='dashed')

opt_fit(price,profit)
#________________________________________________________________________________
#4. podatki, nakup call opcije
dfC <- read.csv("Naloga_C.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
colnames(dfC) <- c('Price','Profit')

price <- as.numeric(gsub(",", ".", dfC$Price))
profit <- as.numeric(gsub(",", ".", dfC$Profit))

dfC <- data.frame(price,profit)
dfC <- dfC[order(dfC$price,decreasing = FALSE),]

price <- dfC$price
profit <- dfC$profit
profit <- profit / 1000


korelacija4 <- cor(price, profit)
korelacija4
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Nakup call opcije",
     pch = 20, cex=1.5)
abline(h = 0, lty='dashed')
opt_fit(price,profit)


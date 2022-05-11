opt_fit <- function(price, profit){
  #spremenimo in dodamo začetni plot že tukaj:
  price <- as.numeric(gsub(",", ".", price))
  profit <- as.numeric(gsub(",", ".", profit))
  df <- data.frame(price,profit)
  df <- df[order(df$price, decreasing = FALSE),]
  price <- as.numeric(df$price)
  profit <- as.numeric(df$profit)
  plot(x = price,
       y = profit,
       xlab = "Cena (v EUR/MWh)",
       ylab = "Profit (v EUR)",
       pch = 20, cex=1.5)
  abline(h = 0, lty='dashed')
  
  if (cor(price, profit) < 0){
    #nakup put opcije (3) ali prodaja call opcije (2)
    #pogledamo napake odstopanj in odlocimo ali gre za put ali call
    
    meja <- length(price)/4
    povpr1 <- mean(profit[1:meja])
    er1 <- 0
    for (i in 1:meja){
      er1 <- er1 + (profit[i] - povpr1)^2}
    povpr1 <- mean(profit[(length(price)-meja):length(price)])
    er2 <- 0
    for (i in (length(price)-meja):length(price)){
      er2 <- er2 + (profit[i] - povpr1)^2}
    
    
    #1. NAKUP PUT OPCIJE__________________________________________________________
    if (er1 > er2){
      tip_opcije <- paste("Gre za nakup put opcije.")
      #print(tip_opcije)
      
      odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (posevni del)
      najboljsi_K = 0
      for (K in 1:length(price)){
        premica1 <- profit[K]
        napaka1 <- rep(0,length(price[K:length(profit)]))
        profiti <- profit[K:length(profit)]
        
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profiti[i])^2))
        }
        odstopanje1[K] <- sum(napaka1)
        
        premica2 <- lm(profit[1:K] ~ price[1:K])
        odstopanje2[K] <- deviance(premica2)
        odstopanje2[K]
      }
      
      odstopanja <- odstopanje1 + odstopanje2
      
      najboljsi_K <- which(min(odstopanja) == odstopanja)
      premica1 <- mean(profit[najboljsi_K:length(profit)])
      abline(h = profit[najboljsi_K], col = 'red', lwd=2)
      premica2 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
      abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue',lwd=2)
      najboljsi_K
      
      # ggplot(dataframe(price,profit),aes(x=price,y=profit)) + 
      #   geom_point(aes(x=price,y=profit)) +
      #   geom_hline(yintercept=0, linetype="dashed", color = "red") +
      #   geom_hline(yintercept= profit[najboljsi_K], col = 'red', lwd=2) +
      #   geom_line(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue',lwd=2)
      # 
      
      #points(price[najboljsi_K], profit[najboljsi_K],type = "p", col = "green")
      
      strike_price = round(price[najboljsi_K],3)
      premija = abs(round(profit[najboljsi_K],3))
      kolicina = abs(round(premica2$coefficients[2],3))
      komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), " EUR/MWh,", " za količino ", as.character(kolicina), " MWh", " in za premijo ", as.character(premija), " EUR", ".",sep="")
      #print(komentar)
      
    }
    
    #PRODAJA CALL OPCIJE__________________________________________________________
    
    if (er2 > er1){
      tip_opcije <- paste("Gre za prodajo call opcije.")
      #print(tip_opcije)
      
      odstopanje1 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z linearno regresijo (posevni del)
      najboljsi_K = 0
      
      for (K in 1:round(length(price)/2,0)){
        premica1 <- mean(profit[1:K])
        napaka1 <- rep(0,length(price[1:K]))
        for (i in 1:length(napaka1)){
          napaka1[i] <- (((premica1 - profit[i])^2))
        }
        odstopanje1[K] <- sum(napaka1)
        
        
        premica2 <- lm(profit[(round(length(price)/2,0)+K):length(price)] ~ price[(round(length(price)/2,0)+K):length(price)])
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
      #points(price[najboljsi_K], profit[najboljsi_K],type = "p", col = "green")
      
      strike_price = round(price[najboljsi_K],3)
      premija = abs(round(profit[najboljsi_K],3))
      kolicina = abs(round(premica2$coefficients[2],3))
      komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), " EUR/MWh,", " za količino ", as.character(kolicina), " MWh", " in za premijo ", as.character(premija), " EUR", ".",sep="")
      #print(komentar)
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
      tip_opcije <- paste("Gre za prodajo put opcije.")
      #print(tip_opcije)
      
      #poiscimo optimalni fit
      odstopanje1 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z linearno regresijo (posevni del)
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
      #points(price[najboljsi_K], profit[najboljsi_K],type = "p", col = "green")
      
      strike_price = round(price[najboljsi_K],3)
      premija = abs(round(profit[najboljsi_K],3))
      kolicina = abs(round(premica2$coefficients[2],3))
      komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), " EUR/MWh,", " za količino ", as.character(kolicina), " MWh", " in za premijo ", as.character(premija), " EUR", ".",sep="")
      #print(komentar)
      
    }
    
    #NAKUP CALL OPCIJE__________________________________________________________
    if (er2 > er1){
      tip_opcije <- paste("Gre za nakup call opcije.")
      #print(tip_opcije)
      
      odstopanje1 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z vodoravno premico
      odstopanje2 <- rep(0,round(length(price)/2,0)) #odstopanje pri aproksimaciji z linearno regresijo (posevni del)
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
        
        
        #POsEVNA PREMICA
        premica2 <- lm(profit[(round(length(price)/2,0)+K):length(price)] ~ price[(round(length(price)/2,0)+K):length(price)])
        odstopanje2[K] <- deviance(premica2)
        odstopanje2[K]
        
      }
      
      odstopanja <- odstopanje1 + odstopanje2
      najboljsi_K <- which(min(odstopanja) == odstopanja)
      premica1 <- profit[najboljsi_K]
      abline(h = profit[najboljsi_K], col = 'red', lwd=2)
      premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
      abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue', lwd=2)
      #points(price[najboljsi_K], profit[najboljsi_K],type = "p", col = "green")
      
      
      strike_price = round(price[najboljsi_K],3)
      premija = abs(round(profit[najboljsi_K],3))
      kolicina = abs(round(premica2$coefficients[2],3))
      komentar <- paste("Približek za izvršilno ceno opcije je ", as.character(strike_price), " EUR/MWh,", " za količino ", as.character(kolicina), " MWh", " in za premijo ", as.character(premija), " EUR", ".",sep="")
      #print(komentar)
    }
  }
  
  komentarji <- c(tip_opcije,komentar)
  return(komentarji)
}


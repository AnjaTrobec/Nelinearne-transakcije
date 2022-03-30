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
#nariši graf
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Cena vs Profit",
     pch = 1, cex=1.5)
abline(h = 0, lty='dashed') #meja med poz in neg profiti
#________________________________________________________________________________
# VALUE OF EU PUT OPTION
putV <- function(S,K) pmax(0,K-S)
putPayoff <- function(S,K,premium) pmax(0-abs(premium),K-S-abs(premium))

# VALUE OF EU PUT OPTION
callV <- function(S,K) pmax(0,S-K)
callPayoff <- function(S,K,premium) pmax(0-abs(premium),S-K-abs(premium))
#________________________________________________________________________________

tip_opcije = 0
#LEGENDA:
#nakup call = 1
#prodaja call = 2
#nakup put = 3
#prodaja put = 4

if (cor(price, profit) < 0){
    #nakup put opcije (3) ali prodaja nakupne opcije (2)
}

if (cor(price, profit) > 0){
    #nakup call opcije (1) ali prodaja put opcije (4)
}


# #1. MOŽNOST: 2 linearni regresiji
# 
# poisci_fit <- function(price, profit) {
#   povprecna_napaka <- function(sm) 
#     mean(sm$residuals^2)
#   
#   odstopanje1 <- rep(10,length(price)-4)
#   odstopanje2 <- rep(10,length(price)-4)
#   tekoci_min = 10^10
#   
#   for (K in 3:(length(price))-2){
#     premica1 <- lm(profit[1:K] ~ price[1:K])
#     abline(premica1$coefficients[1],premica1$coefficients[2], col = 'green')
#     odstopanje1[K-2] <- povprecna_napaka(premica1)
#     
#     premica2 <- lm(profit[K:length(price)] ~ price[K:length(price)])
#     abline(premica2$coefficients[1],premica2$coefficients[2], col = 'pink')
#     odstopanje2[K-2] <- povprecna_napaka(premica2)
#     
#     odstopanja <- odstopanje1 + odstopanje2
#     
#     if (min(odstopanja) < tekoci_min){
#       tekoci_min = min(odstopanja)
#       najboljsi_K = K
#     }
#   }
#   
#   najboljsi_K
#   
#   #narisi najboljsi K 
#   premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
#   #abline(premica1$coefficients[1],premica1$coefficients[2], col = 'red')
#   premica2 <- lm(profit[najboljsi_K+1:length(price)] ~ price[najboljsi_K+1:length(price)])
#   #abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')
# }
# 

#________________________________________________________________________________
# 2. MOŽNOST - ena linearna regresija in ena premica

najboljsi_K = rep(0,length(price))

nakup_put_opcije <- function(price, profit) {
  
  #funkcija za racunanje povprecne napake pri linearni regresiji
  povprecna_napaka <- function(sm){mean(sm$residuals^2)}
  
  odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
  odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
  odstopanja <- rep(0,length(price)) #skupno odstopanje


  for (K in 1:(length(price))){
    
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
    
    premica2 <- lm(profit[K:length(price)] ~ price[K:length(price)])
    #abline(premica2$coefficients[1],premica2$coefficients[2], col = 'pink')
    odstopanje2[K] <- povprecna_napaka(premica2)
    
    odstopanje2[K]
    
    odstopanja[K] <- odstopanje1[K] + odstopanje2[K]
    odstopanja[K]
  }
}

najboljsi_K <- which(min(odstopanja)==odstopanja)
premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
abline(h = profit[najboljsi_K], col = 'red')
premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')

nakup_put_opcije(price,profit)
najboljsi_K


#________________________________________________________________________________
#ideja - narišemo graf in pogledamo kateri premici se najbolj prilega
# NAKUP PUT OPCIJE (long put)
# V profitu bomo, kadar bo cena na trgu nižja od naše izvršilne cene. 
# Èe cena na trgu preseže izvršilno ceno, smo v izgubi. Najveè kar lahko izgubimo 
# je plaèana premija v primeru, da opcije ne izvršimo. 
# Zašèita pred padcem cene! Ker èe cena pade imamo še vedno dogovor, da lahko prodamo
# opcijo po K!

najboljsi_K = rep(0,length(price))

nakup_put_opcije <- function(price, profit) {
  
  #funkcija za racunanje povprecne napake pri linearni regresiji
  povprecna_napaka <- function(sm){mean(sm$residuals^2)}
  
  odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
  odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
  odstopanja <- rep(0,length(price)) #skupno odstopanje
  
  
  for (K in 1:(length(price))){
    
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
    
    premica2 <- lm(profit[K:length(price)] ~ price[K:length(price)])
    #abline(premica2$coefficients[1],premica2$coefficients[2], col = 'pink')
    odstopanje2[K] <- povprecna_napaka(premica2)
    
    odstopanje2[K]
    
    odstopanja[K] <- odstopanje1[K] + odstopanje2[K]
    odstopanja[K]
  }
}

najboljsi_K <- which(min(odstopanja)==odstopanja)
premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
abline(h = profit[najboljsi_K], col = 'red')
premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')

nakup_put_opcije(price,profit)
najboljsi_K

#________________________________________________________________________________

# PRODAJA PUT OPCIJE (short put)



# NAKUP CALL OPCIJE (long call)

najboljsi_K = rep(0,length(price))

nakup_put_opcije <- function(price, profit) {
  
  #funkcija za racunanje povprecne napake pri linearni regresiji
  povprecna_napaka <- function(sm){mean(sm$residuals^2)}
  
  odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
  odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
  odstopanja <- rep(0,length(price)) #skupno odstopanje
  
  
  for (K in 1:(length(price))){
    
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
    
    premica2 <- lm(profit[K:length(price)] ~ price[K:length(price)])
    #abline(premica2$coefficients[1],premica2$coefficients[2], col = 'pink')
    odstopanje2[K] <- povprecna_napaka(premica2)
    
    odstopanje2[K]
    
    odstopanja[K] <- odstopanje1[K] + odstopanje2[K]
    odstopanja[K]
  }
}

najboljsi_K <- which(min(odstopanja)==odstopanja)
premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
abline(h = profit[najboljsi_K], col = 'red')
premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
abline(premica2$coefficients[1],premica2$coefficients[2], col = 'dark blue')

nakup_put_opcije(price,profit)
najboljsi_K





# PRODAJA CALL OPCIJE (short call)





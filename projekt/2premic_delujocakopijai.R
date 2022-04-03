# PRISTOP REŠEVANJA Z RISANJEM DVEH PREMIC
getwd()
setwd("C:/Users/aanja/OneDrive/Dokumenti/fmf/magisterij/matematika z racunalnikom/Nelinearne-transakcije/projekt")
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

#________________________________________________________________________________
# Ena premica dobljena z linearno regresijo in druga premica je vodoravna

# test = rep(0,length(price))
#funkcija za racunanje povprecne napake pri linearni regresiji

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

opt_fit(price,profit)
#________________________________________________________________________________
#ideja - narišemo graf in pogledamo kateri opciji se najbolj prilega
# NAKUP PUT OPCIJE



# # 2. možnost: vzamemo oddaljenost napake 1. premice do vseh toèk v ravnini!
# # nakup_put_opcije <- function(price, profit) {
#   
#   #funkcija za racunanje povprecne napake pri linearni regresiji
# povprecna_napaka <- function(sm){mean(sm$residuals^2)}
# odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
# odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
# odstopanja <- rep(0,length(price)) #skupno odstopanje
# 
# 
# for (K in 1:(length(price))){
#   
#   #VODORAVNA PREMICA
#   premica1 <- profit[K]
#   #abline(h = premica1, col = 'green')
#   
#   pomozni_seznam = rep(0,length(profit))
#   for (i in 1:length(profit)){
#     pomozni_seznam[i] = sqrt(sum((premica1 - profit[i])^2))
#   }
#   for (j in 1:length(pomozni_seznam)){
#     odstopanje1[K] = odstopanje1[K] + pomozni_seznam[j]
#   }
#   
#   odstopanje1[K]
#   
#   premica2 <- lm(profit[K:length(price)] ~ price[K:length(price)])
#   #abline(premica2$coefficients[1],premica2$coefficients[2], col = 'pink')
#   odstopanje2[K] <- povprecna_napaka(premica2)
#   
#   odstopanje2[K]
#   
#   odstopanja[K] <- odstopanje1[K] + odstopanje2[K]
#   odstopanja[K]
# }
# # }
# 
# najboljsi_K <- which(min(odstopanja)==odstopanja)
# premica1 <- lm(profit[1:najboljsi_K] ~ price[1:najboljsi_K])
# abline(h = profit[najboljsi_K], col = 'violet')
# premica2 <- lm(profit[najboljsi_K:length(price)] ~ price[najboljsi_K:length(price)])
# abline(premica2$coefficients[1],premica2$coefficients[2], col = 'blue')
# 
# # nakup_put_opcije(price,profit)
# najboljsi_K

#________________________________________________________________________________

# PRODAJA PUT OPCIJE (short put)



# NAKUP CALL OPCIJE (long call)



# PRODAJA CALL OPCIJE (short call)





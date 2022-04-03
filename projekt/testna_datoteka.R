odstopanje1 <- rep(0,length(price)) #odstopanje pri aproksimaciji z vodoravno premico
odstopanje2 <- rep(0,length(price)) #odstopanje pri aproksimaciji z linearno regresijo (poševni del)
odstopanja <- rep(0,length(price)) #skupno odstopanje
najboljsi_K = 0
range01 <- function(x){(x-min(x))/(max(x)-min(x))}


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


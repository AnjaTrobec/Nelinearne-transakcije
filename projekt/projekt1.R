#GEN-I Nelinearne transakcije

#_______________________________________________________________________________
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
#plot data

plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     main = "Cena vs Profit")

# #________________________________________________________________________________
#definiraj funkciji za vrednost opcije

# VALUE OF EU PUT OPTION
putV <- function(S,K) pmax(0,K-S)
putPayoff <- function(S,K,premium) pmax(0-abs(premium),K-S-abs(premium))

# VALUE OF EU PUT OPTION
callV <- function(S,K) pmax(0,S-K)
callPayoff <- function(S,K,premium) pmax(0-abs(premium),S-K-abs(premium))

#________________________________________________________________________________
# PREMIJA
profit1 <-  rep(0,length(profit))
iscem_premijo <- 0
stevec_negativnih <- 0

#vektor negativnih dobièkov
for (i in 1:length(profit)){
  if (profit[i] < 0){
    profit1[i] <- profit[i]
    stevec_negativnih <- stevec_negativnih + 1}
}
profit1 <- sort(profit1[ profit1 != 0 ])
st <- round(stevec_negativnih*0.1,0) #odrežem zgornjih in spodnjih 10 %
profit1 <- profit1[st:length(profit1)-st]
premija <- mean(profit1)
premija

# plot(x = price,y = profit,xlab = "Cena (v €)",ylab = "Profit (v tisoè €)",xlim = c(200,225),ylim = c(-25,25),main = "Cena vs Profit")

#________________________________________________________________________________
# STRIKE PRICE
# K = Vt + St + premik_za_premijo
okoli_nic = rep(0, length(profit))
for (i in 1:length(profit)){
  if (profit[i] < 1 & profit[i] > -1){
    okoli_nic[i] <- profit[i]}
}

kandidati <- which(profit == okoli_nic)
kandidati_nicle <- rep(0,length(kandidati))

for (i in 1:length(kandidati)){
  kandidati_nicle[i] <- price[kandidati[i]]
}
kandidati_nicle


nicla = mean(kandidati_nicle)
nicla

#nekaj je razdalja med nièlo in K
nekaj = 5

K = nicla + nekaj
#_________________________________________________________________________________
#par(mfrow=c(1,2))
plot(x = price,
     y = profit,
     xlab = "Cena (v €)",
     ylab = "Profit (v tisoè €)",
     xlim = c(185,240),
     ylim = c(-25,150),
     main = "Cena vs Profit")
abline(h=0, col='black', lwd=1)
abline(v=K, col='blue', lwd=1)
lines(price, putPayoff(price,K,premija), col='red')
legend(x = 'topright', legend = c('Podatki', 'EU PUT', 'Strike price'), col=c('black','red','blue'), lwd=2)
#lines(price, vrednost-premija)













# NAPAÈNA SMER
#________________________________________________________________________________
# # nekaj nepotrebnih izraèunov
# average_price <- mean(as.numeric(gsub(",",".",df$Price)))
# average_price
# 
# volatility_price <- sd(as.numeric(gsub(",",".",df$Price)))
# volatility_price
# 
# average_profit <- mean(as.numeric(gsub(",",".",df$Profit)))
# average_profit
#________________________________________________________________________________
#VREDNOST - PREMIJA = PROFIT

#strike price = average_price +- volatility
# K <- average_price + volatility_price/2
# 
# vrednost <- c(1:192)
# for (i in 1:length(price)){
#   vrednost[i] <- max(K-price[i],0)
# }
# 
# mozna_premija <- c(1,192)
# for (i in 1:length(price)){
#   mozna_premija[i] <- vrednost[i]-profit[i]
# }
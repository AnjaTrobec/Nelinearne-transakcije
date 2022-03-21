# GEN-I PROJEKT

#getwd()
#setwd("C:/Users/aanja/OneDrive/Dokumenti/fmf/magisterski študij/matematika z raèunalnikom/Nelinearne-transakcije/projekt")

# read data
df <- read.csv("podatki.csv", sep=';', header = FALSE, stringsAsFactors=FALSE)
colnames(df) <- c('Price','Profit')
df


#plot data
plot(x = as.numeric(gsub(",", ".", df$Price)),
     y = as.numeric(gsub(",", ".", df$Profit)),
     xlab = "Price",
     ylab = "Profit",
     main = "Price vs Profit")


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


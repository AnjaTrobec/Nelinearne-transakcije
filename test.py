import pandas as pd
import matplotlib.pyplot as plt

# Read excel file
headers = ['Price', 'Profit']
df = pd.read_csv(r'C:\Users\aanja\OneDrive\Dokumenti\fmf\magisterski študij\matematika z računalnikom\Nelinearne-transakcije\projekt\podatki.csv', sep=';', names=headers)
print(df)

# Plot
plt.scatter(df['Price'], df['Profit']) 
plt.show()

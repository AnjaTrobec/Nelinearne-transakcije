

# Projektna naloga GEN-I
import csv

with open('GEN-I podatki.csv', 'r', newline='') as file:
    reader = csv.reader(file, dialect='excel')
    for row in reader:
        print(row)






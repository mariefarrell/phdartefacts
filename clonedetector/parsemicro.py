from tabulate import tabulate
from collections import Counter
import sys
import csv

out1 = open("/home/marie/eventclones/Output/microEventanonSummary.csv")
csv_out = csv.reader(out1)

typelist = []
projlist = []
types = []
fullnames = []
fullnameslist = []
macnames = []
macnameslist = []

for row in csv_out:
  for i in range(0, len(row)):
    name = (((row[i])[5:]).split('/'))[1]
    fullname = ((row[i])[5:])
    fullnames.append(fullname)
    macnames.append(fullname.split('/')[2])
    if name in projlist:
      types.append('intra')
    elif name not in projlist and len(projlist) > 0:
      types.append('inter')
    projlist.append(name)
  typelist.append(types)
  fullnameslist.append(fullnames)
  macnameslist.append(macnames)
  macnames = []
  fullnames=[]
  types = []

inter = 0
intra = 0
for i in typelist:
  if 'intra' in i:
    intra = intra + 1
  if 'inter' in i: 
    inter = inter + 1

samemacclone = 0
for y in macnameslist:
  for m in set(y):
    if y.count(m) > 1:
     samemacclone = samemacclone + y.count(m) #-1 to get rid of the initial one    

for x in zip(typelist, fullnameslist):
  print(x)

print("There are "+ str(samemacclone)+ " same mac clones.")
print("There are " + str(intra) + " intra project clones")
print("There are " + str(inter)+ " inter project clones")

    

from tabulate import tabulate
import sys
import csv
import matplotlib as mpl 
import numpy as np
from operator import itemgetter
## agg backend is used to create plot as a .png file
mpl.use('agg')

import matplotlib.pyplot as plt 

metrics = open("metrics.csv")
csvmet = csv.reader(metrics)
projlist = []
reflist =[]
senlist = []
prflist = []
vlist = []


for row in csvmet:
  if row[0] == 'M':
    if row[1] == 'BIG' or row[1] == 'SMALL' or row[1] == 'DIR':
      st1 = (row[2].split('/'))[0] 
      projlist.append('M'+st1)
      senlist.append((st1,row[12]))
      vlist.append((st1, row[7]))
      if row[4] != '0':
        reflist.append(st1)
    elif row[1].startswith('BIG/')  or row[1].startswith('SMALL/')or row[1].startswith('DIR/'):
      st2 = ((row[1])[4:].split('/'))[0]
      vlist.append((st2, row[6]))
      projlist.append('M'+st2)
      if row[4] != '0':
        reflist.append(st2)
      senlist.append((st2,row[12]))
  elif row[0] == 'C':
    if row[1] == 'BIG' or row[1] == 'SMALL' or row[1] == 'DIR':
      st1 = (row[2].split('/'))[0] 
      projlist.append('C'+st1)
      senlist.append((st1,row[10]))
    elif row[1].startswith('BIG/')  or row[1].startswith('SMALL/')or row[1].startswith('DIR/'):
      st2 = ((row[1])[4:].split('/'))[0]
      projlist.append('C'+st2)
      senlist.append((st2,row[10]))
  elif row[0] == 'E':
    if row[1] == 'BIG' or row[1] == 'SMALL' or row[1] == 'DIR':
      st1 = (row[2].split('/'))[0] 
      projlist.append('E'+st1)
      senlist.append((st1,row[11]))
    elif row[1].startswith('BIG/') or row[1].startswith('SMALL/')or row[1].startswith('DIR/'):
      st2 = ((row[1])[4:].split('/'))[0]
      projlist.append('E'+st2)
      senlist.append((st2,row[11]))
  elif row[0] == 'P':
    if row[1] == 'BIG' or row[1] == 'SMALL' or row[1] == 'DIR':
      st1 = (row[2].split('/'))[0] 
      prflist.append((st1, row[3], row [4], row[5]))
    elif rrow[1].startswith('BIG/')  or row[1].startswith('SMALL/') or row[1].startswith('DIR/'):
      st2 = ((row[1])[4:].split('/'))[0]
      prflist.append((st1, row[4], row [5], row[6]))

nodoubles = []
analysis = []
abrialBook = []
mx=0


for i in projlist:
  if i[1:] not in nodoubles:
    sens = 0
    autop = 0
    intp = 0
    refp = 0
    variables = 0
    macnum = projlist.count('M'+i[1:])
    connum = projlist.count('C'+i[1:])
    evnum = projlist.count('E'+i[1:])
    refnum = reflist.count(i[1:])
    for (a,b,c,d) in prflist:
      if a == i[1:]:
        autop = autop + int(b)
        intp= intp+ int(c) 
        refp = refp + int(d)
    for (x,y) in senlist:
      if x == i[1:]:
        sens = sens + int(y)
      if i[1:].startswith('ch'):
        if sens >= mx:
          mx = sens
    for (x,y) in vlist:
      if x == i[1:]:
        variables = variables + int(y)
      
    avgref = 0
    if refnum >0: 
       avgref = int((autop + intp )/refnum)
    nodoubles.append(i[1:])
    analysis.append([i[1:],macnum,connum,evnum,refnum,sens,autop, intp, avgref, refp, variables])

smallanalysis = []
biganalysis = []

for i in analysis:
  if i[5] <= 939: 
    smallanalysis.append(i)
  else:
    biganalysis.append(i)

sorted(biganalysis, key=itemgetter(5),  reverse=True)

print (tabulate(sorted(smallanalysis, key=itemgetter(5),  reverse=True), headers=['Name', 'Machines', 'Contexts','Events', 'Refines', 'Sentences','Automatic Proofs', 'Interactive Proofs','Total Proofs / Refines', 'Refinement Proofs', 'Variables'],tablefmt='latex'))
print ("There are "+str(len(smallanalysis))+ " in this table. ")
print (tabulate(sorted(biganalysis,key=itemgetter(5), reverse=True), headers=['Name', 'Machines', 'Contexts','Events', 'Refines', 'Sentences','Automatic Proofs', 'Interactive Proofs','Total Proofs / Refines', 'Refinement Proofs', 'Variables'],tablefmt='latex'))
print ("There are "+str(len(biganalysis))+ " in this table. ")

evs = []
spos = []
ssenl = []
srefl = []

for i in smallanalysis:
  evs.append(i[3])
  spos.append(i[6]+i[7])
  ssenl.append(i[5])
  srefl.append(i[4])
'''
plt.scatter(ssenl,spos)
plt.plot(np.unique(ssenl), np.poly1d(np.polyfit(ssenl, spos, 1))(np.unique(ssenl)))
plt.xlabel('number of sentences')
plt.ylabel('number of proof obligations')
plt.title("POS and Sentences in Small Dataset")
plt.savefig('Output/smallposen.png')
plt.close()

plt.scatter(srefl,spos)
plt.plot(np.unique(srefl), np.poly1d(np.polyfit(srefl, spos, 1))(np.unique(srefl)))
plt.xlabel('number of refinement steps')
plt.ylabel('number of proof obligations')
plt.title("POS and Refinement Steps in Small Dataset")
plt.savefig('Output/smallporef.png')
plt.close()


plt.scatter(srefl, ssenl)
plt.plot(np.unique(srefl), np.poly1d(np.polyfit(srefl, ssenl, 1))(np.unique(srefl)))
plt.xlabel('number of sentences')
plt.ylabel('number of refinement steps')
plt.title("Sentences and Refinement Steps in Small Dataset")
plt.savefig('Output/smallrefsen.png')
plt.close()

bpos = []
bsenl = []
brefl = []
for i in biganalysis[1:]:
  evs.append(i[3])
  bpos.append(i[6]+i[7])
  bsenl.append(i[5])
  brefl.append(i[4])

plt.scatter(bsenl, bpos)
plt.plot(np.unique(bsenl), np.poly1d(np.polyfit(bsenl, bpos, 1))(np.unique(bsenl)))
plt.xlabel('number of sentences')
plt.ylabel('number of proof obligations')
plt.title("POS and Sentences in Big Dataset")
plt.savefig('Output/bigposen.png')
plt.close()

plt.scatter(brefl, bpos)
plt.plot(np.unique(brefl), np.poly1d(np.polyfit(brefl, bpos, 1))(np.unique(brefl)))
plt.xlabel('number of refinement steps')
plt.ylabel('number of proof obligations')
plt.title("POS and Refinement Steps in Big Dataset")
plt.savefig('Output/bigporef.png')
plt.close()

plt.scatter(brefl, bsenl)
plt.plot(np.unique(brefl), np.poly1d(np.polyfit(brefl, bsenl, 1))(np.unique(brefl)))
plt.xlabel('number of sentences')
plt.ylabel('number of refinement steps')
plt.title("Sentences and Refinement Steps in Big Dataset")
plt.savefig('Output/bigrefsen.png')
plt.close()

macs = []
for m in analysis:
  macs.append(i[1])

cons = []
print("all")
for c in analysis:
  cons.append(i[2])
  print (c)

print("small")
for s in smallanalysis:
  print(s)

print("big")
for s in biganalysis:
  print(s)



#boxplot
fig1 = plt.figure(1,  figsize = ( 9, 5 ))
ax2 = fig1.add_subplot(111)
bp1 = ax2.boxplot([macs])
ax2.set_xticklabels(['Machines'])
fig1.savefig('Output/boxplot.png', bbox_inches='tight')
'''

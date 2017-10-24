from tabulate import tabulate
import sys
import csv
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from scipy import stats

def clonecount(inFile, comp):
  out1 = open(inFile)
  t1count = 0 
  t15count = 0
  t2count = 0
  t3count = 0
  pairscount = 0
  csv_out = csv.reader(out1)
  t3list = []

  for row in csv_out:
    if (row[5] == '100') and (row[6] == '100') and (row[7] == '100') and (row[9] == '100') and (row[10] == '100') and (row[11] == '100'):
    # print (row[0]+" is a type-1 clone of " + row[2] + " using longest common substring")
     t1count = t1count + 1
    elif (row[9] == '100') and (row[10] == '100') and (row[11] == '100'):
    # print (row[0]+" is a type-2 clone of " + row[2] + " using longest common subsequence")
     t2count = t2count + 1  
    elif ((row[7]) != '0') or ((row[11]) != '0'):
    # print (row[0]+" is a type-3 clone of " + row[2] + " using longest common substring")
     t3count = t3count + 1  
     t3list.append(max(float(row[7]), float(row[11])))
    pairscount = pairscount +1
  print("There are " + str(pairscount) + " pairs in the dataset.")
  print("There are " + str(t1count) +" Type-1 clones in the dataset. ")
  print("There are " + str(t2count) +" Type-2 clones in the dataset. ")
  print("There are " + str(t3count) +" Type-3 clones in the dataset. ")
  
  counter=[]
  for l in t3list:
    counter.append(t3list.count(l))

  density = stats.kde.gaussian_kde(t3list)
  x = np.arange(0., 8, .1)
 # plt.plot(x, density(x))
 # plt.hist(t3list, histtype = 'bar', align ='left')
 # plt.show()
 # x = np.random.normal(size=100)
  sns.set_style("white")
  sns.distplot(t3list);
  plt.ylabel('Density', fontsize = 20)
  plt.xlabel('Clone similarity percentage', fontsize = 20)
  plt.title(comp, fontsize = 22)
 # if('Machines' in comp):
  #  plt.ylim([0,225])
  #elif('Events' in comp):
   # plt.ylim([0,1600])
  plt.savefig('Output/'+comp+'.pdf')
  plt.close()
  
def metricsSummary(inFile):
  out2 = open(inFile)
  csv_out2 = csv.reader(out2)
  events = 0
  macs = 0 
  ctxs = 0

  for row in csv_out2:
    if row[0] == 'E':
      events = events + 1
    elif row[0] == 'C':
      ctxs = ctxs + 1
    elif row[0] == 'M':
      macs = macs + 1
  print("There are " + str(macs) + " machines in the dataset")
  print("There are " + str(ctxs) + " contexts in the dataset")
  print("There are " + str(events) + " events in the dataset")


def analyse_micro(inFile):
  out3 = open(inFile)
  csv_out3 = csv.reader(out3)
  
  micpat  =[]
  for row in csv_out3:
    micpat.append((row[0], row[1], row[2], row[3]))
  
  print(tabulate(micpat,headers=['# occurrences', '# sentences', 'micropattern','where'],tablefmt='latex'))

#### Output is generated below ####
sys.stdout = open("Output/FullSummary.txt", "w")

print("#### Full Dataset####")
print("#### LCSCompare on Machines: ####")
clonecount("Output/DIR/lcs_compareMachines.csv" , 'Machines')
print()

print("#### LCSCompare on Machines with Invariants: ####")
clonecount("Output/DIR/lcs_compareMachineswithInvariants.csv" , 'Machines with variants and invariants')
print()

print("#### LCSCompare on Contexts: ####")
clonecount("Output/DIR/lcs_compareContexts.csv", 'Contexts')
print()

print("#### LCSCompare on Events: ####")
clonecount("Output/DIR/lcs_compareEvents.csv", 'Events')
print()

print("#### LCSCompare on Events with Invariants: ####")
clonecount("Output/DIR/lcs_compareEventswithInvariants.csv", 'Events with variants and invariants')
print()

print("##### Metrics #####")
metricsSummary("Output/DIR/metrics.csv")
print()

print("#### BIG Projects: ####")
print("#### LCSCompare on Machines: ####")
clonecount("Output/BIG/lcs_compareMachines.csv" , 'BIGMachines')
print()

print("#### LCSCompare on Machines with Invariants: ####")
clonecount("Output/BIG/lcs_compareMachineswithInvariants.csv" , 'BIGMachines with Invariants')
print()

print("#### LCSCompare on Contexts: ####")
clonecount("Output/BIG/lcs_compareContexts.csv", 'BIGContexts')
print()

print("#### LCSCompare on Events: ####")
clonecount("Output/BIG/lcs_compareEvents.csv", 'BIGEvents')
print()

print("#### LCSCompare on Events with Invariants: ####")
clonecount("Output/BIG/lcs_compareEventswithInvariants.csv", 'BIGEvents with Invariants')
print()

print("##### Metrics #####")
metricsSummary("Output/BIG/metrics.csv")
print()


print("#### SMALL Projects: ####")
print("#### LCSCompare on Machines: ####")
clonecount("Output/SMALL/lcs_compareMachines.csv" , 'SMALLMachines')
print()

print("#### LCSCompare on Machines with Invariants: ####")
clonecount("Output/SMALL/lcs_compareMachineswithInvariants.csv" , 'SMALLMachines with Invariants')
print()

print("#### LCSCompare on Contexts: ####")
clonecount("Output/SMALL/lcs_compareContexts.csv", 'SMALLContexts')
print()

print("#### LCSCompare on Events: ####")
clonecount("Output/SMALL/lcs_compareEvents.csv", 'SMALLEvents')
print()

print("#### LCSCompare on Events with Invariants: ####")
clonecount("Output/SMALL/lcs_compareEventswithInvariants.csv", 'SMALLEvents with Invariants')
print()

print("##### Metrics #####")
metricsSummary("Output/SMALL/metrics.csv")
print()


print("#### Micropatterns on Contexts ####")
#analyse_micro("Output/micropatternsContextAnon.csv")# anonymous on contexts
print()

print("#### Anonymous Micropatterns on Machines ####")
#analyse_micro("Output/micropatternsAnon.csv")
print()

print("#### Micropatterns on Machines withInvariants ####")
#analyse_micro("Output/micropatternsInv.csv") #not anonymous on machines
print()

print("#### Micropatterns on Events ####")
#analyse_micro("Output/micropatternsEvent.csv")#not anonymous
print()

print("#### Anonymous Micropatterns on Events ####")
#analyse_micro("Output/micropatternsEventAnon.csv")
print()

print("#### Anonymous Micropatterns on Events with Invariants ####")
#analyse_micro("Output/micropatternsEventAnonInv.csv")
print()

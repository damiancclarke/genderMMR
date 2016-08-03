import numpy as np
import pylab as plt

data = open('quotaTimes.csv','r')
quotas = [[],[],[],[],[],[],[]]
for line in data:
    code = line.split(",")[0]
    cuml = line.split(",")[2]
    cuml = cuml.replace('\n','')
    if cuml!="qCountries":
        cuml = float(cuml)
    if code=='1':
        quotas[0].append(cuml)
    if code=='2':
        quotas[1].append(cuml)
    if code=='3':
        quotas[2].append(cuml)
    if code=='4':
        quotas[3].append(cuml)
    if code=='6':
        quotas[4].append(cuml)
    if code=='7':
        quotas[5].append(cuml)


years = range(1989,2013)
print quotas

plot = plt.stackplot(years,quotas[0],quotas[1],quotas[2],quotas[3],quotas[4],quotas[5])
plt.legend(["East Asia & Pacific","Europe & Central Asia","Latin America","Middle East & N Africa","South Asia","Sub-Saharan Africa"], loc='upper left')
plt.xlim(1989,2012)
plt.ylabel('Total Number of Countries with a Gender Quota')
plt.xlabel('Year')

    
plt.savefig('quotas/quotasStacked.png')
plt.savefig('quotas/quotasStacked.eps',bbox_inches='tight')

plt.gcf().clear()


#Reserved Only
data = open('quotaTimesRes.csv','r')
quotas = [[],[],[],[],[],[]]
for line in data:
    code = line.split(",")[0]
    cuml = line.split(",")[2]
    cuml = cuml.replace('\n','')
    if cuml!="qCountries":
        cuml = float(cuml)
    if code=='1':
        quotas[0].append(cuml)
    if code=='3':
        quotas[1].append(cuml)
    if code=='4':
        quotas[2].append(cuml)
    if code=='6':
        quotas[3].append(cuml)
    if code=='7':
        quotas[4].append(cuml)

print quotas

years = range(1989,2013)

plot = plt.stackplot(years,quotas[0],quotas[1],quotas[2],quotas[3],quotas[4])
plt.legend(["East Asia & Pacific","Latin America","Middle East & N Africa","South Asia","Sub-Saharan Africa"], loc='upper left')
plt.xlim(1989,2012)
plt.ylabel('Total Number of Countries with a Gender Quota')
plt.xlabel('Year')

    
plt.savefig('quotas/quotasStackedRes.eps',bbox_inches='tight')



plt.gcf().clear()


#Candidates Only
data = open('quotaTimesCan.csv','r')
quotas = [[],[],[],[],[],[]]
for line in data:
    code = line.split(",")[0]
    cuml = line.split(",")[2]
    cuml = cuml.replace('\n','')
    if cuml!="qCountries":
        cuml = float(cuml)
    if code=='1':
        quotas[0].append(cuml)
    if code=='2':
        quotas[1].append(cuml)
    if code=='3':
        quotas[2].append(cuml)
    if code=='4':
        quotas[3].append(cuml)
    if code=='6':
        quotas[4].append(cuml)
    if code=='7':
        quotas[5].append(cuml)

print quotas

years = range(1989,2013)

plot = plt.stackplot(years,quotas[0],quotas[1],quotas[2],quotas[3],quotas[4],quotas[5])
plt.legend(["East Asia & Pacific","Europe & Central Asia","Latin America","Middle East & N Africa","South Asia","Sub-Saharan Africa"], loc='upper left')
plt.xlim(1989,2012)
plt.ylabel('Total Number of Countries with a Gender Quota')
plt.xlabel('Year')

    
plt.savefig('quotas/quotasStackedCan.eps',bbox_inches='tight')


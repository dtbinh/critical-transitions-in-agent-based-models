# ------------------------------------------------------------
# Kyle Scot Shank
# College of the Atlantic - 2014
#
#
# This is the Python code utilized in my senior project:
# "Evaluating Methods for Detecting Critical Transitions in Agent Based Models"
#
#
# ------------------------------------------------------------


# ------------------------------------------------------------
# PREAMBLE
# ------------------------------------------------------------

from math import *
from numpy import *
from matplotlib import mlab
from scipy import integrate
import numpy as np
import pylab as p
import random

# ------------------------------------------------------------
# MODEL
#
# Modeled on Boerlijst et al. 2013
#
# Juv. Prey Density  = dJ/dt = f(A) - g(J) - uJ*J
# Adult Prey Density = dA/dt = g(J) - h(A,P) - uA*A
# Predators Density  = dP/dt = h(A,P)*c - uP*P
#
# f(A) is a function that specifies reproduction rates of Adult Prey
# and is realized as f(A) = bA, with b representing the adult
# reproduction rate. Fecundity here is modeled as a linear process.
# 
# g(J) is a function that specifies the maturation rate of Juv. Prey
# and is realized as g(J) = J/(1+J**2). Maturation here is modeled as
# a non-linear process (thus giving rise to chaotic population trajectories).
#
# h(A,P) is the predation rate on adults and is realized as h(A,P) = A*P
# 
# uJ,uA,uP are death rates, c is a conversion factor
#
# X = [J,A,P] (a vector containing population densities of
# juvenile prey, adult prey, and predators)
# ------------------------------------------------------------

# ------------------------------------------------------------
# MODEL PARAMETERS
# ------------------------------------------------------------

b = 1        # Adult reproduction rate, citation has b = 1
c = 1        # Conversion factor, citation has c = 1
uJ = 0.05    # Mortality rate of Juveniles, citation has uJ = 0.05
uA = 0.1     # Morality rate of Adults, citation has uA = 0.1
uP = 0.1     # Mortality rate of Predators. There are bifurcation points at
             # uP = 0.553 and uP = 0.435.
             
uPpn = 0.100 # uP value for pink noise ODE
m = 0        # Will add to uP in increments of 0.001
mu = 0       # Will add to uP in increments of 0.001 (white noise)
meow = 0     # Will add to uP in increments of 0.001 (pink noise)

# ------------------------------------------------------------
# MODEL SPECIFICATIONS  
# ------------------------------------------------------------

def dX_dt(X,t=0): 
    """
    X is the vector of population densities:
    J(uveniles) X[0],
    A(dults) X[1],
    P(predators) X[2]
    """
    return  [ b*X[1] - (X[0]/(1 + X[0]**2)) - (uJ)*X[0] ,
                    (X[0]/(1 + X[0]**2)) - (X[1]*X[2]) - (uA)*X[1],
                    (X[1]*X[2])*c - ((uP + m)*X[2])]


def dX_dt_WN(X,t=0):
    """
    X is the vector of population densities:
    J(uveniles) X[0],
    A(dults) X[1],
    P(predators) X[2]
    """
    return  [ b*X[1] - (X[0]/(1 + X[0]**2)) - (uJ)*X[0] ,
                    (X[0]/(1 + X[0]**2)) - (X[1]*X[2]) - (uA)*X[1],
                    (X[1]*X[2])*c - ((uP + mu)*X[2])]

def dX_dt_PN(X,t=0):
    """
    X is the vector of population densities:
    J(uveniles) X[0],
    A(dults) X[1],
    P(predators) X[2]
    """
    return  [ b*X[1] - (X[0]/(1 + X[0]**2)) - (uJ)*X[0] ,
                    (X[0]/(1 + X[0]**2)) - (X[1]*X[2]) - (uA)*X[1],
                    (X[1]*X[2])*c - ((uPpn + meow)*X[2])]

# ------------------------------------------------------------
# Model Time and Step Size
# ------------------------------------------------------------

t = 1000
stepSize = 10

# ------------------------------------------------------------
# Make List of Pink (1/f) noise
# ------------------------------------------------------------
length  = np.linspace(0,t,t)
n = len(length)
frequencies = range(1,n+1)  


def noise(freq):
    phase = random.uniform(0,2*math.pi)
    return [math.sin(2*math.pi * freq*x/n + phase)
        for x in range(n)]

def weighted_sum(amplitudes, noises):
    output = [0.0] * n  
    for k in range(len(noises)):
        for x in range(n):
            output[x] += amplitudes[k] * noises[k][x]
    return output

def random_ift(rows, amplitude):
    for i in range(rows):
        amplitudes = [amplitude(f) for f in frequencies]
        noises = [noise(f) for f in frequencies]
        sum_of_noises = weighted_sum(amplitudes, noises)
        return sum_of_noises
        #print(i, sum_of_noises)

pn = random_ift(1, lambda f: 1/float(f))
makeTiny = 0.05
tinyPN = [x * makeTiny for x in pn]

# ------------------------------------------------------------
# Make List of White Noise
# ------------------------------------------------------------

wn = []
for i in range(n):
    wn.append(random.gauss(0,0.005))

# ------------------------------------------------------------
# Solve ODE over time (no Noise)
# ------------------------------------------------------------
ts = []
m_list = []
initialX = [1,1,1]

for i in range(t):
    if i < (t/10):
        m = 0
        m_list.extend([m]*10)
    else:
        m += 0.001
        m_list.extend([m]*10)
    time = np.linspace(0+i,1+i,stepSize)
    solutions = integrate.odeint(dX_dt,initialX,time)
    ts.extend(solutions)
    initialX = solutions[9]

## ------------------------------------------------------------
## Solve ODE over time (White Noise)
## ------------------------------------------------------------
tsWN = []
mu_list = [] 
initialXWN = [1,1,1]
for i in range(t):
    if i < (t/10):
        mu = 0
        mu_list.extend([mu] * 10)
    else:
        mu += 0.001 + wn[(i-t/10)]
        mu_list.extend([mu]* 10)
    time = np.linspace(0+i,1+i,stepSize)
    solutionsWN = integrate.odeint(dX_dt_WN,initialXWN,time)
    tsWN.extend(solutionsWN)
    initialXWN = solutionsWN[9]


## ------------------------------------------------------------
## Solve ODE over time (Pink noise)
## ------------------------------------------------------------
tsPN = []
meow_list = []
initialXPN = [1,1,1]
for i in range(t):
    if i < (t/10):
        meow = 0
        meow_list.extend([meow]*10)
    else:
        uPpn += 0.001
        meow = tinyPN[(i-t/10)]
        meow_list.extend([meow + uPpn]*10)
    time = np.linspace(0+i,1+i,stepSize)
    solutionsPN = integrate.odeint(dX_dt_PN,initialXPN,time,tcrit=[0])
    tsPN.extend(solutionsPN)
    initialXPN = solutionsPN[9]
        
# ------------------------------------------------------------
# Population List
# ------------------------------------------------------------

juv   = [i[0] for i in ts]
adult = [i[1] for i in ts]
pred  = [i[2] for i in ts]

juvWN   = [i[0] for i in tsWN]
adultWN = [i[1] for i in tsWN]
predWN  = [i[2] for i in tsWN]

juvPN   = [i[0] for i in tsPN]
adultPN = [i[1] for i in tsPN]
predPN  = [i[2] for i in tsPN]
# ------------------------------------------------------------
# PLOT FUNCTIONS
# ------------------------------------------------------------

time = np.linspace(0,t,t*stepSize)
pDash = [5,2,10,5]
jDash = [2,4,2,4]
aDash = [1,1,1,1]

f1 = p.figure()
f1.canvas.set_window_title('Population Density with increasing uP')
line1, = p.plot(time, juv, 'r-', label='Juveniles')
line1.set_dashes(jDash)
line2, = p.plot(time, adult  , 'b-', label='Adults')
line2.set_dashes(aDash)
line3, = p.plot(time, pred, 'g-', label='Predators')
line3.set_dashes(pDash)
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population Density')
p.title('Evolution of Population Density with Increasing Mortality')

f2 = p.figure()
f2.canvas.set_window_title('Population Density with (white noise) at uP = 0.55')
line1, = p.plot(time, juvWN, 'r-', label='Juveniles')
line1.set_dashes(jDash)
line2, = p.plot(time, adultWN  , 'b-', label='Adults')
line2.set_dashes(aDash)
line3, = p.plot(time, predWN, 'g-', label='Predators')
line3.set_dashes(pDash)
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population Density')
p.title('Evolution of Population Density')


f3 = p.figure()
f3.canvas.set_window_title('Population Density with increasing uP (pink noise)')
p.plot(time, juvPN, 'r-', label='Juveniles')
p.plot(time, adultPN  , 'b-', label='Adults')
p.plot(time, predPN, 'g-', label='Predators')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population Density')
p.title('Evolution of Population Density')

p.show()


# ------------------------------------------------------------
# OUTPUT FUNCTIONS
#
#
# Writes to output files (.csv) for analysis in R
# ------------------------------------------------------------

## ------------------------------------------------------------
## Write TS with no noise
## ------------------------------------------------------------
output_file = open("tsNoNoise.csv", "w")
data_str = "time" + ",\t" + "juv" + ",\t" + "adult" + ",\t" + "pred" + ",\t" + "uP\n"
output_file.write(data_str)
for i in range(len(juv)):
    data_str = str(i) + ",\t" + str(juv[i]) + ",\t" + str(adult[i]) + ",\t"
    data_str += str(pred[i]) + ",\t" + str(m_list[i]+uP) + "\n"
    output_file.write(data_str)
output_file.close()

# ------------------------------------------------------------
# Write TS with white noise
# ------------------------------------------------------------

output_file = open("tsWhiteNoise.csv", "w")
data_str = "time" + ",\t" + "juv" + ",\t" + "adult" + ",\t" + "pred" + ",\t" + "uP\n"
output_file.write(data_str)
for i in range(len(juvWN)):
    data_str = str(i) + ",\t" + str(juvWN[i]) + ",\t" + str(adultWN[i]) + ",\t"
    data_str += str(predWN[i]) + ",\t" + str(mu_list[i]+uP) + "\n"
    output_file.write(data_str)
output_file.close()

# ------------------------------------------------------------
# Write TS with pink noise
# ------------------------------------------------------------

output_file = open("tsPinkNoise.csv", "w")
data_str = "time" + ",\t" + "juv" + ",\t" + "adult" + ",\t" + "pred" + ",\t" + "uP\n"
output_file.write(data_str)
for i in range(len(juvPN)):
    data_str = str(i) + ",\t" + str(juvPN[i]) + ",\t" + str(adultPN[i]) + ",\t"
    data_str += str(predPN[i]) + ",\t" + str(meow_list[i]) + "\n"
    output_file.write(data_str)
output_file.close()


### ------------------------------------------------------------
### Write noise files for spectral analysis
### ------------------------------------------------------------
output_file = open("pinkNoise.csv","w")
data_str = "time" + ",\t" + "noise\n"
output_file.write(data_str)
for i in range(len(pn)):
    data_str = str(i) + ",\t" + str(tinyPN[i]) + "\n"
    output_file.write(data_str)
output_file.close()

output_file = open("whiteNoise.csv","w")
data_str = "time" + ",\t" + "noise\n"
output_file.write(data_str)
for i in range(len(wn)):
    data_str = str(i) + ",\t" + str(wn[i]) + "\n"
    output_file.write(data_str)
output_file.close()




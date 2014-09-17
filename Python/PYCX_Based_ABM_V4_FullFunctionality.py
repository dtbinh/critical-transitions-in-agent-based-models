# ------------------------------------------------------------
# Agent Based Model Project (based on PyCX module) v.4.0
# 
# Based on simple ecological model for interactions between
# a predator and a stage-structured prey species, as found in
# Boerlijst, Oudman, and de Roos, 2013, which is itself
# found in an earlier paper by van Kotten et al (2005)
#
# Kyle Shank - Feb 28, 2014
#
# v2 Notes (2/28/14):
# Fixed Torus issues via adjustment of Clip function (modulo - %)
#
#
# v3 Notes (3/7/14):
# Added 3rd species
# Turned "Step" function into a function of functions.
# Began passing agent functions (move, eat, die, etc.) to "Step"
# Also, fixed graphing error (no need to pass new pop. data to .Data
# file)
#
#
# v4 Notes:
# Finished adding agent functionality
# Added debugging counters to make sure function are operating correctly
# ------------------------------------------------------------

# ------------------------------------------------------------
# ODE MODEL FOR REFERENCE
# 
# Juv. Prey   = dJ/dt = f(A) - g(J) - uJ*J
# Adult Prey  = dA/dt = g(J) - h(A,P) - uA*A
# Predators   = dP/dt = h(A,P)*c - uP*P
#
# f(A) is a function that specifies reproduction rates of Adult Prey
# and is realized as f(A) = bA, with b representing the adult
# reproduction rate. Fecundity here is modeled as a linear rprocess.
# 
# g(J) is a function that specifies the maturation rate of Juv. Prey
# and is realized as g(J) = J/(1+J**2). Maturation here is modeled as
# a non-linear process (thus giving rus to our chaotic behavior).
#
# h(A,P) is the predation rate on adults and is realized as h(A,P) = AP
# 
# uJ,uA,uP are death rates, c is a conversion factor
#
#
# MODEL PARAMETERS
#
# b = 1        # Adult reproduction rate, citation has b = 1
# c = 1        # Conversion factor, citation has c = 1
# uJ = 0.05    # Mortality rate of Juveniles, citation has uJ = 0.05
# uA = 0.1     # Morality rate of Adults, citation has uA = 0.1
# uP = 0.432   # Mortality rate of Predators. There are bifurcation points at
# uP = 0.553 and uP = 0.435. 
# ------------------------------------------------------------

# ------------------------------------------------------------
# PREAMBLE
# ------------------------------------------------------------

import matplotlib
matplotlib.use('TkAgg')
import pylab as PL
import random as RD
import scipy as SP
import pycxsimulator
RD.seed()


# ------------------------------------------------------------
# PARAMETERS
# ------------------------------------------------------------

width = 100
height = 100

initialJuvPopulation = 10
juvMortality = 0.05

initialAdultPopulation = 10
adultReproductionRate = 1
adultMortality = 0.1

initialPredatorPopulation = 100
predatorPredationRate = 1
predatorConversionRate = 1
predatorMortality = 0.002


collisionDistance = 1
CDsquared = collisionDistance * collisionDistance
## Sets "awareness" distance

toBeRemoved = -1
## sets item in agent list to [-1], thus easy to find for removal

# ------------------------------------------------------------
# THE MODEL:
# PyCX has three functions that pass into it: init(), draw(),
# and step().
# ------------------------------------------------------------

# ------------------------------------------------------------
# init(): this function initializes the popluation lists and
# time
# ------------------------------------------------------------

def init():
    global time, juv, adult, predator, juvData, adultData, predatorData, deadJuv, deadAdult, deadPredator, adultEaten, juvMatured, juvBorn, predatorBorn
    
    time = 0
    ## Sets ticks to zero
    
    juv = []
    for i in xrange(initialJuvPopulation):
        juv.append([RD.uniform(0, width), RD.uniform(0, height)])
    ## Creates an empty list of juveniles and creates the population
    ## where juv[0] is x-coordinate and juv[1] is y-coordinate
    ## FOR LATTICE
##    for i in xrange(initialJuvPopulation):
##        juv.append([RD.randint(0,width), RD.randint(0,height)])
        
    adult = []
    for i in xrange(initialAdultPopulation):
        adult.append([RD.uniform(0, width), RD.uniform(0, height)])
    ## Creates an empty list of adults and creates the population
    ## where adult[0] is x-coordinate and adult[1] is y-coordinate
    ## FOR LATTICE
##    for i in xrange(initialAdultPopulation):
##        adult.append([RD.randint(0,width), RD.randint(0,height)])

    predator = []
    for i in xrange(initialPredatorPopulation):
        predator.append([RD.uniform(0,width),RD.uniform(0,height), 0])
    ## Creates an empty list of predators and creates the population
    ## where predator[0] is x-coordinate, predator[1] is y-coordinate, and
    ## predator[2] is a track of how many adults they've eaten this step.
    ## FOR LATTICE
##    for i in xrange(initialPredatorPopulation):
##        predator.append([RD.randint(0,width), RD.randint(0,height), 0])

    
    juvData = [initialJuvPopulation]
    adultData = [initialAdultPopulation]
    predatorData = [initialPredatorPopulation]
    


# Debugging Counters


    deadJuv = 0
    ## debugs juvMortality
    
    deadAdult = 0
    ## debugs adultMortality
    
    deadPredator = 0
    ## debugs predatorMortality

    adultEaten = 0
    ## debugs predatorEat
    
    juvMatured = 0
    ## debugs juvMature
    
    juvBorn = 0
    ## debugs adultReproduce
    
    predatorBorn = 0
    ## debugs predatorReproduce


# ------------------------------------------------------------
# draw(): this function calls from PyLab to create two plots.
# The first subplot is the actual ABM plot where things move
# (this is a scatter plot which updates at each "step".)
# The second subplot is the time series plot which tracks
# the population counts
#
# Note (3/7/2014): we could produce a bunch more time-series
# plots if needed to track things like # of deaths, # births,
# etc. 
# ------------------------------------------------------------

def draw():
    ## Draws the ABM plot
    
    PL.subplot(1, 2, 1)
    ## Calls Pylab to create the ABM plot (nrows,ncol,plot_number)
    
    PL.cla()
    ## Clears the axes of the current plot
    
    if juv != []:
        x = [agent[0] for agent in juv]
        y = [agent[1] for agent in juv]
        PL.scatter(x, y, color = 'pink')
        ## If there are living juvenile prey (!= []), plot them.
        ## PL.scatter will create a 2D scatter plot of the juveniles
    if adult != []:
        PL.hold(True)
        x = [agent[0] for agent in adult]
        y = [agent[1] for agent in adult]
        PL.scatter(x, y, color = 'red')
        PL.hold(False)
        ## First: check for living adult prey. If true, proceed:
        ## Hold the plot function static, then scatter plot the adults.
        ## Then, release the plot
    if predator != []:
        PL.hold(True)
        x = [agent[0] for agent in predator]
        y = [agent[1] for agent in predator]
        PL.scatter(x,y, color = 'blue')
        PL.hold(False)
        ## First: check for living oredators. If true, proceed:
        ## Hold the plot function static, then scatter plot the predators.
        ## Then, release the plot
        
    PL.axis('scaled')
        ## Changes the limits of the plot box so that x and y are equal
    PL.axis([0, width, 0, height])
    PL.title('t = ' + str(time))

    ## Draws the Time Series Plot
    
    PL.subplot(1, 2, 2)
    ## Calls Pylab to create the TS plot
    PL.cla()
    PL.plot(juvData, color = 'pink')
    PL.hold(True)
    PL.plot(adultData, color = 'red')
    PL.plot(predatorData, color = 'blue')
    PL.title('Population Time Series')
    PL.hold(False)


# ------------------------------------------------------------
# step(): this function does all the work!
# ------------------------------------------------------------

def step():
    ## This is the BIG function that implements all of the updates in each "tick"
    move(), juvMature(), adultReproduce(), predatorEat(), predatorReproduce(), grimReaper(), tick() 

# ------------------------------------------------------------

def worldShape(a, amin, amax):
    ## This function enforces boundary conditions

    ## This enforces boundary conditions for non-torus
    #if a < amin: return amin  # this is for solid edges.  i.e., square.
    #elif a > amax: return amax
    #else: return a
    
    ## This enforces boundary conditions for a Torus
    ## amin should always be 0!
    return a%amax

    ## This enforces boundary conditions for a lattice
    ## STILL ON TO DO LIST 3/7/2014

# ------------------------------------------------------------

def move():
    ## This function moves the agents on the grid 
    global time, juv, adult, predator


    ## Simulates random movement in the juvenile prey agents by first adding random noise
    ## to their X and Y locations, then having them move (via clip()).
    for agent in juv:
        agent[0] += RD.gauss(0, 1)
        agent[1] += RD.gauss(0, 1)
##        For lattice:
##        agent[0] += RD.randint(0, 1)
##        agent[1] += RD.randint(0, 1)
        agent[0] = worldShape(agent[0], 0, width)
        agent[1] = worldShape(agent[1], 0, height)

    ## Simulates random movement in the adult prey agents by first adding random noise
    ## to their X and Y locations, then having them move (via clip()).
    for agent in adult:
        agent[0] += RD.gauss(0, 1)
        agent[1] += RD.gauss(0, 1)
##        For lattice:
##        agent[0] += RD.randint(0, 1)
##        agent[1] += RD.randint(0, 1)
        agent[0] = worldShape(agent[0], 0, width)
        agent[1] = worldShape(agent[1], 0, height)

    ## Simulates random movement in the predator agents by first adding random noise
    ## to their X and Y locations, then having them move (via clip()).
    for agent in predator:
        agent[0] += RD.gauss(0, 1)
        agent[1] += RD.gauss(0, 1)
##        For lattice:
##        agent[0] += RD.randint(0, 1)
##        agent[1] += RD.randint(0, 1)
        agent[0] = worldShape(agent[0], 0, width)
        agent[1] = worldShape(agent[1], 0, height)

# ------------------------------------------------------------

def adultReproduce():
    global juv, adult, juvBorn

    for i in xrange(len(adult)):
        if RD.random() < adultReproductionRate:
            juv.append(adult[i][:])
            juvBorn += 1
            

# ------------------------------------------------------------

def juvMature():
    global juv, adult, juvMatured, juvMatureRate

    juvMaturationRate = (float(len(juv)))/((1.0 + float(len(juv))*float(len(juv))))
    ## sets juvMaturationRate according to ODE non-linear g(J) rule
    
    for i in xrange(len(juv)):
        if RD.random() < juvMaturationRate :
            adult.append(juv[i][:])
            juvMatured += 1
            juv[i] = toBeRemoved

    while toBeRemoved in juv:
        juv.remove(toBeRemoved)


# ------------------------------------------------------------

def predatorEat():
    ## This function lets predators eat adult prey species based on proximity
    global  adult, predator, adultEaten
    

    for i in xrange(len(predator)):
        for j in xrange(len(adult)):
            if adult[j] != toBeRemoved:
                if ((predator[i][0]-adult[j][0]) * (predator[i][0]-adult[j][0])) + ((predator[i][1]-adult[j][1])*(predator[i][1]-adult[j][1])) < CDsquared:
                    ## If predator and prey are within a certain distance (CDsquared), eat them.
                    adult[j] = toBeRemoved
                    predator[i][2] += 1
                    adultEaten += 1
                    ## Add an "eaten" adult to the "things I've eaten" list

    while toBeRemoved in adult:
        adult.remove(toBeRemoved)


# ------------------------------------------------------------

def predatorReproduce():
    global predator, predatorBorn

    for i in xrange(len(predator)):
        if predator[i][2] != 0:
        ## if predator has eaten an adult, reproduce, if not, keep swimming 
            h = predator[i][2]
            predator[i][2] = 0
            ## resets predator "eaten" counter to zero to prevent it copying to the next generation
            predator.append(h*predator[i][:])
            predatorBorn += 1

# ------------------------------------------------------------        
    
def grimReaper():
    ## This is the mortality function for juv, adult, predator
    global juv, adult, predator, juvMortality, adultMortality, predatorMortality, deadJuv, deadAdult, deadPredator

    for i in xrange(len(juv)):
        if juv[i] != toBeRemoved:
            if RD.random() <= juvMortality:
                juv[i] = toBeRemoved
                deadJuv += 1


    for i in xrange(len(adult)):
        if adult[i] != toBeRemoved:
            if RD.random() <= adultMortality:
                adult[i] = toBeRemoved
                deadAdult += 1

    for i in xrange(len(predator)):
        if predator[i] != toBeRemoved:
            if RD.random() <= predatorMortality:
                predator[i] = toBeRemoved
                deadPredator += 1


    while toBeRemoved in juv:
        juv.remove(toBeRemoved)
    while toBeRemoved in adult:
        adult.remove(toBeRemoved)
    while toBeRemoved in predator:
        predator.remove(toBeRemoved)

# ------------------------------------------------------------

def tick():
    ## Updates time clock and updates lists
    
    global time, juvPopulation, adultPopulation, predatorPopulation, juvData, adultData, predatorData
    
    time += 1
    
    juvPopulation = len(juv)
    adultPopulation = len(adult)
    predatorPopulation = len(predator)
    
    juvData.append(len(juv))
    adultData.append(len(adult))
    predatorData.append(len(predator))


# ------------------------------------------------------------
# Execute
# ------------------------------------------------------------    

pycxsimulator.GUI().start(func=[init,draw,step])

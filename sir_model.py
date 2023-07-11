'''
Simple SIR model in Python
'''

import numpy as np
import pylab as pl

# Set parameters
beta = 2.5 # Infection rate
gamma = 1.0 # Recovery rate
I0 = 5 # Number of people initially infected
N = 100 # Total population size
maxtime = 10 # How long to simulate for
npts = 100 # Number of time points during the simulation 
dt = maxtime/npts # Timestep length

# Create the arrays -- one entry per timestep
x = np.arange(npts)
S = np.zeros(npts)
I = np.zeros(npts)
R = np.zeros(npts)
time = x*dt
S[0] = N - I0 # Set initial conditions
I[0] = I0

# Run the simulation
for t in x[:-1]:
    infections = beta/N*S[t]*I[t]*dt # Calculate number of infections
    recoveries = gamma*I[t]*dt # Calculate the number of recoveries
    
    S[t+1] = S[t] - infections # Update the number susceptible
    I[t+1] = I[t] + infections - recoveries # Update the number infected
    R[t+1] = R[t] + recoveries # Update the number recovered

# Plot
pl.plot(time, S, label='Susceptible')
pl.plot(time, I, label='Infectious')
pl.plot(time, R, label='Recovered')
pl.legend()
pl.xlabel('Time')
pl.ylabel('Number of people')
pl.show()
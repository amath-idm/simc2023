## ----Download Libraries-------------------------------------------------------
library(deSolve)   # numerical integration package
library(ggplot2)   # plotting package
library(tidyverse) # packages for handling and reshaping data
library(reshape2) 
library(stats) # statistics package


## ----Define SIR model function to be integrated-------------------------------
sir_model <- function(time, state, parameters) {
  # Unpack Parameters
  with(as.list(c(state,parameters)), {
      # Calculate total population size
      N <-  S + I + R
      
      # Calculate force of infection
      lambda <- beta * I / N
      
      # Calculate derivatives
      dS <- -lambda * S
      dI <- lambda * S - gamma * I
      dR <- gamma * I
      
      # Return derivative
      return(list(c(dS, dI, dR)))
    }
  )
}


## -----------------------------------------------------------------------------
# Initial values for state variables
initial_state_values <- c(S = 9999,
                         I = 1,
                         R = 0
)

# Parameter values
parameters <-  c(beta = 1, # per-contact infection rate
                 gamma = .5 # recovery rate
)

# Define a list of time steps 
times = seq(from = 0, to = 50, by = .1)

# Integrate
sir.output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))


## -----------------------------------------------------------------------------
p <- ggplot(data = sir.output) + 
  geom_line(mapping = aes(x = time, y = S), 
            color = 'blue', size = 2) + 
  geom_line(mapping = aes(x = time, y = R), 
            color = 'black', size = 2) + 
  geom_line(mapping = aes(x = time, y = I), 
            color = 'red', size = 2) + 
  xlim(0,50) + 
  xlab("Time") + 
  ylab("Number of People") + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24)) + 
  theme_bw()

p


## -----------------------------------------------------------------------------
# Find the first time when the number of infected drops below 1
sir.output[sir.output$I <=1,][1,]$R


## -----------------------------------------------------------------------------
sir.output[sir.output$I == max(sir.output$I),]$time


## -----------------------------------------------------------------------------
sir.output[sir.output$I == max(sir.output$I),]$I


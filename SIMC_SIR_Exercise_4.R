## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
# Using the optimized parameter values from before
# (if you found something different, feel free to use those values here)
parameters = c(beta = 2.559449, gamma =  2.109450)

# Define times - start at week 10
times = seq(from = 10, to = 75, by = 1)

# Adjust vaccinated proportion p.vaccinated here:
p.vaccinated = .1
# Initial state values
initial_state_values <- c(S = 55000*(1 - p.vaccinated) - 1,
                         I = 1,
                         R = 55000*(p.vaccinated)
)
sir.vaccinated <- deSolve::ode(initial_state_values, 
                        seq(from = 10, to = 75, by = 1), 
                        sir_model, 
                        parms = fitval$par, )
sir.vaccinated <- data.table::as.data.table(sir.vaccinated)





## -----------------------------------------------------------------------------
p <- ggplot() + 
  geom_line(data = sir.vaccinated, 
            mapping = aes(x = time, y = I), 
            color = 'blue', size = 2) + 
    geom_line(data = sir.fit, 
            mapping = aes(x = time, y = I), 
            color = 'red', size = 2 ) + 
  geom_point(data = plague.dat, mapping = aes(x = time, y = Deaths), size = 3) + 
  ylab("Deaths") + xlab("time (Weeks)") + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24)) + 
  theme_bw()
p


## -----------------------------------------------------------------------------
sir.vaccinated$R %>% tail(1)


## -----------------------------------------------------------------------------
sir.fit$R %>% tail(1)


## ----SIR model with treatment-------------------------------------------------

sir_model_treatment <- function(time, state, parameters) {
  # Unpack Parameters
  with(as.list(c(state,parameters)), {
      # Calculate total population size
      N <-  S + I_t + I + R
      
      # Calculate force of infection
      lambda <- beta * (I + I_t) / N
      
      # Calculate derivatives
      dS <- -lambda * S
      # Notice now that there is this new compartment of people who receive treatment
      dI_t <- lambda * S * p.treatment - gamma/.75 * I_t
      # And this compartment where people do not receive treatment
      dI <- lambda * S * (1-p.treatment) - gamma * I
      dR <- gamma * I + 2*gamma * I_t
      
      # Return derivative
      return(list(c(dS, dI, dI_t, dR)))
    }
  )
}



## -----------------------------------------------------------------------------
# Using the optimized parameter values from before
# (if you found something different, feel free to use those values here)
# Also specify p.treated, the fraction of infected people who receive treatment
parameters = c(beta = 2.559449, gamma =  2.109450, p.treatment = 0.25)

# Define times - start at week 10
times = seq(from = 10, to = 75, by = 1)

# Initial state values
initial_state_values <- c(S = 54999,
                         I = 1,
                         I_t = 0,
                         R = 0
)
sir.treated <- deSolve::ode(initial_state_values, 
                        seq(from = 10, to = 75, by = 1), 
                        sir_model_treatment, 
                        parms = parameters )
sir.treated <- data.table::as.data.table(sir.treated)


## -----------------------------------------------------------------------------
p <- ggplot() + 
  geom_line(data = sir.treated, 
            mapping = aes(x = time, y = I+I_t), 
            color = 'blue', size = 2) + 
    geom_line(data = sir.fit, 
            mapping = aes(x = time, y = I), 
            color = 'red', size = 2 ) + 
  geom_point(data = plague.dat, mapping = aes(x = time, y = Deaths), size = 3) + 
  ylab("Deaths") + xlab("time (Weeks)") + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24)) + 
  theme_bw()

p


## -----------------------------------------------------------------------------
sir.treated$R %>% tail(1)


## -----------------------------------------------------------------------------
sir.fit$R %>% tail(1)


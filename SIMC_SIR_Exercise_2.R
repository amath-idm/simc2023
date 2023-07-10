## -----------------------------------------------------------------------------
## Please run SIMC_SIR_Exercise1.R before running this
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
# Initial state values
initial_state_values <- c(S = 9999,
                         I = 1,
                         R = 0
)

# Define a list of time steps 
times = seq(from = 0, to = 1000, by = .1)

# Loop over a range of beta and gamma, find the size of the epidemic each time
beta.gamma.outbreak <- data.frame(beta = c(), 
                                  gamma = c(), 
                                  outbreak.size= c())
for (beta.value in seq(0,1,.1)){
  for (gamma.value in seq(.1,1.,.1)){
    parameters <- c(beta = beta.value, # per-contact infection rate
                    gamma = gamma.value # recovery rate
                    )
    # Integrate
    sir.output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))
    # Find the total outbreak size
    total.recovered = sir.output[sir.output$I <1,][1,]$R
    
    beta.gamma.outbreak = rbind(beta.gamma.outbreak,
                                data.frame(beta = beta.value, 
                                           gamma = gamma.value, 
                                           outbreak.size = total.recovered)
                                )

  }
}

#beta.gamma.outbreak



## -----------------------------------------------------------------------------
p <- ggplot(data = beta.gamma.outbreak) + 
  geom_point(mapping = aes(x = gamma, y = beta, color = log(outbreak.size)),
             size = 4) + 
  scale_color_gradient2() + 
  labs("Log(Outbreak Size)") + 
  scale_x_continuous(breaks = c(0,.2,.4,.6,.8,1)) + 
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1)) + 
  xlab("gamma") + 
  ylab("Beta") +
  theme_bw()
p


## -----------------------------------------------------------------------------
# Define a list of time steps 
times = seq(from = 0, to = 1000, by = .1)

# Loop over a range of beta and gamma, find the size of the epidemic each time
r0.outbreak <- data.frame(R_0 = c(), 
                          outbreak.size= c())

gamma.value = .2
for (beta.value in seq(0,1,.05)){
    parameters <- c(beta = beta.value, # per-contact infection rate
                    gamma = gamma.value # recovery rate
                    )
    # Integrate
    sir.output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))
    # Find the total outbreak size
    total.recovered = sir.output[sir.output$I <1,][1,]$R
    
    r0.outbreak = rbind(r0.outbreak,
                        data.frame(R_0 = beta.value/gamma.value, 
                                   outbreak.size = total.recovered)
                        )

}

#r0.outbreak


## -----------------------------------------------------------------------------
p <- ggplot(data = r0.outbreak) + 
  geom_line(mapping = aes(x = R_0, y = outbreak.size), size = 2) + 
  xlab("R_0") + 
  ylab("Outbreak size") +
  theme_bw()

p


## -----------------------------------------------------------------------------

# Initial state values
initial_state_values <- c(S = 9999,
                         I = 1,
                         R = 0
)
# Parameter values
parameters <-  c(beta = 1, # per-contact infection rate
                 gamma = .2 # recovery rate
)
# Time
times = seq(from = 0, to = 50, by = .1)

# Integrate
sir.output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = sir_model,
                            parms = parameters))

with(as.list(parameters), {
    ggplot(data = sir.output) + 
      geom_line(mapping = aes(x = time, y = beta/gamma * S/(S + I + R)), 
                size = 2 , color = 'black') + 
      geom_line(mapping = aes(x = time, y = I/(S+I+R)), 
                size = 2, color = 'red') + 
      xlab("Time") + 
      ylab("R_eff") + 
      theme_bw()
  }
)
  


## -----------------------------------------------------------------------------
sir.output[sir.output$I == max(sir.output$I),]$time


## -----------------------------------------------------------------------------

with(as.list(c(sir.output[which(sir.output$I == max(sir.output$I)) ,], parameters)), {
    beta/gamma * S/(S + I + R)
})



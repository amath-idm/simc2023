## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
plague.dat = read.csv(file = "plague_data.csv", header = TRUE)

plague.dat <- plague.dat %>% mutate(time = row_number())

plague.dat %>% head


## -----------------------------------------------------------------------------
sir.sse <- function(model.func, parameters, dat) {
    
    # Calculate model output
    output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = model.func,
                            parms = parameters))
    
    # Calculate sum-of-squares (sse) of model fit
    output <- merge(output,plague.dat, by = "time") %>%
      mutate(sse = (I - Deaths)^2 )
    
    sse = output$sse %>% sum

    return(sse)

}

#sir.sse(sir_model, parameters = c(beta = 2.55, gamma = 2.11), dat = plague.dat)


## ----Run the calibration------------------------------------------------------
# Initial guess for parameters
parameters = c(beta = 1., gamma = .5)

# Define times - start at week 10
times = seq(from = 10, to = 60, by = 1)

# Initial state values
initial_state_values <- c(S = 54999,
                         I = 1,
                         R = 0
)

# Use optim to find the parameter set which comes closest to fitting the data
# Minimizes sum of squared errors function defined above
fitval <- stats::optim( par = parameters,
                  fn  = sir.sse,
                  model.func = sir_model,
                  dat = plague.dat,
                  gr = "BFGS"
             )

# Parameter values from the optimization:
fitval$par



## -----------------------------------------------------------------------------
sir.sse(sir_model, fitval$par, plague.d)


## ----Plot model using optimized parameter values------------------------------
sir.fit <- deSolve::ode(initial_state_values, 
                        seq(from = 10, to = 55, by = 1), 
                        sir_model, 
                        parms = fitval$par, # Optimized parameter values
                        )
sir.fit <- data.table::as.data.table(sir.fit)

p <- ggplot() + 
  geom_line(data = sir.fit, 
            mapping = aes(x = time, y = I), 
            color = 'red', size = 2 ) + 
  geom_point(data = plague.dat, mapping = aes(x = time, y = Deaths), size = 3) + 
  ylab("Deaths") + xlab("time (Weeks)") + 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24)) + 
  theme_bw()

p


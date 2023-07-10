library(ggplot2)

# Read in the data
flu <- read.csv("flu_cases.csv")

# How many days of observation do we have when we need to make a decision?
n_obs <- 5

# What do the data look like at this point?
ggplot(data = flu[1:n_obs, ], aes(x = day, y = cases)) +
  geom_point() +
  labs(title = "Number of infected students", x = "Day") +
  scale_x_continuous(breaks = 1:n_obs) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Define our parameters
beta1 <- 0.5
gamma1 <- 1/50
beta2 <- 0.6
gamma2 <- 1/10
npts <- 100
I0 <- 1
N <- 1000
dt <- 1

# Make vectors where we will store the estimates
x <- 1:npts
S1 <- rep(0, npts)
I1 <- rep(0, npts)
R1 <- rep(0, npts)
S2 <- rep(0, npts)
I2 <- rep(0, npts)
R2 <- rep(0, npts)

# Initial conditions
S1[1] <- N - I0
I1[1] <- I0
S2[1] <- N - I0
I2[1] <- I0

# Simulate the model over time
for (t in 1:(npts - 1)) {
  infections1 <- beta1 * S1[t] * I1[t] / N * dt
  recoveries1 <- gamma1 * I1[t] * dt
  infections2 <- beta2 * S2[t] * I2[t] / N * dt
  recoveries2 <- gamma2 * I2[t] * dt
  
  S1[t + 1] <- S1[t] - infections1
  I1[t + 1] <- I1[t] + infections1 - recoveries1
  R1[t + 1] <- R1[t] + recoveries1
  
  S2[t + 1] <- S2[t] - infections2
  I2[t + 1] <- I2[t] + infections2 - recoveries2
  R2[t + 1] <- R2[t] + recoveries2
}

# Plot the model estimates of the number of infections alongside the data
time <- x * dt

# Plot the model estimates of the number of infections alongside the data
fit_data = data.frame(time = time[1:n_obs], I1 = I1[1:n_obs], I2 = I2[1:n_obs], observed_cases = flu[1:n_obs, 3])
ggplot() +
  geom_line(data = fit_data, aes(x = time, y = I1), color = "blue", linetype = "solid", size = 1) +
  geom_line(data = fit_data, aes(x = time, y = I2), color = "red", linetype = "solid", size = 1) +
  geom_point(data = fit_data, aes(x = time, y = observed_cases)) +
  labs(title = "Number of infected students", x = "Day") +
  scale_x_continuous(breaks = 1:n_obs) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Model validation step: compare how well the model performed in forecasting the next few days
n_for <- 10
val_data = data.frame(time = time[1:n_for], I1 = I1[1:n_for], I2 = I2[1:n_for], observed_cases = flu[1:n_for, 3])
ggplot() +
  geom_line(data = val_data, aes(x = time, y = I1), color = "blue", linetype = "solid", size = 1) +
  geom_line(data = val_data, aes(x = time, y = I2), color = "red", linetype = "solid", size = 1) +
  geom_point(data = val_data, aes(x = time, y = observed_cases)) +
  labs(title = "Number of infected students", x = "Day") +
  scale_x_continuous(breaks = 1:n_for) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


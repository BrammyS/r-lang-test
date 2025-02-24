# Newton's Law of Cooling function
newtons_law_of_cooling <- function(T_env, T_initial, k, t) {
  T_env + (T_initial - T_env) * exp(-k * t)
}

# Example usage for BBQ:
T_env <- 150
T_initial <- 20
T_target <- 130

# Temperature readings (time in minutes, temperature in Celsius)
time <- c(10, 20, 30, 40)  # Time in minutes
temp_readings <- c(30, 50, 55, 57)  # Temperature readings at the given times

# Define the error function to minimize
error_function <- function(k, T_env, T_initial, time, temp_readings) {
  sum((newtons_law_of_cooling(T_env, T_initial, k, time) - temp_readings)^2)
}

# Optimize k using optim
optimization_result <- optim(par = 0.05,  # Initial guess for k
                            fn = error_function,  # Error function to minimize
                            T_env = T_env,
                            T_initial = T_initial,
                            time = time,
                            temp_readings = temp_readings,
                            method = "Brent", # Use Brent's method since k > 0
                            lower = 0, # k must be positive
                            upper = 1) # k must be less than 1

# Extract the optimized k value
k <- optimization_result$par

# Print the optimized k value
print(paste("Optimized k:", k))

# Calculate the time to reach the target temperature
time_to_target_temp <- function(T_env, T_initial, T_target, k) {
  log((T_target - T_env) / (T_initial - T_env)) / -k
}

t_target <- time_to_target_temp(T_env, T_initial, T_target, k)

print(paste("Time to reach", T_target, "degrees Celsius:", t_target, "minutes"))

# Generate data for the plot
time_points <- seq(0, t_target * 1.2, length.out = 100)
temperatures <- newtons_law_of_cooling(T_env, T_initial, k, time_points)

# Create the plot
plot(time_points, temperatures, type = "l",
     xlab = "Time (minutes)", ylab = "Temperature (°C)",
     main = "Newton's Law of Cooling",
     col = "blue",
     ylim = c(min(T_initial, T_target), max(T_env, T_initial)))

# Add a horizontal line for the target temperature
abline(h = T_target, col = "red", lty = 2)

# Add a legend
legend("topright", legend = c("Temperature", "Target Temperature"),
       col = c("blue", "red"), lty = c(1, 2))
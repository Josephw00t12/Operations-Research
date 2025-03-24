# Install lpSolve package if not already installed
if (!require(lpSolve)) install.packages("lpSolve")

# Load lpSolve library
library(lpSolve)

# coefficients of the objective function
objective <- c(3, 4)  # Model 1 = $3, Model 2 = $4

# constraints matrix (resource usage per unit)
constraints <- matrix(c(2, 3,  # Resistor usage
                        2, 1,  # Capacitor usage
                        0, 4), # Chips usage
                      nrow = 3, byrow = TRUE)

# right-hand side of the constraints
rhs <- c(1400, 1000, 800)

# direction of the constraints (<=)
directions <- c("<=", "<=", "<=")

# Solve the lp
solution <- lp(direction = "max",
               objective.in = objective,
               const.mat = constraints,
               const.dir = directions,
               const.rhs = rhs,
               compute.sens = TRUE)

# Output the results
cat("Optimal Solution:\n")
print(solution$solution)

shadow_prices <- solution$duals[1:length(rhs)]  # Extract duals for constraints
names(shadow_prices) <- c("Resistors", "Capacitors", "Chips")
print(shadow_prices)

cat("\nMaximum Profit: $", solution$objval, "\n")

# Analyze resource usage
cat("\nResource Usage Status:\n")
used_resources <- constraints %*% solution$solution
for (i in 1:length(rhs)) {
  unused <- rhs[i] - used_resources[i]
  if (abs(unused) < 1e-10) unused <- 0
  cat(paste0("Resource ", i, ": Used = ", used_resources[i], ", Unused = ", unused, "\n"))
  lower_bound <- solution$duals.from[i]
  upper_bound <- solution$duals.to[i]

  if (solution$duals[i] <= 0) {
    cat(paste0("Resource ", i, ": Unbounded above (Shadow Price = 0)\n"))
  } else {
    cat(paste0("Resource ", i, ": Lower Bound = ", lower_bound,
               ", Upper Bound = ", upper_bound, "\n"))
  }
}

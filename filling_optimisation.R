# Load necessary library
library(lpSolve)

set.seed(40426869)
# Feasibility Check function
feasibility_check <- function(solution) {
  raspberries_constraint <- solution[['special_quantity']] * 0.2 + solution[['basic_quantity']] * 0.2 <= 20
  premium_chocolate_constraint <- solution[['special_quantity']] * 0.8 + solution[['basic_quantity']] * 0.3 <= 60
  non_negative_constraint <- solution[['special_quantity']] >= 0 && solution[['basic_quantity']] >= 0
  demand_constraint_special <- solution[['special_quantity']] <= (190 - 25 * solution[['special_price']])
  demand_constraint_basic <- solution[['basic_quantity']] <= (250 - 50 * solution[['basic_price']])
  
  return(raspberries_constraint & premium_chocolate_constraint & non_negative_constraint & demand_constraint_special & demand_constraint_basic)
}

# Define the objective function to calculate revenue
calculate_revenue <- function(solution) {
  special_revenue <- solution[['special_price']] * solution[['special_quantity']]
  basic_revenue <- solution[['basic_price']] * solution[['basic_quantity']]
  total_revenue <- special_revenue + basic_revenue
  return(total_revenue)
}

# Generate a neighbor solution
generate_neighbor <- function(current_solution) {
  neighbor_solution <- current_solution
  adjustment_factor_special <- runif(1, -1, 1)
  adjustment_factor_basic <- runif(1, -1, 1)
  
  # Generate new quantities within a feasible range
  neighbor_solution[['special_quantity']] <- neighbor_solution[['special_quantity']] + adjustment_factor_special
  neighbor_solution[['basic_quantity']] <- neighbor_solution[['basic_quantity']] + adjustment_factor_basic
  
  # Update prices based on the demand function
  neighbor_solution[['special_price']] <- (190 - neighbor_solution[['special_quantity']]) / 25
  neighbor_solution[['basic_price']] <- (250 - neighbor_solution[['basic_quantity']]) / 50
  
  if (feasibility_check(neighbor_solution)) {
    return(neighbor_solution)
  } else {
    return(current_solution)
  }
}

# Perform Threshold Accepting search
perform_threshold_accepting <- function(max_iterations, initial_solution, initial_threshold, cooling_factor) {
  current_solution <- initial_solution
  current_revenue <- calculate_revenue(current_solution)
  threshold <- initial_threshold
  
  for (i in 1:max_iterations) {
    neighbor_solution <- generate_neighbor(current_solution)
    neighbor_revenue <- calculate_revenue(neighbor_solution)
    revenue_difference <- neighbor_revenue - current_revenue
    
    # Accept if the solution is better or if it's worse but within the acceptance threshold
    if (revenue_difference > 0 || exp(revenue_difference / threshold) > runif(1)) {
      current_solution <- neighbor_solution
      current_revenue <- neighbor_revenue
    }
    
    # Cool down the threshold
    threshold <- threshold * cooling_factor
  }
  return(current_solution)
}

# Run Threshold Accepting multiple times
run_multiple_threshold_accepting <- function(num_runs, max_iterations, initial_threshold, cooling_factor) {
  best_solution <- NULL
  best_revenue <- -Inf
  
  for (run in 1:num_runs) {
    initial_solution <- list(
      special_quantity = runif(1, 0, min(20/0.2, 60/0.8)),
      basic_quantity = runif(1, 0, min(20/0.2, 60/0.3)),
      special_price = 7.6,
      basic_price = 5
    )
    
    initial_solution[['special_price']] <- (190 - initial_solution[['special_quantity']]) / 25
    initial_solution[['basic_price']] <- (250 - initial_solution[['basic_quantity']]) / 50
    
    if (!feasibility_check(initial_solution)) {
      next
    }
    
    solution <- perform_threshold_accepting(max_iterations, initial_solution, initial_threshold, cooling_factor)
    revenue <- calculate_revenue(solution)
    
    if (revenue > best_revenue) {
      best_solution <- solution
      best_revenue <- revenue
    }
  }
  
  return(list(best_solution = best_solution, best_revenue = best_revenue))
}

# Parameters for Threshold Accepting
num_runs <- 10
max_iterations <- 1000
initial_threshold <- 10  # Starting acceptance threshold
cooling_factor <- 0.95  # Factor to reduce the threshold each iteration

# Execute the multiple threshold accepting searches
final_result_TA <- run_multiple_threshold_accepting(num_runs, max_iterations, initial_threshold, cooling_factor)

# Output the best solution found
print(final_result_TA$best_solution)
print(paste("Highest Total Revenue over all runs using Threshold Accepting: ", final_result_TA$best_revenue))

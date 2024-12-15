# simulation_backend.R

# Load district data
district_data <- read.csv("data/district_data.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
district_data$Mandater <- as.numeric(district_data$Mandater)

if (any(is.na(district_data$Mandater)) || !all(district_data$Mandater > 0)) {
  stop("Error: 'Mandater' column in district_data.csv must contain valid, positive integers.")
}

if (any(is.na(district_data$Valgdistrikt))) {
  stop("Error: Missing values in 'Valgdistrikt' column of district_data.csv.")
}

# Ensure consistency in column names
district_data$Valgdistrikt <- trimws(tolower(district_data$Valgdistrikt))

simulate_election <- function(full_input_data, num_simulations = 1000) {
  # Validate and coerce `num_simulations`
  if (!is.numeric(num_simulations) || length(num_simulations) != 1 || num_simulations <= 0) {
    num_simulations <- 1000  # Default to 1000 if invalid
  } else {
    num_simulations <- as.integer(num_simulations)
  }
  
  # Validate input structure
  if (!all(c("Region", "Parti", "Prosent", "Usikkerhet") %in% colnames(full_input_data))) {
    stop("Error: The input data must include 'Region', 'Parti', 'Prosent', and 'Usikkerhet' columns.")
  }
  
  # Split data into district-level
  district_input <- full_input_data[full_input_data$Nivå == "Distrikt", ]
  district_input$Region <- trimws(tolower(district_input$Region))
  
  # Verify district-level data consistency
  missing_regions <- setdiff(unique(district_input$Region), district_data$Valgdistrikt)
  if (length(missing_regions) > 0) {
    stop(paste("Error: The following regions in the input data are missing in district_data.csv:", 
               paste(missing_regions, collapse = ", ")))
  }
  
  # Simulation results container
  results <- list(
    DistrictAllocations = vector("list", num_simulations),
    LevelingSeats = vector("list", num_simulations)
  )
  
  for (i in seq_len(num_simulations)) {
    # Perturb district-level inputs
    perturbed_data <- perturb_inputs(district_input)
    
    # Debugging: Print perturbed regions
    cat("Perturbed Data Regions:\n")
    print(unique(perturbed_data$Region))
    
    # Run Sainte-Laguë algorithm on the perturbed data
    simulation_output <- run_tests(perturbed_data, district_data)
    
    # Collect results
    results$DistrictAllocations[[i]] <- simulation_output$DistrictAllocations
    results$LevelingSeats[[i]] <- simulation_output$LevelingSeats
  }
  
  # Aggregate results
  aggregated_results <- aggregate_simulation_results(results)
  return(aggregated_results)
}

perturb_inputs <- function(simulation_inputs) {
  # Add stochastic variation to district-level percentages
  perturbed_data <- simulation_inputs %>%
    mutate(
      Prosent = mapply(function(mean, sd) {
        if (is.na(mean) || is.na(sd) || mean <= 0 || mean >= 100 || sd <= 0) {
          return(mean) # Handle invalid or missing data gracefully
        }
        alpha <- (mean / 100) * ((mean / 100 * (1 - mean / 100)) / sd^2 - 1)
        beta <- alpha * (1 / (mean / 100) - 1)
        if (alpha > 0 && beta > 0) {
          rbeta(1, alpha, beta) * 100  # Convert back to percentage
        } else {
          mean # Return the original mean if alpha or beta is invalid
        }
      }, Prosent, Usikkerhet)
    )
  
  return(perturbed_data)
}

aggregate_simulation_results <- function(results) {
  # Summarize results across simulations
  district_probabilities <- lapply(results$DistrictAllocations, function(allocation) {
    do.call(rbind, allocation) %>%
      group_by(Party) %>%
      summarise(
        Median = median(Seats),
        CI_Lower = quantile(Seats, 0.025),
        CI_Upper = quantile(Seats, 0.975)
      )
  })
  
  leveling_probabilities <- do.call(rbind, results$LevelingSeats) %>%
    group_by(Region, Party) %>%
    summarise(
      Median = median(Quotient),
      CI_Lower = quantile(Quotient, 0.025),
      CI_Upper = quantile(Quotient, 0.975)
    )
  
  return(list(DistrictProbabilities = district_probabilities, LevelingProbabilities = leveling_probabilities))
}

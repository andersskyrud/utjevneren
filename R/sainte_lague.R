# ======================
# Sainte-Laguë Script
# ======================

# Required Libraries
library(dplyr)

# Load the CSV File
load_input_data <- function(file_path) {
  read.csv(file_path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# Parse Input Data
prepare_input_data <- function(input_data) {
  # National data
  national_data <- input_data %>%
    filter(Nivå == "Nasjonalt") %>%
    select(Parti, Prosent) %>%
    mutate(Votes = round(Prosent * 1000))  # Scale percentages to votes
  
  # District data
  district_data <- input_data %>%
    filter(Nivå == "Distrikt") %>%
    select(Region, Parti, Prosent) %>%
    group_by(Region) %>%
    mutate(Votes = round(Prosent * 1000)) %>%  # Scale percentages to votes
    ungroup()
  
  return(list(national = national_data, district = district_data))
}

# Sainte-Laguë Allocation Function
allocate_seats <- function(votes, total_seats) {
  results <- data.frame(
    Party = names(votes),
    Votes = votes,
    Divisor = rep(1.4, length(votes)),
    Quotient = votes / 1.4,
    Seats = rep(0, length(votes))
  )
  
  for (i in seq_len(total_seats)) {
    max_row <- which.max(results$Quotient)
    results$Seats[max_row] <- results$Seats[max_row] + 1
    results$Divisor[max_row] <- results$Divisor[max_row] + 2
    results$Quotient[max_row] <- results$Votes[max_row] / results$Divisor[max_row]
  }
  
  return(results)
}

# Leveling Seats Allocation
allocate_leveling_seats <- function(district_votes, national_votes, district_seats, total_leveling_seats) {
  # Calculate national seat allocation using Sainte-Laguë
  total_votes <- sum(unlist(national_votes))
  eligible_parties <- names(national_votes[national_votes / total_votes >= 0.04])  # Parties above 4% threshold
  national_seat_allocation <- allocate_seats(national_votes[eligible_parties], 169)
  
  # Subtract district seats already won by each party
  actual_district_seats <- Reduce("+", lapply(names(district_votes), function(region) {
    allocate_seats(district_votes[[region]], district_seats[[region]])$Seats
  }))
  
  # Ensure numeric vector for district seats
  actual_district_seats <- as.numeric(actual_district_seats)
  names(actual_district_seats) <- names(national_votes)
  
  # Calculate leveling seats needed
  leveling_seats_needed <- pmax(national_seat_allocation$Seats - actual_district_seats, 0)
  
  # Assign leveling seats across districts
  leveling_seat_allocations <- data.frame(Region = character(), Party = character(), stringsAsFactors = FALSE)
  
  for (i in seq_len(total_leveling_seats)) {
    # Compute fractions for each district and party
    fractions <- sapply(names(district_votes), function(region) {
      sapply(names(leveling_seats_needed), function(party) {
        if (leveling_seats_needed[party] > 0) {
          votes <- district_votes[[region]][party]
          divisor <- 1 + sum(leveling_seat_allocations$Region == region & leveling_seat_allocations$Party == party) * 2
          if (!is.na(votes)) votes / divisor else 0
        } else {
          0
        }
      })
    })
    
    # Flatten fractions to a numeric vector
    fractions <- unlist(fractions)
    if (length(fractions) == 0) break  # Stop if no more eligible fractions
    
    # Find the maximum fraction
    max_index <- which.max(fractions)
    region <- names(district_votes)[ceiling(max_index / length(leveling_seats_needed))]
    party <- names(leveling_seats_needed)[(max_index - 1) %% length(leveling_seats_needed) + 1]
    
    # Allocate a leveling seat
    leveling_seat_allocations <- rbind(
      leveling_seat_allocations,
      data.frame(Region = region, Party = party, stringsAsFactors = FALSE)
    )
    
    # Reduce the number of needed leveling seats for the party
    leveling_seats_needed[party] <- leveling_seats_needed[party] - 1
  }
  
  return(leveling_seat_allocations)
}

# Run Allocation
run_tests <- function(input_file, district_file) {
  # Load data
  input_data <- load_input_data(input_file)
  district_info <- load_input_data(district_file)
  
  # Prepare data
  input_values <- prepare_input_data(input_data)
  national_data <- input_values$national
  district_data <- input_values$district
  
  # Prepare district vote shares and seats
  district_votes <- split(district_data, district_data$Region) %>%
    lapply(function(d) setNames(d$Votes, d$Parti))
  
  district_seats <- setNames(district_info$Mandater, district_info$Valgdistrikt)
  
  # Allocate district seats
  district_allocations <- lapply(names(district_votes), function(region) {
    allocate_seats(district_votes[[region]], district_seats[[region]])
  })
  names(district_allocations) <- names(district_votes)
  
  # Prepare national vote shares
  national_votes <- setNames(national_data$Votes, national_data$Parti)
  
  # Allocate leveling seats
  leveling_seats <- allocate_leveling_seats(
    district_votes = district_votes,
    national_votes = national_votes,
    district_seats = district_seats,
    total_leveling_seats = 19
  )
  
  # Output results
  return(list(
    DistrictAllocations = district_allocations,
    LevelingSeats = leveling_seats
  ))
}

# Example Usage
# Path to your input files
input_file <- "data/utjevneren_innstillinger-2024-12-07.csv"
district_file <- "data/district_data.csv"

# Run Tests
results <- run_tests(input_file, district_file)

# View Results
print(results)

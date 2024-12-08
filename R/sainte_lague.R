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
  
  # Calculate "Andre" for national data
  andre_percentage <- 100 - sum(national_data$Prosent, na.rm = TRUE)
  national_data <- rbind(
    national_data,
    data.frame(Parti = "Andre", Prosent = andre_percentage, Votes = round(andre_percentage * 1000))
  )
  
  # District data
  district_data <- input_data %>%
    filter(Nivå == "Distrikt") %>%
    select(Region, Parti, Prosent) %>%
    group_by(Region) %>%
    mutate(Votes = round(Prosent * 1000)) %>%  # Scale percentages to votes
    ungroup()
  
  # Calculate "Andre" for district data
  andre_district_data <- district_data %>%
    group_by(Region) %>%
    summarise(Prosent = 100 - sum(Prosent, na.rm = TRUE)) %>%
    mutate(Parti = "Andre", Votes = round(Prosent * 1000))
  
  district_data <- bind_rows(district_data, andre_district_data) %>%
    arrange(Region, Parti)
  
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
  # Exclude "Andre" from national_votes and compute eligibility
  national_votes <- national_votes[names(national_votes) != "Andre"]
  total_votes <- sum(unlist(national_votes), na.rm = TRUE)
  eligible_parties <- names(national_votes[national_votes / total_votes >= 0.04])  # Parties above 4% threshold
  
  # National seat allocation
  national_seat_allocation <- allocate_seats(national_votes[eligible_parties], 169)
  
  # District seat allocation
  actual_district_seats <- Reduce("+", lapply(names(district_votes), function(region) {
    allocate_seats(district_votes[[region]], district_seats[[region]])$Seats
  }))
  
  # Align actual district seats with national votes
  actual_district_seats <- setNames(actual_district_seats, names(national_votes))
  actual_district_seats[is.na(actual_district_seats)] <- 0
  
  # Calculate leveling seats needed
  leveling_seats_needed <- pmax(
    national_seat_allocation$Seats - actual_district_seats[names(national_seat_allocation$Party)], 0
  )
  names(leveling_seats_needed) <- national_seat_allocation$Party
  
  # Track district allocations
  district_leveling_seats <- setNames(rep(0, length(district_votes)), names(district_votes))
  
  # Assign leveling seats across districts
  leveling_seat_allocations <- data.frame(
    Region = character(), Party = character(), Quotient = numeric(), stringsAsFactors = FALSE
  )
  
  for (i in seq_len(total_leveling_seats)) {
    fractions <- sapply(names(district_votes), function(region) {
      if (district_leveling_seats[region] >= 1) return(rep(0, length(leveling_seats_needed)))  # Max 1 seat per district
      
      sapply(names(leveling_seats_needed), function(party) {
        if (leveling_seats_needed[party] > 0 && party != "Andre") {
          votes <- district_votes[[region]][party]
          divisor <- 1.4 + sum(leveling_seat_allocations$Region == region & leveling_seat_allocations$Party == party) * 2
          if (!is.na(votes)) (votes / divisor) * district_seats[region] else 0
        } else {
          0
        }
      })
    })
    
    # Flatten fractions to a numeric vector
    fractions <- unlist(fractions)
    if (length(fractions) == 0 || all(fractions == 0)) break
    
    max_index <- which.max(fractions)
    region <- names(district_votes)[ceiling(max_index / length(leveling_seats_needed))]
    party <- names(leveling_seats_needed)[(max_index - 1) %% length(leveling_seats_needed) + 1]
    quotient <- max(fractions, na.rm = TRUE)
    
    # Allocate a leveling seat
    leveling_seat_allocations <- rbind(
      leveling_seat_allocations,
      data.frame(Region = region, Party = party, Quotient = quotient, stringsAsFactors = FALSE)
    )
    
    # Update constraints
    leveling_seats_needed[party] <- leveling_seats_needed[party] - 1
    district_leveling_seats[region] <- district_leveling_seats[region] + 1
  }
  
  return(leveling_seat_allocations)
}

# Run Allocation
run_tests <- function(input_file, district_file) {
  input_data <- load_input_data(input_file)
  district_info <- load_input_data(district_file)
  
  input_values <- prepare_input_data(input_data)
  national_data <- input_values$national
  district_data <- input_values$district
  
  district_votes <- split(district_data, district_data$Region) %>%
    lapply(function(d) setNames(d$Votes, d$Parti))
  
  district_seats <- setNames(district_info$Mandater, district_info$Valgdistrikt)
  
  district_allocations <- lapply(names(district_votes), function(region) {
    allocate_seats(district_votes[[region]], district_seats[[region]])
  })
  names(district_allocations) <- names(district_votes)
  
  national_votes <- setNames(national_data$Votes, national_data$Parti)
  
  leveling_seats <- allocate_leveling_seats(
    district_votes = district_votes,
    national_votes = national_votes,
    district_seats = district_seats,
    total_leveling_seats = 19
  )
  
  return(list(
    DistrictAllocations = district_allocations,
    LevelingSeats = leveling_seats
  ))
}

# Example Usage
input_file <- "data/utjevneren_innstillinger_test.csv"
district_file <- "data/district_data.csv"

results <- run_tests(input_file, district_file)

print(results$LevelingSeats)

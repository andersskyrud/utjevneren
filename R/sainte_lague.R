# ======================
# Sainte-Laguë Script
# ======================

# Required Libraries
library(dplyr)

# Load the CSV File
load_input_data <- function(file_path) {
  read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}

# Parse Input Data
prepare_input_data <- function(input_data, district_data) {
  # Normalize region names
  input_data$Region <- trimws(tolower(input_data$Region))
  district_data$Valgdistrikt <- trimws(tolower(district_data$Valgdistrikt))
  
  # Verify all regions match and handle any discrepancies
  missing_regions <- setdiff(unique(input_data$Region), district_data$Valgdistrikt)
  if (length(missing_regions) > 0) {
    stop(paste("Error: Missing seat mapping for the following regions:", paste(missing_regions, collapse = ", ")))
  }
  
  # Reorder district_data to match input_data regions
  district_data <- district_data[match(unique(input_data$Region), district_data$Valgdistrikt), ]
  
  # Scale percentages to votes using electorate size
  district_input <- input_data %>%
    left_join(district_data, by = c("Region" = "Valgdistrikt")) %>%
    mutate(Votes = round(Prosent * Elektorat)) %>%
    arrange(Region, Parti)
  
  return(list(district = district_input))
}

# Sainte-Laguë allocation
allocate_seats <- function(votes, total_seats) {
  if (length(total_seats) != 1 || total_seats <= 0) stop("Total seats must be a single positive integer.")
  
  andre_index <- which(names(votes) == "Andre")
  
  results <- data.frame(
    Party = names(votes),
    Votes = votes,
    Divisor = rep(1.4, length(votes)),
    Quotient = votes / 1.4,
    Seats = rep(0, length(votes))
  )
  
  for (i in seq_len(total_seats)) {
    max_row <- which.max(results$Quotient)
    
    if (length(andre_index) > 0 && max_row == andre_index) {
      results$Quotient[max_row] <- -Inf
      next
    }
    
    results$Seats[max_row] <- results$Seats[max_row] + 1
    results$Divisor[max_row] <- ifelse(
      results$Seats[max_row] == 1,
      3,
      2 * results$Seats[max_row] - 1
    )
    results$Quotient[max_row] <- results$Votes[max_row] / results$Divisor[max_row]
  }
  
  return(results)
}

# Allocate Leveling Seats
allocate_leveling_seats <- function(district_votes, national_votes, district_seats, total_leveling_seats) {
  cat("\n--- Debugging: allocate_leveling_seats ---\n")
  
  # Step 1: Exclude parties under the 4% threshold
  total_votes <- sum(unlist(national_votes))
  eligible_parties <- names(national_votes[national_votes / total_votes >= 0.04])
  
  if (length(eligible_parties) == 0) {
    stop("Error: No eligible parties above 4% threshold.")
  }
  
  cat("Eligible Parties:\n")
  print(eligible_parties)
  
  # Filter national votes for eligible parties
  eligible_national_votes <- national_votes[eligible_parties]
  
  # Step 2: Ideal National Allocation
  national_seat_allocation <- allocate_seats(eligible_national_votes, 169)
  cat("Ideal National Allocation:\n")
  print(national_seat_allocation)
  
  # Step 3: Calculate Actual District Seats
  actual_district_seats <- calculate_actual_district_seats(district_votes, district_seats, eligible_parties)
  cat("Actual District Seats:\n")
  print(actual_district_seats)
  
  # Step 4: Calculate Leveling Seats Needed
  leveling_seats_needed <- calculate_leveling_seats_needed(national_seat_allocation, actual_district_seats)
  cat("Leveling Seats Needed:\n")
  print(leveling_seats_needed)
  
  # Step 5: Allocate Leveling Seats Iteratively
  leveling_seat_allocations <- allocate_remaining_leveling_seats(
    district_votes = district_votes,
    leveling_seats_needed = leveling_seats_needed,
    total_leveling_seats = total_leveling_seats
  )
  
  cat("\n--- End Debugging: allocate_leveling_seats ---\n")
  return(leveling_seat_allocations)
}

# Helper: Calculate Actual District Seats
calculate_actual_district_seats <- function(district_votes, district_seats, eligible_parties) {
  Reduce("+", lapply(names(district_votes), function(region) {
    allocation <- allocate_seats(district_votes[[region]][eligible_parties], district_seats[region])
    seats_per_party <- setNames(allocation$Seats, allocation$Party)
    sapply(eligible_parties, function(p) seats_per_party[p] %||% 0)
  }))
}

# Helper: Calculate Leveling Seats Needed
calculate_leveling_seats_needed <- function(national_seat_allocation, actual_district_seats) {
  leveling_seats_needed <- pmax(national_seat_allocation$Seats - actual_district_seats, 0)
  leveling_seats_needed[leveling_seats_needed > 0]
}

# Helper: Allocate Remaining Leveling Seats
allocate_remaining_leveling_seats <- function(district_votes, leveling_seats_needed, total_leveling_seats) {
  leveling_seat_allocations <- data.frame(Region = character(), Party = character(), Quotient = numeric(), stringsAsFactors = FALSE)
  district_leveling_count <- setNames(rep(0, length(district_votes)), names(district_votes))
  
  for (i in seq_len(total_leveling_seats)) {
    # Calculate quotients for all districts and eligible parties
    fractions <- calculate_fractions(district_votes, leveling_seats_needed, leveling_seat_allocations)
    
    if (length(fractions) == 0 || max(fractions) == 0) break
    
    max_index <- which.max(fractions)
    region <- names(district_votes)[ceiling(max_index / length(leveling_seats_needed))]
    party <- names(leveling_seats_needed)[(max_index - 1) %% length(leveling_seats_needed) + 1]
    quotient <- fractions[max_index]
    
    # Allocate the leveling seat
    leveling_seat_allocations <- rbind(
      leveling_seat_allocations,
      data.frame(Region = region, Party = party, Quotient = quotient, stringsAsFactors = FALSE)
    )
    district_leveling_count[region] <- district_leveling_count[region] + 1
    leveling_seats_needed[party] <- leveling_seats_needed[party] - 1
    if (leveling_seats_needed[party] <= 0) {
      leveling_seats_needed <- leveling_seats_needed[leveling_seats_needed > 0]
    }
    
    # Debugging: Print current allocation round
    cat("\nAllocation Round:", i, "\n")
    cat("Region:", region, "\n")
    cat("Party:", party, "\n")
    cat("Quotient:", quotient, "\n")
    cat("Remaining Leveling Seats Needed:\n")
    print(leveling_seats_needed)
  }
  
  row.names(leveling_seat_allocations) <- NULL
  return(leveling_seat_allocations)
}

# Helper: Calculate Fractions for Leveling Seat Allocation
calculate_fractions <- function(district_votes, leveling_seats_needed, leveling_seat_allocations) {
  sapply(names(district_votes), function(region) {
    sapply(names(leveling_seats_needed), function(party) {
      if (!is.na(leveling_seats_needed[party]) && leveling_seats_needed[party] > 0) {
        votes <- district_votes[[region]][party]
        divisor <- 1 + sum(leveling_seat_allocations$Region == region & leveling_seat_allocations$Party == party) * 2
        if (!is.na(votes)) votes / divisor else 0
      } else {
        0
      }
    })
  }) %>%
    unlist()
}

# Run Allocation
run_tests <- function(input_data, district_data, verbose = FALSE) {
  # Validate district_data structure
  if (!all(c("Valgdistrikt", "Mandater", "Elektorat") %in% colnames(district_data))) {
    stop("Error: district_data must contain 'Valgdistrikt', 'Mandater', and 'Elektorat' columns.")
  }
  
  if (any(is.na(district_data$Mandater))) {
    stop("Error: Missing values in the 'Mandater' column.")
  }
  
  # Prepare data
  input_values <- prepare_input_data(input_data, district_data)
  district_data_processed <- input_values$district
  
  # Prepare votes
  district_votes <- split(district_data_processed, district_data_processed$Region) %>%
    lapply(function(d) setNames(d$Votes, d$Parti))
  
  # Prepare seat mapping
  district_seats <- setNames(district_data$Mandater, district_data$Valgdistrikt)
  
  # Check for missing regions
  missing_regions <- setdiff(names(district_votes), names(district_seats))
  if (length(missing_regions) > 0) {
    stop(paste("Error: Missing total_seats for the following regions:", paste(missing_regions, collapse = ", ")))
  }
  
  # Allocate district seats
  district_allocations <- lapply(names(district_votes), function(region) {
    allocate_seats(district_votes[[region]], district_seats[region])
  })
  names(district_allocations) <- names(district_votes)
  
  # Calculate actual district seats
  actual_district_seats <- Reduce("+", lapply(district_allocations, function(allocation) {
    result <- setNames(allocation$Seats, allocation$Party)
    all_parties <- unique(district_data_processed$Parti)
    sapply(all_parties, function(p) result[p] %||% 0)
  }))
  
  # Prepare national votes
  national_votes <- setNames(
    aggregate(Votes ~ Parti, data = district_data_processed, sum)$Votes,
    aggregate(Votes ~ Parti, data = district_data_processed, sum)$Parti
  )
  
  # Exclude parties under 4% national threshold
  total_votes <- sum(national_votes)
  eligible_parties <- names(national_votes[national_votes / total_votes >= 0.04])
  national_votes <- national_votes[eligible_parties]
  
  if (length(eligible_parties) == 0) {
    stop("Error: No parties above the 4% national threshold.")
  }
  
  # Allocate leveling seats
  leveling_seats <- allocate_leveling_seats(
    district_votes = district_votes,
    national_votes = national_votes,
    district_seats = district_seats,
    total_leveling_seats = 19
  )
  
  # Debugging outputs
  if (verbose) {
    cat("District Allocations:\n")
    print(district_allocations)
    cat("Actual District Seats:\n")
    print(actual_district_seats)
    cat("Leveling Seats:\n")
    print(leveling_seats)
  }
  
  return(list(
    DistrictAllocations = district_allocations,
    ActualDistrictSeats = actual_district_seats,
    LevelingSeats = leveling_seats
  ))
}

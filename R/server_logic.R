options(shiny.fullstacktrace = TRUE)

# Server-side logic
server_logic <- function(input, output, session) { 
  
  # Reactive values for national inputs
  national_values <- reactiveValues(
    turnout = 77.2,
    party_percentages = list(
      Arbeiderpartiet = 26.3,
      Høyre = 20.4,
      Senterpartiet = 13.5,
      Fremskrittspartiet = 11.6,
      SV = 7.6,
      Rødt = 4.7,
      Venstre = 4.6,
      MDG = 3.9,
      KrF = 3.8,
      Andre = 3.0
    )
  )
  
  # Reactive value for district overrides
  district_overrides <- reactiveValues()
  
  # Reactive value to track if overrides exist
  override_flags <- reactiveValues()
  
  # Function to collect inputs for simulation or export
  collect_inputs_with_overrides <- function(national_values, district_ratios, district_overrides, input) {
    # Assemble national inputs
    national_inputs <- data.frame(
      Nivå = "Nasjonalt",
      Region = "Nasjonalt",
      Parti = names(national_values$party_percentages),
      Prosent = unlist(lapply(names(national_values$party_percentages), function(party) {
        input[[paste0("percentage_", gsub(" ", "_", party))]]
      })),
      Usikkerhet = unlist(lapply(names(national_values$party_percentages), function(party) {
        input[[paste0("uncertainty_", gsub(" ", "_", party))]]
      })),
      stringsAsFactors = FALSE
    )
    
    # Assemble district inputs
    district_inputs <- do.call(rbind, lapply(seq_len(nrow(district_ratios)), function(i) {
      district <- district_ratios$District[i]
      data.frame(
        Nivå = "Distrikt",
        Region = district,
        Parti = names(national_values$party_percentages),
        Prosent = sapply(names(national_values$party_percentages), function(party) {
          input[[paste0(gsub(" ", "_", party), "_", gsub(" ", "_", district))]]
        }),
        Usikkerhet = sapply(names(national_values$party_percentages), function(party) {
          input[[paste0("uncertainty_", gsub(" ", "_", party), "_", gsub(" ", "_", district))]]
        }),
        stringsAsFactors = FALSE
      )
    }))
    
    # Combine national and district inputs
    full_data <- rbind(national_inputs, district_inputs)
    
    # Debugging: Print collected data
    cat("\n--- Debugging: Collected Inputs ---\n")
    print(head(full_data))
    
    return(full_data)
  }
  
  # Parse uploaded CSV and update inputs
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    
    # Read the uploaded file
    csv_data <- read.csv(input$upload_csv$datapath, encoding = "UTF-8")
    
    # Update national inputs
    national_row <- csv_data[csv_data$Nivå == "Nasjonalt", ]
    updateSliderInput(session, "turnout", value = na.omit(national_row$Prosent[national_row$Region == "Nasjonalt"])[1])
    
    lapply(names(national_values$party_percentages), function(party) {
      updateNumericInput(session, paste0("percentage_", gsub(" ", "_", party)), 
                         value = na.omit(national_row$Prosent[national_row$Parti == party])[1])
      updateNumericInput(session, paste0("uncertainty_", gsub(" ", "_", party)), 
                         value = na.omit(national_row$Usikkerhet[national_row$Parti == party])[1])
    })
    
    # Reset district overrides
    district_overrides$data <- list()
    override_flags$data <- list()
    
    # Update district inputs
    district_rows <- csv_data[csv_data$Nivå == "Distrikt", ]
    for (district in unique(district_rows$Region)) {
      district_subset <- district_rows[district_rows$Region == district, ]
      lapply(names(national_values$party_percentages), function(party) {
        updateNumericInput(session, paste0(gsub(" ", "_", party), "_", gsub(" ", "_", district)),
                           value = na.omit(district_subset$Prosent[district_subset$Parti == party])[1])
        updateNumericInput(session, paste0("uncertainty_", gsub(" ", "_", party), "_", gsub(" ", "_", district)),
                           value = na.omit(district_subset$Usikkerhet[district_subset$Parti == party])[1])
      })
    }
  })
  
  # Automatically update district values when national inputs change
  observe({
    if (is.null(district_ratios) || nrow(district_ratios) == 0) {
      return()
    }
    
    lapply(seq_len(nrow(district_ratios)), function(i) {
      district <- district_ratios$District[i]
      
      # Validate district and turnout_ratio
      if (is.null(district) || district == "") {
        return()
      }
      
      turnout_ratio <- district_ratios$turnout_ratio[i]
      if (is.null(turnout_ratio) || is.na(turnout_ratio)) {
        return()
      }
      
      district_flag <- override_flags$data[[district]]
      
      # Skip if district has a manual override
      if (!is.null(district_flag) && district_flag) {
        return()
      }
      
      # Update turnout slider
      turnout_value <- round(input$turnout * turnout_ratio, 1)
      updateSliderInput(session, paste0("turnout_", gsub(" ", "_", district)), value = turnout_value)
      
      # Update party percentages and uncertainties
      lapply(names(national_values$party_percentages), function(party) {
        ratio <- district_ratios[[party]][i]
        national_percentage <- input[[paste0("percentage_", gsub(" ", "_", party))]]
        
        if (is.null(ratio) || is.na(ratio) || is.null(national_percentage)) {
          return()
        }
        
        updated_percentage <- round(national_percentage * ratio, 1)
        updated_uncertainty <- max(0.5, 3 * (1 - abs(updated_percentage - 50) / 50))
        
        updateNumericInput(session, paste0(gsub(" ", "_", party), "_", gsub(" ", "_", district)), value = updated_percentage)
        updateNumericInput(session, paste0("uncertainty_", gsub(" ", "_", party), "_", gsub(" ", "_", district)), value = updated_uncertainty)
      })
    })
  })
  
  # Run simulation on button click
  observeEvent(input$run_simulation, {
    full_data <- collect_inputs_with_overrides(national_values, district_ratios, district_overrides$data, input)
    district_inputs <- full_data[full_data$Nivå == "Distrikt", ]
    
    cat("\n--- Debugging: Data for Simulation ---\n")
    print(head(district_inputs))
    
    results <- simulate_election(district_inputs, num_simulations = 1000)
    simulation_results(results)
  })
  
  # Export current inputs to CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("utjevneren_innstillinger-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      full_data <- collect_inputs_with_overrides(national_values, district_ratios, district_overrides$data, input)
      write.csv(full_data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

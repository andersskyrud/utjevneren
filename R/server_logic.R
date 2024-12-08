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
  
  # Parse uploaded CSV and update inputs
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    
    # Read the uploaded file
    csv_data <- read.csv(input$upload_csv$datapath, encoding = "UTF-8")
    
    # Update national inputs
    national_row <- csv_data[csv_data$Nivå == "Nasjonalt", ]
    updateSliderInput(session, "turnout", value = national_row$Prosent[national_row$Region == "Nasjonalt"])
    
    lapply(names(national_values$party_percentages), function(party) {
      updateNumericInput(session, paste0("percentage_", gsub(" ", "_", party)), 
                         value = national_row$Prosent[national_row$Parti == party])
      updateNumericInput(session, paste0("uncertainty_", gsub(" ", "_", party)), 
                         value = national_row$Usikkerhet[national_row$Parti == party])
    })
    
    # Update district inputs
    district_rows <- csv_data[csv_data$Nivå == "Distrikt", ]
    for (district in unique(district_rows$Region)) {
      district_data <- district_rows[district_rows$Region == district, ]
      lapply(names(national_values$party_percentages), function(party) {
        updateNumericInput(session, paste0(gsub(" ", "_", party), "_", gsub(" ", "_", district)),
                           value = district_data$Prosent[district_data$Parti == party])
        updateNumericInput(session, paste0("uncertainty_", gsub(" ", "_", party), "_", gsub(" ", "_", district)),
                           value = district_data$Usikkerhet[district_data$Parti == party])
      })
    }
  })
  
  # Automatically update uncertainties for national inputs
  observe({
    lapply(names(national_values$party_percentages), function(party) {
      percentage_id <- paste0("percentage_", party)
      uncertainty_id <- paste0("uncertainty_", party)
      
      percentage <- input[[percentage_id]]
      if (!is.null(percentage)) {
        # Calculate and update uncertainty
        uncertainty <- max(0.5, 3 * (1 - abs(percentage - 50) / 50))
        updateNumericInput(session, uncertainty_id, value = round(uncertainty, 1))
      }
    })
  })
  
  # Automatically update district values based on national inputs
  observe({
    for (i in 1:nrow(district_ratios)) {
      district <- district_ratios$district[i]
      
      # Update turnout for the district
      updateSliderInput(
        session,
        inputId = paste0("turnout_", gsub(" ", "_", district)),
        value = round(input$turnout * district_ratios$turnout_ratio[i], 1)
      )
      
      # Update party percentages and uncertainties for each district
      lapply(names(national_values$party_percentages), function(party) {
        percentage_id <- paste0(gsub(" ", "_", party), "_", gsub(" ", "_", district))
        uncertainty_id <- paste0("uncertainty_", gsub(" ", "_", party), "_", gsub(" ", "_", district))
        
        # Update percentage based on national input and district ratio
        national_percentage <- input[[paste0("percentage_", gsub(" ", "_", party))]]
        updated_percentage <- round(national_percentage * district_ratios[[party]][i], 1)
        
        updateNumericInput(session, inputId = percentage_id, value = updated_percentage)
        
        # Update uncertainty based on the updated percentage
        updated_uncertainty <- max(0.5, 3 * (1 - abs(updated_percentage - 50) / 50))
        updateNumericInput(session, inputId = uncertainty_id, value = round(updated_uncertainty, 1))
      })
    }
  })
  
  # Export current inputs to CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("utjevneren_innstillinger-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Collect national data
      national_data <- data.frame(
        Nivå = "Nasjonalt",
        Region = "Nasjonalt",
        Parti = names(national_values$party_percentages),
        Prosent = sapply(names(national_values$party_percentages), function(party) {
          val <- input[[paste0("percentage_", party)]]
          if (is.null(val)) 0 else val
        }),
        Usikkerhet = sapply(names(national_values$party_percentages), function(party) {
          val <- input[[paste0("uncertainty_", party)]]
          if (is.null(val)) 0.5 else val
        }),
        stringsAsFactors = FALSE
      )
      
      # Collect district data
      district_data <- do.call(rbind, lapply(1:nrow(district_ratios), function(i) {
        district <- district_ratios$district[i]
        data.frame(
          Nivå = "Distrikt",
          Region = district,
          Parti = names(national_values$party_percentages),
          Prosent = sapply(names(national_values$party_percentages), function(party) {
            val <- input[[paste0(gsub(" ", "_", party), "_", gsub(" ", "_", district))]]
            if (is.null(val)) 0 else val
          }),
          Usikkerhet = sapply(names(national_values$party_percentages), function(party) {
            val <- input[[paste0("uncertainty_", gsub(" ", "_", party), "_", gsub(" ", "_", district))]]
            if (is.null(val)) 0.5 else val
          }),
          stringsAsFactors = FALSE
        )
      }))
      
      # Combine national and district data
      full_data <- rbind(national_data, district_data)
      
      # Write to CSV
      write.csv(full_data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

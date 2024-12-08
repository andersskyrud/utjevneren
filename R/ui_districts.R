# District UI
party_names <- c(
  "Arbeiderpartiet", 
  "Høyre", 
  "Senterpartiet", 
  "Fremskrittspartiet", 
  "SV", 
  "Rødt", 
  "Venstre", 
  "MDG", 
  "KrF", 
  "Andre"
)

district_ui <- lapply(1:nrow(district_ratios), function(i) {
  district <- district_ratios$district[i]
  
  tabPanel(
    district,
    sidebarLayout(
      sidebarPanel(
        h3(paste("Stemmeandeler for", district)),
        
        # Turnout input, prefilled with district ratio and default national turnout
        sliderInput(
          inputId = paste0("turnout_", gsub(" ", "_", district)),
          label = "Valgdeltakelse (%)",
          min = 50,
          max = 100,
          value = round(77.2 * district_ratios$turnout_ratio[i], 1),  # Static value for UI setup
          step = 0.1
        ),
        
        # Inputs for parties, using consistent naming
        lapply(party_names, function(party) {
          div(
            h4(party),
            numericInput(
              inputId = paste0(gsub(" ", "_", party), "_", gsub(" ", "_", district)),
              label = paste0(party, " (%)"),
              value = ifelse(party == "Andre", 3.0, NA),  # Placeholder or static value
              min = ifelse(party == "Andre", 3, 0),
              max = ifelse(party == "Andre", 3, 100),
              step = 0.1
            ),
            numericInput(
              inputId = paste0("uncertainty_", gsub(" ", "_", party), "_", gsub(" ", "_", district)),
              label = "Usikkerhet (feilmargin)",
              value = 0.5,  # Placeholder; to be updated dynamically in the server logic
              min = 0.5,
              max = 3,
              step = 0.1
            )
          )
        })
      ),
      mainPanel(
        h3("Resultater"),
        p("Her kommer det resultater for ", district)
      )
    )
  )
})

# National UI
national_ui <- tabPanel(
  "Nasjonalt",
  sidebarLayout(
    sidebarPanel(
      h3("Nasjonale stemmeandeler"),
      
      # Input for nasjonal valgdeltakelse (slider)
      sliderInput(
        inputId = "turnout",
        label = "Valgdeltakelse (%)",
        min = 50,
        max = 100,
        value = 77.2,  # Default value from server_logic
        step = 0.1
      ),
      
      # Input-tabel for partier
      lapply(c("Arbeiderpartiet", "Høyre", "Senterpartiet", "Fremskrittspartiet", 
               "Sosialistisk_Venstreparti", "Rødt", "Venstre", 
               "Miljøpartiet_De_Grønne", "Kristelig_Folkeparti"), function(party) {
                 default_values <- list(
                   Arbeiderpartiet = 26.3,
                   Høyre = 20.4,
                   Senterpartiet = 13.5,
                   Fremskrittspartiet = 11.6,
                   Sosialistisk_Venstreparti = 7.6,
                   Rødt = 4.7,
                   Venstre = 4.6,
                   Miljøpartiet_De_Grønne = 3.9,
                   Kristelig_Folkeparti = 3.8
                 )
                 
                 div(
                   h4(party),
                   numericInput(
                     inputId = paste0("percentage_", gsub(" ", "_", party)),
                     label = "Stemmeandel (%)",
                     value = default_values[[party]],  # Use default values here
                     min = 0,
                     max = 100,
                     step = 0.1
                   ),
                   numericInput(
                     inputId = paste0("uncertainty_", gsub(" ", "_", party)),
                     label = "Usikkerhet (feilmargin)",
                     value = max(0.5, 3 * (1 - abs(default_values[[party]] - 50) / 50)),  # Default uncertainty
                     min = 0.5,
                     max = 3,
                     step = 0.1
                   )
                 )
               }),
      
      # File input for CSV import
      fileInput(
        inputId = "upload_csv",
        label = "Last opp CSV-fil",
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
      ),
      
      # Export button for CSV download
      downloadButton(
        outputId = "download_csv",
        label = "Eksporter til CSV"
      )
    ),
    mainPanel(
      h3("Resultater"),
      p("Her kommer det resultater.")
    )
  )
)

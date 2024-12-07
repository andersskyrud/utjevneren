# UI for the national tab
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
        value = 75,
        step = 0.1
      ),
      
      # Input-tabel for partier
      lapply(c("Arbeiderpartiet", "Høyre", "Senterpartiet", "Fremskrittspartiet", 
               "Sosialistisk Venstreparti", "Rødt", "Venstre", 
               "Miljøpartiet De Grønne", "Kristelig Folkeparti"), function(party) {
                 div(
                   h4(party),
                   numericInput(
                     inputId = paste0("percentage_", gsub(" ", "_", party)),
                     label = "Stemmeandel (%)",
                     value = 10,
                     min = 0,
                     max = 100,
                     step = 0.1
                   ),
                   numericInput(
                     inputId = paste0("uncertainty_", gsub(" ", "_", party)),
                     label = "Usikkerhet (feilmargin)",
                     value = NA,
                     min = 0.5,
                     max = 3,
                     step = 0.1
                   )
                 )
               }),
      
      actionButton("submit", "Send inn verdier")
    ),
    mainPanel(
      h3("Resultater"),
      p("Her kommer det resultater.")
    )
  )
)

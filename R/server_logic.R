# Server-side logic
server_logic <- function(input, output, session) {
  # Automatisk kalkulering av usikkerhetsmargin
  observe({
    lapply(c("Arbeiderpartiet", "Høyre", "Senterpartiet", "Fremskrittspartiet", 
             "Sosialistisk Venstreparti", "Rødt", "Venstre", 
             "Miljøpartiet De Grønne", "Kristelig Folkeparti"), function(party) {
               percentage_id <- paste0("percentage_", gsub(" ", "_", party))
               uncertainty_id <- paste0("uncertainty_", gsub(" ", "_", party))
               
               percentage <- input[[percentage_id]]
               if (!is.null(percentage)) {
                 # Kalkuler usikkerhetsmargin
                 uncertainty <- max(0.5, 3 * (1 - abs(percentage - 50) / 50))
                 updateNumericInput(session, uncertainty_id, value = round(uncertainty, 1))
               }
             })
  })
}

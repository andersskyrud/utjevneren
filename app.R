# =====================
#       Utjevneren     
# =====================
#
# Av: Anders Skyrud Danielsen (MDG)
# Opprettet: 07.12.2024

# Installere første pakka hvis man ikke har den
# install.packages("pacman")

# Last inn nødvendige biblioteker
pacman::p_load(
  shiny
)

# Source modular scripts
source("R/ui_national.R")    # UI for the national tab
source("R/ui_districts.R")   # UI for the district tabs
source("R/server_logic.R")   # Server-side logic

# Kombiner UI-komponenter
ui <- fluidPage(
  titlePanel("Utjevneren"),
  do.call(tabsetPanel, c(
    list(national_ui),  # National input panel
    district_ui         # District input panels
  ))
)

# Server-definisjon
server <- function(input, output, session) {
  server_logic(input, output, session)  # Call server logic from a separate script
}

# Start Shiny-applikasjonen
shinyApp(ui = ui, server = server)

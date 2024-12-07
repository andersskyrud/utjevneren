# Full data for turnout and party ratios by district (relative to national averages)
district_ratios <- data.frame(
  district = c(
    "Aust-Agder", "Vest-Agder", "Akershus", "Buskerud", "Finnmark", "Hedmark", 
    "Hordaland", "Møre og Romsdal", "Nordland", "Oppland", "Oslo", "Rogaland", 
    "Sogn og Fjordane", "Telemark", "Troms", "Nord-Trøndelag", 
    "Sør-Trøndelag", "Vestfold", "Østfold"
  ),
  turnout_ratio = c(
    0.982, 0.996, 1.0, 0.974, 0.932, 0.985, 1.035, 1.005, 0.96, 0.971, 1.017, 1.011, 
    1.033, 0.965, 0.97, 0.991, 1.017, 0.992, 0.945
  ),
  Arbeiderpartiet = c(
    0.931, 0.792, 0.981, 1.08, 1.194, 1.258, 0.863, 0.769, 1.096, 1.33, 0.874, 
    0.852, 1.008, 1.171, 1.031, 1.277, 1.133, 1.027, 1.16
  ),
  Høyre = c(
    0.768, 0.808, 1.343, 1.078, 0.333, 0.519, 1.201, 0.799, 0.75, 0.612, 1.152, 
    1.172, 0.671, 0.769, 0.662, 0.519, 0.808, 1.23, 0.916
  ),
  Senterpartiet = c(
    1.0, 0.763, 0.652, 1.193, 1.363, 2.074, 0.733, 1.296, 1.57, 2.126, 0.227, 
    0.771, 1.057, 1.222, 1.415, 2.133, 1.119, 0.733, 1.044
  ),
  Fremskrittspartiet = c(
    1.155, 1.129, 0.905, 1.052, 0.931, 0.724, 1.086, 1.905, 1.052, 0.637, 0.517, 
    1.448, 0.689, 1.095, 1.207, 0.69, 0.741, 0.845, 1.095
  ),
  SV = c(
    0.724, 0.697, 0.895, 0.737, 0.816, 0.907, 1.171, 0.829, 0.947, 0.737, 1.75, 
    0.671, 0.776, 0.803, 1.421, 0.749, 1.197, 0.816, 0.803
  ),
  Rødt = c(
    0.787, 0.68, 0.83, 0.745, 1.064, 0.723, 1.0, 0.702, 1.149, 0.766, 1.766, 
    0.787, 0.851, 0.978, 1.021, 0.851, 0.894, 0.936, 0.978
  ),
  Venstre = c(
    0.696, 0.783, 1.5, 0.783, 0.304, 0.5, 0.935, 0.63, 0.543, 0.522, 2.174, 
    0.761, 0.739, 0.5, 0.543, 0.435, 0.957, 1.0, 0.652
  ),
  MDG = c(
    0.769, 0.769, 1.205, 0.769, 0.584, 0.538, 1.0, 0.641, 0.589, 0.59, 2.179, 
    0.615, 0.641, 0.718, 0.769, 0.487, 1.231, 1.0, 0.769
  ),
  KrF = c(
    2.289, 3.658, 0.526, 0.605, 0.474, 0.421, 1.289, 1.395, 1.053, 0.421, 0.474, 
    2.132, 1.0, 1.184, 0.579, 0.605, 0.579, 1.0, 0.868
  ),
  check.names = FALSE
)

# UI for district tabs
district_ui <- lapply(1:nrow(district_ratios), function(i) {
  district <- district_ratios$district[i]
  
  tabPanel(
    district,
    sidebarLayout(
      sidebarPanel(
        h3(paste("Stemmeandeler for", district)),
        
        # Turnout input, prefilled with adjusted default
        sliderInput(
          inputId = paste0("turnout_", gsub(" ", "_", district)),
          label = "Valgdeltakelse (%)",
          min = 50,
          max = 100,
          value = round(input$turnout * district_ratios$turnout_ratio[i], 1),
          step = 0.1
        ),
        
        # Inputs for parties, prefilled with adjusted national defaults
        lapply(names(district_ratios)[-c(1:2)], function(party) {
          div(
            h4(party),
            numericInput(
              inputId = paste0(gsub(" ", "_", party), "_", gsub(" ", "_", district)),
              label = paste0(party, " (%)"),
              value = round(input[[paste0("percentage_", gsub(" ", "_", party))]] * district_ratios[[party]][i], 1),
              min = 0,
              max = 100,
              step = 0.1
            ),
            numericInput(
              inputId = paste0("uncertainty_", gsub(" ", "_", party), "_", gsub(" ", "_", district)),
              label = "Usikkerhet (feilmargin)",
              value = max(0.5, 3 * (1 - abs(input[[paste0("percentage_", gsub(" ", "_", party))]] - 50) / 50)),
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

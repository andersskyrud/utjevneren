# Navn på fylkene
districts <- c(
  "Aust-Agder", "Vest-Agder", "Akershus", "Buskerud", "Finnmark",
  "Hedmark", "Hordaland", "Møre og Romsdal", "Nordland", "Oppland",
  "Oslo", "Rogaland", "Sogn og Fjordane", "Telemark", "Troms",
  "Nord-Trøndelag", "Sør-Trøndelag", "Vestfold", "Østfold"
)

# UI for district tabs
district_ui <- lapply(districts, function(district) {
  tabPanel(
    district,
    h3(paste("Her kan du legge til inputs for fylke", district))
  )
})

#grafiken_uebersicht <- read_excel("./Data/metadaten_grafiken_kantonale_Abstimmungen.xlsx")

grafiken_uebersicht <- datawrapper_codes %>%
  filter(grepl("Kantonale Vorlage",Typ) == TRUE)

for (i in 1:nrow(grafiken_uebersicht)) {

metadata_chart <- dw_retrieve_chart_metadata(grafiken_uebersicht$ID[i])

dw_edit_chart(grafiken_uebersicht$ID[i],
              visualize = list("mapView" = "crop"))

dw_publish_chart(grafiken_uebersicht$ID[i])
}


manual_chart_ids <- c("jpSC2", "AwUMb", "We7qQ", "BKhMU", #FR_TOP
                      "ujwRW", "J9buZ", "wj6hZ", "8lrnE", #FR_FLOP
                      "fLrtj","7BcTp", "RrCQ0", "UgJCz", #DE_TOP
                      "cy8UG", "AB5yf", "ZDQNp", "Sw6RJ", #DE_FLOP
                      "hHGul", "6t8Rv", "TzGzo","LMTuR",  #IT_TOP,
                      "uQWKQ", "oSdPG", "7Pwj6", "4FmJW") #IT_FLOP 





#manual_chart_ids <- c("WEJLY","qAifh",
  #                   "dl62W", "uaVK1",
     #                "5Dhvv", "Moyx9",
      #               "hHFLf")


manual_chart_summary <- data.frame("",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "",
                                   "")

manual_chart_summary <- data.frame(matrix(ncol = length(manual_chart_summary), nrow = 0))

colnames(manual_chart_summary) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")


#retrive_test <- dw_retrieve_chart_metadata("Sw6RJ")

#retrive_test$content$title

for (chart_id in manual_chart_ids) {
  
  # Récupérer les métadonnées du graphique
  metadata_chart <- DatawRappr::dw_retrieve_chart_metadata(chart_id)
  
  # Créer une nouvelle entrée de métadonnées
  new_entry <- data.frame(
    "Typ" = "Top10",
    "Vorlage" = "alle",
    "Titel" = metadata_chart$content$title,
    "Sprache" = metadata_chart$content$language,
    "ID" = metadata_chart$id,
    "Link" = metadata_chart$content$publicUrl,
    "Iframe" = metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
    "Script" = metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`
  )
  
  # Ajouter la nouvelle entrée au tableau récapitulatif
  manual_chart_summary <- rbind(manual_chart_summary, new_entry)
}

writexl::write_xlsx(manual_chart_summary, "C:/Users/yove/Documents/R/selfpick/data-raw/resources/vot_fed_11_2024/top_flop_summaries.xlsx")

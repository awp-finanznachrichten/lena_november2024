#Set Working Path
MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20241124_LENA_Abstimmungen"
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

###Load JSON Data
source("./Config/load_json_data.R",encoding = "UTF-8")

Vorlagen_Titel <- as.data.frame(read_excel(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"), 
                                           sheet = "Vorlagen_Uebersicht"))

sprachen <- c("de","fr","it")

monate_de <- c("Januar", "Februar", "März", 
  "April", "Mai", "Juni", "July", 
  "August", "September", "Oktober",
  "November", "Dezember")

monate_fr <- c("janvier","février","mars",
               "avril","mai","juin","juillet",
               "août","septembre","octobre",
               "novembre","décembre")

monate_it <- c("gennaio","febbraio","marzo",
               "aprile","maggio","giugno",
               "luglio","agosto","settembre",
               "ottobre","novembre","dicembre")

#Ids von Karten-Vorlagen
vorlagen_uebersicht <- c("O1i9P","Ocame","ot8Mm")
vorlagen_gemeinden <- c("EuC56","JJ03i","CPwql")
vorlagen_kantone_special_overview <- c("83ecV","SZmQU","7wXV4")
vorlagen_kantone_special_initiative <- c("HpDco","LS2ff","FAOHL")
vorlagen_kantone_special_gegenvorschlag <- c("Gu1as","FsoiR","qiUTe")
vorlagen_kantone_special_stichentscheid <- c("Cc5YB","cDy1R","ygRKA")

#Titel aktuelle Vorlagen
vorlagen_all <- rbind(vorlagen,vorlagen_fr)
vorlagen_all <- rbind(vorlagen_all,vorlagen_it)

#Load Folders
folder_kantonal <- readRDS("./Preparations/folders/folder_kantonal.RDS")
folder_kantone_uebersicht <- readRDS("./Preparations/folders/folder_kantone_uebersicht.RDS")

###Kantonale Abstimmungen
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")

if (length(kantonal_short) > 0) {
for (k in 1:length(kantonal_short)) {
  #Get Title and Info
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number[k]]][["vorlagenId"]][kantonal_add[k]]

  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)

if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
  data_chart <- dw_copy_chart(vorlagen_gemeinden[1])
  created_folder <- dw_create_folder(paste0(kantonal_short[k],"_DE"),parent_id = folder_kantonal$id) 

  dw_edit_chart(data_chart$id,
                title=paste0(substr(kantonal_short[k],1,2),": ",Vorlagen_Info$Vorlage_d),
                intro = "&nbsp;",
                annotate = "&nbsp;",
                folderId = created_folder$id,
                data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                 gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                 "/master/Output_Cantons/",kantonal_short[k],"_dw_",sprachen[1],".csv")),
                visualize=list("hide-empty-regions" = TRUE))
  
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
 
  new_entry <- data.frame("Kantonale Vorlage",
                          kantonal_short[k],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}
if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[2])
    created_folder <- dw_create_folder(paste0(kantonal_short[k],"_FR"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=paste0(substr(kantonal_short[k],1,2),": ",Vorlagen_Info$Vorlage_f),
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short[k],"_dw_",sprachen[2],".csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage",
                            kantonal_short[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[3])
    created_folder <- dw_create_folder(paste0(kantonal_short[k],"_IT"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=paste0(substr(kantonal_short[k],1,2),": ",Vorlagen_Info$Vorlage_i),
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short[k],"_dw_",sprachen[3],".csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage",
                            kantonal_short[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
}
}

#Kantonale Abstimmungen Special
if (length(kantonal_short_special)) {
for (k in 1:length(kantonal_short_special)) {
  
  #Übersicht
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number_special[k]]][["vorlagenId"]][kantonal_add_special[k]]
  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)
  
  if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_overview[1])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Overview_DE"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_d,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[1],"_overview.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Overview",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_overview[2])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Overview_FR"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[2],"_overview.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Overview",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_initiative[3])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Overview_IT"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[3],"_overview.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Overview",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  
  
  
  #Initiative
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number_special[k]]][["vorlagenId"]][kantonal_add_special[k]]
  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)

  if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_initiative[1])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Initiative_DE"),parent_id = folder_kantonal$id)

    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_d,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[1],"_initiative.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Initiative",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_initiative[2])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Initiative_FR"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[2],"_initiative.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Initiative",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_initiative[3])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Initiative_IT"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[3],"_initiative.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Initiative",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }

  #Gegenvorschlag
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number_special[k]]][["vorlagenId"]][kantonal_add_special[k]+1]
  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)

  if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_gegenvorschlag[1])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Gegenvorschlag_DE"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_d,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[1],"_gegenvorschlag.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Gegenvorschlag",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_gegenvorschlag[2])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Gegenvorschlag_FR"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[2],"_gegenvorschlag.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Gegenvorschlag",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_gegenvorschlag[3])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Gegenvorschlag_IT"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[3],"_gegenvorschlag.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Gegenvorschlag",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  
  #Stichentscheid
  vorlage_id <- json_data_kantone[["kantone"]][["vorlagen"]][[kantonal_number_special[k]]][["vorlagenId"]][kantonal_add_special[k]+2]
  Vorlagen_Info <- Vorlagen_Titel %>%
    filter(Vorlage_ID == vorlage_id)
  
  if (is.na(Vorlagen_Info$Vorlage_d) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_stichentscheid[1])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Stichentscheid_DE"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_d,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[1],"_stichentscheid.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Stichentscheid",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_f) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_stichentscheid[2])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Stichentscheid_FR"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_f,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[2],"_stichentscheid.csv")),
                  visualize=)
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Stichentscheid",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (is.na(Vorlagen_Info$Vorlage_i) == FALSE) {
    data_chart <- dw_copy_chart(vorlagen_kantone_special_stichentscheid[3])
    created_folder <- dw_create_folder(paste0(kantonal_short_special[k],"_Stichentscheid_IT"),parent_id = folder_kantonal$id)
    
    dw_edit_chart(data_chart$id,
                  title=Vorlagen_Info$Vorlage_i,
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = created_folder$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",kantonal_short_special[k],"_dw_",sprachen[3],"_stichentscheid.csv")),
                  visualize=list("hide-empty-regions" = TRUE))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame("Kantonale Vorlage Stichentscheid",
                            kantonal_short_special[k],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
}
}


###Overviews Kantone
kantone_list <- json_data_kantone[["kantone"]]

for (k in 1:nrow(kantone_list)) {
vorlagen <- kantone_list$vorlagen[[k]]

vorlage_titel <- vorlagen$vorlagenTitel[[1]]
vorlage_titel <- vorlage_titel %>%
    filter(nchar(text) > 5,
           langKey != "rm")

for (v in 1:nrow(vorlage_titel)) {
  if (vorlage_titel$langKey[v] == "de") {
  titel <- paste0(kantone_list$geoLevelname[k],": Kantonale Abstimmungen vom ",day(voting_date),". ",monate_de[month(voting_date)]," ",year(voting_date))
  l <- 1
  }
  if (vorlage_titel$langKey[v] == "fr") {
  titel <- paste0(kantone_list$geoLevelname[k],": Votations cantonales du ",day(voting_date)," ",monate_fr[month(voting_date)]," ",year(voting_date))
  l <- 2
  }
  if (vorlage_titel$langKey[v] == "it") {
  titel <- paste0(kantone_list$geoLevelname[k],": Votazione cantonale del ",day(voting_date)," ",monate_it[month(voting_date)]," ",year(voting_date))
  l <- 3
  }

  data_chart <- dw_copy_chart(vorlagen_uebersicht[l])
  dw_edit_chart(data_chart$id,
                title=titel,
                folderId = folder_kantone_uebersicht$id) 
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
  new_entry <- data.frame("Uebersicht Kanton",
                          kantone_list$geoLevelname[k],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}  
}

#Daten Speichern
grafiken_uebersicht <- grafiken_uebersicht[-1,]
library(xlsx)
write.xlsx(grafiken_uebersicht,"./Data/metadaten_grafiken_kantonal.xlsx",row.names = FALSE)

#Enter Data in DB
mydb <- connectDB(db_name = "sda_votes")
for (i in 1:nrow(grafiken_uebersicht)) {
  sql_qry <- paste0("INSERT IGNORE INTO datawrapper_codes(Typ,Vorlage,Sprache,ID,date) VALUES ",
                    "('",grafiken_uebersicht$Typ[i],"','",
                    grafiken_uebersicht$Vorlage[i],"','",
                    grafiken_uebersicht$Sprache[i],"','",
                    grafiken_uebersicht$ID[i],"','",
                    voting_date,"')")
  rs <- dbSendQuery(mydb, sql_qry)
}

dbDisconnectAll()


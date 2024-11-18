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
vorlagen_kantone <- c("HH2Hs","G7A2k","sobvY")

#Titel aktuelle Vorlagen
vorlagen_all <- rbind(vorlagen,vorlagen_fr)
vorlagen_all <- rbind(vorlagen_all,vorlagen_it)


#Load Folders
folder_uebersicht <- readRDS("./Preparations/folders/folder_uebersicht.RDS")
folder_gemeindeebene <- readRDS("./Preparations/folders/folder_gemeindeebene.RDS")
folder_kantonsebene <- readRDS("./Preparations/folders/folder_kantonsebene.RDS")
folder_kantone <- readRDS("./Preparations/folders/folder_kantone.RDS")


###CREATE CHARTS OVERVIEWS AND SWISS MAPS GEMEINDEN UND KANTONE###
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")

##OVERVIEW
titel_de <- paste0("Aktueller Stand der Abstimmungen vom ",day(voting_date),". ",monate_de[month(voting_date)]," ",year(voting_date))
titel_fr <- paste0("Etat actuel des votes au ",day(voting_date)," ",monate_fr[month(voting_date)]," ",year(voting_date))
titel_it <- paste0("Situazione attuale delle votazioni del ",day(voting_date)," ",monate_it[month(voting_date)]," ",year(voting_date))

titel_all <- c(titel_de,titel_fr,titel_it)

for (i in 1:3) {
data_chart <- dw_copy_chart(vorlagen_uebersicht[i])
dw_edit_chart(data_chart$id,
              title=titel_all[i],
              folderId = folder_uebersicht$id)
dw_publish_chart(data_chart$id)
metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)

new_entry <- data.frame("Uebersicht",
                        "alle",
                        metadata_chart$content$title,
                        metadata_chart$content$language,
                        metadata_chart$id,
                        metadata_chart$content$publicUrl,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                        metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
}

##SWISS MAPS GEMEINDEN UND KANTONE
for (v in 1:length(vorlagen_short)) {
  title_select <- c(v,v+length(vorlagen_short),v+length(vorlagen_short)+length(vorlagen_short))

  #Alle drei Sprachen
  for (i in 1:3) {
  #Gemeinden  
  data_chart <- dw_copy_chart(vorlagen_gemeinden[i])
  dw_edit_chart(data_chart$id,
                title=vorlagen_all$text[title_select[i]],
                folderId = folder_gemeindeebene$id,
                data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                 gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                 "/master/Output_Switzerland/",vorlagen_short[v],"_dw_",sprachen[i],".csv")))
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)

  new_entry <- data.frame("Schweizer Gemeinden",
                          vorlagen_short[v],
                          metadata_chart$content$title,
                          metadata_chart$content$language,
                          metadata_chart$id,
                          metadata_chart$content$publicUrl,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                          metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
  colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
  grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  
  #Kantone
  data_chart <- dw_copy_chart(vorlagen_kantone[i])
  dw_edit_chart(data_chart$id,
                title=vorlagen_all$text[title_select[i]],
                folderId = folder_kantonsebene$id,
                data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                 gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                 "/master/Output_Switzerland/",vorlagen_short[v],"_dw_kantone.csv")))
  dw_publish_chart(data_chart$id)
  metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
  
  new_entry <- data.frame("Schweizer Kantone",
                          vorlagen_short[v],
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
write.xlsx(grafiken_uebersicht,"./Data/metadaten_grafiken.xlsx",row.names = FALSE)

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

###CREATE CHARTS MAPS KANTONE###
grafiken_uebersicht <- data.frame("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
colnames(grafiken_uebersicht) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")

for (c in 1:nrow(cantons_overview)) {
folder_kanton <- dw_create_folder(cantons_overview$area_ID[c],parent_id = folder_kantone$id)
for (v in 1:length(vorlagen_short)) {
  if (grepl("de",cantons_overview$languages[c]) == TRUE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[1])
    dw_edit_chart(data_chart$id,
                  title=paste0(cantons_overview$area_ID[c],": ",Vorlagen_Titel$Vorlage_d[v]),
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = folder_kanton$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[v],"_dw_de.csv")),
                  visualize = list("mapView" = "crop"))
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame(paste0("Kanton ",cantons_overview$area_ID[c]),
                            vorlagen_short[v],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (grepl("fr",cantons_overview$languages[c]) == TRUE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[2])
    dw_edit_chart(data_chart$id,
                  title=paste0(cantons_overview$area_ID[c],": ",Vorlagen_Titel$Vorlage_f[v]),
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = folder_kanton$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[v],"_dw_fr.csv")),
                  visualize = list("mapView" = "crop"))
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame(paste0("Kanton ",cantons_overview$area_ID[c]),
                            vorlagen_short[v],
                            metadata_chart$content$title,
                            metadata_chart$content$language,
                            metadata_chart$id,
                            metadata_chart$content$publicUrl,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-responsive`,
                            metadata_chart$content$metadata$publish$`embed-codes`$`embed-method-web-component`)
    colnames(new_entry) <- c("Typ","Vorlage","Titel","Sprache","ID","Link","Iframe","Script")
    grafiken_uebersicht <- rbind(grafiken_uebersicht,new_entry)
  }
  if (grepl("it",cantons_overview$languages[c]) == TRUE) {
    data_chart <- dw_copy_chart(vorlagen_gemeinden[3])
    dw_edit_chart(data_chart$id,
                  title=paste0(cantons_overview$area_ID[c],": ",Vorlagen_Titel$Vorlage_i[v]),
                  intro = "&nbsp;",
                  annotate = "&nbsp;",
                  folderId = folder_kanton$id,
                  data=list("external-data"=paste0("https://raw.githubusercontent.com/awp-finanznachrichten/lena_",
                                                   gsub("ä","ae",tolower(monate_de[month(voting_date)])),year(voting_date),
                                                   "/master/Output_Cantons/",cantons_overview$area_ID[c],"_",vorlagen_short[v],"_dw_it.csv")),
                  visualize = list("mapView" = "crop"))
    
    dw_publish_chart(data_chart$id)
    metadata_chart <- dw_retrieve_chart_metadata(data_chart$id)
    
    new_entry <- data.frame(paste0("Kanton ",cantons_overview$area_ID[c]),
                            vorlagen_short[v],
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

#Daten Speichern
grafiken_uebersicht <- grafiken_uebersicht[-1,]
library(xlsx)
write.xlsx(grafiken_uebersicht,"./Data/metadaten_grafiken_kantonskarten.xlsx",row.names = FALSE)

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
MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20241124_LENA_Abstimmungen"

#Working Directory definieren
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

#SET ADDITIONAL CONSTANTS
VOTATION_IDS_SRG <- c(5081,5082)
CATCHWORDS_DE <- c("Verkehr","Mietrecht","Mietrecht","Gesundheit")
CATCHWORDS_FR <- c("Verkehr","Mietrecht","Mietrecht","Gesundheit")
CATCHWORDS_IT <- c("Verkehr","Mietrecht","Mietrecht","Gesundheit")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

###Load texts for VOT-Tool###
texts_canton <-  as.data.frame(read_excel("Texte/Textbausteine Vot-Tool 2.0.xlsx", sheet = "Resultate_Kanton"))
texts_intermediate <-  as.data.frame(read_excel("Texte/Textbausteine Vot-Tool 2.0.xlsx", sheet = "Zwischenstand_Vorlage"))
texts_final_result <-  as.data.frame(read_excel("Texte/Textbausteine Vot-Tool 2.0.xlsx", sheet = "Endergebnis_Vorlage"))
texts_trend <-  as.data.frame(read_excel("Texte/Textbausteine Vot-Tool 2.0.xlsx", sheet = "Flash_Trend_Vorlage"))
texts_extrapolation <-  as.data.frame(read_excel("Texte/Textbausteine Vot-Tool 2.0.xlsx", sheet = "Flash_Hochrechnung_Vorlage"))
texts_staendemehr <-  as.data.frame(read_excel("Texte/Textbausteine Vot-Tool 2.0.xlsx", sheet = "Flash_Staendemehr_Vorlage"))

repeat{
###Load JSON Data
source("./Config/load_json_data.R",encoding = "UTF-8")

#SRG Hochrechnungen -> Generate Flashes and send Mail if new data available
source("./Vot-Tool/SRG_API_Request.R", encoding = "UTF-8")

###Write Data in DB###
source("./Vot-Tool/write_DB_entries.R", encoding = "UTF-8")

###Load DBs
source("./Vot-Tool/load_DBs.R", encoding = "UTF-8")

#####CREATE NEWS AND SEND MAIL IF CANTON IS COMPLETE#####
for (i in 1:nrow(output_overview)) {
    canton_results <- cantons_results %>%
      filter(area_ID == output_overview$area_ID[i]) 

    canton_metadata <- meta_kt %>%
      filter(area_ID == output_overview$area_ID[i])

    if (sum(canton_results$final_results) == nrow(vorlagen)) {
      #SEND MAIL
      if (output_overview$mail_results[i] == "pending") {
      print(paste0("sending mail with results of canton ",canton_results$area_ID[1],"..."))
      source("./Vot-Tool/send_mail_cantonal.R", encoding="UTF-8")
      }  
      
      #CREATE MARS NEWS
      if (output_overview$news_results[i] == "pending") {
      print(paste0("creating mars news with results of canton ",canton_results$area_ID[1],"..."))
      source("./Vot-Tool/create_news_cantonal.R", encoding="UTF-8") 
      }  
    }
  }  
  
#####CREATE FLASH AM STÄNDEMEHR GESCHEITERT (IF NEEDED)####
for (v in 1:nrow(vorlagen)) {
if (output_flashes[output_flashes$votes_ID == vorlagen$id[v],]$flash_staendemehr == "pending") {  
  
  ###GET STAENDE AND YES/NO CANTONS###  
  staende <- meta_kt %>%
    select(area_ID,staende_count)
  all_cantons <- cantons_results %>%
    filter(votes_ID == vorlagen$id[v]) %>%
    left_join(staende)
  
  no_cantons <- all_cantons %>%
    filter(result == "no")
  staende_no <- 0
  if (nrow(no_cantons) > 0) {
    staende_no <- sum(no_cantons$staende_count)
  }  
  if (staende_no > 11) { 
  print(paste0("Vorlage ",vorlagen$text[v]," am Ständemehr gescheitert!"))
    
  storyboard <- get_story_staendemehr()
  for (language in sprachen) {
    texts <- get_texts_vot(storyboard,
                           texts_staendemehr,
                           language)
    texts <- replace_variables_vot(texts,
                                   language)
    source("./Vot-Tool/create_flash_staendemehr.R", encoding="UTF-8") 
  }
  #Set output to done
  mydb <- connectDB(db_name = "sda_votes")  
  sql_qry <- paste0("UPDATE output_flashes SET flash_staendemehr = 'done' WHERE votes_ID = '",vorlagen$id[v],"'")
  rs <- dbSendQuery(mydb, sql_qry)
  dbDisconnectAll() 
  
  #Send Mail
  Subject <- paste0("***TEST***Vorlage ",vorlagen$text[v]," am Ständemehr gescheitert")
  Body <- paste0("Liebes Keystone-SDA-Team,\n\n",
                 "Die Vorlage ",vorlagen$text[v]," ist offiziell am Ständemehr gescheitert.\n\n",
                 "Ihr findet die Flashes dazu im Mars im Input-Ordner Lena.\n\n",
                 "Liebe Grüsse\n\nLENA")
  send_notification(Subject,
                    Body,
                    paste0(DEFAULT_MAILS))  
}  
}  
}
  
#####CREATE INTERMEDIATE NEWS####
if (Sys.time() > output_news_intermediate$timestamp[1]) {
  print("Generiere Zwischenstands-Meldungen...")
  source("./Vot-Tool/create_news_intermediate.R", encoding="UTF-8") 
}

#####CREATE ELECTION COMPLETED REPORT####
if (sum(grepl("pending",output_overview$news_results)) == 0) {
  print("Alle Abstimmungsresultate komplett!")
if (output_overview_national$mail_results[1] == "pending") { 
source("./Vot-Tool/send_mail_election_completed.R", encoding="UTF-8")
}  
if (output_overview_national$news_results[1] == "pending") { 
source("./Vot-Tool/create_news_election_completed.R", encoding="UTF-8") 
}   
}  
Sys.sleep(5)
}  


MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20241124_LENA_Abstimmungen"
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

###Load texts and metadata###
source("./Config/load_texts_metadata.R",encoding = "UTF-8")

source("./Config/load_json_data.R",encoding = "UTF-8")


#Results canton votes
kantone_list <- json_data_kantone[["kantone"]]
mydb <- connectDB(db_name = "sda_votes")
for (k in 1:nrow(kantone_list)) {
  sql_qry <- paste0("INSERT IGNORE INTO output_overview(date,area_ID,voting_type) VALUES ",
                    "('",voting_date,"','",kantone_list$geoLevelname[k],"','cantonal')")
  rs <- dbSendQuery(mydb, sql_qry)
}

dbDisconnectAll()


#Enter Metadata from Spreadsheet
metadata <- as.data.frame(read_excel(paste0("Texte/Textbausteine_LENA_",abstimmung_date,".xlsx"), 
                                          sheet = "Vorlagen_Uebersicht"))

metadata$Vorlage_d <- str_replace_all(metadata$Vorlage_d ,"'","\\\\'")
metadata$Vorlage_f <- str_replace_all(metadata$Vorlage_f ,"'","\\\\'")
metadata$Vorlage_i <- str_replace_all(metadata$Vorlage_i ,"'","\\\\'")

mydb <- connectDB(db_name = "sda_votes")
for (m in 1:nrow(metadata)) {
  sql_qry <- paste0("INSERT IGNORE INTO votes_metadata(votes_ID,date,area_ID,title_de,title_fr,title_it,catchword_de,catchword_fr,catchword_it,status,remarks,type,staendemehr) VALUES ",
                    "('",metadata$Vorlage_ID[m],"','",date_voting,"','",metadata$Kanton[m],"','",
                    metadata$Vorlage_d[m],"','",metadata$Vorlage_f[m],"','",metadata$Vorlage_i[m],"','",
                    metadata$catchword_de[m],"','",metadata$catchword_fr[m],"','",metadata$catchword_it[m],
                    "','upcoming','canton_comparison','initiative','yes')")
  rs <- dbSendQuery(mydb, sql_qry)
}
dbDisconnectAll()

#Set Working Path
MAIN_PATH <- "C:/Users/sw/OneDrive/SDA_eidgenoessische_abstimmungen/20241124_LENA_Abstimmungen"
setwd(MAIN_PATH)

#Load Libraries and Functions
source("./Config/load_libraries_functions.R",encoding = "UTF-8")

###Set Constants###
source("./Config/set_constants.R",encoding = "UTF-8")

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

#Ordnerstruktur erstellen
TEAM_ID <- "6Gn1afus"

main_folder <- dw_create_folder(name=paste0("Abstimmung ",day(voting_date),". ",monate_de[month(voting_date)]," ",year(voting_date)),organization_id = TEAM_ID)

folder_eid <- dw_create_folder("Eidgenössische Abstimmungen",parent_id = main_folder$id)
folder_kantonal <- dw_create_folder("Kantonale Abstimmungen",parent_id = main_folder$id)

folder_uebersicht <- dw_create_folder("_Übersicht",parent_id = folder_eid$id)
folder_einzugsgebiete <- dw_create_folder("Einzugsgebiete",parent_id = folder_eid$id)
folder_kantone <- dw_create_folder("Kantone",parent_id = folder_eid$id)
folder_schweiz <- dw_create_folder("Schweiz",parent_id = folder_eid$id)

folder_gemeindeebene <- dw_create_folder("Gemeindeebene",parent_id = folder_schweiz$id)
folder_kantonsebene <- dw_create_folder("Kantonsebene",parent_id = folder_schweiz$id)

folder_kantone_uebersicht <- dw_create_folder("_Übersicht",parent_id = folder_kantonal$id)

#Save folders
saveRDS(folder_uebersicht,file="./Preparations/folder_uebersicht.RDS")
saveRDS(folder_gemeindeebene,file="./Preparations/folder_gemeindeebene.RDS")
saveRDS(folder_kantonsebene,file="./Preparations/folder_kantonsebene.RDS")
saveRDS(folder_kantone,file="./Preparations/folder_kantone.RDS")
saveRDS(folder_kantonal,file="./Preparations/folder_kantonal.RDS")
saveRDS(folder_kantone_uebersicht,file="./Preparations/folder_kantone_uebersicht.RDS")

###Deutsch
date_and_time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),"+02:00")

#ID
ID <- sample(100000000000000:999999999999999,1)

#ID Long
ID_long <- paste0(format(Sys.Date(), "%Y%m%d"),":",format(Sys.time(), "%Y%m%d%H%M%S"),ID)

#Vorlage laden
vorlage <- read_file("./tools/SDA/Vorlage_SDA_Meldungen.txt")

#Text kreieren
#Text kreieren
headline <- texts[2]
dateline <- texts[3]
text <- ""
service <- "bsd"
catchword <- CATCHWORDS_DE[v]
if (language == "fr") {
  service <- "bsf"
  catchword <- CATCHWORDS_FR[v]
}
if (language == "it") {
  service <- "bsi"
  catchword <- CATCHWORDS_IT[v]
}  


###Daten einfügen
vorlage <- gsub("INSERT_LONGID",ID_long,vorlage)
vorlage <- gsub("INSERT_TIME",date_and_time,vorlage)
vorlage <- gsub("INSERT_PROVIDER","KSDA",vorlage)
vorlage <- gsub("INSERT_STATUS","withheld",vorlage)
vorlage <- gsub("INSERT_SERVICE",service,vorlage)
vorlage <- gsub("INSERT_NOTE","DIES IST EIN TEST",vorlage)
vorlage <- gsub("INSERT_MEMO","DIES IST EIN TEST",vorlage)
vorlage <- gsub("INSERT_HYPERLINK","",vorlage)
vorlage <- gsub("INSERT_URGENCY","1",vorlage)
vorlage <- gsub("INSERT_ID",ID,vorlage)
vorlage <- gsub("INSERT_DATELINE",dateline,vorlage)
vorlage <- gsub("INSERT_LANGUAGE",language,vorlage)
vorlage <- gsub("INSERT_GENRE","CUR",vorlage)
vorlage <- gsub("INSERT_STORYTYPES",
                '<subject type="cpnat:abstract" qcode="sdastorytype:flsh"></subject>',
                vorlage)
vorlage <- gsub("INSERT_CHANNELS",
                paste0('<subject type="cpnat:abstract" qcode="sdamarschannel:POL"></subject>\n',
                       '<subject type="cpnat:abstract" qcode="sdamarschannel:ELE"></subject>\n',
                       '<subject type="cpnat:abstract" qcode="sdamarschannel:GOV"></subject>\n',
                       '<subject type="cpnat:abstract" qcode="sdamarschannel:VOT"></subject>'),
                vorlage)
vorlage <- gsub("INSERT_LOCATIONS",
                paste0('<located type="loctype:city" qcode="sdamarsgeo:BRN">\n<name>Bern</name>\n</located>'),
                vorlage)
vorlage <- gsub("INSERT_CATCHWORD",catchword,vorlage)
vorlage <- gsub("INSERT_HEADLINE",headline,vorlage)
vorlage <- gsub("INSERT_LEAD"," ",vorlage)
vorlage <- gsub("INSERT_CATCHLINE","",vorlage)
vorlage <- gsub("INSERT_TEXT",text,vorlage)

#Datei speichern
setwd("./Output_Mars")
filename <- paste0(format(Sys.Date(),"%Y%m%d"),"_",vorlagen$id[v],"_flash_hochrechnung_",language,".xml")
cat(vorlage, file = (con <- file(filename, "w", encoding="UTF-8"))); close(con)

#Sys.sleep(5)
###FTP-Upload
ftpUpload(filename, paste0("ftp://awp-lena.sda-ats.ch/",filename),userpwd=Sys.getenv("ftp_sda"))

setwd("..")



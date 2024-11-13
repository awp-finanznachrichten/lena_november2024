date_and_time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),"+02:00")

#ID
ID <- sample(100000000000000:999999999999999,1)

#ID Long
ID_long <- paste0(format(Sys.Date(), "%Y%m%d"),":",format(Sys.time(), "%Y%m%d%H%M%S"),ID)

#Vorlage laden
vorlage <- read_file("./tools/SDA/Vorlage_SDA_Meldungen.txt")

#Text kreieren
catchword <- texts[1]
headline <- texts[2]
dateline <- texts[3]
lead <- texts[4]
note <- texts[length(texts)]
text <- ""
service <- "bsd"
if (language == "fr") {
  service <- "bsf"
}
if (language == "it") {
  service <- "bsi"
}  

for (n in seq(5,(4+3*nrow(vorlagen)),3)) {

#Title
text <- paste0(text,'<p class="paragraph">',texts[n],"</p>\n") 

#Table
text <- paste0(text,"<table><tbody>\n",
       "<tr><td>",texts[n+1],"</td></tr>\n",
       "<tr><td>",texts[n+2],"</td></tr>\n</tbody></table>\n"
    )
}  

text <- paste0(text,'<p class="paragraph">',texts[n+3],"</p>\n") 
text <- paste0(text,"<table><tbody>\n")
for (m in seq(6+3*nrow(vorlagen),length(texts)-2,4)) {
#Table
text <- paste0(text,"<tr><td>",texts[m],
                 ifelse(nchar(texts[m+1]) > 0,
                        paste0(", ",texts[m+1]),
                        ""),
               ifelse(nchar(texts[m+2]) > 0,
                      paste0(", ",texts[m+2]),
                      ""),
               ifelse(nchar(texts[m+3]) > 0,
                      paste0(", ",texts[m+3]),
                      ""),
              "\n</td></tr>")  
}  
text <- paste0(text,"</tbody></table>\n")
text <- paste0(text,ifelse(nchar(texts[length(texts)-1]) > 0,
                           paste0('<p class="paragraph">',texts[length(texts)-1],"</p>\n"),
                                  ""))
text <- gsub(":,",":",text)

###Daten einfügen
vorlage <- gsub("INSERT_LONGID",ID_long,vorlage)
vorlage <- gsub("INSERT_TIME",date_and_time,vorlage)
vorlage <- gsub("INSERT_PROVIDER","KSDA",vorlage)
vorlage <- gsub("INSERT_STATUS","withheld",vorlage)
vorlage <- gsub("INSERT_SERVICE",service,vorlage)
vorlage <- gsub("INSERT_NOTE",note,vorlage)
vorlage <- gsub("INSERT_MEMO","DIES IST EIN TEST",vorlage)
vorlage <- gsub("INSERT_HYPERLINK","",vorlage)
vorlage <- gsub("INSERT_URGENCY","2",vorlage)
vorlage <- gsub("INSERT_ID",ID,vorlage)
vorlage <- gsub("INSERT_DATELINE",dateline,vorlage)
vorlage <- gsub("INSERT_LANGUAGE",language,vorlage)
vorlage <- gsub("INSERT_GENRE","RES",vorlage)
vorlage <- gsub("INSERT_STORYTYPES",
                '<subject type="cpnat:abstract" qcode="sdastorytype:tble"></subject>',
                vorlage)
vorlage <- gsub("INSERT_CHANNELS",
                paste0('<subject type="cpnat:abstract" qcode="sdamarschannel:POL"></subject>\n',
                       '<subject type="cpnat:abstract" qcode="sdamarschannel:VOT"></subject>'),
                vorlage)
vorlage <- gsub("INSERT_LOCATIONS",
                paste0('<located type="loctype:city" qcode="sdamarsgeo:',canton_metadata$hauptort_mars_code,'">\n<name>',canton_metadata$hauptort_de,'</name>\n</located>'),
                vorlage)
vorlage <- gsub("INSERT_CATCHWORD",catchword,vorlage)
vorlage <- gsub("INSERT_HEADLINE",headline,vorlage)
vorlage <- gsub("INSERT_LEAD",lead,vorlage)
vorlage <- gsub("INSERT_CATCHLINE","",vorlage)
vorlage <- gsub("INSERT_TEXT",text,vorlage)

#Datei speichern
setwd("./Output_Mars")
filename <- paste0(format(Sys.Date(),"%Y%m%d"),"_",output_overview$area_ID[i],"_results_",language,".xml")
cat(vorlage, file = (con <- file(filename, "w", encoding="UTF-8"))); close(con)

Sys.sleep(5)
###FTP-Upload
ftpUpload(filename, paste0("ftp://awp-lena.sda-ats.ch/",filename),userpwd=Sys.getenv("ftp_sda"))

setwd("..")




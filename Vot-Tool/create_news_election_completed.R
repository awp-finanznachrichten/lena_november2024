date_and_time <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),"+02:00")

#ID
ID <- sample(100000000000000:999999999999999,1)

#ID Long
ID_long <- paste0(format(Sys.Date(), "%Y%m%d"),":",format(Sys.time(), "%Y%m%d%H%M%S"),ID)

#Vorlage laden
vorlage <- read_file("./tools/SDA/Vorlage_SDA_Meldungen.txt")

#Text kreieren
staende <- meta_kt %>%
  select(area_ID,staende_count,area_name_de,area_name_fr,area_name_it)
all_cantons <- cantons_results %>%
  filter(votes_ID ==   vorlagen$id[v]) %>%
  left_join(staende)

catchword <- texts[1]
headline <- texts[2]
dateline <- texts[3]
note <- texts[length(texts)]
text <- ""
service <- "bsd"
if (language == "fr") {
  service <- "bsf"
}
if (language == "it") {
  service <- "bsi"
}  

text <- paste0(text,
               '<p class="paragraph">',texts[4],"</p>\n",
               "<table><tbody>\n",
               "<tr><td>",texts[5],"</td></tr>\n",
               "<tr><td>",texts[6],
               ifelse(nchar(texts[7]> 0),
                      paste0(" ",texts[7]),
                      ""),
               "</td></tr>\n",
               "<tr><td>",texts[8],"</td></tr>\n",
               "<tr><td>",texts[9],"</td></tr>\n</tbody></table>\n"
               )


###GET STAENDE AND YES/NO CANTONS###  
staende <- meta_kt %>%
    select(area_ID,staende_count,area_name_de,area_name_fr,area_name_it)
all_cantons <- cantons_results %>%
  filter(votes_ID ==   vorlagen$id[v]) %>%
  left_join(staende)

#Table
if (language == "de") {
  text <- paste0(text,"<table><tbody>\n",
                 "<tr>",
                 "<td></td>",
                 "<td>Ja</td>",
                 "<td>Ja-Anteil</td>",
                 "<td>Nein</td>",
                 "<td>Nein-Anteil</td>",
                 "<td>Stimmbeteiligung</td></tr>\n")
                 
#Add Schweiz
text <- paste0(text,"<tr>",
"<td>Schweiz</td>",
"<td>",format(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenAbsolut"]][v],big.mark="'"),"</td>",
"<td>",gsub("[.]",",",format(round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]][v],2),nsmall=2)),"%</td>",
"<td>",format(json_data[["schweiz"]][["vorlagen"]][["resultat.neinStimmenAbsolut"]][v],big.mark="'"),"</td>",
"<td>",gsub("[.]",",",format(100-round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]][v],2),nsmall=2)),"%</td>",
"<td>",gsub("[.]",",",format(round2(json_data[["schweiz"]][["vorlagen"]][["resultat.stimmbeteiligungInProzent"]][v],2),nsmall=2)),"%</td></tr>\n")

text <- paste0(text,
               "<tr>",
               "<td></td>",
               "<td></td>",
               "<td></td>",
               "<td></td>",
               "<td></td>",
               "<td></td></tr>\n")

}
if (language == "fr") {
  text <- paste0(text,"<table><tbody>\n",
                 "<tr>",
                 "<td></td>",
                 '<td>Oui</td>',
                 '<td>Pourcentage de oui</td>',
                 '<td>Non</td>',
                 '<td>Pourcentage de non</td>',
                 "<td>Taux de participation</td></tr>\n")  
  
  #Add Schweiz
  text <- paste0(text,"<tr>",
                 "<td>Suisse</td>",
                 "<td>",format(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenAbsolut"]][v],big.mark="'"),"</td>",
                 "<td>",gsub("[.]",",",format(round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]][v],2),nsmall=2)),"%</td>",
                 "<td>",format(json_data[["schweiz"]][["vorlagen"]][["resultat.neinStimmenAbsolut"]][v],big.mark="'"),"</td>",
                 "<td>",gsub("[.]",",",format(100-round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]][v],2),nsmall=2)),"%</td>",
                 "<td>",gsub("[.]",",",format(round2(json_data[["schweiz"]][["vorlagen"]][["resultat.stimmbeteiligungInProzent"]][v],2),nsmall=2)),"%</td></tr>\n")
  
  text <- paste0(text,
                 "<tr>",
                 "<td></td>",
                 "<td></td>",
                 "<td></td>",
                 "<td></td>",
                 "<td></td>",
                 "<td></td></tr>\n")  
  
}  
if (language == "it") {
  text <- paste0(text,"<table><tbody>\n",
                 "<tr>",
                 "<td></td>",
                 '<td>"sì"</td>',
                 '<td>quota di "sì"</td>',
                 '<td>"no"</td>',
                 '<td>quota di "no"</td>',
                 "<td>partecipazione al voto</td></tr>\n")  
  
  #Add Schweiz
  text <- paste0(text,"<tr>",
                 "<td>Svizzera</td>",
                 "<td>",format(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenAbsolut"]][v],big.mark="'"),"</td>",
                 "<td>",gsub("[.]",",",format(round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]][v],2),nsmall=2)),"%</td>",
                 "<td>",format(json_data[["schweiz"]][["vorlagen"]][["resultat.neinStimmenAbsolut"]][v],big.mark="'"),"</td>",
                 "<td>",gsub("[.]",",",format(100-round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]][v],2),nsmall=2)),"%</td>",
                 "<td>",gsub("[.]",",",format(round2(json_data[["schweiz"]][["vorlagen"]][["resultat.stimmbeteiligungInProzent"]][v],2),nsmall=2)),"%</td></tr>\n")
  
  text <- paste0(text,
                 "<tr>",
                 "<td></td>",
                 "<td></td>",
                 "<td></td>",
                 "<td></td>",
                 "<td></td>",
                 "<td></td></tr>\n")  
}  


for (c in 1:nrow(all_cantons)) {
  if (language == "de") {    
    text <- paste0(text,
                   "<tr>",
                   "<td>",all_cantons$area_name_de[c],"</td>",
                   "<td>",ifelse(all_cantons$share_yes_votes[c]>9999,
                                 format(all_cantons$share_yes_votes[c],big.mark="'"),
                                 all_cantons$share_yes_votes[c]),"</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$share_yes_percentage[c],nsmall=2)),"%</td>",
                   "<td>",ifelse(all_cantons$share_no_votes[c]>9999,
                                 format(all_cantons$share_no_votes[c],big.mark="'"),
                                 all_cantons$share_no_votes[c]),"</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$share_no_percentage[c],nsmall=2)),"%</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$voter_participation[c],nsmall=2)),"%</td></tr>\n")
  }
  if (language == "fr") {  
    text <- paste0(text,
                   "<tr>",
                   "<td>",all_cantons$area_name_fr[c],"</td>",
                   "<td>",ifelse(all_cantons$share_yes_votes[c]>9999,
                                 format(all_cantons$share_yes_votes[c],big.mark="'"),
                                 all_cantons$share_yes_votes[c]),"</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$share_yes_percentage[c],nsmall=2)),"%</td>",
                   "<td>",ifelse(all_cantons$share_no_votes[c]>9999,
                                 format(all_cantons$share_no_votes[c],big.mark="'"),
                                 all_cantons$share_no_votes[c]),"</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$share_no_percentage[c],nsmall=2)),"%</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$voter_participation[c],nsmall=2)),"%</td></tr>\n")
  }  
  if (language == "it") {  
    text <- paste0(text,
                   "<tr>",
                   "<td>",all_cantons$area_name_it[c],"</td>",
                   "<td>",ifelse(all_cantons$share_yes_votes[c]>9999,
                                 format(all_cantons$share_yes_votes[c],big.mark="'"),
                                 all_cantons$share_yes_votes[c]),"</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$share_yes_percentage[c],nsmall=2)),"%</td>",
                   "<td>",ifelse(all_cantons$share_no_votes[c]>9999,
                                 format(all_cantons$share_no_votes[c],big.mark="'"),
                                 all_cantons$share_no_votes[c]),"</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$share_no_percentage[c],nsmall=2)),"%</td>",
                   "<td>",gsub("[.]",",",format(all_cantons$voter_participation[c],nsmall=2)),"%</td></tr>\n")
  }  
}
text <- paste0(text,"</tbody></table>\n")

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
                paste0('<located type="loctype:city" qcode="sdamarsgeo:BRN">\n<name>Bern</name>\n</located>'),
                vorlage)
vorlage <- gsub("INSERT_CATCHWORD",catchword,vorlage)
vorlage <- gsub("INSERT_HEADLINE",headline,vorlage)
vorlage <- gsub("INSERT_LEAD"," ",vorlage)
vorlage <- gsub("INSERT_CATCHLINE","",vorlage)
vorlage <- gsub("INSERT_TEXT",text,vorlage)

#Datei speichern
setwd("./Output_Mars")
filename <- paste0(format(Sys.Date(),"%Y%m%d"),"_",format(Sys.time(),"%H"),"h_",vorlagen$id[v],"_final_overview_",language,".xml")
cat(vorlage, file = (con <- file(filename, "w", encoding="UTF-8"))); close(con)

#Sys.sleep(5)
###FTP-Upload
ftpUpload(filename, paste0("ftp://awp-lena.sda-ats.ch/",filename),userpwd=Sys.getenv("ftp_sda"))

setwd("..")






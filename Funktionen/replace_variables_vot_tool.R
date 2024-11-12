replace_variables_vot <- function(texts,
                                  language = "de",
                                  type = "") {
  
  if (type == "trend" || type == "extrapolation" || type == "staendemehr") {
  texts <- str_replace_all(texts,"#Name_Vorlage_d",gsub(":","",vorlagen$text[v]))
  texts <- str_replace_all(texts,"#Name_Vorlage_f",gsub(":","",vorlagen_fr$text[v]))
  texts <- str_replace_all(texts,"#Name_Vorlage_i",gsub(":","",vorlagen_it$text[v]))
  texts <- str_replace_all(texts,"#Topic_Vorlage_d",CATCHWORDS_DE[v])
  texts <- str_replace_all(texts,"#Topic_Vorlage_f",CATCHWORDS_FR[v])
  texts <- str_replace_all(texts,"#Topic_Vorlage_i",CATCHWORDS_IT[v])
  }
  
  if (type == "extrapolation") {
  texts <- str_replace_all(texts,"#Name_Vorlage_d",gsub(":","",vorlagen$text[v]))
  texts <- str_replace_all(texts,"#Name_Vorlage_f",gsub(":","",vorlagen_fr$text[v]))
  texts <- str_replace_all(texts,"#Name_Vorlage_i",gsub(":","",vorlagen_it$text[v]))
  texts <- str_replace_all(texts,"#result_yes",toString(votes_yes))
  texts <- str_replace_all(texts,"#result_no",toString(votes_no)) 
  }  
  
  if (type == "results") {
    texts <- str_replace_all(texts,"#Kanton_short",canton_metadata$area_ID)
    texts <- str_replace_all(texts,"#Kanton_d",canton_metadata$area_name_de)
    texts <- str_replace_all(texts,"#Kanton_f",canton_metadata$area_name_fr)
    texts <- str_replace_all(texts,"#Kanton_i",canton_metadata$area_name_it)

    count <- 1
    for (i in seq(5,(4+3*nrow(vorlagen)),3)) {
    
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#Name_Vorlage_d",gsub(":","",vorlagen$text[count]))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#Name_Vorlage_f",gsub(":","",vorlagen_fr$text[count]))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#Name_Vorlage_i",gsub(":","",vorlagen_it$text[count]))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#Result_Text_d",ifelse(canton_results$result[count] == "yes","JA","NEIN"))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#Result_Text_f",ifelse(canton_results$result[count] == "yes","OUI","NON"))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#Result_Text_i",ifelse(canton_results$result[count] == "yes","SI","NO"))

    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#yes_percentage",gsub("[.]",",",format(canton_results$share_yes_percentage[count],nsmall=2)))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#no_percentage",gsub("[.]",",",format(canton_results$share_no_percentage[count],nsmall=2)))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#yes_absolut",gsub("[.]",",",format(canton_results$share_yes_votes[count],big.mark = "'")))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#no_absolut",gsub("[.]",",",format(canton_results$share_no_votes[count],big.mark = "'")))
    texts[i:(i+2)] <- str_replace_all(texts[i:(i+2)],"#voter_share",gsub("[.]",",",format(canton_results$voter_participation[count],nsmall=2)))
    count <- count+1
    }

    count <- 1
    for (i in seq(6+3*nrow(vorlagen),length(texts)-2,4)) {
      ###GET STAENDE AND YES/NO CANTONS###  
      staende <- meta_kt %>%
        select(area_ID,staende_count)
      all_cantons <- cantons_results %>%
        filter(votes_ID == canton_results$votes_ID[count]) %>%
        left_join(staende)
      yes_cantons <- all_cantons %>%
        filter(result == "yes")
      no_cantons <- all_cantons %>%
        filter(result == "no")
      not_counted <- all_cantons %>%
        filter(is.na(result) == TRUE)
      not_counted_list <- "-"
      if (nrow(not_counted) > 0) {
        not_counted_list <- paste(not_counted$area_ID,collapse = ", ")  
      }  
      counted_cantons <- nrow(yes_cantons) + nrow(no_cantons)
      
      
      ###GET EXTRAPOLATION
      extrapolation_vorlage <- extrapolations %>%
        filter(votes_ID == canton_results$votes_ID[count]) %>%
        arrange(desc(last_update)) %>%
        .[1,]

      texts[i:(i+3)] <- str_replace_all(texts[i:(i+3)],"#Name_Vorlage_d",gsub(":","",vorlagen$text[count]))
      texts[i:(i+3)] <- str_replace_all(texts[i:(i+3)],"#Name_Vorlage_f",gsub(":","",vorlagen_fr$text[count]))
      texts[i:(i+3)] <- str_replace_all(texts[i:(i+3)],"#Name_Vorlage_i",gsub(":","",vorlagen_it$text[count]))
      
      if (is.na(extrapolation_vorlage$share_votes_yes) == FALSE) {
      texts[i:(i+3)] <- str_replace_all(texts[i:(i+3)],"#hochrechnung_yes",toString(extrapolation_vorlage$share_votes_yes))
      texts[i:(i+3)] <- str_replace_all(texts[i:(i+3)],"#hochrechnung_no",toString(extrapolation_vorlage$share_votes_no))
      texts[i:(i+3)] <- str_replace_all(texts[i:(i+3)],"#Hochrechnung_time", format(strptime(extrapolation_vorlage$last_update, "%Y-%m-%d %H:%M:%S"),"%H:%M"))
      }
      count <- count+1
    }

    texts <- str_replace_all(texts,"#List_missing_cantons",not_counted_list)

  }
  
  if (type == "intermediate") {
    
    staende <- meta_kt %>%
      select(area_ID,staende_count,area_name_de)
    all_cantons <- cantons_results %>%
      filter(votes_ID == vorlagen$id[v]) %>%
      left_join(staende)
    yes_cantons <- all_cantons %>%
      filter(result == "yes")
    no_cantons <- all_cantons %>%
      filter(result == "no")
    not_counted <- all_cantons %>%
      filter(is.na(result) == TRUE)
    counted_cantons <- nrow(yes_cantons) + nrow(no_cantons)
    yes_cantons_list <- "-"
    staende_yes <- 0
    if (nrow(yes_cantons) > 0) {
      yes_cantons_list <- paste(yes_cantons$area_ID,collapse = ", ")  
      staende_yes <- sum(yes_cantons$staende_count)
    }  
    no_cantons_list <- "-"
    staende_no <- 0
    if (nrow(no_cantons) > 0) {
      no_cantons_list <- paste(no_cantons$area_ID,collapse = ", ")  
      staende_no <- sum(no_cantons$staende_count)
    }  
    not_counted_list <- "-"
    if (nrow(not_counted) > 0) {
      not_counted_list <- paste(not_counted$area_ID,collapse = ", ")  
    }  
    
    ###GET EXTRAPOLATION
    extrapolation_vorlage <- extrapolations %>%
      filter(votes_ID == vorlagen$id[v]) %>%
      arrange(desc(last_update)) %>%
      .[1,]
    
      texts <- str_replace_all(texts,"#Name_Vorlage_d",gsub(":","",vorlagen$text[v]))
      texts <- str_replace_all(texts,"#Name_Vorlage_f",gsub(":","",vorlagen_fr$text[v]))
      texts <- str_replace_all(texts,"#Name_Vorlage_i",gsub(":","",vorlagen_it$text[v]))

      texts <- str_replace_all(texts,"#cantons_counted",toString(counted_cantons))
      texts <- str_replace_all(texts,"#staende_yes",toString(staende_yes))
      texts <- str_replace_all(texts,"#staende_no",toString(staende_no))
      texts <- str_replace_all(texts,"#List_missing_cantons",not_counted_list)
      
      
      if (is.na(extrapolation_vorlage$share_votes_yes) == FALSE) {
        texts <- str_replace_all(texts,"#hochrechnung_yes",toString(extrapolation_vorlage$share_votes_yes))
        texts <- str_replace_all(texts,"#hochrechnung_no",toString(extrapolation_vorlage$share_votes_no))
        texts <- str_replace_all(texts,"#Hochrechnung_time", format(strptime(extrapolation_vorlage$last_update, "%Y-%m-%d %H:%M:%S"),"%H:%M"))
      }
  }
  
  if (type == "endresult") {
    
    staende <- meta_kt %>%
      select(area_ID,staende_count,area_name_de)
    all_cantons <- cantons_results %>%
      filter(votes_ID == vorlagen$id[v]) %>%
      left_join(staende)
    yes_cantons <- all_cantons %>%
      filter(result == "yes")
    no_cantons <- all_cantons %>%
      filter(result == "no")
    not_counted <- all_cantons %>%
      filter(is.na(result) == TRUE)
    counted_cantons <- nrow(yes_cantons) + nrow(no_cantons)
    yes_cantons_list <- "-"
    staende_yes <- 0
    if (nrow(yes_cantons) > 0) {
      yes_cantons_list <- paste(yes_cantons$area_ID,collapse = ", ")  
      staende_yes <- sum(yes_cantons$staende_count)
    }  
    no_cantons_list <- "-"
    staende_no <- 0
    if (nrow(no_cantons) > 0) {
      no_cantons_list <- paste(no_cantons$area_ID,collapse = ", ")  
      staende_no <- sum(no_cantons$staende_count)
    }  
    not_counted_list <- "-"
    if (nrow(not_counted) > 0) {
      not_counted_list <- paste(not_counted$area_ID,collapse = ", ")  
    }  
    
    result_yes <- gsub("[.]",",",format(round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]][v],2),nsmall=2))
    result_no <- gsub("[.]",",",format(100-round2(json_data[["schweiz"]][["vorlagen"]][["resultat.jaStimmenInProzent"]][v],2),nsmall=2))
    
    texts <- str_replace_all(texts,"#Name_Vorlage_d",gsub(":","",vorlagen$text[v]))
    texts <- str_replace_all(texts,"#Name_Vorlage_f",gsub(":","",vorlagen_fr$text[v]))
    texts <- str_replace_all(texts,"#Name_Vorlage_i",gsub(":","",vorlagen_it$text[v]))
    
    texts <- str_replace_all(texts,"#result_yes",result_yes)
    texts <- str_replace_all(texts,"#result_no",result_no)
    texts <- str_replace_all(texts,"#staende_yes",toString(staende_yes))
    texts <- str_replace_all(texts,"#staende_no",toString(staende_no))
    texts <- str_replace_all(texts,"#List_cantons_yes",yes_cantons_list)
    texts <- str_replace_all(texts,"#List_cantons_no",no_cantons_list)
    
  }
  
    
  if (language == "fr") {
    ##Französisch
    texts <- str_replace_all(texts,"canton de Jura","canton du Jura")
    texts <- str_replace_all(texts,"canton de Tessin","canton du Tessin")
    texts <- str_replace_all(texts,"du canton de Valais","en Valais")
    texts <- str_replace_all(texts,"canton de Valais","canton du Valais")
    texts <- str_replace_all(texts,"canton de Argovie","canton d'Argovie")
    texts <- str_replace_all(texts,"canton de Appenzell Rhodes-Extérieures","canton d'Appenzell Rhodes-Extérieures")
    texts <- str_replace_all(texts,"canton de Appenzell Rhodes-Intérieures","canton d'Appenzell Rhodes-Intérieures")
    texts <- str_replace_all(texts,"L'élu au Conseil national dans le canton d'Appenzell Rhodes-Intérieures","L'élu au Conseil national dans le canton d'Appenzell AI")
    texts <- str_replace_all(texts,"L'élu au Conseil national dans le canton d'Appenzell Rhodes-Extérieures","L'élu au Conseil national dans le canton d'Appenzell AR")
    texts <- str_replace_all(texts,"canton de Grisons","canton des Grisons")
    texts <- str_replace_all(texts,"canton de Obwald","canton d'Obwald")
    texts <- str_replace_all(texts,"canton de Uri","canton d'Uri")
    
    texts <- str_replace_all(texts,"de A","d'A") 
    texts <- str_replace_all(texts,"de E","d'E")
    texts <- str_replace_all(texts,"de I","d'I") 
    texts <- str_replace_all(texts,"de O","d'O") 
    texts <- str_replace_all(texts,"de U","d'U")
    texts <- str_replace_all(texts,"de u","d'u")
    texts <- str_replace_all(texts,"de Yv","d'Yv")
    
    texts <- str_replace_all(texts,"De A","d'A") 
    texts <- str_replace_all(texts,"De E","d'E")
    texts <- str_replace_all(texts,"De I","d'I") 
    texts <- str_replace_all(texts,"De O","d'O") 
    texts <- str_replace_all(texts,"De U","d'U")
    texts <- str_replace_all(texts,"De Yv","d'Yv")
    
    texts <- str_replace_all(texts,"le A","l'A") 
    texts <- str_replace_all(texts,"le E","l'E")
    texts <- str_replace_all(texts,"le I","l'I") 
    texts <- str_replace_all(texts,"le O","l'O") 
    texts <- str_replace_all(texts,"le U","l'U")
    texts <- str_replace_all(texts,"le Yv","l'Yv")
    
    texts <- str_replace_all(texts,"Le A","L'A") 
    texts <- str_replace_all(texts,"Le E","L'E")
    texts <- str_replace_all(texts,"Le I","L'I") 
    texts <- str_replace_all(texts,"Le O","L'O") 
    texts <- str_replace_all(texts,"Le U","L'U")
    texts <- str_replace_all(texts,"Le Yv","L'Yv")
    
    texts <- str_replace_all(texts,"de Les ","des ")
    texts <- str_replace_all(texts,"de Le ","du ")
    texts <- str_replace_all(texts,"à Les ","aux ")
    texts <- str_replace_all(texts,"A Les ","Aux ")
    texts <- str_replace_all(texts,"à Le ","au ")
    texts <- str_replace_all(texts,"A Le ","Au ")
    texts <- str_replace_all(texts,"du Vaud","de Le Vaud")
    
  }
  
  
  return(texts)  
}  

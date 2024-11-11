###Storyfinder Nationalrat Parties
get_story_trend <- function(trend) {
  storyboard <- c("Catchword")
  if (trend == "angenommen") {
  storyboard <- c(storyboard,"Headline_yes")   
  } else if (trend == "abgelehnt") {
  storyboard <- c(storyboard,"Headline_no")  
  }
  storyboard <- c(storyboard,"Provider-Location")
  return(storyboard)
}  

###Storyfinder Nationalrat Parties
get_story_extrapolation <- function(votes_yes,
                                    votes_no) {
  storyboard <- c("Catchword")
  
  if (votes_yes > votes_no) {
    storyboard <- c(storyboard,"Headline_yes") 
  } else if (votes_yes < votes_no) {
    storyboard <- c(storyboard,"Headline_no") 
  } else {
    storyboard <- c(storyboard,"Headline_even")  
  }  
  storyboard <- c(storyboard,"Provider-Location")
  return(storyboard)
}  

get_story_staendemehr <- function() {
  storyboard <- c("Catchword","Headline","Provider-Location")
  return(storyboard)
}  

get_story_results_canton <- function() {
  storyboard <- c("Catchword","Headline","Provider-Location","Lead")
  for (c in 1:nrow(canton_results)) {
  storyboard <- c(storyboard,"Text_results_canton_header",
                  ifelse(canton_results$share_yes_votes[c] > canton_results$share_no_votes[c],
                         "Text_results_canton_yes",
                         "Text_results_canton_no"),
                  "Text_results_canton_voter_share")
  }  
    
  
  if (nrow(canton_results) == 1) {
  storyboard <- c(storyboard,"Text_CH_header_one_vote")  
  } else {
  storyboard <- c(storyboard,"Text_CH_header_several_votes")    
  }  
    
  for (c in 1:nrow(canton_results)) {
    
    ###GET STAENDE AND YES/NO CANTONS###  
    staende <- meta_kt %>%
      select(area_ID,staende_count)
    all_cantons <- cantons_results %>%
      filter(votes_ID == canton_results$votes_ID[c]) %>%
      left_join(staende)
    yes_cantons <- all_cantons %>%
      filter(result == "yes")
    no_cantons <- all_cantons %>%
      filter(result == "no")
    counted_cantons <- nrow(yes_cantons) + nrow(no_cantons)
    
    ###GET EXTRAPOLATION
    extrapolation_vorlage <- extrapolations %>%
      filter(votes_ID == canton_results$votes_ID[c]) %>%
      arrange(desc(last_update)) %>%
      .[1,]
    
    storyboard <- c(storyboard,"Text_CH_name")
    if (counted_cantons == 1) {
    storyboard <- c(storyboard,"Text_CH_counted_cantons_one")
    } else if (counted_cantons == 26) {
    storyboard <- c(storyboard,"") 
    }  else {
    storyboard <- c(storyboard,"Text_CH_counted_cantons_several")  
    }  
    
    if (votes_metadata_CH$staendemehr[c] == "yes") {
    if (sum(yes_cantons$staende_count) > 11.5) {
    storyboard <- c(storyboard,"Text_CH_standemehr_success")  
    } else if (sum(no_cantons$staende_count) >= 11.5) {
    storyboard <- c(storyboard,"Text_CH_staendemehr_fail")  
    }  
    } else {
    storyboard <- c(storyboard,"")  
    }  
  
    if ((extrapolation_vorlage$type == "trend") & (is.na(extrapolation_vorlage$result) == FALSE)) {
    if (extrapolation_vorlage$result == "angenommen") {
      storyboard <- c(storyboard,"Text_CH_Trend_yes")
    } else if (extrapolation_vorlage$result == "abgelehnt") {
      storyboard <- c(storyboard,"Text_CH_Trend_no")
    }  else {
      storyboard <- c(storyboard,"")
    }  

    } else if (is.na(extrapolation_vorlage$share_votes_yes) == FALSE) {
     
    if (extrapolation_vorlage$share_votes_yes > 50) {
        storyboard <- c(storyboard,"Text_CH_Hochrechnung_yes")
    } else if (extrapolation_vorlage$share_votes_yes < 50) {
        storyboard <- c(storyboard,"Text_CH_Hochrechnung_no")
    } else {
      storyboard <- c(storyboard,"")  
    }  
    } else {
      storyboard <- c(storyboard,"")  
    }  

  }      
  if ((counted_cantons > 18) & (counted_cantons < 25)) {
    storyboard <- c(storyboard,"Text_CH_missing_cantons_several")
  } else if (counted_cantons == 25)  {
    storyboard <- c(storyboard,"Text_CH_missing_cantons_one")
  } else {
    storyboard <- storyboard <- c(storyboard,"")
  }  
  
    
  if (canton_results$source_update[1] == "Vot-Tool") {
    storyboard <- storyboard <- c(storyboard,"Disclaimer_Canton")  
  }  else {
    storyboard <- storyboard <- c(storyboard,"Disclaimer")
  }  
  return(storyboard)
}  

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

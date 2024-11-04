#Storybuilder
get_texts_vot <- function(storyboard,
                      texts_spreadsheet,
                      language) {

  if (language == "de"){
    texts_spreadsheet <- texts_spreadsheet[,1:2]
  } else if (language == "fr") {
    texts_spreadsheet <- texts_spreadsheet[,c(1,3)]
  } else if (language == "it") {
    texts_spreadsheet <- texts_spreadsheet[,c(1,4)]  
  }  

  texts <- c()
  for (story in storyboard) {
    text_options <- texts_spreadsheet %>%
      filter(Text_ID == story) 
    if (nrow(text_options) > 0) {
      texts <- c(texts,text_options[sample(1:nrow(text_options),1),2])  
    } else {
      texts <- c(texts,story) 
    }  
  }  
  return(texts)
}  
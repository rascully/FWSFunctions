##### extract the list of surveys by refuges for the psp orders 

# Rewrite to search bioticGroupLevel2 or derivedBioicGropu2 or title 

psp_survey_by_refuge<- function(refuge_code){

  #refuge_code <- "FF08RBRV00" # does not work 
  #refuge_code <- "FF07RAM000" #works 
  
library(httr)
library(jsonlite)
library(tidyverse)

taxon_orders <- c('Procellariformes', 'Pelecaniformes', 'Charadriiformes')

psp_species_list <- read.csv(paste0(dirname(getwd()), "/inputs/psp_species_list.csv"))

psp_species <- psp_species_list %>% 
              select(itis_species)

survey_list       <- GET(paste0('https://ecos.fws.gov:443/primr/surveyApi/flatList?offset=0&max=25000&order=id%2Casc&ccc=',refuge_code))
survey_text       <- content(survey_list, "text")

if (length(fromJSON(survey_text, flatten = TRUE)$data) != 0){
    
    survives           <- as.data.frame(fromJSON(survey_text, flatten = TRUE))
    colnames(survives) <-gsub("data.","",colnames(survives))
    
    current         <- survives %>% 
                          filter(str_detect(endYear, 'Indefinite|Future/TBD'))
    
    s_species <- current %>% 
              select(matches("id|Species|common")) 
    
    s_species <- current %>% 
      select(matches("common")) 
    
    psp_survey_by_refuge <- survives %>% 
        filter(str_detect(derivedBioticGroupLevel2_scientificName, 'Procellariformes|Pelecaniformes|Charadriiformes'))
      if (nrow(psp_survey_by_refuge == 0 )){
        print("no PSP") 
        psp_survey_by_refuge <- "No PSP survives in PRIMER"}
  } else {
  print("no survies in primer")
  psp_survey_by_refuge <- "No survives in PRIMER"
} 

return(psp_survey_by_refuge)

} 



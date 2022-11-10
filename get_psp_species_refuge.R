psp_species_refuge <- function(refuge_code){ 
  
    library(httr)
    library(jsonlite)
    library(tidyverse)
    
  
  
# load the PSP seabird list it is loaded in the R folder inputs (need to update based on the SC feedback in the last meeting)
    input_folder  <- paste0(dirname(getwd()),"/inputs/") 
    tax           <- read.csv(paste0(input_folder, "psp_species_list.csv") ) 
    
  #  refuge_code   <- 'FF08RFRL00'
    
#create a list of tsn numbers from the PSP data
 
 category           <- "Bird"
 request_URL        <- paste0("https://ecos.fws.gov/IRISAPI/SpeciesApi/api/SpeciesList/items/DownloadFile?RefugeCode=",refuge_code,"&CategoryName=Bird&RowsPerPage=10000") 
 species_list       <- httr::GET(request_URL)
 bird               <- content(species_list, as = "parsed")  
 tsn                <- tax %>% 
                          select(tsn_itis) %>% 
                          filter(!is.na(tsn_itis)) %>% 
                          unlist()
# filter the FWSepecies list by the PSP species list, use the itis species name, common name and the TSN number                   
seabird <- bird %>% 
   filter(tolower(ScientificName) %in% tolower(tax$species_itis)& !is.na(ScientificName)| 
            tolower(SourceCommonNames) %in% tolower(tax$common_name_itis)& !is.na(SourceCommonNames)|
            ITIS_TSN %in% tsn)
 
                                                                            
 
 

   if (nrow(bird) == 0) {
      return("No Birds in Refuge Species List or Birds with Record Status: Approved")
    } else if (nrow(seabird) == 0) {
      return(paste0("No PSP Seabirds in Refuge Species List or PSP Seabirds with Record Status: Approved"))
    } else {
      seabird_list <- list (psp_count=as.character(nrow(seabird)), psp_list=seabird )
      return(seabird_list)
    }
}

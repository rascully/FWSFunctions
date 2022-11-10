library(rgdal)
library(sp)
library(tidyverse)

#####  Compare the CMT to the FWSCadastral.gdb#####

#Build PSP refuge list from Coperate Master Table using APIs
regions <- c(1,8,7)
ref_list <-GET(paste0("https://systems.fws.gov/cmt/getOrgs.do?region=1,8,7&orgType=NWR"))
ref_text <- content(ref_list, "text")
CMT_psp_refuges_table   <- as.data.frame(fromJSON(ref_text, flatten = TRUE)) %>% 
  dplyr::filter(!is.na(doiregion)) %>% 
  distinct()

# The input file geodatabase
fgdb <- paste0(getwd(),"/data/FWSCadastral.gdb/FWSCadastral.gdb")

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)

myspdf = readOGR(dsn=fgdb, layer= "FWSBoundaries") 
head(myspdf)

# Read the feature class
fc <- sf::st_read(fgdb, layer = "FWSBoundaries")

refuges_table <- as.data.frame(fc) %>%  
  select(-"SHAPE_Length", -"SHAPE_Area",-"SHAPE") 


#Filter for the NWR that are in FWS region 1, 8, and 7 (for some reason in the table those are 9, 10 and 11??)
psp_fc <- fc %>% 
  filter(IntReg  %in% c("9","10","11"))   %>% 
  filter(!is.na(CostCenter)) %>%  
  filter(RSL_TYPE == "NWR")



# create a simple datatable  
psp_refuges_table <- as.data.frame(psp_fc) %>%  
  select(-"SHAPE_Length", -"SHAPE_Area",-"SHAPE")

##### Why are there a different count of refutes from the CMT and the Geodababase #####

test <- full_join(CMT_psp_refuges_table, psp_refuges_table, by=c("FBMSCode"="CostCenter"))

test2<- test %>% select(orgName,region,FBMSCode,ORGNAME, IntReg)
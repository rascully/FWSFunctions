##  Script name: get_cmt_refuge_complex.r
##
## Script to GET data from the fws corporate master table (CMT), make a flat table so that when appropriate, refuge in Regions 1, 8, 7 are associated with their complex. Save to a .csv. 
##
## 
## Author: Becca Scully 
## Email: rebecca_scully@fws.gov
## Data Created: 2022-10-07 
## -------------------------------------------------------

#Required packages 
library(tidyverse)
library(httr)
library(jsonlite)

# pull the CMT basics for Region 1, 8, 7 sort out all the refuges 
cmt_list <-GET(paste0("https://systems.fws.gov/cmt/getCMTBasic.do?REGION=1,8,7"))
cmt_text <- content(cmt_list, "text")
cmt <- as.data.frame(fromJSON(cmt_text, flatten = TRUE)) %>% 
  discard(~all(is.na(.) | . =="")) %>% 
  distinct() 

#extract the complexes from the CMT, rename the ORGNAME to COMPLESNAME
complex <- cmt %>% 
  filter(ORGTYPEACRO== "RAO") %>% 
  rename(COMPLEXNAME= ORGNAME ) %>% 
  select(c(COMPLEXNAME,ORGCODE))

# join the  refuges RPTORGCODE (report to org code) to the complex ORGCODE (the organization code for the complex. Extract the relevant fields )
# notes: ORGCODE and RPTORGCODE (next higher level organization code). so, for example, kilauea point (12530), hanalei (12522), and huleia (12523) all report to the kauai nwr complex (12519)

refuges <- left_join(cmt %>%  
                       filter(ORGTYPEACRO == 'NWR'), complex, by= c('RPTORGCODE' = "ORGCODE")) %>% 
  select(ORGNAME, COMPLEXNAME, ORGCODE, REGION, MAILSTATEABBR) %>% 
  rename(STATE = MAILSTATEABBR)

write.csv(refuges, getwd(),"cmt__refuges_complexs.csv", row.names = F)

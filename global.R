library(tidyverse)
library(here)
library(jsonlite)
library(glue)
library(dplyr)
## parse all trial.full.json and curate relevant info for a basic summary table

## reactive table 


parseTrials <- function(jsonfile) {
  #jsonfile <- trialsfiles[1]
  trial <- fromJSON(jsonfile)

  # pulling out trial arms
  arm_groups = tibble(cohortlabel = trial$query$arm[[1]]$cohortlabel,
                      drug = trial$query$arm[[1]]$drug,
                      arm_type = trial$query$arm[[1]]$arm_type,
                      line_of_therapy = trial$query$arm[[1]]$line_of_therapy,
                      arm_hold_status = trial$query$arm[[1]]$arm_hold_status,
                      biomarker = trial$query$arm[[1]]$biomarker)
                      
         parsedTrial <- tibble(

    # info
    Protocol = trial$info$Protocol_No,
    NCT = trial$info$NCT,
    JIT = trial$info$jit,
    #Added Protocol Number
    #Protocol_No = trial$info$Protocol_No,
    Name = trial$info$trial_name,

####
#filter cohorts which are open
#filtcohort <- arm_groups %>% select(cohortLabel, arm_hold_status) %>% filter(arm_hold_status == "open"),
####
    # disease
    Disease = trial$disease$summary,
   # disp_disease =  trial$disease[[1]] %>% unnest(details),
  # disp_disease = trial$disease$details[[1]] %>% select(code, selection),
  disp_disease = list(disp_disease = trial$disease$details[[1]]),
  



    # query - general
  Title = trial$query$title,
    Status = trial$query$current_status,
    StatusUpdate = trial$query$status_verif_date,
    Sponsor = trial$query$sponsor,
    Summary = trial$query$brief_summary,
    Conditions = trial$query$conditions,
    Phase = trial$query$phase,
    StudyType = trial$query$type,
    MinAge = if(trial$query$min_age %>% is_empty()) {
      min_age = "Not Available"
    } else
    {
      trial$query$min_age
    },
    Gender = trial$query$gender,
    Link = trial$query$link,
 # NCTandTrialname = paste(Link,Name),
    LastUpdate = trial$query$last_update_date,

    # query - cohorts w/ drug and biomarker information
    arms = list(arms = trial$query$arm[[1]] %>% unnest(biomarker)),

    #query - cohorts only for display table
   # disp_cohorts = list(disp_cohorts = bind_cols(arm_groups %>% select(-biomarker))),

    #biomarker = lapply(1:nrow(arm_groups), function(x) processBiomarker(x)) %>%
    #                                              unlist())),

    # query - biomarkers only for display table
    disp_biomarkers = trial$query$arm[[1]]$biomarker %>%
    bind_rows() %>%
    select(summary) %>% distinct() %>% unlist() %>% na.omit() %>%
    paste0(collapse = " | "),
    # unique() %>%
    #  glue_col(sep = " : "),
    #do.call(paste(., collapse = " ")),

    HoldStatus = trial$query$trial_hold_status,
    Documentation = trial$query$docs,
 TrialLastUpdate = trial$query$doclastupdate,
 Location = trial$query$locations

  )
  return(parsedTrial)

 }             


###############################################

## read in trials from database to use for browse
# create the equivalent of the 'result' tibble


loadDbData <- function() {
  
  db <- mongolite::mongo(collection = "ClinicalTrials", 
              db = "aci", 
              url = db_url)
  
  # add in trials with trial.full.ndjson
  # db$import(file(here("data/trials/01.full.ndjson")))
  # db$import(file(here("data/trials/02.full.ndjson")))
  # db$import(file(here("data/trials/03.full.ndjson")))
  # db$import(file(here("data/trials/04.full.ndjson")))
  # db$import(file(here("data/trials/05.full.ndjson")))
  # db$import(file(here("data/trials/06.full.ndjson")))
  
  
  # aggregate tibble
  db_tbl <- db$aggregate()[,2:4] %>% 
    unnest(cols = c(info, disease, query))
  
  
  db_tbl <- db_tbl %>% mutate(NameProtocol = glue("{trial_name} : {Protocol_No}", .sep=";")) %>% rename(
                               
                             #   %>% rename( 
    # info
   "Protocol" = Protocol_No,
    "JIT" = jit,
    "Name" = trial_name,
    
    # disease
    "Disease" = summary,
    
    # query - general
   "Title" = title,
   "Status" = current_status,
   
    "StatusUpdate" = status_verif_date,
    "Sponsor" = sponsor,
    "Summary" = brief_summary,
   
    "Conditions" = conditions,
    "Phase" = phase,
    "StudyType" = type,
   # "InclExclCriteria" = criteria,
     # "InclExclCriteria" = db_tbl$details[[1]][2],
    "MinAge" = min_age,
    "Gender" = gender,
    "Link" = link,
   
    "LastUpdate" = last_update_date,
    "HoldStatus" = trial_hold_status,
    "Documentation" = docs,
   "Location" = locations,
   "TrialLastUpdate" = doclastupdate
#   "filtcohort" = filtcohort
  )
  
  
  db_tbl = db_tbl %>% mutate(disp_disease = db_tbl$details)
  db_tbl = db_tbl %>% mutate(disp_disease1 = sapply(db_tbl$details, "[[","code"))
  
  
  
  db_tbl = db_tbl %>% mutate(arms = db_tbl$arm)
  
 # db_tbl = db_tbl %>% mutate(disp1 = db_tbl %>% select(disp_disease) %>% unnest(disp_disease) %>% select(code) %>% distinct())
  
  #db_tbl$disp_disease = list(disp_disease = db_tbl$details[[1]])
  
  # db_tbl$diseasecode <-db_tbl$details[[1]] %>% select(code)
  # db_tbl$diseases<- db_tbl$diseasecode$code
  # 
  # db_tbl$diseasestage <-db_tbl$details[[1]] %>% select(stage)
  # db_tbl$stage <-db_tbl$diseasestage$stage
  
  #add list here and make it similar to how it looks in panel_browse.R with ndjson files
 #db_tbl$arms <- list(db_tbl$arm[[1]] %>% unnest(biomarker))
  
 db_tbl$disp_biomarkers <- "NA"
 db_tbl$lnOfTherapy <- "NA"
 
 db_tbl$filtopencohort <- "NA"
#### db_tbl
 # arm_gr = list(cohortlabel = db_tbl$arm$cohortlabel,
 #                    
                  #  arm_hold_status = db_tbl$arm$arm_hold_status)
 # 
 # 
 # db_tbl = db_tbl$arms %>% unnest() %>% select(cohortlabel, arm_hold_status) %>% mutate(filtcohort = filter(arm_hold_status == "open"))
  
  
 ####
 
 
   #db_tbl$arm[[1]]$biomarker %>% bind_rows() %>%
#    select(summary) %>% distinct() %>%
#    unlist() %>%
#    na.omit() %>%
#    paste0(collapse = "|")
  return(db_tbl)
  
}


source(here("R", "read_data.R"))


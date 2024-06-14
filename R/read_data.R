library(here)
library(dplyr)
library(tidyverse)
library(shinyjs)
library(tidyr)
### read in config parameters

## trials data location
t_d_d <- config::get("trial_data_dir")
trial_data_dir <- if_else(t_d_d %>% fs::is_absolute_path(), t_d_d, t_d_d %>% here())

## patient data location
p_d_f <- config::get("pt_data_file") 
pt_data_file <- if_else(p_d_f %>% fs::is_absolute_path(), p_d_f, p_d_f %>% here())

## trial data storage format
storage <- config::get("storage")

if (storage == "json") {
  # create a combined tibble
  trialsfiles <- dir(path = trial_data_dir, pattern = "*.full.ndjson", full.names = T)
  #trialsfiles = trialsfiles[7:10]
  #data <- trialsfiles %>% map_df(~fromJSON(file.path(trialsfiles, .), flatten = TRUE))
  result <- trialsfiles %>% map(parseTrials) %>% bind_rows()
  browse_tbl <<- result
  
  
  
  
}

if (storage == "db") {
  # look for active mongod process based on docker status
  docker <- config::get("docker")
  
  if (docker == "yes") {
    db_url <<- "host.docker.internal:27017,127.0.0.1:27017" 
  }
  
  if (docker == "no") {
    #db_url <<- "mongodb://0.0.0.0:27017" 
    db_url <<- "mongodb://127.0.0.1:27017"
  }
  
  result <<- loadDbData()
  browse_tbl <<- result
  
  ###new
 # browse_tbl <- browse_tbl %>% mutate(comb_col = html(paste(Documentation, "<br>", Link)))
  ###new
  
  
}

for (i in 1:nrow(browse_tbl)) {
  #n <- browse_tbl$arm[[i]]$biomarker %>% bind_rows() %>% select(summary) %>% distinct() %>% unlist() %>% na.omit() %>% paste0(collapse = "|")<< commented july 25th original 
  n <- browse_tbl$arm[[i]]$biomarker %>% bind_rows() %>% 
    
    #filtering for biomarkers only that belong to inclusion criteria
   # select(`Selection`,summary) %>% filter(`Selection` == "include") %>%  ## previous working
    
    #filtering for biomarkers only that belong to inclusion and exclusion criteria
    select(`Selection`,summary) %>% filter(`Selection` == "include" | `Selection` == "exclude") %>%
    
    select(summary) %>% distinct() %>% unlist() %>% na.omit() %>% paste0(collapse = "|")
  
  
  browse_tbl$disp_biomarkers[i] <- n
}

for (e in 1:nrow(browse_tbl)) {
  se <- browse_tbl$arm[[e]] %>% bind_rows()%>% separate_rows(line_of_therapy,sep =";") %>% select(line_of_therapy) %>% distinct() %>% unlist() %>% na.omit() %>% paste0(collapse = " | ")
  browse_tbl$lnOfTherapy[e] <- se
}

for (k in 1:nrow(browse_tbl)) {
  sel <- browse_tbl$arm[[k]]  %>% bind_rows() %>% filter(arm_hold_status == "open") %>% select(cohortlabel, arm_hold_status) %>% distinct() %>% mutate(filtopencohort = paste0(cohortlabel,"-",arm_hold_status)) %>% select(filtopencohort)  %>% unlist() %>% na.omit() %>%  paste0(collapse = " | ")
 browse_tbl$filtopencohort[k] <- sel
}

#Adding stage for display column

for (v in 1:nrow(browse_tbl)) {
 # sf <- browse_tbl$details[[v]] %>% bind_rows() %>% select(stage) %>% unnest(stage) %>% distinct() %>% na.omit() %>% paste0(collapse = " | ")  commented dec 4 previous working 
  
  sf <- browse_tbl$details[[v]] %>% bind_rows() %>% select(stage) %>% unnest(stage) %>% distinct() %>% na.omit() %>% mutate(stages = paste0(stage, collapse = " | ")) %>% select(stages) %>% distinct() %>% na.omit()  # added to avoid c()
 browse_tbl$stages[v] <- sf
}

### stage for display column ends here



#%>% paste0(cohortlabel,"-",arm_hold_status) %>% unlist() %>% na.omit() %>%  paste0(collapse = " | ")
# Make dataframe for each of the filtration criteria - for now - Drug, cancer type, stage, location and line of therapy 

drugAv = browse_tbl %>% select(arms) %>% unnest(arms) %>% select(drug) %>% distinct()
diseasAv = browse_tbl %>% select(disp_disease) %>% unnest(disp_disease) %>% select(code) %>% distinct()
stageAv = browse_tbl %>% select(disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% select(stage) %>% distinct() 
locAv = browse_tbl %>% select(Location) %>% separate_rows(Location,sep=",") %>% mutate(Location=trimws(Location)) %>% distinct()
trialTyAv = browse_tbl %>% select(JIT) %>%  mutate(JIT =trimws(JIT)) %>% distinct()
lineoftxAv = browse_tbl %>% select(arms) %>% unnest(arms) %>% separate_rows(line_of_therapy,sep = c(";")) %>% select(line_of_therapy) %>% distinct() 
#useShinyjs()
#browse_tbl <- browse_tbl %>% mutate(comb_col = html(paste(Documentation, "<br>", Link)))

#the following is for selective display
#seldiscolumns<- browse_tbl %>% select(Protocol, HoldStatus, Phase, Title, Disease, disp_disease1, lnOfTherapy, disp_biomarkers, Documentation) <<< old columns

seldiscolumns<- browse_tbl %>% select(Location, PrincipalInvestigator, Protocol, Diseasecat, HoldStatus, filtopencohort, stages, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers, JIT)

library(here)
library(dplyr)
library(tidyverse)
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
}

for (i in 1:nrow(browse_tbl)) {
  n <- browse_tbl$arm[[i]]$biomarker %>% bind_rows() %>% select(summary) %>% distinct() %>% unlist() %>% na.omit() %>% paste0(collapse = "|")
  browse_tbl$disp_biomarkers[i] <- n
}

for (e in 1:nrow(browse_tbl)) {
  se <- browse_tbl$arm[[e]] %>% bind_rows()%>% separate_rows(line_of_therapy,sep =";") %>% select(line_of_therapy) %>% distinct() %>% unlist() %>% na.omit() %>% paste0(collapse = " | ")
  browse_tbl$lnOfTherapy[e] <- se
}


# Make dataframe for each of the filtration criteria - for now - Drug, cancer type, stage, location and line of therapy 

drugAv = browse_tbl %>% select(arms) %>% unnest(arms) %>% select(drug) %>% distinct()
diseasAv = browse_tbl %>% select(disp_disease) %>% unnest(disp_disease) %>% select(code) %>% distinct()
stageAv = browse_tbl %>% select(disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% select(stage) %>% distinct()
locAv = browse_tbl %>% select(Location) %>% distinct()

lineoftxAv = browse_tbl %>% select(arms) %>% unnest(arms) %>% separate_rows(line_of_therapy,sep = c(";")) %>% select(line_of_therapy) %>% distinct() 


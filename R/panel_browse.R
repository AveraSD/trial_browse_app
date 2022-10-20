## server side
## display all trials
library(reactable)
# browse table
display_browse_db <- reactable(browse_tbl %>% 
                                #select(Link, Name, Disease, disp_biomarkers, Documentation) %>% 
                                # select(Link, JIT, HoldStatus, Name, Disease, disp_biomarkers, Documentation) %>%
                                # select( Link, Name, Summary, Phase , HoldStatus,  Disease, disp_biomarkers, Documentation) %>%
                                # select( Link, NameProtocol, Name,Protocol_No, HoldStatus, Phase, Summary, Disease, disp_biomarkers, Documentation) %>%
                                 select( Link,Protocol_No, HoldStatus, Phase, Summary, Disease,disp_biomarkers, Documentation) %>%
                                 rename("Trial" = Link,
                                        # "TrialName" = paste(Trial,Name,sep=":"), 
                                        "Title" = Summary,
                                       
                                        "Conditions/Disease" = Disease,
                                        "Biomarker" = disp_biomarkers), 
                               filterable = TRUE,
                               #searchable = TRUE,
                               resizable = TRUE,
                               fullWidth = TRUE,
                               defaultColDef = colDef(align = "center"), 
                               striped = TRUE, 
                               showSortable = TRUE, 
                               style = list(minWidth = 800), 
                               #columns = list(Trial = colDef(html = TRUE)),
                               columns = list(Trial = colDef(html = TRUE),Documentation = colDef(html=TRUE)),
                               details = function(index) {
                                 
                                 # create table for cohort level information
                                 # processBiomarker <- function(x) {
                                 #   b <- arm_groups[x,]$biomarker[[1]] %>% 
                                 #     select(summary) %>% 
                                 #     unlist() %>% 
                                 #     glue_collapse(sep = " | ")
                                 #   return(b)
                                 # }
                                 # 
                                 # arm_groups <- browse_tbl$arm[[index]] %>% 
                                 #   select(cohortlabel, drug, arm_type, line_of_therapy, arm_hold_status, biomarker)
                                 # 
                                 # disp_cohorts = bind_cols(arm_groups %>% select(-biomarker), 
                                 #                          biomarker = lapply(1:nrow(arm_groups), function(x) processBiomarker(x)) %>% 
                                 #                            unlist())
                                 # 
                                 # coh <- disp_cohorts
                                 # coh$drug <- gsub(" \\| NA$", "", coh$drug)
                                 
                                 
                                 # coh <- browse_tbl[index, ]$disp_cohorts$disp_cohorts
                                   
                                 # create tables to be displayed if nested rows are expanded
                                 htmltools::div(
                                   # group 3: summary
                                   # reactable(browse_tbl[index, ] %>%
                                   #             select(Summary)),
                                    
                                   # group1: general info
                                   reactable(browse_tbl[index, ] %>%
                                               #select(JIT, Sponsor, Phase, StudyType, Status, HoldStatus)),
                                  # select(Sponsor, Phase, StudyType, Status,)),
                                   select(Sponsor,StudyType, Status,)),
                                 # group 3: summary
                                 # reactable(browse_tbl[index, ] %>%
                                 #             select(Summary)),
                                 
                                 # group 4: trial conditions
                                 reactable(browse_tbl[index, ] %>%
                                             select(Gender, MinAge, StatusUpdate, LastUpdate),
                                           defaultColDef = colDef(align = "center"),
                                           columns = list(MinAge = colDef(name = "Minimum Age"),
                                                          StatusUpdate = colDef(name = "Status Verification Date"),
                                                          LastUpdate = colDef(name = "Last Update"))),
                                   # group2: cohort info
                                   # reactable(browse_tbl[index, ] %>% 
                                   #            # reactable(coh %>% 
                                   #             select(cohortlabel, drug, arm_type,line_of_therapy,arm_hold_status),
                                   #           columns = list(cohortlabel = colDef(name = "Cohort Label"),
                                   #                          drug = colDef(name = "Drug(s)"),
                                   #                          arm_type = colDef(name = "Arm Type"),
                                   #                        #  biomarker = colDef(name = "Biomarker(s)"), 
                                   #                          line_of_therapy = colDef(name = "Line of Tx"), 
                                   #                          arm_hold_status = colDef(name = "Arm HoldStatus"))),
                                  # reactable(browse_tbl[index, ]$arms$arm %>% 
                                               reactable(browse_tbl[index, ]$arms[[1]] %>% 
                                               # reactable(coh %>% 
                                              # select(arms),
                                               select(cohortlabel, drug, arm_type,line_of_therapy,arm_hold_status,Selection,summary),
                                             columns = list(cohortlabel = colDef(name = "Cohort Label"),
                                                            drug = colDef(name = "Drug(s)"),
                                                            arm_type = colDef(name = "Arm Type"),
                                                            #  biomarker = colDef(name = "Biomarker(s)"), 
                                                            line_of_therapy = colDef(name = "Line of Tx"), 
                                                            arm_hold_status = colDef(name = "Arm HoldStatus"),
                                                            Selection = colDef(name = "Criteria"),
                                                            summary = colDef(name = "Biomarker"))),
                                 
                                  #  reactable(browse_tbl[index, ]$disp_disease$disp_disease), 
                                 reactable(browse_tbl[index, ]$disp_disease[[1]] %>% select(code, selection))
                                              # %>% 
                                    #             select(code, selection),
                                    # #          # columns = list(summary = colDef("summary"),
                                    #               columns = list(code = colDef("code"),
                                    #                           selection = colDef("selection"))),

                                   # group 3: summary
                                   # reactable(browse_tbl[index, ] %>%
                                   #             select(Summary)),

                                   
                                   
                                   # group 4: trial conditions
                                   # reactable(browse_tbl[index, ] %>%
                                   #             select(Gender, MinAge, StatusUpdate, LastUpdate),
                                   #           defaultColDef = colDef(align = "center"),
                                   #           columns = list(MinAge = colDef(name = "Minimum Age"),
                                   #                          StatusUpdate = colDef(name = "Status Verification Date"),
                                   #                          LastUpdate = colDef(name = "Last Update")))
                                 )
                               })

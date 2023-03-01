#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(dplyr)
library(tidyverse)



# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  # first time pass all the trials 
  selecTrial = reactiveValues(comTb=tibble())
  
  # reactive to get the data if any of the buttons are clicked 
  #filtered <- eventReactive(input$loc_fil,{
  observeEvent(input$loc_fil,{
    
    # To stop errors popping up in app if nothing is chosen by default
    shinyjs::hide(id = "browsetable")
    shinyjs::show(id = "filterbrowse")
    
    # selection 
    SelStage = as.list.data.frame(input$stageView)
    checkStageSel = browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(stage %in% SelStage) %>% select(NCT) %>% distinct()
    print(isTRUE(SelStage))
    print(length(checkStageSel$NCT))
    SelDise = as.list.data.frame(input$disFil)
    checkDiseSel = browse_tbl %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(code %in% SelDise) %>% select(NCT) %>% distinct()
   
     SelDrug = as.list.data.frame(input$drugFil)
    checkDrugSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% filter(drug %in% SelDrug) %>% select(NCT) %>% distinct()
   
    SelLineofTx = as.list.data.frame(input$lineofTxFil)
    checklineoftxSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% separate_rows(line_of_therapy,sep = ";") %>% filter(line_of_therapy %in% SelLineofTx) %>% select(NCT) %>% distinct()
   
    SelLocat = as.list.data.frame(input$locaFil)
    checklocat = browse_tbl %>% select(NCT,Location) %>% filter(Location %in%  SelLocat) %>% select(NCT) %>% distinct()
    
    
    # ----------------------------------------------------------------------------------------------------------------------- #
    # part 2 options 
    if(length(checkStageSel$NCT) >= 1  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0){

      # in all four options
      completeList = c(unique(checkStageSel$NCT))
      print(completeList)

    }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) >=1 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0){

      # in all disease options
      completeList = c( unique(checkDiseSel$NCT))
      print(completeList)
      
    }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) >=1 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 ){

      # in all Drug options
      completeList =  c(unique(checkDrugSel$NCT))
      print(completeList)


    }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) >= 1 && length(checklocat$NCT) == 0 ){

      # in all line of therapy option
      completeList = c(unique(checklineoftxSel$NCT))
      print(completeList)

    }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) >=1  ){
      
      # in all Location option
      completeList = c(unique(checklineoftxSel$NCT))
      
    }else{
      
      matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT), unique(checklocat$NCT) )
      ntb = as.data.frame(table(matchList))
      maxNb = max(ntb$Freq)
      ntb = ntb %>% filter(Freq >= maxNb )
      completeList = c(ntb$matchList)
      print(completeList)
      
    }


  #  matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT) )
  #  ntb = as.data.frame(table(matchList))
  #  print(ntb)
    
    # ----------------------------------------------------------------------------------------------------------------------- #
    # part 1 options 
     # 
    # if(!is.na(checkStageSel$NCT) || !is.na(checkDiseSel$NCT) || !is.na(checkDrugSel$NCT) || !is.na(checklineoftxSel$NCT) ){
    #   
    #   matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT) )
    #   ntb = table(matchList)
    #   #completeList = ntb %>% filter(n = 1) %>% .$x
    #   
    # }else
    # if(!is.na(checkStageSel$NCT) & !is.na(checkDiseSel$NCT) & !is.na(checkDrugSel$NCT) & !is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all four options
    #  matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT) )
    #  ntb = table(matchList)
    # # completeList = ntb %>% filter(n >= 3) %>% .$x
    #   
    # }else if(!is.na(checkStageSel$NCT) & !is.na(checkDiseSel$NCT) & !is.na(checkDrugSel$NCT) & is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all stage disease drug options
    #   matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT))
    #   ntb = table(matchList)
    #   print("stage disease drug options")
    #   print(ntb)
    #   #completeList = ntb %>% filter(n >= 2) %>% .$x
    #   
    # }else if(!is.na(checkStageSel$NCT) & !is.na(checkDiseSel$NCT) & is.na(checkDrugSel$NCT) & !is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all stage disease line of therapy options
    #   matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checklineoftxSel$NCT))
    #   ntb = table(matchList)
    #  # completeList = ntb %>% filter(n >= 2) %>% .$x
    #   
    #   
    # }else if(is.na(checkStageSel$NCT) & !is.na(checkDiseSel$NCT) & !is.na(checkDrugSel$NCT) & !is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all disease drug line of therapy options
    #   matchList = c(unique(checkDiseSel$NCT),  unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT))
    #   ntb = table(matchList)
    #  # completeList = ntb %>% filter(n >= 2) %>% .$x
    #   
    # }else if(!is.na(checkStageSel$NCT) & is.na(checkDiseSel$NCT) & !is.na(checkDrugSel$NCT) & !is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all stage drug line of therapy options
    #   matchList = c(unique(checkStageSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT))
    #   ntb = table(matchList)
    #   print(" stage drug line of therapy ")
    #   print(ntb)
     # completeList = ntb %>% filter(n >= 2) %>% .$x
      
      
    # }else if(!is.na(checkStageSel$NCT) & !is.na(checkDiseSel$NCT) & is.na(checkDrugSel$NCT) & !is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all stage disease line of therapy options
    #   matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checklineoftxSel$NCT))
    #   ntb = table(matchList)
    #   completeList = ntb %>% filter(n >= 2) %>% .$x
    #   
    #   
    # }else if(is.na(checkStageSel$NCT) & !is.na(checkDiseSel$NCT) & !is.na(checkDrugSel$NCT) & is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all disease drug options
    #   matchList = c(unique(checkDiseSel$NCT),unique(checkDrugSel$NCT))
    #   ntb = table(matchList)
    #   completeList = ntb %>% filter(n = 2) %>% .$x
    #   
    #   
    # }else if(!is.na(checkStageSel$NCT) & is.na(checkDiseSel$NCT) & !is.na(checkDrugSel$NCT) & is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all stage drug options
    #   matchList = c(unique(checkStageSel$NCT),unique(checkDrugSel$NCT))
    #   ntb = table(matchList)
    #   completeList = ntb %>% filter(n = 2) %>% .$x
    #   
    #   
    # }else if(is.na(checkStageSel$NCT) & is.na(checkDiseSel$NCT) & !is.na(checkDrugSel$NCT) & !is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all drug line of therapy options
    #   matchList = c(unique(checklineoftxSel$NCT),unique(checkDrugSel$NCT))
    #   ntb = table(matchList)
    #   completeList = ntb %>% filter(n = 2) %>% .$x
    #   
    #   
    # }else if(is.na(checkStageSel$NCT) & !is.na(checkDiseSel$NCT) & is.na(checkDrugSel$NCT) & !is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all disease line of therapy options
    #   matchList = c(unique(checklineoftxSel$NCT),unique(checkDiseSel$NCT))
    #   ntb = table(matchList)
    #   completeList = ntb %>% filter(n = 2) %>% .$x
    #   
    #   
    # }else if(!is.na(checkStageSel$NCT) & is.na(checkDiseSel$NCT) & is.na(checkDrugSel$NCT) & !is.na(checklineoftxSel$NCT) ){
    #   
    #   # in all stage line of therapy options
    #   matchList = c(unique(checklineoftxSel$NCT),unique(checkStageSel$NCT) )
    #   ntb = table(matchList)
    #   completeList = ntb %>% filter(n = 2) %>% .$x
    #   
    # }else{
    #   matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT) )
    #      ntb = table(matchList)
    # }
    # 
   
    #%>% filter(NCT %in% completeList)
  
    
    #completeList = c(checkStageSel$NCT, checkDiseSel$NCT,checkDrugSel$NCT,checklineoftxSel$NCT )
    #print(completeList)

    # ----------------------------------------------------------------------------------------------------------------------- #
   filTb = browse_tbl %>% filter(NCT %in% completeList )
   
   output$filterbrowse <- renderReactable({
   reactable(filTb %>% dplyr::select(Link, Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers, Documentation),
             filterable = TRUE,
             #searchable = TRUE,
             resizable = TRUE,
             fullWidth = TRUE,
             defaultColDef = colDef(align = "center"),
             striped = TRUE,
             showSortable = TRUE,
             style = list(minWidth = 800),
             #columns = list(Trial = colDef(html = TRUE)),
             columns = list(Link = colDef(html = TRUE,name = "Trial"), HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy"), Disease = colDef(name = "Conditions/Disease"),
                            disp_biomarkers = colDef(name = "Biomarker"), Documentation = colDef(html=TRUE)),
             details = function(index) { 
               
               # create table for cohort level information
               
               
               # create tables to be displayed if nested rows are expanded
               htmltools::div(
                
                 # group1: general info
                 reactable(filTb[index, ] %>% select(Name,Sponsor,StudyType, Location, TrialLastUpdate),
                           defaultColDef = colDef(align = "center"),
                           columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"))
                 ),
                 
                 # group 3: summary
                 reactable(filTb[index, ] %>%
                             select(Summary)),
                 
                 
                 # group 4: trial Status from .gov
                 reactable(filTb[index, ] %>%
                             select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                           defaultColDef = colDef(align = "center"),
                           columns = list(Status = colDef(name = "Clinical.gov Status"),
                                          MinAge = colDef(name = "Minimum Age"),
                                          StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                          LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                 # group2: cohort info
                 
                 # reactable(browse_tbl[index, ]$arms$arm %>%
                 reactable(filTb[index, ]$arms[[1]] %>% unnest(biomarker) %>%
                             # reactable(coh %>%
                             # select(arms),
                             select(cohortlabel, drug, arm_type,line_of_therapy,arm_hold_status,Selection,summary) %>% distinct(),
                           columns = list(cohortlabel = colDef(name = "Cohort Label"),
                                          drug = colDef(name = "Drug(s)"),
                                          arm_type = colDef(name = "Arm Type"),
                                          #  biomarker = colDef(name = "Biomarker(s)"),
                                          line_of_therapy = colDef(name = "Line of Tx"),
                                          arm_hold_status = colDef(name = "Arm HoldStatus"),
                                          Selection = colDef(name = "Criteria"),
                                          summary = colDef(name = "Biomarker")
                                          #details = function(index){
                                          #reactable(browse_tbl[index, ]$arms[[1]] %>% select(Gene,Gene2,Type,Variant,Selection,Function))
                                          # })
                           )),
                 # group 5: CONDITIONS MENTIONED FROM .GOV
                 reactable(filTb[index, ] %>%
                             select(Conditions)),
                 
                 # group 3: disease information
                 
                 #  reactable(browse_tbl[index, ]$disp_disease$disp_disease),
                 reactable(filTb[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
                  
               )
             }) 
   })
   
  # return(filTb)
  })
 
  # Reset button
 observeEvent(input$reset_btn_browse, {
 
   shinyjs::show(id = "browsetable")
   shinyjs::hide(id = "filterbrowse")
   
   updateSelectInput(inputId = "stageView",selected = "")
   updateSelectInput(inputId = "disFil",selected = "")
   updateSelectInput(inputId = "drugFil",selected = "")
   updateSelectInput(inputId = "locaFil",selected = "")
   updateSelectInput(inputId = "lineofTxFil",selected = "")
   updateRadioButtons(inputId = "filtercond", selected = character(0))
 })

  ##### BROWSE ########b
  # main display table for BROWSE
  output$browsetable <- renderReactable({
  
     selecTrial$comTb = as_tibble(browse_tbl)
     
  #selecTrial$comTb %>% select(arm) %>% unnest(arm) %>% select(line_of_therapy)
     
     
     reactable::reactable( selecTrial$comTb %>% dplyr::select(Link, Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers, Documentation),
                filterable = TRUE,
                #searchable = TRUE,
                resizable = TRUE,
                fullWidth = TRUE,
                defaultColDef = colDef(align = "center"),
                striped = TRUE,
                showSortable = TRUE,
                style = list(minWidth = 800),
                #columns = list(Trial = colDef(html = TRUE)),
                columns = list(Link = colDef(html = TRUE,name = "Trial"), HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy") ,Disease = colDef(name = "Conditions/Disease"),
                               disp_biomarkers = colDef(name = "Biomarker"), Documentation = colDef(html=TRUE)),
                details = function(index) {
                  # create tables to be displayed if nested rows are expanded
                  htmltools::div(
                   
                    # group1: general info
                    reactable(selecTrial$comTb[index, ] %>% select(Name,Sponsor,StudyType, Location, TrialLastUpdate),
                              defaultColDef = colDef(align = "center"),
                              columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"))
                    ),
                    
                    # group 3: summary
                    reactable(selecTrial$comTb[index, ] %>%
                                select(Summary)),
                    
                    
                    # group 4: trial Status from .gov
                    reactable(selecTrial$comTb[index, ] %>%
                                select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                              defaultColDef = colDef(align = "center"),
                              columns = list(Status = colDef(name = "Clinical.gov Status"),
                                             MinAge = colDef(name = "Minimum Age"),
                                             StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                             LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                    # group2: cohort info
                    
                    # reactable(browse_tbl[index, ]$arms$arm %>%
                    reactable(selecTrial$comTb[index, ]$arms[[1]] %>% unnest(biomarker) %>%
                                # reactable(coh %>%
                                # select(arms),
                                select(cohortlabel, drug, arm_type,line_of_therapy,arm_hold_status,Selection,summary) %>% distinct(),
                              columns = list(cohortlabel = colDef(name = "Cohort Label"),
                                             drug = colDef(name = "Drug(s)"),
                                             arm_type = colDef(name = "Arm Type"),
                                             #  biomarker = colDef(name = "Biomarker(s)"),
                                             line_of_therapy = colDef(name = "Line of Tx"),
                                             arm_hold_status = colDef(name = "Arm HoldStatus"),
                                             Selection = colDef(name = "Criteria"),
                                             summary = colDef(name = "Biomarker")
                                             #details = function(index){
                                             #reactable(browse_tbl[index, ]$arms[[1]] %>% select(Gene,Gene2,Type,Variant,Selection,Function))
                                             # })
                              )),
                    # group 5: CONDITIONS MENTIONED FROM .GOV
                    reactable(selecTrial$comTb[index, ] %>%
                                select(Conditions)),
                    
                    # group 3: disease information
                    
                    #  reactable(browse_tbl[index, ]$disp_disease$disp_disease),
                    reactable(selecTrial$comTb[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
                    
                    
                  )
                })
  
 #    }
 #    else {
 #      if(!is.null(input$loc_fil)) {
      
 #    }
   # display_browse_db # from panel_browse.R
  })
  
  # collapse button
  observeEvent(input$collapse_btn_browse, {
    updateReactable("browsetable", expanded = FALSE)
  })
  
  
  observeEvent(input$collapse_btn_browse, {
    updateReactable("filterbrowse", expanded = FALSE)
  })
  
  ##### MATCH ########
  # main display table for match
  # output$matchtable <- renderReactable({
  #   display_match_gen # from panel_match.R
  # })

})


# # selection on stage 
#  checkStage = eventReactive(input$filter_stage,{
#   #print(input$stageView)
#   SelStage = as.list.data.frame(input$stageView)
#   #print(SelStage)
#   checkStageSel = browse_tbl %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";")%>% filter(stage %in% SelStage) %>% select(NCT) %>% distinct()
#   #selTrial = browse_tbl %>% filter(NCT %in% checkStageSel$NCT)
#   #print(checkStageSel)
#   return(checkStageSel)
# })
#  
#  # selection on Disease
#  checkDise = eventReactive(input$dise_fil,{
#    #print(input$stageView)
#    SelDise = as.list.data.frame(input$disFil)
#    #print(SelStage)
#    checkDiseSel = browse_tbl %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";")%>% filter(code %in% SelDise) %>% select(NCT) %>% distinct()
#    #selTrial = browse_tbl %>% filter(NCT %in% checkDiseSel$NCT)
#    return(checkDiseSel)
#  })
#  
#  # selection on Drug
#  checkDrug = eventReactive(input$drug_fil,{
#    #print(input$stageView)
#    SelDrug = as.list.data.frame(input$drugFil)
#    #print(SelStage)
#    checkDrugSel = browse_tbl %>% select(NCT,disp_cohorts) %>% unnest(disp_cohorts) %>% filter(drug %in% SelDrug) %>% select(NCT) %>% distinct()
#    #selTrial = browse_tbl %>% filter(NCT %in% checkDrugSel$NCT)
#    return(checkDrugSel)
#    #print(checkDrugSel)
#  })
#  
#  
#  
#  # selection on locations
#  checkLoc = eventReactive(input$loc_fil,{
#    #print(input$stageView)
#    SelLoc = as.list.data.frame(input$locaFil )
#    #print(SelStage)
#    checklocSel = browse_tbl %>% select(NCT,Location) %>% filter(Location %in% SelLoc) %>% select(NCT) %>% distinct()
#    #selTrial = browse_tbl %>% filter(NCT %in% checklocSel$NCT)
#    return(checklocSel)
#    #print(checklocSel)
#  })

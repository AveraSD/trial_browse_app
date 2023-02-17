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




# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  # first time pass all the trials 
  selecTrial = reactiveValues(comTb=tibble())
  # output$input_fst <- renderUI({
  #   input_fst
  # })

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
  
  # reactive to get the data if any of the buttons are clicked 
  #filtered <- eventReactive(input$loc_fil,{
  observeEvent(input$loc_fil,{
    
    shinyjs::hide(id = "browsetable")
    shinyjs::show(id = "filterbrowse")
    # To stop errors popping up in app if nothing is chosen by default
    SelStage = as.list.data.frame(input$stageView)
   # print(SelStage)
    checkStageSel = browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";")%>% filter(stage %in% SelStage) %>% select(NCT) %>% distinct()
    print(checkStageSel)
    SelDise = as.list.data.frame(input$disFil)
    checkDiseSel = browse_tbl %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";")%>% filter(code %in% SelDise) %>% select(NCT) %>% distinct()
    SelDrug = as.list.data.frame(input$drugFil)
    checkDrugSel = browse_tbl %>% select(NCT,disp_cohorts) %>% unnest(disp_cohorts) %>% filter(drug %in% SelDrug) %>% select(NCT) %>% distinct()
    completeList = c(checkStageSel$NCT, checkDiseSel$NCT,checkDrugSel$NCT )
    print(completeList)
    # if (is.null(checkLoc()) || is.null(checkDrug()) || is.null(checkDise()) || is.null(checkStage()) ) {
    #   return(NULL)
    # }
    
    # output$filterhalf <- renderUI({
    #   filUIhalf
    # })
   filTb = browse_tbl  %>% filter(NCT %in% completeList ) %>% distinct()  # Filter based on the interactive input 
     
    
      #filter(NCT %in% c(checkLoc(),checkDrug(),checkDise(),checkStage() ))
   # print(selecTrial$comTb)
  # selecTrial$comTb = filTb 
   #selecTrial$comTb = filtered()
   #selecTrial$comTb = browse_tbl
   #  print(selecTrial$comTb$NCT)
   output$filterbrowse <- renderReactable({
   reactable(filTb %>% select( Link, Protocol, HoldStatus, Phase, Title, Disease, disp_biomarkers, Documentation),
             # rename("Trial" = Link,
             #        # "TrialName" = paste(Trial,Name,sep=":"),
             #        #"Title" = Summary,
             #        "Current Status" = HoldStatus,
             #        "Conditions/Disease" = Disease,
             #        "Biomarker" = disp_biomarkers),
             filterable = TRUE,
             #searchable = TRUE,
             resizable = TRUE,
             fullWidth = TRUE,
             defaultColDef = colDef(align = "center"),
             striped = TRUE,
             showSortable = TRUE,
             style = list(minWidth = 800),
             #columns = list(Trial = colDef(html = TRUE)),
             columns = list(Link = colDef(html = TRUE,name = "Trial"), HoldStatus = colDef(name = "Current Status"),Disease = colDef(name = "Conditions/Disease"),
                            disp_biomarkers = colDef(name = "Biomarker"), Documentation = colDef(html=TRUE)),
             details = function(index) {
               
               # create table for cohort level information
               
               
               # create tables to be displayed if nested rows are expanded
               htmltools::div(
                 # group 3: summary
                 # reactable(browse_tbl[index, ] %>%
                 #             select(Summary)),
                 
                 # group1: general info
                 reactable(filTb[index, ] %>% select(Sponsor,StudyType, Location, TrialLastUpdate),
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
                 reactable(filTb[index, ]$arms[[1]] %>%
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
  # selecTrial$comTb = browse_tbl
  #output$input_fst <- renderUI({
  #  input_fst
  #})
   shinyjs::show(id = "browsetable")
   shinyjs::hide(id = "filterbrowse")
   
   
   #output$filterbrowse <- renderReactable({
  # })
  # updateReactable("filterbrowse", data = NULL)
 })

  ##### BROWSE ########b
  # main display table for BROWSE
  output$browsetable <- renderReactable({
   # brw$selecTrial = browse_tbl
    #selecTrial = reactive(browse_tbl)
   # case_when()
  #  if(is.null(input$loc_fil)){
   
     # selecTrial$comTb = filtered()
     selecTrial$comTb = browse_tbl
      reactable(selecTrial$comTb %>% select( Link, Protocol, HoldStatus, Phase, Title, Disease, disp_biomarkers, Documentation),
                # rename("Trial" = Link,
                #        # "TrialName" = paste(Trial,Name,sep=":"),
                #        #"Title" = Summary,
                #        "Current Status" = HoldStatus,
                #        "Conditions/Disease" = Disease,
                #        "Biomarker" = disp_biomarkers),
                filterable = TRUE,
                #searchable = TRUE,
                resizable = TRUE,
                fullWidth = TRUE,
                defaultColDef = colDef(align = "center"),
                striped = TRUE,
                showSortable = TRUE,
                style = list(minWidth = 800),
                #columns = list(Trial = colDef(html = TRUE)),
                columns = list(Link = colDef(html = TRUE,name = "Trial"), HoldStatus = colDef(name = "Current Status"),Disease = colDef(name = "Conditions/Disease"),
                               disp_biomarkers = colDef(name = "Biomarker"), Documentation = colDef(html=TRUE)),
                details = function(index) {
                  
                  # create table for cohort level information
                  
                  
                  # create tables to be displayed if nested rows are expanded
                  htmltools::div(
                    # group 3: summary
                    # reactable(browse_tbl[index, ] %>%
                    #             select(Summary)),
                    
                    # group1: general info
                    reactable(selecTrial$comTb[index, ] %>% select(Sponsor,StudyType, Location, TrialLastUpdate),
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
                    reactable(selecTrial$comTb[index, ]$arms[[1]] %>%
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
  
  ##### MATCH ########
  # main display table for match
  # output$matchtable <- renderReactable({
  #   display_match_gen # from panel_match.R
  # })

})

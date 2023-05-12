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
library(htmltools)


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
    # 
    SelLineofTx = as.list.data.frame(input$lineofTxFil)
    checklineoftxSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% separate_rows(line_of_therapy,sep = ";") %>% filter(line_of_therapy %in% SelLineofTx) %>% select(NCT) %>% distinct()
   
    SelLocat = as.list.data.frame(input$locaFil)
    checklocat = browse_tbl %>% select(NCT,Location) %>% filter(Location %in%  SelLocat) %>% select(NCT) %>% distinct()
    
    # for the trial type 
    # SelTrialty = as.list.data.frame(input$lxFil) # Ui name
    #checktrlTy = browse_tbl %>% select(NCT,#column name "jit") %>% filter(#column name "jit" %in%  SelTrialty) %>% select(NCT) %>% distinct()
   
   
   # ----------------------------------------------------------------------------------------------------------------------- #
    # part 2 options 
    if(length(checkStageSel$NCT) >= 1  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0){

      # in all four options
      completeList = c(unique(checkStageSel$NCT))
      print(completeList)

    }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) >=1 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0 ){

      # in all disease options
      completeList = c( unique(checkDiseSel$NCT))
      print(completeList)
      
    }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) >=1 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0 ){

      # in all Drug options
      completeList =  c(unique(checkDrugSel$NCT))
      print(completeList)


    }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) >= 1 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0 ){

      # in all line of therapy option
      completeList = c(unique(checklineoftxSel$NCT))
      print(completeList)

    }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) >=1 && length(checktrlTy$NCT) == 0  ){
      
      # in all Location option
      completeList = c(unique(checklineoftxSel$NCT))
      
    }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0  && length(checktrlTy$NCT) >=1  ){
      
      # in all Trial type option
      completeList = c(unique(checktrlTy$NCT))
      
    }else{
      
      matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT), unique(checklocat$NCT), unique(checktrlTy$NCT) )
      ntb = as.data.frame(table(matchList))
      maxNb = max(ntb$Freq)
      ntb = ntb %>% filter(Freq >= maxNb )
      completeList = c(ntb$matchList)
      print(completeList)
      
    }


    # ----------------------------------------------------------------------------------------------------------------------- #
   
    
    
    
    filTb = browse_tbl %>% filter(NCT %in% completeList )
   
    ###checking to combine columns april 25
    #filTb <- filTb %>% mutate(comb_col = html(paste(Documentation, "<br>", Link)))
    ######## april 25
    
    
    
   output$filterbrowse <- renderReactable({
   #reactable(filTb %>% dplyr::select(Link, Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers, Documentation),
             reactable(filTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_disease1, disp_biomarkers,  Documentation), 
                      filterable = TRUE,
             #searchable = TRUE,
             resizable = TRUE,
             fullWidth = TRUE,
             defaultColDef = colDef(align = "center"),
             striped = TRUE,
             showSortable = TRUE,
             style = list(minWidth = 800),
             #columns = list(Trial = colDef(html = TRUE)),
           #  columns = list(Link = colDef(html = TRUE,name = "Trial"), HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy"), Disease = colDef(name = "Conditions/Disease"),
                    #        disp_biomarkers = colDef(name = "Biomarker"), Documentation = colDef(html=TRUE)),
             
             columns = list( HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy"), Disease = colDef(name = "Conditions/Disease"),
                            disp_biomarkers = colDef(name = "Biomarker"), disp_disease1 = colDef(name = "Cancer Type"), Documentation = colDef(html=TRUE), 
                            Title = colDef(name = "Title", minWidth = 300 ,style = list(fontWeight = "bold"))
                            
                            ),
             details = function(index) { 
               
               # create table for cohort level information
               
               
               # create tables to be displayed if nested rows are expanded
               htmltools::div(
                
                 # group1: general info
                 reactable(filTb[index, ] %>% select(Link,Name,Sponsor,StudyType, Location, TrialLastUpdate),
                           defaultColDef = colDef(align = "center"),
                           columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial"))
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
   
   updateSelectInput(inputId = "selcolumns",selected = "")
   
   
   updateRadioButtons(inputId = "filtercond", selected = character(0))
 })

  ##### BROWSE ########b
  # main display table for BROWSE
 
#browse_tbl <- browse_tbl %>% mutate(comb_col = paste(Documentation,Link, sep="\n"))
 
 
 
  output$browsetable <- renderReactable({
  
     selecTrial$comTb = as_tibble(browse_tbl)
     
  #selecTrial$comTb %>% select(arm) %>% unnest(arm) %>% select(line_of_therapy)
     
   # selecTrial$comTb <- selecTrial$comTb %>% mutate(comb_col = htmltools::HTML(paste(Documentation,"<br>",Link)))
     
     
   #  selecTrial$comTb <- selecTrial$comTb %>% mutate(comb_col = glue("<a href='{link}'>{Documentation}</a>"))
     
   #  reactable::reactable( selecTrial$comTb %>% dplyr::select(Link, Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers, Documentation),
                           reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_disease1, disp_biomarkers, Documentation),    
                                                 #    reactable::reactable( selecTrial$comTb[, input$selcolumns], 
                                                 filterable = TRUE,
                                                 #searchable = TRUE,
                                                 
                                                 #       columnDefs = list(list(targets = 4, width = 800)),
                                                 
                                                 resizable = TRUE,
                                                 fullWidth = TRUE,
                                                 defaultColDef = colDef(align = "center"),
                                                 striped = TRUE,
                                                 showSortable = TRUE,
         
               # style = list(minWidth = 800), <<< original
             #  style = list(fontWeight = "bold")  <<< this is for bold all columns working,
         
                #columns = list(Trial = colDef(html = TRUE)),
         
             
         #old       #columns = list(Link = colDef(html = TRUE,name = "Trial"), HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy") ,Disease = colDef(name = "Conditions/Disease"),
             #oldworking    #              disp_biomarkers = colDef(name = "Biomarker"), Documentation = colDef(html=TRUE)),
       
         
          # columns = list(colDef("Title", header = "Title", formatter = function(value){HTML(paste0("<b>", value, "</b>"))})),
       
         columns = list(HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy") ,Disease = colDef(name = "Conditions/Disease"),
                               disp_biomarkers = colDef(name = "Biomarker"), disp_disease1 = colDef(name = "Cancer Type"), Documentation = colDef(html=TRUE), 
                      Title = colDef(name = "Title", minWidth = 300 ,style = list(fontWeight = "bold"))
              
                      #        Title = colDef(name = "Title", minWidth = 300)  
                   #   Title = colDef(Title = list(fontWeight = "bold"))
                      
                      ),
       #company = colDef(name = "Company", minWidth = 100),
                 
        # style = list(".rt-td:nth-child(6)" = list("width" = "1500px"), ".rt-table" = list("width" = "10000px"),
        #              
        #              ".rt-th:nth-child(6)" = list("font-weight" = "bold")),
       
          
      # columnProps = list(Title = colDef(style = "font-weight:bold; min-width: 1500px")),
             
                details = function(index) {
                  # create tables to be displayed if nested rows are expanded
                  htmltools::div(
                   
                    # group1: general info
                    reactable(selecTrial$comTb[index, ] %>% select(Link, Name,Sponsor,StudyType, Location, TrialLastUpdate),
                              defaultColDef = colDef(align = "center"),
                              columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial"))
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

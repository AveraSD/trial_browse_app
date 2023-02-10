#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)




# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  # first time pass all the trials 
  selecTrial = reactive(browse_tbl)

 # selection on stage 
  checkStage = eventReactive(input$filter_stage,{
   #print(input$stageView)
   SelStage = as.list.data.frame(input$stageView)
   #print(SelStage)
   checkStageSel = result %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";")%>% filter(stage %in% SelStage) %>% select(NCT) %>% distinct()
   selTrial = browse_tbl %>% filter(NCT %in% checkStageSel$NCT)
   #print(checkStageSel)
   return(selTrial)
 })
  
  # selection on Disease
  checkDise = eventReactive(input$dise_fil,{
    #print(input$stageView)
    SelDise = as.list.data.frame(input$disFil)
    #print(SelStage)
    checkDiseSel = result %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";")%>% filter(code %in% SelDise) %>% select(NCT) %>% distinct()
    selTrial = browse_tbl %>% filter(NCT %in% checkDiseSel$NCT)
    return(selTrial)
  })
  
  # selection on Drug
  checkDrug = eventReactive(input$drug_fil,{
    #print(input$stageView)
    SelDrug = as.list.data.frame(input$drugFil)
    #print(SelStage)
    checkDrugSel = result %>% select(NCT,disp_cohorts) %>% unnest(disp_cohorts) %>% filter(drug %in% SelDrug) %>% select(NCT) %>% distinct()
    selTrial = browse_tbl %>% filter(NCT %in% checkDrugSel$NCT)
    return(selTrial)
    #print(checkDrugSel)
  })
  
  
  
  # selection on locations
  checkLoc = eventReactive(input$loc_fil,{
    #print(input$stageView)
    SelLoc = as.list.data.frame(input$locaFil )
    #print(SelStage)
    checklocSel = result %>% select(NCT,Location) %>% filter(Location %in% SelLoc) %>% select(NCT) %>% distinct()
    selTrial = browse_tbl %>% filter(NCT %in% checklocSel$NCT)
    return(selTrial)
    #print(checklocSel)
  })
  
  # collapse button
 observeEvent(input$reset_btn_browse, {
   selecTrial = reactive(browse_tbl)
 })

  ##### BROWSE ########
  # main display table for BROWSE
  output$browsetable <- renderReactable({
   # brw$selecTrial = browse_tbl
    #selecTrial = reactive(browse_tbl)
   # case_when()
    if(is.null(checkedTb())){
      print(is.null(checkedTb()) )
      tableExpand = selecTrial()
    }else {
      tableExpand = checkedTb()
    }
    print(tableExpand)
    #tableExpand = selecTrial()
    reactable(tableExpand %>%
                # select( Link, NameProtocol, Name,Protocol_No, HoldStatus, Phase, Summary, Disease, disp_biomarkers, Documentation) %>%
                #select( Link, Protocol,HoldStatus, Phase, Summary, Disease,disp_biomarkers, Documentation) %>%
                select( Link, Protocol, HoldStatus, Phase, Title, Disease, disp_biomarkers, Documentation) %>%
                rename("Trial" = Link,
                       # "TrialName" = paste(Trial,Name,sep=":"),
                       #"Title" = Summary,
                       "Current Status" = HoldStatus,
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
                  reactable(browse_tbl[index, ] %>% select(Sponsor,StudyType, Location, TrialLastUpdate),
                            defaultColDef = colDef(align = "center"),
                            columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"))
                  ),

                  # group 3: summary
                  reactable(browse_tbl[index, ] %>%
                              select(Summary)),

                  # group 4: trial conditions
                  reactable(browse_tbl[index, ] %>%
                              select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                            defaultColDef = colDef(align = "center"),
                            columns = list(Status = colDef(name = "Clinical.gov Status"),
                                           MinAge = colDef(name = "Minimum Age"),
                                           StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                           LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                  # group2: cohort info

                  # reactable(browse_tbl[index, ]$arms$arm %>%
                  reactable(browse_tbl[index, ]$arms[[1]] %>%
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

                  #  reactable(browse_tbl[index, ]$disp_disease$disp_disease),
                  reactable(browse_tbl[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
                 
                 
                )
              })
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

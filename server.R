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
  #  checkStageSel = browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(stage %in% SelStage) %>% select(NCT) %>% distinct() #old working
    
    checkStageSel = browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(str_detect(stage, paste(SelStage,collapse = "|"))) %>% select(NCT) %>% distinct() #old working final
    print(isTRUE(SelStage))
    print(length(checkStageSel$NCT))
    
    # reactive_data1<- reactive({
    #   data <- browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(str_detect(stage, paste(SelStage,collapse = "|"))) %>% select(NCT) %>% distinct()
    #   return(data)
    # })
    #   checkStageSel = reactive_data1
    
          
    
    
    SelDise = as.list.data.frame(input$disFil)
   # checkDiseSel = browse_tbl %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(code %in% SelDise) %>% select(NCT) %>% distinct() #old working
   # checkDiseSel = browse_tbl %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(str_detect(code, paste(SelDise,collapse = "|"))) %>% select(NCT) %>% distinct()
    
    SelDise1<- as.list.data.frame(gsub("\\([^()]*\\)","",SelDise))
   
    SelDise1<- trimws(SelDise1)
    print(SelDise1)
   # checkDiseSel = browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(disp_disease1,SelDise1)) %>% select(NCT) %>% distinct()  # working str_detect
  #  checkDiseSel = browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect((trimws(disp_disease1)),paste(SelDise1,collapse = "|"))) %>% select(NCT) %>% distinct() # working good with trimws

    
    #    the following line is for using without reactive
 #   checkDiseSel = browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",SelDise1,"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()   #works final with b without reactive
    
    
 ####new block for addressing broad and specific cancer type search
    reactive_data <- reactive({
      
      if(SelDise1 == "Lung Cancer" | SelDise1 == "Lung") {
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Lung","Lung Cancer"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }else if(SelDise1 == "Breast Cancer" | SelDise1 == "Breast"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Breast","Breast Cancer"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      } else if(SelDise1 == "Small Cell Lung Cancer"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("^",c("Small Cell Lung Cancer"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      } else if(SelDise1 == "Skin" | SelDise1 == "Melanoma"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Skin","Melanoma"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }else if(SelDise1 == "Ovary/Fallopian Tube" | SelDise1 == "Ovarian Cancer"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Ovary","Ovarian Cancer"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Lymphoid Neoplasm"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Lymphoid","Lymphoma", "Myeloma", "Lymphoid Leukemia"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Cervix"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Cervix","Cervical"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Peritoneum"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Peritoneum","Peritoneal"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Pancreas" | SelDise1 == "Pancreatic Cancer" ){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Pancreas","Pancreatic"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Bladder/Urinary Tract" | SelDise1 == "Bladder Cancer"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Bladder","Bladder Cancer"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Soft Tissue" | SelDise1 == "Sarcoma" | SelDise1 == "DSRCT"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Soft Tissue","Sarcoma"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Liver" | SelDise1 == "Liver Cancer"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Liver","Hepatocellular carcinoma"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Head and Neck" | SelDise1 == "Head and Neck Cancer"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Head and Neck","Hypopharynx", "Larynx", "Oral", "Head and Neck Squamous"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Uterus"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Uterus","Uterine", "Mullerian", "Endometrial"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "CNS/Brain" | SelDise1 == "Brain Cancer"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("CNS/Brain","Glioma","Glioblastoma","Glioblastoma Multiforme"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Esophagus/Stomach" | SelDise1 == "Esophagus/Stomach Cancer" ){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Esophagus","Stomach", "Esophageal", "Esophagogastric", "Gastroesophageal"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
       }
      else if(SelDise1 == "Solid Tumors"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Uterus","Cervix", "Vulva", "Vagina","Brain", "Bowel","Lung","Breast","Bone", "Skin","Biliary Tract",
                                                                                                                     "Ovary","Fallopian Tube","Esophagus", "Stomach","Pancreas","Kidney","Head and Neck","Soft Tissue","Rectal","Oropharynx",
                                                                                                                     "Lymphoid", "Myeloid","Colorectal","Glioblastoma","Melanoma","Colorectal","Pancreatic","Gall bladder","Non-Small Cell Lung",
                                                                                                                     "Hepatocellular Carcinoma","Glioma","Ovarian","Gliosarcoma","Gastroesophageal","Small Cell Lung","Renal","Esophageal",
                                                                                                                     "Sarcoma", "Lymphoma","Endometrial","Mullerian","Uterine","Bile","Myelofibrosis","DSRCT","Uveal", "Thymic", 
                                                                                                                     "Prostate","Bladder","Cervical","Liver","Peritoneum", "Colon", "Thyroid","Solid Tumors"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
     else if(SelDise1 == "Prostate" | SelDise1 == "Prostate Cancer"){
      data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Prostate","Prostate Cancer"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
     }  else if(SelDise1 == "Colorectal Cancer" | SelDise1 == "Bowel"){
         data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Colon","Colorectal Cancer", "Colorectal", "Bowel","Rectal"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()                                                                                                               
      }
      else {
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",SelDise1,"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()   #works final with b
      }
      
      return(data)
    })
        checkDiseSel = reactive_data()
        
  #### new block ends  
        
    #drug not in the filter menu
   #  SelDrug = as.list.data.frame(input$drugFil)
  #   checkDrugSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% filter(drug %in% SelDrug) %>% select(NCT) %>% distinct()
    # 
    SelLineofTx = as.list.data.frame(input$lineofTxFil)
    checklineoftxSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% separate_rows(line_of_therapy,sep = ";") %>% filter(line_of_therapy %in% SelLineofTx) %>% select(NCT) %>% distinct()
   
    SelLocat = as.list.data.frame(input$locaFil)
  #  checklocat = browse_tbl %>% select(NCT,Location) %>% filter(Location %in%  SelLocat) %>% select(NCT) %>% distinct() old working
    checklocat = browse_tbl %>% select(NCT,Location) %>% filter(str_detect(Location, paste(SelLocat, collapse = "|"))) %>% select(NCT) %>% distinct()
    # for the trial type 
     SelTrialty = as.list.data.frame(input$trialTyxFil) # Ui name
 #   checktrlTy = browse_tbl %>% select(NCT,JIT) %>% filter(JIT %in%  SelTrialty) %>% select(NCT) %>% distinct() old working
    checktrlTy = browse_tbl %>% select(NCT,JIT) %>% filter(str_detect(JIT, paste(SelTrialty,collapse = "|"))) %>% select(NCT) %>% distinct()
   
   # ----------------------------------------------------------------------------------------------------------------------- #
    # part 2 options 
    # if(length(checkStageSel$NCT) >= 1  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0){
    # 
    #   # in all four options
    #   completeList = c(unique(checkStageSel$NCT))
    #   print(completeList)
    # 
    # }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) >=1 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0 ){
    # 
    #   # in all disease options
    #   completeList = c( unique(checkDiseSel$NCT))
    #   print(completeList)
    #   
    # }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) >=1 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0 ){
    # 
    #   # in all Drug options
    #   completeList =  c(unique(checkDrugSel$NCT))
    #   print(completeList)
    # 
    # 
    # }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) >= 1 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0 ){
    # 
    #   # in all line of therapy option
    #   completeList = c(unique(checklineoftxSel$NCT))
    #   print(completeList)
    # 
    # }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) >=1 && length(checktrlTy$NCT) == 0  ){
    #   
    #   # in all Location option
    #   completeList = c(unique(checklocat$NCT))
    #   
    # }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0  && length(checktrlTy$NCT) >=1  ){
    #   
    #   # in all Trial type option
    #   completeList = c(unique(checktrlTy$NCT))
    #   
    # }else{
    #   
    #   matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT), unique(checklocat$NCT), unique(checktrlTy$NCT) )
    #   ntb = as.data.frame(table(matchList))
    #   maxNb = max(ntb$Freq)
    #   ntb = ntb %>% filter(Freq >= maxNb )
    #   completeList = c(ntb$matchList)
    #   print(completeList)
    #   }


    #new code on apr 26  1st try for intersection
    
    # check_conditions <- function(dataframes_list) {
    #   completelist <- NULL
    #   for (i in 2:length(dataframes_list)) {
    #     combinations <- combn(dataframes_list, i, simplify = FALSE)
    #     for (j in 1:ncol(combinations)) {
    #       dfs <- combinations[[j]]
    #       if(!any(sapply(dfs,function(df)nrow(df)== 0)) &&
    #       (is.null(completelist) || length(completelist) == 0 ||
    #       (all(lengths(lapply(dfs, function(df) nrow(filter(df, NCT %in% completelist)))) >= 1) &&
    #           all(lengths(lapply(dfs, function(df) nrow(filter(df, NCT %in% completelist)))) == 0)))) {
    #         completelist <- unique(dfs[[1]]$NCT)
    #         break
    #       }
    #     }
    #     if (!is.null(completelist) && length(completelist) > 0 ) break
    #   }
    #   completelist
    # }
    # 
    # # Define your dataframes list
    # dataframes_list <- list(checkStageSel, checkDiseSel, checklineoftxSel, checklocat, checktrlTy)
    # 
    # # Get the common NCT values based on different combinations
    # completelist <- check_conditions(dataframes_list)
    
    # Get the intersection of NCT values from the result objects - 2nd try
    get_intersection <- function(result_objects) {
      # Extract NCT values from result objects
      
      non_null_results<- lapply(result_objects, function(result)
      {
        if(nrow(result) > 0) result$NCT else NULL
      }
        )
      
      non_null_results <- Filter(Negate(is.null),non_null_results)
      
      #original block
      if(length(non_null_results) == 0){
        return(character(0))
      
      }
     #original block
      
      #additional block tested ok with disease combinations
      # if(length(non_null_results) == 1){
      # 
      #      return(NULL) 
      # }
      #additional block tested ends
      
      
      
     # nct_lists <- lapply(nonzeroresults, function(result) result$NCT)
      #print(nct_lists)
      # Get the intersection of NCT values
    #  intersection_nct <- Reduce(intersect, nct_lists)
      intersection_nct <- Reduce(intersect, non_null_results)
      return(intersection_nct)
    }
    
    # Example usage:
    # Assuming result_objects contain the result objects checkDis, checkSt, checkTrl, checkloc, checklot
  #  result_objects <- list(checkStageSel, checkDiseSel, checklineoftxSel, checklocat, checktrlTy)   >>>>>>> old correct
    
    result_objects <- list(checkStageSel, checkDiseSel, checklineoftxSel, checklocat, checktrlTy)
    
    # Get the intersection of NCT values
    intersection_nct <- get_intersection(result_objects)
    
    # Print the intersection NCT values
    print(intersection_nct)
    
    completeList = intersection_nct
    
    # ----------------------------------------------------------------------------------------------------------------------- #
   
    
    
    
    filTb = browse_tbl %>% filter(NCT %in% completeList )
   
    ###checking to combine columns april 25
    #filTb <- filTb %>% mutate(comb_col = html(paste(Documentation, "<br>", Link)))
    ######## april 25
    
    
    
    #adding to have trials open on page loading - july 5th #################### first working
     #  filt_data_initial_filtered_sel<- reactive({
     #   if(input$show_closed){
     #     #filTb[filTb$HoldStatus == "open",input$selcolumns]
     #     filter(names(filTb) %in% input$selcolumns)
     #   }
     #   else{
     #     filTb %>% dplyr::select(Link, Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers, Documentation) %>% filter(HoldStatus == "open")
     #   }
     # })
    
    
    
    

    ###original working <<<july 5th second working but not completed
    # filt_data_initial_filtered<- reactive({
    #   if(input$show_closed){
    #    filTb[,input$selcolumns]  
    #     
    #   }
    #   else{
    #    filTb[filTb$HoldStatus == "open",input$selcolumns] 
    #     
    #   }
    # })
    ##original working <<< july 5
    
    
    
    
    
    
    ###original working - third working
    #checks if first closed trials to display or it will be open trials on page loading
    #next checks if any columns selected otherwise displays a set of eight default columns
    filt_data_initial_filtered<- reactive({
      if(input$show_closed){
        if(length(input$selcolumns > 0)){
      #    filTb[filTb$HoldStatus != "open",input$selcolumns] commented oct'25th
          filTb[filTb$HoldStatus == "closed",input$selcolumns]
        }
        else{
        #  filTb[filTb$HoldStatus != "open",] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers) 
         #commented oct' 25 filTb[filTb$HoldStatus != "open",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Diseasecat, Conditions, stages, disp_biomarkers)
         # filTb[filTb$HoldStatus == "closed",] %>% dplyr::select(Protocol, PrincipalInvestigator, HoldStatus, Phase, Title, Diseasecat, Conditions, stages, disp_biomarkers) #previous display order
          
          #changing display order
       #   filTb[filTb$HoldStatus == "closed",] %>% dplyr::select(Protocol, Title, Phase, stages, Conditions,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
          #changing display columns
          filTb[filTb$HoldStatus == "closed",] %>% dplyr::select(Protocol, Title, Phase, stages, lnOfTherapy , disp_disease1, HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
          }
          
        
      }
      else{
        if(length(input$selcolumns > 0)){
        #  filTb[filTb$HoldStatus == "open",input$selcolumns] commented oct' 25th
          filTb[filTb$HoldStatus != "closed",input$selcolumns]
        }
        else{
       #   filTb[filTb$HoldStatus == "open",] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers) 
      # commented oct 25th..   filTb[filTb$HoldStatus == "open",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Diseasecat, Conditions, stages, disp_biomarkers)
       #   filTb[filTb$HoldStatus != "closed",] %>% dplyr::select(Protocol, PrincipalInvestigator, HoldStatus, Phase, Title, Diseasecat, Conditions, stages, disp_biomarkers) #previous display order
          
          #changing display order
         # filTb[filTb$HoldStatus != "closed",] %>% dplyr::select(Protocol, Title, Phase, stages, Conditions,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
          #changing display columns
          filTb[filTb$HoldStatus != "closed",] %>% dplyr::select(Protocol, Title, Phase, stages, lnOfTherapy , disp_disease1, HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
          } 
        
      }
    })
    ##original
    
    #this is for expandable rows displaying the correct rows for open and close trials
    expandable_data_filt <- reactive({
      if (input$show_closed) {
        #filTb[!filTb$HoldStatus %in% "open", ] commented oct' 25th
        filTb[filTb$HoldStatus %in% "closed", ]
      } else {
      #  filTb[filTb$HoldStatus == "open", ] commented oct' 25th
        filTb[!filTb$HoldStatus == "closed", ]
      }
    })
    
    
   output$filterbrowse <- renderReactable({
   #reactable(filTb %>% dplyr::select(Link, Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers, Documentation),
        #     reactable(filTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_disease1, disp_biomarkers,  Documentation), 
                  #     reactable(filTb %>% dplyr::select(Protocol, HoldStatus,filtopencohort, Phase, Title, Disease, lnOfTherapy,disp_biomarkers), 
                                 
     
                         #displaying conditions column instead of Disease column  <<< original july 5 following line
    # reactable(filTb %>% dplyr::select(Protocol, HoldStatus,filtopencohort, Phase, Title, Conditions, lnOfTherapy,disp_biomarkers), <<<< final decided original
   
     dataListFilter <- function(tableId, style = "width: 100%; height: 28px;") {
       function(values, name) {
         dataListId <- sprintf("%s-%s-list", tableId, name)
         tagList(
           tags$input(
             type = "text",
             list = dataListId,
             oninput = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
             "aria-label" = sprintf("Filter %s", name),
             style = style
           ),
           tags$datalist(
             id = dataListId,
             lapply(unique(values), function(value) tags$option(value = value))
           )
         )
       }
     }
     
     
     
     reactable(filt_data_initial_filtered() ,
     
     #july 5th commenting - following is orginal
       #reactable(filTb[, input$selcolumns],   #### this is using selective display columns
     #july5th commenting
     
            #          filterable = TRUE,
             searchable = TRUE,
             resizable = TRUE,
             fullWidth = TRUE,
             defaultColDef = colDef(align = "center"),
             striped = TRUE,
             showSortable = TRUE,
             style = list(minWidth = 800),
             #columns = list(Trial = colDef(html = TRUE)),
           #  columns = list(Link = colDef(html = TRUE,name = "Trial"), HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy"), Disease = colDef(name = "Conditions/Disease"),
                    #        disp_biomarkers = colDef(name = "Biomarker"), Documentation = colDef(html=TRUE)),
           #change Current Status to Study Status; Rename filtopencohort to Enrollment Status
      
            
     #columns = list( HoldStatus = colDef(name = "Study Status"), filtopencohort = colDef(name = "Enrollment Status"), lnOfTherapy = colDef(name = "Line of Therapy"), Conditions = colDef(name = "Conditions/Disease"),
                     columns = list( HoldStatus = colDef(name = "Study Status"), 
                                 #    Conditions = colDef(name = "Conditions/Disease",filterInput = dataListFilter("conditions-list")),    
                                
                                  Diseasecat = colDef(name = "Disease Category"
                                                     #        filterInput = dataListFilter("disease-list")
                                 ),
                                 lnOfTherapy = colDef(name = "Line of Therapy"),
                                 disp_disease1 = colDef(name = "Cancer Type"),
                                 PrincipalInvestigator = colDef(name = "Principal Investigator"),
                                 
                                  Conditions = colDef(
                                #       filterInput = dataListFilter("conditions-list")), #this kind of id and element-id will not work if you are using renderreactable so use the outputid as below
                                     
                                filterInput = dataListFilter("filterbrowse")),
                                
                                      #     columns = list( HoldStatus = colDef(name = "Study Status"), lnOfTherapy = colDef(name = "Line of Therapy"), Disease = colDef(name = "Conditions/Disease")),
                 #           disp_biomarkers = colDef(name = "Biomarker"), disp_disease1 = colDef(name = "Cancer Type"), Documentation = colDef(html=TRUE), 
                            
                 stages = colDef(name = "Disease Stage"
                                 # filterable = TRUE,
                                 # filterInput = function(values, name) {
                                 #   tags$select(
                                 #     # Set to undefined to clear the filter
                                 #     onchange = sprintf("Reactable.setFilter('stage-select', '%s', event.target.value || undefined)", name),
                                 #     # "All" has an empty value to clear the filter, and is the default option
                                 #     tags$option(value = "", "All"),
                                 #     lapply(unique(values), tags$option),
                                 #     "aria-label" = sprintf("Filter %s", name),
                                 #     style = "width: 100%; height: 28px;"
                                 #   )
                                 # }
                 ),
                 
                 
                 
                 
                 disp_biomarkers = colDef(name = "Biomarker"),
                 
                
                 
                 
                 
                 
                 
                            Title = colDef(name = "Title", minWidth = 300 ,style = list(fontWeight = "bold"))
         #        , elementId = "conditions-list"      #this is not needed if you are using renderreactable and will be used if you are just using reactable without renderreactable
             #    elementId = "stage-select"  #this can be used for selectinput for any column; we have tested for stage here
                            ),
             details = function(index) { 
               
               # create table for cohort level information
               
               
               # create tables to be displayed if nested rows are expanded
               htmltools::div(
                
                 # group1: general info
                # reactable(filTb[index, ] %>% select(Link,Documentation, Name,Sponsor,StudyType, Location, TrialLastUpdate),  <<< original working without select columns and open and closed trials
                           
                            #following is for selecting trials open and closed that is similar with the main display columns     
                           reactable(expandable_data_filt()[index, ] %>% select(Link,Documentation, Name,Sponsor,StudyType, Location, TrialLastUpdate),          
                           
                           defaultColDef = colDef(align = "center"),
                           columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial"),Documentation = colDef(html=TRUE))
                 ),
                 
                 # group 3: summary
                # reactable(filTb[index, ] %>%   <<< original working for without option for selection for open and closed trials
                
                
                #following is for selecting trials open and closed that is similar with the main display columns  
                             reactable(expandable_data_filt()[index, ] %>%
                             select(Summary)),
                 
                 
                 # group 4: trial Status from .gov
                # reactable(filTb[index, ] %>%
                             
                             
                #following is for selecting trials open and closed that is similar with the main display columns
                             reactable(expandable_data_filt()[index, ] %>%
                             select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                           defaultColDef = colDef(align = "center"),
                           columns = list(Status = colDef(name = "Clinical.gov Status"),
                                          MinAge = colDef(name = "Minimum Age"),
                                          StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                          LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                 # group2: cohort info
                 
                 # reactable(browse_tbl[index, ]$arms$arm %>%
                
                
                # reactable(filTb[index, ]$arms[[1]] %>% unnest(biomarker) %>%  <<< original working without selection for open and closed trials
                             
                #following is for selecting trials open and closed that is similar with the main display columns            
                             reactable(expandable_data_filt()[index, ]$arms[[1]] %>% unnest(biomarker) %>%            
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
                # reactable(filTb[index, ] %>%  # original working without selection for open and closed trials
                            # select(Conditions)), <<<< delete this
                             
                             
                             reactable(expandable_data_filt()[index, ] %>%
                             select(Disease)),
                 
                 # group 3: disease information
                 
                 #  reactable(browse_tbl[index, ]$disp_disease$disp_disease), <<<< delete this
                
                
               #  reactable(filTb[index, ]$disp_disease[[1]] %>% select(code, selection,stage)) # original working without selection for open and closed trials
                reactable(expandable_data_filt()[index, ]$disp_disease[[1]] %>% select(code, selection,stage)) 
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
   
   #reset trial type 
   updateSelectInput(inputId = "trialTyxFil",selected = "")
   updateSelectInput(inputId = "selcolumns",selected = "")
   
   
   updateRadioButtons(inputId = "filtercond", selected = character(0))
   
   #adding update checkbox input when reset for show closed trials
   updateCheckboxInput(inputId = "show_closed", value = FALSE)
 })

  ##### BROWSE ########b
  # main display table for BROWSE
 
#browse_tbl <- browse_tbl %>% mutate(comb_col = paste(Documentation,Link, sep="\n"))
 
 
 
 #####introducing open trials only on page loading for non-filtered browse data
 
 
 selecTrial$comTb = as_tibble(browse_tbl)
 
 
 
 ############################ trials open on page loading
 
 #adding to have trials open on page loading - july 5th #################### original working but with warning error
 # browse_data_initial_filtered<- reactive({
 #   if(input$show_closed){
 #    selecTrial$comTb[,input$selcolumns]  ##original commented on july 12th
 #   #  selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers)
 #   }
 #   else{
 #     
 #     
 #     
 #     selecTrial$comTb[selecTrial$comTb$HoldStatus == "open",input$selcolumns] #commented on july 12th
 #  #  selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers) %>% filter(HoldStatus == "open")
 #   }
 # })
 
 
 
 ############################ trials open on page loading --- original working ends here
 
 
 #adding to have trials open on page loading - july 17
 #checks if first closed trials to display or it will be open trials on page loading
 #next checks if any columns selected otherwise displays a set of eight default columns
 
 
 
 browse_data_initial_filtered<- reactive({
   if(input$show_closed){
     if(length(input$selcolumns) > 0){
   #  selecTrial$comTb[selecTrial$comTb$HoldStatus!="open",input$selcolumns] commented oct 25th
       selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",input$selcolumns]
     }
     else{
    #   selecTrial$comTb[selecTrial$comTb$HoldStatus!="open",] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Diseasecat, Phase, Title, Conditions, stages, lnOfTherapy, disp_biomarkers)
    # commented oct 25th   selecTrial$comTb[selecTrial$comTb$HoldStatus!="open",] %>% dplyr::select(Protocol, HoldStatus, Diseasecat, Phase, Title, Conditions, stages, disp_biomarkers)
    #   selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",] %>% dplyr::select(Protocol, PrincipalInvestigator, HoldStatus, Diseasecat, Phase, Title, Conditions, stages, disp_biomarkers) #previous display order
       
       #changing display order
     #  selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",] %>% dplyr::select(Protocol, Title, Phase, stages, Conditions,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
       #changing display columns
       selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",] %>% dplyr::select(Protocol, Title, Phase, stages, lnOfTherapy, disp_disease1,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
       
       }
   } # if closing for show_closed
   else
     { 
       if(length(input$selcolumns) > 0){
    #   selecTrial$comTb[selecTrial$comTb$HoldStatus=="open",input$selcolumns] commented oct25th
         selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",input$selcolumns]
     }
       else {
       
   #    selecTrial$comTb[selecTrial$comTb$HoldStatus=="open",] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Diseasecat, Phase, Title, Conditions, stages, lnOfTherapy, disp_biomarkers)
     #    selecTrial$comTb[selecTrial$comTb$HoldStatus=="open",] %>% dplyr::select(Protocol, HoldStatus, Diseasecat, Phase, Title, Conditions, stages, disp_biomarkers)
       #  selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",] %>% dplyr::select(Protocol, PrincipalInvestigator, HoldStatus, Diseasecat, Phase, Title, Conditions, stages, disp_biomarkers) #previous display order
         
    # changing display order
        # selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",] %>% dplyr::select(Protocol, Title, Phase, stages, Conditions,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
        #changing display columns
         selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",] %>% dplyr::select(Protocol, Title, Phase, stages,  lnOfTherapy, disp_disease1, HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
         }
      
     
   }# else - for this - closing 
   
 }) #reactive
 
 #this is for expandable rows displaying the correct rows for open and close trials
 expandable_data <- reactive({
   if (input$show_closed) {
   #  selecTrial$comTb[!selecTrial$comTb$HoldStatus %in% "open", ] commented oct 25th
     selecTrial$comTb[selecTrial$comTb$HoldStatus %in% "closed", ]
   } else {
   #  selecTrial$comTb[selecTrial$comTb$HoldStatus == "open", ] commented oct 25th
     selecTrial$comTb[!selecTrial$comTb$HoldStatus == "closed", ]
   }
 })
 
 
 
 
 
 
 
 ####july 14th
 # browse_data_initial_filtered<- reactive({
 #   if(input$show_closed){
 #     selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers) ##original commented on july 12th
 # 
 # 
 #   }
 #   else{
 #     selecTrial$comTb[selecTrial$comTb$HoldStatus == "open", ] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers)#commented on july 12th
 # 
 #   }
 # })
 ##july 14th
 
 
 
 
 # #adding autocomplete for Disease category  ----- Sep' 20 2023
 # dataListFilter <- function(tableId, style = "width: 100%; height: 28px;") {
 #   function(values, name) {
 #     dataListId <- sprintf("%s-%s-list", tableId, name)
 #     tagList(
 #       tags$input(
 #         type = "text",
 #         list = dataListId,
 #         oninput = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
 #         "aria-label" = sprintf("Filter %s", name),
 #         style = style
 #       ),
 #       tags$datalist(
 #         id = dataListId,
 #         lapply(unique(values), function(value) tags$option(value = value))
 #       )
 #     )
 #   }
 # }
 # 
 # 
 # 
 # 
 # 
 # # autocomplete ends
 
 
 
 
 
  output$browsetable <- renderReactable({
  
    #adding autocomplete for Disease category  ----- Sep' 20 2023
    dataListFilter <- function(tableId, style = "width: 100%; height: 28px;") {
      function(values, name) {
        dataListId <- sprintf("%s-%s-list", tableId, name)
        tagList(
          tags$input(
            type = "text",
            list = dataListId,
            oninput = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
            "aria-label" = sprintf("Filter %s", name),
            style = style
          ),
          tags$datalist(
            id = dataListId,
            lapply(unique(values), function(value) tags$option(value = value))
          )
        )
      }
    }
    
    
    
    
    
    # autocomplete ends
    
    
    
   #  selecTrial$comTb = as_tibble(browse_tbl)  <<<< original commented july 6 ==== need this if you don't need checkbox for closed rows
    
    
    
    
    
     
  #selecTrial$comTb %>% select(arm) %>% unnest(arm) %>% select(line_of_therapy)
     
   # selecTrial$comTb <- selecTrial$comTb %>% mutate(comb_col = htmltools::HTML(paste(Documentation,"<br>",Link)))
     
     
   #  selecTrial$comTb <- selecTrial$comTb %>% mutate(comb_col = glue("<a href='{link}'>{Documentation}</a>"))
     
   #  reactable::reactable( selecTrial$comTb %>% dplyr::select(Link, Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers, Documentation),
                    #       reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_disease1, disp_biomarkers, Documentation),  
                                          #       reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Disease, lnOfTherapy, disp_biomarkers),
     
                                                                 #displaying conditions instead of Disease which is a summary      
                                                                      # reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Disease, lnOfTherapy, disp_biomarkers),
                                                                                             
                             ##original original                       ## i am using sel columns in next line so commented below                                      
     # 1 reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers), commented - final decided original
                                                                                             
                                      
    #2if you dont need check box then you need line below########################################                                      
                                                    #reactable::reactable( selecTrial$comTb[, input$selcolumns], #original - this is only to select columns -- working
                                                ################################################                          
                                                    reactable::reactable(browse_data_initial_filtered(),
                                               #      defaultSelected = "Title",                    
                        #                         searchable = TRUE,
                                                 searchable = TRUE,
                        
                        #no filter for each column
                                         #        filterable = TRUE,
                                                 #       columnDefs = list(list(targets = 4, width = 800)),
                                                 
                                                 resizable = TRUE,
                                                 fullWidth = TRUE,
                        
                        
                                               defaultColDef = colDef(align = "center"),
                        
                        # #have autocomplete for all columns instead of one column  - not working
                        # defaultColDef = colDef(
                        #   filterInput = function(values, name) {
                        #     if (is.factor(values)) {
                        #       dataListFilter("allcol-list")(values, name)
                        #     }
                        #   }
                        # ),
                        # 
           #              elementId = "allcol-list",
           
           
           
           
                                                 striped = TRUE,
                                                 showSortable = TRUE,
         
               # style = list(minWidth = 800), <<< original
             #  style = list(fontWeight = "bold")  <<< this is for bold all columns working,
         
                #columns = list(Trial = colDef(html = TRUE)),
         
             
         #old       #columns = list(Link = colDef(html = TRUE,name = "Trial"), HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy") ,Disease = colDef(name = "Conditions/Disease"),
             #oldworking    #              disp_biomarkers = colDef(name = "Biomarker"), Documentation = colDef(html=TRUE)),
       
         
          # columns = list(colDef("Title", header = "Title", formatter = function(value){HTML(paste0("<b>", value, "</b>"))})),
         #change Current Status to Study Status
         
         
        #Removed filtopencohort which is enrollment status and lnoftherapy from display 
  #       columns = list(HoldStatus = colDef(name = "Study Status"), filtopencohort = colDef(name = "Enrollment Status"), lnOfTherapy = colDef(name = "Line of Therapy") ,Conditions = colDef(name = "Conditions/Disease"),
                        
                    #    columns = list(HoldStatus = colDef(name = "Study Status"), Conditions = colDef(name = "Conditions/Disease"),
                                       
                                       columns = list(HoldStatus = colDef(name = "Study Status"), 
                                                      
                                                      #Conditions = colDef(name = "Conditions",filterInput = dataListFilter("browsetable")),
        #########                               
                        
        
        
        #adding autocomplete for disease category
        Diseasecat = colDef(name = "Disease Category"
                            #  filterInput = dataListFilter("diseasecat-list") ---> this does not work because you are using renderreactable so you need to use outputid here which is browsetable
                            
                            #    filterInput = dataListFilter("browsetable")  # this is for keeping this column with autocomplete feature - working
                            
        ),
        #     elementId = "cars-list",  #new 
        #autocomplete ends
        lnOfTherapy = colDef(name = "Line of Therapy"),
        disp_disease1 = colDef(name = "Cancer Type"),
        PrincipalInvestigator = colDef(name = "Principal Investigator"),
        
        
        Conditions = colDef(name = "Conditions",filterInput = dataListFilter("browsetable")),
        
        
        #adding stage with selectinput filter
        
        stages = colDef(name = "Disease Stage"
                        
                        #following is for using selectinput - for stage-select 
                        # filterable = TRUE,
                        # filterInput = function(values, name) {
                        #   tags$select(
                        #     # Set to undefined to clear the filter
                        #     onchange = sprintf("Reactable.setFilter('stage-selection', '%s', event.target.value || undefined)", name),
                        #     # "All" has an empty value to clear the filter, and is the default option
                        #     tags$option(value = "", "All"),
                        #     lapply(unique(values), tags$option),
                        #     "aria-label" = sprintf("Filter %s", name),
                        #     style = "width: 100%; height: 28px;"
                        #   )
                        # }
        ),
        
        
        
        
                        
                          #     disp_biomarkers = colDef(name = "Biomarker"), disp_disease1 = colDef(name = "Cancer Type"), Documentation = colDef(html=TRUE), 
                        
                        disp_biomarkers = colDef(name = "Biomarker"),
                        
                       
        
        
        
                        
                       
                        Title = colDef(name = "Title", minWidth = 300 ,style = list(fontWeight = "bold"))
        
        
        
        
        
        
                    #   , elementId = "allcol-list"   
     #   , elementId = "diseasecat-list"
        
        
     #   ,elementId = "stage-selection",
                        #adding stage with selectinput filter ends
                        
                        
                        
                        
           #original before stage added           Title = colDef(name = "Title", minWidth = 300 ,style = list(fontWeight = "bold"))
                      
                      
                      
                      
                      
                      
              
                      #        Title = colDef(name = "Title", minWidth = 300)  
                   #   Title = colDef(Title = list(fontWeight = "bold"))
                      
                      ),
         
         
         #for stage selectinput
 #         elementId = "stage-select",
         #for stage selectinput
         
         
       #company = colDef(name = "Company", minWidth = 100),
                 
        # style = list(".rt-td:nth-child(6)" = list("width" = "1500px"), ".rt-table" = list("width" = "10000px"),
        #              
        #              ".rt-th:nth-child(6)" = list("font-weight" = "bold")),
       
          
      # columnProps = list(Title = colDef(style = "font-weight:bold; min-width: 1500px")),
             
                details = function(index) {
                  # create tables to be displayed if nested rows are expanded
                  htmltools::div(
                   
                    # group1: general info
                  #  reactable(selecTrial$comTb[index, ] %>% select(Link, Documentation,Name,Sponsor,StudyType, Location, TrialLastUpdate), #commented original 
                    
                    #following is added for expandable rows to reflect the trials that are open and closed correctly - july 20th
                    reactable(expandable_data()[index, ] %>% select(Link, Documentation,Name,Sponsor,StudyType, Location, TrialLastUpdate),
                              defaultColDef = colDef(align = "center"),
                              columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial"), Documentation = colDef(html=TRUE))
                    ),
                    
                    # group 3: summary
                   # reactable(selecTrial$comTb[index, ] %>%  <<< commented original
                                
                                #following for expandable rows to reflect open and closed trials correctly
                                reactable(expandable_data()[index, ] %>%
                                select(Summary)),
                    
                    
                    # group 4: trial Status from .gov
                   # reactable(selecTrial$comTb[index, ] %>%   <<<< commented original
                              
                                
                                reactable(expandable_data()[index, ] %>%
                                  select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                              defaultColDef = colDef(align = "center"),
                              columns = list(Status = colDef(name = "Clinical.gov Status"),
                                             MinAge = colDef(name = "Minimum Age"),
                                             StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                             LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                    # group2: cohort info
                    
                    # reactable(browse_tbl[index, ]$arms$arm %>%
                   # reactable(selecTrial$comTb[index, ]$arms[[1]] %>% unnest(biomarker) %>%     <<<< commented original
                                
                                reactable(expandable_data()[index, ]$arms[[1]] %>% unnest(biomarker) %>%
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
                  #  reactable(selecTrial$comTb[index, ] %>%
                              #  select(Conditions)),  <<< changed from Conditions to disease is original is correct 
                                
                                
                                reactable(expandable_data()[index, ] %>%
                                 select(Disease)),
                    # group 3: disease information
                    
                    #  reactable(browse_tbl[index, ]$disp_disease$disp_disease),
                   # reactable(selecTrial$comTb[index, ]$disp_disease[[1]] %>% select(code, selection,stage)) <<< original
                  
                  reactable(expandable_data()[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
                    
                    
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

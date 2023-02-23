#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reactable)
library(reactablefmtr)
library(bslib)
library(readxl)
library(here)
library(mongolite)
library(jsonlite)
library(httr)
library(glue)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    
    ), 
    
    useShinyjs(),
    navbarPage(
      title = span("TrialMatch",style ="color:black; font-size: 25px" ) ,
      
     
      tabPanel("Browse",
               wellPanel(fluidRow(
                 
                 column(4,
                 
                 
                 selectInput(
                   inputId = "stageView",
                   label = "Disease Stages",
                   choices = c(stageAv$stage),
                   #choices = c("Stage I","Stage II","Stage III","Stage IV","Methylated","Un-resectable","resectable",
                   #            "Unmethylated","Advanced Stage","Recurrent","Metastatic","Early stage", "New diagnosis","Relapsed/Refractory","Post Cellular Therapy",
                    #           "Smoldering Myeloma"),
                   multiple = T,
                   #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
                   width = "400px"
                   
                  )),
                 # column(4,
                 #        
                 #        #  style = "display: inline-block;",
                 #        #style = "margin-top: 10px;",
                 #        ),
                 # column(2,
                 #        #style = "display: inline-block;",
                 #        # style = "margin-top: 10px;",
                 #        actionButton("filter_stage", " ",icon = shiny::icon("filter"),size = "sm")
                 # 
                 # ),
                 column(4,
                        
                        # style = "display: inline-block;",
                        #style = "margin-top: 15px;",
                        selectInput(
                          inputId = "disFil",
                          label = "Cancer Type",
                          choices = c(diseasAv$code),
                          multiple = T,
                          #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
                          width = "400px"

                        )),
                 # column(2,
                 #        #style = "display: inline-block;",
                 #        # style = "margin-top: 15px;",
                 #        actionButton("dise_fil", " ",icon = shiny::icon("filter"),size = "sm")
                 # 
                 # ),
                 column(4,
                          
                        # style = "display: inline-block;",
                        #style = "margin-top: 15px;",
                        selectInput(
                          inputId = "drugFil",
                          label = "Drugs Options",
                          choices = c(drugAv$drug),
                          multiple = T,
                          #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
                          width = "400px"
                          
                        )),
                 # column(2,
                 #        #style = "display: inline-block;",
                 #        # style = "margin-top: 15px;",
                 #        actionButton("drug_fil", " ",icon = shiny::icon("filter"),size = "sm")
                 #        
                 # ),
                 
                 
                #added line of therapy
                 column(4,
                        
                        # style = "display: inline-block;",
                        #style = "margin-top: 15px;",
                        selectInput(
                          inputId = "lineofTxFil",
                          label = "Line of therapy",
                          choices = c(lineoftxAv$line_of_therapy),
                         #choices = c("1","2","3","1 2", "1 2 3", "2+"),
                          multiple = T,
                          #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
                          width = "400px"
                          
                        )),
                 
                 column(4,
                        
                        # style = "display: inline-block;",
                        #style = "margin-top: 15px;",
                        selectInput(
                          inputId = "locaFil",
                          label = "Locations",
                          choices = c(locAv$Location),
                          multiple = T,
                          #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
                          width = "400px"
                          
                        )),
                 
                column(4,
                       
                       # style = "display: inline-block;",
                       #style = "margin-top: 15px;",
                       radioButtons(
                         inputId = "filtercond",
                         label = "Please select condition for criteria (And/Or) ",
                         choices = c("and", "or"), selected = character(0),
                         # multiple = F,
                         #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
                         width = "400px"
                         
                       )),
                
                 column(4 ),
                 column(4,
                        #style = "display: inline-block;",
                        # style = "margin-top: 15px;",
                        actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "lg",class = "btn-warning")
                        
                 )

               )),
               br(),
               br(),
              
                 fluidRow(
                   column(6,
                          actionButton("reset_btn_browse", "Reset Trials",class = "btn-success")),
                   column(6, 
                          actionButton("collapse_btn_browse", "Collapse All",class = "btn-info") )
                 ),
               
               br(),
               reactableOutput("filterbrowse"),
               br(),
               reactableOutput("browsetable"),
              
               
             #  input_fil,
             #   input_fst,
                 
      
      # MATCH
      # tabPanel("Match",
      #          reactableOutput("matchtable")),
      
     theme = bs_theme(version = 5,
                      bootswatch = "cosmo",
                      primary = "#246725")
      
    ) # tabpanel close
 ) # navbar close
 ) # taglist close
)
)

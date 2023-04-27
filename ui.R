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
library(shinydashboard)



ui <- dashboardPage(
  dashboardHeader(title = "TrialMatch",titleWidth = 350),
  dashboardSidebar(
    
    tags$style(HTML("
      .main-sidebarmenu{
        width: 350px;
      }
    ")),
    
    
    sidebarMenu(
      menuItem("Browse", tabName = "browse", icon = icon("search")),

      ####
      useShinyjs(),
      tabItems(
        tabItem(tabName = "browse",
 #               fluidRow(
  #                column(12,
   #                      wellPanel(
                        #   fluidRow(
                             
                             # column(
                             #   4,

                               selectInput(
                                 inputId = "stageView",
                                 label = "Disease Stages",
                                 choices = c(stageAv$stage),
                                 multiple = TRUE,
                                 width = "100%"
                               ), #selectinput
                          #   ), #column
                             
                           #  column(4,
                                    selectInput(
                                      inputId = "disFil",
                                      label = "Cancer Type",
                                      choices = c(diseasAv$code),
                                      multiple = TRUE,
                                      width = "100%"
                                    ), #selectinput
                         #    ), #column
                          #   column(4,


                                    selectInput(
                                      inputId = "lineofTxFil",
                                      label = "Line of therapy",


                                      choices = c(lineoftxAv$line_of_therapy),



                                      multiple = T,

                                      width = "100%"

                                    )
                                #    )
                                    ,

                            # column(4,


                                    selectInput(
                                      inputId = "locaFil",
                                      label = "Locations",
                                      choices = c(locAv$Location),
                                      multiple = T,

                                      width = "100%"

                                    )
                                    #)
                                    ,
#following is for selective display
                         #    column(4,


                                    # selectInput(
                                    #   inputId = "selcolumns",
                                    #   label = "Column selection",
                                    # 
                                    #   choices = c(colnames(seldiscolumns),
                                    #               selected = "Title"),
                                    # 
                                    # 
                                    # 
                                    #   multiple = T,
                                    # 
                                    #   width = "100%"
                                    # 
                                    # )
                           #         )
                                    #,










                         #    column(4,

                                    actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "sm",class = "btn-warning",width="50%"),

                           #  ) #column

                             # )
       #                    ), #fluidrow,
                           br(),
                           br(),

                          # fluidRow(
                           #  column(4,
                                    actionButton("reset_btn_browse", "Reset Trials",class = "btn-success", width = "50%"),
# ),
                            # column(4,
                                    actionButton("collapse_btn_browse", "Collapse All",class = "btn-info", width = "50%") ,
# )
  #                         ),
                           #, #fluidrow


                           # br(),
                           #        reactableOutput("filterbrowse"),
                           #       br(),
                           #       reactableOutput("browsetable"),
                           # #       
                           # #       
                           # #       
                           #        theme = bs_theme(version = 5,
                           #                         bootswatch = "cosmo",
                           #                         primary = "#246725")









#     )#wellpanel
#     ),#column
      ####

      collapsible = TRUE
#    )#fluidrow
    )#tabitems
    )#tabitem,
    )#sidebar menu
  ),#sidebar
    dashboardBody(
      useShinyjs(),
      tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content")) ),
      
      br(),
              reactableOutput("filterbrowse"),
             br(),
             reactableOutput("browsetable"),
      # #       
      # #       
      # #       
              theme = bs_theme(version = 5,
                              bootswatch = "cosmo",
                               primary = "#246725")

  )
  
   
)#dashboard page
  
  
  
  
  
  
  
  
  
  
  
#   
#   #dashboardBody(
#     # useShinyjs(),
#     # tabItems(
#     #   tabItem(tabName = "browse",
#     #           fluidRow(
#     #             column(12,
#     #                    wellPanel(
#     #                      fluidRow(
#     #                        column(
#     #                          6,
#     #                          
#     #                    selectInput(
#     #                      inputId = "stageView",
#     #                      label = "Disease Stages",
#     #                      choices = c(stageAv$stage),
#     #                      multiple = TRUE,
#     #                      width = "100%"
#     #                    ) #selectinput
#     #             ), #column
#     #             column(6,
#     #                    selectInput(
#     #                      inputId = "disFil",
#     #                      label = "Cancer Type",
#     #                      choices = c(diseasAv$code),
#     #                      multiple = TRUE,
#     #                      width = "100%"
#     #                    ) #selectinput
#     #             ), #column
#     #             column(6,
#     #                    
#     #                    
#     #                    selectInput(
#     #                      inputId = "lineofTxFil",
#     #                      label = "Line of therapy",
#     #                     
#     #                      
#     #                      choices = c(lineoftxAv$line_of_therapy),
#     #                      
#     #                      
#     #                      
#     #                      multiple = T,
#     #                      
#     #                      width = "100%"
#     #                      
#     #                    )),
#     #             
#     #             column(6,
#     #                    
#     #                    
#     #                    selectInput(
#     #                      inputId = "locaFil",
#     #                      label = "Locations",
#     #                      choices = c(locAv$Location),
#     #                      multiple = T,
#     #                      
#     #                      width = "100%"
#     #                      
#     #                    )),
#     #             
#     #             column(4,
#     #                    
#     #                    
#     #                    selectInput(
#     #                      inputId = "selcolumns",
#     #                      label = "Column selection",
#     #                      
#     #                      choices = c(colnames(seldiscolumns),
#     #                                  selected = "Title"),
#     #                      
#     #                      
#     #                      
#     #                      multiple = T,
#     #                      
#     #                      width = "100%"
#     #                      
#     #                    )),
#     #             
#     #             
#     #             
#     #             
#     #             
#     #             
#     #             
#     #             
#     #             
#     #             
#     #             column(4,
#     #                    
#     #                    actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "lg",class = "btn-warning", width = "100%")
#     #                    
#     #             )
#     #             
#     #          # )
#               ),
#     #   br(),
#     #   br(),
#     #   
#     #   fluidRow(
#     #     column(6,
#     #            actionButton("reset_btn_browse", "Reset Trials",class = "btn-success", width = "100%")),
#     #     column(6, 
#     #            actionButton("collapse_btn_browse", "Collapse All",class = "btn-info", width = "100%") )
#     #   ),
#       
#       br(),
#       reactableOutput("filterbrowse"),
#       br(),
#       reactableOutput("browsetable"),
#       
#       
#       
#       theme = bs_theme(version = 5,
#                        bootswatch = "cosmo",
#                        primary = "#246725")
#       
# #     ) # tabpanel close
# #   ) # navbar close
# # ) # taglist close
# # )
#  )
#                 
#                 
#                 
#                 
#                 
#                 
#                 
#              # ) #fluid row
#       )#tab item
#     ) #tabitems
#   )#dash body
# #) #dashboard page



































#old

#Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   tagList(
#     tags$head(
#       tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
# 
#     ),
# 
#     useShinyjs(),
#     navbarPage(
#       title = span("TrialMatch",style ="color:black; font-size: 25px" ) ,
# 
# 
#       tabPanel("Browse",
#                wellPanel(fluidRow(
# 
#                  column(4,
# 
# 
#                  selectInput(
#                    inputId = "stageView",
#                    label = "Disease Stages",
#                    choices = c(stageAv$stage),
#                    #choices = c("Stage I","Stage II","Stage III","Stage IV","Methylated","Un-resectable","resectable",
#                    #            "Unmethylated","Advanced Stage","Recurrent","Metastatic","Early stage", "New diagnosis","Relapsed/Refractory","Post Cellular Therapy",
#                     #           "Smoldering Myeloma"),
#                    multiple = T,
#                    #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
#                    width = "400px"
# 
#                   )),
# 
#                  column(4,
# 
#                         # style = "display: inline-block;",
#                         #style = "margin-top: 15px;",
#                         selectInput(
#                           inputId = "disFil",
#                           label = "Cancer Type",
#                           choices = c(diseasAv$code),
#                           multiple = T,
#                           #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
#                           width = "400px"
# 
#                         )),
# 
#                  column(4,
# 
#                         # style = "display: inline-block;",
#                         #style = "margin-top: 15px;",
#                         selectInput(
#                           inputId = "drugFil",
#                           label = "Drugs Options",
#                           choices = c(drugAv$drug),
#                           multiple = T,
#                           #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
#                           width = "400px"
# 
#                         )),
# 
# 
#                 #added line of therapy
#                  column(4,
# 
#                         # style = "display: inline-block;",
#                         #style = "margin-top: 15px;",
#                         selectInput(
#                           inputId = "lineofTxFil",
#                           label = "Line of therapy",
#                           #choices = c(lineoftxAv$line_of_therapy),
#                          #choices = c("1","2","3","1 2", "1 2 3", "2+"),
# 
#                          choices = c(lineoftxAv$line_of_therapy),
# 
# 
# 
#                           multiple = T,
#                           #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
#                           width = "400px"
# 
#                         )),
# 
#                  column(4,
# 
#                         # style = "display: inline-block;",
#                         #style = "margin-top: 15px;",
#                         selectInput(
#                           inputId = "locaFil",
#                           label = "Locations",
#                           choices = c(locAv$Location),
#                           multiple = T,
#                           #options = pickerOptions(multipleSeparator = ";",actionsBox = TRUE,liveSearch = TRUE),
#                           width = "400px"
# 
#                         )),
# 
#                 column(4,
# 
# 
#                        selectInput(
#                          inputId = "selcolumns",
#                          label = "Column selection",
# 
#                          choices = c(colnames(seldiscolumns),
#                                      selected = "Title"),
# 
# 
# 
#                          multiple = T,
# 
#                          width = "400px"
# 
#                        )),
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#                  column(4,
#                         #style = "display: inline-block;",
#                         # style = "margin-top: 15px;",
#                         actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "lg",class = "btn-warning")
# 
#                  )
# 
#                )),
#                br(),
#                br(),
# 
#                  fluidRow(
#                    column(6,
#                           actionButton("reset_btn_browse", "Reset Trials",class = "btn-success")),
#                    column(6,
#                           actionButton("collapse_btn_browse", "Collapse All",class = "btn-info") )
#                  ),
# 
#                br(),
#                reactableOutput("filterbrowse"),
#                br(),
#                reactableOutput("browsetable"),
# 
#     #added this for selection of columns for display
#              #  br(),
#               # reactableOutput("table"),
# 
#              #  input_fil,
#              #   input_fst,
# 
# 
#       # MATCH
#       # tabPanel("Match",
#       #          reactableOutput("matchtable")),
# 
#      theme = bs_theme(version = 5,
#                       bootswatch = "cosmo",
#                       primary = "#246725")
# 
#     ) # tabpanel close
#  ) # navbar close
#  ) # taglist close
# )
# )

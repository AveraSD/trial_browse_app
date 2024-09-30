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
#  dashboardHeader(title = "TrialMatch",titleWidth = 350), # with blue original 
  dashboardHeader(title = "TrialMatch", titleWidth = 350,
                  tags$li(class="dropdown",tags$style(HTML(".skin-blue .main-header .navbar, .skin-blue .main-header .logo {background-color: #007852; }")))),
  
  dashboardSidebar(collapsed = TRUE,  # sidebar panel does not load when initial loading
    
                   #commented block working from before
    # tags$style(HTML("
    #   .main-sidebarmenu{
    #     width: 350px;
    #   }
    # ")),
    
    #new block
    width = 350, #original was 370
   # tags$style(HTML('.skin-blue .sidebar { background-color: #E6E4D0  ; width:370px; height: 598vh;}', #this is light tan with green buttons
 #  div(style="overflow-y: scroll"),
       #with gray background and green and tan and purple buttons  
   #changed width 370px to 350px
   tags$style(HTML('.skin-blue .sidebar { background-color: #D1D3D3  ; width:350px; height: 98vh;}',   #height set earlier to 200vh,105 introduced another scroll bar; so adjusted       
                    #    '.left-side, .main-sidebar {padding-top: 20px}',
                    #     '.skin-blue .sidebar .selectize-control { background-color: #D1D3D3 }',
                    #    '.skin-blue .sidebar .selectize-input { background-color: #D1D3D3 }',
                    '.skin-blue .sidebar .selectize-dropdown { background-color: #D1D3D3  }',
                    #  '.skin-blue .sidebar .sidebar-menu .treeview-menu > li >a {color: black;}',
                    #  '.skin-blue .sidebar h5 {color:black; }',
                    '.skin-blue .control-label { color: black; }',
                    '.skin-blue .sidebar .form-group .control-label {color: black; }',
                    #  '.well {color: white;}',
                    '.well .control-label { color: black;}',
                    '.sidebar .checkbox label {color: black;}',
                    #  '.btn-yellow {background-color: #F8EB60; color: #F8EB60 ;}',
                    #  '.btn-yellow {background-color: #F8EB60;color: black ;}', good yellow put back
                    '.btn-green {background-color: #C4E86B;color: black ;}',
                    '.btn-orange {background-color: #FF7F2F; color: black ;}',
                    '.btn-purple {background-color: #644B78; color: white ;}',
                    '.btn-resetorange {background-color: #FF7F2F; color: black ;}',
                    
                    '.btn-ltgreen {background-color: #72AA85; color: white ;}',
                    '.btn-tan {background-color: #9D9666; color: white ;}',
                    
                   #adding freeze header for reactable 
  #                  '
  #   .reactable .rt-thead {
  #     position: sticky;
  #     top: 0;
  #     z-index: 1;
  #     background-color: white;  /* Optional: Keeps header background solid */
  #   }
  # ',
  #                  
                   
                    #   '.skin-blue .sidebar .checkbox .control-label { color: black;}' 
    )),
    
    
    
    
    
    
    sidebarMenu(
      menuItem("Browse", tabName = "browse", icon = icon("search")),

   #  menuItem("Browse", tabName = "browse", style = "background-color: #E6E4D0;"),      
      
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
 
# style = "background-color: #E6E4D0;",
 style = "background-color: #D1D3D3;",
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
                                    #  choices = c(diseasAv$code), original without "" as a choice
                                      choices = c(diseasAv$code,""),
                                 #     multiple = TRUE, original kept as multiple
                                      multiple = FALSE,
                                      selected = "",
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

                                    ),
 
                          #add phase
                                    # selectInput(
                                    # inputId = "PhaseFil",
                                    # label = "Phase",
                               #     choices = c(PhaseAv$Phase), 
                                    # choices = c(PhaseAv$Phase),
                                    # multiple = T,
                                    # width = "100%"
                                    # 
                                    # ),


                          #add phase ends

                                #    )
                                selectInput(
                                  inputId = "trialTyxFil",
                                  label = "Trial Type",
                                  choices = c(trialTyAv$JIT),
                                  multiple = T, width = "100%"
                                  ),
 
                              # add trial status
                                selectInput(
                                inputId = "trstatFil",
                                label = "Trial Status",
                                choices = c(trstatAv$HoldStatus),
                                multiple = T, width = "100%"
                                ),



                              #add trial status ends
                            # column(4,
 


                                    selectInput(
                                      inputId = "locaFil",
                                      label = "Locations",
                                      choices = c(locAv$Location),
                                      multiple = T,

                                      width = "100%"

                                    ),
                                    #)
                                    
#following is for selective display
                            # column(4,


                                    selectInput(
                                      inputId = "selcolumns",
                                      label = "Column selection",

                                      choices = c(colnames(seldiscolumns)),
                                      #            choices = c(colnames(browse_tbl),
                                              # selected = names(browse_tbl)),



                                      multiple = T,

                                      width = "100%"

                                    )
                                #    )
                                    ,

###introducing show only open trials code July 5th##########

#checkboxInput("show_closed","show closed trials",value = FALSE),  ## original placed

#checkboxInput("show_selected","show selected"),



###show open trials only code july 5th

                         #    column(4,

                                 #   actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "sm",class = "btn-warning",width="50%"),
#new
actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "sm",class = "btn-ltgreen",width="50%"),
                           #  ) #column

                             # )
       #                    ), #fluidrow,
                           br(),
                           br(),

                          # fluidRow(
                           #  column(4,
              #                      actionButton("reset_btn_browse", "Reset Trials",class = "btn-success", width = "50%"),
# ),
#new
actionButton("reset_btn_browse", "Reset Trials",class = "btn-tan", width = "50%"),
                            # column(4,
                            #        actionButton("collapse_btn_browse", "Collapse All",class = "btn-info", width = "50%") ,
#new
actionButton("collapse_btn_browse", "Collapse All",class = "btn-purple", width = "50%") ,

# )
  #                         ),
                           #, #fluidrow
###introducing show only open trials code July 5th##########

checkboxInput("show_closed","show closed trials",value = FALSE),

#adding action button for toggle search boxes
#actionButton("toggle_search","Toggle Search Box"),
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
      
      #add reactable sticky header
      tags$head(
        tags$style(HTML("
        /* Reactable sticky header */
       ' .reactable .rt-thead {
          position: sticky !important;
          top: 0 !important;
          z-index: 1 !important;
          background-color: white !important;
        }'
        # /* Sidebar scrollbar on the left */
        # .main-sidebar {
        #   overflow-y: auto;
        #   direction: rtl;
        # }
        # .main-sidebar .sidebar-menu {
        #   direction: ltr;
        # }
      "))
      ),
      
      
      
      # end
      
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
                              # primary = "#246725")
     # primary = "#D6EADD")
      primary = "#D1D3D3")
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

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

  ##### BROWSE ########
  # main display table for BROWSE
  output$browsetable <- renderReactable({
    display_browse_db # from panel_browse.R
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

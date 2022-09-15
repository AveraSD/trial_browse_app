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


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ), 
    navbarPage(
      title = "TrialMatch", 
      
      # BROWSE
      tabPanel("Browse",
               actionButton("collapse_btn_browse", "Collapse All"), 
               reactableOutput("browsetable")),
      
      # MATCH
      tabPanel("Match",
               reactableOutput("matchtable")),
      
      theme = bs_theme(version = 5, 
                       bootswatch = "cosmo",
                       primary = "#246725")
      
    )
  )
))

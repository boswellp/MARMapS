#===================================================================================#
#-----------------------------------------------------------------------------------#
# This is the UI (user interface) for MARMapS v 1.1. Tutorials and instructions can #
# be found at https://github.com/galengorski/MARMapS. Questions can be sent to      #
# ggorski@ucsc.edu                                                                  #
#-----------------------------------------------------------------------------------#
# GG                                                                                #
# 1/20/2020                                                                         #
#-----------------------------------------------------------------------------------#
#===================================================================================#

#===================================================================================#
#####INSTALL PACKAGES#####

library(shiny)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('raster')
library(raster)
#install.packages('rasterVis')
library(rasterVis)
#install.packages('shinyWidgets')
library(shinyWidgets)
#install.packages(rgdal)
library(rgdal)
#install.packages('maptools')
library(maptools)
#install.packages('grid')
library(grid)
#install.packages('tidyverse')
library(tidyverse)
#Run this script of functions
source('checknull_function.R')

#####
#===================================================================================#

#===================================================================================#
#####ORGANIZATIONAL KEY#####

#===============================================================#
#===============================================================#
# INDICATES A DIVISION BETWEEN TABS

#===============================================================#
# INDICATES A DIVISION BETWEEN LAYERS WITHIN TABS

#---------------------------------------------------------------#
# INDICATES A DIVISION BETWEEN FUNCTIONS

#####
#===================================================================================#

#===================================================================================#
#####USER INTERFACE#####
fluidPage(
  titlePanel('MARMapS'),
  h5(strong('M'),'ulti-factor ',strong('A'),'nalysis for',strong('R'),'echarge',strong('Map'),'ipping',strong('S'),'uitability'),
  h5('MARMapS is a free, open source tool that facilitates the creation of landscape suitability maps. Use the options to the left to upload
    raster files and get started.'),
  h5("For instructions and a tutorial",
     a(href="https://github.com/galengorski/MARMapS", "Click Here!")),
  tabsetPanel(type = "tabs",
              tabPanel("Factor Suitability Ranking", 
                       #####Slider and Input Layouts####
                       #Layer1#
                       h4('First Factor'),
                       fluidRow(column(4,
                                       fileInput("file1", "Upload GeoTiff File",accept = c('.tiff')),
                                       fileInput("file_rast_codes","Upload Raster Codes", accept = c('.csv')),
                                       textInput("data_name", "Name", " "),
                                       textInput("data_units", "Units", " "),
                                       uiOutput('good_range'),
                                       actionButton('add_range', 'Add New Range'),
                                       actionButton('classify','Classify'),
                                       #this is here to print outputs if needed, its server bit is located at the end of the server function
                                       textOutput('reclass_matrix'),
                                       uiOutput('change_scale')
                                       
                                       
                       ),
                       column(4,
                              plotOutput('plot1', height = '600px')),
                       column(4,
                              plotOutput('plot2',height = '600px'))),
                       #placeholder for another layer
                       tags$div(id = 'placeholder'),
                       actionButton('addBtn', 'Add Factor'), 
                       #actionButton('removeBtn', 'Remove Factor'),
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }")
              ),
              #===============================================================#
              #===============================================================#
              #WEIGHTING AND COMPOSITE MAPPING TAB
              tabPanel("Weighting and Composite Mapping", 
                       headerPanel('Weight and Aggregate Factors'),
                       fluidRow(column(6,
                                       plotOutput("test_1", height = '600px')),
                                column(3,
                                       plotOutput("dist_comp_suit", height = '320px'),
                                       plotOutput("comp_bplot", height = '320px')),
                                
                                column(3,
                                       h3('Factor Weights'),
                                       uiOutput('weights'),
                                       tags$div(id = 'weights_nl'),
                                       textOutput('sum_weights'),
                                       #actionButton('even_weights', 'Even Weights'),
                                       fileInput(inputId = "sites", label = "Upload Site Locations", multiple  = TRUE, 
                                                 accept = c('.shp', '.dbf','.shx','.qpj','.prj','.cpg')),
                                       downloadButton('download_comp','Download Map')),
                                #downloadButton('download_report','Download Suitability Report')),
                                textOutput('weight_check')),
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }")
                       # fluidRow(column(3,
                       #                 plotOutput("comp_bplot")),
                       #          column(3,
                       #                 plotOutput("dist_comp_suit")),
                       #          column(6,
                       #                 plotOutput("dummy_plot")))
              )
  ))
#####
#===================================================================================#

library(shiny)
library(shinydashboard)
library(proxy)
#library(recommenderlab)
library(reshape2)
library(plyr)
library(dplyr)
library(DT)
library(RCurl)

# load data files
mf_df <- read.csv("Mutual_Funds.csv")

shinyUI(fluidPage(
  
  titlePanel(title = "Mutual Fund Recommender System"),
  sidebarLayout(position = "left",
    sidebarPanel(h3("Enter the fund name"),
                 #textInput("name", "Enter Fund Name", "" ),
                 
                 selectInput("input_item", "Fund Name", choices = c("",as.list(mf_df[,2])))
                          ),


    mainPanel(h4("Recommended Funds"),
              textOutput("rec_fundname"),
              tabItem(tabName = "funds recommended",
                      fluidRow(
                        box(
                          width = 6, status = "info", solidHead = TRUE,
                          title = "Other Funds You Might Like"
                         ,DT::dataTableOutput("table"))
                      )
              )
              )
  )
)

)

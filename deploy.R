################################################################################
# Deploy of the Shiny app 
#
# NOVA SBE - Network Analytics
# March 2022
################################################################################

install.packages("shiny")
install.packages("rsconnect") # used to deploy
library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(rsconnect) 

setAccountInfo(name='mariabaglieri',
               token='FB23D4BEDF5741055B7ADA70E17A66C1',
               secret='f+za586KUfvJnHLaWRHvGvPiGXPAxAZTwun/Cywk')

deployApp('/Users/eugeniasaggioro/Desktop/NA_Project_Group_17')

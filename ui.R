################################################################################
# UI of the Shiny app 
#
# NOVA SBE - Network Analytics
# March 2022
################################################################################
library(data.table)
library(igraph)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(treemap)

################################################################################

ui <- 
  bootstrapPage('',
                navbarPage("Book explorer",
                           tags$style(HTML("
                           .navbar-default .navbar-brand {color:white;} 
                           .navbar-default .navbar-brand:hover {color:white;}
                           .navbar { background-color:#C85250;}
                           .navbar-default .navbar-nav > li > a {color:white;}
                           .navbar-default .navbar-nav > .active > a,
                           .navbar-default .navbar-nav > .active > a:focus,
                           .navbar-default .navbar-nav > .active > a:hover {color:black;background-color:white;} 
                           .fa-bookmark {color:#B34746;}
                           .fa-feather {color:#B34746;}
                           .fa-users {color:#B34746;}
                           .fa-book-open {color:#B34746;} 
                           .fa-calendar-day {color:#B34746;}
                           .fa-layer-group {color:#B34746;}")),
                           
                           tabPanel("Our Library",
                                    fluidRow(
                                      column(12,
                                             column(6,
                                                    box(title = "About our ShinyApp", width = NULL, solidHeader = TRUE, 
                                                        p("Dear visitor,", 
                                                          
                                                          "Thank you for visiting our Shiny app about books. We are five students from the Master's in Business Analytics at NOVA SBE and 
                                                        this is our Final Project for the Course in Network Analytics.", 
                                                          
                                                          p("Do you want to find the next book to read? Do you want to know which are the most famous books based on our users' experience? 
                                                        Or probably you simply want some inspiration based on an author/publisher you already know.
                                                        With this app, we will try to provide you with useful information and analyses regarding all 
                                                        the books we know and the network of users who read and rated those books. "), 
                                                          
                                                          p("The app is divided into four main pages.", 
                                                            
                                                            "The", strong("Statistics"), "page shows the key features and descriptive statistics of our dataset. 
                                                        Check here which are the main characteristics of our books and readers."),
                                                          
                                                          p("The", strong("Overall Plot"), "page allows you to see several different visualizations of our huge book-reader network. 
                                                         You can filter the plot based on a Genre you usually prefer, an Author you appreciate, or a Publisher you like. 
                                                         Based on the filter, the plot will show all the books (vertices) and the users who read those books (edges). 
                                                         The width of the edge is proportioned to the number of readers."), 
                                                          
                                                          p("Following the navigation bar, on the fourth page we created an", strong("elaborate analysis"), "that could be useful for you to understand 
                                                        which books our users will probably read, based on their past experience. Based on books with common readers, we are trying in this 
                                                        page to predict some future links among books. The probability that a user will read the displayed books is based on the analysis of common neighbors 
                                                        (see setup.R file for more explanation of the logic behind this page). The highest the probability, the fewer the books that will appear. 
                                                        e.g. If User X already read Book A, Book B, and Book C, which book he will probably read next.
                                                        On this page, you have to search for a specific book, so that you can see which books are strictly linked to your selection."),
                                                          
                                                          p("The last page is probably for the ones who want just to know which are the", strong("most appreciated books"), "based on Average Rating.
                                                        Here you can always choose your filters and check for some of the most central books in our dataset."), sep="<br/>")
                                                    )
                                             ),
                                             column(6,
                                                    box(img(src = ("foto.png"), width = 600)))
                                      )
                                    ),
                                    tags$br(),
                                    h4("Here below you can find a table with the main variables of the whole dataset. This might probably help you navigate in our app."),
                                    tags$br(),
                                    fluidRow(column(width = 12, dataTableOutput("our.library")))
                           ),
                           tabPanel("Statistics",
                                               dashboardBody(
                                                 wellPanel(
                                                   fluidRow(
                                                     column(12, h3("Key features of our book dataset"),
                                                            helpText(" Here below the counts of our main variables"),
                                                            column(2,valueBox(dt.Complete.Books[, length(unique(ISBN))], 
                                                                              "Books", icon = icon("book-open", lib = "font-awesome", "fa-3x"))),
                                                            column(2, valueBox(dt.Complete.Books[, length(unique(Book_Author))], 
                                                                               "Authors", icon = icon("feather", lib = "font-awesome", "fa-3x"))),
                                                            column(2, valueBox(dt.Complete.Books[, length(unique(User_ID))],
                                                                               "Readers", icon = icon("users", lib = "font-awesome", "fa-3x"))), 
                                                            column(2, valueBox(dt.Complete.Books[, length(unique(Publisher))],
                                                                               "Publishers", icon = icon("bookmark", lib = "font-awesome", "fa-3x"))),
                                                            column(2, valueBox(paste0(min(dt.Complete.Books$Year_of_Publication),"-", max(dt.Complete.Books$Year_of_Publication)),
                                                                               "Years", icon = icon("calendar-day", lib = "font-awesome", "fa-3x"))), 
                                                            column(2, valueBox(dt.Complete.Books[, length(unique(Category))],
                                                                               "Genres", icon = icon("layer-group", lib = "font-awesome", "fa-3x"))), align = "center"),
                                                   )),
                                                 fluidRow(column(6, 
                                                                 h2("Number of books published over years"), 
                                                                 helpText("The highest number of books in our dataset have been published between the 90s and the first decade of 21st century! "),
                                                                 plotOutput("stat.books.per.year")),
                                                          column(6, 
                                                                 h2("Which editors published the most?"), 
                                                                 helpText("Ballantine Books, Harlequin and Pocket are the Top 3 book publishers in this dataset!"),
                                                                 plotOutput("stat.publisher"))),
                                                 br(),
                                                 fluidRow(column(6, 
                                                                 h2("Number of books for each genre"), 
                                                                 helpText("More than half of the books in our dataset belongs to Fiction genre!"),
                                                                 dataTableOutput("stat.books.per.genre")),
                                                          column(6, 
                                                                 h2("Average rating distribution across books"), 
                                                                 helpText("On a scale from 1-10, our users'average rating is below the median."),
                                                                 plotOutput("stat.rating"))),
                                                 br(),
                                                 wellPanel(
                                                   h3("Some main statistics regarding our users", align = "center"),
                                                   fluidRow(column(6,
                                                                   h3("Readers activity statistics:", align = "center"),
                                                                   htmlOutput("main.stat.readers"), align = "center"),
                                                            column(6,
                                                                   h3("Reader rating statistics:", align = "center"),
                                                                   htmlOutput("main.stat.ratings"), align = "center")
                                                            )
                                                 ))),
                           
                           tabPanel("Overall Plot",
                                    fluidPage(
                                      pageWithSidebar(
                                        headerPanel (title = "Subset the overall network"),
                                        sidebarPanel(
                                          h3("Filters"),
                                          helpText("Choose one of these filters below to subset the overall plot."),
                                          fluidRow(
                                            column(12, 
                                                   radioButtons(inputId = "plot_type", 
                                                                label = "Plot the network, please choose one:",
                                                                choices = c("Genres" = "genre", "Authors" = "authors", "Publishers" = "publishers"),
                                                                inline = TRUE),
                                                   conditionalPanel(condition = "input.plot_type == 'genre'",
                                                                    selectizeInput(inputId = "genre",
                                                                                label = "Choose a genre:",
                                                                                choices = unique(sort(dt.Complete.Books$Category)))),
                                                   conditionalPanel(condition = "input.plot_type == 'authors'",
                                                                    selectizeInput(inputId = "authors",
                                                                                   label = "Write the name of an author:",
                                                                                   choices = unique(sort(dt.Complete.Books$Book_Author)))),
                                                   conditionalPanel(condition = "input.plot_type == 'publishers'",
                                                                    selectizeInput(inputId = "publishers",
                                                                                label = "Check for a publisher:",
                                                                                choices = unique(sort(dt.Complete.Books$Publisher)))),
                                                   h3("Network statistics"),
                                                   helpText("Let's see the statistics of the displayed Network!"))),
                                          htmlOutput("network.statistics"),
                                          htmlOutput("nan"),
                                          h4("Histogram of the network's nodes degrees", align = "center"),
                                          plotOutput("hist.degree")
                                        ),
                                        mainPanel(
                                          textOutput("error"),
                                          plotOutput("network.plot", width = "100%", height = "1000px"),
                                        )
                                      )
                                    ),
                                    wellPanel(h3("All the books present in the overhead Network:"),
                                              h5("Do you want to know more about one of these books? Search for it in Our Library page!"),
                                              dataTableOutput("network.books"))
                           ),
                           
                           tabPanel("Book Recommendation",
                                    wellPanel(
                                      headerPanel(title = "Find the right book for you!"),
                                      helpText("In this page you can see which books you should read based on the one you select.
                                      The recommendation system shows books that our users will probably read, based on your selection.
                                      You can also show the recommendation according to the probability that our users will read the associated books."),
                                      selectizeInput(inputId = "book.title",
                                                     label = "Insert the Title of a Book:",
                                                     choices = unique(sort(dt.main.readers$Book_Title)),
                                                     selected = "The Poisonwood Bible"),
                                      selectInput("min.width", "Choose the Level of Connections:",
                                                  c("High" = 6,
                                                    "Medium" = 4,
                                                    "Low" = 2)),
                                      plotOutput("recommender.system"),
                                      div(h3(
                                          htmlOutput("no.recommendations"))),
                                      dataTableOutput("recommended.list"))
                           ),
                           
                           tabPanel("Top 10",
                                    wellPanel(
                                        h2(" TOP 10 BOOKS", align = "center"),
                                        helpText(" Check which are the most famous books based on average ratings. You can filter based on genre, author or publisher and see how much our users ranked that book."),
                                        radioButtons(inputId = "top",
                                                     label = "Choose a filter:",
                                                     choices = c("None" = "all", "Genre" = "genretop", "Author" = "authortop", "Publisher" = "publishertop"),
                                                     inline = TRUE),
                                        conditionalPanel(condition = "input.top == 'genretop'",
                                                         selectizeInput(inputId = "gen",
                                                                        label = "Select a genre: ",
                                                                        choices = unique(sort(dt.Complete.Books$Category)))),
                                        conditionalPanel(condition = "input.top == 'authortop'",
                                                         selectizeInput(inputId = "aut",
                                                                        label = "Check for the author's name: ",
                                                                        choices = unique(sort(dt.Complete.Books$Book_Author))
                                                         )),
                                        conditionalPanel(condition = "input.top == 'publishertop'",
                                                         selectizeInput(inputId = "pub",
                                                                        label = "Check for the publisher's name: ",
                                                                        choices = unique(sort(dt.Complete.Books$Publisher)))),
                                      ),
                                    fluidRow(
                                      column(12, dataTableOutput("table.top10"))
                                    )
                           )
                           )
  )


ui
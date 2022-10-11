################################################################################
# Server of the Shiny app 
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

server <- function(input, output) {

  output$our.library <- renderDataTable({
    unique(dt.Complete.Books[, list("Book Title" = Book_Title, 
                                    "Book Author" = Book_Author, 
                                    "Year of Publication" = Year_of_Publication,
                                    "Publisher" = Publisher,
                                    ISBN)], by = "ISBN")})
  
  output$stat.books.per.year <- renderPlot({
    dt.Complete.Books$Year_of_Publication <- as.numeric(as.character(dt.Complete.Books$Year_of_Publication))
    hist(dt.Complete.Books$Year_of_Publication, xlab = "Years" , ylab= "Number of books", main = "", breaks = 18 ,col="#CB857C")})
  
  output$stat.publisher <- renderPlot({treemap(dt.Complete.Books, index = "Publisher", vSize = "Nr_books_per_publisher", type="index", 
                                         palette= c("#fff1f1", "#f9e0e0", "#f3d0d0", "#edbfbf", "#e7aeae", "#e09e9e", "#da8d8d", 
                                                    "#d47b7b", "#cd6a6a", "#c65757", "#bf4343", "#b72d2d"),
                                         fontcolor.labels = c("black"), border.col = c("white"), title = "")}) 
  
  output$stat.books.per.genre <- renderDataTable({
    unique(dt.Complete.Books[, list("Genre" = Category, "Books" = Nr_books_per_category)], by = "Genre")[order(-Books)]},
    options = list(pageLength = 10))
  
  output$stat.rating <- renderPlot({
    hist(unique(dt.Complete.Books$Average_Rating, by = "ISBN"), 
         xlab = "Average rating", ylab = "Number of books", main = "", col = "#CB857C")})
  
  output$main.stat.readers <- renderText({
    paste("How many books does one reader read? It differs:", "<br/>",
          "<b>Mean: ", round(mean(unique(dt.Complete.Books$Nr_books_per_user)), 2), "<br/>",
          "<b>Median: ",round(median(unique(dt.Complete.Books$Nr_books_per_user)), 2), "<br/>",
          "<b>Standard Deviation: ", round(sd(unique(dt.Complete.Books$Nr_books_per_user)), 2), "<br/>",
          "<b>Min: ", min(dt.Complete.Books$Nr_books_per_user), "<br/>",
          "<b>Max: ", max(dt.Complete.Books$Nr_books_per_user))})

  
  output$main.stat.ratings <- renderText({paste("How high does one reader rate on average?", "<br/>",
                                      "<b>Mean: ", round(mean(dt.Complete.Books[, Average_Rating]), 2), "<br/>",
                                      "<b>Median: ",round(median(dt.Complete.Books[, Average_Rating]), 2), "<br/>",
                                      "<b>Standard Deviation: ", round(sd(dt.Complete.Books[, Average_Rating]), 2), "<br/>",
                                      "<b>Min: ", min(dt.Complete.Books[, Average_Rating]), "<br/>",
                                      "<b>Max: ", max(dt.Complete.Books[, Average_Rating]))})
  
  output$network.plot <- renderPlot({
    validate(
      need(input$genre != "", "Please check the list and select a valid genre.")
    )
    if (input$plot_type == 'genre') {
      all.books <-  dt.Complete.Books[dt.Complete.Books$Category == input$genre, list(name = unique(Book_Title), type = FALSE)]
      all.userid <- dt.Complete.Books[dt.Complete.Books$Category == input$genre, list(name = unique(User_ID), type = TRUE)] 
      all.vertices <- rbind(all.books, all.userid)
      g.book <- graph.data.frame(dt.Complete.Books[dt.Complete.Books$Category == input$genre, list(Book_Title, User_ID, Nr_Readers)], directed = FALSE, vertices = all.vertices)
      if (nrow(all.books) >= 350) {
        V(g.book)$degree <- degree(g.book)
        vertices.to.delete <- V(g.book)[V(g.book)$degree < 60]
        g.book.filtered <- delete.vertices(g.book, vertices.to.delete)
        g.book.author.id <- bipartite.projection(g.book.filtered, multiplicity = TRUE, which = FALSE)
      } else {
        g.book.author.id <- bipartite.projection(g.book, multiplicity = TRUE, which = FALSE)
      }
    } else if (input$plot_type == 'authors') {
      validate(
        need(input$authors != "", "Please check the list and select a valid author.")
      )
      all.books <-  dt.Complete.Books[dt.Complete.Books$Book_Author == input$authors, list(name = unique(Book_Title), type = FALSE)]
      all.userid <- dt.Complete.Books[dt.Complete.Books$Book_Author == input$authors, list(name = unique(User_ID), type = TRUE)] 
      all.vertices <- rbind(all.books, all.userid)
      g.book <- graph.data.frame(dt.Complete.Books[dt.Complete.Books$Book_Author == input$authors, list(Book_Title, User_ID, Nr_Readers)], directed = FALSE, vertices = all.vertices)
      g.book.author.id <- bipartite.projection(g.book, multiplicity = TRUE, which = FALSE)
    } else if (input$plot_type == 'publishers') {
      validate(
        need(input$publishers != "", "Please check the list and select a valid editor.")
      )
      all.books <-  dt.Complete.Books[dt.Complete.Books$Publisher == input$publishers, list(name = unique(Book_Title), type = FALSE)]
      all.userid <- dt.Complete.Books[dt.Complete.Books$Publisher == input$publishers, list(name = unique(User_ID), type = TRUE)] 
      all.vertices <- rbind(all.books, all.userid)
      g.book <- graph.data.frame(dt.Complete.Books[dt.Complete.Books$Publisher == input$publishers, list(Book_Title, User_ID, Nr_Readers)], directed = FALSE, vertices = all.vertices)
      if (nrow(all.books) >= 350) {
        V(g.book)$degree <- degree(g.book)
        vertices.to.delete <- V(g.book)[V(g.book)$degree < 50]
        g.book.filtered <- delete.vertices(g.book, vertices.to.delete)
        g.book.author.id <- bipartite.projection(g.book.filtered, multiplicity = TRUE, which = FALSE)
      } else {
        g.book.author.id <- bipartite.projection(g.book, multiplicity = TRUE, which = FALSE)
      }
    }
    if (length(E(g.book.author.id)) > 0) {
      E(g.book.author.id)$width <- E(g.book.author.id)$weight
    }
    if (length(V(g.book.author.id)) < 50) {
      V(g.book.author.id)$label <- V(g.book.author.id)$name
    } else {V(g.book.author.id)$label <- ""}
    plot(g.book.author.id,
         vertex.shape ="circle", 
         vertex.color = "#CCAFA5", 
         vertex.frame.color = "cornsilk", 
         vertex.size = 15, 
         vertex.label.font = 2,
         vertex.label.family = "Arial",
         vertex.label.color = "black")
    
    output$network.statistics <- renderUI({
      list(p("Average path length:", round(mean_distance(g.book.author.id), 2)),
           p("Average clustering coefficient:", round(transitivity(g.book.author.id, type = "average"), 2)),
           p("Diameter of this network:", round(diameter(g.book.author.id), 2))
      )
    })
    
    output$nan <- renderUI({
      if (length(E(g.book.author.id))<=1){
      p("If NaN is displayed it means that our network has zero or one edge, so that it's not possible to calculate those centralities.")
      }
    })
    
    output$hist.degree <- renderPlot({
      hist(degree(g.book.author.id),
           xlab = "Degree",
           ylab = "Frequency",
           main = "",
           col = "#CCAFA5")
    })
    
    output$network.books <- renderDataTable({
      dt.network.books <- data.table(as.vector(V(g.book.author.id)$name))
      colnames(dt.network.books)[1] <- "Book Title"
      dt.network.books
    })
  })
  
  output$recommender.system <- renderPlot({
    edges.to.delete <- E(g.predicted.edges)[width < input$min.width]
    g.predicted.edges.filtered <- delete.edges(g.predicted.edges, edges.to.delete)
    validate(
      need(input$book.title != "", "Please check the list and select a valid book title.")
    )
    g.predicted.edges.filtered <- induced.subgraph(g.predicted.edges.filtered, neighborhood(g.predicted.edges.filtered, order = 1,
                                                                                            nodes = V(g.predicted.edges.filtered)[input$book.title])[[1]])
    if (length(V(g.predicted.edges.filtered)) < 50) {
      V(g.predicted.edges.filtered)$label <- V(g.predicted.edges.filtered)$name
    } else {V(g.predicted.edges.filtered)$label <- ""}
    plot(g.predicted.edges.filtered,
         vertex.shape = "circle", 
         vertex.color = "#CCAFA5", 
         vertex.frame.color = "cornsilk",
         edge.size = c(1,10),
         vertex.size = 15, 
         vertex.label.font = 2,
         vertex.label.family = "Arial",
         vertex.label.color = "black")
    
    output$no.recommendations <- renderUI({
      if (length(V(g.predicted.edges.filtered)) <= 1) {
        ("Sorry, we do not have any book to recommend you based on your choices!")
      } else {
        list(("All the books you should read, according to the one you selected:"),
        h5("Do you want to know more about one of these books? Search for it in Our Library page!"))
      }
    })
    
    output$recommended.list <- renderDataTable({
      if (length(V(g.predicted.edges.filtered)) > 1) {
      dt.recommended.books <- data.table(as.vector(V(g.predicted.edges.filtered)$name))
      dt.recommended.books <- dt.recommended.books[!(dt.recommended.books$V1 == input$book.title)]
      colnames(dt.recommended.books)[1] <- "Book Title"
      dt.recommended.books}})
    
  })
  
  output$table.top10 <- renderDataTable({
    if (input$top == "all") {
      df <- unique(dt.Complete.Books[, list(Ranking = "",
                                            "Title" = Book_Title, 
                                            "Author" = Book_Author , 
                                            "Year of Publication" = Year_of_Publication, 
                                            ISBN, 
                                            "Genre" = Category, 
                                            "Rate" = Average_Rating, 
                                            "Readers" = Nr_Readers)], by = "ISBN")[order(-Rate, -Readers)]
      df$Ranking <- 1:nrow(df)
      data.table(df)
      
    } else if (input$top == "genretop") {
      df <- unique(dt.Complete.Books[dt.Complete.Books$Category == input$gen, 
                                     list(Ranking = "",
                                          "Title"= Book_Title, 
                                          "Author"= Book_Author , 
                                          "Year of Publication" = Year_of_Publication, 
                                          ISBN, 
                                          "Genre" = Category, 
                                          "Rate" = Average_Rating)], by = "ISBN")[order(-Rate)]
      df$Ranking <- 1:nrow(df)
      data.table(df)
      
    } else if (input$top == "authortop") {
      df <- unique(dt.Complete.Books[dt.Complete.Books$Book_Author == input$aut, 
                                     list( Ranking = "",
                                           "Title"= Book_Title, 
                                           "Author"= Book_Author , 
                                           "Year of Publication" = Year_of_Publication, 
                                           ISBN, 
                                           "Genre" = Category, 
                                           "Rate" = Average_Rating)], by = "ISBN")[order(-Rate)]
      df$Ranking <- 1:nrow(df)
      data.table(df)
    } else if (input$top == "publishertop") {
      df <- unique(dt.Complete.Books[dt.Complete.Books$Publisher == input$pub, 
                                     list(Ranking = "",
                                          "Title"= Book_Title, 
                                          "Author"= Book_Author , 
                                          "Year of Publication" = Year_of_Publication, 
                                          ISBN, 
                                          "Genre" = Category, 
                                          "Rate" = Average_Rating)], by = "ISBN")[order(-Rate)]
      df$Ranking <- 1:nrow(df)
      data.table(df)
      
    }}, (options = list(pageLength = 10, dom = "t")), escape = FALSE)
  
  }

server

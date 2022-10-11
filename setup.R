---
title: "Books"
output: html_document
---

#Import libraries
library(data.table)
library(igraph)
library(stringr)


#Import datasets
Books <- read.csv("Books.csv")
Ratings <- read.csv("Ratings.csv")

##Data cleaning of the book dataset
dt.Books <- data.table(Books)
dt.Books[dt.Books == '0'] <- NA
dt.Books[dt.Books == 'NA'] <- NA
dt.Books <- na.omit(dt.Books)
dt.Books <- setNames(dt.Books, c("ISBN","Book_Title","Book_Author","Year_of_Publication", "Publisher", "Image_URL_S", "Image_URL_M", "Image_URL_L"))
transform(dt.Books, Year_of_Publication = as.numeric(Year_of_Publication))
dt.Books <- dt.Books[dt.Books$Year_of_Publication >= '1960' & dt.Books$Year_of_Publication <= '2022',]
as.numeric(as.character(dt.Books$Year_of_Publication))
#We have deleted all the NA values of the Year_of_Publication column and selected the ones between 1960 and 2022.

#New columns
dt.Books[, Nr_books_per_author := .N, by = Book_Author]
dt.Books[, Nr_books_per_publisher := .N, by = Publisher]
dt.Books <- dt.Books[dt.Books$Nr_books_per_publisher >= 200,]
#We decided to keep only the books of the relevant publishers. 

#Data cleaning of the rating dataset
dt.Ratings <- data.table(Ratings)
dt.Ratings <- setNames(dt.Ratings, c("User_ID", "ISBN", "Book_Rating"))
dt.Ratings[, Average_Rating := round(mean(Book_Rating),2), by = ISBN]
dt.Ratings[, Nr_Readers := length(unique(User_ID)), by = ISBN]
dt.Ratings <- dt.Ratings[dt.Ratings$Nr_Readers >= 5, ]

#Merge the Book and Rating dataset
dt.Complete.Books <- merge(dt.Books, dt.Ratings, by="ISBN", all.x=FALSE) 

#Import and merge the dataset with categories
Prepocessed <- read.csv("Preprocessed_data.csv")
dt.Prepocessed <- data.table(Prepocessed)
dt.Complete.Books <- merge(dt.Complete.Books, unique(dt.Prepocessed[, c("isbn","Category")]), by.x="ISBN",
                           by.y="isbn", all=FALSE)

#Data cleaning of the new datatable
dt.Complete.Books$Category <- gsub("[[:punct:]]", "", dt.Complete.Books$Category)
dt.Complete.Books$Category[dt.Complete.Books$Category == "Christian Life"] <- "Religion"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Bible"] <- "Religion"
dt.Complete.Books$Category[dt.Complete.Books$Category == "9"] <- "NA"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Childrens stories America"] <- "Childrens stories"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Adventure stories"] <- "Adventure"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Adventure and adventurers"] <- "Adventure"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Family  Relationships"] <- "Families"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Games Activities"] <- "Religion"
dt.Complete.Books$Category[dt.Complete.Books$Category == "House  Home"] <- "House"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Science fiction"] <- "Science"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Great Britain"] <- "England"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Baggins Frodo Fictitious character"] <- "Fantasy"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Brothers and sisters"] <- "Brothers"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Brunetti Guido Fictitious character"] <- "Detective and mystery stories"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Businesswomen"] <- "Business  Economics"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Child psychologists"] <- "Suspense"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Childrens stories American"] <- "Childrens stories"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Covenant Thomas Fictitious character"] <- "Detective and mystery stories"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Intelligence service"] <- "Detective and mystery stories"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Diary fiction"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Actors"] <- "Biography  Autobiography"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Amsterdam Netherlands"] <- "Biography  Autobiography"
dt.Complete.Books$Category[dt.Complete.Books$Category == "African Americans"] <- "American literature"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Indians of North America"] <- "American literature"
dt.Complete.Books$Category[dt.Complete.Books$Category == "American drama"] <- "Drama"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Antiques  Collectibles"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Arthurian romances"] <- "Fantasy"
dt.Complete.Books$Category[dt.Complete.Books$Category == "American fiction"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "American poetry"] <- "Poetry"
dt.Complete.Books$Category[dt.Complete.Books$Category == "American wit and humor"] <- "Humor"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Dogs"] <- "Animals"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Cats"] <- "Animals"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Pets"] <- "Animals"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Veterinarians"] <- "Animals"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Dune Imaginary place"] <- "Fantasy"
dt.Complete.Books$Category[dt.Complete.Books$Category == "England"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Young Adult Fiction"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "English fiction"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "FICTION"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Fiction in English"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Domestic fiction"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "United States"] <- "American literature"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Authors American"] <- "American literature"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Americans"] <- "American literature"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Fantasy fiction"] <- "Fantasy"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Fantasy fiction American"] <- "Fantasy"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Frontier and pioneer life"] <- "Travel"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Games  Activities"] <- "Games"
dt.Complete.Books$Category[dt.Complete.Books$Category == "German language materials"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "German fiction"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Horror tales"] <- "Horror stories"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Horror tales American"] <- "Horror stories"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Humorous stories"] <- "Humor"
dt.Complete.Books$Category[dt.Complete.Books$Category == "JUVENILE FICTION"] <- "Juvenile Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Manwoman relationships"] <- "Romance fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Motion picture actors and actresses"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Performing Arts"] <- "Art"
dt.Complete.Books$Category[dt.Complete.Books$Category == "True Crime"] <- "Crime"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Assassins"] <- "Crime"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Murder"] <- "Crime"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Aunts"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Australia"] <- "Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Berlin Germany"] <- "Fantasy"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Blind"] <- "Juvenile Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Boarding schools"] <- "Juvenile Fiction"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Boston Mass"] <- "Detective and mystery stories"
dt.Complete.Books$Category[dt.Complete.Books$Category == "England"] <- "British"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Sports  Recreation"] <- "Health  Fitness"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Body Mind  Spirit"] <- "Health  Fitness"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Foreign Language Study"] <- "Language Arts  Disciplines"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Interplanetary voyages"] <- "Life on other planets"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Extraterrestrial beings"] <- "Life on other planets"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Crafts  Hobbies"] <- "Hobbies"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Gardening"] <- "Hobbies"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Camping"] <- "Hobbies"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Authors English"] <- "British"
dt.Complete.Books$Category[dt.Complete.Books$Category == "Curiosities and wonders"] <- "Hobbies"

dt.Complete.Books$Book_Author <- str_squish(dt.Complete.Books$Book_Author)   
dt.Complete.Books$Book_Author <- gsub("\\.","\\. ",dt.Complete.Books$Book_Author) 
dt.Complete.Books$Book_Author <- str_to_title(dt.Complete.Books$Book_Author)

dt.Complete.Books[, Nr_books_per_category := length(unique(ISBN)), by=Category]
dt.Complete.Books <- na.omit(dt.Complete.Books)
dt.Complete.Books <- dt.Complete.Books[dt.Complete.Books$Nr_books_per_category >= 5 
                                       & dt.Complete.Books$Category != 'NA',]

dt.Complete.Books <- dt.Complete.Books[dt.Complete.Books$Book_Author != dt.Complete.Books$Book_Title, ]
duplicates <- data.table(intersect(dt.Complete.Books$Book_Title, dt.Complete.Books$Book_Author))
colnames(duplicates) <- c('Book_Title')
dt.Complete.Books <- dt.Complete.Books[!duplicates, on = 'Book_Title']

dt.Complete.Books[, Image_URL_S:=NULL]
dt.Complete.Books[, Image_URL_M:=NULL]
dt.Complete.Books[, Image_URL_L:=NULL]

dt.Complete.Books[, Nr_books_per_user := .N, by = User_ID]

#Network for the predictive analysis.
dt.main.readers <- dt.Complete.Books[dt.Complete.Books$Nr_books_per_user == 10, ]
all.books <-  dt.main.readers[, list(name = unique(Book_Title), type = FALSE)]
all.userid <- dt.main.readers[, list(name = unique(User_ID), type = TRUE)] 
all.vertices <- rbind(all.books, all.userid)
g.book <- graph.data.frame(dt.main.readers[, list(Book_Title, User_ID)], 
                           directed = FALSE, vertices = all.vertices)
g.book.author.id <- bipartite.projection(g.book, multiplicity = TRUE, which = FALSE)
m.predicted.edges <- as.matrix(cocitation(g.book.author.id)*(1-get.adjacency(g.book.author.id)))
g.predicted.edges <- graph_from_adjacency_matrix(m.predicted.edges, mode="undirected", weighted=TRUE)
E(g.predicted.edges)$width <- E(g.predicted.edges)$weight


#Save the datatable and graph
save(list=c("dt.Complete.Books", "g.predicted.edges", "dt.main.readers"), file = "nda-book-explorer.RData")
load("nda-book-explorer.RData") 

##load up the libraries
library(rvest)
library(stringr)

#1)go to IMDB.com webpage with the list of all movies that came out in 2015.
Movies2015 <-
        read_html(
                "http://www.imdb.com/search/title?sort=moviemeter,asc&start=1&title_type=feature&year=2015,2015"
        )

#Get the max number of search results to determine number of page iterations IMDB maxes out at 50

NumResults <- html_nodes(Movies2015,"div div #left") %>% html_text
NumResults <- substr(NumResults[1],10,14)

##Calculate number of search result pages
NumPages <-
        ceiling(as.numeric(str_replace(NumResults,",","")) / 50)

##Get the search results on this page
AllMovieInfo <- html_nodes(Movies2015,"td.title")

# Args:
#   ID: IDs of the movies.
#
# Returns:

#what I want for each movie.
# Top 10 billed Cast Members, budget, running time, MPAA rating, IMDBrating
#Genre, Release Date, Gross USA Box Office
#In A data frame containing one line per movie, and 9 columns.

MyIMDBfun <- function(ID) {
        IDMovie.df <- data.frame()
        URL <- paste0("http://www.imdb.com/title/", ID)
        IDMovieInfo <- read_html(URL)
        
        ##Get Movie Title
        IDTitle <-
                html_nodes(IDMovieInfo, ".header .itemprop") %>% html_text
        
        
        ##Get Release Date
        if (length(html_nodes(IDMovieInfo, ".infobar .nobr a") %>% html_text) == 0) {
                IDRelDate<-NA_character_
        }
        else
                IDRelDate <-
                html_nodes(IDMovieInfo, ".infobar .nobr a") %>% html_text
                IDRelDate <-
                as.Date(gsub(" ","",substr(IDRelDate,1,18)),"%d%B%Y")
        
        
        ##Get MPAA rating
        if (length(
                html_nodes(
                        IDMovieInfo, "div.infobar meta[itemprop='contentRating']"
                ) %>% html_attr("content")
        ) == 0) {
                IDMPAArating<-NA_character_
        }
        else
                IDMPAArating <-
                html_nodes(IDMovieInfo, "div.infobar meta[itemprop='contentRating']") %>% html_attr("content")
        
        ##Get Viewer rating
        if (length(html_nodes(IDMovieInfo, ".star-box-giga-star") %>% html_text) == 0) {
                IDViewrating<-NA_character_
        }
        else
                IDViewrating <-
                html_nodes(IDMovieInfo, ".star-box-giga-star") %>% html_text
        
        ##Get run time
        if (length(html_nodes(IDMovieInfo, "time") %>% html_attr("datetime")) == 0) {
                IDruntime<-NA_character_
        }
        else
                IDruntime<-html_nodes(IDMovieInfo, "time") %>% html_attr("datetime")
                IDruntime <- gsub("PT","",IDruntime[1])
        
        
        ##Get Genre
        if (length(html_nodes(IDMovieInfo,"a span[itemprop='genre']") %>% html_text) == 0) {
                IDgenre<-NA_character_
        }
        else
                IDgenre <-
                html_nodes(IDMovieInfo,"a span[itemprop='genre']") %>% html_text
        
        
        ##Get budget
        ## IDbudget<-html_nodes(IDMovieInfo,"div.txt-block h4.inline") %>% html_text
        ##Get Total Gross Box Office Sales
        ##IDBoxSales<-html_nodes(IDMovieInfo,"div.txt-block h4.inline")
        ##Get Top billed cast.
        ##IDActers<-html_nodes(IDMovieInfo, "")
        
        IDMovie.df <-
                data.frame(
                        Title = IDTitle, RelDate = IDRelDate, MPAArating = str_trim(IDMPAArating), Viewrating =
                                str_trim(IDViewrating), runtime = IDruntime, genre = paste(IDgenre,collapse =
                                                                                                   ","),stringsAsFactors = FALSE
                )
        
        return(IDMovie.df)
}




ListTitles <- function(x) {
        titles <- character(length(AllMovieInfo))
        for (g in 1:length(x)) {
                titles[g] <- substr(as.character(x[g]),65,73)
                
        }
        return(titles)##Store the list as a bunch of IMDB movie title IDs.
}

##Create list of title IDs to index list of Movie pages to scrape
##This takes about 6 minutes
FulltitleList <- list()
for (h in 1:NumPages) {
        PageInfo <-
                read_html(
                        paste0(
                                "http://www.imdb.com/search/title?sort=moviemeter,asc&start=",((h - 1) *
                                                                                                       50) + 1,"&title_type=feature&year=2015,2015",sep =
                                        "",collapse = ""
                        )
                )
        AllMovieInfo <- html_nodes(PageInfo,"td.title")
        titleList <- ListTitles(AllMovieInfo)
        FulltitleList <-
                append(FulltitleList,titleList, after = length(FulltitleList))
}



#3)Pull up the webpage associated with each title ID and pull out the information
AllInfo <- data.frame()
for (g in 1:length(FulltitleList)) {
        TitleInfo <- MyIMDBfun(FulltitleList[g])
        AllInfo <- rbind(AllInfo,TitleInfo)
}


#Set up the data 
#MPAArating as factors:
AllInfo$MPAArating<-factor(AllInfo$MPAArating)

#Viewrating as numbers
AllInfo$Viewrating<-as.numeric(AllInfo$Viewrating)

#runtime values as numerics:
AllInfo$runtime<- as.numeric(unlist(strsplit(AllInfo$runtime,split='M', fixed=TRUE)))


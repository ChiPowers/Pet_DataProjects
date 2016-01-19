MyIMDBfun <- function(ID) {
        IDMovie.df <- data.frame()
        URL <- paste0("http://www.imdb.com/title/", ID)
        IDMovieInfo <- read_html(URL)
        
        ##Get Movie Title
        IDTitle <-
                html_nodes(IDMovieInfo, ".header .itemprop") %>% html_text
        
        
        ##Get Release Date
        IDRelDate <-
                html_nodes(IDMovieInfo, ".infobar .nobr a") %>% html_text
        IDRelDate <-
                as.Date(gsub(" ","",substr(IDRelDate,1,18)),"%d%B%Y")
        if (length(IDRelDate) == 0) {
                is.na <- IDRelDate
                nrows[2] = 1
        }
        
        ##Get MPAA rating
        IDMPAArating <-
                html_nodes(IDMovieInfo, "div.infobar meta[itemprop='contentRating']") %>% html_attr("content")
        if (length(IDMPAArating) == 0) {
                IDMPAArating <- "No rating"
        }
        ##Get Viewer rating
        IDViewrating <-
                html_nodes(IDMovieInfo, ".star-box-giga-star") %>% html_text
        if (length(IDViewrating) == 0) {
                is.na <- IDViewrating
                nrows[4] = 1
        }
        
        ##Get run time
        IDruntime <-
                html_nodes(IDMovieInfo, "time") %>% html_attr("datetime")
        if (length(IDruntime) == 0) {
                is.na <- IDruntime
                nrows[5] = 1
        }
        
        ##Get Genre
        IDgenre <-
                html_nodes(IDMovieInfo,"a span[itemprop='genre']") %>% html_text
        if (length(IDgenre) == 0) {
                is.na <- IDgenre
                nrows[6] = 1
        }
        
        ##Get budget
        ## IDbudget<-html_nodes(IDMovieInfo,"div.txt-block h4.inline") %>% html_text
        ##Get Total Gross Box Office Sales
        ##IDBoxSales<-html_nodes(IDMovieInfo,"div.txt-block h4.inline")
        ##Get Top billed cast.
        ##IDActers<-html_nodes(IDMovieInfo, "")
        
        IDMovie.df <-
                data.frame(
                        Title = IDTitle, RelDate = IDRelDate, MPAArating = str_trim(IDMPAArating), Viewrating =
                                str_trim(IDViewrating), runtime = gsub("PT","",IDruntime[1]), genre = paste(IDgenre,collapse =
                                                                                                                    ","),stringsAsFactors = FALSE
                )
        
        return(IDMovie.df)
}

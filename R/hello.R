# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

library(feedeR)

# code at https://github.com/DataWookie/feedeR/blob/master/R/read.R
# blog at https://www.r-bloggers.com/feeder-reading-rss-and-atom-feeds-from-r/


# Some URLs
guardian.url <- "https://www.theguardian.com/world/rss"
slashdot.url <- "http://rss.slashdot.org/Slashdot/slashdot"
badscience.url <- "http://www.badscience.net/feed/"

feed.extract.with.error.handling <- function(url){
    # return a list of 5 if success, otherwise a list of 1 "status:ERR"
    res <- list()
    tryCatch({
        res <- feed.extract(url)
        res$status <- "OK"
        return(res)
    }, error = function(err){
        # print(paste("Error in extracting feed:", err))
        res <- list(status="ERR")
        return(res)
    },
    warning = function(war){
        # print(paste("Warning in extracting feed:", war))
        res <- list(status="ERR")
        return(res)
    },
    finally = {
        # return(res)
    })
}

top.entries <- function(feed, number.of.entries = 5){

}
urls <- data.frame(name = c("guardian",
                            "slashdot",
                            "bad.science"),
                   url = c(guardian.url,
                           slashdot.url,
                           badscience.url))


distract <- function(){
    # interactive function that ask user what he wants to see and returns a df
    # with date/time and entry title

    input.prompt <- paste0("Which feed you'd like to visualize:\n",
                           "1. The Guardian\n",
                           "2. Slashdot\n",
                           "3. Bad Science\n\n")
    ch <- as.integer(readline(input.prompt))
    feed.extract.with.error.handling(as.character(urls[ch,"url"]))
}

u <- "https://www.theguardian.com/world/rss"

# r <- feed.extract.with.error.handling(urls[1,2])

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# this to provide some functions to help you distract from your focused task
# but avoiding the healthy habit to walk away from the pc screen, by providing the latest headlines
# before you need to turn on the browser (that may have been blocked by a productivity software)

# TODO
# put links in external file

distractions <- new.env(parent = emptyenv())

library(feedeR)
library(clipr)

# code at https://github.com/DataWookie/feedeR/blob/master/R/read.R
# blog at https://www.r-bloggers.com/feeder-reading-rss-and-atom-feeds-from-r/

# Some URLs
guardian.url <- "https://www.theguardian.com/world/rss"
slashdot.url <- "http://rss.slashdot.org/Slashdot/slashdot"
badscience.url <- "http://www.badscience.net/feed/"
rbloggers.url <- "https://feeds.feedburner.com/RBloggers"
rjobs.url <- "https://www.r-users.com/feed/?rss_featured=1"

feed.extract.with.error.handling <- function(url){
    # return a list of 5 if success, otherwise a list of 1 "status:ERR"

    res <- list()
    # error handling: if the rss call is not succesfull, return an error list
    tryCatch({
        res <- feed.extract(url)
        res$status <- "OK"
        distractions$last.feed <- res
        return(res)
    }, error = function(err){
        res <- list(status="ERR", level="Error")
        return(res)
    },
    warning = function(war){
        res <- list(status="ERR", level="Warning")
        return(res)
    },
    finally = {
        # return(res)
    })
}

top.entries <- function(url, number.of.entries = 5){
    # get data from web
    parsed <- feed.extract.with.error.handling(url)
    # if error, exit
    if (parsed$status == "ERR") return("Error in parsing the feed")
    # keep only item list
    res <- data.frame(parsed$items[,1],
                      parsed$items[,2],
                      parsed$items[,3])
    names(res) <- c("what","when","links")
    # sort by date/time
    res <- res[order(res[,"when"], decreasing = TRUE),]
    # limit to number of entries asked
    res <- res[1:number.of.entries,]
    # store for future use
    distractions$last.entries <- res
    # keep only date and content
    res <- res[,c("what","when")]
    # return
    return(res)
}

distract <- function(){
    # interactive function that ask user what he wants to see and returns a df
    # with date/time and entry title
    urls <- data.frame(name = c("guardian",
                                "slashdot",
                                "bad science",
                                "r-bloggers",
                                "r users jobs"),
                       url = c(guardian.url,
                               slashdot.url,
                               badscience.url,
                               rbloggers.url,
                               rjobs.url))
    input.prompt <- paste0("Which feed you'd like to visualize:\n",
                           "1. The Guardian\n",
                           "2. Slashdot\n",
                           "3. Bad Science\n",
                           "4. R-bloggers\n",
                           "5. R jobs\n")
    ch <- as.integer(readline(input.prompt))
    res <- top.entries(as.character(urls[ch,"url"]))
    print(res, row.names = FALSE)
    print("Type 'links()' to get links")
}

links <- function(entry.n = NULL){
    if (is.null(entry.n)){
        print(distractions$last.entries)
    } else {
        print(distractions$last.entries[entry.n,])
    }
    print("Use 'copy(n) to copy hyperlink n to system clipboard")
}

copy <- function(entry.n = 1){
    d <- distractions$last.entries[entry.n, "links"]
    print(d)
    print("Link copied to system clipboard")
    write_clip(d)
}

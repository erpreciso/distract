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
# save output somewhere so the user can then get the hyperlink to copy/paste
# put links in external file
#


library(feedeR)

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
                      parsed$items[,2])
    names(res) <- c("what","when")
    # sort by date/time
    res <- res[order(res[,"when"], decreasing = TRUE),]
    # limit to number of entries asked
    res <- res[1:number.of.entries,]
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
}

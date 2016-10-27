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
# warning if save with id alreayd in


distractions <- new.env(parent = emptyenv())

# library(feedeR)
# library(clipr)
# library(rjson)

feed.extract.with.error.handling <- function(url){
    # return a list of 5 if success, otherwise a list of 1 "status:ERR"

    res <- list()
    # error handling: if the rss call is not succesfull, return an error list
    tryCatch({
        res <- feedeR::feed.extract(url)
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

get.urls <- function(){
    return(read.table("urls.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE))
}

distract <- function(){
    # interactive function that ask user what he wants to see and returns a df
    # with date/time and entry title
    urls <- get.urls()
    print(urls)
    input.prompt <- paste0("Which feed you'd like to visualize: ")
    ch <- as.integer(readline(input.prompt))
    res <- top.entries(as.character(urls[ch,"url"]))
    print(res, row.names = FALSE)
    print("Type 'links()' to get links")
}

save.rss.feed <- function(id, name, url, category){
    x <- data.frame(id=id,name=name,url=url,category=category)
    u <- get.urls()
    write.feed.file(rbind(u,x))
}

write.feed.file <- function(dataframe){
    write.table(dataframe,"urls.txt", sep = ",")
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

# save.rss.feed("universetoday","Universe Today","http://www.universetoday.com/feed/","space")

remove.rss <- function(id){
    urls <- get.urls()
    new.urls <- urls[!grepl(id, urls$id),]
    write.feed.file(new.urls)
}

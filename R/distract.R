
# TODO
# add NASA APOD
# warning if save with id alreayd in
# remove interactivity from distract
# reformat code with formatR::tidy_dir("R")

distractions <- new.env(parent = emptyenv())

#' Feed reader with a custom error handling.
#'
#' Try to parse the url, and if it encounters an error, returns a list with an
#' unique value 'status: ERR'
#'
#' @inheritParams feedeR::feed.extract
#' @return List with parsed feed, or status: ERR
#'
feed.extract.with.error.handling <- function(url) {
    # return a list of 5 if success, otherwise a list of 1 'status:ERR'
    res <- list()
    # error handling: if the rss call is not succesfull, return an error list
    tryCatch({
        res <- feedeR::feed.extract(url)
        res$status <- "OK"
        distractions$last.feed <- res
        return(res)
    }, error = function(err) {
        res <- list(status = "ERR", level = "Error")
        return(res)
    }, warning = function(war) {
        res <- list(status = "ERR", level = "Warning")
        return(res)
    }, finally = {
        # return(res)
    })
}

top.entries <- function(url, number.of.entries = 5) {
    # get data from web
    parsed <- feed.extract.with.error.handling(url)
    # if error, exit
    if (parsed$status == "ERR")
        return("Error in parsing the feed")
    # keep only item list
    res <- data.frame(parsed$items[, 1], parsed$items[, 2], parsed$items[, 3])
    names(res) <- c("what", "when", "links")
    # sort by date/time
    res <- res[order(res[, "when"], decreasing = TRUE), ]
    # limit to number of entries asked
    res <- res[1:number.of.entries, ]
    # store for future use
    distractions$last.entries <- res
    # keep only date and content
    res <- res[, c("what", "when")]
    # return
    return(res)
}

#' Get available feeds, or feed of a preselected one.
#'
#' \code{distract()} given with no input displays the list
#' of available feeds. Otherwise, passing the feed number, returns
#' the feed.
#'
#' @param feed.number row number of the desired feed
#' @return List of feeds from the chosen source
#' @examples
#' distract()
#' distract(1)
distract <- function(feed.number = NULL) {
    if (is.null(feed.number)){
        return(paste0("Have a look at urls already available with saved.feeds,",
                     " then distract(n) with n as the feed number"))
    }
    if (!is.numeric(feed.number)) {
        stop("enter a numeric value for the feed number")
    } else if (feed.number > nrow(saved.feeds)){
        msg <- paste0("feed not existing: enter a number from 1 to ", nrow(saved.feeds))
        stop(msg)
    } else {
        res <- top.entries(as.character(saved.feeds[feed.number, "url"]))
        print(res, row.names = FALSE)
        if (!grepl("Error", res)) {
            print("Type 'links()' to get links")
        }
    }
}

save.rss.feed <- function(id, name, url, category) {
    x <- data.frame(id = id, name = name, url = url, category = category)
    u <- saved.feeds
    write.feed.file(rbind(u, x))
}

# write.feed.file <- function(dataframe) {
    # write.table(dataframe, "urls.txt", sep = ",")
# }

links <- function(entry.n = NULL) {
    if (is.null(entry.n)) {
        print(distractions$last.entries)
    } else {
        print(distractions$last.entries[entry.n, ])
    }
    print("Use 'copy(n) to copy hyperlink n to system clipboard")
}

copy <- function(entry.n = 1) {
    d <- distractions$last.entries[entry.n, "links"]
    print(d)
    print("Link copied to system clipboard")
    write_clip(d)
}

# save.rss.feed('universetoday','Universe
# Today','http://www.universetoday.com/feed/','space')

# remove.rss <- function(id) {
    # urls <- saved.feeds
    # new.urls <- urls[!grepl(id, urls$id), ]
    # write.feed.file(new.urls)
# }

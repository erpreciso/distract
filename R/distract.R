
# TODO
# add NASA APOD
# reformat code with formatR::tidy_dir("R")

globalVariables("feeds")
.onLoad <- function(libname, pkgname){
    distractions <<- new.env(parent = emptyenv())
}

.onAttach <- function(libname, pkgname){
    packageStartupMessage("Distract yourself")
}

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

#' Return most recent entries from a feed.
#'
#' Given a url, it parses the feed and creates a dataframe, then it sorts by
#' date / time and return the most recent entries. It saves a complete dataframe
#' in local storage, and returns just title and date / time.
#'
#' @inheritParams feedeR::feed.extract
#' @param number.of.entries number of entries to return
#' @return a dataframe with title of each entry (what) and its date / time
#' @examples
#' \dontrun{top.entries("https://feeds.feedburner.com/RBloggers", 3)}
top.entries <- function(url, number.of.entries = 5) {
    if (is.null(url)) stop("enter a url")
    if (!RCurl::url.exists(url)) stop("url not valid")
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
    # return only date and content
    return(res[, c("what", "when")])
}

#' Get available feeds, or feed of a preselected one.
#'
#' \code{distract()} given with no input displays the list
#' of available feeds. Otherwise, passing the feed number, returns
#' the feed.
#'
#' @export
#' @param feed.number row number of the desired feed
#' @return List of feeds from the chosen source
#' @examples
#' distract()
#' distract(1)
distract <- function(feed.number = NULL) {
    if (is.null(feed.number)){
        return(paste0("Have a look at urls already available with feeds,",
                     " then distract(n) with n as the feed number"))
    }
    if (!is.numeric(feed.number)) {
        stop("enter a numeric value for the feed number")
    } else if (feed.number > nrow(feeds)){
        msg <- paste0("feed not existing: enter a number from 1 to ", nrow(feeds))
        stop(msg)
    } else {
        res <- top.entries(as.character(feeds[feed.number, "url"]))
        print(res, row.names = FALSE)
        if (length(res) == 2) {
            print("Type 'links()' to get links")
        }
    }
}

#' Save a favourite feed in local env.
#'
#' If you want to add a url to the predefined list of feeds, pass it to this
#' function and it will be available to \code{distract()}.
#'
#' @export
#' @param name Name of the feed
#' @param url URL
#' @return "Saved" if succesfull, otherwise error message
save.rss.feed <- function(name, url) {
    feeds <- NULL
    x <- data.frame(name = name, url = url)
    feeds <<- rbind(feeds, x)
    return("Saved")
}

#' Remove a feed.
#'
#' Remove the n-th entry from the feed list.
#'
#' @export
#' @param feed.index index of the feed to be removed
#' @return "Removed" if success
#' @examples
#' \dontrun{
#' remove.rss.feed(1)
#' }
remove.rss.feed <- function(feed.index) {
    feeds <- NULL
    feeds <<- feeds[-feed.index,]
    return("Removed")
}

#' Return last entries retrieved.
#'
#' \code{distract(n)} returns a list of entries from the n-th feed in the feed
#' list. \code{links} provide the associated links (of the entire list or, if k
#' is given, of the k-th entry). When the links are then shown, you can use
#' \code{copy(k)} to copy the k-th entry in your system's clipboard, to be used
#' in your favourite browser.
#'
#' @export
#' @param entry.n Index of the entry you want to retrieve the link.
#' @return Links for each entry got from \code{distract()}
#' @examples
#' \dontrun{
#' links(1)
#' }
#' \dontrun{
#' links()
#' }
links <- function(entry.n = NULL) {
    if (!is.null(entry.n) & !is.numeric(entry.n)) stop("enter a valid entry number")
    if (is.null(distractions$last.entries)){
        stop("no entries get yet. Use distract()")
    }
    if (is.null(entry.n)) {
        e <- distractions$last.entries
    } else if (is.numeric(entry.n)){
        n <- nrow(distractions$last.entries)
        if (entry.n > n){
            m <- paste0("enter an entry number from 1 to ", n)
            stop(m)
        }
        e <- distractions$last.entries[entry.n, ]
    }
    print(e)
    return("Use 'copy(n) to copy hyperlink n to system clipboard")
}

#' Copy the selected entry link in system cliboard.
#'
#' Given an entry number, \code{copy()} copies the link of the entry in system
#' clipboard, to be used with your favourite web browser.
#'
#' @export
#' @param entry.n Number of the entry to copy. Get it from the \code{links} or
#'   the \code{distract} commands.
#' @return the link is copied in system clipboard, and print the copied link.
#' @examples
#' \dontrun{
#' copy(1)
#' }
copy <- function(entry.n = 1) {
    if (is.null(distractions$last.entries)){
        stop("no entries get yet. Use distract()")
    }
    d <- as.character(distractions$last.entries[entry.n, "links"])
    print(d)
    # clipr::write_clip(d)
    tryCatch({
        clipr::write_clip(d)
        return("Link copied to system clipboard")
    }, error = function(err) {
        return("Error: link not copied to system clipboard")
    }, warning = function(war) {
        return("Error: link not copied to system clipboard")
    }, finally = {
    })
}

## Retrieve data from Content Service




#' Person queries
#' @param name Person’s first or/and last name
#' @param id Person's OrcidId or DbId
#' @param attributes Property for a person
#' @param host reactome 
#' @param path path to PERSON
#' @return a list of requested information
#' @examples
#' getPerson(name="Steve Jupe", attributes=c("displayName", "publications"))
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @rdname getPerson
#' @export 

getPerson <- function(name=NULL, id=NULL, attributes=NULL, 
                      host="https://reactome.org", path="ContentService/data/person") {
  # ensure the input
  if (is.null(name) & is.null(id)) stop("Must specify either a name or a ID.")
  
  # choose an id if name input only
  if (is.null(id)) {
    cat(paste0('Matching "', name, '" with names in current data...'), "\n")
    names.df <- .listPeople(name)
    if (nrow(names.df) == 1) {
      id <- as.character(names.df$dbId)
    } else {
      print(names.df, row.names=FALSE)
      id <- readline(prompt="Enter the matched dbId: ")
      while (!id %in% names.df$dbId) {
        id <- readline(prompt="Wrong dbId, re-enter: ")
      }
    }
  } 
  
  if (is.null(attributes)) {
    # retrieve all info by default
    url <- file.path(host, path, id)
    
    res <- GET(file.path(host, path, id))
    all.info <- fromJSON(content(res, 'text'))
    
    #### add authoredPathways or not? ####
    ap.url <- file.path(url, "authoredPathways")
    all.info[["authoredPathways"]] <- fromJSON(content(GET(ap.url),'text'))
  } else {
    # retrieve specified properties
    all.info <- list(Id=id)
    for (a in attributes) {
      tmp.url <- file.path(host, path, id, a)
      tmp <- content(GET(tmp.url), "parse")
      ifelse(is.character(tmp), all.info[[a]] <- tmp, 
             all.info[[a]] <- fromJSON(content(GET(tmp.url), "text")))
    }
  }
  all.info
}


.listPeople <- function(name, host="https://reactome.org", 
                        path="ContentService/data/people/name", ...) {
  # modify the name and write full URL
  url <- ifelse(grepl("\\s", name),
                file.path(host, path, tolower(gsub(" ", "%20", name)), "exact"),
                file.path(host, path, tolower(name))
  )
  
  res <- tryCatch(GET(url, ...), error = function(e) message("Reactome is not responding"))
  
  if (status_code(res) == "404") {
    stop(paste0(fromJSON(content(res, "text"))[["messages"]], ", enter another one."))
  }
  names.df <- fromJSON(content(res, "text"))
  names.df[ ,c(1:5)]
}

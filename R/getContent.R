## Retrieve data from Content Service




#### Person queries
#' @param name Person’s first or/and last name
#' @param id Person's OrcidId or DbId
#' @param attributes Property for a specific person’s
#' @param host reactome 
#' @param path path to person
#' @return a list of requested information
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export 

getRtPerson <- function(name=NULL, id=NULL, attributes=NULL, 
                        host="https://reactome.org", path="ContentService/data/person") {
  # ensure the input
  if (is.null(name) & is.null(id)) stop("Must specify either a name or a ID.")
  
  # choose an id if name input only
  if (is.null(id)) {
    cat(paste0('Matching "', name, '" with names in current data...'), "\n")
    names.df <- .listPeople(name)
    print(names.df, row.names=FALSE)
    
    id <- readline(prompt="Enter the matched dbId: ")
    while (!id %in% names.df$dbId) {
      id <- readline(prompt="Wrong dbId, re-enter: ")
    }
  } 
  
  if (is.null(attributes)) {
    # retrieve all info by default
    url <- file.path(host, path, id)
    ap.url <- file.path(url, "authoredPathways")
    
    res <- GET(file.path(host, path, id))
    all.info <- fromJSON(content(res, 'text'))
    
    #### add authoredPathways or not? ####
    ap <- fromJSON(content(GET(ap.url),'text'))
    all.info[["authoredPathways"]] <- ap
  } else {
    # retrieve specified properties
    all.info <- list(Id=id)
    for (a in attributes) {
      tmp.url <- file.path(host, path, id, a)
      tmp <- content(GET(tmp.url), "text")
      all.info[[a]] <- ifelse(is.character(tmp), tmp, fromJSON(tmp))
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

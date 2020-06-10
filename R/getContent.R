## Retrieve data from Content Service




#' Person queries
#' @param name Person’s first or/and last name
#' @param id Person's OrcidId or DbId
#' @param attributes Property for a person. Retrieve all available attributes if it is not specified.
#' @return a list of requested information
#' @examples
#' getPerson(name="Robin Haw", attributes=c("displayName", "affiliation"))
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @rdname getPerson
#' @export 

getPerson <- function(name=NULL, id=NULL, attributes=NULL) {
  # ensure the input
  if (is.null(name) & is.null(id)) stop("Must specify either a name or an ID.")
  
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
        id <- readline(prompt=paste0(id, " is not in IDs above, re-enter: "))
      }
    }
  } 
  
  # retrieve person's information
  path <- "data/person"
  if (is.null(attributes)) {
    # retrieve all info by default
    url <- file.path(getOption("base.address"), path, id)
    all.info <- fromJSON(content(GET(url), 'text'))
    
    # add authored pathways if any
    ap.url <- file.path(url, "authoredPathways")
    authoredPathways <- fromJSON(content(GET(ap.url),'text'))
    if (length(authoredPathways) != 0) all.info[["authoredPathways"]] <- authoredPathways
  } else {
    # retrieve specified properties
    all.info <- list(Id=id)
    for (attribute in attributes) {
      tmp.url <- file.path(getOption("base.address"), path, id, attribute)
      tmp <- content(GET(tmp.url), "parse")
      ifelse(is.character(tmp), 
             all.info[[attribute]] <- tmp, 
             all.info[[attribute]] <- fromJSON(content(GET(tmp.url), "text")))
    }
  }
  all.info
}

.listPeople <- function(name, ...) {
  path <- "data/people/name"
  # modify the name and write full URL
  url <- ifelse(grepl("\\s", name),
                file.path(getOption("base.address"), path, tolower(gsub(" ", "%20", name)), "exact"),
                file.path(getOption("base.address"), path, tolower(name)))
  
  res <- tryCatch(GET(url, ...), error = function(e) message("Reactome is not responding"))
  .checkStatus(res)
  
  names.df <- fromJSON(content(res, "text"))
  names.df[ ,c(1:5)]
}



#' List the whole Reactome search items (species, types, compartments, keywords)
#' @param what categories of query
#' @param facet return faceting information or not
#' @return all available search items
#' @examples
#' listSearchItems(what=c("species", "keyword"))
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname listSearchItems
#' @export

listSearchItems <- function(what=c("all", "species", "type", "compartment", "keyword"),
                            facet=FALSE) {
  path <- "search/facet"
  
  # ensure inputs
  what <- match.arg(what, several.ok = TRUE)
  print(what)
  
  # retrieve
  res <- GET(file.path(getOption("base.address"), path))
  .checkStatus(res)
  list <- fromJSON(content(res, "text"))
  
  # filter
  ifelse("all" %in% what,
         select.name <- names(list),
         select.name <- c(names(list)[1], names(list)[gsub("Facet$", "", names(list)) %in% what]))
  
  final.list <- list[select.name]
  final.list <- lapply(final.list, function(x) if (inherits(x, "list")) {x[["available"]]} else {x})
  
  # modify final return
  if (!facet) {
    # remove the counts
    final.list <- final.list[sapply(final.list, function(x) inherits(x, "data.frame"))]
    final.list <- lapply(final.list, function(x) x$name)
    names(final.list) <- gsub("Facet$", "", names(final.list))
  }
  final.list
  ## originally I just want to retrieve all available items for c("species", "types", 
  ## "compartments", "keywords"), a little uncertain whether the faceting information include all terms already 
  ## since the number of species here is fewer than that from `getSpecies()`
}



#' Search query
#' @param query search term
#' @param filters filter conditions
#' @param cluster cluter returned data or not
#' @param range start row and the nubmer of rows to include
#' @return a list of information about the search term
#' @examples
#' searchQuery("Biological oxidation", 
#' filters=c(species="Mus musculus", types="", compartments="", keywords=""), 
#' range=c(0,20))
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname searchQuery
#' @export 

searchQuery <- function(query, filters=c(species="", types="", 
                                         compartments="", keywords=""), 
                        cluster=TRUE, range=NULL) {
  # write full url
  path <- "search/query"
  query <- gsub("\\s", "%20", query)
  url <- file.path(getOption("base.address"), paste0(path, "?query=", query))
  
  for (filter in names(filters)) {
    url <- paste0(url, "&", filter, "=", gsub("\\s", "%20", filters[filter]))
  }
  url <- paste0(url, "&cluster=", tolower(cluster))
  if (!is.null(range)) url <- paste0(url, "&Start%20row=", range[1], "&rows=", range[2])
  
  # retrieve
  res <- GET(url)
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' Species queries
#' @param main determine whether return main species, which are those have either manually curated or computationally inferred pathways
#' @return a dataframe of species information
#' @examples
#' getSpecies(main=TRUE)
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname getSpecies
#' @export 

getSpecies <- function(main=FALSE) {
  path <- "data/species"
  # write the url
  url <- ifelse(main, 
                file.path(getOption("base.address"), path, "main"),
                file.path(getOption("base.address"), path, "all"))
  
  res <- GET(url)
  .checkStatus(res)
  
  fromJSON(content(res, "text"))
}



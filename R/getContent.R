## Retrieve data from Content Service


#' Search engines discovery schema
#' @param id an event identifier
#' @return a list of the event schema
#' @examples
#' discover("R-HSA-73893")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname discover
#' @export 

discover <- function(id) {
  ### any way to ensure it's an event id? ###
  path <- "data/discover"
  res <- GET(file.path(getOption("base.address"), path, id))
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' PhysicalEntity queries
#' @param id physical entity id or id from resources other than Reactome
#' @param retrieval entities to be retrieved
#' @param resource resource other than Reactome, e.g. UniProt, Ensembl
#' @param subunitsExcludeStructures whether contained complexes and entity sets are excluded when retrieving subunits
#' @return a dataframe containing requested information
#' @examples
#' getEntities("R-HSA-5674003", "subunits")
#' getEntities("P00533", "complexes", "UniProt")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname getEntities
#' @export 

getEntities <- function(id, retrieval=c("subunits", "complexes", "componentOf", "otherForms"),
                        resource="Reactome", subunitsExcludeStructures=TRUE) {
  # check the inputs
  if (length(retrieval) > 1) stop("Specify one kind of retrieval")
  retrieval <- match.arg(retrieval)
  if (retrieval == "complexes") {
    if (resource == "Reactome") stop("Please use an id from other resources to retrieve complexes")
  } else {
    if (resource != "Reactome") stop(paste0("Please use Reactome as resource and Reactome stable or db id"))
  }
  
  # retrieve
  if (retrieval == "subunits") {
    # id is stable or db id
    url <- file.path(getOption("base.address"), "data/complex", id, 
                     paste0("subunits?excludeStructures=", tolower(subunitsExcludeStructures)))
  } else if (retrieval == "complexes") {
    url <- file.path(getOption("base.address"), "data/complexes", resource, id)
  } else {
    url <- file.path(getOption("base.address"), "data/entity", id, retrieval)
  }
  res <- GET(url)
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' EventsHierarchy queries
#' @param species species name or taxon id
#' @return a nested dataframe containing full event hierarchy for any given species
#' @examples
#' getEventsHierarchy("Caenorhabditis elegans")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname getEventsHierarchy
#' @export 

getEventsHierarchy <- function(species) {
  path <- "data/eventsHierarchy"
  url <- file.path(getOption("base.address"), path, gsub("\\s", "%20", species))
  res <- GET(url)
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' Format exporter for events
#' @param event.id a stable or db id of a pathway or reaction
#' @param format either in sbgn (Systems Biology Graphical Notation) or sbml (Systems Biology Markup Language)
#' @return content of sbgn or sbml
#' @examples
#' exportEvent("R-HSA-68616", "sbml")
#' @importFrom httr GET content
#' @rdname exportEvent
#' @export 

exportEvent <- function(event.id, format=c("sbgn", "sbml")) {
  path <- "exporter/event"
  format <- match.arg(format, several.ok=FALSE)
  url <- file.path(getOption("base.address"), path, paste0(event.id, ".", format))
  res <- GET(url)
  .checkStatus(res)
  content(res, "text")
}



#' Mapping related queries
#' @param id a stable or db id of an event or entity
#' @param resource database name other than Reactome
#' @param species species name or taxon id or dbId
#' @param mapTo retrieve pathways or reactions where an identifier can be mapped to
#' @return a dataframe containing requested pathways or reactions
#' @examples
#' getMapping("Q7Z569", "GeneCards", "9606", "reactions")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname getMapping
#' @export 

getMapping <- function(id, resource, species, mapTo=c("pathways", "reactions")) {
  path <- "data/mapping"
  mapTo <- match.arg(mapTo, several.ok=FALSE)
  url <- file.path(getOption("base.address"), path, resource, 
                   id, paste0(mapTo, "?species=", gsub("\\s", "%20", species)))
  res <- GET(url)
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' Orthology related queries
#' @param id a stable or db id of an event or entity
#' @param species.id the taxon id for a species
#' @return a dataframe containing requested participants
#' @examples
#' getOrthology("R-HSA-5674003", "49633")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname getOrthology
#' @export 

getOrthology <- function(id, species.id) {
  path <- "data/orthology"
  url <- file.path(getOption("base.address"), path, id, "species", species.id)
  res <- GET(url)
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' Participants queries
#' @param event.id a stable or db id of an Event
#' @param class retrieve all participants or PhysicalEntities or referenceEntities
#' @return a dataframe containing requested participants
#' @examples
#' getParticipants("5205685", "physicalEntities")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname getParticipants
#' @export 

getParticipants <- function(event.id, class=c("all", "physicalEntities", "referenceEntities")) {
  path <- "data/participants"
  
  # write url
  url <- file.path(getOption("base.address"), path, event.id)
  class <- match.arg(class, several.ok = FALSE)
  if (class == "physicalEntities") {
    url <- file.path(url, "participatingPhysicalEntities")
  } else if (class == "referenceEntities") {
    url <- file.path(url, "referenceEntities")
  }
  
  # retrieve
  res <- GET(url)
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' Pathway related queries
#' @param id a stable or db id of a PhysicalEntity or Event present in the pathways
#' @param species species name or taxon id
#' @param allForms whether to return all low level pathwyas that contain the given PhysicalEntity (not Event) in all forms
#' @return a dataframe containing requested low level pathways
#' @examples
#' getPathways("199420", "Homo sapiens")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname getPathways
#' @export 

getPathways <- function(id, species=NULL, allForms=FALSE) {
  path <- "data/pathways/low/entity"
  # write the full url
  url <- file.path(getOption("base.address"), path, id)
  if (allForms) url <- file.path(url, "allForms")
  if (!is.null(species)) url <- paste0(url, "?species=", gsub("\\s", "%20", species))
  
  # retrieve
  res <- GET(url)
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' Person queries
#' @param name Person’s first or/and last name
#' @param id Person's OrcidId or DbId
#' @param attributes Property for a person. Retrieve all available attributes if it is not specified.
#' @return a list of requested information
#' @examples
#' getPerson(name="Robin Haw", attributes=c("displayName", "affiliation"))
#' @importFrom httr GET content
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

# list people in Reactome with partly or full name
.listPeople <- function(name, ...) {
  path <- "data/people/name"
  # modify the name and write full URL
  url <- ifelse(grepl("\\s", name),
                file.path(getOption("base.address"), path, tolower(gsub("\\s", "%20", name)), "exact"),
                file.path(getOption("base.address"), path, tolower(name)))
  
  res <- tryCatch(GET(url, ...), error = function(e) message("Reactome is not responding"))
  .checkStatus(res)
  
  names.df <- fromJSON(content(res, "text"))
  names.df[ ,c(1:5)]
}



#' List the whole Reactome search items (species, types, compartments, keywords)
#' @param items categories of query
#' @param facet return faceting information or not
#' @return all available search items
#' @examples
#' listSearchItems(items=c("species", "keyword"))
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname listSearchItems
#' @export

listSearchItems <- function(items=c("all", "species", "type", "compartment", "keyword"),
                            facet=FALSE) {
  path <- "search/facet"
  
  # ensure inputs
  items <- match.arg(items, several.ok = TRUE)
  
  # retrieve
  res <- GET(file.path(getOption("base.address"), path))
  .checkStatus(res)
  list <- fromJSON(content(res, "text"))
  
  # filter
  ifelse("all" %in% items,
         select.name <- names(list),
         select.name <- c(names(list)[1], names(list)[gsub("Facet$", "", names(list)) %in% items]))
  
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
}



#' Common data retrieval
#' @param id a Reactome stable or db id
#' @return a dataframe containing all reference entities for a given id
#' @examples
#' query("R-HSA-60140")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname query
#' @export 

query <- function(id) {
  path <- "data/query/enhanced"
  res <- GET(file.path(getOption("base.address"), path, id))
  .checkStatus(res)
  fromJSON(content(res, "text"))
}



#' ReferenceEntity queries
#' @param external.id an id from external dabatases, e.g. ChEBI, UniProt
#' @return a dataframe containing all reference entities for a given id
#' @examples
#' getReferences("15377")
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @rdname getReferences
#' @export 

getReferences <- function(external.id) {
  path <- "references/mapping"
  res <- GET(file.path(getOption("base.address"), path, external.id))
  .checkStatus(res)
  fromJSON(content(res, "text"))
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
  
  ## add filters for the query
  for (filter in names(filters)) {
    url <- paste0(url, "&", filter, "=", gsub("\\s", "%20", filters[filter]))
  }
  ## cluster the returned data or not
  url <- paste0(url, "&cluster=", tolower(cluster)) 
  ## restrict rows to include
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



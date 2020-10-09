## Retrieve data from Content Service


#' Search engines discovery schema
#' @param event.id stable id or db id of an Event
#' @return a list of the event schema
#' @examples
#' discover("R-HSA-73893")
#' @rdname discover
#' @export

discover <- function(event.id) {
  path <- "data/discover"
  url <- file.path(getOption("base.address"), path, event.id)
  .retrieveData(url, as="text")
}



#' PhysicalEntity queries
#' 
#' This method retrieves instances related to PhysicalEntity. 
#' 
#' Details on options of `retrieval` arg:
#' - \strong{subunits}: retrieves subunits that constitute any given Complex
#' - \strong{complexes}: retrieves Complexes that contain the given `id` and `resource`
#' - \strong{componentOf}: retrieves structures (Complexes and Sets) that include the given Entity as their component
#' - \strong{otherForms}: retrieves Entities containing all other forms of the given PhysicalEntity
#' 
#' @param id stable or db id of a Reactome PhysicalEntity, or id from resources other than Reactome
#' @param retrieval entities to be retrieved, including "subunits", "complexes", "componentOf", "otherForms"
#' @param resource resource other than Reactome, e.g. UniProt, Ensembl
#' @param subunitsExcludeStructures whether contained Complexes and EntitySets are excluded when retrieving "subunits"
#' @return a dataframe containing requested information
#' @examples
#' getEntities("R-HSA-5674003", retrieval="subunits")
#' getEntities("P00533", retrieval="complexes", resource="UniProt")
#' @rdname getEntities
#' @family getContent
#' @export 

getEntities <- function(id, retrieval=c("subunits", "complexes", "componentOf", "otherForms"),
                        resource="Reactome", subunitsExcludeStructures=FALSE) {
  # check the inputs
  if (missing(retrieval)) {
    message("Retrieval argument not specified, retrieving 'subunits'... For 'complexes', 'componentOf', 'otherForms', specify 'retrieval'")
  }
  retrieval <- match.arg(retrieval, several.ok=FALSE)
  
  if (retrieval == "complexes" && resource == "Reactome") {
    stop("Please use an id from other resources (e.g. UniProt, Ensembl) 
         to retrieve complexes and specify the resource that the id comes from")
  }
  if (retrieval != "complexes" && resource != "Reactome") {
    stop("Please use Reactome as resource and specify a Reactome stable or db id")
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
  .retrieveData(url, as="text")
}



#' EventsHierarchy queries
#' 
#' Events (Pathways and Reactions) in Reactome are organized in a hierarchical 
#' structure for every species. By following all `hasEvent` relationships, 
#' this method retrieves the full event hierarchy for any given \strong{main species}. 
#' 
#' @param main.species name or taxon/db id or abbreviation of \strong{main species} in Reactome
#' @return a nested dataframe containing full event hierarchy for a given main species
#' @examples
#' \dontrun{
#' getEventsHierarchy("chicken")
#' getEventsHierarchy("XTR")
#' }
#' @rdname getEventsHierarchy
#' @seealso \code{\link{getSpecies}} to get the main species list
#' @family getContent
#' @export 

getEventsHierarchy <- function(main.species) {
  path <- "data/eventsHierarchy"
  taxon.id <- .matchSpecies(main.species, "taxId")
  url <- file.path(getOption("base.address"), path, taxon.id)
  .retrieveData(url, as="text")
}



#' Orthology related queries
#' 
#' This function retrieves the orthology for any given Event or Entity in the specified species. 
#' More information on inferred events see \href{here}{https://www.reactome.org/pages/documentation/electronically-inferred-events/}.
#' 
#' @param id a stable or db id of an Event or Entity
#' @param species name or taxon id or dbId or abbreviation of species
#' @return a list containing the orthology for given Event or Entity in the specified species
#' @examples
#' getOrthology("R-HSA-5674003", "pig")
#' @rdname getOrthology
#' @family getContent
#' @export 

getOrthology <- function(id, species) {
  path <- "data/orthology"
  species.name <- .matchSpecies(species, "displayName")
  species.id <- .matchSpecies(species, "dbId") #dbId only
  url <- file.path(getOption("base.address"), path, id, "species", species.id)
  # retrieve data
  cat(paste0("Returning inferred instances of ", id, " in species ", species.name, "...\n"))
  note.msg <- "Note that only orthologous Events or Entities in a different species can be retrieved"
  .retrieveData(url, customizedMsg=note.msg, as="text")
}



#' Participants related queries
#' 
#' Data in Reactome are organized in a hierarchical manner - Pathways contain Reactions, 
#' Reactions contain PhysicalEntities. This function is to get the participants 
#' of a given Event.
#' 
#' Details on options of `retrieval` arg:
#' - \strong{AllInstances}: retrieves all participants (PhysicalEntities) from a given Event and their ReferenceEntities
#' - \strong{PhysicalEntities}: retrieves all the PhysicalEntities that take part in a given Event
#' - \strong{ReferenceEntities}: retrieves the ReferenceEntities for all PhysicalEntities in every constituent Pathway/Reaction
#' - \strong{EventsInPathways}: recursively retrieves all the Events contained in any given Event
#' 
#' @param event.id a stable or db id of an Event (pathways and reactions)
#' @param retrieval participants to be retrieved, including "AllInstances", "PhysicalEntities", "ReferenceEntities", "EventsInPathways"
#' @return a dataframe containing requested participants
#' @examples
#' getParticipants("R-HSA-6804741", "AllInstances")
#' getParticipants("R-HSA-69306", "EventsInPathways")
#' @rdname getParticipants
#' @family getContent
#' @export 

getParticipants <- function(event.id, retrieval=c("AllInstances", "PhysicalEntities", 
                                                  "ReferenceEntities", "EventsInPathways")) {
  path <- "data/participants"
  
  # write url
  url <- file.path(getOption("base.address"), path, event.id) #all participants
  if (missing(retrieval)) {
    message("Retrieval argument not spcified, retrieving 'AllInstances'... For others, specify 'retrieval'")
  }
  retrieval <- match.arg(retrieval, several.ok = FALSE)
  
  msg <- NULL
  if (retrieval == "PhysicalEntities") {
    url <- file.path(url, "participatingPhysicalEntities")
  } else if (retrieval == "ReferenceEntities") {
    url <- file.path(url, "referenceEntities")
  } else if (retrieval == "EventsInPathways") {
    # in a different path/method - /data/pathway/{id}/containedEvents
    url <- file.path(getOption("base.address"), "data/pathway", event.id, "containedEvents")
    msg <- "'Events' are found in the 'hasEvent' attribute of Pathways"
  }
  
  # retrieve
  participants <- .retrieveData(url, customizedMsg=msg, as="text")
  
  # annotate instances in ReactionLikeEvents if retrieving AllInstances
  if (retrieval == "AllInstances") {
    all.info <- query(event.id)
    if (all.info[["className"]] == "Reaction") {
      # Empty columns
      participants$type <- rep(0, nrow(participants)) -> participants$numOfEntries
      
      # input/output/catalysts/regulations
      for (component in c("input", "output", "catalystActivity", "regulatedBy")) {
        sub.info <- all.info[[component]]
        # If it's a df, entries are all unique in the component;
        # if it's list, multiple entries exist
        # put dataframe into a list
        if (is.data.frame(sub.info)) sub.info <- list(sub.info)
        
        for (list in sub.info) {
          if (is.integer(list) && list %in% participants$peDbId) {
            # only an id, no other info
            participants[participants$peDbId == list, ]$numOfEntries <- participants[participants$peDbId == list, ]$numOfEntries + 1
            if (participants[participants$peDbId == list, ]$type == 0) participants[participants$peDbId == list, ]$type <- component
          } else if (is.list(list)) {
            # get the id
            if (component == "catalystActivity") {
              id <- list$physicalEntity$dbId
            } else if (component == "regulatedBy") {
              id <- list$regulator$dbId
            } else {
              id <- list$dbId
            }
            
            for (i in id) {
              if (i %in% participants$peDbId) {
                tmp.type <- participants[participants$peDbId == i, ]$type
                if (tmp.type != 0) {
                  # already has role(s)
                  participants[participants$peDbId == i, ]$type <- paste0(tmp.type, ",", component)
                } else {
                  participants[participants$peDbId == i, ]$type <- component
                  participants[participants$peDbId == i, ]$numOfEntries <- participants[participants$peDbId == i, ]$numOfEntries + 1
                }
              } 
            }
          }
        }
      }
      # rename
      participants$type <- gsub("catalystActivity", "catalyst", participants$type)
      participants$type <- gsub("regulatedBy", "regulator", participants$type)
      # rearrange the columns
      participants <- participants[ ,c("peDbId", "displayName", "schemaClass", "type", "numOfEntries", "refEntities")]
    }
  }
  participants
}



#' Pathway related queries
#' 
#' To get the Events that contain the given PhysicalEntity or Event (i.e. subpathway).
#' 
#' @param id a stable or db id of a PhysicalEntity or Event present in the pathways
#' @param species name or taxon id or dbId or abbreviation of species
#' @param allForms if set to \code{TRUE}, all low level pathways that contain the given PhysicalEntity (not Event) in all forms returned
#' @param top.level if set to \code{TRUE}, only top-level pathways returned
#' @return a dataframe containing requested pathways
#' @examples
#' getPathways("R-HSA-199420", "Homo sapiens")
#' @rdname getPathways
#' @family getContent
#' @importFrom data.table rbindlist
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @export 

getPathways <- function(id, species=NULL, allForms=FALSE, top.level=FALSE) {
  path <- "data/pathways/low/entity"
  # write the full url
  url <- file.path(getOption("base.address"), path, id)
  if (allForms) url <- file.path(url, "allForms")
  if (!is.null(species)) url <- paste0(url, "?species=", .matchSpecies(species, "taxId"))
  
  # retrieve
  pathways <- .retrieveData(url, as="text")
  
  # map to top level pathways
  if (top.level) {
    cl <- makeCluster(1)
    registerDoParallel(cl)
    
    dfcomb <- function(...) {
      rbindlist(list(...), fill = TRUE)
    }
    
    top.pathways <- foreach(id=pathways$dbId, .export=c(".retrieveData", ".checkStatus"), .combine=dfcomb) %dopar% {
      # /data/event/{id}/ancestors
      ancestors.url <- file.path(getOption("base.address"), "data/event", id, "ancestors")
      ancestors <- .retrieveData(ancestors.url, as="text")
      ancestors <- ancestors[[1]]
      ancestors[ancestors$schemaClass == "TopLevelPathway",]
    }
    stopCluster(cl)
    
    rownames(top.pathways) <- seq(1, nrow(top.pathways))
    return(top.pathways)
  } else {
    return(pathways)
  }
}



#' Person queries
#' 
#' Retrieves a specific person’s property by his/her name or OrcidId or dbId.
#' 
#' @param name Person’s first or/and last name
#' @param id Person's Orcid Id or DbId
#' @param attributes Property for a person. Return all available attributes if it is not specified.
#' @return a list of requested information
#' @examples
#' getPerson(name="Robin Haw", attributes=c("displayName", "affiliation"))
#' @rdname getPerson
#' @family getContent
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
    all.info <- .retrieveData(url, as="text")
    
    # add authored pathways if any
    ap.url <- file.path(url, "authoredPathways")
    authoredPathways <- .retrieveData(ap.url, as="text")
    if (length(authoredPathways) != 0) all.info[["authoredPathways"]] <- authoredPathways
  } else {
    # retrieve specified properties
    all.info <- list(Id=id)
    for (attribute in attributes) {
      tmp.url <- file.path(getOption("base.address"), path, id, attribute)
      tmp <- .retrieveData(tmp.url, fromJSON=FALSE, as="parse")
      ifelse(is.character(tmp), 
             all.info[[attribute]] <- tmp, 
             all.info[[attribute]] <- .retrieveData(tmp.url, fromJSON=TRUE, as="text"))
    }
  }
  all.info
}



#' List filter items
#' 
#' To list the available filtering options for `searchQuery()`, and their counts.
#' 
#' @param items categories of query, including "species", "types", "compartments", "keywords", or "all"
#' @param facet return faceting information or not
#' @return available search items
#' @examples
#' listSearchItems()
#' @rdname listSearchItems
#' @seealso \code{\link{searchQuery}} to search in Reactome
#' @export

listSearchItems <- function(items=c("all", "species", "types", "compartments", "keywords"), facet=FALSE) {
  path <- "search/facet"
  
  # ensure inputs
  if (missing(items)) message('Item argument not specified, returning all kinds of items...')
  items <- match.arg(items, several.ok = TRUE)
  
  # retrieve
  url <- file.path(getOption("base.address"), path)
  list <- .retrieveData(url, as="text")
  
  # filter
  ifelse("all" %in% items,
         select.name <- names(list),
         select.name <- c(names(list)[1], names(list)[gsub("Facet$", "", names(list)) %in% items]))
  
  final.list <- list[select.name]
  final.list <- lapply(final.list, function(x) if (inherits(x, "list")) {x[["available"]]} else {x})
  
  # modify final return
  if (!facet) {
    # remove the counts
    final.list <- final.list[vapply(final.list, function(x) inherits(x, "data.frame"), logical(1))]
    final.list <- lapply(final.list, function(x) data.frame(name=x$name))
    names(final.list) <- gsub("Facet$", "", names(final.list))
  }
  final.list
}



#' Common data retrieval
#' 
#' This function retrieves a Reactome Database object that has all its properties 
#' and direct relationships (relationships of depth 1) filled, while it also 
#' includes any second level relationships regarding regulations and catalysts.
#' 
#' @param id a stable or db id of \strong{any} Reactome entry
#' @return a list containing comprehensive information (all attributes) for a given id
#' @examples
#' query("R-HSA-60140")
#' @rdname query
#' @family getContent
#' @seealso \code{\link{searchQuery}} to search in Reactome
#' @export 

query <- function(id) {
  path <- "data/query/enhanced"
  url <- file.path(getOption("base.address"), path, id)
  .retrieveData(url, as="text")
}



#' Schema class queries
#' 
#' Fetch instances by Class. All Classes see 
#' \href{https://reactome.org/content/schema/DatabaseObject}{Reactome data schema}.
#' 
#' @param class schema class name
#' @param species name or taxon id or dbId or abbreviation of species. Only Event and PhysicalEntity classes can specify species
#' @param all to return ALL entries or not, default is \code{FALSE}
#' @param rows the number of entries retrieved, default is 1000
#' @param minimised to retrieve simplified entries (db id, stable id, displayName, type) or not, default is \code{FALSE}
#' @param reference to retrieve simplified reference objects (db id, external identifier, 
#' external database name) or not, default is \code{FALSE}. Only for ReferenceEntity or ExternalOntology class
#' @return a sorted dataframe containing entries that belong to the specified schema class
#' @examples
#' \dontrun{
#' getSchemaClass(class="Drug", all=TRUE)
#' }
#' getSchemaClass(class="Regulation", rows=20, minimised=TRUE)
#' getSchemaClass(class="Complex", species="pig", rows=10)
#' @importFrom data.table rbindlist
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @rdname getSchemaClass
#' @family getContent
#' @export

getSchemaClass <- function(class, species=NULL, all=FALSE, rows=1000,
                           minimised=FALSE, reference=FALSE) {
  # reminder
  if (reference && !class %in% c("ReferenceEntity", "ExternalOntology")) {
    stop("Note that 'class' needs to either ReferenceEntity or ExternalOntology, and no species filter")
  }
  
  path <- "data/schema"
  url <- file.path(getOption("base.address"), path, class)
  msg <- NULL
  
  # get the count first
  cnt.url <- file.path(url, "count")
  if (!is.null(species)) {
    species.id <- .matchSpecies(species, "taxId")
    cnt.url <- paste0(cnt.url, "?species=", species.id)
    msg <- 'Note that if "species" is specified, "class" needs to be an instance of Event or subclasses in PhysicalEntity'
  }
  all.cnt <- as.integer(.retrieveData(cnt.url, customizedMsg=msg, fromJSON=FALSE, as="text"))
  
  # set the range of entries
  if ((all) || (!all && rows > all.cnt)) rows <- all.cnt
  
  species.name <- ifelse(!is.null(species), .matchSpecies(species, "displayName"), "ALL")
  cat(paste0("Total ", all.cnt, " entries of ", class, " with species ", species.name,
             ", retrieving ", format(rows, scientific=FALSE), " of them...\n"))
  
  # calculate the range of pages
  max.class.offset <- 25
  max.other.offset <- 20000
  offset <- ifelse(!minimised && !reference, max.class.offset, max.other.offset)
  end.page <- ceiling(rows / offset) #round it up
  if ((rows / offset) %% 1 != 0) {
    if (end.page == 1) offset <- rows
    if(!all && end.page != 1) end.offset <- rows %% offset
  }
  
  # retrieve data
  if (minimised) url <- file.path(url, "min")
  if (reference) url <-file.path(url, "reference")
  
  url <- paste0(url, "?offset=", offset)
  if (!is.null(species)) url <- paste0(url, "&species=", species.id)
  
  # use doParallel - parallelly GET the urls with different pages
  cl <- makeCluster(2, outfile="") # make clusters. 'outfile' for progress bar
  registerDoParallel(cl)
  
  pb <- txtProgressBar(min=0, max=end.page, style=3)
  dfcomb <- function(...) {
    rbindlist(list(...), fill = TRUE)
  }
  
  page <- 1 #to avoid note in R check
  final.df <- foreach(page=seq(1, end.page), .export=c(".retrieveData", ".checkStatus"), .combine=dfcomb) %dopar% {
    setTxtProgressBar(pb, page)
    # change the offset for the last page if it's different
    if (page == end.page && exists("end.offset")) {
      url <- gsub(paste0("offset=", offset), paste0("offset=", end.offset), url)
    }
    tmp.url <- paste0(url, "&page=", page)
    .retrieveData(tmp.url, fromJSON=TRUE, as="text")
  }
  stopCluster(cl)
  
  # sort by dbId
  final.df <- final.df[order(final.df$dbId),]
  rownames(final.df) <- seq(1, nrow(final.df))
  final.df
}



#' Search query
#' 
#' Search for Reactome objects by name or identifiers.
#' 
#' @param query name or dbId or stId of a search term from any class
#' @param species name or taxon id or dbId or abbreviation of species
#' @param types type filter, such as "Protein", "Complex", "Reaction", etc
#' @param compartments compartment filter, such as "cytosol", "plasma membrane", "nucleoplasm", etc
#' @param keywords keyword filter, such as "binds", "phosphorylates", "transports", etc
#' @param cluster cluster returned data or not
#' @param range start row and the number of rows to include, e.g. `range = c(0, 2)`
#' @return a list of information about the search term
#' @examples
#' searchQuery(query="Biological oxidation", species="Mus musculus", types=c("Pathway", "Reaction"))
#' @seealso \code{\link{listSearchItems}} for available filters
#' @rdname searchQuery
#' @export

searchQuery <- function(query, species=NULL, types=NULL, compartments=NULL,
                        keywords=NULL, cluster=TRUE, range=NULL) {
  # write full url
  args <- as.list(environment())
  args <- args[vapply(args, function(arg) !is.null(arg), logical(1))]
  print(names(args))
  path <- "search/query"
  url <- file.path(getOption("base.address"), paste0(path, "?query=", gsub("\\s", "%20", query)))
  
  ## add filters for the query
  filters <- args[!names(args) %in% c("query", "cluster", "range")]
  if ("species" %in% names(filters)) {
    filters[["species"]] <- .matchSpecies(filters[["species"]], "displayName")
  }
  msg <- paste0("Searching for term '", query, "'... ")
  for (filter in names(filters)) {
    msg <- paste0(msg, filter, ":'", paste(filters[[filter]], collapse = "' & '"), "' ")
    for (term in filters[[filter]]) {
      url <- paste0(url, "&", filter, "=", gsub("\\s", "%20", term)) 
    }
  }
  cat(paste0(msg, "\n"))
  
  ## cluster the returned data or not
  url <- paste0(url, "&cluster=", tolower(cluster)) 
  ## restrict rows to include
  if (!is.null(range)) url <- paste0(url, "&Start%20row=", range[1], "&rows=", range[2])
  
  # retrieve
  check.msg <- .spellCheck(query)
  .retrieveData(url, customizedMsg=check.msg, as="text")
}



#' Species queries
#' 
#' This method retrieves the list of all or main species in Reactome knowledgebase.
#' 
#' @param main determine whether return main species, which are those have 
#' either manually curated or computationally inferred pathways
#' @return a dataframe of species information
#' @examples
#' # get a list of main species
#' getSpecies(main=TRUE)
#' @rdname getSpecies
#' @family getContent
#' @export 

getSpecies <- function(main=FALSE) {
  path <- "data/species"
  # write the url
  url <- ifelse(main, 
                file.path(getOption("base.address"), path, "main"),
                file.path(getOption("base.address"), path, "all"))
  
  .retrieveData(url, as="text")
}

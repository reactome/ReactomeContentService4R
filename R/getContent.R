## Retrieve data from Content Service


#' Search engines discovery schema
#' @param event.id stable id or db id of an event
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
#' @param id stable or db id of a Reactome PhysicalEntity, or id from resources other than Reactome
#' @param retrieval entities to be retrieved
#' @param resource resource other than Reactome, e.g. UniProt, Ensembl
#' @param subunitsExcludeStructures whether contained complexes and entity sets are excluded when retrieving subunits
#' @return a dataframe containing requested information
#' @examples
#' getEntities("R-HSA-5674003", "subunits")
#' getEntities("P00533", "complexes", "UniProt")
#' @rdname getEntities
#' @export 

getEntities <- function(id, retrieval=c("subunits", "complexes", "componentOf", "otherForms"),
                        resource="Reactome", subunitsExcludeStructures=FALSE) {
  # check the inputs
  if (missing(retrieval)) message("Retrieval argument not specified, retrieving 'subunits'... For other entities, specify 'retrieval'")
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
#' @param main.species name or taxon/db id or abbreviation of main species, which could be searched using `getSpecies(main=T)`
#' @return a nested dataframe containing full event hierarchy for any given main species
#' @examples
#' #getEventsHierarchy("chicken")
#' #getEventsHierarchy("XTR")
#' @rdname getEventsHierarchy
#' @export 

getEventsHierarchy <- function(main.species) {
  path <- "data/eventsHierarchy"
  taxon.id <- .matchSpecies(main.species, "taxId")
  url <- file.path(getOption("base.address"), path, taxon.id)
  .retrieveData(url, as="text")
}



#' Mapping related queries
#' @param id a stable or db id of an event or entity
#' @param resource database name other than Reactome (e.g. UniProt, GeneCards)
#' @param species name or taxon id or dbId or abbreviation of species
#' @param mapTo retrieve pathways or reactions where an identifier can be mapped to
#' @return a dataframe containing requested pathways or reactions
#' @examples
#' getMapping("Q7Z569", "GeneCards", "human", "reactions")
#' @rdname getMapping
#' @export 

getMapping <- function(id, resource, species, mapTo=c("pathways", "reactions")) {
  path <- "data/mapping"
  if (missing(mapTo)) message("MapTo argument not specified, mapping to pathways... For reactions, specify mapTo='reactions'")
  mapTo <- match.arg(mapTo, several.ok=FALSE)
  taxon.id <- .matchSpecies(species, "taxId")
  url <- file.path(getOption("base.address"), path, resource, 
                   id, paste0(mapTo, "?species=", taxon.id))
  .retrieveData(url, as="text")
}



#' Orthology related queries
#' @param id a stable or db id of an event or entity
#' @param species name or taxon id or dbId or abbreviation of species
#' @return a list containing the orthology for given event or entity in the specified species
#' @examples
#' getOrthology("R-HSA-5674003", "Sus scrofa")
#' @rdname getOrthology
#' @export 

getOrthology <- function(id, species) {
  path <- "data/orthology"
  species.id <- .matchSpecies(species, "dbId") #dbId only
  url <- file.path(getOption("base.address"), path, id, "species", species.id)
  .retrieveData(url, as="text")
}



#' Participants queries
#' @param event.id a stable or db id of an Event (pathways and reactions)
#' @param retrieval to retrieve all participants or PhysicalEntities or ReferenceEntities in an Event, or ReactionLikeEvents in a pathway
#' @return a dataframe containing requested participants
#' @examples
#' getParticipants("R-HSA-73916", "AllInstances")
#' # getParticipants("R-HSA-69306", "ReactionLikeEventsInPathways")
#' # getParticipants("R-HSA-5205685", "PhysicalEntities")
#' @rdname getParticipants
#' @export 

getParticipants <- function(event.id, retrieval=c("AllInstances", "PhysicalEntities", 
                                                  "ReferenceEntities", "ReactionLikeEventsInPathways")) {
  path <- "data/participants"
  
  # write url
  url <- file.path(getOption("base.address"), path, event.id) #all participants
  if (missing(retrieval)) message("Retrieval argument not spcified, retrieving AllInstances... For others, specify 'retrieval'")
  retrieval <- match.arg(retrieval, several.ok = FALSE)
  
  msg <- NULL
  if (retrieval == "PhysicalEntities") {
    url <- file.path(url, "participatingPhysicalEntities")
  } else if (retrieval == "ReferenceEntities") {
    url <- file.path(url, "referenceEntities")
  } else if (retrieval == "ReactionLikeEventsInPathways") {
    # in a different path/method
    url <- file.path(getOption("base.address"), "data/pathway", event.id, "containedEvents")
    msg <- "'ReactionLikeEvents' are in those pathways with 'hasEvent' attribute"
  }
  
  # retrieve
  participants <- .retrieveData(url, customizedMsg=msg, as="text")
  
  # annotate instances in ReactionLikeEvents
  if (retrieval == "AllInstances") {
    all.info <- query(event.id)
    #if (all.info[["schemaClass"]] %in% c("Reaction", "BlackBoxEvent", "Depolymerisation", "FailedReaction", "Polymerisation")) {
    if (all.info[["className"]] == "Reaction") {
      participants$type <- rep(NA, nrow(participants))
      
      # input/output/catalysts/regulations
      for (pe in c("input", "output", "catalystActivity", "regulatedBy")) {
        # if no weird IDs then it is a dataframe
        if (is.data.frame(all.info[[pe]])) {
          apply(all.info[[pe]], 1, function(row) {
            if (row$dbId %in% participants$peDbId) {
              id <- row$dbId
              participants[participants$peDbId == id, ]$type <<- pe
            } else if (row$physicalEntity.dbId %in% participants$peDbId){
              id <- row$physicalEntity.dbId
              participants[participants$peDbId == id, ]$type <<- pe
            }
          })
        } else if (is.list(all.info[[pe]])) {
          # a list
          for (list in all.info[[pe]]) {
            id <- list["dbId"]
            if (!is.null(id) && id %in% participants$peDbId) participants[participants$peDbId == id, ]$type <- pe
          }
        }
      }
      participants$type <- gsub("catalystActivity", "catalyst", participants$type)
      participants <- participants[ ,c("peDbId", "displayName", "schemaClass", "type", "refEntities")] # rearrange the columns
    }
  }
  
  participants
}



#' Pathway related queries
#' @param id a stable or db id of a PhysicalEntity or Event present in the pathways
#' @param species name or taxon id or dbId or abbreviation of species
#' @param allForms whether to return all low level pathways that contain the given PhysicalEntity (not Event) in all forms
#' @param top.level If set to \code{TRUE}, results would all be top-level pathways
#' @return a dataframe containing requested low level pathways
#' @examples
#' getPathways("R-HSA-199420", "Homo sapiens")
#' @rdname getPathways
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
    
    return(top.pathways)
  } else {
    return(pathways)
  }
}



#' Person queries
#' @param name Person’s first or/and last name
#' @param id Person's Orcid Id or DbId
#' @param attributes Property for a person. Return all available attributes if it is not specified.
#' @return a list of requested information
#' @examples
#' getPerson(name="Robin Haw", attributes=c("displayName", "affiliation"))
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
      tmp <- .retrieveData(tmp.url, fromJSON=F, as="parse")
      ifelse(is.character(tmp), 
             all.info[[attribute]] <- tmp, 
             all.info[[attribute]] <- .retrieveData(tmp.url, fromJSON=T, as="text"))
    }
  }
  all.info
}



#' List the whole Reactome search items (species, types, compartments, keywords)
#' @param items categories of query
#' @param facet return faceting information or not
#' @return available search items
#' @examples
#' listSearchItems(items=c("species", "keyword"))
#' @rdname listSearchItems
#' @export

listSearchItems <- function(items=c("all", "species", "type", "compartment", "keyword"),
                            facet=FALSE) {
  path <- "search/facet"
  
  # ensure inputs
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
    final.list <- final.list[sapply(final.list, function(x) inherits(x, "data.frame"))]
    final.list <- lapply(final.list, function(x) data.frame(name=x$name))
    names(final.list) <- gsub("Facet$", "", names(final.list))
  }
  final.list
}



#' Common data retrieval
#' @param id a stable or db id of a Reactome entry
#' @return a list containing comprehensive information for a given id
#' @examples
#' query("R-HSA-60140")
#' @rdname query
#' @export 

query <- function(id) {
  path <- "data/query/enhanced"
  url <- file.path(getOption("base.address"), path, id)
  .retrieveData(url, as="text")
}



#' ReferenceEntity queries
#' @param external.id an id from external dabatases, e.g. ChEBI, UniProt
#' @return a list containing all reference entities for a given id
#' @examples
#' getReferences("15377") #ChEBI id
#' @rdname getReferences
#' @export 

getReferences <- function(external.id) {
  path <- "references/mapping"
  url <- file.path(getOption("base.address"), path, external.id)
  ref.df <- .retrieveData(url, as="text")
  as.list(ref.df)
}



#' Schema class queries
#' @param class schema class name, details see \href{https://reactome.org/content/schema/DatabaseObject}{Reactome data schema}
#' @param species name or taxon id or dbId or abbreviation of species. Only Event or PhysicalEntity class can specify species
#' @param all to return all entries or not, default is \code{FALSE}
#' @param rows the number of rows of entries retrieved, default is 1000
#' @param minimised to retrieve simplified entries (db id, stable id, displayName, type) or not, default is \code{FALSE}
#' @param reference to retrieve simplified reference objects (db id, external identifier, 
#' external database name) or not, default is \code{FALSE}. Only for ReferenceEntity or ExternalOntology class
#' @return a sorted dataframe containing entries that belong to the specified schema class
#' @examples
#' getSchemaClass(class="Drug", all=TRUE)
#' # getSchemaClass("Regulation", rows=2000, minimised=TRUE)
#' # getSchemaClass("Complex", species="pig", rows=100)
#' @importFrom data.table rbindlist
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @rdname getSchemaClass
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
    msg <- 'Please note that if "species" is specified, "class" needs to be an instance of Event or PhysicalEntity'
  }
  all.cnt <- as.integer(.retrieveData(cnt.url, customizedMsg=msg, fromJSON=FALSE, as="text"))
  if (length(all.cnt) == 0) stop("as above", call.=FALSE)
  
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
  final.df <- foreach(page=1:end.page, .export=c(".retrieveData", ".checkStatus"), .combine=dfcomb) %dopar% {
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
  rownames(final.df) <- 1:nrow(final.df)
  final.df
}



#' Search query
#' @param query name or dbId or stId of a search term from any class
#' @param species name or taxon id or dbId or abbreviation of species
#' @param types type filter, such as "Protein", "Complex", "Reaction", etc
#' @param compartments compartment filter, such as "cytosol", "plasma membrane", "nucleoplasm", etc
#' @param keywords keyword filter, such as "binds", "phosphorylates", "transports", etc
#' @param cluster cluster returned data or not
#' @param range start row and the number of rows to include
#' @return a list of information about the search term
#' @examples
#' searchQuery(query="Biological oxidation", species="Mus musculus")
#' @rdname searchQuery
#' @export

searchQuery <- function(query, species=NULL, types=NULL, compartments=NULL,
                        keywords=NULL, cluster=TRUE, range=NULL) {
                        
  # write full url
  args <- as.list(environment())
  path <- "search/query"
  query <- gsub("\\s", "%20", query)
  url <- file.path(getOption("base.address"), paste0(path, "?query=", query))
  
  ## add filters for the query
  filters <- args[!names(args) %in% c("query", "cluster", "range")]
  filters[["species"]] <- .matchSpecies(species, "displayName")
  
  for (filter in names(filters)) {
    url <- paste0(url, "&", filter, "=", gsub("\\s", "%20", filters[[filter]]))
  }
  ## cluster the returned data or not
  url <- paste0(url, "&cluster=", tolower(cluster)) 
  ## restrict rows to include
  if (!is.null(range)) url <- paste0(url, "&Start%20row=", range[1], "&rows=", range[2])
  
  # retrieve
  check.msg <- .spellCheck(query)
  .retrieveData(url, customizedMsg=check.msg, as="text")
}



#' Species queries
#' @param main determine whether return main species, which are those have either manually curated or computationally inferred pathways
#' @return a dataframe of species information
#' @examples
#' getSpecies(main=TRUE)
#' @rdname getSpecies
#' @export 

getSpecies <- function(main=FALSE) {
  path <- "data/species"
  # write the url
  url <- ifelse(main, 
                file.path(getOption("base.address"), path, "main"),
                file.path(getOption("base.address"), path, "all"))
  
  .retrieveData(url, as="text")
}

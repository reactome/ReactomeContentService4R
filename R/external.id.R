#' Non-Reactome id mappings
#' 
#' Mappings between non-Reactome identifiers and Reactome objects.
#' 
#' Descriptions of functions:
#' - [map2RefEntities] maps a non-Reactome id to ReferenceEntities that store the given external id
#' - [map2Events] fetches Events related to a given non-Reactome id
#' - [event2Ids] gets all non-Reactome ids associated with a given Event
#' 
#' @name nonReactomeId
#' @rdname mapId
#' @family nonReactomeId
#' @param external.id an id from external dabatases, e.g. ChEBI, UniProt
#' @examples
#' map2RefEntities("15377") #ChEBI id
#' @export

map2RefEntities <- function(external.id) {
  path <- "references/mapping"
  url <- file.path(getOption("base.address"), path, external.id)
  ref.df <- .retrieveData(url, as="text")
  as.list(ref.df)
}



#' @param resource database name other than Reactome (e.g. UniProt, GeneCards)
#' @param species name or taxon id or dbId or abbreviation of species
#' @param mapTo retrieve Pathways or Reactions where an identifier can be mapped to
#' @examples
#' map2Events("Q7Z569", resource="GeneCards", species="human", mapTo="reactions")
#' @rdname mapId
#' @family nonReactomeId
#' @export 

map2Events <- function(external.id, resource, species, mapTo=c("pathways", "reactions")) {
  path <- "data/mapping"
  if (missing(mapTo)) message("MapTo argument not specified, mapping to pathways... For reactions, specify mapTo='reactions'")
  mapTo <- match.arg(mapTo, several.ok=FALSE)
  taxon.id <- .matchSpecies(species, "taxId")
  url <- file.path(getOption("base.address"), path, resource, external.id, paste0(mapTo, "?species=", taxon.id))
  .retrieveData(url, as="text")
}



#' @param event.id a stable or db id of an Event (Pathways and Reactions)
#' @examples
#' event2Ids("R-HSA-69541")
#' @rdname mapId
#' @family nonReactomeId
#' @export

event2Ids <- function(event.id) {
  # get all ReferenceEntities of the Event
  re <- getParticipants(event.id, retrieval = "ReferenceEntities")
  if (nrow(re) == 0) stop("No ReferenceEntity for this Event")
  
  id.list <- list()
  # all gene symbols
  id.list[["geneSymbol"]] <- unlist(re$geneName)
  # all ReferenceEntity displayName + dbId
  id.list[["primaryIdentifier"]] <- re[ ,c("dbId", "displayName")]
  # secondary ids
  id.list[["secondaryIdentifier"]] <- unlist(re$secondaryIdentifier)
  # other ids
  id.list[["otherIdentifier"]] <- unlist(re$otherIdentifier)
  id.list
}


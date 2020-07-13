## Mappings between non-Reactome identifiers and Reactome objects


#' Non-Reactome id to ReferenceEntity
#' @param external.id an id from external dabatases, e.g. ChEBI, UniProt
#' @return a list containing all ReferenceEntities for a given id
#' @examples
#' map2RefEntities("15377") #ChEBI id
#' @rdname map2RefEntities
#' @export 

map2RefEntities <- function(external.id) {
  path <- "references/mapping"
  url <- file.path(getOption("base.address"), path, external.id)
  ref.df <- .retrieveData(url, as="text")
  as.list(ref.df)
}



#' Non-Reactome id to Events
#' @param id non-Reactome identifier
#' @param resource database name other than Reactome (e.g. UniProt, GeneCards)
#' @param species name or taxon id or dbId or abbreviation of species
#' @param mapTo retrieve pathways or reactions where an identifier can be mapped to
#' @return a dataframe containing requested pathways or reactions
#' @examples
#' map2Events("Q7Z569", "GeneCards", "human", "reactions")
#' @rdname map2Events
#' @export 

map2Events <- function(id, resource, species, mapTo=c("pathways", "reactions")) {
  path <- "data/mapping"
  if (missing(mapTo)) message("MapTo argument not specified, mapping to pathways... For reactions, specify mapTo='reactions'")
  mapTo <- match.arg(mapTo, several.ok=FALSE)
  taxon.id <- .matchSpecies(species, "taxId")
  url <- file.path(getOption("base.address"), path, resource, 
                   id, paste0(mapTo, "?species=", taxon.id))
  .retrieveData(url, as="text")
}



#' Event to non-Reactome ids
#' @param event.id a stable or db id of an Event (Pathways and Reactions)
#' @return a list containing all non-Reactome identifiers associated with the given Event
#' @examples
#' event2Ids("69541")
#' @rdname event2ExtIds
#' @seealso \code{\link{getParticipants}} for all entities of an Event
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


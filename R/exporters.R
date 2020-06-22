## Export files or images of Reactome data


#' File exporter for events
#' @param event.id a stable or db id of a pathway or reaction
#' @param format either in sbgn (Systems Biology Graphical Notation) or sbml (Systems Biology Markup Language)
#' @param file full path of the output file
#' @return content of sbgn or sbml or a file saved into specified path
#' @examples
#' exportEventFile("R-HSA-432047", "sbgn")
#' #exportEventFile("R-HSA-68616", "sbml", "R-HSA-68616.sbml.xml")
#' @importFrom utils write.table
#' @rdname exportEventFile
#' @export

exportEventFile <- function(event.id, format=c("sbgn", "sbml"), file=NULL) {
  path <- "exporter/event"
  format <- match.arg(format, several.ok=FALSE)
  url <- file.path(getOption("base.address"), path, paste0(event.id, ".", format))
  file.content <- .retrieveData(url, fromJSON=FALSE, as="text")
  # save into a file
  if (!is.null(file)) {
    write.table(file.content, file=file, quote=F, row.names=F, col.names=F)
  } else {
    return(file.content) 
  }
}



#' Image exporter
#' @param id stable or db id of a ReactionLikeEvent for "reaction" output, or id of an Event for "diagram"
#' @param output type of exported image including "diagram", "fireworks", "reaction"
#' @param species name or db id or taxon id of a species. Used in "fireworks" output
#' @param format output format including "png", "jpg", "jpeg", "svg", "gif"
#' @param quality result image quality between 1-10, default is 5
#' @param flg gene name, protein or chemical identifier or Reactome identifier used to flag elements in the diagram
#' @param flgInteractors defines whether to take into account interactors for the flagging
#' @param sel highlight element(s) selection in the diagram
#' @param title whether the name of the pathway is shown below
#' @param margin defines the image margin between 0-20, default is 15
#' @param ehld whether textbook-like illustration are taken into account
#' @param diagramProfile color profile, "modern" or "standard"
#' @param token token from Reactome Analysis Service
#' @param resource the analysis resource for which the results will be overlaid on top of the given pathways overview
#' @param analysisProfile analysis color profile including "Standard", "Strosobar", "Copper%20plus"
#' @param expColumn the specific expression analysis results column to be overlaid. If it is not specified (null),
#' the first one is selected. If it is not specified (null) and format is gif, then an animated gif is generated with all the columns.
#' @param fireworksCoverage to overlay analysis coverage values or not in fireworks image
#' @param file full path of the output file
#' @param ... additional parameters passed to `magick::image_write()`
#' @return a file saved into the specified path or a magick image object. More magick processing see the \href{https://cran.r-project.org/web/packages/magick/vignettes/intro.html}{vignette}.
#' @examples
#' ## animated gifs of EHLDs
#' # gif <- exportImage(id="R-HSA-69278", output="diagram", format="gif",
#' #                   sel="R-HSA-69242", token="MjAyMDA2MTcyMDM5NDBfMzU2")
#' # print(gif)
#'
#' ## fireworks
#' # fw <- exportImage(species="9606", output="fireworks", format="jpg",
#' #                  quality=7, sel="R-HSA-68918")
#' # print(fw)
#'
#' ## reaction
#' # exportImage(id="R-HSA-6787403", output="reaction", format="svg",
#' #           flg="MTO1", analysisProfile="Copper%20plus", file="R-HSA-6787403.svg")
#' @importFrom magick image_read_svg image_read image_write
#' @rdname exportImage
#' @export

exportImage <- function(id=NULL, output=c("diagram", "fireworks", "reaction"),
                        species=NULL, format=c("png", "jpg", "jpeg", "svg", "gif"), 
                        quality=5, flg=NULL, flgInteractors=TRUE, sel=NULL, title=TRUE, 
                        margin=15, ehld=TRUE, diagramProfile="Modern", token=NULL, 
                        resource="TOTAL", analysisProfile="Standard", expColumn=NULL,
                        fireworksCoverage=FALSE, file=NULL, ...) {
  
  # ensure the arguments
  output <- match.arg(output, several.ok=FALSE)
  format <- match.arg(format, several.ok=FALSE)
  
  args <- as.list(environment()) #collect arguments except dots
  if (output == "fireworks") id <- .matchSpecies(species, "taxId") #replace id with taxon id
  
  # add path and quality parameter to url
  url <- file.path(getOption("base.address"), "exporter", output, paste0(id, ".", format, "?quality=", quality))
  
  # add other parameters to url
  boolean.args <- sapply(args, is.logical) #turn boolean arguments into lower case characters
  args[boolean.args] <- lapply(args[boolean.args], tolower)
  args <- args[!names(args) %in% c("id", "output", "species", "format", "quality", "file")]
  
  if (output != "diagram") args <- args[names(args) != "ehld"]
  ifelse(output == "fireworks", 
         args <- args[names(args) != "analysisProfile"],
         args <- args[names(args) != "fireworksCoverage"])
  # concatenate specified parameters
  for (arg in names(args)) {
    if (!is.null(args[[arg]])) url <- paste0(url, "&", arg, "=", args[[arg]]) 
  }
  
  # get image using magick package
  if (format == "svg") {
    img <- image_read_svg(url)
  } else {
    img <- image_read(url)
  }
  
  # write into a file if file path provided
  if (!is.null(file)) {
    image_write(image=img, path=file, format=format, ...)
  } else {
    return(img)
  }
}

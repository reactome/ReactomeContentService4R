## Exporters for files and images


#' File exporter
#' 
#' Export Reactome pathway diagrams in SBGN or SBML format.
#' 
#' @param id a stable or db id of an Event (Pathway or Reaction)
#' @param format either in "sbgn" (SBGN, Systems Biology Graphical Notation) or 
#' "sbml" (SBML, Systems Biology Markup Language)
#' @param writeToFile If set to `TRUE`, the returned data would be written into a file. 
#' If `file=NULL`, the output file will be automatically saved into the working 
#' directory and named based on the `id` and `format`
#' @param file full path of the output file
#' @return a character object with the content of SBGN/SBML for a given id, or 
#' a SBGN/SMBL file saved into the specified path. If the output is empty character 
#' or list, please check on \href{https://reactome.org/ContentService/}{ContentService} 
#' or contact HelpDesk \email{help@@reactome.org}.
#' @examples
#' exportEventFile("R-HSA-432047", "sbgn", writeToFile=FALSE)
#' \dontrun{exportEventFile("R-HSA-68616", "sbml", file="orc.assembly.sbml")}
#' @importFrom utils write.table
#' @rdname exportEventFile
#' @family exporter
#' @export

exportEventFile <- function(id, format=c("sbgn", "sbml"), 
                            writeToFile=TRUE, file=NULL) {
  # write url
  path <- "exporter/event"
  if (missing(format)) {
    message("Format argument not specified, exporting in SGBN format... For SBML, specify format='sbml'")
  }
  format <- match.arg(format, several.ok=FALSE)
  url <- file.path(getOption("base.address"), path, paste0(id, ".", format))
  file.content <- .retrieveData(url, fromJSON=FALSE, as="text")
  
  # save into a file
  if (writeToFile) {
    # get the current working directory if file path not specified
    if (is.null(file)) file <- file.path(getwd(), paste0(id, ".", format))
    cat(paste0("File exported to '", file, "'...\n"))
    write.table(file.content, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)
  } else {
    return(file.content)
  }
}



#' Image exporter
#' 
#' The diagram exporter allows researchers to include images of their favorite 
#' pathway diagrams into their publications, posters or presentations. For details 
#' see Reactome [diagram exporter](https://reactome.org/dev/content-service/diagram-exporter) guide.
#' 
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
#' @param token token from Reactome \href{https://reactome.org/dev/analysis}{Analysis Service}
#' @param resource the analysis resource for which the results will be overlaid on top of the given pathways overview
#' @param analysisProfile analysis color profile including "Standard", "Strosobar", "Copper%20plus"
#' @param expColumn the specific expression analysis results column to be overlaid. 
#' If it is not specified (null), the first one is selected. If it is not specified (null) 
#' and format is gif, then an animated gif is generated with all the columns.
#' @param fireworksCoverage set `TRUE` to overlay analysis coverage values in a fireworks image
#' @param file full path of the output file
#' @param ... additional parameters passed to \code{\link[magick]{image_write}}
#' @return an image saved into the specified path or a magick image object. 
#' More magick processing see the \href{https://docs.ropensci.org/magick/}{package}.
#' @examples
#' # fireworks
#' fw <- exportImage(species="9606", output="fireworks", 
#'                   format="jpg", quality=7)
#' print(fw)
#' 
#' \dontrun{
#' # animated gifs of EHLDs
#' # can use your own token
#' gif <- exportImage(id="R-HSA-69278", output="diagram", format="gif",
#'                    sel="R-HSA-69242", token="MjAyMDA2MTcyMDM5NDBfMzU2")
#' print(gif)
#'
#' # reaction
#' exportImage(id="R-HSA-6787403", output="reaction", format="svg",
#'             flg="MTO1", analysisProfile="Copper%20plus", file="R-HSA-6787403.svg")
#' }
#' @importFrom magick image_read_svg image_read image_write
#' @rdname exportImage
#' @family exporter
#' @seealso \code{\link[magick]{magick}} to further process the image object
#' @export

exportImage <- function(id=NULL, output=c("diagram", "fireworks", "reaction"),
                        species=NULL, format=c("png", "jpg", "jpeg", "svg", "gif"), 
                        quality=5, flg=NULL, flgInteractors=TRUE, sel=NULL, title=TRUE, 
                        margin=15, ehld=TRUE, diagramProfile="Modern", token=NULL, 
                        resource="TOTAL", analysisProfile="Standard", expColumn=NULL,
                        fireworksCoverage=FALSE, file=NULL, ...) {
  
  # ensure the arguments
  if (missing(format)) {
    message("Format argument not spcified, exporting as 'png'... For 'jpg', 'jpeg', 'svg', 'gif', specify 'format'")
  }
  output <- match.arg(output, several.ok=FALSE)
  format <- match.arg(format, several.ok=FALSE)
  
  args <- c(as.list(environment()), list(...)) #collect arguments
  #replace id with taxon id
  if (output == "fireworks") id <- .matchSpecies(species, "taxId")
  
  # add path and quality parameter to url
  url <- file.path(getOption("base.address"), "exporter", output, 
                   paste0(id, ".", format, "?quality=", quality))
  
  # add other parameters to url
  ## turn boolean arguments into lower case characters
  boolean.args <- vapply(args, is.logical, logical(1))
  args[boolean.args] <- lapply(args[boolean.args], tolower)
  args <- args[!names(args) %in% c("id", "output", "species", "format", "quality", "file", names(list(...)))]
  
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
    cat(paste0("Image exported to '", file, "'...\n"))
    image_write(image=img, path=file, format=format, ...)
  } else {
    return(img)
  }
}

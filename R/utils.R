## helper functions

.checkVersion <- function() {
  r <- httr::GET(url="https://reactome.org/ContentService/data/database/version")
  content(r, "text")
}


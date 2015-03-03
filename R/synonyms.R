#' Get synonymous for a given word id
#'
#' @param word The input word
#' @return List of all synonyms for a given word
#' @seealso \code{\link{getSynonymsAPI}}
#' @import XML
#' @examples
#' car_synonyms <- synonyms("машина")
#' @export
synonyms <- function(word) {
  # Checks parameters
  if (missing(word)) {
    stop("Missing word argument")
  }
  
  if (!is.character(word)) {
    stop("Word must be a string")
  }
  
  # Find all synsets for a given word
  synsets <- getSynsets(word)
  if (length(synsets) == 0) {
    ""
  } else {
    references <- unique(unlist(sapply(synsets, function(synset)
      sapply(synset["word"], function(node) xmlGetAttr(node, name = "ref"))
    )))
    syns <- as.vector(sapply(references, getWordById))
    syns[!is.na(syns)]
  }
}
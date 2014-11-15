#' Поиск синонимов для указанного слова
#'
#' Краткое описание работы функции
#' @param word Слово, для которого производится поиск синонимов
#' @return Список найденных синонимов
#' @seealso \code{\link{synonymsAPI}}
#' @export
#' @import XML
#' @examples
#' car_synonyms <- synonyms("машина")
synonyms <- function(word) {
  # Checks parameters
  if (missing(word)) {
    stop("No word!")
  }
  
  if (!is.character(word)) {
    stop("Word must be a string")
  }
  
  # if (is.null(.yarn$root)) {
  #   stop("You must specify the yarn dictionary")
  # }
  # root <- getDict()
  
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
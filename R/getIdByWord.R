#' Get id by word
#'
#' @param word The input word
#' @return Word id
#' @import XML
#' @export
getIdByWord <- function(word = "") {
  # if (is.null(.yarn$root)) stop('could not find YARN dictionary. Use setYarn() function')
  if (!is.character(word)) stop('word must be a character')
  
  root <- getDict()
  node <- getNodeSet(root, sprintf('//words/wordEntry/word[text()="%s"]', word))
  if (identical(node[1], list(NULL))) {
    ''
  } else {
    xmlGetAttr(xmlParent(node[[1]]), name = 'id')
  }
}
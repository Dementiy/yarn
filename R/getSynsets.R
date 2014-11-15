#' Get synsets for a given word
#' 
#' @param word The input word
#' @import XML
#' @export
getSynsets <- function(word) {
  # if (is.null(.yarn$root)) stop('you must specify the yarn dictionary')
  if (!is.character(word)) stop('word must be a string')
  
  root <- getDict()
  word_id <- getIdByWord(word)
  synsets <- getNodeSet(root, sprintf('//synsets/synsetEntry/word[@ref="%s"]', word_id))
  xmlSApply(synsets, xmlParent)
}
#' Get synsets for a given word
#' 
#' @param word The input word
#' @return A list of synsets
#' @seealso \code{\link{getSynsetsAPI}}
#' @examples
#' getSynsets('музыка')
#' @export
getSynsets <- function(word) {
  # if (is.null(.yarn$root)) stop('you must specify the yarn dictionary')
  if (!is.character(word)) stop('word must be a string')
  
  root <- getDict()
  word_id <- getIdByWord(word)
  synsets <- getNodeSet(root, sprintf('//synsets/synsetEntry/word[@ref="%s"]', word_id))
  xmlSApply(synsets, xmlParent)
}
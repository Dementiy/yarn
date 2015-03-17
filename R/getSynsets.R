#' Get synsets for a given word
#' 
#' @param word The input word
#' @return A list of synsets
#' @seealso \code{\link{getSynsetsAPI}}
#' @examples \dontrun{
#' getSynsets('house')
#' }
#' @export
getSynsets <- function(word) {
  if (!is.character(word)) stop('word must be a string')
  
  root <- getDict()
  word_id <- getIdByWord(word)
  synsets <- getNodeSet(root, 
                        sprintf('/ns:yarn/ns:synsets/ns:synsetEntry/ns:word[@ref="%s"]', word_id), 
                        c(ns = "http://russianword.net"))
  xmlSApply(synsets, xmlParent)
}
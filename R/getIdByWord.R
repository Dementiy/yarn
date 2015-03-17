#' Get the ID for a given word
#'
#' @param word The input word
#' @return ID
#' @seealso \code{\link{getWordById}, \link{getIdByWordAPI}}
#' @examples \dontrun{
#' getIdByWord('house')
#' }
#' @export
getIdByWord <- function(word = "") {
  if (!is.character(word)) stop('word must be a character')
  
  root <- getDict()
  node <- getNodeSet(root, 
                     sprintf('/ns:yarn/ns:words/ns:wordEntry/ns:word[text()="%s"]', word), 
                     c(ns = "http://russianword.net"))
  if (identical(node[1], list(NULL))) {
    ''
  } else {
    xmlGetAttr(xmlParent(node[[1]]), name = 'id')
  }
}
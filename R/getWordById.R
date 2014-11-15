#' Возвращает слово по его индентификатору
#' 
#' Краткое описание функции
#' @param id Идентификатор искомого слова
#' @return Слово
#' @export
#' @import XML
getWordById <- function(id) {
  # if (is.null(.yarn$root)) stop("You must specify the yarn dictionary")
  root <- getDict()
  
  # Can reproduce NA's!!!
  as.vector(xmlValue(getNodeSet(root, sprintf('//words/wordEntry[@id="%s"]/word', id))[[1]]))
}
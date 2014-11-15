#' Установить путь к словарю
#'
#' Краткое описание функции
#' @param path Путь к словарю
#' @export
#' @import XML
setDict <- function(path) {
  if (!file.exists(path)) {
    stop("File doesn't exists")
  }
  
  yarn <- xmlParse(path, useInternalNodes = TRUE, options = HUGE)
  .yarn$root <- xmlRoot(yarn)
  if (xmlName(.yarn$root) != 'yarn') {
    .yarn$root <- NULL
    stop("Wrong dictionary")
  }
}


getDict <- function() {
  if (!is.null(.yarn$root)) {
    .yarn$root
  } else {
    stop("Could not find YARN dictionary. For more details see ?setDict")
  }
}
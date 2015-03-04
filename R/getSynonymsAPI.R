#' Get synonymous for a given word id
#' 
#' @param word_id Id
#' @return A list holding the synonums for a given word id
#' @seealso \code{\link{synonyms}, \link{getRawSynonymsAPI}}
#' @references
#' Dmitriy Ustalov. YARN API. \url{http://nlpub.ru/YARN/API}
#' @examples
#' word_id <- getIdByWordAPI("машина")
#' words2df(getSynonymsAPI(word_id))
#' @export
getSynonymsAPI <-function(word_id) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'words/', word_id, '/synonyms.xml', sep='')
  response <- GET(query)
  synonyms_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synonyms_objects, "//words/word")
}


#' Get raw synonymous for a given word id
#' 
#' @param word_id Id
#' @return A list holding the synonums for a given word id
#' @seealso \code{\link{getSynonymsAPI}}
#' @examples
#' word_id <- getIdByWordAPI("машина")
#' words2df(getRawSynonymsAPI(word_id))
#' @export
getRawSynonymsAPI <-function(word_id) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'words/', word_id, '/raw_synonyms.xml', sep='')
  response <- GET(query)
  raw_synonyms_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(raw_synonyms_objects, "//words/word")
}
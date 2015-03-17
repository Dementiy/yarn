#' Get synsets for a given word id
#' 
#' @param word_id The input word id
#' @return A list of synsets
#' @references
#' Dmitriy Ustalov. YARN API. \url{http://nlpub.ru/YARN/API}
#' @examples \dontrun{
#' getSynsets(131)
#' }
#' @export
getSynsetsAPI <- function(word_id) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  if (!is.numeric(word_id)) stop('word_id must be an integer')
  
  query <- paste(.yarn$domain, 'words/', word_id, '/synsets.xml', sep='')
  response <- GET(query)
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synsets/synset")
}


#' Get the IDs words in a synset
#' 
#' @param synset_object The synset
#' @return An integer vector holding IDs words
#' @examples \dontrun{
#' synsets <- getSynsetsAPI(word_id = 131)
#' getWordsIdsFromSynset(synsets[[1]])
#' }
#' @export
getWordsIdsFromSynset <- function(synset_object) {
  as.numeric(xmlSApply(getNodeSet(synset_object, "words-ids/words-id"), xmlValue))
}
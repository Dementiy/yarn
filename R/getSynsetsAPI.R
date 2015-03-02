#' Get synsets for a given word id
#' 
#' @param word_id Id
#' @return The output is an array with objects of class Synset
#' @export
getSynsetsAPI <- function(word_id) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words/', word_id, '/synsets.xml', sep='')
  response <- GET(query)
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synsets/synset")
}


#' Get ids for a given synset
#' 
#' @param synset_object
#' @examples
#' synsets <- getSynsetsAPI(word_id = 131)
#' getWordsIdsFromSynset(synsets[[1]])
#' @export
getWordsIdsFromSynset <- function(synset_object) {
  xmlSApply(getNodeSet(synset_object, "words-ids/words-id"), xmlValue)
}
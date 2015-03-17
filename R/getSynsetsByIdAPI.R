#' Get a full list of known synsets with page breakdown
#' 
#' @param page Page number
#' @param id Synset id
#' @return A list of synsets
#' @references
#' Dmitriy Ustalov. YARN API. \url{http://nlpub.ru/YARN/API}
#' @examples \dontrun{
#' getSynsetsByIdAPI(page=1)
#' getSynsetsByIdAPI(id=1)
#' }
#' @export
getSynsetsByIdAPI <- function(page=1, id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'synsets', sep='')
  if (id != '')
    query <- paste(query, '/', id, '.xml', sep='')
  else
    query <- paste(query, '.xml?page=', page, sep='')
  response <- GET(query)
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synset")
}


#' Word search in the synsets
#' 
#' A list of the synsets including specified word `word`
#' @param word The input word
#' @return A list of synsets
#' @examples \dontrun{
#' searchSynsetsAPI('house')
#' }
#' @export
searchSynsetsAPI <- function(word='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'synsets/search.xml?word=', word, sep='')
  response <- GET(URLencode(query))
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synset")
}


#' Binding word to the synset
#' 
#' Information about binding of the word id
#' @param word_id The input word id
#' @return The output is an array with objects of class SynsetWord
#' @export
synsetWordsAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'synset_words/', word_id, '.xml', sep='')
  response <- GET(query)
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synset-word")
}
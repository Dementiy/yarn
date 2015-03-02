#' Get synsets
#' 
#' @param page Page
#' @param id Id
#' @examples
#' synsets <- getSynsetsByIdAPI()
#' words_ids <- getWordsIdsFromSynset(synsets[[1]])
#' @export
getSynsetsByIdAPI <- function(page=1, id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'synsets', sep='')
  if (id != '')
    query <- paste(query, '/', id, '.xml', sep='')
  else
    query <- paste(query, '.xml?page=', page, sep='')
  response <- GET(query)
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synset")
}


#' Word search in the synset
#' 
#' A list of the synset including specified word word
#' @param word The input word
#' @return The output is an array with objects of class Synset
#' @examples
#' synset_objs <- searchSynsetsAPI('кот')
#' @export
searchSynsetsAPI <- function(word='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'synsets/search.xml?word=', word, sep='')
  response <- GET(URLencode(query))
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synset")
}


#' Binding word to the synset
#' 
#' Information about binding of the word id
#' @param word_id Id
#' @return The output is an array with objects of class SynsetWord
#' @export
synsetWordsAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'synset_words/', word_id, '.xml', sep='')
  response <- GET(query)
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synset-word")
}
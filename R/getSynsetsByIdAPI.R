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
  # content(response, type='text/xml', encoding = 'UTF-8')
  synset_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synset")
}


searchSynsetsAPI <- function(word='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'synsets/search.xml?word=', word, sep='')
  response <- GET(URLencode(query))
  # httr::content(response, type='text/xml', encoding = 'UTF-8')
  synset_objects <- httr::content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synset_objects, "//synset")
}

# searchSynsetsAPI('кот')

synsetWordsAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'synset_words/', word_id, '.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}
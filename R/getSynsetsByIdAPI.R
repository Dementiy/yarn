#' Get synsets
#' 
#' @param page Page
#' @param id Id
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
  content(response, type='text/xml', encoding = 'UTF-8')
}


searchSynsetsAPI <- function(word='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'synsets/search.xml?word=', word, sep='')
  response <- GET(URLencode(query))
  content(response, type='text/xml', encoding = 'UTF-8')
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
library(httr)

domain <- 'http://russianword.net/'

### Words

#' Get words by API
#' 
#' @param page Page
#' @param start Start
#' @export
getWordsAPI <- function(page=1, start='') {
  query <- paste(domain, 'words.xml', sep='')
  if (start != '')
    query <- paste(query, '?q=', start, '&page=', page, sep='')
  response <- GET(URLencode(query))
  content(response, type='text/xml', encoding = 'UTF-8')
}


#' Convert words to data.frame object
#' 
#' @param words The input words
#' @export
words2df <- function(words) {
  words <- as.data.frame(
    do.call(rbind,
            lapply(xmlChildren(xmlRoot(words)),
                   function(node) c(word=xmlValue(xmlChildren(node)$word), id=xmlValue(xmlChildren(node)$id)))
    )
  )
  rownames(words) <- NULL
  words
}


#' Get id for a given word
#' 
#' @param word The input word
#' @export
getIdByWordAPI <- function(word='') {
  words <- getWordsAPI(start=word)
  word  <- getNodeSet(words, sprintf('//words/word/word[text()="%s"]', word))
  xmlValue(xmlChildren(xmlParent(word[[1]]))$id)
}

getIdByWordAPI('машина')

#' Get synsets for a given word id (require registration)
#' 
#' @param word_id
#' @export
getSynsetsAPI <- function(word_id) {
  query <- paste(domain, 'words/', word_id, '/synsets.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}


#' Get synonymous for a given word id (require registration)
#' 
#' @param word_id Id
#' @export
getSynonymsAPI <-function(word_id) {
  query <- paste(domain, 'words/', word_id, '/synonyms.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}


#' Get raw synonymous for a given word id (require registration)
#' 
#' @param word_id Id
#' @export
getRawSynonymsAPI <-function(word_id) {
  query <- paste(domain, 'words/', word_id, '/raw_synonyms.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}


### Definitions

#' Get definitions
#' 
#' @param page Page
#' @param id Id
#' @export
getDefinitionsAPI <- function(page=1, id='') {
  query <- paste(domain, 'definitions', sep='')
  if (id != '')
    query <- paste(query, '/', id, '.xml', sep='')
  else
    query <- paste(query, '.xml?page=', page, sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}

getDefinitionsAPI(id=131)


### Synsets

#' Get synsets
#' 
#' @param page Page
#' @param id Id
#' @export
getSynsetsAPI <- function(page=1, id='') {
  query <- paste(domain, 'synsets', sep='')
  if (id != '')
    query <- paste(query, '/', id, '.xml', sep='')
  else
    query <- paste(query, '.xml?page=', page, sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}


searchSynsetsAPI <- function(word='') {
  query <- paste(domain, 'synsets/search.xml?word=', word, sep='')
  response <- GET(URLencode(query))
  content(response, type='text/xml', encoding = 'UTF-8')
}

searchSynsetsAPI('кот')

#' Get definitions
#' 
#' @param page Page
#' @param id Id
#' @export
getDefinitionsAPI <- function(page=1, id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'definitions', sep='')
  if (id != '')
    query <- paste(query, '/', id, '.xml', sep='')
  else
    query <- paste(query, '.xml?page=', page, sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}

# xmlValue(xmlChildren(xmlChildren(getDefinitionsAPI(id=0))$definition)$text)

#' Определения, привязанные в синсетах к слову с указанным идентификатором id
#' 
#' @param id Id
#' @export
getWordDefinitionsAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words/', word_id, '/definitions.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}

#' Определения из «сырых» словарей для слова с указанным идентификатором id
#' 
#' @param id Id
#' @export
getWordRawDefinitionsAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words/', word_id, '/raw_definitions.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}

# sapply(getNodeSet(getWordDefinitionsAPI(113), "//definitions/definition/text"), function(definition) xmlValue(definition))


#' Примеры употребления слова, привязанные в синсетах к слову с указанным идентификатором id
#' 
#' @param id Id
#' @export
getExamplesAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words/', word_id, '/examples.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}


#' Примеры употребления из «сырых» словарей для слова с указанным идентификатором id
#' 
#' @param id Id
#' @export
getRawExamplesAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words/', word_id, '/raw_examples.xml', sep='')
  response <- GET(query)
  content(response, type='text/xml', encoding = 'UTF-8')
}
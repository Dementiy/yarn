#' Get a list of all definitions with page breakdown
#' 
#' @param page The page number
#' @param id The input definition id
#' @return A list of objects holding definitions for a given id
#' @references
#' Dmitriy Ustalov. YARN API. \url{http://nlpub.ru/YARN/API}
#' @examples
#' definition_objs <- getDefinitionsAPI(page=1)
#' definitions2df(definition_objs)
#' 
#' definition_obj <- getDefinitionsAPI(id=1)
#' definitions2df(definition_obj)
#' @export
getDefinitionsAPI <- function(page=1, id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'definitions', sep='')
  if (id != '')
    query <- paste(query, '/', id, '.xml', sep='')
  else
    query <- paste(query, '.xml?page=', page, sep='')
  response <- GET(query)
  definition_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(definition_objects, "//definition")
}


#' Definitions, bound to the given word id in synset
#' 
#' @param id The input word id
#' @return A list of objects holding definitions for a given word id
#' @seealso \code{\link{getWordRawDefinitionsAPI}}
#' @examples
#' word_id <- getIdByWordAPI("машина")
#' definitions2df(getWordDefinitionsAPI(word_id))
#' @export
getWordDefinitionsAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'words/', word_id, '/definitions.xml', sep='')
  response <- GET(query)
  definition_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(definition_objects, "//definition")
}


#' Convert a list of definitions to data frame
#' 
#' @param definition_objects A list with objects of class Definition
#' @export
definitions2df <- function(definition_objects) {
  definitions <- as.data.frame(
    do.call(rbind,
            lapply(definition_objects,
                   function(definition_object) 
                     c(id=xmlValue(xmlChildren(definition_object)$id),
                       text=xmlValue(xmlChildren(definition_object)$text)))
    )
  )
  rownames(definitions) <- NULL
  definitions
}


#' Definitions from the "raw" dictionaries for a given word id
#' 
#' @param id The input word id
#' @return A list of objects holding definitions for a given word id
#' @seealso \code{\link{getWordDefinitionsAPI}}
#' @examples
#' word_id <- getIdByWordAPI("машина")
#' definitions2df(getWordRawDefinitionsAPI(word_id))
#' @export
getWordRawDefinitionsAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'words/', word_id, '/raw_definitions.xml', sep='')
  response <- GET(query)
  definition_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(definition_objects, "//definition")
}


#' Word usage examples, bound to the given word id in synsets
#' 
#' @param id The input word id
#' @return A list of objects holding examples for a given word id
#' @seealso \code{\link{getRawExamplesAPI}}
#' @examples
#' word_id <- getIdByWordAPI("машина")
#' examples2df(getExamplesAPI(word_id))
#' @export
getExamplesAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'words/', word_id, '/examples.xml', sep='')
  response <- GET(query)
  example_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(example_objects, "//example")
}


#' Word usage examples from the "raw" dictionaries for a given word id
#' 
#' @param id The input word id
#' @return A list of objects holding examples for a given word id
#' @seealso \code{\link{getExamplesAPI}}
#' @examples
#' word_id <- getIdByWordAPI("машина")
#' examples2df(getRawExamplesAPI(word_id))
#' @export
getRawExamplesAPI <- function(word_id='') {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(.yarn$domain, 'words/', word_id, '/raw_examples.xml', sep='')
  response <- GET(query)
  raw_example_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(raw_example_objects, "//example")
}


#' Convert a list of examples to data frame
#' 
#' @param example_objects A list with objects of class Example
#' @export
examples2df <- function(example_objects) {
  examples <- as.data.frame(
    do.call(rbind,
            lapply(example_objects,
                   function(example_object) 
                     c(id=xmlValue(xmlChildren(example_object)$id),
                       text=xmlValue(xmlChildren(example_object)$text),
                       source=xmlValue(xmlChildren(example_object)$source)))
    )
  )
  rownames(examples) <- NULL
  examples
}
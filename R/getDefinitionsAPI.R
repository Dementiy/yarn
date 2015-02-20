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
  definition_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(definition_objects, "//definition")
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
  # content(response, type='text/xml', encoding = 'UTF-8')
  definition_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(definition_objects, "//definition")
}


#' Преобразовать список объектов во фрейм данных
#' 
#' @param definitions
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
  # content(response, type='text/xml', encoding = 'UTF-8')
  definition_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(definition_objects, "//definition")
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
  # content(response, type='text/xml', encoding = 'UTF-8')
  example_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(example_objects, "//example")
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
  # content(response, type='text/xml', encoding = 'UTF-8')
  raw_example_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(raw_example_objects, "//example")
}

#' Преобразовать список объектов во фрейм данных
#' 
#' @param examples
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
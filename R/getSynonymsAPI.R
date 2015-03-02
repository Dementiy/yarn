#' Get synonymous for a given word id (require registration)
#' 
#' @param word_id Id
#' @return The output is an array with objects of class Word
#' @export
getSynonymsAPI <-function(word_id) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words/', word_id, '/synonyms.xml', sep='')
  response <- GET(query)
  synonyms_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(synonyms_objects, "//words/word")
}

# words2df(getSynonymsAPI(131))

#' Get raw synonymous for a given word id (require registration)
#' 
#' @param word_id Id
#' @return The output is an array with objects of class Word
#' @export
getRawSynonymsAPI <-function(word_id) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr package needed for this function to work. Please install it.", .call = FALSE)
  }
  
  query <- paste(domain, 'words/', word_id, '/raw_synonyms.xml', sep='')
  response <- GET(query)
  raw_synonyms_objects <- content(response, type='text/xml', encoding = 'UTF-8')
  getNodeSet(raw_synonyms_objects, "//words/word")
}
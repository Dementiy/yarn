% Generated by roxygen2 (4.0.2): do not edit by hand
\name{getDefinitionsAPI}
\alias{getDefinitionsAPI}
\title{Get a list of all definitions with page breakdown}
\usage{
getDefinitionsAPI(page = 1, id = "")
}
\arguments{
\item{page}{The page number}

\item{id}{The input definition id}
}
\value{
A list of objects holding definitions for a given id
}
\description{
Get a list of all definitions with page breakdown
}
\examples{
\dontrun{
definition_objs <- getDefinitionsAPI(page=1)
definitions2df(definition_objs)

definition_obj <- getDefinitionsAPI(id=1)
definitions2df(definition_obj)
}
}
\references{
Dmitriy Ustalov. YARN API. \url{http://nlpub.ru/YARN/API}
}


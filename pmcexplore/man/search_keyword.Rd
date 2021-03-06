% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_keyword.R
\name{search_keyword}
\alias{search_keyword}
\title{Search a database by keyword}
\usage{
search_keyword(keyword, n = 100, db = "pmc")
}
\arguments{
\item{keyword}{A string of one of multiple keywords, separated by a space.}

\item{n}{An integer number of document IDs to return, defaults to 100.}

\item{db}{A string specifying the dataset to pull the XML from, defaults to "pmc" (PubMed Central).
Other database options include "pubmed" (PubMed), "mesh" (MeSH), and "nlmcatalog" (NLM Catalog).
The full list of database options are available on the NCBI E-utilities guide.}
}
\value{
A tibble with one column (labeled PMCID) that contains all the PMCID
numbers returned from the database search
}
\description{
Search a database by a keyword or keywords to get a list of documents
that correspond to the input keyword. Uses the ESearch functionality of
E-utilities to search a specified database. This function currently only
returns documents that are NIH-funded and open access.
}
\examples{
search_keyword("protein")
search_keyword("data", n = 20)

}

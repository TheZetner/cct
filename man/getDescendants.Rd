% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lineages.R
\name{getDescendants}
\alias{getDescendants}
\title{Get Descendants from a Specific Lineage}
\usage{
getDescendants(x, node_name, table = FALSE, vector = FALSE)
}
\arguments{
\item{x}{node network produce by \code{fromDFtoNetwork()}}

\item{node_name}{Lineage of interest ie. "BA.1"}

\item{table}{logical, return tree as dataframe of level/lineage}

\item{vector}{logical, return vector  of lineage names}
}
\value{
one of three options depending on table and vector params
\itemize{
\item a Node class network object of the nodes beneath node_name
\item a table from the node network with level and lineage names
\item a vector of lineage names
}
}
\description{
Get a lineage's descendants out of a \code{fromDFtoNetwork()}node network.
Vector being TRUE supersedes table being TRUE.
Default return node network.
}

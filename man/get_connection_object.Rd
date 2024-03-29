% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_connection_object.R
\name{get_connection_object}
\alias{get_connection_object}
\title{Set up a database connection}
\usage{
get_connection_object(dsn)
}
\arguments{
\item{dsn}{A string of text representing the name of a DSN entry}
}
\value{
either a DBMS connection object, or an ODBC connection object, from DBI::dbConnect
}
\description{
Set up a database connection
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Create a map of FARS accidents in a particular state}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{A scalar of a type that can be coerced to an integer of the integer identifier of the state.
A list of the state identifying integers for the FARS data can be found
\href{https://crashstats.nhtsa.dot.gov/#/DocumentTypeList/4}{here}.  If \code{state.num} is
not coercible to an integer or if the resulting integer is not a valid state ID in FARS, an error
will be thrown}

\item{year}{A scalar four-digit year of a type that can be coerced to integer representing the year
of data to be mapped.  Currently, only the years 2013-2015 are in the data.  If \code{year} cannot be
coerced to integer or if it does not refer to a valid four-digit year in the data, an error will
be thrown}
}
\value{
This function returns a graphical map of the locations of all of the accidents in the
   FARS database in the given state in the given year.  Each accident is a dot, and state outlines
   are shown.  If either \code{state.num} or \code{year} cannot be coerced to integer or do not
   refer to a valid identifier, an error is thrown instead
}
\description{
This function returns a map showing the geographic locations of all of the accidents in the FARS
database in a particular state for a particular year.  Each accident is a dot in the outline of the
state
}
\details{
The FARS data can be found at the National Highway Traffic Safety Association
     \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}.
     2013-2015 data is distributed with this package.  The location of the files can be found using
     \code{system.file('extdata',package='FARScoursera')}
     These files need to be in your working directory for this function to work correctly.
}
\examples{
\dontrun{fars_map_state(1,2015)}
\dontrun{fars_map_state(6,2013)}

}


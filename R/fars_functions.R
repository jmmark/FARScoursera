#' Load a file from the FARS database
#'
#' A function for loading the FARS data as specified by \code{filename}.
#' If \code{filename} exists it returns the data as a dplyr \code{\link{dplyr::tbl_df}}.  If it
#' does not exist, it stops with an error.
#'
#' @param filename A string containing the filename to be read from the FARS data.
#'      be sure to include a valid path if the data are not in the current working directory.
#'      See details below for finding the data
#'
#' @return This function returns a dplyr tibble containing the data.  If \code{filename} is not valid,
#'      the function stops with an error message
#'
#' @details The FARS data can be found at the National Highway Traffic Safety Association
#'      \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}.
#'      If you choose to download it directly, be sure to include the correct path to the data.
#'
#' @details  Data for 2013-2015 are included in the distribution of this package in the \code{extdata}
#'      directory.  To find it, use the \code{system.file()} function.  For example,
#'      \code{system.file('extdata','accident_2013.csv.bz2',package = 'FARScoursera')}
#'
#' @examples
#' fars_read('accident_2013.csv.bz2')
#' fars_read(system.file('extdata','accident_2013.csv.bz2',package = 'FARScoursera'))
#' fars_read(make_filename(2013))
#' fars_read('not_a_valid_file') #shows the expected errors
#'
#' @seealso \code{\link{make_filename}} for convenient creation of valid \code{filename} input
#'
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create a vaild FARS filename for a given year
#'
#' FARS files are of the form 'accident_YEAR.csv.bz2'.  This function takes \code{year}
#' as an input, and returns the file name in the correct form.  \code{year} must be a scalar
#' of a type coercible into an integer, otherwise it will throw an error.  No guarantee is made
#' that the file of the name created by this function exists
#'
#' @param year A four-digit year, in a format coerceable into an integer.  Currently, only files
#' covering 2013-2013 are valid.  If \code{year} cannot be coerced to an integer, the function will
#' throw an error
#'
#' @return This function returns a string of the form 'accident_YEAR.csv.bz2', with YEAR replaced
#' by whatever is passed by \code{year}.  A string is returned, although
#' a warning is givin if \code{year} cannot be coerced
#' to an integer, even though that may not form a valid filename.  No path is prepended
#'
#'
#'
#'
#' @examples
#' make_filename(2013)
#' make_filename('2014')
#' fars_read(make_filename(2013))
#' make_filename(12345) # still returns a filename, but the file does not exist
#'
#' @seealso \code{\link{fars_read}} To easily load the file named in the returned string
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Load multiple years of FARS data
#'
#' This function takes a vector of \code{years}, and returns a list of dplyr tibbles containting the
#' FARS data for that year.  The tibble contians only the month of the accident an the year in the
#' format of the \code{years} vector.  This is primarily a helper function for
#' \code{\link{fars_summarize_years}} as much of the data contained in the files is lost.
#'
#' Any component of \code{years} which does not correspond to a valid filename will produce a warning, but a list
#' will still be returned, with NULL in position for each invalid component.
#'
#' @param years A vector of four-digit years of the FARS data to load.  Currently only data from 2013-2015 are available.
#'    \code{years} must be coerceable to an integer, or else an error will be generated.
#'
#' @return This function returns a list of dplyr tibbles containing FARS data for each year in \code{years}.
#'    The tibbles contain only the month and year of each accident, so the list is mostly of use as an
#'    input into other functions.   If any element in \code{years} does not correspond to a valid FARS data
#'    file, a warning is generated and the corresponding component of the returned list is NULL
#'
#'
#' @details The FARS data can be found at the National Highway Traffic Safety Association
#'      \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}.
#'      2013-2015 data is distributed with this package.  The location of the files can be found using
#'      \code{system.file('extdata',package='FARScoursera')}
#'      These files need to be in your working directory for this function to work correctly.
#'
#' @examples
#' fars_read_years(2013:2015))
#' fars_read_years(c('2013','2014','2015'))
#' fars_read_years(2012:2016) # shows effect of invalid years in input
#'
#' @importFrom dplyr %>%
#'
#' @seealso \code{\link{fars_read}}
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Monthly count of accidents by year
#'
#' This function produces a monthly count of the number of accidents in the FARS database for each year
#' given in \code{years}.
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a dplyr tibble with the month in rows and the year in columns.
#'     The entries are the total number of accidents in that year / month.  If \code{years} is a mix
#'     of valid and invalid years, the tibble is still returned by with warnings.  If \code{years}
#'     does not contain any valid years, then an error is thrown
#'
#' @details The FARS data can be found at the National Highway Traffic Safety Association
#'      \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}.
#'      2013-2015 data is distributed with this package.  The location of the files can be found using
#'      \code{system.file('extdata',package='FARScoursera')}
#'      These files need to be in your working directory for this function to work correctly.
#'
#' @examples
#' fars_summarize_years(c('2013','2014'))
#' fars_summarize_years(2013:2014)
#'
#' @importFrom dplyr %>%
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Create a map of FARS accidents in a particular state
#'
#' This function returns a map showing the geographic locations of all of the accidents in the FARS
#' database in a particular state for a particular year.  Each accident is a dot in the outline of the
#' state
#'
#' @param state.num A scalar of a type that can be coerced to an integer of the integer identifier of the state.
#'    A list of the state identifying integers for the FARS data can be found
#'    \href{https://crashstats.nhtsa.dot.gov/#/DocumentTypeList/4}{here}.  If \code{state.num} is
#'    not coercible to an integer or if the resulting integer is not a valid state ID in FARS, an error
#'    will be thrown
#' @param year A scalar four-digit year of a type that can be coerced to integer representing the year
#'    of data to be mapped.  Currently, only the years 2013-2015 are in the data.  If \code{year} cannot be
#'    coerced to integer or if it does not refer to a valid four-digit year in the data, an error will
#'    be thrown
#'
#' @return This function returns a graphical map of the locations of all of the accidents in the
#'    FARS database in the given state in the given year.  Each accident is a dot, and state outlines
#'    are shown.  If either \code{state.num} or \code{year} cannot be coerced to integer or do not
#'    refer to a valid identifier, an error is thrown instead
#'
#' @details The FARS data can be found at the National Highway Traffic Safety Association
#'      \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{here}.
#'      2013-2015 data is distributed with this package.  The location of the files can be found using
#'      \code{system.file('extdata',package='FARScoursera')}
#'      These files need to be in your working directory for this function to work correctly.
#'
#' @examples
#' fars_map_state(1,2015)
#' fars_map_state(6,2013)
#'
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}

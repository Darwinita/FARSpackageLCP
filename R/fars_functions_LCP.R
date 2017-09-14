#' @name fars_read
#'
#' @title fars_read
#'
#' @description A read data file function.
#'
#' \code{fars_read} This function reads a filename from the Fatality Analysis
#' Reporting System (FARS) into a dplyr version of an R data frame. It stops if
#' the file does not exist.
#'
#' @references
#'   \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#'
#' @param filename Is a character string giving the file name of the input data
#'   file.
#'
#' @return If a file name is provided this functions returns a dplyr version of
#'   an R data frame with the data contained in filename. Otherwise it stops.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2015.csv.bz2")
#' }
#'
#' \dontrun{
#' fars_read()
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = TRUE, quote = "")
        })
        dplyr::tbl_df(data)
}

#' @name make_filename
#'
#' @title make_filename
#'
#' @description A simple function to create the filename of accident data for a given year.
#'
#' \code{make_filename} This function takes an user provided year to generate a
#' filename from FARS.
#'
#' @references
#'   \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @param year A numeric corresponding to the user provided year.
#'
#' @return A character string corrresponding to the full file name with the
#'   format. accident_year.csv.bz2.
#'
#' @details The function explictly converts the numeric to an integer.
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        system.file("extdata", sprintf("accident_%d.csv.bz2", year), package ="FARSpackageLCP")
        # sprintf("accident_%d.csv.bz2", year)
        #filename <- sprintf("accident_%d.csv.bz2", year)
        #system.file("extdata", filename, package="BuildinganRPackageLCP")
}

#' @name fars_read_years
#'
#' @title fars_read_years
#'
#' @description A function to extraxct monthly data from a given set of years.
#'
#' \code{fars_read_years} This function uses an user provided series of years to
#' extract their FARS monthly data
#'
#' @references
#' \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @param years Is a numeric vector corresponding to the series of years for
#'   which you want to extract monthly data.
#'
#' @return A list of subsetted dataframes in dplyr format with the monthly data from
#'   every year included in years.
#'
#' @details For each year provided in the numeric vector argument to the
#'   function, the make_filename function from this package generates its
#'   corresponding filename. The data from each file will then be extracted
#'   using the fars_read function from this package. Subsequently, the monthly
#'   data is subsetted by means of the dplyr functions mutate and select. The
#'   functions evaluates the code and assigns a warning handler to years for
#'   which data are not available.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr "%>%"
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013,2014,2015))
#'}
#' \dontrun{
#' fars_read_years(c(2016,2017))
#' }
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                filename <- make_filename(year)
                print(filename)
                tryCatch({
                        dat <- fars_read(filename)
                        dplyr::mutate(dat, year = ~year) %>%
                                dplyr::select("MONTH", "year")
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' @name fars_summarize_years
#'
#' @title fars_summarize_years
#'
#' @description A function to summarize FARS events per month and year.
#'
#' \code{fars_summarize_years} This function uses an user provided series of
#' years to create a summary of the total number of FARS events per month and
#' year.
#'
#' @references
#' \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @param years Is a numeric vector corresponding to a series of years for which
#'   to summarize FARS data.
#'
#' @return A dataframe with the summary of fars events per month for every given
#'   year.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014,2015))
#' }
#' \dontrun{
#' fars_summarize_years(c(2016,2017))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(~year, ~MONTH) %>%
                dplyr::summarize(n = ~n()) %>%
                tidyr::spread("year", "n")
}

#' @name fars_map_state
#'
#' @title fars_map_state
#'
#' @description A function to create a map of FARS events and points a plot for a given a
#' state number and year.
#'
#' \code{fars_map_state} This function uses an user provided state number and
#' year to create a map of FARS events and their corresponding points plot.
#'
#' @references
#' \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @param state.num Is a numeric corresponding to the user provided state
#'   number.
#' @param year Is a numeric corresponding to the user provided year.
#'
#' @return a map of FARS events per state and year.
#'
#' @details The make_filename function from this package generates a filename
#'   for the input year. The data from the generated file is then extracted
#'   using the fars_read function from this package. The state number is
#'   explicitly converted into an integer. The function checks if the year is
#'   not included in the dataset and stops in that case. If no accidents are reported for
#'   that state, a warning message is returned. LONGITUD and LATITUDE values are
#'   converted to NA if higher than 900 or 90, respectively.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(28,2015)
#' }
#' \dontrun{
#' fars_map_state(3,2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, ~STATE == state.num)
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

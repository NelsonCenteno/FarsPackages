#' Title fars_read
#'
#' This function allows a CSV extension file and returns a tbl_df with the data
#' If the parameter filnename does not exist, return an error
#'
#' @import readr dplyr
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename a character of filename of a csv file to read
#'
#' @return a tbl_df with the data of the CSV file.
#'     The file must exist, because an error message will be show if isnt
#'
#' @source extdate/accident_xxxx.csv.bz2
#'
#' @examples
#' \dontrun{x <- fars_read('myFile.csv')}
#' \dontrun{system.file("extdata", "accident_xxxx.csv.bz2", package = "packFars") }
#'
#' @export


fars_read <- function(filename) {
  # Checks if the file exists
  if(!file.exists(filename))
    # If the file does not exist
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    # Use read_csv from the readr package
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}



#' Title make_filename
#'
#' This function allows the user to create a string accident data file name by year.
#' bz2 files are compressed files.
#'
#' @param year The year in the name of the file, no format Restriction
#'
#' @note This package does not depends on any extensions
#'
#' @return a filename in the format 'accident_xxxx.csv.bz2'.
#'
#' @source extdate/accident_xxxx.csv.bz2
#'
#' @examples
#' newFileName <- make_filename('2017')
#' \dontrun{system.file("extdata", "accident_xxxx.csv.bz2", package = "packFars")}
#'
#' @export


make_filename <- function(year) {
  # Attempt to coerce to integer type,  the answer will be NA in case the coerce isnt succeeds
  year <- as.integer(year)
  # Return character vector containing formatted combination of text and the input value
  sprintf("accident_%d.csv.bz2", year)
}



#'#' Title fars_read_years
#'
#' This function accepts one or a list of years, calls the previous function make_filename()
#' and with each of the years of the list will run, and  mutate a new column with the year and then select
#' the columns MONTH and year, if the file does not exist an error will be thrown.
#'
#' Uses make_filename(year)
#'      fars_read(file)
#'
#' @param years one or a list of years
#'
#' @import dplyr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#'
#' @return Creates one or more datasets based on year number.  Returns NULL if there is an error
#'
#' @examples
#' \dontrun{
#'      fars_read_years(as.list(2013, 2014, 2015))
#' }
#'
#' @export


fars_read_years <- function(years) {
  # Apply a FUN thar will be created below to run on each list value
  lapply(years, function(year) {
    # use the function make_filename to create a filename
    file <- make_filename(year)
    tryCatch({
      # Read the filene with fars_read function
      dat <- fars_read(file)
      # create a column with the name year
      dplyr::mutate_(dat, year = year) %>%
        # select the month and year colums
        dplyr::select_('MONTH', 'year')
      # A error will be to apears in case of invalid year
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Title fars_summarize_years
#'
#' Accepts one or a list of years and use the function fars_read_years().
#' Uses various dplyr functions count the number of observations by month for a year.
#'
#' @param years One or more years, no error checking
#'
#' @import dplyr tidyr
#' @importFrom  dplyr bind_rows
#' @importFrom  dplyr group_by
#' @importFrom  dplyr summarize
#' @importFrom  tidyr spread
#'
#'
#' @return A wide data frame of counts by month and year,
#'
#' @examples
#' \dontrun{
#'   fars_summarize_years(c(2013, 2014))
#' }
#'
#' @export

fars_summarize_years <- function(years) {
  #  run fars_read_years with the year
  dat_list <- fars_read_years(years)
  # Bind all the rows and use piping
  dplyr::bind_rows(dat_list) %>%
    # Group by the year and Month and use piping
    dplyr::group_by_('year', 'MONTH') %>%
    # Summarise the data and add a count column called n
    dplyr::summarize_(n = ~n()) %>%
    # Spread the data
    tidyr::spread_('year', 'n')
}

#' Title fars_map_state
#'
#' Accepts a state number and year from user and write the filename using the year and the make_filename function
#' gets a data frame from fars_read(), Error checks to make sure the state number exists
#' If so, uses maps and graphics to create plots based on latitude and longitude from the data file
#'
#' @param state.num Number of a state
#' @param year The year in question
#'
#' @import dplyr maps
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @importFrom maps map
#'
#' @return A plot or set of plots based on latitude and longitude from the data file
#'
#' @examples
#' \dontrun{
#'   fars_map_state(2, 2014)
#' }
#'
#'
#' @export
#'

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  STATE <- NULL

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

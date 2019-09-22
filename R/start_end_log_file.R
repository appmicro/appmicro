#' Start a log file
#'
#' start_log_file() is a function that opens a connection and diverts output/
#' messages to a .log file
#'
#' @param file_name path to .log file where diverted output and
#' messages will be written
#'
#' @export
start_log_file <- function(file_name) {
  # open connection
  con <- file(file_name)
  sink(con)
  sink(con, type = "message")

  # print start time
  print(paste0(file_name, ".R"))
  paste("Start Time:", Sys.time())
}

#' End a log file
#'
#' end_log_file() is a function that closes the connection opened by
#' start_log_file()
#'
#' @param log_file if TRUE output/messages are being diverted to a .log
#' file and end_log_file() will close the connection
#'
#' @export
end_log_file <- function(log_file) {
  if (log_file == T) {
    print("-----------------------------------------------------------------")
    paste("Finish Time:", Sys.time())
    print("Done.")
    # close connections
    sink()
    sink(type = "message")
  }
}

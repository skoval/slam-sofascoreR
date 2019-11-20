#' Establish a Databse Connection
#'
#'
#'
#' @return An S4 object that inherits from DBIConnection
#'
#' @export
make_connection <- function(driver = "ODBC Driver 17 for SQL Server", database = "OnCourtDB", uid = "MatchDataUser", pwd = "MatchData2016", server = "DEV-MELDBTA16"){
	
	DBI::dbConnect(odbc::odbc(),
			    driver = driver,
                   database = database,
                   uid = uid,
                   pwd = pwd,
                   server = server)                             
}

make_connection <- function(driver = RMariaDB::MariaDB(), group = "tennis"){
	
	DBI::dbConnect(drv = driver, group = group)                             
}

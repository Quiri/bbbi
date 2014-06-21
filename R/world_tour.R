#' Attempt to apply query to multiple TSO DWHs
#'
#' This function allows you to fetch data from all the TSO databases using your query.
#' @param query SQL query in a text format
#' @param countries Countries you like to query from. Text format. Default is c("DE","PL","FR","BR","CZ","NCSA_ES","US","NL","UK","RU","EMEA_ES","IT","GR","RO","CN","MENA")
#' @usage world_tour assumes you have ODBC setups such that each ODBC is named like TSO_<country> eg. TSO_DE.
#' @keywords query, database, sql, odbc
#' @export
#' @examples dd <- world_tour("SELECT date(datedim) date, dr FROM agg_revenue") 
#' it <- world_tour("SELECT date(datedim) date, dr FROM agg_revenue","IT")
#' @import RODBC
#' plyr
#' 
#' 
world_tour <- function(query,countries = c("DE","PL","FR","BR","CZ","NCSA_ES","US","NL","UK","RU","EMEA_ES","IT","GR","RO","CN","MENA")){
  library(RODBC)
  library(plyr)
  
  basef <- function(ctr){
    DSN <- paste0('TSO_',ctr)
    conn <-odbcConnect(dsn=DSN)
    raw <- sqlQuery(conn,query , stringsAsFactors=FALSE)
    raw$country <- ctr
    return(raw)
    close(conn)
  }
  
  Fu <- function (ctr) {
    return(tryCatch(basef(ctr), error=function(e) NULL))
  }
  
  flist <- lapply(countries,Fu)
  dat <- ldply(flist,data.frame)

  return(dat)

}
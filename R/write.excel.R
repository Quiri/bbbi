#' write.excel
#'
#' Write data in xlsx format just as easily as write_csv
#' @keywords excel, xlsx
#' @param data data you like to save in Excel format
#' @param sheet Sheet name (default "data")
#' @param file File name (default "data.xlsx")
#' @param row.names If you like to include row names, logical (default F)
#' @export  
#' @examples data(iris)
#' write.excel(iris)
#' @import xlsx
#' 
#' 
#' 
write.excel <- function(data,sheet='data',file='data.xlsx',row.names=F){
  library(xlsx)
  outwb <- createWorkbook()
  Sheet<- createSheet(outwb, sheetName = sheet)
  addDataFrame(data, Sheet,  startRow=1, startColumn=1,row.names=row.names)
  saveWorkbook(outwb, file)
}
#' Table converting function
#'
#' This funcition converts your dataframe to jira format. It prints the jira code to your console.
#' @param df This is the dataframe you want to convert to jira format
#' @usage df2jira(df)
#' @keywords dataframe, jira, table
#' @export
#' @examples df2jira(df)
#' @import 
#' 
#' 
#' 
df2jira <- function(res) {
  res <- data.frame(lapply(res, as.character), stringsAsFactors=FALSE)
  # Heading
  cat("|")
  cat(paste0("|", names(res)))
  cat("|| \n")
  for (i in 1:nrow(res)) {
    cat(paste0("|", res[i,]))
    cat("| \n")
  }
}


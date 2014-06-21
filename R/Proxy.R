#' Proxy
#'
#' Set BlueByte company proxy
#' @usage Set BlueByte company proxy so you can work with github or API related tasks.
#' @keywords proxy,firewall
#' @export
#' @examples Proxy()
#' @import httr
#' 
#' 
#' 
Proxy <- function(){
  library(httr)
  set_config(use_proxy(url="10.26.0.16", port=3128))
}
extrai_nos <- function(str){
  
  pat <- "http://twitter.com/(.+)/statuses"
  stringr::str_match(str, pat)[,2]
}
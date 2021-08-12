.onLoad <- function(libname = find.package("ltmc"), pkgname = "ltmc") {
  
  #' @importFrom magrittr "%>%"
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      c(".",".data")
    )
  
  
  invisible(NULL)
}

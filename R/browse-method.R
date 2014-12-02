#' view html object in a browser
#' 
#' a story to be told
#' 
#' @param object the object to be viewed
#' @param ... further arguments
#' @exportMethod browse
#' @rdname browse
#' @aliases browse browse,html-method browse,data.frame-method
setGeneric("browse", function(object, ...){standardGeneric("browse")})

#' @rdname browse
setMethod("browse", "html", function(object){
  tmpFile <- tempfile(fileext=".html")
  cat(object, file=tmpFile)
  browseURL(tmpFile)
})

#' @rdname browse
setMethod("browse", "data.frame", function(object){
  html <- as.DataTables(object)
  browse(html)
})
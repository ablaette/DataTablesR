#' turn object into html using DataTables
#'
#' a story to be told
#'
#' @param object the relevant object 
#' @param align defaults to null, if supplied, same length as table
#' @param jsDir pass a specified path
#' @param ... further arguments
#' @return a html object
#' @exportMethod as.DataTables
#' @rdname as.DataTables
#' @aliases as.DataTables as.DataTables,data.frame-method
setGeneric("as.DataTables", function(object, ...){standardGeneric("as.DataTables")})

#' @rdname as.DataTables
setMethod("as.DataTables", "data.frame", function(object, align=NULL, jsDir=NULL){
  htmlTemplate <- scan(
    file=system.file("templates", "dataTableTemplate.html", package="DataTablesR"),
    what="character", sep="\n", quiet=TRUE
  )
  htmlTable <- paste(htmlTemplate, collapse="\n")
  lookUp <- c("jquery.dataTables.css", "jquery.js", "jquery.dataTables.min.js")
  if (is.null(jsDir)){
    dataTablesPath <- sapply(lookUp, function(x) system.file("js", x, package="DataTablesR"))    
  } else {
    dataTablesPath <- sapply(lookUp, function(x) file.path(jsDir, x))
  }
  for (x in names(dataTablesPath)) htmlTable <- gsub(x, dataTablesPath[x], htmlTable)
  if (!is.null(align)){
    if (length(align) != ncol(object)) {
      warning("if supplied, length of align needs to correspond to number of columns")
    } else {
      align2css <- list("l"="alignLeft", "c"="alignCenter", "r"="alignRight")
      colDefs <- sapply(
        c(1:length(align)),
        function(x){
          sprintf('{"aTargets" : [ %d ], "sClass" : "%s"}', x-1, align2css[[ align[x] ]]  )
        })
      format <- paste('{"aoColumnDefs" : [', paste(colDefs, collapse=", "),']}', sep="")
      htmlTable <- gsub(
        "\\.DataTable\\(\\);",
        paste(".DataTable(", format, ");"),
        htmlTable
        )
    }
  }
  thead <- sapply(
    colnames(object),
    function(x){
      paste("\t<th>", x, "</th>", sep="")
    }
  )
  thead <- paste("<tr>", paste(thead, collapse="\n"), "</tr>", sep="\n")
  htmlTable <- gsub("THEAD", thead, htmlTable)
  tbody <- sapply(
    c(1:nrow(object)),
    function(i){
      td <- sapply(object[i,], function(j) paste("\t<td>", as.character(j), "</td>", sep=""))
      td <- paste(td, collapse="\n")
      tr <- paste("<tr>\n", td, "\n</tr>\n", sep="")
    })
  tbody <- paste(tbody, collapse="")
  htmlTable <- gsub("TBODY", tbody, htmlTable)
  class(htmlTable) <- "html"
  htmlTable
})

#' @rdname as.DataTables
#' @exportMethod show
setMethod("show", "html", function(object){
  tmpFile <- tempfile(fileext=".html")
  cat(object, file=tmpFile)
  browseURL(tmpFile)
})




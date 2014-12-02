#' turn object into html using DataTables
#'
#' a story to be told
#'
#' @param object the relevant object 
#' @param ... further arguments
#' @return a html object
#' @exportMethod as.DataTables
#' @rdname as.DataTables
#' @aliases as.DataTables as.DataTables,data.frame-method
setGeneric("as.DataTables", function(object, ...){standardGeneric("as.DataTables")})

#' @rdname as.DataTables
setMethod("as.DataTables", "data.frame", function(object){
  lookUp <- c("jquery.dataTables.css", "jquery.js", "jquery.dataTables.min.js")
  dataTablesPath <- sapply(lookUp, function(x) system.file("js", x, package="DataTablesR"))
  #  dataTablesPath["jquery.dataTables.css"] <- "/Users/blaette/Lab/github/DataTablesR/inst/js/jquery.dataTables.css"
  htmlTemplate <- scan(
    file=system.file("templates", "dataTableTemplate.html", package="DataTablesR"),
    what="character", sep="\n", quiet=TRUE
  )
  #   htmlTemplate <- scan(
  #     file="/Users/blaette/Lab/github/polmineR/inst/html/dataTableTemplate.html",
  #     what="character", sep="\n"
  #   )
  htmlTable <- paste(htmlTemplate, collapse="\n")
  for (x in names(dataTablesPath)){
    htmlTable <- gsub(x, dataTablesPath[x], htmlTable)
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


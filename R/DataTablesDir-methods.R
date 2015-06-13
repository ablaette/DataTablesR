setOldClass("DataTablesDir")

setMethod("show", "DataTablesDir", function(object){
  browseURL(paste(object, "/index.html", sep=""))
})

setMethod("print", "DataTablesDir", function(x){
  browseURL(paste(x, "/index.html", sep=""))
})

# setMethod("browse", "DataTablesDir", function(object){
#  browseURL(paste(object, "/index.html", sep=""))
# })


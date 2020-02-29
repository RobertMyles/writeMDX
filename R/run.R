#' Run the command line interface
# Inspired by: https://github.com/ankane/jetpack/blob/master/R/run.R
#' @importFrom docopt docopt
#' @export
#' @keywords internal
run <- function() {

  doc <- "Usage:
    writeMDX <input>
    writeMDX help"

  opts <- NULL
  tryCatch({
    opts <- docopt(doc)
  }, error = function(err){
    msg <- conditionMessage(err)
    if (!grepl("usage:", msg)) {
      warning(msg)
    }
    message(doc)
    quit(status = 1)
  })

  tryCatch({
    if(opts$help){
      message(doc)
    } else{
      writeMDX(input = opts$input,
               config = list(
                 include = list("date", "title", "featuredImage"),
                 exclude = list("author", "output")
               ))
    }
  }, error=function(err) {
    msg <- conditionMessage(err)
    quit(status = 1)
  })
}

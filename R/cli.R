#' Install the command line interface
# Inspired by: https://github.com/ankane/jetpack/blob/master/R/cli.R
#' @export
#' @examples \dontrun{
#' writeMDX::cli()
#' }
cli <- function() {
  if (.Platform$OS.type != "unix") {
    file <- "C:/ProgramData/writeMDX/bin/writemdx.cmd"
    rscript <- file.path(R.home("bin"), "Rscript.exe")
    dir <- dirname(file)
    if (!file.exists(dir)) {
      dir.create(dir, recursive=TRUE)
    }
    write(paste0("@", rscript, " -e \"writeMDX::run()\" %* "), file = file)
    message(paste("Wrote", windowsPath(file)))
    message(paste0("Be sure to add '", windowsPath(dir), "' to your PATH"))
  } else {
    file <- "/usr/local/bin/writeMDX"
    write("#!/usr/bin/env Rscript\n\nwriteMDX::run()", file = file)
    Sys.chmod(file, "755")
    message(paste("Wrote", file))
  }
}

windowsPath <- function(path) {
  gsub("/", "\\\\", path)
}

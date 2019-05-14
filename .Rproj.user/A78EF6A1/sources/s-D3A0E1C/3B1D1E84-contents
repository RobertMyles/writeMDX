#' @importFrom rmarkdown render
#' @title writeMDX
#' @description write and Rmarkdown (.Rmd) file to MDX (.mdx) format.
#' @param input Input file (.Rmd format).
#' @example
#' \dontrun{
#' # supposing you have a 'test.Rmd' file:
#' writeMDX("test.Rmd")
#' }
#' @export
writeMDX <- function(input){
  rmarkdown::render(input, mdx_format())
}

#' @importFrom rmarkdown render
#' @title writeMDX
#' @description write and Rmarkdown (.Rmd) file to MDX (.mdx) format.
#' @param input Input file (.Rmd format).
#' @param config \code{list}. Configurable YAML fields to check for. Default is
#' config = list("date", "title", "featuredImage")
#' @examples
#' \dontrun{
#' # supposing you have a 'test.Rmd' file:
#' writeMDX("test.Rmd")
#' }
#' @export
writeMDX <- function(input, config = list("date", "title", "featuredImage")){
  rmarkdown::render(input, mdx_format(cfig = config))
}

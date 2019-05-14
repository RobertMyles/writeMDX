# These functions are from the rmarkdown R package (https://github.com/rstudio/rmarkdown)
# they are included here as dependencies for writeMDX()

# read and write UTF-8
read_utf8 <- function(file, encoding = 'UTF-8') {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = encoding); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

write_utf8 <- function (text, con, ...) {
  opts <- options(encoding = "native.enc"); on.exit(options(opts), add = TRUE)
  writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
}

#-------------------------------------------------------
# blank check
is_blank <- function (x){
  if (length(x))
    all(grepl("^\\s*$", x))
  else TRUE
}

#------------------------------------------------------
# preserve YAML
partition_yaml_front_matter <- function(input_lines){
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] >
                                    1) && grepl("^---\\s*$", input_lines[delimiters[1]])) {
      if (delimiters[1] == 1)
        TRUE
      else is_blank(input_lines[1:delimiters[1] - 1])
    }
    else {
      FALSE
    }
  }
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {
    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]
    input_body <- c()
    if (delimiters[1] > 1)
      input_body <- c(input_body, input_lines[1:delimiters[1] -
                                                1])
    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body, input_lines[-(1:delimiters[2])])
    list(front_matter = front_matter, body = input_body)
  }
  else {
    list(front_matter = NULL, body = input_lines)
  }
}

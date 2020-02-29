mdx_format <- function(variant = "markdown", preserve_yaml = TRUE,
                       dev = "png", df_print = "tibble", fig_width = 7,
                       fig_height = 5, cfig) {
  args <- ""

  # add post_processor for yaml preservation
  post_processor <- function(metadata, input_file, output_file, clean, verbose) {
      input_lines <- read_utf8(input_file)
      partitioned <- partition_yaml_front_matter(input_lines)

      if (length(cfig) > 0) {
        yaml_headers <- partitioned$front_matter
        last <- length(yaml_headers)
        yaml_headers <- yaml_headers[-last]
        yaml_headers <- yaml_headers[-1]
        if (length(cfig$include) > 0) {
          yaml_fields_include <- unlist(cfig$include)
        } else {
          yaml_fields_include <- NULL
        }
        if (length(cfig$exclude) > 0) {
          yaml_fields_exclude <- unlist(cfig$exclude)
        } else {
          yaml_fields_exclude <- NULL
        }
        if (!is.null(yaml_fields_include)) {
          for (i in 1:length(yaml_fields_include)) {
            param <- yaml_fields_include[[i]]
            present <- which(grepl(param, yaml_headers))
            if (length(present) > 1) stop("There are duplicate YAML header entries.")
            if (length(present == 1) & param == "date") {
              date_string <- str_after_first(yaml_headers[[present]], "date: ")
              date_string <- gsub(x = date_string, pattern = '"', replacement = "")
              first <- strex::str_before_first(date_string, "/")
              if (nchar(first) < 2) first <- paste0("0", first)
              second <- str_after_first(date_string, "/")
              third <- str_after_last(second, "/")
              second <- str_before_first(second, "/")
              if (nchar(second) < 2) second <- paste0("0", second)
              # account for RStudio default RMarkdown creating dates as "2/28/2020"
              date_out <- paste0(third, "-", first, "-", second)
              date_position <- which(grepl("date", partitioned$front_matter))
              yaml_date_out <- paste0("date: '", date_out, "'")
              partitioned$front_matter[date_position] <- yaml_date_out
            } else if (length(present) < 1) {
              stop(paste0("Config item '", param, "' is missing from YAML."))
            }
          }
        }
        if (!is.null(yaml_fields_exclude)) {
          for (j in 1:length(yaml_fields_exclude)) {
            param <- yaml_fields_exclude[[j]]
            take_out <- which(grepl(param, partitioned$front_matter))
            if (length(take_out) > 0) {
              partitioned$front_matter <- partitioned$front_matter[-take_out]
            }
          }
        }
      }
      if (!is.null(partitioned$front_matter)) {
        output_lines <- c(partitioned$front_matter, "", read_utf8(output_file))
        write_utf8(output_lines, output_file)
      }
      output_file
    }

  # return format
  rmarkdown:::output_format(
    knitr = rmarkdown:::knitr_options_html(fig_width, fig_height, fig_retina = NULL, FALSE, dev),
    pandoc = rmarkdown:::pandoc_options(
      to = variant,
      from = rmarkdown::from_rmarkdown(),
      args = args,
      ext = ".mdx"
    ),
    keep_md = FALSE,
    clean_supporting = FALSE,
    df_print = df_print,
    post_processor = post_processor
  )
}

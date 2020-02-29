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
        yaml_fields <- unlist(cfig)

        for (i in 1:length(cfig)) {
          param <- yaml_fields[[i]]
          present <- grepl(yaml_fields[[i]], yaml_headers[[i]])
          if (!present) {
            stop(paste0("Config item '", param, "' is missing from YAML."))
          } else if (param == "date") {
            date_string <- strex::str_after_first(yaml_headers[[i]], "date: ")
            date_string <- gsub(x = date_string, pattern = '"', replacement = "")
            first <- strex::str_before_first(date_string, "/")
            if (nchar(first) < 2) first <- paste0("0", first)
            second <- strex::str_after_first(date_string, "/")
            third <- strex::str_after_last(second, "/")
            second <- strex::str_before_first(second, "/")
            if (nchar(second) < 2) second <- paste0("0", second)
            # account for RStudio default RMarkdown creating dates as "2/28/2020"
            date_out <- paste0(third, "-", first, "-", second)
            date_position <- which(grepl("date", partitioned$front_matter))
            yaml_date_out <- paste0("date: '", date_out, "'")
            partitioned$front_matter[date_position] <- yaml_date_out

          }
        }
        if (length(yaml_headers) > length(yaml_fields)) {
          message("You have more items in YAML headers than your config. Just to let you know.")
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

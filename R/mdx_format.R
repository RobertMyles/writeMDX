mdx_format = function(variant = "markdown_strict",
                      preserve_yaml = TRUE,
                      dev = 'png',
                      df_print = "tibble",
                      fig_width = 7,
                      fig_height = 5){
  args <- ""

  # add post_processor for yaml preservation
  post_processor <- if (preserve_yaml) {
    function(metadata, input_file, output_file, clean, verbose) {
      input_lines <- read_utf8(input_file)
      partitioned <- partition_yaml_front_matter(input_lines)
      if (!is.null(partitioned$front_matter)) {
        output_lines <- c(partitioned$front_matter, "", read_utf8(output_file))
        write_utf8(output_lines, output_file)
      }
      output_file
    }
  }

  # return format
  rmarkdown:::output_format(
    knitr = rmarkdown:::knitr_options_html(fig_width, fig_height, fig_retina = NULL, FALSE, dev),
    pandoc = rmarkdown:::pandoc_options(to = variant,
                            from = rmarkdown::from_rmarkdown(),
                            args = args,
                            ext = '.mdx'),
    keep_md = FALSE,
    clean_supporting = FALSE,
    df_print = df_print,
    post_processor = post_processor
  )
}

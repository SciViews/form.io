.onLoad <- function(lib, pkg) { # nocov start
  # Automatically set up rmarkdown and knitr to output numbered and captioned
  # figures and tables
  if (isTRUE(getOption("form.io_captioner", default = TRUE)))
    install_captioner()
} # nocov end

.is_notebook_output <- function()
  getOption("rstudio.notebook.executing", FALSE)

.get_output_format <- function() {
  if (.is_notebook_output()) {
    res <- "html_notebook"
  } else {
    res <- try(rmarkdown::default_output_format(knitr::current_input())$name,
      silent = TRUE)
    if (inherits(res, "try-error"))
      res <- NULL
  }
  res
}

.is_bookdown_format <- function() {
  # Try to guess if the output format provides 'bookdown' extensions
  # (fig, tab & eq numbering)
  # The extended format are called like 'bookdown::html_document2'
  # TODO: are there exceptions to look for also? distill? other?
  
  # Allow to setting this in the YAML section
  output_format <- try(rmarkdown::default_output_format(knitr::current_input()),
    silent = TRUE)
  if (inherits(output_format, "try-error"))
    output_format <- NULL
  if (is.null(output_format))
    output_format <- list(name = "", options = list())
  if (.is_notebook_output())
    output_format$name <- "html_notebook"
  res <- output_format$options$bookdown
  
  # Allow for setting this in an option, as a second resort
  if (is.null(res))
    res <- getOption("form.io_use_bookdown", default = NULL)
  
  # Guess it
  if (is.null(res)) # Check if 'bookdown' is in the name of the output format
    res <- grepl("bookdown", output_format$name)
  
  isTRUE(res)
}

.get_chunk_label <- function() {
  if (.is_notebook_output()) {
    get(".rs.rnb.getHtmlCaptureContext", mode = "function")()$chunkOptions$label
  } else {
    knitr::opts_current$get("label")
  }
}

.is_word_output <- function() {
  res <- try(rmarkdown::default_output_format(knitr::current_input())$name ==
      "word_document", silent = TRUE)
  if (inherits(res, "try-error")) FALSE else res
}

# The function pandoc.formula.return() is *not* exported from pander. So, keep
# a copy here (also need get.caption() and get.storage())
#.get.storage <- function(what) {
#  # storage is an environment in pander that is not explorted!
#  res <- tryCatch(get(what, envir = storage, inherits = FALSE), 
#    error = function(e) NULL)
#  if (is.null(attr(res, "permanent")) || !attr(res, "permanent")) {
#    assign(what, NULL, envir = storage)
#  }
#  return(res)
#}

#.get.caption <- function() 
#  .get.storage("caption")

.check_caption <- function(caption) {
  if (length(caption) > 1)
    stop("The caption should be exactly one string.")
  if (!(is.character(caption) | is.null(caption)))
    stop("The caption should be string (character class) or NULL.")
  invisible(TRUE)
}

.pandoc.formula.return <- function(x, text = NULL, max.width = 80, caption,
add.line.breaks = FALSE, ...) {
  mc <- match.call()
  if (is.null(mc$caption)) {
    if (is.null(attr(t, "caption"))) {
      #caption <- .get.caption()
      caption <- NULL # Modified, because we don't have access to pander:::storage!
    } else {
      caption <- attr(t, "caption")
    }
  }
  res <- paste(sub("^[ ]*", "", deparse(x, width.cutoff = max.width)), 
    collapse = "")
  if (!is.null(text)) {
    res <- paste(text, res, sep = " ")
  }
  if (add.line.breaks) {
    res <- paste(res, "\n\n")
  }
  if (!is.null(caption) && caption != "" && .check_caption(caption)) {
    res <- paste0(res, "\n\n", panderOptions("formula.caption.prefix"), 
      caption, "\n\n")
  }
  return(res)
}

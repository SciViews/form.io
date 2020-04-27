#' Create a Figure caption
#' 
#' A caption for a figure can be created in two ways: either directly when you
#' provide both `text=` and `image=`, or indirectly, when you call [Figure()]
#' within an R chunk with `image=` not provided. In that case, the caption is
#' saved for later use, and `fig()` retrieves it when needed. The label of the
#' figure is automatically set to the label of the chunk.
#'
#' @param text The text of the caption. It can contain Markdown formatting.
#' @param image The Markdown code to include a figure like
#'   `![alt](path/to/figure.png)`, see example.
#' @param label The label to use (not required if the function is run from
#'  within an R chunk because the chunk label is used by default).
#' @param hold Should we save the caption for later use, and return invisibly?
#'   Default value is `FALSE` if `image=` is provided or the function is *not*
#'   called from within an R chunk.
#' @param ... Same arguments as for [Figure()].
#'
#' @return
#' The caption with a code to number the figure is returned (invisibly, if
#' `hold = TRUE`).
#' 
#' @export
#' @author Philippe Grosjean
#' @keywords utilities
#' @concept automatic numbering of items in documents
#' @examples
#' Figure("The official R logo.",
#' "
#' ![R logo](https://www.r-project.org/logo/Rlogo.png){width='80%'}
#' ")
Figure <- function(text= NULL, image = NULL, label = NULL, hold = NULL) {
  chunk_label <- .get_chunk_label()
  if (is.null(hold)) {
    # Determine default value: if run from a chunk without image data and NOT
    # from an R Notebook then TRUE, else FALSE
    hold <- !is.null(chunk_label) && is.null(image) && !.is_notebook_output()
  }
  
  if (isTRUE(hold) && !is.null(text)) {# Hold caption for future use
    text <- Figure(text, label = label, hold = FALSE)
    assign_temp("next_fig", text)
    return(invisible(text))
  }
  
  if (missing(text)) {
    # When text is missing, try to get it from the held version
    # Get it from `next_fig` if it exists, and get rid of it to avoid using it
    # twice!
    text <- get_temp("next_fig", default = NULL)
    rm_temp("next_fig")
  }
  
  if (!length(text))
    return(text)
  
  # If tag already defined, do not change text, but get label
  if (grepl("^ *\\(\\\\#fig\\:[\\-[:alnum:]]+\\)", text[1], perl = TRUE)) {
    label <- sub("^ *\\(\\\\#fig\\:([\\-[:alnum:]]+)\\).*$", "\\1", text[1],
      perl = TRUE)
  } else {# Tag not defined yet
    # Resolve label from chunk if it is missing
    if (missing(label))
      label <- .get_chunk_label()
    if (!is.null(label)) {
      # In case we are in bookdown and no image= is provided,
      # let bookdown do this!
      if (!.is_bookdown_format() || !is.null(image))
        text[1] <- paste0('(\\\\#fig:', label, ') ', text[1])
    }
  }
  
  # If image contains something, we format the complete Figure:
  # caption\n\n<image> stuff and we change its class so it prints correctly!
  if (!is.null(image)) {
    # image can be either a path to an image, or
    # '\n![<any alt text to ignore>](<image>){<image attributes>}\n'
    image <- paste0(image, collapse = "\n")
    if (grepl("^\\s*\n+\\!\\[[^]]*\\]\\([^)]+\\).*\n+\\s*$", image)) {
      if (grepl("^\\s*\n+\\!\\[[^]]*\\]\\([^)]+\\)\\{[^}]*\\}", image)) {
        attr <- sub("^\\s*\n+\\!\\[[^]]*\\]\\([^)]+\\)\\{([^}]*)\\}.*$", "\\1",
          image)
      } else attr <- ""
      image <- sub("^\\s*\n+\\!\\[[^]]*\\]\\(([^)]+)\\).*$", "\\1", image)
    } else {
      attr <- ""
    }
    # We collapse the whole caption into a single line
    text <- paste(text, collapse = " ")
    # ... and rework everything so thart it fits in
    # \n![<caption>](<image>){id='<tag>' attr}\n
    # and we change its class so that it prints correctly
    text <- structure(glue(
      "\n![<<text>>](<<image>>){id='fig:<<label>>' <<attr>>}",
      .open = "<<", .close = ">>"), class = "knitr_asis")
  }
  
  # Since this could be invoked from within inline R expressions, and ` is not
  #  useable theme it is possible to use ''' instead. Transform it now...
  res <- gsub("'''", "`", text)
  
  # If this is a R notebook and no image was provided,
  # make it a caption class span and also make it an anchor
  if (.is_notebook_output()) {
    if (is.null(image))
      res <- paste0('[', res, ']{id="fig:', label, '" .caption}')
    asis_output(res)
  } else res
}

#' @export
#' @rdname Figure
fig <- function(..., hold = FALSE) {
  res <- Figure(..., hold = FALSE)
  #if (.is_notebook_output()) {
  #  # R Notebooks do not construct captions from fig.cap=. So, we need to print
  #  # them by ourselve now
  #  knit_print(asis_output(res))
  #  invisible(res)
  #} else {# Just provide the caption
    res
  #}
}

#' @export
#' @rdname Figure
fig_id <- function(label = NULL) {
  # In case we are in bookdown, let it do it and return NULL as default
  if (.is_bookdown_format())
    return(NULL)
  
  if (isTRUE(is_latex_output()))
    return("") # If we return NULL, markdown makes the fig and label is not set!
  
  if (missing(label))
    label <- .get_chunk_label()
  if (is.null(label))
    return(NULL)
  
  # Return an id for this figure
  paste0('id="fig:', label, '"')
}

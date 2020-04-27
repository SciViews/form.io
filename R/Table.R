#' Create a Table caption
#' 
#' A caption for a table can be created in two ways: either directly when you
#' provide both `text=` and `table=`, or indirectly, when you call [Table()]
#' within an R chunk with `table=` not provided. In that case, the caption is
#' saved for later use, and `tab()` retrieves it when needed. The label of the
#' table is automatically set to the label of the chunk.
#'
#' @param text The text of the caption. It can contain Markdown formatting.
#' @param table The textual description of the table (in plain Markdown).
#' @param label The label to use (not required if the function is run from
#'  within an R chunk because the chunk label is used by default).
#' @param hold Should we save the caption for later use, and return invisibly?
#'   Default value is `FALSE` if `table=` is provided or the function is *not*
#'   called from within an R chunk.
#' @param ... Same arguments as for [Table()], or arguments to respective
#'   [print()] methods.
#' @param x Any R object to print.
#' @param sep Separator to use for printing multiple strings.
#'
#' @return
#' The caption with a code to number the table is returned (invisibly, if
#' `hold = TRUE`).
#' 
#' @export
#' @author Philippe Grosjean
#' @keywords utilities
#' @concept automatic numbering of items in documents
#' @examples
#' Table("This is my caption", "
#' |  x  |  y  |
#' |:---:|:---:|
#' |  1  |  2  |
#' |  3  |  5  |
#' ")
Table <- function(text = NULL, table = NULL, label = NULL, hold = NULL) {
  chunk_label <- .get_chunk_label()
  if (is.null(hold)) {
    # Determine default value: if run from a chunk without table data TRUE,
    # else FALSE
    hold <- !is.null(chunk_label) && is.null(table)
  }
  
  if (isTRUE(hold) && !is.null(text)) {# Hold caption for future use
    text <- Table(text, label = label, hold = FALSE)
    assign_temp("next_tab", text)
    return(invisible(text))
  }
  
  if (missing(text)) {
    # When text is missing, try to get it from the held version
    # Get it from `next_tab` if it exists, and get rid of it to avoid using it
    # twice!
    text <- get_temp("next_tab", default = NULL)
    rm_temp("next_tab")
  }
  
  if (!length(text))
    return(text)
  
  # If tag already defined, return the string unchanged
  if (grepl("^ *\\(\\\\#tab\\:[\\-[:alnum:]]+\\)", text[1], perl = TRUE))
    return(text)
  
  # Resolve label from chunk if it is missing
  if (missing(label))
    label <- chunk_label
  if (!is.null(label)) {
    # In case we are in bookdown and no table= is provided, let bookdown do this!
    if (!.is_bookdown_format() || !is.null(table))
      text[1] <- paste0('(\\\\#tab:', label, ') ', text[1])
  }
  
  # If table contains something, we format the complete Table: caption\n\n<table> stuff
  # and we change its class so that it prints correctly!
  if (!is.null(table)) {
    table <- as.character((table))
    if (grepl("^\\s*$", table[1]))
      table <- table[-1]
    table <- sub("^\\s*\n", "", table)
    text[1] <- paste("Table:", text[1])
    text <- structure(c(text, "", table), class = "knitr_asis")
  }
  
  # Since this could be invoked from within inline R expressions, and ` is not useable theme
  # it is possible to use ''' instead. Transform it now...
  gsub("'''", "`", text)
}

#' @export
#' @rdname Table
tab <- function(..., hold = FALSE)
  Table(..., hold = FALSE)

#' @export
#' @rdname Table
#' @method print knitr_asis
print.knitr_asis <- function(x, ..., sep = "\n") {
  # This is for a nicer R console printing
  x[is.na(x)] <- ""
  cat(x, ..., sep = sep)
  invisible(x)
}

#' @details
#' The SciViews 'form.io' package provides a series of functions to enhance the
#' way R objects are output in R Markdown documents. It focuses on scientific
#' documents, and as such, emphasizes with the creation of figures and tables
#' that are numbered and captioned. The 'bookdown' package provides such a
#' feature through special formats (e.g., `bookdown::html_document2` instead of
#' `html_document`), but the functions provided here provide the same
#' enhancement for *all* R Markdown files. The [form()] generic function is
#' there to provide a decent output format for many R objects that works in
#' HTML, PDF, Word and Markdown, as well as, at the R console. Under the hood,
#' it reuses formatting functions from other packages like [knitr::kable()], or
#' [pander::pander()], but it makes sure that outputs are well-formatted in
#' (almost) all cases when working with R Markdown documents.
#' 
#' @section Important functions:
#'
#' - [form()] to format a series of R objects for R Markdown output.
#'
#' - [Figure()] and [Table()] to construct captions for numbered figures and
#' tables.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom data.io label
#' @importFrom glue glue
#' @importFrom knitr asis_output current_input is_html_output is_latex_output
#'   knit_hooks knit_print opts_chunk opts_current
#' @importFrom pander pander pander_return panderOptions
#' @importFrom rmarkdown default_output_format metadata
#' @importFrom stats na.omit
#' @importFrom svMisc assign_temp get_temp rm_temp
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

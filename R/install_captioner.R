#' Install hook and default chunk values for numbered figures, tables and equations
#'
#' [install_captioner()] is called when 'form.io' loads. It is not necessary
#' to call it directly, except if you want to refresh these data. The automatic
#' installation when the 'form.io' package load can be disabled if you use
#' `options(form.io_captioner = FALSE)` before loading the package. In that case
#' you can still install it later with [install_captioner()].
#' 
#' @return
#' Nothing, the function is intended for its side-effect of installing the
#' required knitr hook for processing numbered and captioned items. It also sets
#' default values for two chunk options: `fig.cap = form.io::fig()` and
#' `out.extra = form.io::fig_id()`. Both these chunk options allow to
#' automatically build a caption from the last call to [Figure()] within the
#' R chunk where the plot is created.
#' 
#' @export
#' @author Philippe Grosjean
#' @keywords utilities
#' @concept automatic numbering of items in documents
install_captioner <- function() {
  knitr::opts_chunk$set(
    fig.cap = substitute(form.io::fig()),
    out.extra = substitute(form.io::fig_id()))
  knitr::knit_hooks$set(document = .captioner_hook)
}

# Install a document hook to resolve the `\@ref` and other stuff...
.captioner_hook <- function(x) {
  
  # Allow for \tag{eq:label} to be equivalent to (\#eq:label
  x <- gsub("\\\\tag\\{(eq\\:[\\-[:alnum:]]+)\\}", "\n(\\\\#\\1)\n", x,
    perl = TRUE)
  
  # If this is a bookdown -(derived) format, do minimal treatment and leave
  # bookdown doing the rest
  is_bookdown <- .is_bookdown_format()
  if (!is_bookdown) {
    # We have to manage figures, tables and equations numbering and captioning
    
    #---- First step: resolve numbers for figures, tables and equations
    all_tags <- c(`__fake_item__` = 0)
    n_figs <- 0
    n_tabs <- 0
    n_eqs <- 0
    m <- gregexpr("(?<=[^`])\\(\\\\{1,2}\\#([-:[:alnum:]]+)\\)", x, perl = TRUE)
    ids <- regmatches(x, m)
    for (i in seq_along(ids)) {
      if (length(ids[[i]]) == 0)
        next
      labels <- sub("^\\(\\\\{1,2}\\#(.+)\\)$", "\\1", ids[[i]])
      # Get tags (number in the list) from the succession of items
      # Generic tags are same as labels, but with _ replaced by space
      tags <- gsub("_", " ", labels)
      names(tags) <- labels
      
      # First, a generic substitution (for anything not resolved as fig:, tab:
      # or eq:)
      if (is_html_output()) {
        strings <- glue('<span id="<<labels>>"><<tags>></span>',
          .open = "<<", .close = ">>")
      } else {# Just create plain Markdown code
        strings <- glue("[<<tags>>]{id='<<labels>>'}",
          .open = "<<", .close = ">>")
      }
      names(strings) <- labels
      
      # Figures
      is_fig <- substr(labels, 1, 4) == "fig:"
      if (any(is_fig)) {
        unique_figs <- unique(labels[is_fig])
        # If a label already appeared before, reuse it
        unique_tags <- all_tags[unique_figs]
        names(unique_tags) <- unique_figs
        is_new <- is.na(unique_tags)
        n_new <- sum(is_new)
        if (n_new) {
          unique_tags[is_new] <- (1:n_new) + n_figs
          n_figs <- n_figs + n_new
        }
        tags[is_fig] <- unique_tags[labels[is_fig]]
        # Get custom figure header possibly
        header <- metadata$language$label$fig
        spacer <- "" # default value
        if (is.null(header)) {
          header <- "Figure"
        } else {
          # If header ends with two or more spaces, add a non-breakable space
          # before the : (this is the French ponctuation, for instance)
          if (grepl("  $", header))
            spacer <- "\\ "
          header <- trimws(header)
        }
        if (is_html_output()) {
          if (spacer == "\\ ") spacer <- "&nbsp;"
          strings[is_fig] <- glue("<<header>>&nbsp;<<tags>><<spacer>>:",
            .open = "<<", .close = ">>")[is_fig]
        } else if (is_latex_output()) {
          # Let LaTeX do this
          strings[is_fig] <- ""
        } else {# For anything not specific formatting, use plain Markdown
          strings[is_fig] <- glue("<<header>>\\ <<tags>><<spacer>>:",
            .open = "<<", .close = ">>")[is_fig]
        }
      }
      
      # Tables
      is_tab <- substr(labels, 1, 4) == "tab:"
      if (any(is_tab)) {
        unique_tabs <- unique(labels[is_tab])
        # If a label already appeared before, reuse it
        unique_tags <- all_tags[unique_tabs]
        names(unique_tags) <- unique_tabs
        is_new <- is.na(unique_tags)
        n_new <- sum(is_new)
        if (n_new) {
          unique_tags[is_new] <- (1:n_new) + n_tabs
          n_tabs <- n_tabs + n_new
        }
        tags[is_tab] <- unique_tags[labels[is_tab]]
        # Get custom table header possibly
        header <- metadata$language$label$tab
        spacer <- "" # default value
        if (is.null(header)) {
          header <- "Table" # Default value
        } else {
          # If header ends with two or more spaces, add a non-breakable space
          # before the : (this is the French ponctuation, for instance)
          if (grepl("  $", header))
            spacer <- "\\ "
          header <- trimws(header)
        }
        if (is_html_output()) {
          if (spacer == "\\ ") spacer <- "&nbsp;"
          strings[is_tab] <- glue(
            '<span id="<<labels>>"><<header>>&nbsp;<<tags>><<spacer>>:</span>',
            .open = "<<", .close = ">>")[is_tab]
        } else if (is_latex_output()) {
          # Let LaTeX do the job, but we still have to place an anchor and a
          # label for that table
          strings[is_tab] <- glue("[]{id='<<labels>>'}\\label{<<labels>>}",
            .open = "<<", .close = ">>")[is_tab]
        } else {# For anything not specific formatting, use plain Markdown
          strings[is_tab] <- glue(
            "[<<header>>\\ <<tags>><<spacer>>:]{id='<<labels>>'}",
            .open = "<<", .close = ">>")[is_tab]
        }
      }
      
      # Equations
      is_eq <- substr(labels, 1, 3) == "eq:"
      if (any(is_eq)) {
        unique_eqs <- unique(labels[is_eq])
        # If a label already appeared before, reuse it
        unique_tags <- all_tags[unique_eqs]
        names(unique_tags) <- unique_eqs
        is_new <- is.na(unique_tags)
        n_new <- sum(is_new)
        if (n_new) {
          unique_tags[is_new] <- (1:n_new) + n_eqs
          n_eqs <- n_eqs + n_new
        }
        tags[is_eq] <- unique_tags[labels[is_eq]]
        # For eq, Mathjax syntax is not supported for numbered item and we need
        # a hack
        if (.is_word_output()) {
          # Since Pandoc to Word does not seems to support exportation of
          # numbered equations, we simulate it the best we can. It is done by
          # using $$<equation> \qquad (<tag>)$$, but links are lost
          strings[is_eq] <- glue("\\qquad (<<tags>>) \\label{<<labels>>}",
            .open = "<<", .close = ">>")[is_eq]
        } else {
          strings[is_eq] <- glue("\\label{<<labels>>} \\tag{<<tags>>}",
            .open = "<<", .close = ">>")[is_eq]
        }
      }
      
      # Just in case... missing links
      tags[is.na(tags)] <- "???"
      
      ids[[i]] <- strings
      all_tags <- c(all_tags, tags)
    }
    regmatches(x, m) <- ids
    
    #---- Second step: resolve links to numbered items
    m <- gregexpr("(?<=[^`])\\\\@ref\\(([-:[:alnum:]]+)\\)", x, perl = TRUE)
    refs <- regmatches(x, m)
    for (i in seq_along(refs)) {
      if (length(refs[[i]]) == 0)
        next
      labels <- sub("^\\\\@ref\\((.+)\\)$", "\\1", refs[[i]])
      tags <- all_tags[labels]
      tags[is.na(tags)] <- "???"
      if (is_latex_output()) {
        # It does not work!?
        #links <- glue("\\ref{<<labels>>}", .open = "<<", .close = ">>")
        #links <- glue("\\hyperref[<<labels>>]{\\ref*{<<labels>>} }",
        #  .open = "<<", .close = ">>")
        # Instead of tags, I use \ref{label}
        links <- glue("[\\ref*{<<labels>>}](#<<labels>>)",
          .open = "<<", .close = ">>")
      } else if (.is_word_output()) {
        # No links in Word, only tags
        links <- glue("<<tags>>", .open = "<<", .close = ">>")
      } else {# If not LaTeX or Word, manage tags ourselves
        links <- glue("[<<tags>>](#<<labels>>)", .open = "<<", .close = ">>")
      }
      
      # For eq: links outside of Word, we use Mathjax to resolve the link!
      is_eq <- substr(labels, 1, 3) == "eq:"
      if (any(is_eq)) {
        eqref_style <- metadata$style$eqref
        if (is.null(eqref_style) || isTRUE(eqref_style)) {
          eqref <- "eqref"
        } else {
          eqref <- "ref"
        }
        if (.is_word_output()) {
          links[is_eq] <- tags[is_eq]
          if (eqref == "eqref") {
            # We want tags to be (1) instead of 1
            links[is_eq] <- paste0("(", links[is_eq], ")")
          }
        } else {# Not in Word
          eq_labels <- labels[is_eq]
          links[is_eq] <- glue("$\\<<eqref>>{<<eq_labels>>}$",
            .open = "<<", .close = ">>")
        }
      }
      
      refs[[i]] <- links
    }
    regmatches(x, m) <- refs
    
    if (isTRUE(getOption("form.io_debug_md")) && length(all_tags) > 1)
      message("Numbered items found in the document:\n",
        paste(names(all_tags[-1]), "=", all_tags, sep = "",  collapse = ", "),
        "\n")
    
    # For Word, we still need to finalize our display equations by prepending
    # with anchors
    # No links yet!
    #if (.is_word_output())
    #  x <- gsub('(\\$\\$[^\\$\u2021]+)\u2021([^\u2021]+)\u2021([^\\$]*\\$\\$)',
    #    '[]{id="\\2"}\\1\\3', x)
    # End of !is_bookdown job
  } else {# is_bookdown
    # Sometimes, we got (\\#label) instead of (\#label)
    # => make the correction now so that bookdown can process these items too
    x <- gsub("\\(\\\\\\\\\\#(.[^)]+)\\)", "(\\\\#\\1)", x)
  }
  
  # Replace any occurrence of -> <- by nothing
  x <- gsub("-> <-", "", x, fixed = TRUE)
  
  # Inject <style>....</style> fragment, if necessary
  if (is_html_output()) {
    style <- metadata$style
    if (is.null(style)) # Use default value
      style <- list()
    if (is.null(style$label)) {
      # Table captions are 'caption' in HTML are color "#777777",
      # but Figure captions are 'p' with class='caption' and default color is
      # "#333333". Homogenize this to #555555 for both  by default.
      style$label <- 'color: #555555;'
    }
    # ... add some more here, if needed
    # TODO: citation text a little bit too large => reduce it here by default
    # TODO: place figures and tables centered by default
    
    # Process style
    style_string <- ""
    if (style$label != "no")
      style_string <- paste0(style_string,
        "\n  caption, .caption {", style$label, "}")
    #... What more?
    # Finalize
    style_string <- paste0("\n<style>", style_string, "\n</style>\n")
    # and inject at the beginning of the document
    # Note: x[1] is the YAML header!
    x[2] <- paste0(style_string, x[2])
  }
  
  # Return the transformed document
  x
}

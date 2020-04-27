#' Form tabular and textual output of various R objects using markdown
#'
#' @param x An R object
#' @param ... Further arguments (depends on the class of `x`)
#' @param caption Legend for tables (if `""`, none)
#' @param digits Number of digits to print
#' @param data A data frame or tibble to be used to extract labels and/units
#' @param options Pander options (see [panderOptions()])
#' @param table_split Number of characters to accept for the width of the table
#' before splitting it ([pander()] functions)
#' @param table_row_align How to align table row names (`"left"` by default)
#' @param signif_stars For `anova` objects, do we write significance stars?
#' @param lang The language to use for the output. By default, it is the
#' `data.io_lang` options (same one as for the `data.io` package)
#'
#' @description Use rich-text formatting (R markdown) to output the content of
#' \R objects by using [kable()] or [pander()] functions. Output are
#' readable at the R console, inside inline elements in an R Notebook and in
#' HTML, LaTeX or Word documents. Moreover, they are translatable in different
#' languages `lang =` argument, and labels and/or units for variables
#' can also be used in place of the name of these variables.
#' 
#' @return A character string with the formatted results
#' @author Philippe Grosjean (phgrosjean@sciviews.org)
#' @export
#' @seealso [knitr::kable()], [pander::pander()]
#' @keywords utilities
#' @concept rich-formatted outputs
#'
#' @examples
#' library(data.io) # Even better to use SciViews::R()
#' options(data.io_lang = "FR")
#' trees <- read("trees", package = "datasets")
#' lm_1 <- lm(data = trees, volume ~ .)
#' form(trees)
#' form(head(trees), data = trees) # Try without data =
#' form(anova(lm_1), data = trees)
form <- function(x, ...)
  UseMethod("form")

#' @export
#' @rdname form
form.default <- function(x, caption = attr(x, "caption"), digits = 2, ...,
data = NULL, options = list(digits = digits, missing = "",
table.style = "rmarkdown"), table_split = 120L, table_row_align = "left",
lang = getOption("data.io_lang", "en")) {
  # Get pander options
  p_old_opts <- p_opts <- panderOptions()
  
  # Change pander options according to lang
  lang <- tolower(lang)
  # Currently only use "en" or "us" (no changes), or "fr"
  p_new_opts <- switch(lang,
    en    = list(),
    en_us = list(),
    fr    = list(decimal.mark = ",", formula.caption.prefix = "Formule : ",
      date = "%d/%m/%Y %H:%M:%S", #table.caption.prefix = "Tableau : ",
      table.continues = "Le tableau continue ci-dessous",
      table.continues.affix = "(continue ci-dessous)", p.copula = " et "),
    list())
  # Replace new language options into p_opts
  p_opts[names(p_new_opts)] <- p_new_opts
  # Replace custom options into p_opts
  p_opts[names(options)] <- options
  # Use as default pander options for now
  options(pander = p_opts)
  # Change length for table split and row names alignment
  panderOptions("table.split.table", table_split)
  panderOptions("table.alignment.rownames", table_row_align)
  # Restore previous pander options on exit
  on.exit(options(pander = p_old_opts))
  
  # If we knit a document, or execute the code outside of an R Notebook chunk
  if (!missing(caption))
    attr(x, "caption") <- caption
  if (isTRUE(getOption("knitr.in.progress")) ||
    !getOption("rstudio.notebook.executing", FALSE)) {
    # Use default pander() function
    pander(x, ...)
  } else {# In an R Notebook, use special code to produce a knit_asis object
    asis_output(pander_return(x, ...))
  }
}
#print.form <- function(x, ...) {
#  cat("print\n")
#}

#knit_print.form <- function(x, ...) {
#  cat("knit_print\n")
#}

rephrase <- function(x, data = NULL, ...,
lang = getOption("data.io_lang", "en"))
  UseMethod("rephrase")

# The default method just returns the object unmodified
rephrase.default <- function(x, data = NULL, ...,
lang = getOption("data.io_lang", "en"))
  x

rephrase.anova <- function(x, data = NULL, ...,
lang = getOption("data.io_lang", "en")) {
  # Minimal check of the object
  if (!is.list(x)) {
    warning("Strange 'anova' object (not a list)")
    return(x)
  }
  
  # Get translated stuff
  tr <- switch(tolower(lang),
    fr = list(
      header4 =
        c("DDL", "D\u00e9viance", "DDL des r\u00e9sidus", "D\u00e9viance des r\u00e9sidus"),
      header5 =
        c("DDL", "Somme carr\u00e9s", "Carr\u00e9s moyens", "Stat. _F_", "Valeur _p_"),
      header6 =
        c("DDL r\u00e9s.", "Somme carr\u00e9s r\u00e9s.", "DDL", "Somme carr\u00e9s", "Stat. _F_",
          "Valeur _p_"),
      residuals = "R\u00e9sidus", response = "R\u00e9ponse :",
      caption = "Analyse de la variance",
      model = "mod\u00e8le"),
    en_us = ,
    en_uk = ,
    en = ,
    list(
      header4 =
        c("Df", "Deviance", "Residuals Df", "Residuals Deviance"),
      header5 =
        c("DF", "Sum Squares", "Mean Squares", "_F_ Stat.", "_p_-value"),
      header6 =
        c("Res. Df", "Res. Sum Squares.", "Df", "Sum Squares", "Stat. _F_",
          "Valeur _p_"),
      residuals = "Residuals", response = "Response:",
      caption = "Analysis of Variance",
      model = "model")
  )
  
  if (length(x) == 4) {# ANOVA for a glm object 
    # Replace column headers
    names(x) <- tr$header4
    # TODO: I need to translate heading... for now, I just replace \n\n by ;
    attr(x, "heading") <- gsub("\n\n", "; ",
      sub("\n\n$", "", attr(x, "heading")))
    
  } else if (length(x) == 5) {# Usual ANOVA table
    # Replace column headers
    names(x) <- tr$header5
    
    # Replace Residuals in row.names (last item)
    rn <- rownames(x)
    names(rn) <- rn
    rn[length(rn)] <- tr$residuals
    # If data is provided, try to replace variables by their labels
    labels <- data.io::label(data)
    labels[labels == ""] <- NA
    labels_rn <- na.omit(labels[rn])
    nlabels_rn <- names(labels_rn)
    rn[nlabels_rn] <- labels[nlabels_rn]
    rownames(x) <- as.character(rn)
    
    # Possibly replace heading
    head <- attr(x, "heading")
    if (!is.null(head)) {
      response_var <- strsplit(head[2], ": ", fixed = TRUE)[[1]][2]
      response_label <- labels[response_var]
      if (!is.na(response_label) && !is.null(response_label))
        response_var <- response_label
      attr(x, "heading") <-
        paste0(tr$caption, " - ", paste(tr$response, response_var))
    }
    
  } else if (length(x) == 6) {# Comparison of nested models
    # Replace column headers
    names(x) <- tr$header6
    
    # Possibly replace heading
    head <- attr(x, "heading")
    if (!is.null(head)) {
      attr(x, "heading") <-
        paste0(tr$caption, " - ",
          gsub("Model", tr$model, gsub("\n", ", ", head[2])))
    }
  }
  x
}

#' @export
#' @rdname form
#' @method form anova
form.anova <- function(x, caption = attr(x, "caption"), data = NULL,
signif_stars = TRUE, ..., lang = getOption("data.io_lang", "en")) {
  x <- rephrase(x, data = data, lang = lang)
  res <- form.default(x, caption = caption, data = data,
    add.significance.stars = signif_stars, ..., lang = lang)
  
  # Replace Signif. codes:
  # TODO: how to do that? When pander() is called, I cannot change its output!
  res
}

# Also works on data.tables, because they also are data.frames
rephrase.data.frame <- function(x, data = NULL, labels = TRUE, units = TRUE,
..., lang = getOption("data.io_lang", "en")) {
  # Replace column headers by labels if data is provided,
  # otherwise use labels from x if present
  if (!isTRUE(labels)) return(x)
  
  if (isTRUE(units)) {
    if (!is.null(data)) {
      labels <- sapply(data, data.io::label, units = TRUE)
    } else {
      labels <- sapply(x, data.io::label, units = TRUE)
    }
    # Replace two spaces before units with a single one
    labels <- sub("  \\[", " [", labels)
    # Replace ^x by ^x and _x by ~x~ for uppercase or lowercase numbers
    labels <- gsub("\\^([0-9])", "^\\1^", labels)
    labels <- gsub("_([0-9])", "~\\1~", labels)
  } else {# No units
    if (!is.null(data)) {
      labels <- data.io::label(data)
    } else {
      labels <- data.io::label(x)
    }
  }
  labels[labels == ""] <- NA
  x_names <- names(x)
  names(x_names) <- x_names
  labels_rn <- na.omit(labels[x_names])
  nlabels_rn <- names(labels_rn)
  x_names[nlabels_rn] <- labels[nlabels_rn]
  names(x) <- as.character(x_names)
  x
}

#' @export
#' @rdname form
#' @method form data.frame
#' @param labels Should we use labels for column names?
#' @param units Should we add units to labels in column names? 
form.data.frame <- function(x, caption = attr(x, "caption"), data = NULL,
labels = TRUE, units = TRUE, ..., lang = getOption("data.io_lang", "en")) {
  x <- rephrase(x, data = data, labels = labels, units = units, lang = lang)
  lang <- tolower(lang)
  form.default(x, caption = caption, data = data, ..., lang = lang)
}

rephrase.summary.lm <- function(x, data = NULL, ...,
  lang = getOption("data.io_lang", "en")) {
  # Minimal check of the object
  if (!is.list(x)) {
    warning("Strange 'summary.lm' object (not a list)")
    return(x)
  }
  
  # Get translated stuff
  tr <- switch(tolower(lang),
    fr = list(coef_header =
        c("Estimateur", "Erreur type", "Stat. _t_", "Valeur _p_"),
      intercept = "(Ordonn\u00e9e \u00e0 l'origine)",
      caption = "Mod\u00e8le lin\u00e9aire :"),
    en_us = ,
    en_uk = ,
    en = ,
    list(coef_header =
        c("Estimate", "Std. Error", "_t_ Stat.", "_p_-value"),
      intercept = "(Intercept)",
      caption = "Fitting linear model:")
  )
  
  # Replace column headers in $coefficients
  colnames(x$coefficients) <- tr$coef_header
  
  # Replace (Intercept) in row.names in various places
  rn <- rownames(x$coefficients)
  names(rn) <- rn
  rn[rn == "(Intercept)"] <- tr$intercept
  # If data is provided, try to replace variables by their labels
  labels <- data.io::label(data)
  labels[labels == ""] <- NA
  labels_rn <- na.omit(labels[rn])
  nlabels_rn <- names(labels_rn)
  rn[nlabels_rn] <- labels[nlabels_rn]
  rn <- as.character(rn)
  rownames(x$coefficients) <- rn
  rownames(x$cov.unscaled) <- rn
  colnames(x$cov.unscaled) <- rn
  if (!is.null(x$aliased))
    names(x$aliased) <- rn
  
  #Replace/create caption
  attr(x, "caption") <- .pandoc.formula.return(x$call$formula, 
    text = tr$caption)
  x
}

#' @export
#' @rdname form
#' @method form summary.lm
form.summary.lm <- function(x, caption = attr(x, "caption"), data = NULL,
summary = TRUE, digits = 3, signif_stars = TRUE, ...,
lang = getOption("data.io_lang", "en")) {
  if (isTRUE(summary)) {
    
    # Get translated stuff
    tr <- switch(tolower(lang),
      fr = list(
        glm_1 = "Dispersion parameter for",
        glm_2 = "family taken to be",
        glm_caption = "R\u00e9sum\u00e9 de la r\u00e9gression lin\u00e9aire g\u00e9n\u00e9ralis\u00e9e :",
        lm_header =
          c("Observations", "Erreur type des r\u00e9sidus", "_R^2^_", "_R^2^_ ajust\u00e9"),
        lm_caption = "R\u00e9sum\u00e9 de la r\u00e9gression lin\u00e9aire :"),
      en_us = ,
      en_uk = ,
      en = ,
      list(
        glm_1 = "Dispersion parameter for",
        glm_2 = "family taken to be",
        glm_caption = "Summary for the generalized linear regression:",
        lm_header =
          c("Observations", "Residual Std. Error", "_R^2^_", "Adjusted _R^2^_"),
        lm_caption = "Summary for linear model:")
    )
    
    # This is adapted from pander:::pander.summary.lm()
    if (inherits(x, "summary.glm")) {
      cat("\n(", tr$glm_1, " ", x$family$family, 
        " ", tr$glm_2, " ", format(x$dispersion), 
        ")\n\n")
      res <- cbind(paste(format(c("Null", "Residual"), 
        justify = "right"), "deviance:"),
          apply(cbind(format(unlist(x[c("null.deviance", 
            "deviance")]), digits = pander::panderOptions("digits")), 
           " on", format(unlist(x[c("df.null", "df.residual")])), 
           " degrees of freedom\n"), 1L, paste, collapse = " "))
      rownames(res) <- NULL
      colnames(res) <- NULL
      attr(res, "caption") <- .pandoc.formula.return(x$call$formula, 
        text = tr$glm_caption)
    } else {# summary.lm
      res <- data.frame(Observations = length(x$residuals), 
        `Residual Std. Error` = x$sigma, `$R^2$` = x$r.squared, 
        `Adjusted $R^2$` = x$adj.r.squared, check.names = FALSE)
      colnames(res) <- tr$lm_header
      attr(res, "caption") <- .pandoc.formula.return(x$call$formula, 
        text = tr$lm_caption)
    }
  } else {# summary = FALSE
    res <- rephrase(x, data = data, lang = lang)
  }
  if (missing(caption) || is.null(caption))
    caption <- attr(res, "caption")
  form.default(res, caption = caption, data = data, digits = digits,
    add.significance.stars = signif_stars, summary = summary, ..., lang = lang)
}

#' @export
#' @rdname form
#' @method form lm
form.lm <- function(x, caption = attr(x, "caption"), data = NULL,
digits = 2, signif_stars = TRUE, ..., lang = getOption("data.io_lang", "en")) {
  # Get translated stuff
  tr <- switch(tolower(lang),
    fr = list(
      lm_caption = "Param\u00e8tres de la r\u00e9gression lin\u00e9aire :"),
    en_us = ,
    en_uk = ,
    en = ,
    list(
      lm_caption = "Coefficients for linear model:")
  )
  if (missing(caption) || is.null(caption))
    caption <- .pandoc.formula.return(x$call$formula, 
      text = tr$lm_caption)
  form(summary(x), caption = caption, data = data, digits = digits,
    signif_stars = signif_stars,
    summary = FALSE, ..., lang = lang)
}

#' @export
#' @rdname form
#' @method form summary.glm
#' @param summary Should we add a summary of the object?
form.summary.glm <-  function(x, caption = attr(x, "caption"), data = NULL,
summary = TRUE, digits = 3, signif_stars = TRUE, ...,
lang = getOption("data.io_lang", "en")) {
  # Get translated stuff
  tr <- switch(tolower(lang),
    fr = list(
      glm_caption = "Param\u00e8tres de la r\u00e9gression lin\u00e9aire g\u00e9n\u00e9ralis\u00e9e :"),
    en_us = ,
    en_uk = ,
    en = ,
    list(
      glm_caption = "Coefficients for generalized linear model :")
  )
  if (missing(caption))
    attr(x, "caption") <- .pandoc.formula.return(x$call$formula, 
      text = tr$glm_caption)
  form.summary.lm(x, caption = caption, data = data,
    digits = digits, signif_stars = signif_stars, ..., lang = lang)
}

rephrase.summary.nls <- function(x, data = NULL, ...,
  lang = getOption("data.io_lang", "en")) {
  rephrase.summary.lm(x, dazta = data, ..., lang = lang)
}

#' @export
#' @rdname form
#' @method form summary.nls
form.summary.nls <- function(x, caption = attr(x, "caption"), data = NULL,
summary = TRUE, digits = 3, signif_stars = TRUE, ...,
lang = getOption("data.io_lang", "en")) {
  # Get translated stuff
  tr <- switch(tolower(lang),
    fr = list(
      nls_header = c("Observations", "DDL", "Erreur type des r\u00e9sidus",
        "It\u00e9rations", "Tol\u00e9rance finale"),
      nls_caption = "R\u00e9sum\u00e9 de la r\u00e9gression non lin\u00e9aire :",
     not_conv = "pas de convergence"),
    en_us = ,
    en_uk = ,
    en = ,
    list(
      nls_header = c("Observations", "DF", "Std. Error of Residuals",
        "Iterations", "Final Tolerance"),
      nls_caption = "Summary for nonlinear model:",
      not_conv = "not converged")
  )

  res <- data.frame(
    obs = sum(x$df), df = x$df[2], error = x$sigma,
    iter = x$convInfo$finIter,
    tol = if (x$convInfo$isConv) x$convInfo$finTol else tr$not_conv
  )
  names(res) <- tr$nls_header
  
  if (missing(caption) || is.null(caption))
    caption <- .pandoc.formula.return(x$call$formula, 
      text = tr$nls_caption)
  form.default(res, data = data, digits = digits, caption = caption,
    add.significance.stars = signif_stars, summary = summary, ..., lang = lang)
}

#' @export
#' @rdname form
#' @method form nls
#' @param params_labels The label to use for the parameters.
form.nls <- function(x, caption = attr(x, "caption"), data = NULL,
params_labels, digits = 2, signif_stars = TRUE, ..., lang = getOption("data.io_lang", "en")) {
  # Get translated stuff
  tr <- switch(tolower(lang),
    fr = list(
      nls_caption = "Param\u00e8tres de la r\u00e9gression non lin\u00e9aire :"),
    en_us = ,
    en_uk = ,
    en = ,
    list(
      nls_caption = "Coefficients for nonlinear model:")
  )
  obj <- rephrase(summary(x), data = data, ..., lzng = lang)
  res <- obj$coefficients
  if (missing(caption) || is.null(caption)) {
    attr(res, "caption") <- .pandoc.formula.return(x$call$formula, 
      text = tr$nls_caption)
  } else {
    attr(res, "caption") <- caption
  }
  
  if (!missing(params_labels)) {
    params_labels <- as.character(params_labels)
    rn <- rownames(res)
    if (length(params_labels) == length(rn)) {
      rownames(res) <- params_labels
    } else {
      warning("params_labels must have same length as the table (",
        length(rn), ")")
    }
  }
  form(res, data = data, digits = digits,
    signif_stars = signif_stars, ..., lang = lang)
}

# form.io - To do list

## Numbered figures, Tables and equations

- Anchors for R chunk-generated figures in `pdf_document`.

- Captions for `html_document` and the like when `fig_caption: no`.

- Like for `include_graphics()` allows to produce both `png` and `pdf` version of a figure and use the best one infonction of the context (i.e., the PDF for LaTeX, but the PNG for HTML).

- Allow for Markdown formatting in captions for LaTeX, and escape automatically problematic characters.

- Allow to generate `Table()`s and `Figure()`s from a single multiline character string. In the case of figures, allow to place Figure after the image code (using pipe operator?)

- For HTML Notebook, the `fig.cap=` option is not recognized. So, if you put your cpation there, the figure has no caption and no label! This is a shortcoming of the R Notebook format. so, always use the in-chunk `Figure()` instruction for maximum of comptatibility.

- `bookdown::pdf_document2` does not produces correct numbers and links for equations.

- `bookdown::word_document2` does not dispalys the label of equations correctly.

## form()

- Get a local copy of `pander:::pandoc.formula.return()`

- Use something else than `caption=` (may be `comment=`?) for the default legend of the table in `form()`.

- A `form()` method for the comparison of two or more models

## Pander - To do

    pander.anova*  add.significance.stars = FALSE
pander.aov*
pander.aovlist*
pander.Arima*
    pander.call*
pander.cast_df*
    pander.character*
pander.clogit*
pander.coxph*
pander.cph*
pander.CrossTable*
    pander.data.frame*
    pander.data.table*
    pander.Date*
    pander.default*
pander.density*
pander.describe*
pander.ets*
pander.evals*
!!!!pander.factor* (is it buggy (only prints numerical levels!))
    pander.formula*
pander.ftable*
    pander.function*
++pander.glm*
++pander.Glm*
pander.gtable*
++pander.htest*
pander.image*
pander.irts*
    pander.list*
    pander.lm*
pander.lme*
    pander.logical* (should we provide alternatives for TRUE or FALSE???)
pander.lrm*
pander.manova*
     pander.matrix* (shouldn't we use labels)
pander.microbenchmark*
    pander.name*
++pander.nls*
    pander.NULL*
    pander.numeric*
pander.ols*
pander.orm*
pander.polr*
    pander.POSIXct*
    pander.POSIXlt*
pander.prcomp*
pander.randomForest*
pander.rapport*
pander.rlm*
pander.sessionInfo*
pander.smooth.spline*
pander.stat.table*
pander.summary.aov*
pander.summary.aovlist*
++pander.summary.glm*
    pander.summary.lm*
pander.summary.lme*
pander.summary.manova*
++pander.summary.nls*
pander.summary.polr*
pander.summary.prcomp*
pander.summary.rms*
pander.summary.survreg*
pander.summary.table*
pander.survdiff*
pander.survfit*
pander.survreg*
++pander.table*
pander.tabular*
pander.ts*
pander.zoo* (seems buggy???)

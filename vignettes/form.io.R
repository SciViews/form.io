## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Better to use SciViews::R() here
library(data.io)
library(form.io)
# To change language to, e.g., French
options(data.io_lang = "FR") # Same language option is used by form.io::form()

## -----------------------------------------------------------------------------
# Test
trees <- read("trees", package = "datasets")
lm_1 <- lm(data = trees, volume ~ .)
form(trees)
form(head(trees), data = trees) # Try without data =
form(anova(lm_1), data = trees)

## -----------------------------------------------------------------------------
trees <- read("trees", package = "datasets")

lm_1 <- lm(data = trees, volume ~ .)
lm_2 <- lm( data = trees, volume ~ I(diameter^2))

## -----------------------------------------------------------------------------
form(trees, caption = "Mesure de cerisiers noirs")
# Equivalent to:
#form(trees, caption = "Mesure de cerisiers noirs", labels = TRUE, units = TRUE)

## -----------------------------------------------------------------------------
form(head(trees))
form(head(trees), data = trees)

## -----------------------------------------------------------------------------
library(data.table)
trees_dt <- as.data.table(trees)
class(trees_dt)
form(trees_dt)

## -----------------------------------------------------------------------------
form(lm_1, data = trees, signif_stars = TRUE)

## -----------------------------------------------------------------------------
form(summary(lm_1))

## -----------------------------------------------------------------------------
form(anova(lm_1), data = trees) #, caption = "ANOVA de ma regression linéaire")

## -----------------------------------------------------------------------------
# From ?glm
## Dobson (1990) Page 93: Randomized Controlled Trial :
ad <- data.frame(treatment = gl(3, 3), outcome = gl(3, 1, 9),
  counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12))
glm_1 <- glm(data = ad, counts ~ outcome + treatment, family = poisson())
glm_1
summary(glm_1)
anova(glm_1)

## -----------------------------------------------------------------------------
anorexia <- read("anorexia", package = "MASS")
form(anorexia)
glm_2 <- glm(data = anorexia, Postwt ~ Prewt + Treat, family = gaussian)
glm_3 <- glm(data = anorexia, Postwt ~ Prewt + Treat + offset(Prewt), family = gaussian)
form(glm_2)
form(summary(glm_2))
#form(anova(glm_2))
form(aov(glm_2))

## -----------------------------------------------------------------------------
# A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)
clotting <- data.frame(
  u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
  lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
  lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12))
glm_4 <- glm(data = clotting, lot1 ~ log(u), family = Gamma)
glm_4
summary(glm_4)
anova(glm_4)

glm_5 <- glm(data = clotting, lot2 ~ log(u), family = Gamma)
glm_5
summary(glm_5)
anova(glm_5)

## Aliased ("S"ingular) -> 1 NA coefficient
(fS <- glm(lot2 ~ log(u) + log(u^2), data = clotting, family = Gamma))
tools::assertError(update(fS, singular.ok = FALSE), verbose = interactive())
## -> .. "singular fit encountered"

## -----------------------------------------------------------------------------
nls_1 <- nls(data = trees, volume ~ SSlogis(diameter, Asymptote, Inflexion, Echelle))
form(nls_1)
form(nls_1, params_labels = c("Asym", "xmoyen", "scal")) # Rename the parameters
form(summary(nls_1))

## -----------------------------------------------------------------------------
nls_2 <- nls(data = trees, volume ~ SSgompertz(diameter, Asymptote, B2, B3))
form(anova(nls_1, nls_2))

## -----------------------------------------------------------------------------
form(list(a = 1:3, b = TRUE, c = LETTERS))

## -----------------------------------------------------------------------------
form(nottem, caption = "températures à Notthingam")

## -----------------------------------------------------------------------------
form(structure(list(a = 1, b = 3:5), class = "new_oject"))

## -----------------------------------------------------------------------------
#options( knitr.table.format = "markdown")
knitr::kable(trees, caption = "The `trees` dataset")
#kableExtra::xtable2kable(xtable::xtable(linear.1))

## -----------------------------------------------------------------------------
knitr::kable(broom::glance(lm_1)[, c(1, 3, 5, 8, 11)])

## -----------------------------------------------------------------------------
knitr::kable(broom::tidy(lm_1), caption = "Tableau des coefficients")

## -----------------------------------------------------------------------------
#stargazer::stargazer(trees, type = "text")
#stargazer::stargazer(trees, type = "latex")
#stargazer::stargazer(lm_1, type = "text")


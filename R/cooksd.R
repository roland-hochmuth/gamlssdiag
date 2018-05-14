library(tidyverse) # An opinionated collection of R packages designed for data science (https://www.tidyverse.org/).
library(gamlss)    # Generalized Additive Models for Location, Scale and Shape (http://www.gamlss.com/).
library(psych)     # Procedures for Psychological, Psychometric, and Personality Research.

#' Evaluates Cook's Distance.
#'
#' @note Although lm/glm stores the original data in the model at m$data, that is not the case in GAMLSS.
#' Therefore, unlike the function cooks.distance which only requires a model parameter, this function requires
#' the formula and data.
#'
#' @param fn The function to use for creating the model. Either glm or gamlss.
#' @param formula The formula to use in the model.
#' @param data The dataframe.
#' @param family The distribution family. E.g. "poisson" if using glm. PO or NBI if using gamlss.
#'
#' @return A vector of distances
#'
#' #' @examples
#' cooksd(glm, f, df, "poisson")
#' cooksd(gamlss, f, df, PO)
#' cooksd(gamlss, f, df, NBI)
#'
#' @references https://www.ime.usp.br/~abe/lista/pdf1USQwcGBX1.pdf
#' @references Robust Diagnostic Regression Analysis, Anthony Atkinson and Marco Riani
#'
#' @export
cooksd <- function(fn, formula, data, family) {
  # Evaluate the full model, m, and predictions, p.
  m <- fn(formula, data=data, family=family)
  p <- exp(predict(m, newdata=data, data=data, what=c("mu")))

  coefficients <- coef(m)
  num_coefficients <- length(coefficients) + 1
  num_observations <- nrow(data)

  # Evaluate the residuals for the full model
  r <- p - df$BTSP

  # Evaluate sum of squared residuals / ...
  s2 <- sum(r^2)/(num_observations - num_coefficients)

  d <- vector(mode = "double", length = num_observations)

  for (i in seq_along(d)) {
    # Create the leave-one-out (loo) dataframe.
    loo_data <- filter(data, !row_number() %in% i)

    # Evaluate the loo model and predictions
    loo_m <- fn(formula, data=loo_data, family=family)
    loo_p <- exp(predict(loo_m, newdata=data, data=loo_data, what=c("mu")))

    d[i] <- sum((loo_p - p)^2) / (num_coefficients*s2)
  }
  return(d)
}

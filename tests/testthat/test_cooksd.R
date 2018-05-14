library(tidyverse) # An opinionated collection of R packages designed for data science (https://www.tidyverse.org/).
library(gamlss)    # Generalized Additive Models for Location, Scale and Shape (http://www.gamlss.com/).

df <- read_csv("bird_count.csv")
df <- df %>%
  dplyr::select(
    BTSP,
    Easting_thousands,
    Northing_thousands,
    SHRUB_Cover,
    DENS_15,
    DENS_15to4,
    MaxNum250m_4
  )
df$MaxNum250m_4 <- as.factor(df$MaxNum250m_4)

df <- df %>%
  na.omit()

f <-
  as.formula(
    BTSP ~ Easting_thousands + Northing_thousands + SHRUB_Cover + DENS_15 + DENS_15to4 + MaxNum250m_4
  )

m_glm <- glm(f, data = df, family = "poisson")


test_that("cooksd using glm", {
  d <- cooksd(glm, f, df, "poisson")
  expect_equal(which.max(d), 114)
})

test_that("cooksd using gamlss with PO", {
  d <- cooksd(gamlss, f, df, PO)
  expect_equal(which.max(d), 114)
})

test_that("cooksd using gamlss with NBI", {
  d <- cooksd(gamlss, f, df, NBI)
  expect_equal(which.max(d), 114)
})

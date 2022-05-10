\name{datamultv}
\alias{datamultv}
\docType{data}
\title{datamultv data}
\description{Longitudinal data set with the five occasions for each reference person. Generated from the Continuous PNAD (IBGE/Brazil) from the first quarter of 2018 until the first quarter of 2019 following households included in the sample at the first quarter of 2018.
}
\usage{data(datamultv)}

\format{
  A \code{data.frame} with 1685 observations and the following 13 variables.
  \describe{
    \item{\code{Y}}{the logarithm of the monthly income in the main job for people aged 14 and over (only for people who received cash, products or goods in the main job) plus 1}
    \item{\code{X1}}{age of the resident in the reference date centered around 40}
    \item{\code{X2}}{indicator variable for male residents}
    \item{\code{X3}}{indicator variable for residents of white color or race}
    \item{\code{X4}}{the logarithm of hours actually worked in the reference week in all jobs for people aged 14 and over plus 1}
    \item{\code{X5}}{years of study (people aged 5 or over) standardized for elementary school 9 YEARS SYSTEM}
    \item{\code{PSU}}{level 2 identifiers, is the group identifier for this data}
    \item{\code{STRAT}}{variable that identifies the strata (not needed in the command functions)}
    \item{\code{wave}}{time-dummies for level 1 units}
    \item{\code{idd}}{level 1 identifiers}
    \item{\code{wj}}{weights corresponding to level 2 units}
    \item{\code{w_ij}}{vector of weights corresponding to level 1 units, conditional to their respective level 2 unit (also longitudinal weights in the multivariate data)}
    \item{\code{wi_j}}{weights corresponding to level 1 and 2 units (not needed in the command functions)}
}
}

\examples{
data(datamultv)
summary(datamultv)
}
\keyword{datasets}

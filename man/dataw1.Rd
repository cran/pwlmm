\name{dataw1}
\alias{dataw1}
\docType{data}
\title{dataw1 data}
\description{Crosssectional data set. A two-level data containing  wave one from \code{datamultv} data. Generated from the first quarter of 2018 data of the Continuous PNAD (IBGE/Brazil).
}
\usage{data(dataw1)}

\format{
  A \code{data.frame} with 337 observations and the following 13 variables.
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
data(dataw1)
summary(dataw1)
}

\keyword{datasets}

#' Sample from the Ten-Year Follow-up (1980) of the 1970 British Cohort Study
#'
#' This dataset is a sample of few variables taken fron the Ten-Year Follow-up
#' (1980) of the 1970 British Cohort Study (BSC70).
#'
#' The BCS70 began in 1970 when data were collected about the 17,198 babies born
#' in England, Scotland, Wales and Northern Ireland in the week 5-11 April.
#'
#' At this time, the study was named the British Births Survey (BBS) and it was
#' sponsored by the National Birthday Trust Fund, in association with the Royal
#' College of Obstetricians and Gynecologists.
#'
#' Since 1970, there have been four attempts to gather information from the full
#' cohort - when they were aged 5, 10, 16 and 26 years. 90% of the 1970 Cohort
#' were traced and completed one or more survey documents.
#'
#' In 1975 and 1980, the cohort was augmented by the inclusion of immigrants to
#' Britain who were born in the target week in 1970. Subjects from Northern
#' Ireland who had been included in the birth survey, were dropped from the
#' study in all subsequent sweeps.
#'
#' The BCS70 Ten-year Follow-up is the second full national follow-up of the
#' 1970 cohort born in Great Britain 5-11 April 1970. The cohort has been
#' surveyed comprehensively at birth, five, ten, 16 and 26 years, and samples
#' were seen at 22 months, 42 months, seven and 21 years. The BCS70 Ten-year
#' Follow-up was originally titled the Child Health and Education Study (CHES),
#' but in 1991 the whole 1970 Cohort Study was renamed the British Cohort Study
#' 1970 (BCS70) and the ten-year sweep became known as BCS70 Ten-year Follow-up.
#'
#' @format A data frame with 7 variables:
#' \describe{
#'   \item{sex}{Child's sex}
#'   \item{height}{Child's height in cm}
#'   \item{cigarettes}{No. of cigarettes parents smoke daily}
#'   \item{milk}{Glasses of milk per day}
#'   \item{mother_weight}{Mother's weight in Kg}
#'   \item{weight}{Child's weight in Kg}
#'   \item{drink}{Did mother drink during pregnancy, early}
#' }
#' @source Study number 3723 from the BCS70: \url{https://discover.ukdataservice.ac.uk/series/?sn=200001}
"UK"


#' The Fungi dataset
#'
#' This dataset is the result of an experiment aiming at assessing how the heat
#' and pre-exposure to heat influences the growth rate and survival of two
#' species of fungi in petry dishes.
#'
#' This dataset has not yet been published, so the identity of the species and
#' the exact protocol of the experiement will not be revealed.
#'
#' The experiement ran in two steps: (i) a pre-treatment was performed on some
#' but not all fungi, during which the fungi are kept at elevated temperature
#' (36 or 38 degrees) for some time. (ii) the treatment, during which all fungi
#' are kept for some time at an elevated temperature (36 or 38 degrees). The
#' growth rate and survival status of the fungi were recorded at the end of the
#' experiement.
#'
#' Before being put in petry dishes each species were grown in three different
#' batches referred to as colonies. Each row of the dataset corresponds to a
#' different petry dish.
#'
#' @format A data frame with 8 variables:
#'  \describe{
#'    \item{colony}{An identifier indicating the colony}
#'    \item{species}{An identifier referring to the species}
#'    \item{T36}{A binary indicating if the heating treatment was 36 degrees}
#'    \item{T38}{A binary indicating if the heating treatment was 38 degrees}
#'    \item{PT36}{A binary indicating if a pre-treatment at 36 degrees was performed}
#'    \item{PT38}{A binary indicating if a pre-treatment at 38 degrees was performed}
#'    \item{alive}{A boolean indicating if the fungi was alive at the end of the experiment}
#'    \item{growth}{A quantitative measure of the growth rate}
#'  }
#'
#' @source An excellent bachelor student
"Fungi"


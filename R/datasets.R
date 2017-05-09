#' Sample from the Ten-Year Follow-up (1980) of the 1970 British Cohort Study
#'
#' This dataset is a sample of few variables taken from the Ten-Year Follow-up
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
#' @format A data frame with 14875 rows and 19 variables:
#' \describe{
#'   \item{sex}{Child's sex}
#'   \item{age}{Child's age in days}
#'   \item{weight}{Child's weight in Kg}
#'   \item{height}{Child's height in cm}
#'   \item{glasses}{Whether the child is wearing glasses}
#'   \item{cigarettes_mum}{No. of cigarettes mum smokes daily}
#'   \item{cigarettes_dad}{No. of cigarettes dad smokes daily}
#'   \item{cigarettes_mum}{No. of cigarettes parents smoke daily}
#'   \item{cigarettes_kid}{Information of the child smoking habits}
#'   \item{cigarettes_friends}{Information on child's friends who smoke}
#'   \item{bronchitis}{Whether the child ever had bronchitis}
#'   \item{drink}{Did mother drink during pregnancy, early}
#'   \item{milk}{Glasses of milk drank per day}
#'   \item{coca}{Glasses of coca-cola or pepsi drank per day}
#'   \item{backward}{Number of steps the child makes before making an error when asked to walk backwards for 20 steps}
#'   \item{mother_height}{Mother's height in cm}
#'   \item{father_height}{Father's height in cm}
#'   \item{mother_weight}{Mother's weight in Kg}
#'   \item{father_weight}{Father's weight in Kg}
#' }
#' @source Study number 3723 from the BCS70: \url{https://discover.ukdataservice.ac.uk/series/?sn=200001}
"UK"

#' Sample from the Health Survey for England Cardiovascular Disease’98
#'
#' This dataset is a sample of few variables taken from the Health Survey for
#' England Cardiovascular Disease’98. This dataset is restricted to females.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{period}{Whether still having periods}
#'   \item{age}{Age last birthday}
#'   \item{smoked}{Whether ever smoked cigarettes}
#'   \item{drunkLast3M}{Whether slightly (or very) drunk in the last three months}
#'   \item{drinkPb}{Number of drinking problems experienced}
#'   \item{education}{Age finished education}
#'   \item{bmi}{Valid BMI}
#' }
#'
#' @source UK Data Archive Study Number 4150
"HSE98women"

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
#' @source Bachelor thesis of Magdalena Nagel, FU Berlin.
"Fungi"

#' The Bats dataset
#'
#' This dataset has been collected during field studies with the goal of studying the impact of disturbance on the health of forest-dwelling paleotropical bats.
#' This dataset has been used to produce the results concerning the body mass of bats in the paper by Seltmann et al. (see source for details).
#'
#' @format A data frame with 10 variables:
#'  \describe{
#'    \item{log_weight}{The log body weight in grams}
#'    \item{Spp.y}{An identifier referring to the species}
#'    \item{Site.y}{A factor indicating the type of habitats in which individuals have been captured}
#'    \item{FA.y}{The forearm length in mm}
#'    \item{log_FAz}{The log forearm length turned into z-score by centering and scalling the measurement within species}
#'    \item{Fed}{A binary factor indicating if the individuals have recently been feeding (Y = Yes, N = No)}
#'    \item{REP}{A factor indicating the reproductions status (NR = non-reproductive, PR = pregnant, L = lactating or M = post-lactating)}
#'    \item{Year}{A factor indicating the year of sampling}
#'    \item{Season}{A factor indicating the season of sampling}
#'  }
#'
#' @source Anne Seltmann, Gábor Á. Czirják, Alexandre Courtiol, Henry Bernard, Matthew J. Struebig, Christian C. Voigt; Habitat disturbance results in chronic stress and impaired health status in forest-dwelling paleotropical bats. Conserv Physiol 2017; 5 (1): cox020. doi: 10.1093/conphys/cox020
"Bats"

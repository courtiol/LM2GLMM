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
#'   \item{cigarettes}{No. of cigarettes parents smoke daily}
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
#' species of fungi in petri dishes.
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
#' Before being put in petri dishes each species were grown in three different
#' batches referred to as colonies. Each row of the dataset corresponds to a
#' different petri dish.
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
#'    \item{Site.x}{A factor indicating the type of forest patch in which individuals have been captured}
#'  }
#'
#' @source Anne Seltmann, Gábor Á. Czirják, Alexandre Courtiol, Henry Bernard,
#'   Matthew J. Struebig, Christian C. Voigt; Habitat disturbance results in
#'   chronic stress and impaired health status in forest-dwelling paleotropical
#'   bats. Conserv Physiol 2017; 5 (1): cox020. doi: 10.1093/conphys/cox020
"Bats"


#' The Gryphon dataset
#'
#' This dataset contains a simulated dataset and a simulated pedigree of the
#' gryphons. The gryphon is a legendary creature with the body, tail, and back
#' legs of a lion; the head and wings of an eagle; and an eagle's talons as its
#' front feet. This dataset has been released with the publication of a paper
#' introducing the animal model, a particular LMM used to infer the heritability
#' of phenotypic traits (see sources). It thus offers the possibility to compare
#' different packages and software for estimating the heritability.
#'
#' @format A list containing:
#'  \describe{
#'    \item{data}{The dataframe containing the mother id, the birth year, the sex, the birth weight, the tarsus length and the individual id of the gryphons}
#'    \item{pedigree}{The dataframe containing the individual id, the mother id and the father id of each gryphon}
#'  }
#'
#'  @source Alastair J. Wilson, Denis Réale, Michelle N. Clements, Michael M.
#'    Morrissey, Erik Postma, Craig A. Walling, Loeske E. B. Kruuk; Daniel H.
#'    Nussey; An ecologist's guide to the animal model. Journal of Animal
#'    Ecology 2010; 79, 13--26. doi: 10.1111/j.1365-2656.2009.01639.x
"Gryphon"


#' The Surprise dataset
#'
#' This dataset is the result of a controlled study in which 852 parents from one
#' canadian school were divided into six groups of equal size and given gifts of
#' different types (1 type per group) and of different costs (between ca 15 to 100
#' dollars). After delivering the gifts to their children the parents reported
#' the perceived joy of their children on a continuous scale between 0 and 100.
#'
#' @format A dataframe with 3 variables:
#'   \describe{
#'     \item{type}{the type of gift}
#'     \item{price}{the price coverted in US$}
#'     \item{hapiness}{the continuous scale used to record the children's emotional response}
#'   }
#'
#' @source Justin Matejka, George Fitzmaurice, Toronto Ontario Canada. DOI: http://dx.doi.org/10.1145/3025453.3025912
"Surprise"


#' The Challenger Space Shuttle O-Ring Dataset
#'
#' Edited from (Draper, 1993): The motivation for collecting this database was
#' the explosion of the USA Space Shuttle Challenger on 28 January, 1986. An
#' investigation ensued into the reliability of the shuttle's propulsion system.
#' The explosion was eventually traced to the failure of one of the three field
#' joints on one of the two solid booster rockets. Each of these six field
#' joints includes two O-rings, designated as primary and secondary, which fail
#' when phenomena called erosion and blowby both occur. The night before the
#' launch a decision had to be made regarding launch safety. The discussion
#' among engineers and managers leading to this decision included concern that
#' the probability of failure of the O-rings depended on the temperature t at
#' launch, which was forecase to be 31 degrees F. There are strong engineering
#' reasons based on the composition of O-rings to support the judgment that
#' failure probability may rise monotonically as temperature drops. One other
#' variable, the pressure at which safety testing for field join leaks was
#' performed, was available, but its relevance to the failure process was
#' unclear.
#'
#' @format A dataframe with 5 variables:
#'   \describe{
#'     \item{oring_tot}{The number of O-rings at risk on a given flight}
#'     \item{oring_dt}{The number experiencing thermal distress}
#'     \item{temp}{The launch temperature (degrees F)}
#'     \item{psi}{The leak-check pressure (psi)}
#'     \item{flight}{The temporal order of flight}
#'    }
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Challenger+USA+Space+Shuttle+O-Ring}
#'
#' Original dataset from: Draper,D. (1993). Assessment and propagation of model uncertainty. In Proceedings of the Fourth International Workshop on Artificial Intelligence and Statistics (pp. 497--509). Ft. Lauderdale, FL: Unpublished.
"Challenger"


#' Record of household effort in a flatshare (Berlin winter 2016 - spring 2017)
#'
#' Ten flatmates recorded their household efforts ias numbers of "half hours per month" for each of the three
#' following chores: shopping, cleaning and other. Data are anonymous, gender and number of days absent
#' are noted. Before the observed time period a consensus was reached on trying to keep cleaning
#' effort balanced across genders.
#'
#' @format A data frame with 60 rows and 7 variables:
#' \describe{
#'  \item{individual}{identity of flatmate}
#'  \item{gender}{gender of flatmate}
#'  \item{month}{November-December 2016 and Januar-April 2017}
#'  \item{shopping}{number of half hours spent shopping in respective month}
#'  \item{cleaning}{number of half hours spent cleaning in respective month}
#'  \item{other}{number of half hours spent on tasks except shopping or cleaning}
#'  \item{absent}{number of days flatmate is absent in respective month}
#' }
#' @source Privately gathered data
"Flatwork"


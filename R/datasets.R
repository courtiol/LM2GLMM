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
#'    \item{REP}{A factor indicating the reproductions status (NR = non-reproductive, L = lactating or M = male)}
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
#' @source Alastair J. Wilson, Denis Réale, Michelle N. Clements, Michael M.
#'   Morrissey, Erik Postma, Craig A. Walling, Loeske E. B. Kruuk; Daniel H.
#'   Nussey; An ecologist's guide to the animal model. Journal of Animal
#'   Ecology 2010; 79, 13--26. doi: 10.1111/j.1365-2656.2009.01639.x
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


#' The Hedgehog dataset
#'
#' Data come from a camera trap that was set up in the same location for 2 years.
#' The column \var{presence} indicates whether an hedgehog was observed on this day or not.
#' All the other data correspond to measurements done in a 4-hour time frame
#' around sunset and all values are the mean over those 4 hours. The rain measurement
#' was a binary indicator for rain measured every hour so the values in this column
#' are the mean of this indicator. The cloud measurements were done categorically
#' on a scale from 0 to 8 so this is also the mean over 4 hours. Moon phase is also
#' categorical with 4 phases.
#'
#' @format A data frame with 386 rows and 10 variables:
#' \describe{
#'   \item{time}{the date as a Date R object}
#'   \item{presence}{A binary indicating whether an hedgehog has been observed or not}
#'   \item{temperature}{the mean temperature during the 4-hour window}
#'   \item{humidity}{the mean humidity during the 4-hour window}
#'   \item{precipitation}{the precipitation amount over the 4-hour window?}
#'   \item{rain}{the proportion of hours with rain}
#'   \item{ground_temp}{the mean ground temperature during the 4-hour window}
#'   \item{cloud_cover}{the mean cloud cover during the 4-hour window}
#'   \item{moon_phase}{the phase of the moon on that night}
#'   \item{time_rel}{the number of days since the begining of the data collection}
#' }
#' @source Internship from Wanja Rast, IZW Berlin???
"Hedgehog"


#' The second Bats dataset
#'
#' The dataset provide isotopic measurements from tissues sampled from different
#' bat species in different locations. Each tissue has a different turnover
#' which can show the difference in the diet through time. We don't provide
#' discrimination factors for each tissue.

#'
#' @format A data frame with 82 rows and 14 variables:
#' \describe{
#'   \item{location}{the locations were different for every mistnet}
#'   \item{date}{the date as a Date R object}
#'   \item{Species}{the species name}
#'   \item{Sex}{the sex, male or female}
#'   \item{Age}{the age in age class: Juvenile or Adult}
#'   \item{X15N_fur}{isotopic measurement}
#'   \item{X13C_fur}{isotopic measurement}
#'   \item{X15N_membrane}{isotopic measurement}
#'   \item{X13C_membrane}{isotopic measurement}
#'   \item{X15N_liver}{isotopic measurement}
#'   \item{X13C_liver}{isotopic measurement}
#'   \item{X15N_stomach_content}{isotopic measurement}
#'   \item{X13C_stomach_content}{isotopic measurement}
#'   \item{time_rel}{the number of days since the begining of the data collection}
#' }
#' @source Data collected by Cecilia Kruszynski, Center of Nuclear Energy in Agriculture, Piracicaba, São Paulo.
"Bats2"


#' The third Bats dataset
#'
#' The dataset describes the vocal activity of Noctule bats in autumn time in
#' Eastern Ukraine. Not long ago, bats were migrating to the south
#' to get food before hibernation, but now they stay where they were born for
#' hibernation and become sedentary. The goal is to understand which
#' environmental variables predict their level of activity (the column "calls"
#' which contain a number of recorded calls which bats emit in flight at the
#' site) and the probability of feeding (the column "buzz" represent 0 and 1 of
#' presence/absence of specific feeding calls, which confirm feeding activity at
#' the site). However, the data could be zero-inflated and yet the probability
#' of buzz usually will be related to the presence of vocal activity first.
#'
#' Land type use categories are given in square meters within plots of 1 km radius around recording points.
#'
#' @format A data frame with 70 rows and 21 variables:
#' \describe{
#'   \item{Site}{the ID of recording site}
#'   \item{lat}{the latitude}
#'   \item{long}{the longitude}
#'   \item{date}{the date of recording}
#'   \item{temp}{the temperature in C}
#'   \item{cloud}{cloudiness in parts (max 1)}
#'   \item{precip}{0 or 1 of rain (usually 0 as we won't expect bat activity during the rain)}
#'   \item{light }{categorical variable 0,1,2,3 where 0 absolute absence of artificial light and 3 full illumination}
#'   \item{time}{the time of recording}
#'   \item{calls}{the number of recorded calls of Noctule bat}
#'   \item{buzz}{0 and 1 of feeding buzzes}
#'   \item{eser}{the number of calls of another bat species Serotine bat}
#'   \item{pkuhl}{the number of calls of another bat species Kuhl's pipistrelle}
#'   \item{farm}{the area of farmland}
#'   \item{green}{the area of vegetation coverage}
#'   \item{road}{the area of roads}
#'   \item{urban}{the area of urban coverage}
#'   \item{water}{the area of water}
#'   \item{ndvi_mean}{the NDVI index mean for the plot}
#'   \item{ndvi_SD}{the standard deviation of NDVI index for plot}
#'   \item{time_rel}{the number of hours since the first collection}
#'   }
#' @source Data collected by Kseniia Kravchenko, UIEI Ukraine
"Bats3"


#' Poison data (taken from package fastR)
#'
#' The data give the survival times (in hours) in a 3 x 4 factorial experiment,
#' the factors being (a) three poisons and (b) four treatments. Each
#' combination of the two factors is used for four animals. The allocation to
#' animals is completely randomized.
#'
#' @format A data frame with 48 observations on the following 3 variables.
#' \itemize{ \item{Poison}{ type of poison (1, 2, or 3)}
#' \item{Treatment}{ manner of treatment (1, 2, 3, or 4)}
#' \item{Time}{ time until death (hours)} }
#' @references Box, G. E. P., and Cox, D. R. (1964). An analysis of
#' transformations (with Discussion). J. R. Statist. Soc. B, 26, 211-252.
#'
#' Aitkin, M. (1987). Modelling variance heterogeneity in normal regression
#' using GLIM. Appl. Statist., 36, 332-339.
#'
#' Smyth, G. K., and Verbyla, A. P. (1999). Adjusted likelihood methods for
#' modelling dispersion in generalized linear models. Environmetrics 10,
#' 696-709. \url{http://www.statsci.org/smyth/pubs/ties98tr.html}.
#' @source These data are also available from OzDASL, the Australian Data and
#' Story Library (\url{http://www.statsci.org/data/}).  (Note: The time measurements
#' of the data at OzDASL are in units of tens of hours.)
#' @keywords datasets
"poison"


#' Survival of Passengers on the Titanic (from package carData)
#'
#' Information on the survival status, sex, age, and passenger class of 1309
#' passengers in the Titanic disaster of 1912.
#'
#' This is part of a larger data set compiled by Thomas Cason.  Many additional
#' details are given in the sources cited below.
#'
#' @docType data
#' @format A data frame with 1309 observations on the following 4 variables. \describe{
#'   \item{survived}{\code{no} or \code{yes}.} \item{sex}{\code{female} or \code{male}.}
#'   \item{age}{in years (and for some children, fractions of a year); age is missing for
#'   263 of the passengers.}
#'   \item{passengerClass}{\code{1st}, \code{2nd}, or \code{3rd} class.} }
#' @references \url{http://www.encyclopedia-titanica.org/}
#'
#' F. E. Harrell, Jr. (2001) \emph{Regression Modeling Strategies} New York:
#' Springer.
#' @source Data set \code{titanic3} from
#' \url{http://biostat.mc.vanderbilt.edu/twiki/bin/view/Main/DataSets}.
#' @keywords datasets
"TitanicSurvival"


#' Canadian Crime-Rates Time Series (from package carData)
#'
#' This data frame has 38 rows and 7 columns.
#' The data are an annual time-series from 1931 to 1968. There are
#' some missing data.
#'
#' The post-1948 crime rates have been adjusted to account for
#' a difference in method of recording. Some of your results will differ
#' in the last decimal place from those in Table 14.1 of Fox (1997) due
#' to rounding of the data. Missing values for 1950 were interpolated.
#'
#' @docType data
#' @format This data frame contains the following columns:
#' \describe{
#'      \item{year}{
#'        1931--1968.
#'      }
#'      \item{tfr}{
#'        Total fertility rate per 1000 women.
#'      }
#'      \item{partic}{
#'        Women's labor-force participation rate per 1000.
#'      }
#'      \item{degrees}{
#'      Women's post-secondary degree rate per 10,000.
#'      }
#'      \item{fconvict}{
#'        Female indictable-offense conviction rate per 100,000.
#'      }
#'      \item{ftheft}{
#'        Female theft conviction rate per 100,000.
#'      }
#'      \item{mconvict}{
#'        Male indictable-offense conviction rate per 100,000.
#'      }
#'      \item{mtheft}{
#'        Male theft conviction rate per 100,000.
#'      }
#'    }
#' @source Personal communication from T. Hartnagel,
#' Department of Sociology, University of Alberta.
#'
#' @references
#'  Fox, J., and Hartnagel, T. F (1979)
#'  Changing social roles and female crime in Canada:
#'    A time series analysis.
#'  \emph{Canadian Review of Sociology and Anthroplogy},
#'  \bold{16}, 96--104.
#'
#'  Fox, J. (2016)
#'  \emph{Applied Regression Analysis and Generalized Linear Models},
#'  Third Edition. Sage.
"Hartnagel"


#' Self-Reports of Height and Weight (from package carData)
#'
#' The \code{Davis} data frame has 200 rows and 5 columns.
#' The subjects were men and women engaged in regular exercise.
#' There are some missing data.
#'
#' @format  This data frame contains the following columns:
#'   \describe{
#'     \item{sex}{
#'       A factor with levels:
#'         \code{F}, female;
#'       \code{M}, male.
#'     }
#'     \item{weight}{
#'       Measured weight in kg.
#'     }
#'     \item{height}{
#'       Measured height in cm.
#'     }
#'     \item{repwt}{
#'       Reported weight in kg.
#'     }
#'     \item{repht}{
#'       Reported height in cm.
#'     }
#'   }
#'
#' @source
#' Personal communication from C. Davis, Departments of
#' Physical Education and Psychology, York University.
#'
#' @references
#' Davis, C. (1990)
#' Body image and weight preoccupation: A comparison between exercising
#' and non-exercising women.
#' \emph{Appetite}, \bold{15}, 13--21.
#'
#' Fox, J. (2016)
#' \emph{Applied Regression Analysis and Generalized Linear Models},
#' Third Edition. Sage.
#'
#' Fox, J. and Weisberg, S. (2019)
#' \emph{An R Companion to Applied Regression}, Third Edition, Sage.
#'
"Davis"


#' Brain and Body Weights for 62 Species of Land Mammals (from package MASS)
#'
#'  A data frame with average brain and body weights for 62 species
#'  of land mammals.
#'
#' @format
#'   \describe{
#'     \item{\code{body}}{
#'       body weight in kg.
#'     }
#'     \item{\code{brain}}{
#'       brain weight in g.
#'     }
#'     \item{\code{name}}{
#'       Common name of species.
#'       (Rock hyrax-a = \emph{Heterohyrax brucci},
#'         Rock hyrax-b = \emph{Procavia habessinic.}.)
#'     }
#'   }
#'
#' @source
#'  Weisberg, S. (1985)
#'  \emph{Applied Linear Regression.} 2nd edition. Wiley, pp. 144--5.
#'
#'  Selected from:
#'  Allison, T. and Cicchetti, D. V. (1976)
#'  Sleep in mammals: ecological and constitutional correlates.
#'  \emph{Science} \bold{194}, 732--734.
#'
#' @references
#'  Venables, W. N. and Ripley, B. D. (1999)
#'  \emph{Modern Applied Statistics with S-PLUS.} Third
#'  Edition. Springer.
"mammals"


#' Brain and Body Weights for 28 Species (from package MASS)
#'
#' Average brain and body weights for 28 species of land animals
#'
#' @format
#'   \describe{
#'     \item{\code{body}}{
#'       body weight in kg.
#'     }
#'     \item{\code{brain}}{
#'       brain weight in g.
#'     }
#'   }
#'
#' @note The name \code{Animals} avoids conflicts with a system dataset \code{animals} in S-PLUS 4.5 and later.
#'
#' @source
#'   P. J. Rousseeuw  and A. M. Leroy (1987)
#'   \emph{Robust Regression and Outlier Detection.}
#'   Wiley, p. 57.
#'
#' @references
#'  Venables, W. N. and Ripley, B. D. (1999)
#'  \emph{Modern Applied Statistics with S-PLUS.} Third
#'  Edition. Springer.
"Animals"


#' Rat weight over time for different diets (adapted from package nlme)
#'
#' The \code{BodyWeight} data frame has 176 rows and 4 columns.
#'
#'  Hand and Crowder (1996) describe data on the body weights of rats
#'  measured over 64 days.  These data also appear in Table 2.4 of
#'  Crowder and Hand (1990).  The body weights of the rats (in grams)
#'  are measured on day 1 and every seven days thereafter until day 64,
#'  with an extra measurement on day 44. The experiment started several
#'  weeks before ``day 1.''  There are three groups of rats, each on a
#'  different diet.
#'
#' @format
#' This data frame contains the following columns:
#' \describe{
#'     \item{weight}{
#'         a numeric vector giving the body weight of the rat (grams).
#'     }
#'     \item{Time}{
#'         a numeric vector giving the time at which the measurement is
#'         made (days).
#'     }
#'     \item{Rat}{
#'         an factor identifying the rat whose weight is measured.
#'     }
#'     \item{Diet}{
#'         a factor with levels \code{1} to \code{3} indicating the diet that the rat receives.
#'     }
#' }
#'
#' @source
#'   Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
#'       and S-PLUS}, Springer, New York. (Appendix A.3)
#'
#'   Crowder, M. and Hand, D. (1990), \emph{Analysis of Repeated
#'       Measures}, Chapman and Hall, London.
#'
#'   Hand, D. and Crowder, M. (1996), \emph{Practical Longitudinal Data
#'       Analysis}, Chapman and Hall, London.
#'
"bodyweight"


#' Split-plot Experiment on Varieties of Oats (adapted from package nlme)
#'
#' The \code{Oats} data frame has 72 rows and 4 columns.
#'
#'  These data have been introduced by Yates (1935) as an example of a
#'  split-plot design. The treatment structure used in the experiment was
#'  a \eqn{3 \times 4}{3 x 4} full factorial, with three varieties of oats
#'  and four concentrations of nitrogen. The experimental units were
#'  arranged into six blocks, each with three whole-plots subdivided into
#'  four subplots. The varieties of oats were assigned randomly to the
#'  whole-plots and the concentrations of nitrogen to the subplots.  All
#'  four concentrations of nitrogen were used on each whole-plot.
#'
#' @format
#' This data frame contains the following columns:
#'  \describe{
#'    \item{Block}{
#'      an ordered factor with levels
#'      \code{VI} < \code{V} < \code{III} < \code{IV} < \code{II} < \code{I}
#'    }
#'    \item{Variety}{
#'      a factor with levels
#'      \code{Golden Rain}
#'      \code{Marvellous}
#'      \code{Victory}
#'    }
#'    \item{nitro}{
#'      a numeric vector
#'    }
#'    \item{yield}{
#'      a numeric vector
#'    }
#'  }
#'
#' @source
#'  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
#'    and S-PLUS}, Springer, New York.  (Appendix A.15)
#'
#'  Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied
#'    Statistics with S. (4th ed)}, Springer, New York.
#'
"oatsyield"

#' Carnivora body sizes and life history traits (adapted from package ape)
#'
#' Dataset adapted from Gittleman (1986), including 2 morphological variables (body and brain sizes),  8 life history traits variables and 4 taxonomic variables.
#'
#'@format
#'  A data frame with 112 observations on 7 variables.
#'
#'  \tabular{rlll}{
#'    [,1]  \tab Order       \tab factor  \tab Carnivora order \cr
#'    [,2]  \tab SuperFamily \tab factor  \tab Super family (Caniformia or Feliformia) \cr
#'    [,3]  \tab Family      \tab factor  \tab Carnivora family \cr
#'    [,4]  \tab Genus       \tab factor  \tab Carnivora genus \cr
#'    [,5]  \tab Species     \tab factor  \tab Carnivora species \cr
#'    [,6]  \tab Brain       \tab numeric \tab Average brain weight of adult male and adult female (g) \cr
#'    [,7]  \tab Weight      \tab numeric \tab Average body weight of adult male and adult female (kg) \cr
#'  }
#'
#'@source
#'  Gittleman, J. L. (1986) Carnivore life history patterns: allometric,
#'  phylogenetic and ecological associations. \emph{American Naturalist},
#'  \bold{127}: 744--771.
#'
#' @references
#'   Paradis E. & Schliep K. 2018. ape 5.0: an environment for modern phylogenetics
#'   and evolutionary analyses in R. Bioinformatics 35: 526-528.
#'
"carnivora"


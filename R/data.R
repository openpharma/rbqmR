#' The Kalbfleisch and Prentice (1980) VA lung dataset.
#' 
#'  @format A tibble with 137 rows and 8 variables:
#'  \describe{
#'    \item{Treatment}{Treatment group}
#'    \item{CellType}{cell type}
#'    \item{SurvivalTime}{Survival time since randomisation}
#'    \item{Status}{1=Event, 0=Censored}
#'    \item{KPS}{Karnofsky Performance Status}
#'    \item{Age}{Age in years at randomisation}
#'    \item{PriorTherapy}{Boolean.  received prior therapy?}
#'    }
#'  @source Kalbfleisch D and Prentice RL (1980), The Statistical Analysis of
#'  Failure Time Data. Wiley, New York 
"vaLung"

#' The Bortkiewicz cavalry dataset
#'
#' A dataset number of Prussian cavalry soldiers kicked to death by their horses
#'
#' @format A tibble with 280 rows and 3 variables:
#' \describe{
#'   \item{Corps}{The corps}
#'   \item{Year}{The year}
#'   \item{Deaths}{The number of deaths in the given corps in the given year}
#'   ...
#' }
#' @source \url{https://archive.org/details/dasgesetzderklei00bortrich}
"cavalryDeaths"

#' Data adapted from Berry et al (2011) pp 52-63
#'
#'A dataset of subject and event counts by site
#'
#' @format A tibble with 9 rows and 4 columns
#' \describe{
#'   \item{Site}{The site ID}
#'   \item{Subjects}{The number of subjects at the site}
#'   \item{Events}{The number of subjects ith at least one event at the site}
#'   \item{ObservedResponse}{The observed response rate `Events`/`Subjects` at
#'    the site}
#' }
"berrySummary"

#' 
#' Data derived and adapted from Berry et al (2011) pp 52-63
#'
#' A dataset of subject and event flags
#'
#' @format A tibble with 150 rows and 3 columns
#' \describe{
#'   \item{Site}{The site ID}
#'   \item{SubjectID}{The subject ID}
#'   \item{Event}{Whether or not the subject experienced an event}
#' }
"berrySubject"

#' 
#' A dataset of event rates
#'
#' @format A tibble with 47 rows and 6 columns
#' \describe{
#'   \item{Site}{The site ID}
#'   \item{Country}{The subject ID}
#'   \item{Patients}{The number of patients at the site}
#'   \item{Exposure}{The total exposure at the site, in months}
#'   \item{Events}{The total number of events at the site}
#'   \item{Rate}{The site's event rate (events per month)}
#' }
"siteRates"
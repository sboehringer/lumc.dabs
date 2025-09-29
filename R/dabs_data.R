#
#	dabs_data.R
#Mon Sep 15 02:12:26 PM CEST 2025
#	Description of all data sets

#' CD19 Dataset
#'
#' Data set with T-cell measurements readarding surface protein CD19.
#'
#' @format A data frame with 150 rows and 5 columns:
#' \describe{
#'   \item{Sepal.Length}{Length of the sepal in centimeters}
#'   \item{Sepal.Width}{Width of the sepal in centimeters}
#'   \item{Petal.Length}{Length of the petal in centimeters}
#'   \item{Petal.Width}{Width of the petal in centimeters}
#'   \item{Species}{Species of the iris flower}
#' }
#' @source The NEO study.
#' @docType data
#' @name cd19
"cd19"


#' NECOSAD Dataset
#'
#' Data from The Netherlands Cooperative Study on the Adequacy of Dialysis (NECOSAD) trial measuring physical activity of dialysis patients.
#'
#' @format A data frame with 1201 rows and 6 columns:
#' \describe{
#'   \item{patnr}{Patient number}
#'   \item{sf_phys0}{Physical activity score on a scale from 0 to 100 measured at baseline}
#'   \item{sf_phys6}{Physical activity score on a scale from 0 to 100 measured at 6 month}
#'   \item{gender}{Sex as reported in the electronic health record}
#'   \item{age}{Age measured in years}
#' }
#' @source The NECOSAD study. https://pubmed.ncbi.nlm.nih.gov/16105051/
#' @docType data
#' @name cd19

"necosad"

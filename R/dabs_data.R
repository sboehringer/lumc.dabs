#
#	dabs_data.R
#Mon Sep 15 02:12:26 PM CEST 2025
#	Description of all data sets

#' CD19 Dataset
#'
#' Hematopoietic stem cell transplantation is an effective treatment in a variety of diagnoses, and is investigated intensively in the LUMC. One research line concerns injection of CD34+-cells from a donor in patients with, for instance, leukaemia. CD34 is a surface marker for  hematopoietic progenitor cells which can differentiate into a variety of lymphoid and myeloid hematopoietic cells. The efficacy of such an injection can be evaluated by measuring CD19+ cells in bone marrow which represent newly differentiated B-cells. CD19 is a cell-surface protein present on normal and neoplastic B-cells.<br>
#' In a biomedical experiment the goal is to evaluate (1) the dose-effect relationship between injection of CD34+ cells and the engraftment, as measured by the percentage of CD19+ cells, and (2) the difference in dose-effect between injecting CD34+ cells from umbilical cord blood (UCB), bone marrow (BM) or mobilized peripheral blood (mPB). A number of mice were injected with varying doses of CD34+ cells from UCB, BM or mPB, and in each animal the engraftment was evaluated by measuring the percentage CD19+ cells in bone marrow of these animals.
#'
#' @format A data frame with 60 rows and 3 columns:
#' \describe{
#'   \item{cd19}{Continuous variable with cd19+ concentration measurements (unit cells/μL)}
#'   \item{dose}{Factor with levels "0.10", "0.5", "1.0", "2.0" corresonding to the concentration of CD34+ cells (unit cells/μL)}
#'   \item{source}{Factor with levels UCB (umbilical cord blood), BM (bone marrow), mPB (mobilized peripheral blood), corresponding to the origin of the sample.}
#' }
#' @docType data
#' @name cd19
"cd19"


#' NECOSAD Dataset
#'
#' Data from The Netherlands Cooperative Study on the Adequacy of Dialysis (NECOSAD) trial measuring physical activity of dialysis patients. \strong{This data set cannot be used to reproduce findings from the study as this is data set is a subset of the total study data that has been tailored for use in teaching}.
#'
#' @format A data frame with 1201 rows and 6 columns:
#' \describe{
#'   \item{patnr}{Patient number}
#'   \item{sf_phys0}{Physical activity score on a scale from 0 to 100 measured at baseline}
#'   \item{sf_phys6}{Physical activity score on a scale from 0 to 100 measured at 6 month}
#'   \item{gender}{Sex as reported in the electronic health record. Levels: femaale, male}
#'   \item{age}{Age measured in years}
#'   \item{therapy}{The treatment given to the patient. Levels: peritoneal, hemodialysis}
#' }
#' @source The NECOSAD study. https://pubmed.ncbi.nlm.nih.gov/16105051/
#' @docType data
#' @name necosad
"necosad"


#' Pea data set from a factorial experiment
#'
#' The second problem is that more and more apps are now checking to see if your phone is rooted, and then refuse to run. I don't know if LineageOS can lie to the apps but the stock Android can't, so if I root my phone I can lose access to my banking apps (which are becoming pretty much mandatory for life nowadays).
#'
#' @format The 'pea' data frame has 24 rows and 5 columns:
#' \describe{
#'   \item{block}{which block (label 1 to 6).}
#'   \item{N}{indicator (0/1) for the application of nitrogen.}
#'   \item{P}{indicator (0/1) for the application of phosphate.}
#'   \item{K}{indicator (0/1) for the application of potassium.}
#'   \item{yield}{Yield of peas, in pounds/plot (the plots were (1/70) acre)}
#'	 \item{combination}{Factor representing the combination of columns N, P, K for each row as a factor level.}
#' }
#' @source Derived from the R dataset npk
#' @seealso npk
#' @docType data
#' @name pea
"pea"

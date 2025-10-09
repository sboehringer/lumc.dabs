#
#	dabsFunctions.R
#Thu Oct  9 10:22:34 AM CEST 2025

# Functionsn needed for the DABS exercises



#' Compute summary statistics on vector
#'
#' Compute summary statistics mean and standard deviation for given numeric vector.
#'
#' @param x Numeric vector for which summaries are to be computed
#' @details Compute summary statistics mean and standard deviation for given numeric vector. Missing values are ignored in the calculations.
#' @returns Returns list with elements \code{mean} and \code{sd}
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso varSummaries
#' @keywords exercise summary
#' @examples
#'
#' data(iris)
#' varSummary(iris[, 1])
#' @export varSummary
varSummary <- function(x) {
	Mean <- mean(x, na.rm = TRUE)
	Sd <- sd(x, na.rm = TRUE)
	summ <- c(mean = Mean, sd = Sd)
}

#' Compute stratified summary statistics continuous variables
#'
#' Compute stratified summary statistics mean and standard deviation for columns containing continuous variables in a data frame.
#'
#' @param data data frame with data to be summarized
#' @param strat name of column containing the stratifying factor
#' @param cols names of columns to be summarized
#' @details Compute stratified summary statistics mean and standard deviation for columns in a data frame. If no columns are specified all columns are analyzed. The stratification variable is given by name and used to create subsets of the data for each factor level. Summaries are computed for the resulting data subsets.
#' @returns Returns list with elements \code{mean} and \code{sd}
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso varSummary varFrequenciesBy
#' @keywords exercise summary
#' @examples
#'
#' data(iris)
#' varSummariesBy(iris, 'Species')
#' varSummariesBy(iris, 'Species', c('Sepal.Length', 'Sepal.Width'))
#' @export varSummariesBy
varSummariesBy <- function(data, strat, cols = names(data)) {
	cols = setdiff(cols, strat)
    S = by(data, data[[strat]], function(d) {
        do.call(rbind, nlapply(cols, \(.)varSummary(d[[.]])))
    })
    SetNames(S, Dimnames = listKeyValue(strat, dimnames(S)[1]))
}

#' Compute stratified frequencies
#'
#' Compute stratified frequencies for columns containing categorical variables in a data frame.
#'
#' @param data data frame with data to be summarized
#' @param strat name of column containing the stratifying factor
#' @param cols names of columns to be summarized
#' @details Compute stratified frequency statistics for columns in a data frame. If no columns are specified all columns are analyzed. The stratification variable is given by name and used to create subsets of the data for each factor level. Frequencies are computed for the resulting data subsets.
#' @returns Returns list of frequncy tables for the columns
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso varSummary varSummariesBy
#' @keywords exercise summary
#' @examples
#'
#' data(mtcars)
#' varFrequenciesBy(mtcars, 'am', 'gear')
#' @export varFrequenciesBy
varFrequenciesBy <- function(data, strat, cols = names(data), simplify = TRUE) {
	cols = setdiff(cols, strat)
    tableFreq <- function(x) { table(x)/sum(table(x)) }
	freqs = nlapply(cols, \(.) {
		F = by(data, data[[strat]], function(d) {
			T = tableFreq(d[[.]]);
			SetNames(T, Dimnames = listKeyValue(., dimnames(T)[1]))
		})
		F = SetNames(F, Dimnames = listKeyValue(strat, dimnames(F)[1]));
	});
	if (simplify && length(cols) == 1) return(freqs[[1]]);
	return(freqs);
}

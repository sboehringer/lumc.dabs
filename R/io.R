#
#	io.R
#Tue Sep 30 11:46:15 AM CEST 2025

# Functions for writing templates to a working folder


writePackageFile = function(name, folder, exts = c('Rmd', 'md', 'pdf'), overwrite = FALSE) {
	path = firstSystemFile(name, exts, package = 'lumc.dabs');
	if (is.null(path)) stop(Sprintf('[writePackageFile] Error not found: name=%{names}s'));
	file.copy(path, folder, overwrite = overwrite)
}


#' List available exercies
#'
#' This function lists all available exercises
#'
#' @return Returns a character vector with names of the exercises
#' @details This function lists all available exercises
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso writeExercise
#' @keywords exercise template
#' @examples
#'
#' listExercises()
#' @export listExercises
listExercises = function() {
	exercises = list.files(system.file('exercises', package = 'lumc.dabs'), pattern = '[.]Rmd$');
	cat('Available Exercises\n');
	sapply(sort(exercises), (cat(Sprintf(' • %{.}q\n'))) %.% (splitPath(.)$base));
	invisible(sapply(exercises, (splitPath(.)$base) %.% .))
}

#' Write exercise template to disk
#'
#' This function write an exercise by a given name into a folder on disk.
#'
#' @param name name of the exercise (see see-also)
#' @param folder folder to which the file is to be written. Deafults to the current working directory.
#' @param overwrite Should the file be overwritten if already existing?
#'
#' @details This function creates a new file in the specified folder.
#' @author Stefan Böhringer, \email{r-packages@@s-boehringer.org}
#' @seealso listExercises
#' @keywords write exercise template
#' @examples
#'
#' print(tempdir())
#' writeExercise('Linear Regression', tempdir())
#' @export writeExercise
writeExercise = function(name, folder = getwd(), overwrite = FALSE) {
	writePackageFile(Sprintf('exercises/%{name}s'), folder, overwrite = overwrite);
}


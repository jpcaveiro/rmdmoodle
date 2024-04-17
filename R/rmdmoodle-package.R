
# https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE


# environment for package ====


#' Trata-se de um "environment" local mas que permite mudar
#' o valor da variável "global" dentro deste package.
pkg_env <- new.env()
pkg_env$EXERCISE_ROOT <- ""
pkg_env$WARNINGS.BOOLEAN <- FALSE





#' Set a global variable with the OS path of the questions
#' "database" in the filesystem .
#'
#' @param pathstr string
#'
#' @return string
#' @export
#'
#' @examples
#' set_exercise_root("c:/Users/name/Documents/rmdmoodle")
set_exercise_root <- function(pathstr) {
  pkg_env$EXERCISE_ROOT <- pathstr
  return(pkg_env$EXERCISE_ROOT)
}


#' Get a global variable with the OS path of the questions
#' "database" in the filesystem .
#'
#' @return A string containing a OS path.
#' @export
#'
get_exercise_root <- function() {
  return(pkg_env$EXERCISE_ROOT)
}





#' Turn TRUE or FALSE the
#' printing of warnings.
#'
#' @param b - a boolean TRUE or FALSE
#'
#' @return
#' @export
set_warnings <- function(b) {
  pkg_env$WARNINGS.BOOLEAN <- b
}




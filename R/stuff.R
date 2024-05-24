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
#' \dontrun{set_exercise_root("c:/Users/name/Documents/rmdmoodle")}
set_exercise_root <- function(pathstr) {
  pkg_env$EXERCISE_ROOT <- pathstr
  return(pkg_env$EXERCISE_ROOT)
}


#' Get a global variable with the OS path of the questions
#' "database" in the filesystem .
#'
#' @return A string containing a OS path.
get_exercise_root <- function() {
  return(pkg_env$EXERCISE_ROOT)
}





#' Turn TRUE or FALSE the
#' printing of warnings.
#'
#' @param b - a boolean TRUE or FALSE
#'
#' @return nothing
set_warnings <- function(b) {
  pkg_env$WARNINGS.BOOLEAN <- b
}




# other functions ====



#' Transform "Title f(2024)" into "Title_f-2024-"
#'
#' @param x character string
#' @param non_alphanum_replace "-" if nonalpha appears in `x`
#' @param space_replace "_" if spaces appears in `x`
#' @param tolower FALSE
#'
#' @return a string
#'
#' @examples
#' slugify("Title f(2024)")
slugify <- function(x, non_alphanum_replace="-", space_replace="_", tolower=FALSE) {
  x <- gsub("[^[:alnum:] ]", non_alphanum_replace, x)
  x <- trimws(x)
  x <- gsub("[[:space:]]", space_replace, x)

  if(tolower) { x <- tolower(x) }

  return(x)
}




#' Add an extension to a file name
#'
#' If the file name has the extension then
#' nothing is added.
#'
#' @param filename possibly with or without extension
#' @param extension Rmd, R, .Rmd, .R, or any other
#'
#' @return character file name with extension
#'
#' @examples
#' add_extension("f",".Rmd")
#' add_extension("f",".R")
#' add_extension("f.Rmd","R")
add_extension <- function(filename, extension) {

  if(!grepl("\\.",extension)) {
    extension <- paste0(".",extension)
  }

  nlen_ext <- nchar(extension)
  nlen_filename <- nchar(filename)

  if (nlen_filename>=nlen_ext) {

    # Extract the last 5 characters
    last_chars <- substr(filename, nlen_filename - (nlen_ext-1), nlen_filename)


    if (last_chars == extension) {
      filename_ext <- filename
    } else {
      filename_ext <- paste0(filename, extension)
    }
  } else {
    filename_ext <- paste0(filename, extension)
  }

  return(filename_ext)
}




#' Remove an extension to a file name
#'
#' If the file name has the extension then
#' nothing is added.
#'
#' @param filename possibly with or without extension
#' @param extension Rmd, R, .Rmd, .R, or any other
#'
#' @return character file name with extension
#'
#' @examples
#' \dontrun{remove_extension("f",".Rmd")}
#' \dontrun{remove_extension("f",".R")}
#' \dontrun{remove_extension("f.Rmd","R")}
remove_extension <- function(filename, extension) {

  if(!grepl("\\.",extension)) {
    filename_no_ext <- filename
  }

  nlen_ext <- nchar(extension)
  nlen_filename <- nchar(filename)

  if (nlen_filename >= nlen_ext) {
    filename_no_ext <- substr(filename, 1, nlen_filename - (nlen_ext-1))
  } else {
    stop(paste0("Filename '", filename, "' should be greater than '", extension, "'.\n"))
  }

  return(filename_no_ext)
}





# variável local
DEBUG <- F


# environment local mas que permite mudar
# o valor da variável dentro deste package.
pkg.env <- new.env()
pkg.env$EXERCISE_ROOT <- ""


# EXERCISE_ROOT <- "C:/Users/pedrocruz/Documents/GitHub/bioestatistica/rmdmoodle"



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
  pkg.env$EXERCISE_ROOT <- pathstr
  return(pkg.env$EXERCISE_ROOT)
}


#' Get a global variable with the OS path of the questions
#' "database" in the filesystem .
#'
#' @return A string containing a OS path.
#' @export
#'
get_exercise_root <- function() {
  return(pkg.env$EXERCISE_ROOT)
}


#Talvez esta função não seja ainda útil
#porque o autor deve especificar quantas variantes pretende.
# Se pretender mais que VARCOUNT vai surgir erro quando VAR>VARCOUNT.
get_varcount <- function(curr_line) {
    # parse: VARCOUNT = 6 or VARCOUNT    <-   3

    #produce a list with one element: r[[1]]
    #r[[1] is a c() of strings
    #nchar() is the lengths of each string
    r <- strsplit(curr_line, " ")
    clean_r <- r[[1]][nchar(r[[1]])!=0]

    if (length(clean_r)<3) {
        stop("VARCOUNT as no value")
    }
    return(as.integer(clean_r[3]))
}




next_line <- function(curr_lineno, lines) {

  curr_lineno <- curr_lineno + 1

  if (DEBUG) {
    cat("next line", curr_lineno, "is:", lines[curr_lineno], '\n')
  }
  return(curr_lineno)

}





skip_chunk <- function(curr_lineno, lines) {

    nlines <- length(lines)

    if (grepl("^```", lines[curr_lineno])) {
        if (DEBUG) {
          cat("chunk starts in line ", curr_lineno, '\n')
        }
        curr_lineno <- curr_lineno + 1
        while (curr_lineno <= nlines && !grepl("^```", lines[curr_lineno])) {
          curr_lineno <- next_line(curr_lineno,lines)
        }
        if (curr_lineno > nlines) {
          stop("Chunk does not stop!")
        } else {
          curr_lineno <- next_line(curr_lineno,lines)
        }
        if (DEBUG) {
          cat("chunk end in line ", curr_lineno-1, '\n')
        }

    } else {
        curr_lineno <- next_line(curr_lineno,lines)
    }


    return(curr_lineno)
}




change_state <- function(to, debug_str) {

  if (DEBUG) {
    cat("   change parser state to ", debug_str, '\n')
  }

  return(to)
}


get_ex_type <- function(curr_line) {
  #TODO: improve
  #line with #enredo can have a type:
  # enredo <empty>
  # enredo cloze
  # enredo essay
  # etc
  ty <- trimws( substr(curr_line, start = 8, stop = nchar(curr_line)) ) #enredo
  if (length(ty)==0) {
    ty <- "cloze"
  }
  return(ty)
}




#' Parse an Rmd file containing an exercise ()
#'
#' `parse_exrmdfile`
#'
#'
#' @param rmdfilename
#'
#' @return list(ex_title, ex_type, code, enredo, alineas, alineas_title)
#'
#' @examples
parse_exrmdfile <- function(rmdfilename) {


  # inside no code yet
  S_BEFORECODE <- 0

  # after line with # código has been recognized
  S_CODE <- 20

  # line with # enredo has been recognized
  S_ENREDO <- 30

  # line with # alínea has been recognized
  S_ALINEAS <- 40

  # Loop through each line of the Rmd file
  # Read the Rmd file into lines
  lines <- readLines(rmdfilename)
  nlines <- length(lines)
  curr_state <- change_state(S_BEFORECODE, "S_BEFORECODE")
  curr_lineno <- next_line(0,lines)


  #variáveis resultantes do parse
  #varcount <- FALSE
  line_start_code <- NULL
  line_end_code <- NULL
  ex_type <- NULL
  ex_title <- NULL
  line_start_enredo <- NULL
  line_end_enredo <- NULL
  list_of_alineas <- list()
  list_of_alineas_title <- list()
  no_of_alineas <- 0



  #This "while()" it's a state machine whose states are:
  # S_BEFORECODE, S_CODE, S_ENREDO, S_ALINEAS
  while (curr_lineno <= nlines) {

    #to avoid infinite loops
    protect_counter <- curr_lineno


    if (curr_state == S_BEFORECODE) {

      curr_line <- lines[curr_lineno]

      if (grepl("^title:",curr_line)) {
        ex_title <- substr(curr_line, start = 8, stop = nchar(curr_line))
        ex_title <- gsub('"', ' ', ex_title)
        ex_title <- trimws(ex_title)
        curr_lineno <- next_line(curr_lineno,lines)
      } else if (grepl("^#", curr_line) &&
                 grepl("código", tolower(curr_line))) {
        curr_state <- change_state(S_CODE, "S_CODE")
        curr_lineno <- next_line(curr_lineno,lines)
        line_start_code <- curr_lineno
      } else {
        curr_lineno <- next_line(curr_lineno,lines)
      }

    } else if (curr_state == S_CODE) {

      curr_line <- lines[curr_lineno]

      #if (grepl("VARCOUNT",curr_line)) {
      #
      #  varcount <- get_varcount(curr_line)
      #  curr_lineno <- next_line(curr_lineno,lines)
      #
      #} else

      if (grepl("^#", curr_line) && #line # enredo
                 grepl("enredo", tolower(curr_line))) {

        ex_type <- get_ex_type(curr_line)

        line_end_code <- curr_lineno - 1

        curr_state  <- change_state(S_ENREDO, "S_ENREDO")
        curr_lineno <- next_line(curr_lineno,lines)
        line_start_enredo <- curr_lineno

        #if (!varcount) {
        #  #improve this message
        #  #paste() is used to break into smaller lines
        #  stop(paste0("Missing chunk with 'VARCOUNT = 6' ",
        #              "or any other value."))
        #}


      } else {

        curr_lineno <- skip_chunk(curr_lineno, lines)

      }

    } else if (curr_state == S_ENREDO) {

      curr_line <- lines[curr_lineno]

      if (grepl("^#", curr_line)) {

        line_end_enredo <- curr_lineno - 1

        curr_state  <- change_state(S_ALINEAS, "S_ALINEAS")

        curr_lineno <- next_line(curr_lineno,lines)
        line_start_alinea <- curr_lineno
        #store the next alinea title
        list_of_alineas_title[[no_of_alineas+1]] <- curr_line


      } else {

        curr_lineno <- skip_chunk(curr_lineno, lines)

      }


    } else if (curr_state == S_ALINEAS) {

      curr_line <- lines[curr_lineno]

      if (grepl("^#", curr_line)) {

        line_end_alinea <- curr_lineno - 1

        #store previous alínea
        no_of_alineas <- no_of_alineas + 1
        list_of_alineas[[no_of_alineas]] <-
          paste(lines[line_start_alinea:line_end_alinea],
                collapse = '\n')

        #store the next alinea title
        list_of_alineas_title[[no_of_alineas+1]] <- curr_line


        curr_lineno <- next_line(curr_lineno,lines)

        line_start_alinea <- curr_lineno


      } else {

        curr_lineno <- skip_chunk(curr_lineno, lines)

      }


    } else {
      stop("curr_state is unknown.")
    }

    if (DEBUG &&   protect_counter == curr_lineno) {
      stop("probably, infinite loop")
    }


  } #end while

  if (line_start_alinea <= nlines) {
    #store alínea
    no_of_alineas <- no_of_alineas + 1
    list_of_alineas[[no_of_alineas]] <- paste(lines[line_start_alinea:nlines],collapse='\n')
  }


  return(list(title = ex_title,
              type = "cloze",
              code    = paste0(lines[line_start_code:line_end_code], collapse='\n'),
              enredo  = paste0(lines[line_start_enredo:line_end_enredo], collapse='\n'),
              alineas = list_of_alineas,
              alineas_title = list_of_alineas_title))

}


#DEBUG
#res <- parse_exrmdfile("c3-estimativa.Rmd")
#print(res)



#' Find "alinea" Rmd text by searching `alinea_tag`.
#'
#' * `ex_struct$alineas`is a list of Rmd strings;
#' * `ex_struct$alineas_title` is a list of titles (containing an `alinea_tag`).
#'
#' @param ex_struct see return value of `parse_exrmdfile()`
#' @param alinea_tag
#'
#' @return string (rmd string)
#'
#' @examples
find_alinea <- function(alinea_tag,ex_struct) {

  if (DEBUG) {
    print("exercise structure:")
    print(ex_struct)
  }

  nalineas <- length(ex_struct$alineas)
  i <- 1
  found <- F
  while (!found && i<=nalineas) {
    if (grepl(alinea_tag,ex_struct$alineas_title[[i]])) {
      found <- T
    } else {
      i <- i + 1
    }
  }
  if (!found) {
    stop(sprintf("`alinea_tag = %s` cannot be found in exercise %s", alinea_tag, ex_struct$title))
  }
  return(ex_struct$alineas[[i]])
}




# Templating
#library(whisker)


part1_header <- paste0("---",
                      "title: \"2022-2023 1o teste (turno 1)\"",
                      "author: \"(preencher)\"",
                      "date: \"(preencher)\"",
                      "output: html_document",
                      "---",
                      collapse='\n')
#cat(part1_header,'\n')


# https://www.statology.org/turn-off-scientific-notation-in-r
# opções por defeito nos chunks
# str(knitr::opts_chunk$get())
part2_show <- paste0( "Para controlar o que globalmente surge:",
                      "```{r setup, include = F, echo = F, results = F}",
                      "#todo - improve",
                      "knitr::opts_chunk$set(echo = TRUE, results='show', message=FALSE, fig.show='show')",
                      "options(scipen = 999)", #turn-off-scientific-notation-in-r
                      "SHOWCODE <- F",
                      "SHOWRESULTS <- F",
                      "```",
                      collapse = '\n')
#cat(part2_show,'\n')








#' exer2rmdstring
#'
#' @param nvar number of variants (nrep <= VARCOUNT defined inside exercise)
#' @param ... ex. "cap3/c3-estimativa.Rmd", "alinea01", "alinea04"
#'
#' @return string containing Rmd with # and ## sections (exercise title and variants)
#'
#' @examples
#'    exerc2rmdstring(6, "cap3/c3-estimativa.Rmd", "alinea01", "alinea04")
exer2rmdstring <- function(nvar, ...) {


  #list(ex_title = ex_title,
  #     ex_type = "cloze",
  #     code = lines[line_start_code:line_end_code],
  #     enredo = lines[line_start_enredo:line_end_enredo],
  #     alineas = list_of_alineas))


  exrequest = list(...)[[1]]

  if (grepl(".Rmd", exrequest[[1]])) {
    rmdfilename <- exrequest[[1]]
  } else {
    rmdfilename <- paste0(exrequest[[1]],".Rmd", collapse='')
  }

  ex <- parse_exrmdfile(file.path(pkg.env$EXERCISE_ROOT,rmdfilename))


  # output string containing Rmd (headerless) to be joint with other exercises
  ex_rmdtext <- ""

  # A # section is an exercise in Rmd.
  ex_header  <- paste0("# ", ex$title, "-", toupper(ex$type), collapse=' ')
  ex_rmdtext <- paste0(ex_rmdtext, ex_header, collapse = '\n\n')

  #code
  ex_rmdtext <- paste0(ex_rmdtext, ex$code, collapse = '\n\n')

  # FAZER
  #   ## variante 1
  #    <enredo>
  #    (1) alinea
  #    (2) outra alinea
  #   ## variante 2
  #    <enredo>
  #    (1) alinea
  #    (2) outra alinea

  #alíneas
  al_request_number <- length(exrequest) - 1 #sem o rmdfilename
  al_total <- length(ex$alineas)
  al_string = ""

  for (al in 1:al_request_number) {

    al_rmdtext <- find_alinea(
                    alinea_tag = exrequest[[al+1]],
                    ex_struct = ex)

    # Generate variant: enredo (1) blá blá \n\n (2) blá blá \n\n etc
    al_string <- paste0(al_string,
                        paste0("**(", as.character(al), ")**", al_rmdtext),
                        collapse = '\n\n')

  }


  al_string <- paste0(al_string,
                      "\n\n### feedback\n\n",
                      collapse = '\n\n')


  for (v in 1:nvar) {

    #section ## Variante `r VAR<- v;VAR`

    sec_text <- sprintf("\n## variante `r VAR <- %d; VAR`\n\n", v)

    #generate variants
    ex_rmdtext <- paste0(ex_rmdtext,
                         sec_text,
                         ex$enredo,
                         al_string,
                         collapse = '\n\n')

  }


  return(ex_rmdtext)
}



#' Joins questions and items in an Rmd file (to be compiled).
#'
#' `rmdexam()` builds an Rmd file containing
#' an exam to be exported for moodle in a xml file.
#' After the revision of the author of the exam, then,
#' `xmlmoodle()` function must be called to make
#'  the xml file. Importing to moodle has its own
#'  "tricks" to be described elsewhere.
#'
#' @param nvariants numeric (number of variants for each question)
#' @param rmdfilename filename to store the "rmd exam"
#' @param ... each argument is a `c()` of strings;
#'
#' @return A file written in name given in `rmdfilename`
#' @export
#'
#' @examples
#' make_rmdmoodle(6,
#'                "my-new-moodle-exam.Rmd",
#'                c("cap1/c1-estimation.Rmd","est-mean","est-var"),
#'                c("cap2/c2-normalprob.Rmd","prob-less", "prob-greater", "prob-between"))
rmdexam <- function(nvariants, rmdfilename, ...) {

  #args is a list of lists
  #args[[1]][[1]] access the first exercise-filename to be written
  #to the new Rmd
  argslist = list(...)

  ex_rmdtext <- ""

  # Runs all requested questions
  for (qnum in 1:length(argslist)) {
    res <- exer2rmdstring(nvariants, argslist[[qnum]])
    ex_rmdtext <- paste0(ex_rmdtext,
                         res,
                         collapse = '\n\n')
  }


  comando_txt <- sprintf("rmdexam(%d,\n\"%s\",\n%s)\n",
                         nvariants,
                         rmdfilename,
                         paste( argslist, collapse=", \n" ))


  head_txt <- paste0(
    "---\n",
    "title: \"", rmdfilename, "\"\n",
    "author: \"(autores)\"\n",
    "date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\"\n",
    "output: html_document\n",
    "---\n\n",
    "A versão inicial deste documento foi construída com:\n",
    "```\n",
    "library(rmdmoodle)\n",
    "set_exercise_root( \"C:/Users/USERNAME/<Where is the exercise folder?>\" )\n",
    comando_txt,
    "```\n",
    "\n\n",
    collapse = '\n\n')
  #cat(head_txt)


  ex_rmdtext <- paste0(head_txt,
                       ex_rmdtext,
                       collapse = '\n\n')


  # Open and write
  con <- file(rmdfilename, "w", encoding = "UTF-8")
  cat(ex_rmdtext, file = con)
  close(con)


  cat("The author of the exam can check file", rmdfilename, "and search for eventual problems.\n")

  rmdfilename_no_ext <- substr(rmdfilename, 1, nchar(rmdfilename) - 4)

  xmlmoodle(  rmdfilename_no_ext )

  cat("Then import ", rmdfilename_no_ext, ".xml to moodle.\n", sep='')

  return(rmdfilename)
}



# library(openxlsx)
# # Create a new workbook and add a worksheet
# wb <- createWorkbook()
# addWorksheet(wb, "Sheet1")
# # Write some data to specific cells
# writeData(wb, "Sheet1", "Hello", startCol = 1, startRow = 1)
# writeData(wb, "Sheet1", "world!", startCol = 2, startRow = 1)
# # Save the workbook
# saveWorkbook(wb, "example.xlsx")

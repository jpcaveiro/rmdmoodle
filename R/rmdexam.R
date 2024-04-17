
# global variables ====

#' TODO: Isto não funciona quando colocado no ficheiro .lintr:
#' `indentation_linter(indent = 2L, hanging_indent_style = c("tidy", "always", "never"), assignment_as_infix = TRUE)`



#' Ativa alguns `cat()` espalhados pelo código que
#' processa os exercícios. Este módulo faz parse
#' de texto: ativando esta flag passam a ser vistos
#' muitos `cat()` no processo de debug.
DBG_rmdexam <- F



# functions ====


#' Get VARCOUNT <- 6 or VARCOUNT = 6.
#' Search in the line for that pattern.
#'
#' @return numeric>0 or 0 if no varcount
get_varcount <- function(text_line) {


  # exemplo "[^{^}]{\\d*:NUMERICAL:=(?<inside>[^}]*)}"
  # (?:regex) digere mas ignora a regex
  parsed <- regexpr("^\\s*VARCOUNT\\s*(?:<-|=)\\s*(?<VC>\\d+)",
                    text_line, perl = TRUE,
                    ignore.case = FALSE)

  #print(parsed)

  if (attr(parsed, "match.length") > 0) {
    cs <- attr(parsed, "capture.start")
    cl <- attr(parsed, "capture.length")

    #maybe it's not needed
    tryCatch(
      varcount <- as.numeric(substring(text_line, cs, cs + cl - 1))
    )

    if (is.na(varcount)) {
      varcount <- 0
    }

  } else {
    varcount <- 0
  }

  return(varcount)
}



#' Increases `curr_lineno` but
#' it `cat()` if debug is activated.
#'
#' @param curr_lineno
#' @param all_lines
#'
#' @return integer
next_line <- function(curr_lineno, all_lines) {

  curr_lineno <- curr_lineno + 1

  if (DBG_rmdexam) {
    cat("next line", curr_lineno, "is:", all_lines[curr_lineno], "\n")
  }
  return(curr_lineno)

}




#' To avoid search a pattern ^# Código or ^# enredo
#' etc, this function skips until a chunk ends.
#'
#' @return integer (line number after chunk ends)
skip_chunk <- function(curr_lineno, all_lines, rmdfilename) {

  nlines <- length(all_lines)

  if (grepl("^```", all_lines[curr_lineno])) {
    if (DBG_rmdexam) {
      cat("chunk starts in line ", curr_lineno, "\n")
    }
    curr_lineno <- curr_lineno + 1
    while (curr_lineno <= nlines && !grepl("^```", all_lines[curr_lineno])) {
      curr_lineno <- next_line(curr_lineno, all_lines)
    }
    if (curr_lineno > nlines) {
      stop(paste0("There is a Rmarkdown chunk ```{r} ... ``` that seems does not has an end in file ",
                  rmdfilename))
    } else {
      curr_lineno <- next_line(curr_lineno, all_lines)
    }
    if (DBG_rmdexam) {
      cat("chunk end in line ", curr_lineno - 1, "\n")
    }

  } else {
    curr_lineno <- next_line(curr_lineno, all_lines)
  }


  return(curr_lineno)
}




#' Parser depends on states. This
#' function prints the next state if
#' it changes.
#'
#' @param to
#' @param debug_str
#'
#' @return integer (a state is an integer)
change_state <- function(to, debug_str) {

  if (DBG_rmdexam) {
    cat("   change parser state to ", debug_str, "\n")
  }

  return(to)
}



#' Get exercise type (cloze, essay, multichoice).
#' It is not working.
#' TODO: implement ex type.
#'
#' @param curr_line
#'
#' @return character
get_ex_type <- function(curr_line) {
  #TODO: improve
  #line with #enredo can have a type:
  # enredo <empty>
  # enredo cloze
  # enredo essay
  # etc
  ty <- trimws(substr(curr_line, start = 8, stop = nchar(curr_line))) #enredo
  if (length(ty) == 0) {
    ty <- "cloze"
  }
  return(ty)
}




#' Parse an Rmd file containing an exercise
#'
#' `parse_exrmdfile`
#'
#'
#' @param rmdfilename
#'
#' @return list(ex_title, ex_type, code, enredo, alineas, alineas_title)
#'
#' @examples
parse_exrmdfile <- function(rmdfilename) { # nolint: cyclocomp_linter.


  # inside no code yet
  S_BEFORECODE <- 0

  # after line with # código has been recognized
  S_CODE <- 20

  # line with # enredo has been recognized
  S_ENREDO <- 30

  # line with # alínea has been recognized
  S_ALINEAS <- 40

  # Loop through each line of the Rmd file
  # Read the Rmd file into all_lines


  # tryCatch({
  #   fileConn <- file(rmdfilename, encoding = "UTF-8")
  #   all_lines <- readLines(fileConn, ok = FALSE, warn = TRUE, skipNul = TRUE)
  # },
  #
  # error = function(e) {
  #   cat("Check" ,rmdfilename, "\n")
  #   stop(e)
  #   return(e)
  # },
  #
  # finally = {
  #   close(fileConn)
  # })

  all_lines <- readLines(rmdfilename, encoding = "UTF-8", warn=FALSE)



  nlines <- length(all_lines)
  curr_state <- change_state(S_BEFORECODE, "S_BEFORECODE")
  curr_lineno <- next_line(0, all_lines)


  #variáveis resultantes do parse
  #varcount <- FALSE
  line_start_code <- NULL
  line_end_code <- NULL
  #ex_type <- NULL #not yet used
  ex_title <- NULL
  line_start_enredo <- NULL
  line_end_enredo <- NULL
  list_of_alineas <- list()
  list_of_alineas_title <- list()
  no_of_alineas <- 0

  varcount <- 0

  #Before parsing an file with question and items
  #a serach for VARCOUNT <- 6 is done.
  for (lineno in seq_len(nlines)) {

    #debug
    #cat(paste0(all_lines[curr_lineno]," : ", varcount,"\n"))

    varcount <- get_varcount(all_lines[lineno])
    if (varcount) break

  }

  if (!varcount) {
    stop(paste0("Please add: VARCOUNT <- 6, or other number of variants, to the '# code' section in file ",
                rmdfilename,
                "\n"))
  }





  #This "while()" it's a state machine whose states are:
  # S_BEFORECODE, S_CODE, S_ENREDO, S_ALINEAS
  while (curr_lineno <= nlines) {

    curr_line <- all_lines[curr_lineno]

    #debug
    #cat("curr_lineno=", curr_lineno, "with state", curr_state, "\n")
    #cat(">>>", class(curr_line), "\n")
    #cat(">>>", grepl("código", curr_line), "\n")


    #if (DBG_rmdexam) {
    #  browser()
    #}


    #to avoid infinite loops
    protect_counter <- curr_lineno


    if (curr_state == S_BEFORECODE) {

      if (grepl("^title:", curr_line)) {
        if (DBG_rmdexam) {
          cat("DBG_rmdexam: entou na linha 'title' do ficheiro *.Rmd\n")
        }
        ex_title <- substr(curr_line, start = 8, stop = nchar(curr_line))
        ex_title <- gsub('"', ' ', ex_title) # nolint
        ex_title <- trimws(ex_title)
        curr_lineno <- next_line(curr_lineno, all_lines)
      } else if (grepl("^# ", curr_line) ) {
                 # &&
                 # ( grepl("código", tolower(curr_line)) || grepl("codigo", tolower(curr_line)) ) ) { # nolint
        curr_state <- change_state(S_CODE, "S_CODE")
        curr_lineno <- next_line(curr_lineno, all_lines)
        line_start_code <- curr_lineno
      } else {
        curr_lineno <- next_line(curr_lineno, all_lines)
      }

    } else if (curr_state == S_CODE) {

      #if (grepl("VARCOUNT",curr_line)) {
      #
      #  varcount <- get_varcount(curr_line)
      #  curr_lineno <- next_line(curr_lineno,all_lines)
      #
      #} else

      if (grepl("^# ", curr_line) ) {
            # && #line # enredo
            # grepl("enredo", tolower(curr_line))) {

        # ex_type <- get_ex_type(curr_line) #not used

        line_end_code <- curr_lineno - 1

        curr_state  <- change_state(S_ENREDO, "S_ENREDO")
        curr_lineno <- next_line(curr_lineno, all_lines)
        line_start_enredo <- curr_lineno

        #if (!varcount) {
        #  #improve this message
        #  #paste() is used to break into smaller all_lines
        #  stop(paste0("Missing chunk with 'VARCOUNT = 6' ",
        #              "or any other value."))
        #}


      } else {

        curr_lineno <- skip_chunk(curr_lineno, all_lines, rmdfilename)

      }

    } else if (curr_state == S_ENREDO) {

      if (grepl("^# ", curr_line)) {

        line_end_enredo <- curr_lineno - 1

        curr_state  <- change_state(S_ALINEAS, "S_ALINEAS")

        curr_lineno <- next_line(curr_lineno, all_lines)
        line_start_alinea <- curr_lineno
        #debug

        #store the next alinea title
        list_of_alineas_title[[no_of_alineas + 1]] <- curr_line


      } else {

        curr_lineno <- skip_chunk(curr_lineno, all_lines, rmdfilename)

      }


    } else if (curr_state == S_ALINEAS) {

      if (grepl("^# ", curr_line)) {

        line_end_alinea <- curr_lineno - 1

        #Debug
        if (!exists("line_start_alinea")) {
          cat("curr_state é S_ALINEAS. Linha atual é:", curr_lineno, "\n")
          stop()
        }

        #store previous alínea
        no_of_alineas <- no_of_alineas + 1
        list_of_alineas[[no_of_alineas]] <-
          paste(all_lines[line_start_alinea:line_end_alinea],
                collapse = "\n")

        #store the next alinea title
        list_of_alineas_title[[no_of_alineas + 1]] <- curr_line


        curr_lineno <- next_line(curr_lineno, all_lines)

        line_start_alinea <- curr_lineno


      } else {

        curr_lineno <- skip_chunk(curr_lineno, all_lines, rmdfilename)

      }


    } else {
      #TODO: melhorar este erro
      stop("curr_state is unknown.")
    }

    if (DBG_rmdexam &&   protect_counter == curr_lineno) {
      stop("probably, infinite loop")
    }


  } #end while

  #Debug
  #if (!exists("line_start_alinea")) {
  #  print("Já fora do ciclo principal. Linha atual é:", curr_lineno, "\n")
  #  browser()
  #}

  if (line_start_alinea <= nlines) {
    #store alínea
    no_of_alineas <- no_of_alineas + 1
    # See https://github.com/jpcaveiro/rmdmoodle/issues/17
    # TODO: maybe add "\n\n" adding newline in case the user don't press "enter" after last line.
    list_of_alineas[[no_of_alineas]] <- paste(all_lines[line_start_alinea:nlines], collapse = "\n")
  }

  return(list(title = ex_title,
              varcount = varcount,
              type = "cloze", #TODO: e se o autor quiser um ESSAY sem alíneas?
              code    = paste0(all_lines[line_start_code:line_end_code], collapse = "\n"),
              enredo  = paste0(all_lines[line_start_enredo:line_end_enredo], collapse = "\n"),
              alineas = list_of_alineas,
              alineas_title = list_of_alineas_title))

}



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
find_alinea <- function(alinea_tag, ex_struct) {

  #if (DBG_rmdexam) {
  #  print("exercise structure:")
  #  print(ex_struct)
  #}

  nalineas <- length(ex_struct$alineas)
  i <- 1
  found <- FALSE
  while (!found && i <= nalineas) {
    if (grepl(alinea_tag, ex_struct$alineas_title[[i]])) {
      found <- TRUE
    } else {
      i <- i + 1
    }
  }
  if (!found) {
    stop(sprintf("`alinea_tag = %s` cannot be found in exercise %s", alinea_tag, ex_struct$title))
  }
  return(ex_struct$alineas[[i]])
}







#' exer2rmdstring
#'
#' @param ... ex. "cap3/c3-estimativa.Rmd", "alinea01", "alinea04"
#'
#' @return string containing Rmd with # and ## sections (exercise title and variants)
#'
#' @examples
#'    exerc2rmdstring(6, "cap3/c3-estimativa.Rmd", "alinea01", "alinea04")
exer2rmdstring <- function(...) {

  #o argumento nvar seria o autor a pedir mas foi removido

  #list(ex_title = ex_title,
  #     ex_type = "cloze",
  #     code = all_lines[line_start_code:line_end_code],
  #     enredo = all_lines[line_start_enredo:line_end_enredo],
  #     alineas = list_of_alineas))


  exrequest <- list(...)[[1]]

  if (grepl(".Rmd", exrequest[[1]])) {
    rmdfilename <- exrequest[[1]]
  } else {
    rmdfilename <- paste0(exrequest[[1]], ".Rmd", collapse = "")
  }

  #ex is a list:
  # list(title = ex_title,
  #            varcount = varcount,
  #            type = "cloze", #TODO: e se o autor quiser um ESSAY sem alíneas?
  #            code    = paste0(all_lines[line_start_code:line_end_code], collapse="\n"),
  #            enredo  = paste0(all_lines[line_start_enredo:line_end_enredo], collapse="\n"),
  #            alineas = list_of_alineas,
  #            alineas_title = list_of_alineas_title))

  ex_path <- file.path(pkg_env$EXERCISE_ROOT, rmdfilename)

  #debug
  #print(ex_path)
  ex <- parse_exrmdfile(ex_path)


  # output string containing Rmd (headerless) to be joint with other exercises
  #DEBUG
  ex_rmdtext <- ""

  # A # section is an exercise in Rmd.
  # antes: ex_header  <- paste0("# ", ex$title, "-", toupper(ex$type), collapse = " ")
  #now:
  ex_header  <- paste0("# ", slugify(ex$title), "-", toupper(ex$type), collapse = " ")
  ex_rmdtext <- paste0(ex_rmdtext, ex_header, collapse = "\n\n")

  #code
  ex_rmdtext <- paste0(ex_rmdtext, ex$code, collapse = "\n\n")

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
  #al_total <- length(ex$alineas) #not used
  al_string <- ""

  for (al in seq_len(al_request_number)) {

    al_rmdtext <- find_alinea(
                    alinea_tag = exrequest[[al + 1]],
                    ex_struct = ex)

    # Generate variant: enredo (1) blá blá \n\n (2) blá blá \n\n etc
    al_string <- paste0(al_string,
                        paste0("\n\n**(", as.character(al), ")**\n\n", al_rmdtext),
                        collapse = "\n\n")

  }


  al_string <- paste0(al_string,
                      "\n\n### feedback\n\n\n",
                      collapse = "\n\n")

  #O número de variantes é lido
  #dentro de cada exercício numa
  #linha: VARCOUNT <- 6 ou VARCOUNT = 6
  for (v in seq_len(ex$varcount)) {

    #section ## Variante `r VAR<- v;VAR`

    #debug
    #cat( sprintf("for (v in seq_len(ex$varcount)): %d, %d",v,ex$varcount))

    #sec_text <- sprintf("\n## variante `r VAR <- %d; VAR`\n\n", v)
    sec_text <- sprintf("\n## variante `r (VAR <- %d)`\n\n", v)

    #generate variants
    ex_rmdtext <- paste0(ex_rmdtext,
                         sec_text,
                         ex$enredo,
                         al_string,
                         collapse = "\n\n")

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
#' @param rmdfilename filename to store the "rmd exam"
#' @param ... each argument is a `c()` of strings;
#'
#' @return A file written in name given in `rmdfilename`
#' @export
#'
#' @examples
#' make_rmdmoodle("my-new-moodle-exam.Rmd",
#'                c("cap1/c1-estimation.Rmd","est-mean","est-var"),
#'                c("cap2/c2-normalprob.Rmd","prob-less", "prob-greater", "prob-between"))
rmdexam <- function(rmdfilename, ...) {


  # TODO: maybe remove after all testing on new exams
  if (is.numeric(rmdfilename)) {
    #it will print an integer (before april/2024 it uses a number in 1st arg)
    stop(paste0("Please remove first argument ",
                rmdfilename, #is an integer (see above)
                " from instruction 'rmdexam(", rmdfilename, "...)'.\n  Use VARCOUNT <- ",
                rmdfilename, " in code.\n"))
  }

  cat("\nBuilding", rmdfilename, "\n\n")

  #args is a list of lists
  #args[[1]][[1]] access the first exercise-filename to be written
  #to the new Rmd
  argslist <- list(...)

  ex_rmdtext <- ""

  # Runs all requested questions
  for (qnum in seq_along(argslist)) {
    res <- exer2rmdstring(argslist[[qnum]])
    ex_rmdtext <- paste0(ex_rmdtext,
                         res,
                         collapse = "\n\n")
  }


  #Comando com nvariants
  #   comando_txt <- sprintf("rmdexam(%d,\n\"%s\",\n%s)\n",
  #                          nvariants,
  #                          rmdfilename,
  #                          paste( argslist, collapse=", \n" ))
  comando_txt <- sprintf("rmdexam(\"%s\",\n%s)\n\n\n", #precisa pelo menos 2 \n
                         rmdfilename,
                         paste(argslist, collapse = ", \n"))


  head_txt <- paste0(
    "---\n",
    "title: \"", rmdfilename, "\"\n",
    "author: \"User '", Sys.info()["user"], "' compilou este exame\"\n",
    "date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\"\n",
    "output: html_document\n",
    "---\n\n\n",
    "A versão inicial deste documento foi construída com:\n",
    "```\n",
    "library(rmdmoodle)\n",
    "set_exercise_root(\"", pkg_env$EXERCISE_ROOT, "\")\n",
    #"set_exercise_root( \"C:/Users/USERNAME/<Where is the exercise folder?>\" )\n",
    comando_txt,
    "```\n",
    collapse = "\n\n")

    # "**Configuração do que se vê no HTML de verificação**\n",
    # "\n",
    # "\n",
    # "```{r echo=TRUE, results=FALSE}\n",
    # "VARCOUNT    = ", nvariants, "  # total de variantes\n",
    # "SHOWCODE    = FALSE  # Se mostra o código R (no HTML de verificação)\n",
    # "SHOWRESULTS = FALSE  # Se mostra o output do R (no HTML de verificação)\n",
    # "SET.SEED    = ", round(runif(1,1000,3000),0), " # set.seed(SET.SEED)\n",
    # "#Avoid scientific notation in all document\n",
    # "options(scipen = 999)\n",
    # "#Na commandline: str(knitr::opts_chunk$get())\n",
    # "knitr::opts_chunk$set(echo = TRUE) \n",
    # "```\n",
    # "\n\n",
    # collapse = "\n\n")

  #debug
  #cat(head_txt)


  ex_rmdtext <- paste0(head_txt,
                       ex_rmdtext,
                       collapse = "\n\n")



  # Open and write
  con <- file(rmdfilename, "wt", encoding = "UTF-8")
  #cat(ex_rmdtext, file = con)

  #Debug
  #cat("class(ex_rmdtext)=", class(ex_rmdtext), "\n")

  writeLines(ex_rmdtext, con = con) #, useBytes = TRUE)
  close(con)


  cat("Check file", rmdfilename, "and search for eventual problems or change for needs.\n")
  cat(paste0("Then, run xmlmoodle(\"", rmdfilename, "\") to produce the xml file to be exported.\n\n\n"))

  rmdfilename_no_ext <- substr(rmdfilename, 1, nchar(rmdfilename) - 4)

  xmlmoodle(rmdfilename_no_ext)

  cat("\nPlease, import file ", rmdfilename_no_ext, ".xml to moodle.\n\n", sep = "")

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

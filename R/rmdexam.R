
# global variables ====

#' TODO: Isto não funciona quando colocado no ficheiro .lintr:
#' `indentation_linter(indent = 2L, hanging_indent_style = c("tidy", "always", "never"), assignment_as_infix = TRUE)`



#' Ativa alguns `cat()` espalhados pelo código que
#' processa os exercícios. Este módulo faz parse
#' de texto: ativando esta flag passam a ser vistos
#' muitos `cat()` no processo de debug.
DBG_rmdexam <- F


#library(yaml) #use yaml::as.yaml

# functions ====



#' Extract the last word using substr
#'
#' @return char
get_lastword <- function(my_string) {

  #debug
  #print(my_string)

  ssplit = strsplit(my_string, " ")[[1]]

  # Only keep non empty words
  ssplit_new <- c()
  for (j in seq_along(ssplit)) {
    if (nchar(ssplit[j])>0) {
      ssplit_new <- c(ssplit_new, ssplit[j])
    }
  }

  # Reverse
  rev_ssplit_new <- rev(ssplit_new)

  # Extract the last word using substr
  last_word <- rev_ssplit_new[1]

  #debug
  #print(last_word)

  return(last_word)
}



#' Get VARCOUNT <- 6 or VARCOUNT = 6.
#' Search in the line for that pattern.
#'
#' @return numeric>0 or 0 if no varcount
varcount_line <- function(text_line) {


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



#' Search for a pattern like "VARCOUNT <- 6"
#' in all lines (already read from rmdfile)
#'
#' @param all_lines - vector of lines
#' @param rmdfilename - just to inform user of the filename
#'
#' @return integer (varcount value)
get_varcount <- function(all_lines, rmdfilename) {

  varcount <- 0

  #Before parsing an file with question and items
  #a serach for a pattern like "VARCOUNT <- 6"
  for (lineno in seq_along(all_lines)) {

    #debug
    #cat(paste0(all_lines[curr_lineno]," : ", varcount,"\n"))

    varcount <- varcount_line(all_lines[lineno])
    if (varcount) break

  }

  if (!varcount) {
    stop(paste0("If using `pq()`, add VARCOUNT <- 6 (or other number of variants) to the '# code' section in file:\n",
                rmdfilename,"\nOtherwise use `rq(number=6,...)` (or other number of variants) when building the exam.\n"))
  }

  return(varcount)
}



#' Increases `curr_lineno`
#'
#' But it `cat()` if debug is activated.
#'
#' @param curr_lineno current line number
#' @param all_lines vector of lines of text
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




#' Parser depends on states.
#'
#' This function prints the next state if it changes.
#'
#' @param to next state number
#' @param debug_str textual name of the next state number
#'
#' @return integer (a state is an integer)
change_state <- function(to, debug_str) {

  if (DBG_rmdexam) {
    cat("   change parser state to ", debug_str, "\n")
  }

  return(to)
}



#' Get exercise type (cloze, essay, multichoice).
#'
#' It is not working.
#' TODO: implement ex type.
#'
#' @param curr_line current line
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
#' `varcount` is the number of variants of a question
#' from which a random one is selected to be presented to student.
#'
#' `parse_exrmdfile`
#'
#'
#' @param rmdfilename R Markdown file name.
#' @param number minimum 1
#' @param varcount if null it extracts varcount from file in rmdfilename
#'
#' @return list(ex_title, ex_type, code, enredo, alineas, alineas_title)
#'
#' @examples
parse_exrmdfile <- function(ex_path,
                            rmdfilename,
                            ex_params=NULL, #for moodle title
                            varcount = NULL, #WHAT?
                            number = 1) { #usado para o return de ex$number

  if (is.null(ex_params)) {

    # Read params of the rmd file.
    # if they exist there.
    ex_yaml <- rmarkdown::yaml_front_matter(ex_path)
    ex_params <- ex_yaml$params
  }


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


  #Read all lines from path/some_question.Rmd.
  all_lines <- readLines(ex_path, encoding = "UTF-8", warn=FALSE)



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


  #' varcount is the number of
  #' repetitions of a question
  if (is.null(varcount)) {
    varcount <- get_varcount(all_lines, rmdfilename)
    if (is.null(number)) {
      number <- varcount
    }
  }



  inside_author <- NULL


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

    # https://www.statology.org/r-date-format/

    if (curr_state == S_BEFORECODE) {

      if (grepl("^author:", curr_line)) {
        if (DBG_rmdexam) {
          cat("DBG_rmdexam: entou na linha 'title' do ficheiro *.Rmd\n")
        }
        inside_author <- substr(curr_line, start = 8, stop = nchar(curr_line))
        inside_author <- gsub('"', ' ', inside_author)
        inside_author <- trimws(inside_author)


        curr_lineno <- next_line(curr_lineno, all_lines)
      } else if (grepl("^# ", curr_line) ) {
                 # &&
                 # ( grepl("código", tolower(curr_line)) || grepl("codigo", tolower(curr_line)) ) ) { # nolint


        # With information from "author:" line, if exists,
        # it will built the "moodle random question" title:
        if (is.null(inside_author)) {
          inside_author <- "author"
          cat(paste0("In Rmd file header put an author line: 'author: ", '"..."', "'.\n"))
        }
        if (is.null(ex_params)) {
          ex_title <- paste0(rmdfilename, ", ",
                             inside_author, ", ",
                             paste0(format(Sys.Date(),"%y-%m-%d"), " ",
                                    format(Sys.time(),"%H:%M"))
          )
        } else {
          ex_title <- paste0(rmdfilename, " ",
                             paste(names(ex_params), ex_params, collapse = ","), ", ",
                             inside_author, ", ",
                             paste0(format(Sys.Date(),"%y-%m-%d"), " ",
                                    format(Sys.time(),"%H:%M"))
          )
        }

        #debug
        #cat("ex_title: ", ex_title, "\n")



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
        #if (!exists("line_start_alinea")) {
        #  cat("DEBUG: curr_state is S_ALINEAS. Linha atual é:", curr_lineno, "\n")
        #  stop()
        #}

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
              varcount = varcount, #available number of variants
              number = number, #request number of variants
              type = "cloze", #TODO: e se o autor quiser um ESSAY sem alíneas?
              code    = paste0(all_lines[line_start_code:line_end_code], collapse = "\n"),
              enredo  = paste0(all_lines[line_start_enredo:line_end_enredo], collapse = "\n"),
              alineas = list_of_alineas,
              alineas_title = list_of_alineas_title,
              ex_params = ex_params,
              filename = rmdfilename))

}




#' Find "alinea" Rmd text by searching `alinea_tag`.
#'
#' * `ex_struct$alineas`is a list of Rmd strings;
#' * `ex_struct$alineas_title` is a list of titles (containing an `alinea_tag`).
#'
#' @param ex_struct see return value of `parse_exrmdfile()`
#' @param alinea_tag Each question file has items and `alinea_tag` names the item.
#'
#' @return string (rmd string)
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
    stop(sprintf("Item `%s` cannot be found in question %s.", alinea_tag, ex_struct$filename))
  }
  return(ex_struct$alineas[[i]])
}




#' `rq()` ("Random Question") Extracts items from a given question.
#'
#' Since it's a "random" question, code is replicated before each variant.
#' There is no need to use access `.[[VAR]]` or `.[VAR]` because there
#' is no pre calculated problems.
#'
#' Optional arguments are:
#'
#' - seed (default a random integer): sets seed
#' - varcount (default is 10): number of variants to be exported to moodle
#'
#' A seed can help an author to revise an exam after it is verified or delivered
#' and corrected later.
#'
#' @param ... optional arguments and vector like "cap3/c3-estimativa.Rmd", "alinea01", "alinea04"
#'
#' @return string containing Rmd with # and ## sections (exercise title and variants)
#' @export
#'
#' @examples
#' \dontrun{rq(seed = 90, number = 10, "cap3/c3-estimate.Rmd", "mean-item", "var-item")}
#' \dontrun{rq(number = 10, "cap3/c3-estimate.Rmd", "mean-item", "var-item")}
#' \dontrun{rq(seed = 90, "cap3/c3-estimate.Rmd", "mean-item", "var-item")}
#' \dontrun{rq("cap3/c3-estimate.Rmd", "mean-item", "var-item")}
rq <- function(...) {

  arglist <- list(...)

  # varcount
  if (is.null(arglist$number)) {
    number <- 10
  } else {
    #store
    number <- arglist$number

    #delete varcount
    arglist["number"] <- NULL
  }


  # seed
  if (is.null(arglist$seed)) {
    seed <- 10
  } else {
    #store
    seed <- arglist$seed

    #delete seed
    arglist["seed"] <- NULL
  }

  # params
  if (is.null(arglist$params)) {
    ex_params <- NULL
  } else {
    ex_params <- arglist$params

    #delete seed
    arglist["params"] <- NULL
  }

    #debug
  #cat("varcount is", varcount, "\n")

  #new name
  exrequest <- arglist

  rmdfilename <- add_extension(exrequest[[1]], "Rmd")


  question_path <- file.path(pkg_env$EXERCISE_ROOT, rmdfilename)
  #debug
  #print(question_path)

  #ex is a list:
  # list(title = ex_title,
  #            varcount = varcount,
  #            number = number,
  #            type = "cloze", #TODO: e se o autor quiser um ESSAY sem alíneas?
  #            code    = paste0(all_lines[line_start_code:line_end_code], collapse="\n"),
  #            enredo  = paste0(all_lines[line_start_enredo:line_end_enredo], collapse="\n"),
  #            alineas = list_of_alineas,
  #            alineas_title = list_of_alineas_title,
  #            params  )
  ex <- parse_exrmdfile(question_path,
                        rmdfilename = rmdfilename, #for title
                        ex_params=ex_params, #If ex_params is null it can be read from question_path, if exists there
                        varcount = number,
                        number = number)


  # output string containing Rmd (headerless) to be joint with other exercises
  #DEBUG
  ex_rmdtext <- ""

  # A # section is an exercise in Rmd.
  # antes: ex_header  <- paste0("# ", ex$title, "-", toupper(ex$type), collapse = " ")
  #now:
  #slugex_header  <- paste0("# ", slugify(ex$title), "-", toupper(ex$type), "\n\n")
  ex_header  <- paste0("# ", ex$title, "-", toupper(ex$type), "\n\n")
  ex_seed <- paste0("```{r}\nset.seed(", seed, ")\n```\n\n")
  ex_rmdtext <- paste0(ex_rmdtext, ex_header, ex_seed, "\n\n")

  #code (vai para entro de cada variante)
  #ex_rmdtext <- paste0(ex_rmdtext, ex$code, collapse = "\n\n")


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
                        paste0("\n\n**(", letters[al], ")**\n\n", al_rmdtext),
                        collapse = "\n\n")

  }


  al_string <- paste0(al_string,
                      "\n\n### feedback\n\n\n",
                      collapse = "\n\n")


  for (v in seq_len(ex$number)) {

    #section ## Variante `r VAR<- v;VAR`

    #debug
    #cat( sprintf("for (v in seq_len(ex$varcount)): %d, %d",v,ex$varcount))

    #sec_text <- sprintf("\n## variante `r VAR <- %d; VAR`\n\n", v)
    sec_text <- sprintf("\n## variante %02d\n\n", v)

    #generate variants
    ex_rmdtext <- paste0(ex_rmdtext,
                         sec_text,
                         ex$code, "\n\n",
                         ex$enredo,
                         al_string,
                         collapse = "\n\n")

  }


  return(list(rmdtext = ex_rmdtext, params = ex$ex_params))
}







#' `pq()` ("Planned Question") Extracts items from a given question. Since it's a planned
#' question, values inside its variants are accessible using `.[[VAR]]` or `.[VAR]`.
#'
#' @param ... vector like "cap3/c3-estimativa.Rmd", "alinea01", "alinea04"
#'
#' @return string containing Rmd with # and ## sections (exercise title and variants)
#' @export
#'
#' @examples
#' \dontrun{planned_q("cap3/c3-estimativa.Rmd", "alinea01", "alinea04")}
pq <- function(...) {

  #o argumento nvar seria o autor a pedir mas foi removido

  #list(ex_title = ex_title,
  #     ex_type = "cloze",
  #     code = all_lines[line_start_code:line_end_code],
  #     enredo = all_lines[line_start_enredo:line_end_enredo],
  #     alineas = list_of_alineas))

  arglist <- list(...)

  #TALVEZ NÃO SEJA PRECISO
  #if (length(arglist)==1) {
  #  # Happens when c("Question.Rmd", "Items1", "Item2") is
  #  # used instead of pq("Question.Rmd", "Items1", "Item2")
  #  # for convenience of the first rmdexam version
  #  arglist <- as.list(arglist[[1]])
  #}

  #Debug
  #cat("pq(...)\n")
  #print(arglist)


  # number
  if (is.null(arglist$number)) {
    number <- NULL
  } else {
    #store
    number <- arglist$number

    #delete number
    arglist["number"] <- NULL
  }


  # seed
  if (is.null(arglist$seed)) {
    seed <- NULL
  } else {
    seed <- arglist$seed

    #delete seed
    arglist["seed"] <- NULL
  }


  # params
  if (is.null(arglist$params)) {
    ex_params <- NULL
  } else {
    ex_params <- arglist$params

    #delete seed
    arglist["params"] <- NULL
  }


  # Arglist should be like:
  # list("question.Rmd", "itemX", "itemZ")
  # list("question", "itemX", "itemZ")
  exrequest <- arglist
  #rmdfilename_no_ext <- remove_extension(arglist[[1]][1], "Rmd")
  rmdfilename <- add_extension(arglist[[1]][1], "Rmd")

  ex_path <- file.path(pkg_env$EXERCISE_ROOT, rmdfilename)
  #debug
  #print(ex_path)


  #ex is a list:
  # list(title = ex_title,
  #            varcount = varcount,
  #            type = "cloze", #TODO: e se o autor quiser um ESSAY sem alíneas?
  #            code    = paste0(all_lines[line_start_code:line_end_code], collapse="\n"),
  #            enredo  = paste0(all_lines[line_start_enredo:line_end_enredo], collapse="\n"),
  #            alineas = list_of_alineas,
  #            alineas_title = list_of_alineas_title,
  #            params  )
  ex <- parse_exrmdfile(ex_path,
                        rmdfilename = rmdfilename, #for title
                        ex_params=ex_params, #If ex_params is null it can be read from question_path, if exists there
                        number = number) #number will be put on ex$number


  # output string containing Rmd (headerless) to be joint with other exercises
  #DEBUG
  ex_rmdtext <- ""


  # A # section is an exercise in Rmd.
  # antes: ex_header  <- paste0("# ", ex$title, "-", toupper(ex$type), collapse = " ")
  #now:
  #slug ex_header  <- paste0("# ", slugify(ex$title), "-", toupper(ex$type), collapse = " ")
  ex_header  <- paste0("# ", ex$title, "-", toupper(ex$type), collapse = " ")


  if (!is.null(seed)) {
    ex_seed <- paste0("```{r}\nset.seed(", seed, ")\n```\n\n")
  } else {
    ex_seed <- "\n"
  }

  # introduce ex_seed and ex_header
  ex_rmdtext <- paste0(ex_rmdtext, ex_seed, ex_header, collapse = "\n\n")

  # introduce code
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

  # Add all items if none is supplied.
  if (al_request_number==0) {

    #Get all items from the question
    for (i in seq_along(ex$alineas_title)) {
      items_id <- get_lastword(ex$alineas_title[[i]])
      exrequest <- c(exrequest, items_id)
    }

    al_request_number <- length(exrequest) - 1
  }

  #al_total <- length(ex$alineas) #not used
  al_string <- ""

  for (al in seq_len(al_request_number)) {

    al_rmdtext <- find_alinea(
                    alinea_tag = exrequest[[al + 1]],
                    ex_struct = ex)

    # Generate variant: enredo (1) blá blá \n\n (2) blá blá \n\n etc
    al_string <- paste0(al_string,
                        paste0("\n\n**(", letters[al], ")**\n\n", al_rmdtext),
                        collapse = "\n\n")

  }


  al_string <- paste0(al_string,
                      "\n\n### feedback\n\n\n",
                      collapse = "\n\n")

  #O número de variantes é lido
  #dentro de cada exercício numa
  #linha: VARCOUNT <- 6 ou VARCOUNT = 6

  for (v in seq_len(ex$number)) {

    #section ## Variante `r VAR<- v;VAR`

    #debug
    #cat( sprintf("for (v in seq_len(ex$varcount)): %d, %d",v,ex$varcount))

    #sec_text <- sprintf("\n## variante `r VAR <- %d; VAR`\n\n", v)

    if (ex$number <= ex$varcount) {
      # Se forem pedidas igual ou menos que as variantes
      VAR_is <- v  #v in 1 ... ex$number
    } else {
      # Se forem pedidas mais questões que as ex$varcount variantes
      VAR_is <- (v %% ex$varcount)  #v in 1 ... ex$number
      if (VAR_is == 0) {
        VAR_is <- ex$varcount
      }
    }

    sec_text <- sprintf("\n## variante `r (VAR <- %d)`\n\n", VAR_is)

    #generate variant `r (VAR <- VAR_is)`
    ex_rmdtext <- paste0(ex_rmdtext,
                         sec_text,
                         ex$enredo,
                         al_string,
                         collapse = "\n\n")

  }


  return(list(rmdtext = ex_rmdtext, params = ex$ex_params)) #TODO: vou aqui np pq()
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
#' \dontrun{rmdexam("my-new-moodle-exam.Rmd",
#'                pq("cap1/c1-estimation.Rmd", "est-mean", "est-var"),
#'                rq("cap2/c2-normalprob.Rmd", "prob-less", "prob-greater", "prob-between"))}
rmdexam <- function(rmdfilename, ...) {


  #' Remove 6 from rmdexam(6, "2022-2023-exame.Rmd",...)
  #' TODO: maybe remove after all testing on new exams.
  if (is.numeric(rmdfilename)) {
    #it will print an integer (before april/2024 it uses a number in 1st arg)
    stop(paste0("Please remove first argument ",
                rmdfilename, #is an integer (see above)
                " from instruction 'rmdexam(", rmdfilename, "...)'.\n  Use VARCOUNT <- ",
                rmdfilename, " in code.\n"))
  }


  cat("\nBuilding", rmdfilename, "\n\n")


  #args is a list
  #args[[1]][[1]] access the first exercise-filename to be written
  #to the new Rmd
  # ... is something like
  # list( list(rmdtext="...",params=...), ... )
  argslist <- list(...)

  ex_rmdtext <- ""

  all_params <- list()

  # Runs all requested questions
  for (qnum in seq_along(argslist)) {

    #' "arg" is like list(rmdtext="...", params=...)
    arg <- argslist[[qnum]]

    # Tells user to stop using c(...) notation
    # and use What is the argument type?
    if (class(arg) == "character") {
      stop(paste0("Do not use `c(...)`. Use pq(...) or rq(...), for [p]lanned [q]uestion or [r]andom [q]uestion types."))
    }

    # Concat all parameters to be used
    # in knitr::render
    if (!is.null(arg$params)) {
      all_params <- c(all_params, arg$params)
    }

    # Join every question into a big string
    ex_rmdtext <- paste0(ex_rmdtext,
                         arg["rmdtext"],
                         collapse = "\n\n")
  }


  if (length( unique( names(all_params) ) ) < length( all_params )) {
    print(all_params)
    stop("There are yaml params with same names in different R Markdown files.")
  }

  #' Reproduce the instruction that
  #' originates the exam for later
  #' reproducibility.
  make_exam_script <- sprintf("rmdexam(\"%s\",\n%s)\n", #precisa pelo menos 2 \n
                         rmdfilename,
                         paste(argslist, collapse = ", \n"))

  if (length(all_params)>0) {
    paramsstr <- yaml::as.yaml(list(params = all_params), indent = 4, indent.mapping.sequence = TRUE)

    head_txt <- paste0(
      "---\n",
      "title: \"", rmdfilename, "\"\n",
      "author: \"User '", Sys.info()["user"], "' compilou este exame\"\n",
      "date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\"\n",
      "output: html_document\n",
      paramsstr,
      "---\n\n\n")
  } else{
    head_txt <- paste0(
      "---\n",
      "title: \"", rmdfilename, "\"\n",
      "author: \"User '", Sys.info()["user"], "' compilou este exame\"\n",
      "date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\"\n",
      "output: html_document\n",
      "---\n\n\n")
  }

  #TODO: remove this commented lines
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


  #' Save exam in `rmdfilename`
  #' Open and write
  con <- file(rmdfilename, "wt", encoding = "UTF-8")
  #cat(ex_rmdtext, file = con)

  #Debug
  #cat("class(ex_rmdtext)=", class(ex_rmdtext), "\n")

  writeLines(ex_rmdtext, con = con) #, useBytes = TRUE)
  close(con)


  rmdfilename_no_ext <- substr(rmdfilename, 1, nchar(rmdfilename) - 4)

  xmlmoodle(rmdfilename_no_ext, params = NULL) #all_params)

  cat(paste0(
    "File \"", rmdfilename, "\" contains a Moodle exam. ",
    "Eventually, it can be reviewed and then with `xmlmoodle(\"", rmdfilename, "\")`",
    "a new Moodle XML file is produced to be imported.\n\n"
  ))

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

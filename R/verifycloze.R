# nolint


# library(stringdist) -afind



# algoritmo ====

# Notes:
# 1. module xmlmoodle.R calls verifycloze() defined in this file.
# 2. See test04-regex.R in WorkPackages/2020-rmdmoodle for experiences


#' 1. limpar 0D 0D 0A para espaço
#' 2.




# functions ====

#' Ideia:
#' Aplicar o filtro verificador de instruções cloze
#' apenas no XML antes que este siga para o Moodle
#' evitando os erros lacónicos do Moodle.


#' Two problems
#' 1. se a linha tem mais que um multichoice
#' 2. se uma mesma linha pode ter multichoice e numerical, etc
#'
#' One solution
#' scan, first, all multichoices, then all numerical, etc



AFIND_MAX_DISTANCE <- 4 # nolint
# TODO:
#
# número versus NUMERICAL com "AFIND_MAX_DISTANCE <- 6" dispara a situação
#


#' Before formal parse (with regex), it searches using `stringdist::afind`
#' for MULTICHOICE_S, NUMERICAL and SHORTANSWER patterns.
#' Matching is case-sensitive in `stringdist::afind`.
#'
#' @param uclozetext - an uppercase text
#'
#' @return FALSE or list(pattern="...pattern...", afind=afind_result)
maybe_pattern <- function(uclozetext) {

  # stringdist::afind returns
  # $ location: int [1, 1] 11
  # $ distance: num [1, 1] 4
  # $ match   : chr [1, 1] ":MULTICHOICE:fal"


  # MAYBE a :MULTICHOICE_S:
  afind_result <- stringdist::afind(toupper(uclozetext), ":MULTICHOICE_S:")
  if (afind_result$distance <= AFIND_MAX_DISTANCE &&
      grepl("[\\{\\}=~]", uclozetext)) { #TODO: tune this 6
    return(list(pattern = "[^{^}]{\\d*:MULTICHOICE_S:=(?<inside>[^}]*)}",
                afind = afind_result))
  }


  # MAYBE a :NUMERICAL:
  afind_result <- stringdist::afind(toupper(uclozetext), ":NUMERICAL:")
  if (afind_result$distance <= AFIND_MAX_DISTANCE &&
      grepl("[\\{\\}=~]", uclozetext)) { #TODO: tune this 6
    return(list(pattern = "[^{^}]{\\d*:NUMERICAL:=(?<inside>[^}]*)}",
                afind = afind_result))
  }



  #TODO: other patterns


  #found no pattern
  return(FALSE)
}




#' Formal verification of cloze instructions (using regex).
#'
#' @param uclozetext text with Moodle cloze instructions
#' @param resultofmaybe is list(pattern="...pattern...", afind=afind_result)
#'
#' @return list(type="...",  options = options, pos.s = pos.s,  pos.e = pos.e)
validate_pattern <- function(uclozetext, resultofmaybe) {

  # See test04-regex.R in "regexpr" section for a study of cases.
  # resultofmaybe$pattern could be:
  #   "[^}]{\\d*:MULTICHOICE_S:=(?<inside>[^}]*)}"

  parsed <- regexpr(resultofmaybe$pattern,
                    uclozetext, perl = TRUE,
                    ignore.case = TRUE)

  if (parsed == -1) {
    return(FALSE)
  }

  # > str(parsed)
  # int 9
  # - attr(*, "match.length")= int 43
  # - attr(*, "capture.start")= int [1, 1] 28
  # ..- attr(*, "dimnames")=List of 2
  # .. ..$ : NULL
  # .. ..$ : chr "inside"
  # - attr(*, "capture.length")= int [1, 1] 23
  # ..- attr(*, "dimnames")=List of 2
  # .. ..$ : NULL
  # .. ..$ : chr "inside"
  # - attr(*, "capture.names")= chr "inside"


  # get field "inside"
  ml <- attr(parsed, "match.length")
  cs <- attr(parsed, "capture.start")
  cl <- attr(parsed, "capture.length")

  options <- substring(uclozetext, cs, cs + cl - 1)

  #debug
  #cat("options=",options,"\n")


  # MULTICHOICE_S or NUMERICAL options situations
  if (grepl("<SUB>", options, ignore.case = TRUE)) {
    cat("    Use '\\~' instead of only '~' in RMarkdown file. An isolated ~, in Rmd, means italic and not a false answer.\n")
  }
  options_list <- strsplit(options, "~")

  # NUMERICAL options situations
  if (grepl("\\^", options, ignore.case = TRUE) && grepl("NUMERICAL", resultofmaybe$pattern, ignore.case = TRUE) ) {
    cat(paste0("    Error: moodle does not support curly braces in numbers like 10^{2}. Suggested use of `options(scipen = 999)` in code. See '", options, "' in XML moodle file.\n"))
  }

  # NUMERICAL options situations
  if (grepl(" ", options, ignore.case = TRUE) && grepl("NUMERICAL", resultofmaybe$pattern, ignore.case = TRUE) ) {
    cat(paste0("    Error: there is a space in a {:NUMERICAL:=...}.  See '", options, "' in XML moodle file.\n"))
  }


  attributes(parsed) <- NULL
  pos_s <- parsed + 1
  pos_e <- parsed + ml - 1


  if (grepl("MULTICHOICE_S", resultofmaybe$pattern, ignore.case = TRUE)) {
    type <- "MULTICHOICE_S"
  } else if (grepl("NUMERICAL", resultofmaybe$pattern, ignore.case = TRUE)) {
    type <- "NUMERICAL"
  } else if (grepl("SHORTANSWER", resultofmaybe$pattern, ignore.case = TRUE)) {
    type <- "SHORTANSWER"
  }

  return(list(type = type, options = options_list, pos_s = pos_s,  pos_e = pos_e))
}


#' Check for wrong CLOZE instructions.
#'
#' A ideia é fazer uma função `verifycloze(clozetext)` que
#' tente encontrar problemas em instruções cloze como
#' - falta de  _S
#' - falta de =
#' - resultado como 8e-4 em numerical
#' i.e., se parece que é uma instrução cloze
#' então avisa caso ela não esteja com o formato desejado.
#'
#' Depois do `knitr` é que se fica a saber
#' se existem situações de 8e-4, por exemplo.
#'
#' Outra situação é com "~" que pode ser convertido em itálico:
#' - procurar <em> dentro de multichoice ou numerical ou etc.
#'
#' Assim, esta rotina deve ser aplicada ao produto final em HTML.
#'
#' Como informar o autor do local do erro
#' passando apenas um trecho do ficheiro!
#'
#' Dificuldade:
#' - em html perde-se a noção do contexto Rmd !!!
#' Solução:
# - imprimir antes e depois para situar o autor.
#'
#' @param sometext
#'
#' @return List of problems in output.
#' @export
#'
#' @examples
#' verifycloze("some text {:NUMERICAL:=10:0} etc\n etc\n {15:MULTICHOICE_S:=A~B~C}\n")
#'
verifycloze <- function(clozetext, showprogress = FALSE) {

  # Algorithm:
  # text: "from {:multihcoice_s:...} and blá blá {:numerical:=...} until the end!"
  # step 1: from MULTICHOICE and blá blá {...} until the end!
  # step 2: from MULTICHOICE and blá blá NUMERICAL until the end!
  # step 3: no more moodle cloze curly instructions.


  #put upper case
  #uclozetext <- toupper(clozetext)
  uclozetext <- clozetext

  #pos <- 1
  #nn <- nchar(clozetext)
  must_try <- TRUE

  imax_debug <- 1

  while (must_try) {

    if (showprogress) {
      cat("uclozetext:\n", uclozetext, "\n")
    }


    resultofmaybe <- maybe_pattern(uclozetext)

    must_try <- FALSE

    # TODO - it does not work : if(resultofmaybe)
    #        when "resultofmaybe" is a list() or FALSE
    if (class(resultofmaybe) == "list") {

      # validade formaly and warn if errors
      resultofvalidate <- validate_pattern(uclozetext, resultofmaybe)
      must_try <- TRUE #must try again to find next instruction

      if (class(resultofvalidate) == "list") {

        #TODO: make the next cat() optional for user/author:

        # if (resultofvalidate$type=="MULTICHOICE_S") {
        #   cat("    Detected MULTICHOICE_S with options: ")
        #   cat(paste0(resultofvalidate$options[[1]],collate="; "),"\n",sep="")
        # } else if (resultofvalidate$type=="NUMERICAL") {
        #   #debug
        #   #cat("options=\n")
        #   #print(resultofvalidate$options)
        #   cat("    Detected NUMERICAL with options: ")
        #   cat(paste0(resultofvalidate$options[[1]],collate="; "),"\n",sep="")
        # } else if (resultofvalidate$type=="SHORTANSWER") {
        #   cat("    Detected SHORTANSWER with options", resultofvalidate$options[[1]], "\n")
        # }

        #build new uclozetext from "resultofvalidate"
        pos_s <- resultofvalidate$pos_s
        pos_e <- resultofvalidate$pos_e

        uclozetext <- paste0(substr(uclozetext, 1, pos_s - 1),
                             "REMOVED-CORRECTINSTRUCTION",
                             substr(uclozetext, pos_e + 1, nchar(uclozetext)))

      } else {

        #debug
        #print(str(resultofmaybe))


        #warn user/author
        #TODO: improve error message with solution
        cat(paste0("    Check eventual situation in CLOZE syntax: "))
        #cat(paste0("    ", resultofmaybe$afind$match, "\n"))
        start_pos <- resultofmaybe$afind$location
        stop_pos  <- min(resultofmaybe$afind$location + 50, nchar(uclozetext))
        cat(paste0("    ", substr(uclozetext, start_pos, stop_pos)))
        cat("\n")

        #build new uclozetext from "resultofmaybe"
        uclozetext <- gsub(stringr::str_escape(resultofmaybe$afind$match),
                           "REMOVED-POSSIBLEINSTRUCTION",
                           uclozetext,
                           ignore.case = TRUE)

      }

      #debug
      imax_debug <- imax_debug + 1
      if (imax_debug > 100) {
        stop("rmdmoodle::verifycloze() is not working! Contact developer.")
      }

    }
  } #end while(must_try)

  if (showprogress) {
    cat("uclozetext:\n", uclozetext, "\n")
  }


}

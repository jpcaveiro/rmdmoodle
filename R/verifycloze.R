

# Notes:
# - module xmlmoodle.R calls verifycloze() defined n this file.
# - see test04-regex.R in WorkPackages/2020-rmdmoodle for experiences


# variável local
DEBUG <- F






#library(stringdist) -afind



# algoritmo ====

# 1. limpar 0D 0D 0A para espaço
# 2.

# main ====

# Ideia:
# Aplicar o filtro verificador de instruções cloze
# apenas no XML antes que este siga para o Moodle
# evitando os erros lacónicos do Moodle.


# Two problems
# 1. se a linha tem mais que um multichoice
# 2. se uma mesma linha pode ter multichoice e numerical, etc
# One solution
# scan, first, all multichoices, then all numerical, etc




#' Before formal parse (with regex), it searches using `stringdist::afind`
#' for MULTICHOICE_S, NUMERICAL and SHORTANSWER patterns.
#' Matching is case-sensitive in `stringdist::afind`.
#'
#' @param uclozetext - an uppercase text
#'
#' @return FALSE or list(pattern="...pattern...", afind=afind.result)
maybe.a.pattern <- function(uclozetext) {

  # stringdist::afind returns
  # $ location: int [1, 1] 11
  # $ distance: num [1, 1] 4
  # $ match   : chr [1, 1] ":MULTICHOICE:fal"


  # MAYBE a :MULTICHOICE_S:
  afind.result <- stringdist::afind(toupper(uclozetext),":MULTICHOICE_S:")
  if (afind.result$distance<=6) { #TODO: tune this 6
    return(list(pattern="[^{^}]{\\d*:MULTICHOICE_S:=(?<inside>[^}]*)}", afind=afind.result))
  }


  # MAYBE a :NUMERICAL:
  afind.result <- stringdist::afind(toupper(uclozetext),":NUMERICAL:")
  if (afind.result$distance<=6) { #TODO: tune this 6
    return(list(pattern="[^{^}]{\\d*:NUMERICAL:=(?<inside>[^}]*)}", afind=afind.result))
  }



  #TODO: other patterns


  #found no pattern
  return( FALSE )
}




#' Formal verification of cloze instructions (using regex).
#'
#' @param uclozetext
#' @param resultofmaybe is list(pattern="...pattern...", afind=afind.result)
#'
#' @return list(type="...",  options = options, pos.s = pos.s,  pos.e = pos.e)
#'
validate.pattern <- function(uclozetext,resultofmaybe) {

  # See test04-regex.R in "regexpr" section for a study of cases.

  # resultofmaybe$pattern could be:
  #   "[^}]{\\d*:MULTICHOICE_S:=(?<inside>[^}]*)}"

  parsed <- regexpr(resultofmaybe$pattern, uclozetext, perl = T, ignore.case = T)

  if (parsed==-1) {
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



  ml <- attr(parsed, "match.length")
  cs <- attr(parsed, "capture.start")
  cl <- attr(parsed, "capture.length")

  options <- substring(uclozetext, cs, cs + cl - 1)

  #debug
  #cat("options=",options,"\n")

  if( grepl("<SUB>",options, ignore.case=T) ) {
    cat("    Error: use '\\~' instead of only '~' in RMarkdown file to avoid produce '<sub>' tag.\n")
  }
  options <- strsplit(options, "~")

  attributes(parsed) <- NULL
  pos.s <- parsed+1
  pos.e <- parsed+ml-1


  if ( grepl("MULTICHOICE_S",resultofmaybe$pattern, ignore.case = T) ) {
    type <- "MULTICHOICE_S"
  } else if ( grepl("NUMERICAL",resultofmaybe$pattern, ignore.case = T) ) {
    type <- "NUMERICAL"
  } else if ( grepl("SHORTANSWER",resultofmaybe$pattern, ignore.case = T) ) {
    type <- "SHORTANSWER"
  }

  return( list(type = type, options = options, pos.s = pos.s,  pos.e = pos.e) )
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
verifycloze <- function(clozetext,showprogress=F) {

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
  must.try = T

  imax_debug <- 1

  while(must.try) {

    if (showprogress) {
      cat("uclozetext:\n",uclozetext,"\n")
    }


    resultofmaybe <- maybe.a.pattern(uclozetext)

    must.try <- F

    # TODO - it does not work : if(resultofmaybe)
    #        when "resultofmaybe" is a list() or FALSE
    if (class(resultofmaybe)=="list") {

      # validade formaly and warn if errors
      resultofvalidate <- validate.pattern(uclozetext,resultofmaybe)
      must.try <- T #must try again to find next instruction

      if (class(resultofvalidate)=="list") {

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
        pos.s <- resultofvalidate$pos.s
        pos.e <- resultofvalidate$pos.e

        uclozetext <- paste0(
                substr(uclozetext, 1, pos.s - 1),
                "REMOVED-CORRECTINSTRUCTION",
                substr(uclozetext, pos.e + 1, nchar(uclozetext)))

      } else {

        #debug
        #print(str(resultofmaybe))


        #warn user/author
        #TODO: improve error message with solution
        cat("    Error, please check", resultofmaybe$afind$match,"\n")

        #build new uclozetext from "resultofmaybe"
        uclozetext <- gsub(resultofmaybe$afind$match, "REMOVED-POSSIBLEINSTRUCTION", uclozetext, ignore.case = T)

      }

      #debug
      imax_debug <- imax_debug + 1
      if (imax_debug>100) {
        stop("rmdmoodle::verifycloze() is not working! Contact developer.")
      }

    }
  } #end while(must.try)

  if (showprogress) {
    cat("uclozetext:\n",uclozetext,"\n")
  }


}




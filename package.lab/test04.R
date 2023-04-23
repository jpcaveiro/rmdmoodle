
# test04.R  ====

stop("Don't source this file.")

# * alt+O
# * alt+shift+O

# Instructions:
#   browser() to enter debug
# 

# dell
setwd("~/GitHub/rmdmoodle/package.lab") 

# bigi7
setwd("~/Documents/GitHub/rmdmoodle/package.lab") 


# Ubuntu 22 ====

# Primeiro instalar o R como indica na página do R
# sudo apt-get install r-base-dev
# sudo apt install libxml2-dev
# Bash
#  sudo apt install make
#  sudo apt install gcc
#  sudo apt install g++


# experiências ====


# https://stackoverflow.com/questions/46862384/how-do-i-use-try-statements-in-r

# library(reticulate)
# tryCatch({
#   reticulate::repl_python()
# },
# error = function(e) {
#   #what should be done in case of exeption?
#   str(e) # prints structure of exeption
# }
# )


# Open and write
con <- file("Ad_GeradorTeste1turno1mar2023_Bio.Rmd", "r", encoding = "UTF-8")
lines <- readLines(con)
close(con)


#regexpr and gregexpr support ‘named capture’.
# If groups are named, e.g., "(?<first>[A-Z][a-z]+)" then the positions of
# the matches are also returned by name. (Named backreferences are not supported by sub.)
#https://search.r-project.org/R/refmans/base/html/regex.html

#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase {1:MULTICHOICE_S:=falsa ~%-25%verdadeira }: A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."

#goal: name.rex <- "{{(?<pontos>[[:digit:]]*):MULTICHOICE_S:=(?<opcoes>[[^}]]*)}}"
name.rex <- "[^}]{\\d*:MULTICHOICE_S:=(?<inside>[^}]*)}"
parsed <- regexpr(name.rex, txt, perl = T, ignore.case = F)
parsed
st <- attr(parsed, "capture.start")
opcoes <- substring(txt, st, st + attr(parsed, "capture.length") - 1)
opcoes
strsplit(opcoes, "~")



# stringdist tests ====

#stringdist
#  tem que se usar toupper() em cima do texto a ser procurado!
install.packages("stringdist")
library(stringdist)

#amatch é comparação de duas strings iguais ou quase iguais
amatch("MULTICHOICE",txt, method="soundex")

#texto 1
#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase {1:MULTICHOICE_S:=falsa ~%-25%verdadeira }: A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."
afind(txt,":MULTICHOICE_S:")

#texto 1.1
#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase {1:multichoice_s:=falsa ~%-25%verdadeira }: A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."
afind(txt,":MULTICHOICE_S:") #não existe ignore.case=T
# $distance é 12  !!!

#texto 1.2
#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase {1:multichoice_s:=falsa ~%-25%verdadeira }: A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."
afind(toupper(txt),":MULTICHOICE_S:")
# $distance é 0


#texto 2
#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase {1:MULTICHOICE_S:=falsa ~%-25%verdadeira }: A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."
afind(txt,":MULTICHOICE:") #SEM "_S"
# $distance é 1




#texto 3
#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase {1:MULTICHOICE:falsa ~%-25%verdadeira }: A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."
afind(txt,":MULTICHOICE_S:=") #SEM "_S"
# $distance é 4




#texto 4
#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase {1:MULTIPLECHOICE_S:=falsa ~%-25%verdadeira }: A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."
afind(txt,":MULTICHOICE_S:") #SEM "_S"
# $distance é 6 em  15 characters




#texto 5
#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase {1:MULTIPLECHOICE_S:=falsa ~%-25%verdadeira }: A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."
afind(txt,"SOPASOPA") #SEM "_S"
# $distance é 7 em 8 letras de SOPASOPA



#texto 5
#      0         1         2         3         4         5
#      0123456789012345678901234567890123456789012345678901
txt <- "* Frase monorevisão do texto A mediana das idades dos golfinhos que deram à costa é necessariamente maior a 15 anos."
afind(toupper(txt),":MULTICHOICE_S:") #SEM "_S"
# $distance é 9 em 15 letras de :MULTICHOICE_S:
#a posição do match é 54 "" GOLFINHOS QUE ""


# Sugestão: talvez distance<=5 para assumir que é um comando do MOODLE.



# named capture
notables <- c("  Ben Franklin and Jefferson Davis",
              "\tMillard Fillmore")
# name groups 'first' and 'last'
name.rex <- "(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)"
(parsed <- regexpr(name.rex, notables, perl = TRUE))
gregexpr(name.rex, notables, perl = TRUE)
parse.one <- function(res, result) {
  m <- do.call(rbind,
               lapply(seq_along(res),
                      function(i) {
                        if(result[i] == -1) return("")
                        #st é start
                        st <- attr(result, "capture.start")[i, ]
                        substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
                      }
  ))
  colnames(m) <- attr(result, "capture.names")
  m
}
parse.one(notables, parsed)




# main ====


#' Search for a formal parse of multichoice
#' and give first and last position.
#'
#' @param txtline 
#'
#' @return
#' @export
#'
#' @examples
parse_multichoice <- function(txtline) {
  
}




#' Before formal parse (grep) it searches for
#' `afind` for MULTICHOICE.
#'
#' @param txtline 
#'
#' @return an integer with the position (if any, otherwise -1)
#' @export
#'
#' @examples
maybe_multichoice <- function(txtline) {
  #TODO: se a linha tem mais que um multichoice
  #todo: uma mesma linha pode ter multi e numerical etc
  
  return( grep("MULTI", txtline, ignore.case = T) )
}


#' Check for wrong CLOZE instructions.
#'
#' A ideia é fazer uma função `cloze.check(texto)` que
#' tente encontrar problemas em instruções cloze como
#' - falta de  _S
#' - falta de =
#' - resultado como 8e-4 em numerical
#' i.e., se parece que é uma instrição cloze
#' então avisa caso ela não esteja com o formato desejado.
#'
#' Depois de correr knitr é que se fica a saber
#' se existem situações de 8e-4, por exemplo.
#'
#' Outra situação é com "~" que pode ser convertido em itálico.
#'
#' Assim, esta rotina deve ser aplicada ao produto final em HTML.
#' Se for injetado Rmd então o knitr() deve ser chamado para
#' fazer o parse no HTML.
#' QUestão de como informar o autor do local do erro
#' passando apenas um trecho do ficheiro!
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
#' cloze.check("some text {:NUMERICAL:=10:0} etc\n etc\n {15:MULTICHOICE_S:=A~B~C}\n")
#'
moodle.cloze.check <- function(sometext) {
  pos <- 1
  nn <- nchar(sometext)
  while(pos <= nn) {
    # find possible multichoice_s tag
    mchoice_pos <- find_multichoice(substr(sometext,start=pos,stop=nn))
    if (mchoice_pos<=nn) {
      # validade formaly
      mchoice_end <- validate_next_multichoice(substr(sometext,start=pos,stop=nn))
      if (mchoice_end<=nn) {
        pos <- mchoice_end+1
      } else {
        cat("   Maybe a problem in a multichoice. See:\n")
        cat(substr(sometext,start=pos,stop=nn))
        cat("   ====end====")
      }
    }
    
    
  }
  
}

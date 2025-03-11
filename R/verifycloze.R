# nolint


# Libraries at DESCRIPTION
# library(stringdist): afind
# library(stringr): str_match
# Use functions like lib::name(...)



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

# parse_cloze ====




CLOZE_PATTERN = '\\{(\\d*):([^:]+):([^}]+)\\}'



#cloze_match <- str_match_all(cloze_text, CLOZE_PATTERN)[[1]]
#cloze_match <- str_match_all("cloze_text", CLOZE_PATTERN)[[1]]

get_cloze_type <- function(tag) {

  #is done before
  #tag <- toupper(tag)

  if (tag %in% c("SHORTANSWER", "SA",  "MW")) {

    # short answers (SHORTANSWER or SA or MW), case is unimportant,
    tag <- "SHORTANSWER"

  } else if (tag %in% c("SHORTANSWER_C", "SAC", "MWC")) {

    # short answers (SHORTANSWER_C or SAC or MWC), case must match,
    tag <- "SHORTANSWER_C"

  } else if (tag %in% c("NUMERICAL", "NM")) {

    # numerical answers (NUMERICAL or NM),
    tag <- "NUMERICAL"

  } else if (tag %in% c("MULTICHOICE", "MC")) {

    # multiple choice (MULTICHOICE or MC), represented as a dropdown menu in-line in the text,
    tag <- "MULTICHOICE"

  } else if (tag %in% c("MULTICHOICE_V", "MCV")) {

    # multiple choice (MULTICHOICE_V or MCV), represented as a vertical column of radio buttons, or
    tag <- "MULTICHOICE_V"

  } else if (tag %in% c("MULTICHOICE_H", "MCH")) {

    # multiple choice (MULTICHOICE_H or MCH), represented as a horizontal row of radio-buttons,
    tag <- "MULTICHOICE_H"

  } else if (tag %in% c("MULTIRESPONSE", "MR")) {

    # multiple choice (MULTIRESPONSE or MR), represented as a vertical row of checkboxes
    tag <- "MULTIRESPONSE"

  } else if (tag %in% c("MULTIRESPONSE_H", "MRH")) {

    # multiple choice (MULTIRESPONSE_H or MRH), represented as a horizontal row of checkboxes
    tag <- "MULTIRESPONSE_H"

  } else if (tag %in% c("MULTICHOICE_S", "MCS")) {

    # multiple choice (MULTICHOICE_S or MCS), represented as a dropdown menu in-line in the text,
    tag <- "MULTICHOICE_S"

  } else if (tag %in% c("MULTICHOICE_VS", "MCVS")) {

    # multiple choice (MULTICHOICE_VS or MCVS), represented as a vertical column of radio buttons, or
    tag <- "MULTICHOICE_VS"

  } else if (tag %in% c("MULTICHOICE_HS", "MCHS")) {

    # multiple choice (MULTICHOICE_HS or MCHS), represented as a horizontal row of radio-buttons.
    tag <- "MULTICHOICE_HS"

  } else if (tag %in% c("MULTIRESPONSE_S", "MRS")) {

    # multiple choice (MULTIRESPONSE_S or MRS), represented as a vertical row of checkboxes
    tag <- "MULTIRESPONSE_S"

  } else if (tag %in% c("MULTIRESPONSE_HS", "MRHS")) {

    # multiple choice (MULTIRESPONSE_HS or MRHS), represented as a horizontal row of checkboxes
    tag <- "MULTIRESPONSE_HS"

  } else {

    tag <- "no tag"

  }

  return(tag)
}



parse_one_option <- function(opt_text) {

  #answer <- ""
  #feedback <- ""


  #get percent valid
  if (substr(opt_text,1,1) == '=') {

    percent <- "100"
    opt_text <- substr(opt_text, 2, nchar(opt_text))

  } else if (substr(opt_text,1,1) == '%') {

    percent_match <- stringr::str_match(opt_text, "^%([-+]?\\d+)%(.+)")
    #percent_match <- percent_match[[1]]


    num_of_matches <- length(percent_match) #/ 4

    if (num_of_matches > 0) {
      percent  <- percent_match[[1,2]]
      opt_text <- percent_match[[1,3]]
    } else {
      percent <- "0"
    }
  } else {

    percent <- "0"

  }


  #Find feedback
  text_hash_feedback <- strsplit(opt_text, '#')[[1]]
  if (length(text_hash_feedback) == 1) {

    #No hastag
    answer <- text_hash_feedback
    feedback <- ""

  } else if (length(text_hash_feedback) == 2) {

    #answer and feedback
    answer   <- text_hash_feedback[1]
    feedback <- text_hash_feedback[2]

  } else {

    #probabl feedback contains hastag
    #TODO: possible error
    answer <- text_hash_feedback[1]
    feedback <- paste(text_hash_feedback[2:length(text_hash_feedback)], collapse = "#")
  }


  o = list("percent"=percent, "answer"=answer, "feedback"=feedback)

  return(o)
}




#' Parse one cloze instruction
#'
#' Instructions are like {:SHORTANSWER:=Paris#Correct!~*#Wrong answer}
#' and breaks them into its parts.
#'
#' @param cloze_instruction - instructions has here ...moodle cloze page....
#' @param output - see line: if (output=="cloze")
#'
#' @returns a list (cloze question parts)
#' @export
#'
#' @examples
#' \dontrun{cloze_instruction <- "{:SHORTANSWER:=Paris#Correct!~*#Wrong answer}"}
#' \dontrun{print(parse_one_cloze_instruction(cloze_instruction, output="html"))}
#' \dontrun{cloze_instruction <- "{1:MCV:%50%Jupiter#Partial Correct!~=Mars#Correct~Saturn#Wrong}"}
#' \dontrun{print(parse_one_cloze_instruction(cloze_instruction, output="html"))}
parse_one_cloze_instruction <- function(cloze_instruction, output="originalcloze") {
  #    The function only checks the syntax of CLOZE expressions.

  #cloze_match <- str_match_all(cloze_instruction, CLOZE_PATTERN)[[1]]
  cloze_match <- stringr::str_match(cloze_instruction, CLOZE_PATTERN)

  if (length(cloze_match) / 4 != 1) {
    return(paste("parse_cloze_instruction: function must be applied to a single cloze instruction. Check", cloze_instruction,"\n", colappse=""))
  }

  m <- 1 # only match

  question_fullstr <- cloze_match[m,1]
  question_points <- cloze_match[m,2] #poins
  if (question_points == "") {
    question_points <- "1"
  }
  question_type   <- toupper(cloze_match[m,3]) #instruction
  question_content<- cloze_match[m,4] #configuration


  tag <- get_cloze_type(question_type)
  if (tag == "no tag") {
    cat("CLOZE ERROR: ", question_fullstr, "\n")
    cat("  Instruction '", question_type, "' does not exist (please, check spelling).\n", sep="")
    next
  } else {
    question_type <- tag
  }

  #Debug
  #cat("CLOZE: ", cloze_match[m,1], "\n")
  #cat("  Points: ", question_points, "\n")
  #cat("  Type: ", question_type, "\n")
  #cat("  Content: ", question_content, "\n")


  # #Normalize use of: ~, =, %100%, etc.
  # #=resposta correta~incorreta#feedback
  # #Text should not contain those symbols, yet.
  options <- strsplit(question_content, "~")[[1]]

  if (length(options) == 0) {
    cat("CLOZE Error: question '", question_fullstr, "without options.\n", sep="")
    next
  }

  list_options <- c()
  for (opt_text in options) {


    #Debug
    #print(opt_text)

    #TODO: protect from mistakes like "<no text>#Some feedback"
    o <- parse_one_option(opt_text)

    list_options <- append(list_options, list(o))
  }

  #Verifica se há 100 em alguma a opção :
  # tot = [o.percent>=100 for o in all_options]
  tot  <- sapply(list_options, function(o) o["percent"] >= 100)

  if (!any(tot)) {
    cat("CLOZE ERROR: in '", question_fullstr, "' there is no correct option.\n", sep="")
    next
  }

  cloze_question_parts <- list("points"=question_points, "type"=question_type, "options"=list_options)
  #Debug
  #print(q)



  if (output=="originalcloze") {
    return(cloze_match[m,1]) #original cloze string
  #TODO
  #} else if (output=="regexcloze") {
  #  return(cloze2regexcloze(cloze_question_parts))
  #} else if (output=="html") {
  #  return(cloze2html(cloze_question_parts))
  } else {
    return(cloze_question_parts)
  }
}





#' Transform text written in cloze format intro something else.
#'
#'
#' @param cloze_text text
#' @param output "I" (put "III") TODO...
#'
#' @returns a list (cloze text parts)
#' @export
#'
cloze_transform <- function(cloze_text, output = "originalcloze") {
  matches_info <- gregexpr(CLOZE_PATTERN, cloze_text, perl = TRUE)[[1]]

  #print(matches_info[1])
  #print(attr(matches_info,"match.length"))


  n_clozematches <- length(matches_info)
  cloze_start    <- c(matches_info)
  cloze_len      <- c(attr(matches_info,"match.length"))


  cloze_text_parts <- c()

  text_pos <- 1

  # Situations:
  #
  # 1. text{instruction}text{instruction}text
  # 2. {instruction}text{instruction}text
  # 3. text{instruction}text{instruction}
  # 4. {instruction}{instruction}


  for(m in 1:n_clozematches) {

    if (text_pos < cloze_start[m]) {
      cloze_text_parts <- c(cloze_text_parts, substr(cloze_text, text_pos, cloze_start[m]-1))
    } else {
      #não é adicionado texto porque text_pos = cloze_start
    }

    # call parse_one_cloze_instruction()
    if (output == "I") {
      instruction <- paste(rep("I",cloze_len[m]),collapse="")
    } else {
      original_instruction <- substr(cloze_text, cloze_start[m], cloze_start[m]+cloze_len[m]-1)
      instruction <- parse_one_cloze_instruction(original_instruction, output)
    }

    cloze_text_parts <- c(cloze_text_parts, instruction)

    #Prepare next
    text_pos <- cloze_start[m] + cloze_len[m]
  }
  cloze_text_length <- nchar(cloze_text)
  if (text_pos<=cloze_text_length) {
    cloze_text_parts <- c(cloze_text_parts, substr(cloze_text, text_pos, cloze_text_length))
  }



  return(cloze_text_parts)

}




# verifycloze ====

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

  # Apagar isto:
  #put upper case
  #uclozetext <- toupper(clozetext)
  #uclozetext <- clozetext


  cloze_parts <- cloze_transform(clozetext, output = "originalcloze")

  for (cloze_part in cloze_parts) {

    if (showprogress) {
      cat("uclozetext:\n", uclozetext, "\n")
    }

    cl_length <- nchar(cloze_part)

    #assume below is a condition like "{correct instruction}"
    #but it could not be
    #TODO: improve code in cloze_transform() to say what partes are text and what parts are instructions
    if ((substring(cloze_part, 1, 1) == "{") && (substring(cloze_part, cl_length, cl_length)=="}")) {

      #It's a cloze instruction.
      #The string is a perfect cloze instruction
      #but it could contain <SUB> or other strange things.

      if (length(strsplit(cloze_part, "\n")[[1]])>1) {
        cat(paste0("\n    WARNING: probably a closing '}' is missing.  See '", cloze_part, "' in XML moodle file.\n"))
      }

      # MULTICHOICE_S or NUMERICAL options situations
      if (grepl("<SUB>", cloze_part, ignore.case = TRUE)) {
        cat("\n    Use '\\~' instead of only '~' in RMarkdown file. An isolated ~, in Rmd, means italic and not a false answer.\n")
      }

      # NUMERICAL options situations
      if (grepl("\\^", cloze_part, ignore.case = TRUE) && grepl("NUMERICAL", cloze_part, ignore.case = TRUE) ) {
        cat(paste0("\n    Error: moodle does not support curly braces in numbers like 10^{2}. Suggested use of `options(scipen = 999)` in code. See '", cloze_part, "' in XML moodle file.\n"))
      }

      # NUMERICAL options situations
      if (grepl(" ", cloze_part, ignore.case = TRUE) && grepl("NUMERICAL", cloze_part, ignore.case = TRUE) ) {
        cat(paste0("\n    Error: there is a space in a {:NUMERICAL:=...}.  See '", cloze_part, "' in XML moodle file.\n"))
      }

      if (grepl(":MULTICHOICE:", cloze_part, ignore.case = TRUE)) {
        cat(paste0("\n    Maybe you should use :MULTICHOICE_S:, because it scrambles options, instead of :MULTICHOICE:.  See '", cloze_part, "' in XML moodle file.\n"))
      }

    } else {

      #It's text or a wrong cloze instruction.
      resultofmaybe <- maybe_pattern(cloze_part)
      if (class(resultofmaybe) == "list") {

        #TODO: improve error message with solution
        cat(paste0("    Check eventual situation in CLOZE syntax: "))
        cat(paste0("    ", resultofmaybe$afind$match, "\n"))

      }

    }

  }

}

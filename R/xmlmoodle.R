

# How it works ====

# Algoritmo geral:
#
# 1. Ler via read_html()
# 2. Lê o html e coloca numa estrutura "xml_document"
#
#
# References:
# 1. templating https://cran.r-project.org/web/packages/whisker/whisker.pdf
# 2. json: install.packages("rjson")



# se multichoice: enunciado da variante nas células 2 a n-2;
#    a célula n-1 tem a resposta <ul> e n tem o feeback
# se numerical: enunciado da variante nas células 2 a n-2;
#    a célula n-1 tem a resposta <ul> com opções numericas e n tem o feeback
# se essay: enunciado da variante nas células 2 a n-1;
#    a célula  n tem o feeback
# se cloze: enunciado da variante nas células 2 a n-1;
#    a célula  n tem o feeback

# if (question_type == "MULTICHOICE") {
#     question_tuple <- moodle_question_multichoice(question_name, partes_da_questao)
# } else if (question_type == "NUMERICAL") {
#     question_tuple <- moodle_question_numerical(question_name, partes_da_questao)
# } else if (question_type == "CLOZE") {
#     question_tuple <- moodle_question_cloze(question_name, partes_da_questao)
# } else if (question_type == "ESSAY") {
#     question_tuple <- moodle_question_essay(question_name, partes_da_questao)
# } else {
#     stop("question_type não existente.")
# }



# library(...) imports ====

# TODO: colocar que funções são usadas
# library(rjson) rjson::fromJSON
# library(rmarkdown)  rmarkdown::render


library(whisker)  # whisker.render
library(rvest) # imports read_html, html_children, html_text
library(stringr)
library(xml2)



# xml templates ====


# template: {{exam_name}}, {{question_name}}, {{variant_name}},
#  {{{question_problem}}}, {{{question_problem}}}, {{{answer_correct}}},
#  {{{answer_incorrect1}}}, {{{answer_incorrect2}}}, {{{answer_incorrect3}}}


MULTICHOICE_template4 <- '/
  <question type="category">
    <category>
      <text>$course$/top/importados/{{exam_title}}/{{question_title}}</text>
    </category>
    <info format="html">
      <text></text>
    </info>
    <idnumber></idnumber>
  </question>
  <question type="multichoice">
    <name>
      <text>{{variant_title}} de {{question_title}}</text>
    </name>
    <questiontext format="html">
      <text><![CDATA[{{{question_problem}}}]]></text>
    </questiontext>
    <generalfeedback format="html">
      <text><![CDATA[{{{feedbackglobal}}}]]></text>
    </generalfeedback>
    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.3333333</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
    <single>true</single>
    <shuffleanswers>true</shuffleanswers>
    <answernumbering>none</answernumbering>
    <correctfeedback format="html">
      <text><![CDATA[<p>A sua resposta está correta.</p>]]></text>
    </correctfeedback>
    <partiallycorrectfeedback format="html">
      <text><![CDATA[<p>A sua resposta está parcialmente correta.</p>]]></text>
    </partiallycorrectfeedback>
    <incorrectfeedback format="html">
      <text><![CDATA[<p>A sua resposta está incorreta.</p>]]></text>
    </incorrectfeedback>
    <shownumcorrect/>
    <answer fraction="100" format="html">
      <text><![CDATA[{{{answer_correct}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
    <answer fraction="0" format="html">
      <text><![CDATA[{{{answer_incorrect1}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
    <answer fraction="0" format="html">
      <text><![CDATA[{{{answer_incorrect2}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
    <answer fraction="0" format="html">
      <text><![CDATA[{{{answer_incorrect3}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
  </question>

  '


MULTICHOICE_template5 <- '
  <question type="category">
    <category>
      <text>$course$/top/importados/{{exam_title}}/{{question_title}}</text>
    </category>
    <info format="html">
      <text></text>
    </info>
    <idnumber></idnumber>
  </question>
  <question type="multichoice">
    <name>
      <text>{{variant_title}} de {{question_title}}</text>
    </name>
    <questiontext format="html">
      <text><![CDATA[{{{question_problem}}}]]></text>
    </questiontext>
    <generalfeedback format="html">
      <text><![CDATA[{{{feedbackglobal}}}]]></text>
    </generalfeedback>
    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.3333333</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
    <single>true</single>
    <shuffleanswers>true</shuffleanswers>
    <answernumbering>none</answernumbering>
    <correctfeedback format="html">
      <text><![CDATA[<p>A sua resposta está correta.</p>]]></text>
    </correctfeedback>
    <partiallycorrectfeedback format="html">
      <text><![CDATA[<p>A sua resposta está parcialmente correta.</p>]]></text>
    </partiallycorrectfeedback>
    <incorrectfeedback format="html">
      <text><![CDATA[<p>A sua resposta está incorreta.</p>]]></text>
    </incorrectfeedback>
    <shownumcorrect/>
    <answer fraction="100" format="html">
      <text><![CDATA[{{{answer_correct}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
    <answer fraction="0" format="html">
      <text><![CDATA[{{{answer_incorrect1}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
    <answer fraction="0" format="html">
      <text><![CDATA[{{{answer_incorrect2}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
    <answer fraction="0" format="html">
      <text><![CDATA[{{{answer_incorrect3}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
    <answer fraction="0" format="html">
      <text><![CDATA[{{{answer_incorrect4}}}]]></text>
      <feedback format="html">
        <text></text>
      </feedback>
    </answer>
  </question>
'


NUMERICAL_ANSWER_template <- '
    <answer fraction="{{FRACTION}}" format="moodle_auto_format">
      <text>{{VALUE}}</text>
      <feedback format="html">
        <text><![CDATA[{{FEEDBACK_ANSWER}}]]></text>
      </feedback>
      <tolerance>{{TOLERANCE}}</tolerance>
    </answer>
'


NUMERICAL_template <- '
  <question type="category">
    <category>
      <text>$course$/top/importados/{{exam_title}}/{{question_title}}</text>
    </category>
    <info format="html">
      <text></text>
    </info>
    <idnumber></idnumber>
  </question>
  <question type="numerical">
    <name>
      <text>{{variant_title}} de {{question_title}}</text>
    </name>
    <questiontext format="html">
      <text><![CDATA[{{{question_problem}}}]]></text>
    </questiontext>
    <generalfeedback format="html">
      <text><![CDATA[{{{feedbackglobal}}}]]></text>
    </generalfeedback>
    <defaultgrade>1.0000000</defaultgrade>
    <penalty>0.3333333</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
    <unitgradingtype>0</unitgradingtype>
    <unitpenalty>0.1000000</unitpenalty>
    <showunits>3</showunits>
    <unitsleft>0</unitsleft>
    {{{NUMERICAL_ANSWER}}}
  </question>
' # NUMERICAL_template


CLOZE_template <- '
  <question type="category">
    <category>
      <text>$course$/top/importados/{{exam_title}}/{{question_title}}</text>
    </category>
    <info format="html">
      <text></text>
    </info>
    <idnumber></idnumber>
  </question>
  <question type="cloze">
    <name>
      <text>{{variant_title}} de {{question_title}}</text>
    </name>
    <questiontext format="html">
      <text><![CDATA[{{{question_problem}}}]]></text>
    </questiontext>
    <generalfeedback format="html">
      <text><![CDATA[{{{feedbackglobal}}}]]></text>
    </generalfeedback>
    <penalty>0.3333333</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
  </question>
'  #end of CLOZE_template

ESSAY_template <- '
  <question type="category">
    <category>
      <text>$course$/top/importados/{{exam_title}}/{{question_title}}</text>
    </category>
    <info format="html">
      <text></text>
    </info>
    <idnumber></idnumber>
  </question>
  <question type="essay">
    <name>
      <text>{{variant_title}} de {{question_title}}</text>
    </name>
    <questiontext format="html">
      <text><![CDATA[{{{question_problem}}}]]></text>
    </questiontext>
    <generalfeedback format="html">
      <text><![CDATA[{{{feedbackglobal}}}]]></text>
    </generalfeedback>
    <penalty>0.3333333</penalty>
    <hidden>0</hidden>
    <idnumber></idnumber>
  </question>
'  #end of ESSAY_template




# functions ====


is_level <- function(html_part, str_level, keyword) {
  #html_part: a html "node" like "<div ......."
  #str_level: "section level2" or "section level3"
  #keyword: "feedback", "resposta", "variante"


  if (class(html_part) == "xml_nodeset") {
    html_part <- html_part[[1]]
  }

  #debug
  # GGG <<- html_part # nolint
  # cat('\n\n\ninício de is_level\n')
  # print(html_part)
  # cat( 'contém a keyword = ', grepl(keyword,html_part,ignore.case=TRUE),"\n" )
  # print(html_attrs(html_part)) #umas vezes precisa de [[1]] e outras não !! # AQUI AQUI
  # cat("class de html_part = ", class(html_part), "\n")
  # cat(
  #     html_name(html_part)=='div' && grepl(keyword,html_part,ignore.case=TRUE) &&
  #     html_attrs(html_part)[2] == str_level,
  #     '\nfim de is_level\n\n\n\n'
  # )
  #stop()

  return(html_name(html_part) == "div" &&
         grepl(keyword, html_part, ignore.case = TRUE) &&
         html_attrs(html_part)[2] == str_level)

}




skip_li <- function(snode) {
  #browser()
  st <- paste0(snode)
  #debug
  #cat(s)
  st <- gsub("<li>", "", st)
  st <- gsub("</li>", "", st)
  return(st)
}


question_with_variants <- function(question_title, question_html, main_question_type) {
  # A variante pode ter tipos (multi, numerical, cloze, essay) mas só são efetivos
  # se a questão (que contem variantes) não definir o tipo.
  # A ver se o autor aceita isto!

  # html_question: xml_nodeset que começa com <h1> e depois uma ou mais "variantes"
  # question_type: "MULTICHOICE", etc

  #Nota: desce demais para dentro dos "filhos" (netos?)
  # html _ children(html_question)

  # O Rmd produz um <div ...> ... </div>, ilustrado abaixo, que contém todo o exercício
  # incluindo o nome da questão em <h1>....nome...titulo ... </h1>
  #
  #<div id="capítulo-5.1---enunciado-1---correlação-idade-x-altura---multichoice" class="section level1">
  #

  #Get question name in <h1>
  #question_name <- html_text( html_question[1] )


  #debug
  #cat("\n\n\n---------------\n")
  #print(question_title)
  #cat("\n---------------\n\n\n")




  # Algoritmo:
  # 1. procurar "variante"   (futuro: o tipo de questão associado?)
  #    procurando class="section level2"
  #     <div id="variante-1-para-esta-questão-dentro-deste-teste" class="section level2">
  # 2. procurar resposta como sendo level3 e dentro desta:
  # 2a. Se MULTICHOICE procurar <ul> <li> ... </li> </ul> sendo a primeira verdadeira
  # 2b. Se NUMERICAL procurar <ul> <li> ... </li> </ul> sendo a primeira 100% e as
  #     outras logo se vê (valor, tol, percentagem) ?
  # 2c. Se CLOZE ou OPENQUESTION só passar
  # 3. Procurar feedback, como sendo level3, como sendo o feedback ao estudante.

  # Percorre as variantes ou ignora
  total_variants <- 0
  num_of_nodes <- length(question_html)
  variants <- list()

  #first node was the question title
  #Other nodes are expected to contain 'variants'
  for (node in seq_len(num_of_nodes)) {

    question_part <- question_html[node]


    if (is_level(question_part, "section level2", "variante") == TRUE) {
      #mesmo que: if (html_name(question_part)=='div' && html_attrs(question_part)[[2]] == "section level2") {
      # Se <div variante ....class = "section level2":

      variant_contents <- html_children(question_part)

      #Será algo como <h2>variante 1</h2> será o 1º "filho" dentro do <div>
      variant_title <- html_text(variant_contents[1])

      # Pode ser:
      # se multichoice: enunciado da variante nas células 2 a n-2; a célula n-1 tem a resposta <ul> e n tem o feeback
      # se numerical: enunciado da variante nas células 2 a n-2; a célula n-1 tem a resposta <ul> com opções numericas e n tem o feeback
      # se openquestion: enunciado da variante nas células 2 a n-1; a célula  n tem o feeback
      # se cloze: enunciado da variante nas células 2 a n-1; a célula  n tem o feeback

      #Se a questão tem um tipo de questão "indefinido" então
      # as variantes devem definir o tipo.
      if (main_question_type == "indefinido") {

        if (grepl("MULTICHOICE", variant_title, ignore.case = TRUE)) {
          question_type <- "MULTICHOICE"
        } else if (grepl("NUMERICAL", variant_title, ignore.case = TRUE)) {
          question_type <- "NUMERICAL"
        } else if (grepl("CLOZE", variant_title, ignore.case = TRUE)) {
          question_type <- "CLOZE"
        } else if (grepl("ESSAY", variant_title, ignore.case = TRUE)) {
          question_type <- "ESSAY"
        } else {
          stop(cat(question_title,
                   "(or the variant title) must have MULTICHOICE, NUMERICAL, CLOZE, ESSAY tag\n"))
        }
      } else {
        question_type <- main_question_type
      }


      if (question_type == "MULTICHOICE") {
        cat(" Processing", variant_title, "\n")
        total_variants <- total_variants + 1
        variants[[total_variants]] <- multichoice(variant_title, variant_contents)
      } else if (question_type == "NUMERICAL") {
        cat(" Processing", variant_title, "\n")
        total_variants <- total_variants + 1
        variants[[total_variants]] <- numerical(variant_title, variant_contents)
      } else if (question_type == "CLOZE") {
        cat(" Processing", variant_title, "\n")
        total_variants <- total_variants + 1
        variants[[total_variants]] <- cloze(variant_title, variant_contents)
      } else if (question_type == "ESSAY") {
        cat(" Processing", variant_title, "\n")
        total_variants <- total_variants + 1
        variants[[total_variants]] <- essay(variant_title, variant_contents)
      } else {
        warning(paste("Question type ('question_type') not existing.\nMust be: MULTICHOICE, NUMERICAL, CLOZE, ESSAY. Review question:", question_title, "\n"))
      }
    }
    #else: ignora (para já) e nada diz ao autor!
  }

  if (total_variants < 1) {
    stop("A question with variants is expected: a section defined by '##' must have the syntax '## variant <and some text>'.\nOr this Rmd file is not an isolated test but is part of a question base (rmdmoodle format).")
  }

  return(list(question_title = question_title,
              variants = variants))
}



numerical <- function(variant_title, variant_contents) {

  n <- length(variant_contents)

  enunciado <- paste0(variant_contents[2:(n - 2)], collapse = "\n")

  # resposta - deve ocorrer sempre na posição n-1
  if (is_level(variant_contents[n - 1], "section level3", "respostas") ||
      is_level(variant_contents[n - 1], "section level3", "answers")
      ) {

    #Formato a usar no Rmd:
    #
    # * "answer": 100.1, "tol": 0.001, "fraction": 100, "feedback" : "Aqui uma longa frase 1."
    # * "answer": 100.1, "tol": 0.001, "fraction": 100, "feedback" : "Aqui uma longa frase 2."

    r <- html_children(variant_contents[n - 1])
    ulist_items <- html_children(r[2])

    # Debug
    # nr <- length(ulist_items)
    # resposta <- paste( r[2:nr], collapse = "\n")


    # Debug
    #XX <<- paste('{',html_text(ulist_items[1]),'}',sep='')
    #XX <<- fromJSON(paste('{',html_text(ulist_items[1]),'}',sep=''))

    # Zona perigosa: ', ", “, ”:
    #   O autor introduz:   "coisa": 10
    #   No html surge:      “coisa”: 10
    #
    #[1] "{\"answer”: 100.1, \"tol”: 0.001, \"fraction”: 100, \"feedback” : \"Aqui um alonga resposta 1”}"
    #> xx = gsub('“', '\"', '{“answer”: 100.1, “tol”: 0.001, “fraction”: 100, “feedback” : “Aqui um alonga resposta 1”}' ) # nolint
    #> xx
    #[1] "{\"answer”: 100.1, \"tol”: 0.001, \"fraction”: 100, \"feedback” : \"Aqui um alonga resposta 1”}"
    #> xx = gsub('“', '\"', '{“answer”: 100.1, “tol”: 0.001, “fraction”: 100, “feedback” : “Aqui um alonga resposta 1”}' ); xx
    #[1] "{\"answer”: 100.1, \"tol”: 0.001, \"fraction”: 100, \"feedback” : \"Aqui um alonga resposta 1”}"
    #> xx = gsub('“', "'", '{“answer”: 100.1, “tol”: 0.001, “fraction”: 100, “feedback” : “Aqui um alonga resposta 1”}' ); xx
    #[1] "{'answer”: 100.1, 'tol”: 0.001, 'fraction”: 100, 'feedback” : 'Aqui um alonga resposta 1”}"
    #> xx2 = gsub('”', "'", '{“answer”: 100.1, “tol”: 0.001, “fraction”: 100, “feedback” : “Aqui um alonga resposta 1”}' ); xx2 # nolint
    #[1] "{“answer': 100.1, “tol': 0.001, “fraction': 100, “feedback' : “Aqui um alonga resposta 1'}"



    respostas <- lapply(ulist_items, html_to_json_protected)

    #Debug
    #RESPOSTAS <<- respostas

  } else {
    stop("In a 'NUMERICAL' question, there must be a '### answers' section and a '### feedback' section in each variant.\nAfter modifying, you have to 'knitr'.") # nolint
  }

  # feedback global - deve ocorrer sempre na posição n
  if (is_level(variant_contents[n], "section level3", "feedback")) {
    #coleciona todo o feedback
    h <- html_children(variant_contents[n])
    nh <- length(h)
    if (nh == 1) {
      feedbackglobal <- "\n\n\n"
      cat("    Variante 'NUMERICAL' com feedback vazio.\n")
    } else {
      feedbackglobal <- paste0(h[2:nh], collapse = "\n")
    }
  } else {
    stop("In a 'NUMERICAL' question, there must be a '### answers' section and a '### feedback' section in each variant.\nAfter modifying, you have to 'knitr'.")
  }


  return(list(variant_title = variant_title,
              variant_type = "NUMERICAL",
              enunciado = enunciado,
              respostas = respostas,
              feedbackglobal = feedbackglobal))

}



multichoice <- function(variant_title, variant_contents) {
  # A "question", in a RMarkdown file, can contain one or more "variants"
  # and this routine gets contents of a variant:
  # problem statement "enunciado"
  # answer in moodel format "resposta"
  # feedback "feedback" for each student

  # verificações no caso de multichoice
  # se multichoice: enunciado da variante nas células 2 a n-2;
  # a célula n-1 tem a resposta <ul> e n tem o feeback

  n <- length(variant_contents)

  # enunciado - ocorre da 2 a n-2; requer (n-2)
  enunciado <- paste0(variant_contents[2:(n - 2)], collapse = "\n")
  #debug - ainda não se entende o 4 na linha acima
  #cat("\n\n\n--------------------\n\n")
  #VC <<- variant_contents
  #VCN <<- n
  #print(variant_contents[4:n-2])
  #cat('enunciado:\n',enunciado,'\n\n')
  #cat("\n--------------------\n\n")

  # resposta - deve ocorrer sempre na posição n-1
  if (is_level(variant_contents[n - 1], "section level3", "respostas")) {

    r <- html_children(variant_contents[n - 1])
    ulist_items <- html_children(r[2])
    for (u in ulist_items) {
      if (grepl("start=", u)) {
        warning("In ## answers, please avoid using '* 9.' (with an explicit dot '.'). Use only '* 9' without a dot.")
        #TODO: improve this
      }
    }

    #respostas <- sapply(ulist_items, html_text) skip_li
    respostas <- lapply(ulist_items, skip_li)


    #Debug
    #browser()
    #RESPOSTAS <<- respostas

  } else {
    stop("In a 'MULTICHOICE' question, there must be a '### answers' section and a '### feedback' section in each variant.\nAfter modifying, you have to 'knitr'.")
  }

  # feedback - deve ocorrer sempre na posição n
  if (is_level(variant_contents[n], "section level3", "feedback")) {
    #coleciona todo o feedback
    h <- html_children(variant_contents[n])
    nh <- length(h)
    if (nh == 1) {
      feedbackglobal <- "\n\n\n"
      cat("    Variant 'MULTICHOICE' with empty feedback.\n")
    } else {
      feedbackglobal <- paste0(h[2:nh], collapse = "\n")
    }

  } else {
    stop("In a 'MULTICHOICE' question, there must be a '### answers' section and a '### feedback' section in each variant.\nAfter modifying, you have to do 'knitr'.")
  }


  return(list(variant_title = variant_title,
              variant_type = "MULTICHOICE",
              enunciado = enunciado,
              respostas = respostas,
              feedbackglobal = feedbackglobal))

}



#' @title cloze - joins text and verify moodle-cloze {....} instructions
#'
#' @param variant_title a string
#' @param variant_contents a string
#'
#' @return alist with several attributes
cloze <- function(variant_title, variant_contents) {

  n <- length(variant_contents)

  enunciado <- paste0(variant_contents[2:(n - 1)], collapse = "\n")
  enunciado <- gsub("\x0D\x0D\x0A", "", enunciado)
  #debug
  #cat("enunciado:\n",enunciado,"\n")

  #in file verifycloze.R
  verifycloze(enunciado)


  # feedback global - deve ocorrer sempre na posição n
  if (is_level(variant_contents[n], "section level3", "feedback")) {
    #coleciona todo o feedback
    h <- html_children(variant_contents[n])
    nh <- length(h)
    if (nh == 1) {
      feedbackglobal <- "\n\n\n"
      if (pkg_env$WARNINGS.BOOLEAN) {
        cat("    There is a 'CLOZE' variant with empty feedback.\n")
      }
    } else {
      feedbackglobal <- paste0(h[2:nh], collapse = "\n")
    }
  } else {
    stop("In a 'cloze' question, it must exist a section '### feedback', possibly empty, in each variant.\n")
    #stop(_("Numa questão 'cloze' tem que existir a secção '### feedback' em cada variante (ainda que possa estar vazia).\nApós a modificação tem que fazer 'knitr'."))
  }


  return(list(variant_title = variant_title,
              variant_type = "CLOZE",
              enunciado = enunciado,
              feedbackglobal = feedbackglobal))

}




essay <- function(variant_title, variant_contents) {

  n <- length(variant_contents)
  enunciado <- paste(variant_contents[2:(n - 1)], collapse = "\n")

  # feedback global - deve ocorrer sempre na posição n
  if (is_level(variant_contents[n], "section level3", "feedback")) {
    #coleciona todo o feedback
    h <- html_children(variant_contents[n])
    nh <- length(h)
    if (nh == 1) {
      feedbackglobal <- "\n\n\n"
      cat("    Variante 'ESSAY' com feedback vazio.\n")
    } else {
      feedbackglobal <- paste(h[2:nh], collapse = "\n")
    }
  } else {
    stop("In an 'ESSAY' question there must be a '### feedback' section in each variant (even though it may be empty).\nAfter the modification you have to do 'knitr'.")
  }


  return(list(variant_title = variant_title,
              variant_type = "ESSAY",
              enunciado = enunciado,
              feedbackglobal = feedbackglobal))

}




#' From a list of questions produce a xml file
#'
#' The xml file is to be imported in Moodle.
#'
#' @param exam_title - name of the future file
#' @param all_questions - a list
#'
#' @return - a saved file in system
saveto_xmlmoodle <- function(filename_no_extension, exam_title, all_questions) {


  #<?xml version="1.0" encoding="UTF-8"?>
  #<quiz>
  #</quiz>

  xml_str <- '<?xml version="1.0" encoding="UTF-8"?>\n<quiz>\n'

  #nquestions <- length(all_questions)

  for (q in seq_along(all_questions)) {

    question <- all_questions[[q]]

    #debug
    #QUESTION <<- question


    variants <- question$variants
    #nvariants <- length(question)-2
    nvariants <- length(variants)


    for(v in seq_len(nvariants)) {

      #variant <- question[[v+2]]
      variant <- variants[[v]]

      if (variant$variant_type == "MULTICHOICE") {

        #debug
        #TODO inventar um relatório promenorizado do teste
        #e colocar lá esta informação
        #cat("Nr. de opções obtidas =", length(variant$respostas),"\n")

        if (length(variant$respostas) == 4) {
          xml_str <- paste(xml_str,
                          whisker.render(MULTICHOICE_template4, list(
                            exam_title        = exam_title,
                            question_title    = question[[1]],
                            variant_title     = variant$variant_title,
                            question_problem  = variant$enunciado,
                            answer_correct    = variant$respostas[1], #"correta",
                            answer_incorrect1 = variant$respostas[2], #"incorreta 1",
                            answer_incorrect2 = variant$respostas[3], #"incorreta 2",
                            answer_incorrect3 = variant$respostas[4], #"incorreta 3",
                            feedbackglobal    = variant$feedbackglobal)
                          ),
                          sep = "\n"
          )
        } else {
          xml_str <- paste(xml_str,
                          whisker.render(MULTICHOICE_template5, list(
                            exam_title        = exam_title, # nolint
                            question_title    = question[[1]],
                            variant_title     = variant$variant_title,
                            question_problem  = variant$enunciado,
                            answer_correct    = variant$respostas[1], #"correta",
                            answer_incorrect1 = variant$respostas[2], #"incorreta 1",
                            answer_incorrect2 = variant$respostas[3], #"incorreta 2",
                            answer_incorrect3 = variant$respostas[4], #"incorreta 3",
                            answer_incorrect4 = variant$respostas[5], #"incorreta 4",
                            feedbackglobal    = variant$feedbackglobal)
                          ),
                          sep = "\n"
          )
        }
      } else if (variant$variant_type == "NUMERICAL") {

        NUMERICAL_ANSWER_xml <- ""

        # Debug
        #print("A exportar numerical para xml:")
        #print(variant$respostas)
        #VVV <<- variant$respostas

        for(r in variant$respostas) {
          NUMERICAL_ANSWER_xml <- paste(
            NUMERICAL_ANSWER_xml,
            whisker.render(
              NUMERICAL_ANSWER_template,
              list(
                FRACTION         = r$fraction,
                VALUE            = r$answer,
                FEEDBACK_ANSWER  = r$feedback,
                TOLERANCE        = r$tol
              )
            ),
            sep = "\n"
          )
        }

        #nr <- length(variant$respostas)

        xml_str <- paste(xml_str,
                         whisker.render(NUMERICAL_template, list(
                           exam_title        = exam_title,
                           question_title    = question[[1]],
                           variant_title     = variant$variant_title,
                           question_problem  = variant$enunciado,
                           NUMERICAL_ANSWER  = NUMERICAL_ANSWER_xml,
                           feedbackglobal    = variant$feedbackglobal
                         )
                         ),
                         sep = "\n"
        )

      } else if (variant$variant_type == "CLOZE") {

        xml_str <- paste(xml_str,
                         whisker.render(CLOZE_template, list(
                           exam_title        = exam_title,
                           question_title    = question[[1]],
                           variant_title     = variant$variant_title,
                           question_problem  = variant$enunciado,
                           feedbackglobal    = variant$feedbackglobal)
                         ),
                         sep = "\n"
        )

      } else if (variant$variant_type == "ESSAY") {

        xml_str <- paste(xml_str,
                         whisker.render(ESSAY_template, list(
                           exam_title        = exam_title,
                           question_title    = question[[1]],
                           variant_title     = variant$variant_title,
                           question_problem  = variant$enunciado,
                           feedbackglobal    = variant$feedbackglobal)
                         ),
                         sep = "\n"
        )
      } else {
        warning(paste0("Unknown variant$variant_type: ", variant$variant_type, "\n"))
      }

    } #end for variants

  } #end for questions


  xml_str <- c(xml_str, "</quiz>\n")

  #debug
  #cat(xml_str,"\n")

  f <- file(paste0(filename_no_extension, ".xml"),
            open = "wt",
            encoding = "utf8")
  write(xml_str, file = f)
  close(f)

}





#' Extracts "moodle questions" from a html file
#'
#' @param filename - an user/author written filename
#' @param noneiscorrect
#'
#' @return - a list with "moodle questions"
extractquestions_fromhtml <- function(filename_no_extension) {

  library(rvest)

  filename <- paste0(filename_no_extension, ".html")

  #read_html is a function from rvest library
  #`html
  html <- read_html(filename, encoding = "UTF-8")

  # Extracts section <body> ... </body>
  body <- html_elements(html, "body")

  # Inside <body> there are sub sections "children":
  body_children <- html_children(body)

  # TODO: melhorar a frase com exemplo
  # Só interessa o primeiro elemento dessas secções
  # Os outros elemento são javascript e não interessam no moodle
  conteudo <- body_children[1]

  # Get "title:" now in <h1> tag
  all_h1_elements <- html_elements(conteudo, "h1")
  exam_title <- html_text(all_h1_elements[1])

  # Debug
  #cat("\n\nNome do teste/exame: ", exam_title, '\n\n')

  all_questions <- list()
  total_questions <- 0

  # Get all sections<h1>
  # position 1 of all_h1_elements has the "title:" (originally in RMarkdown)
  # position 2 of all_h1_elements has the name of the first question
  # etc
  first_question_title <- html_text(all_h1_elements[2])
  start <- 1
  text_on_h1 <- html_text(html_children(conteudo)[start])
  while (grepl(first_question_title,
               text_on_h1,
               ignore.case = TRUE) == FALSE) {
    start <- start + 1
    text_on_h1 <- html_text(html_children(conteudo)[start])
  }


  # TODO: why?
  # Only 3rd <h1> and after has questions.
  for (i in seq(2, length(all_h1_elements))) { #i=1 é o título do exame, i=2 será a primeira questão "h1"

    question_title <- html_text(all_h1_elements[i])

    #debug
    #cat("----------------\n")
    #cat(question_title,"\n")
    #cat("----------------\n")

    cat("\n------------------\n")
    cat(paste0("Processing question: ", question_title, " (", filename_no_extension, ".Rmd)\n"))
    cat("--------------------\n\n")

    if (grepl("MULTICHOICE", question_title, ignore.case = TRUE)) {
      main_question_type <- "MULTICHOICE"
    } else if (grepl("ESSAY", question_title, ignore.case = TRUE)) {
      main_question_type <- "ESSAY"
    } else if (grepl("CLOZE", question_title, ignore.case = TRUE)) {
      main_question_type <- "CLOZE"
    } else if (grepl("NUMERICAL", question_title, ignore.case = TRUE)) {
      main_question_type <- "NUMERICAL"
    } else {
      cat("   (Questão sem tipo pré-definido)\n")
      main_question_type <- "indefinido" #deixa-se para as variantes
    }

    question_div_h1 <- html_children(html_children(conteudo)[start + i - 2])
    total_questions <- total_questions + 1
    all_questions[[total_questions]] <-
     question_with_variants(question_title, question_div_h1, main_question_type)

  }

  #debug
  #cat("xml_moodle: final\n")
  #print(all_questions)

  # Escreve informação no "moodle xml".


  return(list(all_questions = all_questions,
              exam_title = exam_title))
}






#' `find_setwd()` search for instructions `setwd()`
#'
#' The idea is to warn user to put data and images files in same path
#' or to add an absolut path.
#'
#' Called in `xmlmoodle()`
#'
#' @return nothing
find_setwd <- function(filename_no_extension) {

  filename <- paste0(filename_no_extension, ".Rmd")

  all_lines <- readLines(filename)

  lineno <- 0

  for (l in all_lines) {

    lineno <- lineno + 1

    if (grepl("setwd",l)) {

      cat(paste0("\nIn line ", lineno,
                 " of ", filename,
                 " there is a setwd() call.\n"))
      cat("Avoid use of setwd() and place data or images files in same folder, or a subfolder, of Rmd files.\n")
      cat("Another option is to use global path like 'c://users//name//etc//data.csv' or for images 'c://users//name//etc//an_image.png'\n\n")
    }
  }

}


html_to_json_protected <- function(html_code) {
  #função usada em numerical(....)
  # author protected input
  # Problem: author must input JSON in Rmd file
  # This can be a problem because of the extreme
  # rigorous syntax.
  # This function warns the user (at least).

  txt <- html_text(html_code)
  txt1 <- gsub('“', '"', txt) # nolint
  txt2 <- gsub('”', '"', txt1)# nolint
  txt3 <- paste('{', txt2, '}', sep="") # nolint

  #Debug
  #cat(txt3,"\n")

  # Source: https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  out <- tryCatch(
    rjson::fromJSON(txt3),  #para várias linhas de código incluir {...linhas...}
    error = function(cond) {
      message(paste('You need to revise the syntax in this line:\n"', txt, "\n"))
      message('The notation is expected to look like this:\n   "palavrachave": 123.456\n   "palavrachave": "texto entre aspas"\n   apenas uma "," entre palavrashcave.\n')
      message('Each line must be the same as:\n* "answer": 100.1, "tol": 0.001, "fraction": 100, "feedback" : "Aqui um alonga resposta 1"\n')
      message("\nThe error message in the R code may help:")
      message(cond)
      stop()
    },
    warning = function(cond) {
      message(paste("You need to revise the syntax in this line:", txt))
      message(cond)
      stop()
    }
    #,
    #finally={
    ## NOTE:
    ## Here goes everything that should be executed at the end,
    ## regardless of success or error.
    ## If you want more than one expression to be executed, then you
    ## need to wrap them in curly brackets ({...}); otherwise you could
    ## just have written 'finally=<expression>'
    #    message(paste("Processed URL:", url))
    #    message("Some other message at the end")
    #}
  )
  return(out)
}



#' Exam in R Markdown to XML Moodle exam.
#'
#' Convert an Rmd file containing questions and variants
#' to xml moodle file to be imported for the "exam" tool
#' in moodle.
#'
#' @param filename_no_extension file where an exam in Rmd is stored
#' @param params list with same yaml params at top of R Markdown file
#'
#' @return A xml file to be imported in moodle.
#' @export
#'
#' @examples
#' \dontrun{xmlmoodle("my_exam.Rmd")}
#' \dontrun{xmlmoodle("my_exam")}
xmlmoodle <- function(filename_no_extension, params = NULL) {

  if (grepl("\\.", filename_no_extension)) {
    filename_no_extension <- tools::file_path_sans_ext(filename_no_extension)
  }


  #' Warn author if `setwd()` is used.
  #' Search for `setwd()` in order to warn
  #' user to put data and images files in same path
  #' or to add an absolut path.
  find_setwd(filename_no_extension)

  #debug
  #print(params)


  #' Método sem multivariantes
  #' TODO: o que são multivariantes ??
  #'
  tryCatch(
    {
      # Just to highlight: if you want to use more than one
      # R expression in the "try" part then you'll have to
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression
      # in case the "try" part was completed successfully

      cat(paste0("\nMaking ", filename_no_extension, ".html\n\n"))
      if (is.null(params)) {

        #debug
        #cat("rmarkdown::render without params\n")

        rmarkdown::render(paste0(filename_no_extension, ".Rmd"),
                          output_format = "html_document",
                          quiet = TRUE)
      } else {

        #debug
        #cat("rmarkdown::render WITH params\n")

        rm(params)
        cat(paste0("File ", filename_no_extension, ".Rmd has params.\n"))
        rmarkdown::render(paste0(filename_no_extension, ".Rmd"),
                          params = params,
                          output_format = "html_document",
                          quiet = TRUE)
      }

      # The return value of `rmarkdown::render()` is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error = function(cond) {

      #DEBUG
      #A palavra "Quitting" não surge no cond$message
      #"at("Quitting is", grepl("Quitting", cond$message), "\n")
      #print(str(cond))

      if (grepl("Duplicate chunk label", cond)) {
        message("\nAvoid using code chunk labels. Code chunks from several files could have same name and `knitr()` does not accept two equal named code chunks. Find information about the code chunk in the next message.\n")
      } else if (grepl("not found", cond)) {
        #message("\nAn used variable has no declaration. Probably the code declared in an item, that contains that variable, must be relocated to '# code' where common code resides.\n")
        print(cond)
        message(paste0("\nA variable was not found knitting ", filename_no_extension, ".Rmd.\n",
                       "If `rmdexam(...)` is being used, find in which question the variable is declared and move that declaration code to '# code' section.\n"
                       ))
      } else if (grepl("cannot open the connection", cond)) {
        stop(paste0("\nRun knitr on file '", filename_no_extension, ".Rmd' and check for missing files (probably data files).\n"))
      } else {
        cat(paste0("Please, open '", filename_no_extension, ".Rmd' and execute `knitr` to check possible errors.\n\n"))
        message(cond)
        cat("\n\n")
      }

      #message(paste("URL does not seem to exist:", url))
      #message("Here's the original error message:")
      # Choose a return value in case of error
      #return(NA)
    },
    warning = function(cond) {
      #message(paste("URL caused a warning:", url))
      #message("Here's the original warning message:")
      #message(cond)
      #Debug
      if (grepl("fetch resource", cond)) {
        cat(paste0("Copy the image to ", filename_no_extension, ".Rmd\n"))
      } else {
        print(class(cond))
      }
      # Choose a return value in case of warning
      #return(NULL)
    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>'
      #message(paste("Processed URL:", url))
      #message("Some other message at the end")
    }
  )



  cat(paste0("\nMaking ", filename_no_extension, ".xml\n\n"))

  #Produce a list() with all questions
  q_t <- extractquestions_fromhtml(filename_no_extension)

  slug_exam_title <- slugify(q_t$exam_title, space_replace = " ") #espaços ok!

  saveto_xmlmoodle(filename_no_extension, slug_exam_title, q_t$all_questions)



  cat(paste0("\n"))
  cat(paste0("\nPlease, import file ", filename_no_extension, ".xml into moodle.\n\n"))


}

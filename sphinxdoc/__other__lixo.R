

# -----------------------
# funções por documentar
# -----------------------


exshow <- function(filename) {
  cat(knitr::knit_child(filename,quiet = TRUE));
}


# -----------
# HERE
# -----------
variants <- function(nrep, enredo, alineas) {

  # TODO: melhorar
  # tempfile(fileext = ".Rmd")
  temp_filename = 'LIXO.Rmd'
  filecon <- file(temp_filename, 'wt')


  # Build a variant
  writeLines(text = "## Variante `r VAR`\n\n",
             con = filecon)

  #Lê o enredo
  extxt = readLines(paste0(EXDIR,enredo))

  #Escreve o enredo
  writeLines(text = extxt, con = filecon)

  # Lê e escreve as alíneas
  if (length(alineas)==1) {

    # alína única não tem numeração "(a)"

    extxt = readLines(paste0(EXDIR,alineas[1])) #uma única

    #writeLines(text=paste0("\n\n**(", letters[l], ")**\n\n"), con=filecon)

    writeLines(text = extxt, con = filecon)

  } else {

    # várias alíneas

    l = 1
    for( expath in alineas) {
      extxt = readLines(paste0(EXDIR,expath))

      writeLines(text=paste0("\n\n**(", letters[l], ")**\n\n"), con=filecon)
      writeLines(text = extxt, con = filecon)

      l = l+1

    }
  }

  # Build a variant
  writeLines(text = "\n\n### feedback\n\n",
             con = filecon)

  flush(filecon)
  close(filecon)



  for(VAR in 1:nrep) {
    VAR <<- VAR
    assertthat::assert_that(VAR <= COUNT)
    cat(knitr::knit_child(temp_filename, quiet = TRUE));
  }

  # Now let's delete it.
  # unlink(txtPath)
}


multivariantes <- function(ficheiro_sem_ext) {

  #https://stringr.tidyverse.org/reference

  # https://stackoverflow.com/questions/12626637/read-a-text-file-in-r-line-by-line
  conr = file(paste0(ficheiro_sem_ext,'.Rmd',collapse=''), "r", encoding="utf-8")
  conw = file(paste0(ficheiro_sem_ext,'_variantes.Rmd',collapse=''), "w", encoding="utf-8")

  entrou_em_multivariante <- FALSE
  conteudo_multivariante <- list()

  while ( TRUE ) {

    line = readLines(conr, n = 1)
    if ( length(line) == 0 ) {
      break
    }

    #no final do ficheiro Rmd há comandos
    #que não devem passar para o "..._variantes.Rmd"
    res <- str_match(line, "<!-- fim -->")
    if (!is.na(res[1])) {
      break
    }

    #Debug
    #print(line)
    #print(length(line))

    res <- str_match(line, "\\s*##\\s+multivariante\\s+(\\w+)")
    #debug
    #print(res)

    if (! is.na(res[2]) ) {

      entrou_em_multivariante <- T

      multivariant_text = list()

      varname <- res[2]

      param <- eval(parse(text=varname))

      #Debug
      print(eval(parse(text=varname)))

    } else {

      res <- str_match(line, "\\s*##\\s+fim")
      #print(res[1])

      if (! is.na(res[1]) ) {

        template <- paste0(conteudo_multivariante,collapse='\n')


        varnum <- 0

        #gerar as variantes
        for( p in param) {

          txt <- whisker.render(template, p)

          varnum <- varnum + 1
          writeLines( sprintf("## variante %02d", varnum), conw)
          writeLines(txt, conw)

        }

        entrou_em_multivariante <- F
        conteudo_multivariante <- list()

      } else {

        if (entrou_em_multivariante) {
          conteudo_multivariante <- c(conteudo_multivariante, line)
        } else {
          writeLines(line, conw)
        }

      }

    }

    #print(line)
  }

  close(conr)
  close(conw)
}

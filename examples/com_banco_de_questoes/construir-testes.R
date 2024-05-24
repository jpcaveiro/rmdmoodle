

# Para instalar do github.
devtools::install_github("https://github.com/jpcaveiro/rmdmoodle", reset=TRUE)


# Load functions
library(rmdmoodle)


# Mudar, por favor, indicando onde
# estão os exercícios  ("~" é a pasta Documentos)
set_exercise_root("~/banco_de_questoes" )


# Mudar, por favor, indicando onde
# se vão gravar os testes?
setwd("~/os_meus_testes")




# Construir o Teste 1 do Turno 1
rmdexam("2022-2023-Teste1-Turno1.Rmd", # nome do teste, para um turno, em Rmd
        #Questão 1 com 2 alíneas (pq() quer dizer planned question)
        pq("c1-descritiva.Rmd", "média", "conceito1"),
        #Questão 2 com 2 alíneas (todas as repetições em VARCOUNT)
        pq("c3-proptest.Rmd", "prop01", "th01", "concl01")
)


# Construir o Teste 1 do Turno 2
rmdexam("2022-2023-Teste1-Turno2.Rmd", # nome do teste, para um turno, em Rmd
        #Questão 1 com 2 alíneas (rq() quer dizer random question)
        rq(number=6,"c1-descritiva-aleatoria.Rmd", "desvio-padrão", "conceito1"),
        #Questão 2 com 2 alíneas (todas as repetições em VARCOUNT)
        pq("c3-proptest.Rmd", "prop01", "Tobs01", "concl02")
)


# Ficheiros de imagens ou de dados devem estar
# nas duas pastas: banco de questões e onde se
# guardam os testes.


# Após execução, são produzidos os ficheiros:
# 2022-2023-Teste1-Turno1.xml
# 2022-2023-Teste1-Turno2.xml


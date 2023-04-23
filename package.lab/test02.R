

setwd("~/WorkPackages/2020-rmdmoodle/2023-package-tests")
library(devtools)


# Pode ser preciso:
#.rs.restartR()


# Evita a instalação e library(rmdmoodle):
# With reset=TRUE, unload and reload the package for a clean start
load_all("C:/Users/pedrocruz/Documents/GitHub/rmdmoodle", reset=TRUE)


set_exercise_root( "C:/Users/pedrocruz/Documents/GitHub/rmdmoodle/examples" )
get_exercise_root()


# Create an exam from exercises.
r <- rmdexam(6,
             "exame-de-hoje.Rmd",
             c("cap3/c3-estimativa.Rmd", "alinea01", "alinea04"),
             c("cap4/c4-anova-1fator-com-dados.Rmd", "id01", "concept01") )
print(r)


# ------
# IDEIA
# ------

# Create an exam from exercises.
r <- rmdexam(6, #<- número de variantes a usar (TODO: melhorar)
                    "exame-de-hoje.Rmd",
                    c("cap3/c3-estimativa.Rmd", "alinea01", "alinea04", 60), #<- 6 valores para a questão cloze
                    c("cap4/c4-anova-1fator-com-dados.Rmd", "id01", "concept01", 140) ) #<- 14 valores para a questão cloze
print(r)

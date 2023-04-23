
#
# https://journal.r-project.org/




# Organizar

# Do at startup:

setwd("~/WorkPackages/2020-rmdmoodle/2023-package-tests")

# Pode ser preciso:
#.rs.restartR()


library(devtools)


# Correr com setwd() dentro do package ou passar a diretoria do package.
# correr duas vezes para retirar o erro
#    Warning messages:
#        1: Objects listed as exports, but not present in namespace
#    https://stackoverflow.com/questions/26575147/building-r-package-error-objects-listed-as-exports-but-not-present-in-namespac
devtools::document("C:/Users/pedrocruz/Documents/GitHub/rmdmoodle")


# Instalar mesmo. Depois tem que ser library(rmdmoodle).
install("C:/Users/pedrocruz/Documents/GitHub/rmdmoodle", build = TRUE)


#verifica a compilação
devtools::check("C:/Users/pedrocruz/Documents/GitHub/rmdmoodle")




# Evita a instalação e library(rmdmoodle):
# With reset=TRUE, unload and reload the package for a clean start
#load_all("C:/Users/pedrocruz/Documents/GitHub/rmdmoodle", reset=TRUE)


# VER test02.R

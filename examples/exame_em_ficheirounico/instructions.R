
# rmdmoodle
# exam in single file examples


# To install last version from github
# devtools::install_github("https://github.com/jpcaveiro/rmdmoodle", reset=TRUE)


library(rmdmoodle)


# An exam in english
xmlmoodle("exam-en.Rmd")


# An exam in portuguese
xmlmoodle("exam-pt.Rmd")


# Outputs are
# exam-en.xml (to be imported into Moodle XML)
# exam-pt.xml (to be imported into Moodle XML)


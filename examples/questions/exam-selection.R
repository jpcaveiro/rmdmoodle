

# Install from github using devtools package
library(devtools
install_github("jpcaveiro/rmdmoodle")

# Load the installed package
library(rmdmoodle)

# Using exercises in examples inside package folder
pkg_dir <- system.file(package = "rmdmoodle")
rmdmoodle::set_exercise_root( paste0(pkg_dir,"/examples", collapse="") )


# Check directory where ready exercises are
rmdmoodle::get_exercise_root()


# Create an exam from exercises
rmdmoodle::rmdexam(6,
        "some-exam.Rmd",
        c("cap3/c3-estimativa.Rmd", "alinea01", "alinea04"),
        c("cap4/c4-anova-1fator-com-dados.Rmd", "id01", "concept01") )

#rmdmoodle


#TODO: futuramente será 
#library(rmdmoodle)

library(devtools)


# Pode ser preciso:
#.rs.restartR()


# Evita a instalação e library(rmdmoodle):
# With reset=TRUE, unload and reload the package for a clean start
load_all("C:/Users/pedrocruz/Documents/GitHub/rmdmoodle", reset=TRUE)

xmlmoodle("exam01")

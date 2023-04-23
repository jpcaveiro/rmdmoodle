# Testes para converter exercício em rmd moodle.


setwd("~/GitHub/bioestatistica/bream/0exercicios/Adelaide/cap3-acacias")

#source("C:/Users/pedrocruz/Documents/GitHub/rmdmoodle/R/parse_exerc.R")

source("parse_exerc.R")
parse_exer("c3-estimativa.Rmd")







# use of readLines and seq_along
rmd <- readLines("c3-estimativa.Rmd")
for (i in seq_along(rmd)) {
  line <- rmd[i]
  print(line)
}


# Install the 'enumerate' package if it's not already installed
if (!require(enumerate)) {
  install.packages("enumerate")
}
# Create a vector
my_vector <- c("apple", "banana", "cherry")

# Create an enumeration of the vector
my_enumeration <- enumerate(my_vector)

# Print the enumeration
print(my_enumeration)

# Output:
# 1  apple
# 2  banana
# 3  cherry



# return pair
f <- function(x) {
  return(list("x"=x,"2x"=2*x))
}
f(5)
f(5)$`2x`



#split string
r <- strsplit(" VARCOUNT     <-  3   ", " ")
nchar(r[[1]])
r[[1]][nchar(r[[1]])!=0]

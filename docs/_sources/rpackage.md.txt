

# build package rmdmoodle


## Learn how to create a package

* https://r-pkgs.org/
* https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
* https://www.cytel.com/blog/creating-r-package-template-guide
* RTools 42: [para que serve?](https://cran.rstudio.com/bin/windows/Rtools/rtools42/rtools.html)
* exemplo de package aceite no Journal of Statistical Software: https://github.com/ozancinar/poolr
* [roxygen2](https://roxygen2.r-lib.org/): para gerar documentação com base em linhas junto das funções em R
* python, if needed, inside R with `reticulate <http://cran.nexr.com/web/packages/reticulate/vignettes/introduction.html>`_ 



## starting


* "devtools": development tools to do the work easily.
* "testthat": for unit-testing.
* "rmarkdown": documentation language to create vignettes with code chunks.
* "roxygen2": permits function auto-documentation.


```r
install.packages("devtools")
library("devtools")
install.packages("roxygen2")
library(roxygen2)

create("rmdmoodle")
```



## Function documentation

Documenting:

```r
#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

cat_function <- function(love=TRUE){
    if(love==TRUE){
        print("I love cats!")
    }
    else {
        print("I am not a cool person.")
    }
}
```


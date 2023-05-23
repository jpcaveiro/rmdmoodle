[homepage in construction](https://jpcaveiro.github.io/rmdmoodle/)

# rmdmoodle

rmdmoodle

# develop

**docker**

Testar com:

- Abrir docker no Windows
- Abrir WSL2 ubuntu
- Correr os comandos seguintes:


1. `docker run -d -p 8787:8787 rocker/rstudio`
   - correr e instalar o devtools
2. docker commit sharp_brown rocker-rstudio-devtools:latest
   - gravar o rstudio já com devtools instalado 
3. docker run -v ~/GitHub/WorkPackages/2020-rmdmoodle/:/home/rstudio -e PASSWORD=nobrecaminho -d -p 8787:8787 rocker-rstudio-devtools
   - correr uma versão para testar instalação.
   - podem ser executados outros containers com testes


 

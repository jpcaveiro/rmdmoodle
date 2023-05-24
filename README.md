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
2. `docker commit sharp_brown rocker-rstudio-devtools:latest`
   - gravar o rstudio já com devtools instalado 
3. `docker run -v ~/GitHub/WorkPackages/2020-rmdmoodle/:/home/rstudio -e PASSWORD=nobrecaminho -d -p 8787:8787 rocker-rstudio-devtools`
   - correr uma versão para testar instalação.
   - podem ser executados outros containers com testes
   - abrir `http://localhost:8787/` com user: rstudio e pass: nobrecaminho


**docker: management**

```
$ docker ps
CONTAINER ID   IMAGE                     COMMAND   CREATED          STATUS          PORTS                    NAMES
a27181eb032a   rocker-rstudio-devtools   "/init"   30 minutes ago   Up 30 minutes   0.0.0.0:8787->8787/tcp   youthful_austin
$ docker stop youthful_austin
youthful_austin
$ docker ps
CONTAINER ID   IMAGE     COMMAND   CREATED   STATUS    PORTS     NAMES
$
```

 

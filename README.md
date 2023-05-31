[homepage in construction](https://jpcaveiro.github.io/rmdmoodle/)

# rmdmoodle

rmdmoodle

# develop

**docker**

Testar com:

- **Abrir docker no Windows** - não é intuitivo que para usar docker no WSL2 seja preciso primeiro abrir no Windows.
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

**testar com outras versões do R**

Ver exemplo, a dockerfile para o [R 4.1.2](https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/rstudio_4.1.2.Dockerfile).

* Guardar a dockerfile acima especificada num ficheiro de texto `rocker_rstudio_412`, por exemplo, e executar:
 
```
$ docker build . -f ./rocker_rstudio_412 -t rocker-rstudio-412
```
que produz uma imagem com nome `rocker-rstudio-412`.


 

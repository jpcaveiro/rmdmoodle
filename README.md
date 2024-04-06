
# homepage

* [homepage in construction](https://jpcaveiro.github.io/rmdmoodle/)
* gitpages https://quarto.org/docs/publishing/github-pages.html


# rmdmoodle

rmdmoodle

# develop


## With R in Windows for developing

* You can hold down the Control key during the launch of RStudio you can cause the R version selection dialog to display at startup. Fonte: https://support.rstudio.com/hc/en-us/articles/200486138-Using-Different-Versions-of-R

(por fazer)


```r
install.packages("devtools")
library(devtools)

# Pode ser preciso:
#.rs.restartR()

# Evita a instalação e library(rmdmoodle):
# With reset=TRUE, unload and reload the package for a clean start
# It could necessary to install rvest package
load_all("C:/Users/pedrocruz/Documents/GitHub/rmdmoodle", reset=TRUE)
```



## com docker

Testar com:

- **Abrir docker no Windows** - não é intuitivo que para usar docker no WSL2 seja preciso primeiro abrir no Windows.
- Abrir WSL2 ubuntu

Porque é muito lento instalar o `devtools` pode valer a pena fazer um `commit`, ou seja, instalar e gravar uma nova `image` do docker rstudio já com o `devtools` a funcionar:
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

*testar com outras versões do R - docker**

- Não funcionou com a dockerfile tal como está no link devido a um conflito/erro/omissão no sistema operativo base. Há que rever esta solução:

Ver exemplo, a dockerfile para o [R 4.1.2](https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/rstudio_4.1.2.Dockerfile).

* Guardar a dockerfile acima especificada num ficheiro de texto `rocker_rstudio_412`, por exemplo, e executar:
 
```
$ docker build . -f ./rocker_rstudio_412 -t rocker-rstudio-412
```
que produz uma imagem com nome `rocker-rstudio-412`.

**Outras operações**

Abrir uma consola de um container em curso:

```
docker exec -it <CONTAINER-name> bash
```
 


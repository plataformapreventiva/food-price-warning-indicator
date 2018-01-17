########################################
## Instalación y preparación de Ambiente
########################################

# Corre la siguiente función para instalar los paquetes usados en este repositorio
instalar <- function(paquete) {
  
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

paquetes <- c("tidyverse")


lapply(paquetes, instalar);



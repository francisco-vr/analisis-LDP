## Análisis de datos de La Lista del Pueblo ##

#Carga de paquetes

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("tidyverse","dplyr","haven","ggplot2","readxl","summarytools", "patchwork", "sf")
ipak(packages)

ldp <-read.csv(file = "Data Frames/votacion_comuna.csv")

### Votos por Comuna, agrupados por distrito ##


#D12
Pint <-filter(ldp, comuna == "LA PINTANA")
aggregate(Pint$votos, by=list(Pint$pacto), FUN=sum)

Flor <-filter(ldp, comuna == "LA FLORIDA")
aggregate(Flor$votos, by=list(Flor$pacto), FUN=sum)

PteAlt <-filter(ldp, comuna == "PUENTE ALTO")
aggregate(PteAlt$votos, by=list(PteAlt$pacto), FUN=sum)

Pirque <-filter(ldp, comuna == "PIRQUE")
aggregate(Pirque$votos, by=list(Pirque$pacto), FUN=sum)

SnJos <-filter(ldp, comuna == "SAN JOSE DE MAIPO")
aggregate(SnJos$votos, by=list(SnJos$pacto), FUN=sum)

table(ldp$comuna)

## tareas

# ver forma de crear variables de distritos a partir de las comunas
# Crear mapas con los números de aprobación de LDP x comuna



votoPact <-ctable(ldp$pacto, ldp$votos, prop = "c",style = 'rmarkdown',


BLEH <-ldp%>%
  group_by(ldp$comuna)

aggregate(BLEH$votos, by=list(category=BLEH$pacto), FUN=sum)


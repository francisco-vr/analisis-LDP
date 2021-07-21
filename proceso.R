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

lista <-read.csv(file = "Data Frames/votacion_comuna.csv")


# Crear distritos por comuna

lista <-lista %>%
  mutate(distrito = case_when(comuna %in% c("ARICA", "CAMARONES", "GENERAL LAGOS", "PUTRE") ~ 'D1',
                              comuna %in% c("ALTO HOSPICIO", "CAMINA", "COLCHANE", "HUARA", "IQUIQUE",
                                          "PICA","POZO ALMONTE") ~ 'D2',
                              comuna %in% c("ANTOFAGASTA","CALAMA","MARIA ELENA","MEJILLONES","OLLAGUE",
                                          "SAN PEDRO DE ATACAMA","SIERRA GORDA","TALTAL","TOCOPILLA") ~ 'D3',
                              comuna %in% c("ALTO DEL CARMEN", "CALDERA", "CHANARAL", "COPIAPO","DIEGO DE ALMAGRO",
                                          "FREIRINA","HUASCO","TIERRA AMARILLA","VALLENAR") ~ 'D4',
                              comuna %in% c("ANDACOLLO","CANELA","COMBARBALA","COQUIMBO","ILLAPEL","LA HIGUERA",
                                          "LA SERENA","LOS VILOS","MONTE PATRIA","OVALLE","PAIHUANO","PUNITAQUI",
                                          "RIO HURTADO","SALAMANCA","VICUNA") ~ 'D5',
                              comuna %in% c("CABILDO","CALERA","CALLE LARGA","CATEMU","HIJUELAS","LA CRUZ","LA LIGUA",
                                          "LIMACHE","LLAILLAY","LOS ANDES","NOGALES","OLMUE","PANQUEHUE","PAPUDO",
                                          "PETORCA","PUCHUNCAVI","PUTAENDO","QUILLOTA","QUILPUE","QUINTERO","RINCONADA",
                                          "SAN ESTEBAN","SAN FELIPE","SANTA MARIA","VILLA ALEMANA","ZAPALLAR") ~ 'D6',
                              comuna %in% c("ALGARROBO","CARTAGENA","CONCON","EL QUISCO","EL TABO","CASABLANCA",
                                          "ISLA DE PASCUA","JUAN FERNANDEZ","SAN ANTONIO","SANTO DOMINGO",
                                          "VALPARAISO","VINA DEL MAR") ~ 'D7',
                              comuna %in% c("CERRILLOS","COLINA","ESTACION CENTRAL","LAMPA","MAIPU","PUDAHUEL","TILTIL",
                                          "QUILICURA") ~ 'D8',
                              comuna %in% c("CERRO NAVIA","CONCHALI","HUECHURABA","INDEPENDENCIA","LO PRADO",
                                          "QUINTA NORMAL","RECOLETA","RENCA") ~ 'D9',
                              comuna %in% c("LA GRANJA","MACUL","NUNOA","PROVIDENCIA","SAN JOAQUIN","SANTIAGO") ~ 'D10',
                              comuna %in% c("LA REINA","LAS CONDES","LO BARNECHEA","PENALOLEN","VITACURA") ~ 'D11',
                              comuna %in% c("LA FLORIDA","LA PINTANA","PIRQUE","PUENTE ALTO","SAN JOSE DE MAIPO") ~ 'D12',
                              comuna %in% c("EL BOSQUE","LA CISTERNA","LO ESPEJO","PEDRO AGUIRRE CERDA","SAN MIGUEL",
                                          "SAN RAMON") ~ 'D13',
                              comuna %in% c("ALHUE","BUIN","CALERA DE TANGO","CURACAVI","EL MONTE","ISLA DE MAIPO",
                                          "MARIA PINTO","MELIPILLA","PADRE HURTADO","PAINE","PENAFLOR","SAN BERNARDO",
                                          "SAN PEDRO","TALAGANTE") ~ 'D14',
                              comuna %in% c("CODEGUA","COINCO","COLTAUCO","DONIHUE","GRANEROS","MACHALI","MALLOA",
                                          "MOSTAZAL","OLIVAR","QUINTA DE TILCOCO","RANCAGUA","RENGO","REQUINOA") ~ 'D15',
                              comuna %in% c("CHEPICA","CHIMBARONGO","LA ESTRELLA","LAS CABRAS","LITUECHE","LOLOL",
                                          "MARCHIGUE","NANCAGUA","NAVIDAD","PALMILLA","PAREDONES","PERALILLO","PEUMO",
                                          "PICHIDEGUA","PICHILEMU","PLACILLA","PUMANQUE","SAN FERNANDO","SAN VICENTE",
                                          "SANTA CRUZ") ~ 'D16',
                              comuna %in% c("CONSTITUCION","CUREPTO","CURICO","EMPEDRADO","HUALANE","LICANTEN","MAULE",
                                          "MOLINA","PELARCO","PENCAHUE","RAUCO","RIO CLARO","ROMERAL",
                                          "SAGRADA FAMILIA","SAN CLEMENTE","SAN RAFAEL","TALCA",
                                          "TENO","VICHUQUEN") ~ 'D17',
                              comuna %in% c("CAUQUENES","CHANCO","COLBUN","LINARES","LONGAVI","PARRAL","PELLUHUE",
                                          "RETIRO","SAN JAVIER","VILLA ALEGRE","YERBAS BUENAS") ~ 'D18',
                              comuna %in% c("BULNES","CHILLAN","CHILLAN VIEJO","COBQUECURA","COELEMU","COIHUECO",
                                          "EL CARMEN","NINHUE","NIQUEN","PEMUCO","PINTO","PORTEZUELO","QUILLON",
                                          "QUIRIHUE","RANQUIL","SAN CARLOS","SAN FABIAN","SAN IGNACIO","SAN NICOLAS",
                                          "TREHUACO","YUNGAY") ~ 'D19',
                              comuna %in% c("CHIGUAYANTE","CONCEPCION","CORONEL","FLORIDA","HUALPEN","HUALQUI","PENCO",
                                          "SAN PEDRO DE LA PAZ","SANTA JUANA","TALCAHUANO","TOME") ~ 'D20',
                              comuna %in% c("ALTO BIOBIO","ANTUCO","ARAUCO","CABRERO","CANETE","CONTULMO","CURANILAHUE",
                                          "LAJA","LEBU","LOS ALAMOS","LOS ANGELES","LOTA","MULCHEN","NACIMIENTO",
                                          "NEGRETE","QUILACO","QUILLECO","SAN ROSENDO","SANTA BARBARA","TIRUA",
                                          "TUCAPEL","YUMBEL") ~ 'D21',
                              comuna %in% c("ANGOL","COLLIPULLI","CURACAUTIN","ERCILLA","GALVARINO","LAUTARO","LONQUIMAY",
                                          "LOS SAUCES","LUMACO","MELIPEUCO","PERQUENCO","PUREN","RENAICO","TRAIGUEN",
                                          "VICTORIA","VILCUN") ~ 'D22',
                              comuna %in% c("CARAHUE","CHOLCHOL","CUNCO","CURARREHUE","FREIRE","GORBEA","LONCOCHE",
                                          "NUEVA IMPERIAL","PADRE LAS CASAS","PITRUFQUEN","PUCON","SAAVEDRA",
                                          "TEMUCO","TEODORO SCHMIDT","TOLTEN","VILLARRICA") ~ 'D23',
                              comuna %in% c("CORRAL","FUTRONO","LA UNION","LAGO RANCO","LANCO","LOS LAGOS","MAFIL",
                                          "MARIQUINA","PAILLACO","PANGUIPULLI","RIO BUENO","VALDIVIA") ~ 'D24',
                              comuna %in% c("FRESIA","FRUTILLAR","LLANQUIHUE","LOS MUERMOS","OSORNO","PUERTO OCTAY",
                                          "PUERTO VARAS","PURRANQUE","PUYEHUE","RIO NEGRO","SAN JUAN DE LA COSTA",
                                          "SAN PABLO") ~ 'D25',
                              comuna %in% c("ANCUD","CALBUCO","CASTRO","CHAITEN","CHONCHI","COCHAMO","CURACO DE VELEZ",
                                          "DALCAHUE","FUTALEUFU","HUALAIHUE","MAULLIN","PALENA","PUERTO MONTT",
                                          "PUQUELDON","QUEILEN","QUELLON","QUEMCHI","QUINCHAO") ~ 'D26',
                              comuna %in% c("AYSEN","CHILE CHICO","CISNES","COCHRANE","COYHAIQUE","GUAITECAS",
                                          "LAGO VERDE","O'HIGGINS","RIO IBANEZ","TORTEL") ~ 'D27',
                              comuna %in% c("CABO DE HORNOS(EX-NAVARINO) Y ANTARTICA","LAGUNA BLANCA","NATALES",
                                          "PORVENIR","PRIMAVERA","PUNTA ARENAS","RIO VERDE","SAN GREGORIO",
                                          "TIMAUKEL","TORRES DEL PAINE") ~ 'D28'))
# Ingreso de regiones

lista <-lista %>%
  mutate(region = case_when(distrito == 'D1' ~ 'Arica y Parinacota',
                            distrito == 'D2' ~ 'Tarapaca',
                            distrito == 'D3' ~ 'Antofagasta',
                            distrito == 'D4' ~ 'Atacama',
                            distrito == 'D5' ~ 'Coquimbo',
                            distrito %in% c("D6", "D7") ~ 'Valparaiso',
                            distrito %in% c("D8","D9","D10","D11","D12","D13","D14") ~ 'Metropolitana',
                            distrito %in% c("D15", "D16") ~ 'OHiggins',
                            distrito %in% c("D17", "D18") ~ 'Maule',
                            distrito %in% c("D19","D20","D21") ~ 'BioBio',
                            distrito %in% c("D22", "D23") ~ 'Araucania',
                            distrito == 'D24' ~ 'Los Rios',
                            distrito %in% c("D25","D26") ~ 'Los Lagos',
                            distrito == 'D27' ~ 'Aisen',
                            distrito == 'D28' ~ 'Magallanes'))

### Votos por Comuna, agrupados por distrito ##

aggregate(votos~pacto, lista, sum)

## creación de nuevas bases de datos, ya que sólo el pacto LDP está normalizado

## coreccion base de datos (problema la serena)

listaCor <-read.csv(file = "voto_total_const_comuna.csv")
aggregate(votos~pacto, listaCor, sum)


##base completa

write_csv(lista,file = "voto_total_const_comuna.csv")


# Base filtrada de LDP estandarizada

LLDP <-listaCor%>%
  filter(pacto == 'Del pueblo')%>%
  select(candidato,votos,electo,lista,comuna,pacto,distrito,region)%>%
  write_csv(file = "votos_LDP_comuna.csv")

table(LLDP$comuna)

# Base agregada con la suma por lista

LDPSum <-aggregate(votos~pacto+comuna, listaCor, sum)%>%
  write_csv("votos_LDP_sumados_comuna.csv")




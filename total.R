library(tidyverse)
library(stringi)
library(readxl)
library(readr)
library(tmaptools)
library(ggmap)
library(rworldmap)
library(sf)
library(rworldxtra)
library(tmap)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
adiv_encod<- function(file) {
  prueba<-file %>% 
    guess_encoding(n_max = 3)
  tryCatch(
    readLines(file)[1] %>% 
      iconv(from=prueba$encoding[1],to="UTF-8") ,
    expr = {
      return(prueba$encoding[1])
    },
    error = function(e) {
      message(paste("Error in file:", files[which(!str_detect(files, "Tabla"))][1]))
      if (length(prueba$encoding) > 1) {
        return(prueba$encoding[2]) # Return the second encoding if available
      } else {
        return("UTF-8") # Default if no second encoding
      }
    }
  )
}
columnerror<-function(file, encod, skip=3){
  tryCatch(
    expr = {
      read.csv(file, fileEncoding = encod, encoding = "UTF-8", skip=skip, na.strings = c("NA", "N/A", ""))
      return(TRUE) # If the file is read successfully, return TRUE
    },
    error = function(e) {
      message(paste("Error reading file:", file))
      return(FALSE)
    }
  )
}
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
buscar<-function(x){
  register_google(key = "AIzaSyCzSvZS-R9aVkjyl4Q-EqG9ShP7DYLFc5U")
  y<-geocode(as.character(x$loc))
  y<-as.data.frame(y)
  return(y)
}
buscar2<-function(x){
  y<-subset(x[which(!is.na(x$loc)),], select=c("loc"))
  y<-unique(y$loc)
  y<-as.data.frame(y)
  colnames(y)<-"loc"
  z<-buscar(y)
  y$lon<-z$lon
  y$lat<-z$lat
  return(y)
}


####1. data####
#####1.0 loading #####
umas<-read_csv("../Rsecihti/umas.csv")
sni<-read_csv("../Rsecihti/sni1.csv")
snina<-sni %>% filter(is.na(nivel))
snina$anos<-as.numeric(str_extract(snina$fin, "\\d{4}$"))- as.numeric(str_extract(snina$inicio, "\\d{4}$"))
snina$nivel<-ifelse(snina$anos<=4, "C", NA)
sni<- rbind(
  sni %>% subset(!is.na(nivel)), snina %>% subset(!is.na(nivel), select=-anos)
)
sni$nivel<-ifelse(str_detect(sni$nivel, "^E"), "E", sni$nivel)
colnames(umas)<-c("year", "uma")
sni<-left_join(sni, umas, by="year")
sni$estimulo<-ifelse(sni$nivel=="C", 3*sni$uma, 
                     ifelse(sni$nivel=="1", 6*sni$uma, 
                            ifelse(sni$nivel=="2", 8*sni$uma, 
                                   ifelse(str_detect(sni$nivel, "3|E"), 14*sni$uma, NA))))
sni$simp2<-paste0(sni$apell_stand%>% trimws(which = "both"), 
                  ", ", sni$nom_stand%>% trimws(which = "both")) %>% tolower() %>%
  str_replace_all("[^[:alpha:],[']]", " ") %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "")
pnt<-read_csv("tablapnt.csv")
pnt<-pnt %>% subset(!str_detect(universidad, "unam|universidad\\snacional\\sautonoma")) %>%
  rename(apellido1=primerapellido, apellido2=segundoapellido) %>%
  mutate(apellido1=stri_trans_general(apellido1, id="Latin-ASCII")%>% tolower(), 
         apellido2=stri_trans_general(apellido2, id="Latin-ASCII")%>% tolower(), 
         nombre=stri_trans_general(nombre, id="Latin-ASCII")%>% tolower(), 
         denominacion=stri_trans_general(denominacion, id="Latin-ASCII")%>% tolower(), 
         universidad=stri_trans_general(universidad, id="Latin-ASCII")%>% tolower())
unam<-read_csv("tablaunam.csv")
unam<-unam %>% 
  mutate(apellido1=stri_trans_general(apellido1, id="Latin-ASCII")%>% tolower(), 
         apellido2=stri_trans_general(apellido2, id="Latin-ASCII")%>% tolower(), 
         nombre=stri_trans_general(nombre, id="Latin-ASCII")%>% tolower(), 
         denominacion=stri_trans_general(denominacion, id="Latin-ASCII")%>% tolower(), 
         universidad=stri_trans_general(universidad, id="Latin-ASCII")%>% tolower())
universidades<-rbind(pnt, unam)
rm(pnt, unam)

#####1.1 incompatible info in universities #####
######1.1.1 dealing with gender######
universidades$sexo<-ifelse(str_detect(universidades$sexo, "ombre"), "masculino", 
                           ifelse(str_detect(universidades$sexo, "ujer"), "femenino", NA)) %>% 
  str_to_lower()
universidades<- universidades %>% group_by(apellido1, apellido2, nombre) %>%
  fill(sexo, .direction="downup") %>%
  mutate(sexo=mode(sexo))
genero0<-universidades %>% subset(!is.na(sexo)&!is.na(nombre), select=c(apellido1, apellido2, nombre, sexo)) %>% distinct()
genero1<- genero0 %>% subset(select=c(nombre, sexo)) %>% count(nombre, sexo)
genero2<- genero1 %>% mutate(nombre=str_extract(nombre, "^\\w+")) %>% 
  subset(nchar(nombre)>2) %>% group_by(nombre, sexo)%>%summarize(n=sum(n), .groups="keep")
genero3<- genero1 %>% mutate(nombre=str_extract(nombre, "(?<=\\s)\\w+$")) %>% 
  subset(!is.na(nombre)&nchar(nombre)>2) %>% group_by(nombre, sexo)%>%summarize(n=sum(n), .groups="keep")
genero1<- genero1 %>%
  pivot_wider(names_from=sexo, values_from=n, values_fill=0) %>%
  mutate(femprob=femenino/(femenino+masculino), mascprob=masculino/(femenino+masculino)) %>%
  mutate(fem=ifelse(femprob>mascprob, "femenino", 
                    ifelse(femprob<mascprob, "masculino", NA)), 
         prob=ifelse(femprob>mascprob, femprob, 
                     ifelse(femprob<mascprob, mascprob, NA)))%>%
  subset(prob>0.7, select=c(nombre, fem, prob))
genero2<-genero2 %>% 
  pivot_wider(names_from=sexo, values_from=n, values_fill=0) %>%
  mutate(femprob=femenino/(femenino+masculino), mascprob=masculino/(femenino+masculino)) %>%
  mutate(fem=ifelse(femprob>mascprob, "femenino", 
                    ifelse(femprob<mascprob, "masculino", NA)), 
         prob=ifelse(femprob>mascprob, femprob, 
                     ifelse(femprob<mascprob, mascprob, NA))) %>%
  subset(prob>0.7, select=c(nombre, fem, prob))
genero3<-genero3 %>% 
  pivot_wider(names_from=sexo, values_from=n, values_fill=0) %>%
  mutate(femprob=femenino/(femenino+masculino), mascprob=masculino/(femenino+masculino)) %>%
  mutate(fem=ifelse(femprob>mascprob, "femenino", 
                    ifelse(femprob<mascprob, "masculino", NA)), 
         prob=ifelse(femprob>mascprob, femprob, 
                     ifelse(femprob<mascprob, mascprob, NA))) %>%
    subset(prob>0.85, select=c(nombre, fem, prob)) 

gen0dum<-genero0 %>% subset(select=c("nombre", "sexo")) %>% mutate(prob=1) %>% rename(fem=sexo)
gendum<-rbind(genero1, genero2, genero3)
mean(gendum$prob)

colnames(genero0)<-c("apellido1", "apellido2", "nombre", "fem")
genero0$prob<-1
colnames(genero1)<-c("nombre", "fem", "prob")
colnames(genero2)<-c("nombre1", "fem", "prob")
colnames(genero3)<-c("nombre2", "fem", "prob")


universidades$nombre1<-str_extract(universidades$nombre, "^\\w+")
universidades$nombre2<-str_extract(universidades$nombre, "(?<=\\s)\\w+")
universidades<-universidades %>% 
  left_join(genero0) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-fem) %>%
  left_join(genero1) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-fem) %>%
  left_join(genero2) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-fem) %>%
  left_join(genero3) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-c(fem, nombre1, nombre2))
length(unique(universidades$nombre[which(is.na(universidades$sexo))]))
######1.1.1 dealing with institutions######
adscripciones<-universidades %>%
  group_by(ejercicio, nombre, apellido1, apellido2, universidad) %>%
  mutate(maximo=max(monto_total)) %>% ungroup() %>%
  subset(monto_total==maximo, select=-c(maximo, monto_total, sexo, estimulos)) %>% 
  distinct() %>%
  rename(adscripcion=universidad)
universidades<-left_join(universidades, adscripciones, multiple="first")
universidades$universidad<-NULL
universidades$apellidos<-paste(universidades$apellido1, universidades$apellido2, sep=" ") %>% tolower()
universidades$nombres<-universidades$nombre%>% tolower()
totalesu<-universidades %>% count(ejercicio, adscripcion) %>% subset(!is.na(adscripcion))

#####1.2 incompatible info for secihti#####
secihti<-read_csv("../Rsecihti/secihti3.csv")
secihti$ejercicio<-secihti$year
secihti$apellidos<-tolower(secihti$apellidos)
secihti$nombres<-tolower(secihti$nombres)

######1.2.1 dealing with gender ######

secihti$nombre1<-str_extract(secihti$nombres, "^\\w+")
secihti$nombre2<-str_extract(secihti$nombres, "^\\w+")
secihti$sexo<-NA
secihti<-secihti %>% 
  left_join(genero0) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-c(fem, prob)) %>%
  left_join(genero1) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-c(fem, prob)) %>%
  left_join(genero2) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-c(fem, prob)) %>%
  left_join(genero3) %>% mutate(sexo=ifelse(is.na(sexo), fem, sexo)) %>%  select(-c(fem, nombre1, nombre2))
secihti$prob<-ifelse(is.na(secihti$prob)&!is.na(secihti$sexo), 1, secihti$prob)
secihti$prob<-NULL
length(unique(secihti$nombres[which(is.na(secihti$sexo))]))
length(unique(secihti$nombres))
######1.2.2 dealing with rest of information ######
secihti$monto<-ifelse(secihti$yfin==secihti$year, abs(secihti$total)/(secihti$mfin), 
                           ifelse(secihti$yinicio==secihti$year, abs(secihti$total)/(13-secihti$minicio), 
                                  abs(secihti$total)/12))
secihti$nivel<-ifelse(str_detect(secihti$nivel, "DOC$"), "DOCTORADO", 
                      ifelse(str_detect(secihti$nivel, "MAESTRIA$|MAE$"), "MAESTRIA", 
                             ifelse(str_detect(secihti$nivel, "ESPECIALI$|ESP$"), "ESPECIALIDAD", 
                                    ifelse(str_detect(secihti$nivel, "\\sESTANCIA|EST$|EST\\sTEC"), "ESTANCIA TECNICA", 
                                           ifelse(str_detect(secihti$convocatoria, "catedr"), "CATEDRA",
                                                  ifelse(str_detect(secihti$tipo, "repatria"), "REPATRIACION", 
                                                         secihti$nivel)
                                                  )
                                           )
                                    )
                             )
                      )
secihti$tipo<-ifelse(secihti$nivel=="CATEDRA", "CATEDRA", secihti$tipo)
montos<-secihti %>% subset(!is.na(monto)&!is.na(nivel)) %>%
  group_by(year, nivel, tipo) %>% summarize(monto_est=median(abs(monto)), .groups = "keep")
nas<-secihti %>% subset(is.na(nivel)) %>% 
  subset(select=-c(nivel, tipo)) 
nacio<-c("MEXICO", "N/A", "PUEBLA", "CHIAPAS", "AUSTRALIA", "UAM", "QUERETARO", "NUEVO LEON", "ZACATECAS", "MICHOACAN", "SINALOA", 
         "JALISCO", "SONORA", "GUANAJUATO", "UNAM", "ZITACUARO", "OAXACA", "SONORA", "SAN LUIS POTOSI", "COLPOS", "YUCATAN", 
         "ECOSUR", "GUANAJUATO", "MORELOS", "GUADALAJARA", "GUERRERO", "OAXACA", "TAMAULIPAS", "BAJA CALIFORNIA", "IPN") %>% 
  paste(collapse="|")
nas$tipo<-ifelse(str_detect(nas$pais_entidad, nacio), "nacional", 
                 ifelse(!is.na(nas$pais_entidad), "extranjero", NA))
nas2<-nas  %>% left_join(montos, relationship = "many-to-many") %>%
  mutate(diff=abs(abs(monto)-monto_est)) %>% group_by(year, simpname, tipo) %>%
  mutate(dummy=ifelse(diff==min(diff), 1, 0)) %>% subset(dummy==1, select=-c(diff, dummy, monto_est))
secihti<-secihti %>% subset(!is.na(nivel)) %>% rbind(nas2)

extras<-secihti %>% subset(str_detect(nivel, "ESTANC")|str_detect(tolower(tipo), "extra|repat|cate")) %>%
  mutate(monto_neto=monto) %>% select(-c(monto))
nacional<-secihti  %>% subset(!str_detect(nivel, "ESTANC")&!str_detect(tolower(tipo), "extra|repat|cate")) %>% 
  left_join(montos) %>% mutate(rdif=100*abs(abs(monto)-monto_est)/monto_est, diff=abs(abs(monto)-monto_est)) %>%
  mutate(monto_neto=monto_est)%>% select(-c(monto, monto_est, rdif, diff))

secihti<-rbind(extras, nacional) %>%
  mutate(monto_neto=ifelse(is.na(monto_neto), 0, monto_neto))
totales<-secihti %>% count(ejercicio, nivel) %>% subset(!is.na(nivel))

##### 1.3 consolidating names#####
univers2<-universidades %>%
  mutate(tipo="universidad", 
         institucion=tolower(adscripcion)) %>% 
  select(ejercicio, apellidos, nombres, monto_neto, estimulos,institucion, denominacion, sexo) %>% distinct()
secihti2<-secihti  %>% subset(!is.na(apellidos)) %>% 
  select(ejercicio, apellidos, nombres, sexo, monto_neto, areasni, institucion, nivel, programa) %>% distinct() %>%
  mutate(institucion=tolower(institucion), estimulos=NA,
         areasni=tolower(areasni)) %>%
  rename(denominacion=programa)

sni2<-sni %>% select(year, apell_stand, nom_stand, simp2, cvu, nivel, areasni, institucion, estimulo) %>%
  distinct() %>% mutate(simpname=simp2, institucion=tolower(institucion) %>% str_replace_all("[^[:alpha:]]", " ") %>%
                          str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", ""),
                        areasni=tolower(areasni) %>% str_replace_all("[^[:alpha:]]", " ") %>% 
                          str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", ""),
                        nivel=tolower(nivel)) %>%
  mutate(apellidos=str_to_lower(apell_stand), nombres=str_to_lower(nom_stand)) %>% select(-c(apell_stand, nom_stand))

secihti2$simpname<-paste(secihti2$apellidos, secihti2$nombres, sep=", ") %>% 
  str_replace_all("[^[:alpha:],[']]", " ") %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "")
univers2$simpname<-paste(univers2$apellidos, univers2$nombres, sep=", ") %>% 
  str_replace_all("[^[:alpha:],[']]", " ") %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "")
secihti2$institucion<-secihti2$institucion %>% str_replace_all("[^[:alpha:]]", " ") %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "") %>% tolower()
univers2$institucion<-univers2$institucion %>% str_replace_all("[^[:alpha:]]", " ") %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "") %>% tolower()

nombres<-secihti2 %>% subset(select=simpname) %>% 
  rbind(univers2 %>% subset(select=simpname), sni2 %>% subset(select=simpname)) %>%
  arrange(simpname)
instituciones<-secihti2 %>% subset(select=institucion) %>% 
  rbind(univers2 %>% subset(select=institucion), sni2 %>% subset(select=institucion)) %>%
  subset(!is.na(institucion)&!str_detect(institucion, "^\\s*$")) %>%
  arrange(institucion) %>%distinct()
write_csv(nombres, "nombrestotales.csv")
write_csv(instituciones, "institucionestotales.csv")
nombres2<-read_csv("nombrestotales2.csv") %>% distinct()
nombres3<-nombres %>% distinct() %>% left_join(nombres2, by="simpname") 
nombres3$simp2<-ifelse(is.na(nombres3$simp2), nombres3$simpname, nombres3$simp2)
nombres3$simp2<-str_replace_all(nombres3$simp2, "\\sna(?=[,])", "")
instituciones2<-read_csv("institucionestotales2.csv")%>%
  subset(!is.na(institucion)&!str_detect(institucion, "^\\s*$"))
instituciones3<-instituciones %>% distinct() %>% left_join(instituciones2, by="institucion") %>%
  mutate(insti2=ifelse(is.na(insti2), institucion, insti2))
#####1.4 changing to normalized names after the processing in open refine#####
nombres3 <-nombres3 %>% distinct()
instituciones3<-instituciones3 %>% distinct()
sni3<-left_join(sni2, nombres3) %>% left_join(instituciones) %>%
  subset(!is.na(simp2)) %>% distinct() %>% select(year, simp2, areasni, nivel, cvu, estimulo) %>%
  rename(ejercicio=year, nivelsni=nivel, estsni=estimulo)
secihti3<-left_join(secihti2, nombres3) %>% left_join(instituciones) %>%
  subset(!is.na(simp2)) %>% distinct() %>% left_join(sni3%>%select(-areasni))
univers3<-left_join(univers2, nombres3) %>% left_join(instituciones) %>%
  mutate(nivel=NA) %>%
  subset(!is.na(simp2)) %>% distinct() %>% left_join(sni3)

##### 1.5 changing to official names######
oficiales<-univers3 %>% subset(select=c(simp2, simpname)) %>% distinct() %>% rename(simp3=simpname) %>% arrange(simp2)
oficiales$dum<-ifelse((oficiales$simp2==lag(oficiales$simp2)&oficiales$simp3!=lag(oficiales$simp3))|
                        (oficiales$simp2==lead(oficiales$simp2)&oficiales$simp3!=lead(oficiales$simp3)), 1, 0)
ofidum<-oficiales %>% subset(dum==1) %>% select(simp2, simp3) %>%
  distinct() %>% arrange(simp2) %>%
  group_by(simp2) %>%
  nest()
for(i in 1:nrow(ofidum)){
  maxl<-max(nchar(ofidum$data[[i]]$simp3))
  minl<-min(nchar(ofidum$data[[i]]$simp3))
  k<-maxl-minl
  if(k==1){
    x<-ofidum$data[[i]]$simp3[which(nchar(ofidum$data[[i]]$simp3)==maxl)][1]
  } else{
    x<-ofidum$data[[i]]$simp3[which(nchar(ofidum$data[[i]]$simp3)==minl)][1]
  }
  ofidum$data[[i]]$simp3<-x
}
ofidum<-ofidum %>% unnest(cols=c(data)) %>% distinct()
oficiales<-oficiales %>% subset(is.na(dum)|dum!=1) %>% select(-dum) %>%
  rbind(ofidum) 

secihti4<-left_join(secihti3, oficiales, by="simp2") %>%
  mutate(simp2=ifelse(is.na(simp3), simp2, simp3)) %>% select(-simp3) %>%
  distinct() %>% subset(!is.na(simp2))
univers4<-left_join(univers3, oficiales, by="simp2") %>%
  mutate(simp2=ifelse(is.na(simp3), simp2, simp3)) %>% select(-simp3) %>%
  distinct() %>% subset(!is.na(simp2))
univers4$denominacion<-univers4$denominacion %>% stri_trans_general(id="Latin-ASCII") %>%
  str_replace_all("[^[:alpha:][:space:],]", "") %>% str_to_lower() %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "")

##### 1.6 disambiguation#####
homonimos<-read_csv("homonimos2.csv") %>% fill(id, .direction="down") %>%
  select(simp2, ejercicio, institucion, id, areasni, denominacion) %>% 
  rename(desam=id)
denom<-read_csv("denom.csv")
denom$denominacion<- denom$denominacion%>% stri_trans_general(id="Latin-ASCII") %>%
  str_replace_all("[^[:alpha:][:space:],]", "") %>% str_to_lower() %>%
  str_squish() %>% str_replace_all("(?<=\\s)\\s|^\\s+|\\s+$", "")
denom2<-read_csv("denom2.csv")
den<-c("\\bchef\\b", "\\bmant\\b", "^no\\s", "^apoyo")
nden<-c("escola", "estudian", "estudio","docen[c|t]", "medic[o|a]", "escola", "biblio","academic", 
         "^jef[e|a].*departa","^jef[e|a].*depto","^direc", "^subdire", "^prof", "^analista", "^ptc") %>%
  paste(collapse="|")
denom2<-c(den, denom2$denom2) %>% paste(collapse="|")
univers4$denom<-NULL
denoms2<-univers4 %>% 
  subset(str_detect(denominacion, denom2)&!str_detect(denominacion, nden), select=denominacion) %>% 
  distinct() %>% arrange(denominacion)
denoms2$denom<-0
denom<-rbind(denom, denoms2) %>% distinct()
secihti5<-left_join(secihti4, homonimos %>% select(-denominacion) %>% distinct()) %>%
  mutate(desam=ifelse(is.na(desam), 1, desam))
univers5<-left_join(univers4, homonimos%>% distinct()) %>%
  mutate(desam=ifelse(is.na(desam), 1, desam)) %>% 
  left_join(denom) %>%
  mutate(denom=ifelse(is.na(denom)&str_detect(denominacion, "vigilan|intenden|afanad"), 0, denom))
#####1.7 postions#####
posiciones<-c("\\bprof", "\\binv", "ptc", "docen", "academi", "cated", "tecnic", "secreta", "medico",
              "jefe.*departament", "jefe.*carrera", "facilitador.*educativo", "adminis", "coordinador",
              "^e$", "^it$", "^ip$", "^ia$", "^is$",
              "funcionario", "director",  "abogad", nden) %>% unique() %>%
  paste(collapse = "|")
status<-c("\\btit", "\\btemp", "\\basoc", "\\basist", "\\bayud", "\\bhonor", "\\bemeri", "\\basign", "\\bmaest") %>%
  paste(collapse = "|")
univers5$posicion<-str_extract(univers5$denominacion, posiciones)
univers5$status<-str_extract(univers5$denominacion, status)
secihti5$posicion<-"becari"
secihti5$status<-"becar"

#####1.8 locations#####
ubicaciones<-read_csv("ubicaciones.csv")
instituciones<-rbind(secihti5, univers5 %>% subset(select=-c(apellido1,apellido2, nombre, denom))) %>% 
  subset(!is.na(institucion), select=c(institucion)) %>% distinct() %>% arrange(institucion)
instituciones$loc<-instituciones$institucion
instituciones<-left_join(instituciones, ubicaciones)
univers6<-univers5 %>% left_join(instituciones)
secihti6<-secihti5 %>% left_join(instituciones)
######1.9 general dataset#####
total<-rbind(univers6 %>% select(-denom), secihti6 %>% mutate(apellido1=NA, apellido2=NA, nombre=NA)) %>% 
  arrange(simp2, desam, ejercicio) %>% subset(!is.na(ejercicio)) %>%
  mutate(
    areasni=ifelse(str_detect(areasni, "interd"), "ix. interdisciplinaria", areasni),
    espe=ifelse(str_detect(nivel %>% str_to_upper(), "ESPECIALIDAD"), 1, 0),
    mae=ifelse(str_detect(nivel %>% str_to_upper(), "MAESTRIA"), 1, 0),
    phd=ifelse(str_detect(nivel %>% str_to_upper(), "DOCTORADO"), 1, 0),
    pos=ifelse(str_detect(nivel %>% str_to_upper(), "POSDOCT"), 1, 0),
    rep=ifelse(str_detect(nivel %>% str_to_upper(), "REPATRIACION"), 1, 0),
    cate=ifelse(str_detect(nivel %>% str_to_upper(), "CATEDRA"), 1, 0),
    trab=ifelse(is.na(nivel), 1, 0), 
    snii=ifelse(!is.na(nivelsni), 1, 0)
  ) %>%
  mutate(espe=ifelse(is.na(espe), 0, espe),
         mae=ifelse(is.na(mae), 0, mae),
         phd=ifelse(is.na(phd), 0, phd),
         pos=ifelse(is.na(pos), 0, pos),
         rep=ifelse(is.na(rep), 0, rep),
         cate=ifelse(is.na(cate), 0, cate),
         snii=ifelse(is.na(snii), 0, snii),
         trab=ifelse(is.na(trab), 0, trab)) %>%
  fill(apellido1, apellido2, nombre, .direction="updown") 
inpc<-read_csv("../Rpnt/inpc.csv") %>% rename(ejercicio=year)
total<-left_join(total, inpc, by="ejercicio") %>%
  mutate(mensual_real=100*monto_neto/inpc, est_real=100*estimulos/inpc, sni_real=100*estsni/inpc)

#####1.7 intersections between secihti and univers#####
inpc<-read_csv("inpc.csv") %>% rename(ejercicio=year)
dum1<-univers6 %>% subset(is.na(denom)&!is.na(denominacion), select=c(simp2, desam)) %>% distinct() %>% mutate(dum=1)
dum2<- secihti6 %>% select(simp2,desam) %>% distinct() %>% mutate(dum=1)
seci<-left_join(secihti6, dum1) %>% subset(dum==1, select=-dum) %>% distinct() %>% 
  mutate(areasni=ifelse(is.na(areasni), "extranjero", areasni))
univ<-univers6 %>% subset(is.na(denom)&!is.na(denominacion)) %>% 
  left_join(dum2) %>% subset(dum==1, select=-c(dum,denom))%>% distinct()
intersections<-rbind(univ, seci) %>% arrange(simp2, desam, ejercicio) %>% 
  subset(!is.na(ejercicio)) %>% group_by(simp2, desam) %>% 
  fill(cvu, sexo, apellido1, apellido2, nombre, .direction="updown") %>% 
  fill(nivelsni, estsni, .direction="down")

intersections<-left_join(intersections, inpc, by="ejercicio") %>%
  mutate(mensual_real=100*monto_neto/inpc, est_real=100*estimulos/inpc, sni_real=100*estsni/inpc)
intersections<-intersections %>% group_by(simp2, desam) %>% fill(institucion, .direction="downup")
intersections$areasni<-ifelse(str_detect(intersections$areasni, "interd"), "ix. interdisciplinaria", intersections$areasni)
intersections$espe<-ifelse(str_detect(intersections$nivel%>% str_to_upper(), "ESPECIALIDAD"), 1, 0)
intersections$mae<-ifelse(str_detect(intersections$nivel%>% str_to_upper(), "MAESTRIA"), 1, 0)
intersections$phd<-ifelse(str_detect(intersections$nivel%>% str_to_upper(), "DOCTORADO"), 1, 0)
intersections$pos<-ifelse(str_detect(intersections$nivel%>% str_to_upper(), "POSDOCT"), 1, 0)
intersections$rep<-ifelse(str_detect(intersections$nivel%>% str_to_upper(), "REPATRIACION"), 1, 0)
intersections$cate<-ifelse(str_detect(intersections$nivel%>% str_to_upper(), "CATEDRA"), 1, 0)
intersections$simp2<-str_replace_all(intersections$simp2, "\\sna(?=[,])", "")
intersections2<- intersections %>% 
  group_by(across(-c(mensual_real, est_real, sni_real, monto_neto, estimulos, estsni))) %>%
  summarize(mensual_real=mean(mensual_real, na.rm=TRUE), 
            est_real=mean(est_real, na.rm=T),
            sni_real=mean(sni_real, na.rm=T),
            .groups="keep") %>%
  ungroup() %>% distinct() %>% 
  mutate(mensual_real=ifelse(is.nan(mensual_real)|is.na(mensual_real), 0, mensual_real),
         est_real=ifelse(is.nan(est_real)|is.na(est_real), 0, est_real),
         sni_real=ifelse(is.nan(sni_real)|is.na(sni_real), 0, sni_real)) %>%
  arrange(simp2, desam, ejercicio)
intersections2$sexo<-ifelse(str_detect(intersections2$sexo, "ombre"), "masculino", 
                            ifelse(str_detect(intersections2$sexo, "ujer"), "femenino",
                                   ifelse(str_detect(intersections2$sexo, "dato"), NA,
                                          intersections2$sexo %>% str_to_lower())))
intersections2<-intersections2 %>%
  mutate(
    espe=ifelse(str_detect(nivel %>% str_to_upper(), "ESPECIALIDAD"), 1, 0),
    mae=ifelse(str_detect(nivel %>% str_to_upper(), "MAESTRIA"), 1, 0),
    phd=ifelse(str_detect(nivel %>% str_to_upper(), "DOCTORADO"), 1, 0),
    pos=ifelse(str_detect(nivel %>% str_to_upper(), "POSDOCT"), 1, 0),
    rep=ifelse(str_detect(nivel %>% str_to_upper(), "REPATRIACION"), 1, 0),
    cate=ifelse(str_detect(nivel %>% str_to_upper(), "CATEDRA"), 1, 0),
    snii=ifelse(!is.na(nivelsni), 1, 0),
    trab=ifelse(is.na(nivel), 1, 0)
  ) %>%
  mutate(espe=ifelse(is.na(espe), 0, espe),
         mae=ifelse(is.na(mae), 0, mae),
         phd=ifelse(is.na(phd), 0, phd),
         pos=ifelse(is.na(pos), 0, pos),
         rep=ifelse(is.na(rep), 0, rep),
         cate=ifelse(is.na(cate), 0, cate),
         snii=ifelse(is.na(snii), 0, snii),
         trab=ifelse(is.na(trab), 0, trab)) 
tablatotales<-total %>% 
  mutate(dum2=str_extract(simp2, "^.*(?=[,])")) %>%
  subset(nchar(dum2)>2&ejercicio<2026) %>% 
  select(ejercicio, simp2, desam, espe, mae, phd, pos, rep, cate, snii, trab) %>%
  distinct() %>% group_by(ejercicio) %>%
  summarise(espe=sum(espe, na.rm=T), mae=sum(mae, na.rm=T), 
            phd=sum(phd, na.rm=T), pos=sum(pos, na.rm=T), rep=sum(rep, na.rm=T), 
            cate=sum(cate, na.rm=T), snii=sum(snii, na.rm=T), trab=sum(trab, na.rm=T)) %>% mutate(tipo="totales")
tablainter<-intersections2 %>% mutate(dum2=str_extract(simp2, "^.*(?=[,])")) %>%
  subset(nchar(dum2)>2&ejercicio<2026) %>% select(ejercicio, simp2, desam, espe, mae, phd, pos, rep, cate, snii, trab) %>%
  distinct() %>% group_by(ejercicio) %>%
  summarise(espe=sum(espe, na.rm=T), mae=sum(mae, na.rm=T), 
            phd=sum(phd, na.rm=T), pos=sum(pos, na.rm=T), rep=sum(rep, na.rm=T), 
            cate=sum(cate, na.rm=T), snii=sum(snii, na.rm=T), trab=sum(trab, na.rm=T)) %>% mutate(tipo="intersections")
tablas<-rbind(tablainter, tablatotales)
write_csv(total, "total.csv")
write_csv(intersections2, "intersections.csv")
write_csv(tablas, "tablastotales.csv")

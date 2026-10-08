library(tidyverse)
library(stringi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

####0 juntando bases####
#####0.1 posgrados#####
posgradoextra<-read_csv("posgradoextra.csv", na = c("", "NA", "N/A", "N/D","S/D"))
posgradonacio<-read_csv("posgradonacio.csv", na = c("", "NA", "N/A", "N/D","S/D"))
colnames(posgradonacio)<-c("year", "nombre","inicio", "fin","nivel", "institucion", "pais_entidad", "programa", "areasni", "convocatoria", "total")
colnames(posgradoextra)<-c("year", "nombre","inicio", "fin","nivel", "institucion", "pais_entidad", "programa", "areasni", "total", "convocatoria")
posgradonacio<-posgradonacio %>% subset(!is.na(year))%>%
  select(year, nombre, inicio, fin, nivel, institucion, pais_entidad, programa, areasni, convocatoria, total)
posgradoextra<-posgradoextra %>% subset(!is.na(year))%>%
  select(year, nombre, inicio, fin, nivel, institucion, pais_entidad, programa, areasni, convocatoria, total)
posgradonacio$tipo<-"nacional"
posgradoextra$tipo<-"extranjero"
posgradonacio$yinicio<-as.numeric(str_extract(posgradonacio$inicio, "\\d{4}"))
posgradoextra$yinicio<-as.numeric(str_extract(posgradoextra$inicio, "\\d{4}"))
posgradonacio$yfin<-as.numeric(str_extract(posgradonacio$fin, "\\d{4}"))
posgradoextra$yfin<-as.numeric(str_extract(posgradoextra$fin, "\\d{4}"))
posgradonacio$minicio<-as.numeric(str_extract(posgradonacio$inicio, "(?<=/)\\d{1,2}(?=/)"))
posgradoextra$minicio<-as.numeric(str_extract(posgradoextra$inicio, "^\\d{1,2}(?=/)"))
posgradonacio$mfin<-as.numeric(str_extract(posgradonacio$fin, "(?<=/)\\d{1,2}(?=/)"))
posgradoextra$mfin<-as.numeric(str_extract(posgradoextra$fin, "(?<=/)\\d{1,2}(?=/)"))
posgradonacio<-posgradonacio %>% 
  mutate(idenom=ifelse(year<2018|(year==2018&yinicio<2018)|(year==2018&yinicio==2018&minicio<6), "pre2019", "pos2019")) 
posgradoextra<-posgradoextra %>%
  mutate(idenom=ifelse(year<2018|(year==2018&yinicio<2018)|(year==2018&yinicio==2018&minicio<2), "pre2019", "pos2019"))
posgrados<-rbind(posgradonacio, posgradoextra) %>%
  arrange(nombre, year)
posgrados$nombre<-posgrados$nombre%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posgrados$institucion<- posgrados$institucion %>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posgrados$nivel<- posgrados$nivel%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posgrados$pais_entidad<- posgrados$pais_entidad%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posgrados$programa<- posgrados$programa%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posgrados$areasni<- posgrados$areasni%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
write_csv(posgrados, "posgrados.csv")
#####0.2 posdocs#####
repatriaciones<-read_csv("repatriaciones.csv")
posdocextra<-read_csv("posdocextra.csv")
posdocnacio<-read_csv("posdocnacio.csv")
colnames(posdocextra)<-c("year", "nombre","inicio", "fin","institucion", "pais_entidad", "areasni", "convocatoria", "total")
colnames(posdocnacio)<-c("year", "nombre","inicio", "fin","institucion", "pais_entidad", "areasni", "total", "convocatoria")
colnames(repatriaciones)<-c("year", "nombre","inicio", "fin","institucion", "pais_entidad", "areasni", "total", "convocatoria")
repatriaciones$tipo<-"repatriacion"
posdocnacio$tipo<-"nacional"
posdocextra$tipo<-"extranjero"
posdocs<-rbind(posdocnacio, posdocextra, repatriaciones) %>%
  arrange(nombre, year)
posdocs$nivel<-"posdoctorado"
posdocs$programa<-NA
posdocs$yinicio<-as.numeric(str_extract(posdocs$inicio, "\\d{4}"))
posdocs$yfin<-as.numeric(str_extract(posdocs$fin, "\\d{4}"))
posdocs$minicio<-str_extract(posdocs$inicio, "^\\d{1,2}(?=/)") %>% as.numeric()
posdocs$mfin<-str_extract(posdocs$fin, "^\\d{1,2}(?=/)") %>% as.numeric()
posdocs<-posdocs %>% arrange(year, yfin, yinicio, mfin, minicio) 
posdocs$idenom<-ifelse(posdocs$year<2018|(posdocs$tipo!="nacional"&posdocs$year==2018&posdocs$yfin>1), "pre2019", "pos2019")
posdocs$nombre<-posdocs$nombre %>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posdocs$institucion<-posdocs$institucion %>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posdocs$nivel<-posdocs$nivel%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posdocs$pais_entidad<-posdocs$pais_entidad%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posdocs$programa<-posdocs$programa%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posdocs$areasni<-posdocs$areasni%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
posdocs$convocatoria<-posdocs$convocatoria%>% stri_trans_general(id="Latin-ASCII") %>% toupper()
write_csv(posdocs, "posdocs.csv")
#####0.3 sni#####
sni<-read_csv("sni.csv")
colnames(sni)<-c("year", "cvu", "nombre", "nivel", "inicio", "fin", "areasni", "disciplina", "subdisciplina", "especialidad", "institucion", "dependencia", 
                 "entidad", "posdoc", "institucioncomision", "dependenciacomision", "ubicacioncomision", "pais", "expediente", "nobilis")
sni$entidad<-ifelse(str_detect(tolower(sni$entidad), "^sin\\s|^except|^exterior"), NA, sni$entidad) %>% stri_trans_general(id="Latin-ASCII")
sni$pais<-ifelse(str_detect(tolower(sni$pais), "^sin\\s|^except|^exterior"), NA, sni$pais) %>% stri_trans_general(id="Latin-ASCII")
sni$pais_entidad<-ifelse(is.na(sni$entidad), sni$pais, sni$entidad)
sni$nombre<-str_replace_all(sni$nombre, "(\\s)(?=(\\s))|^\\s+|^[:punct:]", "") %>% stri_trans_general(id="Latin-ASCII")
sni$institucion<- sni$institucion %>% stri_trans_general(id="Latin-ASCII")
sni$institucioncomision<- sni$institucioncomision%>% stri_trans_general(id="Latin-ASCII")
sni$dependenciacomision<- sni$dependenciacomision%>% stri_trans_general(id="Latin-ASCII")
sni$ubicacioncomision<- sni$ubicacioncomision%>% stri_trans_general(id="Latin-ASCII")
sni$areasni<-sni$areasni%>% stri_trans_general(id="Latin-ASCII")
sni$nivel<-sni$nivel%>% stri_trans_general(id="Latin-ASCII")
sni$apellidos<-str_extract(sni$nombre, "^.*(?=[,])")
sni$nombres<-str_extract(sni$nombre, "(?<=,).*$")
sni$nombres<-str_replace_all(sni$nombres, "(\\s)(?=(\\s))|^\\s+", "")
sni$apellidos<-str_replace_all(sni$apellidos, "(\\s)(?=(\\s))|^\\s+", "")
sni$apellidos<-str_replace_all(sni$apellidos, "Ð", "Ñ")
sni$nombres<-str_replace_all(sni$nombres, "Ð", "Ñ")
sni$cvu<-str_replace_all(sni$cvu, "\\s+|[:punct:]", "")
sni$cvu<-ifelse(sni$cvu=="", NA, sni$cvu)
sni<-sni %>% group_by(apellidos, nombres) %>%
  fill(cvu, .direction = "downup")
#####0.4 creating catalogo de nombres con cvu######
catalogo<-sni %>% subset(!is.na(cvu)) %>% select(cvu, apellidos, nombres) %>% 
  arrange(cvu, apellidos) %>%
  group_by(cvu) %>% fill(apellidos, nombres) %>%distinct() %>% nest()
for(i in 1:nrow(catalogo)){
  apell<-catalogo$data[[i]]$apellidos[which(nchar(catalogo$data[[i]]$apellidos)==max(nchar(catalogo$data[[i]]$apellidos)))][1]
  nomb<-catalogo$data[[i]]$nombres[which(nchar(catalogo$data[[i]]$nombres)==max(nchar(catalogo$data[[i]]$nombres)))][1]
  catalogo$data[[i]]$apellidos<-apell
  catalogo$data[[i]]$nombres<-nomb
}
catalogo<-catalogo %>% unnest(cols = c(data)) %>% distinct()
colnames(catalogo)<-c("cvu", "apell_stand", "nom_stand")
sni<-left_join(sni, catalogo)
sni$apellidos<-ifelse(is.na(sni$apell_stand), sni$apellidos, sni$apell_stand)
sni$nombres<-ifelse(is.na(sni$nom_stand), sni$nombres, sni$nom_stand)
sni$disciplina<-sni$disciplina %>% stri_trans_general(id="Latin-ASCII") %>% toupper()
sni$subdisciplina<-sni$subdisciplina %>% stri_trans_general(id="Latin-ASCII") %>% toupper()
sni$especialidad<-sni$especialidad %>% stri_trans_general(id="Latin-ASCII") %>% toupper()
##processing the rows without id###
nombr<-sni %>% subset(is.na(apellidos)) %>% 
  arrange(cvu, nombre, nivel)
write_csv(nombr, "temp.csv")
nombr2<-read_csv("temp2.csv")
nombr2$cvu<-as.character(nombr2$cvu)
sni1<-rbind(sni%>%subset(!is.na(apellidos)), nombr2) %>% 
  arrange(apell_stand, nom_stand, year)
sni$apell_stand<-stri_trans_general(sni$apell_stand, id="Latin-ASCII") %>% toupper()
sni$nom_stand<-stri_trans_general(sni$nom_stand, id="Latin-ASCII") %>% toupper()
write_csv(sni1, "sni1.csv")

####1 first extraction####
#####1.1 posgrados y posdocs#######
posgrados<-read_csv("posgrados.csv")
posdocs<-read_csv("posdocs.csv")
secihti<-rbind(posgrados, posdocs) %>%
  arrange(nombre, year)
#####1.2 taking out institutional#####
institucional<-c("fundacion", "archivo", "aportaci[o|ó]n", "organizacion", "deutsche","centro", "universidad", "ciesas", "consejo", "escuela", "instituto", "n/a")
institucional<-paste("^", institucional, sep="") %>%  paste(collapse="|")
instituciones<-secihti %>% filter(str_detect(tolower(nombre), institucional))
write_csv(instituciones, "institucional.csv")
secihti<-secihti %>% filter(!str_detect(tolower(nombre), institucional)) %>%
  mutate(nombre=str_replace_all(nombre, "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$", "")) %>% distinct()
#####1.3 bases de apellidos######
secihti<-separate(secihti, nombre, into=c("apellidos", "nombres"), sep=",", remove=F)
secihtidef<-secihti %>% subset(!is.na(nombres)&!str_detect(nombres, "^(\\s)*$")) %>%
  mutate(nombres=str_replace_all(nombres, "^\\s+|\\s+(?=(\\s))|\\s+$|[^[:alpha:]\\s]", ""), 
         apellidos=str_replace_all(apellidos, "^\\s+|\\s+(?=(\\s))|\\s+$|[^[:alpha:]\\s]", ""))
secihtiund<-secihti %>% subset(is.na(nombres)|str_detect(nombres, "^(\\s)*$")) %>%
  mutate(nombres=NA, apellidos=NA)
  
apellidos<-secihtidef %>% subset(select=c("apellidos", "nombres")) %>% 
  distinct() %>% 
  mutate(nchar=nchar(apellidos)) %>% arrange(desc(nchar)) 
apellidosplural<-apellidos %>% subset(str_detect(apellidos, "\\s")) %>%
  distinct(apellidos)
#####1.4 extraction with secihti######
rm(instituciones, posdocs, posgrados)
apell<-apellidosplural$apellidos %>% str_replace_all("[^[:alpha:]\\s]", "") %>% 
  str_replace_all("^(\\s+)|(?<=(\\s))(\\s+)|(\\s+)$", "") %>% unique()
apell1<-paste("^", apell, "(\\b)", sep="")
apell2<-paste("(\\b)", apell, "$",sep="")

nombresund<-secihtiund %>% 
  mutate(pre19=ifelse(idenom=="pre2019", 1, 0)) %>%
  subset(select=c(pre19, nombre)) %>% distinct() %>%
  mutate(apellidos=NA, nombres=NA)

for(i in 1:length(apell)){
  nombresund$apellidos<-ifelse(!is.na(nombresund$apellidos), nombresund$apellidos, 
                               ifelse(nombresund$pre19==1, str_extract(nombresund$nombre, apell1[i]), 
                                      str_extract(nombresund$nombre, apell2[i])))
  print(paste(i*100/length(apell), "%", sep=""))
}

nombresund<-nombresund %>% subset(!is.na(apellidos), select=-pre19) %>% distinct()
secihtiundt<-secihtiund %>% select(-c(apellidos, nombres)) %>% left_join(nombresund) %>%
  arrange(nombre, year, apellidos, nombres) 

secihtiund2<-secihtiundt %>% subset(is.na(apellidos)) 
secihtideft<-secihtiundt %>% subset(!is.na(apellidos)) 

for(i in 1:nrow(secihtideft)){
  secihtideft$nombres[i]<-str_replace(secihtideft$nombre[i], secihtideft$apellidos[i] , "")
  secihtideft$nombres[i]<-str_replace_all(secihtideft$nombres[i], "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$|[:punct:]", "")
  print(paste(i*100/nrow(secihtideft), "%", sep=""))
}
secihtidef2<-rbind(secihtidef, secihtideft)
write_csv(secihti, "secihti0a.csv")
write_csv(secihtidef2, "secihtidef.csv")
write_csv(secihtiund2, "secihtiund.csv")

#####1.5 extraction with sni#####
sni<-read_csv("sni1.csv")
secihtiund<-read_csv("secihtiund.csv") 
secihtidef<-read_csv("secihtidef.csv") 
apellidos<-sni %>% subset(select=c("apellidos", "nombres")) %>% 
  distinct() %>% 
  mutate(nchar=nchar(apellidos)) %>% arrange(desc(nchar)) 
apellidosplural<-apellidos %>% subset(str_detect(apellidos, "\\s")) %>%
  distinct(apellidos)
apell<-apellidosplural$apellidos %>% str_replace_all("[^[:alpha:]\\s]", "") %>% 
  str_replace_all("^(\\s+)|(?<=(\\s))(\\s+)|(\\s+)$", "") %>% unique()
apell1<-paste("^", apell, "(\\b)", sep="")
apell2<-paste("(\\b)", apell, "$",sep="")

nombresund<-secihtiund %>% 
  mutate(nombre=toupper(nombre),
    pre19=ifelse(idenom=="pre2019", 1, 0)) %>%
  subset(select=c(pre19, nombre)) %>% distinct() %>%
  mutate(apellidos=NA, nombres=NA)

for(i in 1:length(apell)){
  nombresund$apellidos<-ifelse(!is.na(nombresund$apellidos), nombresund$apellidos, 
                               ifelse(nombresund$pre19==1, str_extract(nombresund$nombre, apell1[i]), 
                                      str_extract(nombresund$nombre, apell2[i])))
  print(paste(i*100/length(apell), "%", sep=""))
}

nombresund<-nombresund %>% subset(!is.na(apellidos), select=-pre19) %>% distinct()
secihtiundt<-secihtiund %>% select(-c(apellidos, nombres)) %>% left_join(nombresund) %>%
  arrange(nombre, year, apellidos, nombres) 

secihtiund2<-secihtiundt %>% subset(is.na(apellidos)) 
secihtideft<-secihtiundt %>% subset(!is.na(apellidos)) 

for(i in 1:nrow(secihtideft)){
  secihtideft$nombres[i]<-str_replace(secihtideft$nombre[i], secihtideft$apellidos[i] , "")
  secihtideft$nombres[i]<-str_replace_all(secihtideft$nombres[i], "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$|[:punct:]", "")
  print(paste(i*100/nrow(secihtideft), "%", sep=""))
}
secihtidef2<-rbind(secihtidef, secihtideft)
write_csv(secihtidef2, "secihtidef1.csv")
write_csv(secihtiund2, "secihtiund1.csv")


#####1.5 patterns of names and lastnames####
sni<-read_csv("sni1.csv")
secihtidef<-read_csv("secihtidef1.csv")
apellsec<-secihtidef %>% subset(select=c("apellidos", "nombres")) %>% 
  distinct() 
apellsni<-sni %>% subset(!is.na(nombres), select=c("apell_stand", "nom_stand")) %>% 
  distinct() %>% rename(apellidos=apell_stand, nombres=nom_stand) 
total<-rbind(apellsec, apellsni) %>% distinct() 
total$apellidos<-str_replace_all(total$apellidos, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
total$nombres<-str_replace_all(total$nombres, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
apcompues<-c("Y\\sDE\\sL[A|O]S",  "Y\\sDE\\sLA", "Y\\sDEL", "DE\\sL[A|O]S", "DE\\sLA",  "Y\\sDE", "V[A|O]N", "DEL", "DE", "Y")
apcompues<-paste("\\b", apcompues, "\\s\\w{3,}", sep="") 
apellidos<-total %>% select(apellidos)
apellidos$apcompues<-NA
for(i in 1:length(apcompues)){
  apellidos$apcompues<-ifelse(!is.na(apellidos$apcompues), apellidos$apcompues, 
                               str_extract(apellidos$apellidos, apcompues[i]))
  apellidos$apellidos<-ifelse(!is.na(apellidos$apcompues), 
                              str_replace(apellidos$apellidos, apcompues[i], ""), apellidos$apellidos)
  apellidos$apellidos<-str_replace_all(apellidos$apellidos, "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$", "")
}
apellidos<-apellidos %>%  separate(apellidos, into=c("apell1", "apell2"), sep="\\s")
apell<-c(apellidos$apell1, apellidos$apell2, apellidos$compuestos) %>% 
  as.data.frame() 
colnames(apell)<-c("pattern")
apell<- apell %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_apell") %>%
  arrange(desc(n_apell))

nombres<-total %>% select(nombres)
nmcompues<-c("MARIA\\sDE\\sLOS\\s\\w+", "MARIA\\sDEL\\s\\w+", "MARIA\\sDE\\s\\w+", "\\w+\\DE\\sJESUS") %>%
  paste(collapse="|")
ncompues<-str_extract(total$nombres, nmcompues) 
compuestos2<-c("\\bDE\\sL[A|O]S", "\\bDE\\sLA", "\\bDEL", "\\bDE")
compuestos2<-paste(compuestos2, "\\s", sep="") %>% 
  paste(collapse="|")
nombres$nombres<- str_replace_all(nombres$nombres, compuestos2, "")
nombres<-nombres %>%  separate(nombres, into=c("nom1", "nom2"), sep="\\s")
nom<-c(nombres$nom1, nombres$nom2, ncompues) %>% 
  as.data.frame()
colnames(nom)<-c("pattern")
nom<- nom %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_nom") %>%
  arrange(desc(n_nom))
patrones<-merge(apell, nom, by="pattern", all=TRUE) %>% subset(nchar(pattern)>2)
patrones[is.na(patrones)]<-0
patrones$tipo<-ifelse(patrones$n_apell>=(4*patrones$n_nom), "ape", 
                      ifelse(patrones$n_nom>=(4*patrones$n_apell), "nom", "und") 
)
patrones<- patrones %>% subset(tipo!="und") 
write_csv(patrones, "patrones0.csv")

#######1.6 dealing with mistakes or identification######
patrones<-read_csv("patrones0.csv") %>% select(pattern, tipo) %>% rename(type=tipo)
sec_def<-read_csv("secihtidef1.csv") %>% arrange(nombre, year)
sec_def$dum<-ifelse((sec_def$nombre==lead(sec_def$nombre)&sec_def$apellidos!=lead(sec_def$apellidos))|
                      (sec_def$nombre==lag(sec_def$nombre)&sec_def$apellidos!=lag(sec_def$apellidos)), 1, 0)
sec_def$dum<-ifelse(!is.na(sec_def$dum), sec_def$dum, 
                    ifelse(!is.na(lead(sec_def$dum)), lead(sec_def$dum), 
                           ifelse(!is.na(lag(sec_def$dum)), lag(sec_def$dum), 0)))
duplicates<-sec_def %>% subset(dum==1) 
duplicates$firstapell<-str_extract(duplicates$apellidos, "^(\\w+)")
duplicates$firstnom<-str_extract(duplicates$nombres, "^(\\w+)")
duplicates<- duplicates %>% left_join(patrones, by=c("firstapell"="pattern"))  %>%
  mutate(type=ifelse(is.na(type), "und", type)) %>%  rename(apelltype=type) %>%
  left_join(patrones, by=c("firstnom"="pattern")) %>%
  mutate(type=ifelse(is.na(type), "und", type)) %>% rename(nomtype=type) %>% 
  mutate(correct=ifelse(apelltype=="nom"&nomtype=="ape", -2, 
                        ifelse(apelltype=="ape"&nomtype=="nom", 2,
                               ifelse(apelltype=="nom"|nomtype=="ape", -1,
                                      ifelse(apelltype=="ape"|nomtype=="nom", 1,
                                             0)
                               )
                        )
  )
  ) %>% group_by(nombre) %>% nest()
for(i in 1:nrow(duplicates)){
  apell_stand<-duplicates$data[[i]]$apellidos[which(duplicates$data[[i]]$correct==max(duplicates$data[[i]]$correct))][1]
  nom_stand<-duplicates$data[[i]]$nombres[which(duplicates$data[[i]]$correct==max(duplicates$data[[i]]$correct))][1]
  duplicates$data[[i]]$apell_stand<-apell_stand
  duplicates$data[[i]]$nom_stand<-nom_stand
  duplicates$data[[i]]<-duplicates$data[[i]] %>% arrange(desc(correct), year) %>% distinct(year, apell_stand, nom_stand, .keep_all = TRUE)
}  
duplicates<- duplicates %>% unnest(cols = c(data)) %>% 
  select(-c(firstapell, firstnom, apelltype, nomtype, correct)) %>%
  mutate(apellidos=ifelse(is.na(apell_stand), apellidos, apell_stand),
         nombres=ifelse(is.na(nom_stand), nombres, nom_stand)) %>%
  select(-c(apell_stand, nom_stand))
sec_defclean<-sec_def %>% subset(dum==0) %>% rbind(duplicates) %>% 
  select(-dum) %>% mutate(status="defined")
sec_und<-read_csv("secihtiund1.csv") %>% mutate(status="undefined")
sec_1stround<-rbind(sec_defclean, sec_und) %>% 
  arrange(nombre, year, apellidos, nombres) 
write_csv(sec_1stround, "secihti0b.csv")

#####1.7 imputing names in original dataset####
sec_ori<-read_csv("secihti0a.csv") %>% 
  arrange(nombre, year) %>% select(-c(apellidos, nombres)) %>% distinct()
sec_1st<-read_csv("secihti0b.csv") %>%
  arrange(nombre, year) 
sec_ori2<-sec_ori %>% 
  left_join(sec_1st %>% 
              select(nombre, apellidos, nombres) %>% distinct()) %>%
  distinct()
write_csv(sec_ori2, "secihti1.csv")

####2 second round####
#####2.1 creating catalogues with new list#####
sni<-read_csv("sni1.csv")
secihtidef<-read_csv("secihti1.csv") %>%  subset(!is.na(apellidos)&!is.na(nombres))
apellsec<-secihtidef %>% subset(select=c("apellidos", "nombres")) %>% 
  distinct() 
apellsni<-sni %>% subset(!is.na(nombres), select=c("apell_stand", "nom_stand")) %>% 
  distinct() %>% rename(apellidos=apell_stand, nombres=nom_stand) 
total<-rbind(apellsec, apellsni) %>% distinct() 
total$apellidos<-str_replace_all(total$apellidos, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
total$nombres<-str_replace_all(total$nombres, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
compuestos<-c("Y\\sDE\\sL[A|O]S",  "Y\\sDE\\sLA", "Y\\sDEL", "DE\\sL[A|O]S", "DE\\sLA",  "Y\\sDE", "V[A|O]N", "DEL", "DE", "Y")
compuestos<-paste("\\b", compuestos, "\\s\\w+", sep="") 
apellidos<-total %>% select(apellidos)
apellidos$compuestos<-NA
for(i in 1:length(compuestos)){
  apellidos$compuestos<-ifelse(!is.na(apellidos$compuestos), apellidos$compuestos, 
                               str_extract(apellidos$apellidos, compuestos[i]))
  apellidos$apellidos<-ifelse(!is.na(apellidos$compuestos), 
                              str_replace(apellidos$apellidos, compuestos[i], ""), apellidos$apellidos)
  apellidos$apellidos<-str_replace_all(apellidos$apellidos, "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$", "")
}
apellidos<-apellidos %>%  separate(apellidos, into=c("apell1", "apell2"), sep="\\s")
apell<-c(apellidos$apell1, apellidos$apell2, apellidos$compuestos) %>% 
  as.data.frame() 
colnames(apell)<-c("pattern")
apell<- apell %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_apell") %>%
  arrange(desc(n_apell))

nombres<-total %>% select(nombres)
nmcompues<-c("MARIA\\sDE\\sLOS\\s\\w+", "MARIA\\sDEL\\s\\w+", "MARIA\\sDE\\s\\w+", "\\w+\\DE\\sJESUS") %>%
  paste(collapse="|")
ncompues<-str_extract(total$nombres, nmcompues)
compuestos2<-c("DE\\sL[A|O]S", "DE\\sLA", "DEL")
compuestos2<-paste(compuestos2, "\\s", sep="") %>% 
  paste(collapse="|")
nombres$nombres<- str_replace_all(nombres$nombres, compuestos2, "")
nombres<-nombres %>%  separate(nombres, into=c("nom1", "nom2"), sep="\\s")
nom<-c(nombres$nom1, nombres$nom2, ncompues) %>% 
  as.data.frame()
colnames(nom)<-c("pattern")
nom<- nom %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_nom") %>%
  arrange(desc(n_nom))
patrones<-merge(apell, nom, by="pattern", all=TRUE) %>% subset(nchar(pattern)>2)
patrones[is.na(patrones)]<-0
patrones$type<-ifelse(patrones$n_apell>=(4*patrones$n_nom), "ape", 
                      ifelse(patrones$n_nom>=(4*patrones$n_apell), "nom", "und") 
)
patrones<- patrones %>% subset(type!="und") 
write_csv(patrones, "patrones1.csv")

#####2.2 extracting ######
secund<-read_csv("secihti1.csv") %>%  subset(is.na(apellidos)|is.na(nombres))
secund$nombre<-str_replace_all(secund$nombre, "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$|[-]", "")
secund$nombre<-str_replace_all(secund$nombre, "\\bMA[.]", "MARIA")
patrones<-read_csv("patrones1.csv") %>% select(pattern, type) 
names<-paste("\\b", patrones$pattern[which(patrones$type=="nom")], "\\b", sep="") %>% 
  paste(collapse = "|")
lnames<-paste("\\b", patrones$pattern[which(patrones$type=="ape")], "\\b", sep="") %>% 
  paste(collapse = "|")
secund$nombres1<-NA
secund$apellidos1<-NA
secund$nombres2<-NA
secund$apellidos2<-NA
nompre19<-secund %>% subset(idenom=="pre2019", select=c(nombre, nombres1, apellidos1, nombres2, apellidos2)) %>% distinct()
nompos19<-secund %>% subset(idenom=="pos2019", select=c(nombre, nombres1, apellidos1, nombres2, apellidos2)) %>% distinct()
for(i in 1:nrow(nompre19)){
  locations1<-str_locate_all(nompre19$nombre[i], names)
  locmin<-min(locations1[[1]][,1])
  nompre19$nombres1[i]<-substr(nompre19$nombre[i], locmin, nchar(nompre19$nombre[i])) %>% trimws(which="both")
  nompre19$apellidos1[i]<-substr(nompre19$nombre[i],1, locmin-1)%>% trimws(which="both")
  locations2<-str_locate_all(nompre19$nombre[i], lnames)
  locmax<-max(locations2[[1]][,2])
  nompre19$nombres2[i]<-substr(nompre19$nombre[i], locmax+1, nchar(nompre19$nombre[i]))%>% trimws(which="both")
  nompre19$apellidos2[i]<-substr(nompre19$nombre[i],1, locmax)%>% trimws(which="both")
  rm(locations1, locations2, locmix, locmax)
  print(paste(i*100/nrow(nompre19), "%", sep=""))
}
for(i in 1:nrow(nompos19)){
  locations1<-str_locate_all(nompos19$nombre[i], names)
  locmax<-max(locations1[[1]][,2])
  nompos19$nombres1[i]<-substr(nompos19$nombre[i], 1, locmax)  
  nompos19$apellidos1[i]<-substr(nompos19$nombre[i],locmax+1, nchar(nompos19$nombre[i]))
  locations2<-str_locate_all(nompos19$nombre[i], lnames)
  locmin<-min(locations2[[1]][,1])
  nompos19$nombres2[i]<-substr(nompos19$nombre[i], locmin, nchar(nompos19$nombre[i])) %>% trimws(which="both")
  nompos19$apellidos2[i]<-substr(nompos19$nombre[i],1, locmin-1)%>% trimws(which="both")
  rm(locations1, locations2, locmix, locmax)
  print(paste(i*100/nrow(nompos19), "%", sep=""))
}
nompre19$pre19<-1
nompos19$pre19<-0
  
secundtemp<-rbind(nompre19, nompos19)
write_csv(secundtemp, "secundtemp.csv")
#####2.3 cleaning names and lastnames####
secundtemp<-read_csv("secundtemp.csv")
secundtemp$correct<-ifelse(secundtemp$apellidos1==secundtemp$apellidos2, 1, 0)
secundtemp$apellidos1<-ifelse(str_detect(secundtemp$apellidos1, "^\\s*$"), NA, secundtemp$apellidos1)
secundtemp$apellidos2<-ifelse(str_detect(secundtemp$apellidos2, "^\\s*$"), NA, secundtemp$apellidos2)
secundtemp$nombres1<-ifelse(str_detect(secundtemp$nombres1, "^\\s*$"), NA, secundtemp$nombres1)
secundtemp$nombres2<-ifelse(str_detect(secundtemp$nombres2, "^\\s*$"), NA, secundtemp$nombres2)
correct<-secundtemp %>% subset(correct==1) %>% 
  mutate(apellidos=ifelse(is.na(apellidos1), apellidos2, apellidos1),
         nombres=ifelse(is.na(nombres1), nombres2, nombres1)) %>%
  select(c(nombre, apellidos, nombres)) %>% distinct()
secundtemp2<-secundtemp %>% left_join(correct) %>% select(-correct)
corrected<-secundtemp2 %>% subset(!is.na(apellidos)) %>% distinct()
uncorrected<-secundtemp2 %>% subset(is.na(apellidos)) %>% 
  select(-c(apellidos, nombres)) %>% distinct()
uncorrected$spaceapell1<-str_count(uncorrected$apellidos1, "\\s")
uncorrected$spaceapell2<-str_count(uncorrected$apellidos2, "\\s")
uncorrected$complnom1<-ifelse(!is.na(uncorrected$apellidos1)&!is.na(uncorrected$nombres1), 1, 0)
uncorrected$complnom2<-ifelse(!is.na(uncorrected$nombres2)&!is.na(uncorrected$apellidos2), 1, 0)
uncorrected<- uncorrected %>% 
  mutate(apellidos=ifelse(complnom1==1&(is.na(complnom2)|complnom2!=1)&spaceapell1==1, apellidos1, 
                          ifelse(complnom1==1&complnom2==1&spaceapell1==1&spaceapell2!=1, apellidos1,
                                 ifelse(complnom2==1&(is.na(complnom1)|complnom1!=1)&spaceapell2==1, apellidos2, 
                                        ifelse(complnom2==1&complnom1==1&spaceapell2==1&spaceapell1!=1, apellidos2,
                                               NA)
                                 )
                          )
  ), 
  nombres=ifelse(complnom1==1&(is.na(complnom2)|complnom2!=1)&spaceapell1==1, nombres1, 
                 ifelse(complnom1==1&complnom2==1&spaceapell1==1&spaceapell2!=1, nombres1,
                        ifelse(complnom2==1&(is.na(complnom1)|complnom1!=1)&spaceapell2==1, nombres2, 
                               ifelse(complnom2==1&complnom1==1&spaceapell2==1&spaceapell1!=1, nombres2,
                                      NA)
                        )
                 )
  )
  ) 
uncorrected<-uncorrected %>% arrange(nombre, apellidos) %>%
  mutate(apellidos=ifelse((nombre==lag(nombre)&apellidos!=lag(apellidos))|(nombre==lead(nombre)&apellidos!=lead(apellidos)), 
                          NA, apellidos),
         nombres=ifelse((nombre==lag(nombre)&nombres!=lag(nombres))|(nombre==lead(nombre)&nombres!=lead(nombres)), NA, nombres))
uncorrected$apellidos[1]<-ifelse(uncorrected$nombre[1]==uncorrected$nombre[2], uncorrected$apellidos[2], uncorrected$apellidos[2])
uncorrected$apellidos[nrow(uncorrected)]<-ifelse(uncorrected$nombre[nrow(uncorrected)]==uncorrected$nombre[nrow(uncorrected)-1], 
                                                  uncorrected$apellidos[nrow(uncorrected)-1], uncorrected$apellidos[nrow(uncorrected)-1])
uncorrected$nombres[1]<-ifelse(uncorrected$nombre[1]==uncorrected$nombre[2], uncorrected$nombres[2], uncorrected$nombres[2])
uncorrected$nombres[nrow(uncorrected)]<-ifelse(uncorrected$nombre[nrow(uncorrected)]==uncorrected$nombre[nrow(uncorrected)-1], 
                                                  uncorrected$nombres[nrow(uncorrected)-1], uncorrected$nombres[nrow(uncorrected)-1])
corrected2<-uncorrected %>% 
  subset(!is.na(apellidos), select=-c(complnom1, complnom2, spaceapell1, spaceapell2)) %>% 
  rbind(corrected) %>% select(nombre, apellidos, nombres) %>% distinct()
secundtemp3<-secundtemp %>% left_join(corrected2)
uncorrected2<-secundtemp3 %>% subset(is.na(apellidos))

#####2.4 dealing with mistakes or identification######
patape<-c("DE\\sL\\w{1,2}\\s\\w+", "DE\\s\\w+", "\\w+") 
patapeini<- paste("^", patape, sep="") %>% 
  paste(collapse = "|")
patapefin<- paste(patape, "$", sep="") %>% 
  paste(collapse = "|")
inverted<-uncorrected2 %>% 
  subset(nombres1==apellidos2&!is.na(apellidos1)) %>% 
  mutate(patternap1=str_extract(apellidos1, patapeini), 
         patternap2=str_extract(apellidos2, patapeini))
patrones<-read_csv("patrones1.csv") %>% select(pattern, type)
inverted<-inverted %>% left_join(patrones %>% rename(patternap1=pattern, type1=type), by="patternap1") %>%
  left_join(patrones %>% rename(patternap2=pattern, type2=type), by="patternap2") 
inverted$apellidos<-ifelse(inverted$type1=="ape"&(inverted$type2=="nom"|is.na(inverted$type2)), inverted$apellidos1, 
                            ifelse((inverted$type1=="nom"|is.na(inverted$type1))&inverted$type2=="ape", inverted$apellidos2, NA)
                           )
inverted$nombres<-ifelse(inverted$type1=="ape"&(inverted$type2=="nom"|is.na(inverted$type2)), inverted$nombres1, 
                          ifelse((inverted$type1=="nom"|is.na(inverted$type1))&inverted$type2=="ape", inverted$nombres2, NA)
                          )
invercorr<- inverted %>% 
  subset(!is.na(apellidos), select=c(nombre, apellidos, nombres)) %>%  distinct()

corrected3<-rbind(corrected2, invercorr) %>% distinct()
secundtemp3<-secundtemp %>% left_join(corrected3)
uncorrected3<-secundtemp3 %>% subset(is.na(apellidos))
#####2.5 evaluating comparatively####

uncorrected3<- uncorrected3 %>%
  arrange(apellidos1, nombres1, apellidos2, nombres2) %>%
  mutate(lpatternom1=str_extract(nombres1, "\\w+$"), lpatternom2=str_extract(nombres2, "\\w+$"), 
         lpatternap1=str_extract(apellidos1, patapefin), lpatternap2=str_extract(apellidos2, patapefin))
uncorrected3<-uncorrected3 %>% 
  left_join(patrones %>% rename(lpatternom1=pattern, typen1=type)) %>%
  left_join(patrones %>% rename(lpatternom2=pattern, typen2=type)) %>%
  left_join(patrones %>% rename(lpatternap1=pattern, typea1=type)) %>%
  left_join(patrones %>% rename(lpatternap2=pattern, typea2=type))
uncorrected3$apellidos<-ifelse((uncorrected3$typea1=="ape"&(is.na(uncorrected3$typen1)|uncorrected3$typen1=="nom")&!is.na(uncorrected3$nombres1))|
                                 (is.na(uncorrected3$apellidos2)&is.na(uncorrected3$nombres2)&!is.na(uncorrected3$nombres1)), 
                               uncorrected3$apellidos1, 
                               ifelse((uncorrected3$typea2=="ape"&(is.na(uncorrected3$typen2)|uncorrected3$typen2=="nom")&!is.na(uncorrected3$nombres2))|
                                        (is.na(uncorrected3$apellidos1)&is.na(uncorrected3$nombres1)&!is.na(uncorrected3$nombres2)), 
                                      uncorrected3$apellidos2, NA))
uncorrected3$nombres<-ifelse((uncorrected3$typea1=="ape"&(is.na(uncorrected3$typen1)|uncorrected3$typen1=="nom")&!is.na(uncorrected3$nombres1))|
                               (is.na(uncorrected3$apellidos2)&is.na(uncorrected3$nombres2)&!is.na(uncorrected3$nombres1)), 
                             uncorrected3$nombres1, 
                             ifelse((uncorrected3$typea2=="ape"&(is.na(uncorrected3$typen2)|uncorrected3$typen2=="nom")&!is.na(uncorrected3$nombres2))|
                                      (is.na(uncorrected3$apellidos1)&is.na(uncorrected3$nombres1)&!is.na(uncorrected3$nombres2)), 
                                    uncorrected3$nombres2, NA))
test<-uncorrected3[which(!is.na(uncorrected3$apellidos)),] %>% distinct() %>%
  arrange(nombre, apellidos, nombres) %>%
  mutate(error=ifelse((nombre==lag(nombre)&apellidos!=lag(apellidos))|(nombre==lead(nombre)&apellidos!=lead(apellidos)), 1, 0)) %>%
  subset(is.na(error)|error==0, select=c(nombre, apellidos, nombres)) %>% distinct()

corrected4<-rbind(corrected3,test) %>% distinct()
secundtemp4<-secundtemp %>% left_join(corrected4)
uncorrected4<-uncorrected3 %>% subset(is.na(apellidos))
#####2.6 establishing number of names and lastnames####
apcomp<-c("Y\\sDE\\sL[A|O]S",  "Y\\sDE\\sLA", "Y\\sDEL", "DE\\sL[A|O]S", "DE\\sLA",  "Y\\sDE", "V[A|O]N", "DE", "Y")
apcomp<-paste("\\b", apcomp, "\\s\\w{3,}", sep="") %>% 
  paste(collapse = "|")
nmcomp<-c("\\bMARIA\\sDE\\w+", "\\w+\\sDE\\sJES[U|Ú]S\\b") %>% paste(collapse="|")
uncorrected4$ncom<-str_count(uncorrected4$nombre, nmcomp)
uncorrected4$acom<-str_count(uncorrected4$nombre, apcomp)-uncorrected4$ncom
uncorrected4$ncom1<-str_count(uncorrected4$nombres1, nmcomp)
uncorrected4$ncom2<-str_count(uncorrected4$nombres2, nmcomp)
uncorrected4$acom1<-str_count(uncorrected4$apellidos1, apcomp)-uncorrected4$ncom1
uncorrected4$acom2<-str_count(uncorrected4$apellidos2, apcomp)-uncorrected4$ncom2

compis<-uncorrected4 %>% subset(acom+ncom>0) 
compis2<-compis %>%
  subset((ncom1==ncom&acom==acom1)|(ncom2==ncom&acom==acom2)) %>%
  mutate(ncom1=ifelse(is.na(ncom1), 0, ncom1),
         acom1=ifelse(is.na(acom1), 0, acom1),
         ncom2=ifelse(is.na(ncom2), 0, ncom2),
         acom2=ifelse(is.na(acom2), 0, acom2))
compis2$apellidos<-ifelse(compis2$acom==compis2$acom1&compis2$ncom==compis2$ncom1, compis2$apellidos1, 
                         ifelse(compis2$acom==compis2$acom2&compis2$ncom==compis2$ncom2, compis2$apellidos2, NA))
compis2$nombres<-ifelse(compis2$acom==compis2$acom1&compis2$ncom==compis2$ncom1, compis2$nombres1,
                        ifelse(compis2$acom==compis2$acom2&compis2$ncom==compis2$ncom2, compis2$nombres2, NA))
compis2<- compis2 %>% select(nombres, apellidos, nombre)
compis<-compis %>% select(-c(nombres, apellidos)) %>% left_join(compis2, by="nombre") %>% 
  subset(!is.na(apellidos), select=colnames(corrected4))

corrected5<-rbind(corrected4, compis) %>% distinct()
secundtemp5<-secundtemp %>% left_join(corrected5) %>% select(c(nombre, apellidos, nombres)) %>% distinct() %>% 
  arrange(nombre, apellidos, nombres) %>% filter(!is.na(apellidos)&!is.na(nombres))
write_csv(secundtemp5, "secundtempcorr.csv")

#####2.7 rounding up#####
secundtemp<-read_csv("secundtempcorr.csv") %>% select(nombre, apellidos, nombres) %>% 
  distinct() %>% arrange(nombre, apellidos, nombres)
secihti<-read_csv("secihti1.csv") 
secihtidef<-secihti %>%  subset(!is.na(apellidos)&!is.na(nombres))
secihtiund<-secihti %>% subset(is.na(apellidos)|is.na(nombres)) %>%
  select(-c(apellidos, nombres))
secihtiund2<-secihtiund %>% left_join(secundtemp)
secihti2<-rbind(secihtidef, secihtiund2) %>% 
  arrange(nombre, year, apellidos, nombres) 
secundtemp2<-secihti2 %>% subset(is.na(apellidos)|is.na(nombres)) %>% 
  select(c(nombre,idenom)) %>% distinct()
write_csv(secihti2, "secihti2.csv")
####3. Third round####
#####3.1 creating catalogues with new list#####
sni<-read_csv("sni1.csv")
secihtidef<-read_csv("secihti2.csv") %>%  subset(!is.na(apellidos)&!is.na(nombres))
apellsec<-secihtidef %>% subset(select=c("apellidos", "nombres")) %>% 
  distinct() 
apellsni<-sni %>% subset(!is.na(nombres), select=c("apell_stand", "nom_stand")) %>% 
  distinct() %>% rename(apellidos=apell_stand, nombres=nom_stand) 
total<-rbind(apellsec, apellsni) %>% distinct() 
total$apellidos<-str_replace_all(total$apellidos, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
total$nombres<-str_replace_all(total$nombres, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
compuestos<-c("Y\\sDE\\sL[A|O]S",  "Y\\sDE\\sLA", "Y\\sDEL", "DE\\sL[A|O]S", "DE\\sLA",  "Y\\sDE", "V[A|O]N", "DEL", "DE", "Y")
compuestos<-paste("\\b", compuestos, "\\s\\w+", sep="") 
apellidos<-total %>% select(apellidos)
apellidos$compuestos<-NA
for(i in 1:length(compuestos)){
  apellidos$compuestos<-ifelse(!is.na(apellidos$compuestos), apellidos$compuestos, 
                               str_extract(apellidos$apellidos, compuestos[i]))
  apellidos$apellidos<-ifelse(!is.na(apellidos$compuestos), 
                              str_replace(apellidos$apellidos, compuestos[i], ""), apellidos$apellidos)
  apellidos$apellidos<-str_replace_all(apellidos$apellidos, "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$", "")
}
apellidos<-apellidos %>%  separate(apellidos, into=c("apell1", "apell2"), sep="\\s")
apell<-c(apellidos$apell1, apellidos$apell2, apellidos$compuestos) %>% 
  as.data.frame() 
colnames(apell)<-c("pattern")
apell<- apell %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_apell") %>%
  arrange(desc(n_apell))

nombres<-total %>% select(nombres)
nmcompues<-c("MARIA\\sDE\\sLOS\\s\\w+", "MARIA\\sDEL\\s\\w+", "MARIA\\sDE\\s\\w+", "\\w+\\DE\\sJESUS") %>%
  paste(collapse="|")
ncompues<-str_extract(total$nombres, nmcompues)
compuestos2<-c("DE\\sL[A|O]S", "DE\\sLA", "DEL")
compuestos2<-paste(compuestos2, "\\s", sep="") %>% 
  paste(collapse="|")
nombres$nombres<- str_replace_all(nombres$nombres, compuestos2, "")
nombres<-nombres %>%  separate(nombres, into=c("nom1", "nom2"), sep="\\s")
nom<-c(nombres$nom1, nombres$nom2, ncompues) %>% 
  as.data.frame()
colnames(nom)<-c("pattern")
nom<- nom %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_nom") %>%
  arrange(desc(n_nom))
patrones<-merge(apell, nom, by="pattern", all=TRUE) %>% subset(nchar(pattern)>2)
patrones[is.na(patrones)]<-0
patrones$type<-ifelse(patrones$n_apell>=(4*patrones$n_nom), "ape", 
                      ifelse(patrones$n_nom>=(4*patrones$n_apell), "nom", "und") 
)
patrones<- patrones %>% subset(type!="und") 
write_csv(patrones, "patrones2.csv")

#####3.2 extracting ######
secund<-read_csv("secihti2.csv") #%>%  subset(is.na(apellidos)|is.na(nombres), select=nombre) %>% distinct()
patrones<-read_csv("patrones2.csv") %>% select(pattern, type)
names<-paste("\\b", patrones$pattern[which(patrones$type=="nom")], "\\b", sep="") %>% 
  paste(collapse = "|")
lnames<-paste("\\b", patrones$pattern[which(patrones$type=="ape")], "\\b", sep="") %>% 
  paste(collapse = "|")
secund$nombres1<-NA
secund$apellidos1<-NA
secund$nombres2<-NA
secund$apellidos2<-NA
for(i in 1:nrow(secund)){
  locations1<-str_locate_all(secund$nombre[i], names)
  locmin<-min(locations1[[1]][,1])
  locmax<-max(locations1[[1]][,2])
  secund$nombres1[i]<-substr(secund$nombre[i], locmin, locmax) %>% trimws(which="both")
  secund$apellidos1[i]<-str_replace(secund$nombre[i], secund$nombres1[i], "") %>% trimws(which="both")
  locations2<-str_locate_all(secund$nombre[i], lnames)
  locmin<-min(locations2[[1]][,1])
  locmax<-max(locations2[[1]][,2])
  secund$apellidos2[i]<-substr(secund$nombre[i], locmin, locmax)%>% trimws(which="both")
  secund$nombres2[i]<-str_replace(secund$nombre[i], secund$apellidos2[i], "") %>% trimws(which="both")
  rm(locations1, locations2, locmin, locmax)
  print(paste(i*100/nrow(secund), "%", sep=""))
}
secund$apellidos1<-ifelse(str_detect(secund$apellidos1, "^\\s*$"), NA, secund$apellidos1) 
secund$apellidos2<-ifelse(str_detect(secund$apellidos2, "^\\s*$"), NA, secund$apellidos2)
secund$nombres1<-ifelse(str_detect(secund$nombres1, "^\\s*$"), NA, secund$nombres1)
secund$nombres2<-ifelse(str_detect(secund$nombres2, "^\\s*$"), NA, secund$nombres2)
secund$correct<-ifelse(secund$apellidos1==secund$apellidos2, 1, 0)
correct<-secund %>% subset(correct==1) %>% 
  mutate(apellidos=ifelse(is.na(apellidos1), apellidos2, apellidos1),
         nombres=ifelse(is.na(nombres1), nombres2, nombres1)) %>%
  select(c(nombre, apellidos, nombres)) %>% distinct()
secihti<-read_csv("secihti2.csv") 
secihtidef<-secihti %>%  subset(!is.na(apellidos)&!is.na(nombres))
secihtiund<-secihti %>% subset(is.na(apellidos)|is.na(nombres)) %>%
  select(-c(apellidos, nombres))
secihtiund2<-secihtiund %>% left_join(correct)
secihti2<-rbind(secihtidef, secihtiund2) %>% 
  arrange(nombre, year, apellidos, nombres) 
secihti2def<-secihti2 %>% subset(!is.na(apellidos)&!is.na(nombres)) %>% 
  distinct()
secihti2und<-secihti2 %>% subset(is.na(apellidos)|is.na(nombres)) %>% 
 distinct()
secundtemp<-secund %>% select(nombre, idenom, nombres1, nombres2, apellidos1, apellidos2, correct) %>% distinct() 
write_csv(secihti2def, "secihti2def.csv")
write_csv(secihti2und, "secihti2und.csv")
write_csv(secundtemp, "secundtemp.csv")

#####3.3 creating catalogues with new list#####
sni<-read_csv("sni1.csv")
secihtidef<-read_csv("secihti2def.csv") 
apellsec<-secihtidef %>% subset(select=c("apellidos", "nombres")) %>% 
  distinct() 
apellsni<-sni %>% subset(!is.na(nombres), select=c("apell_stand", "nom_stand")) %>% 
  distinct() %>% rename(apellidos=apell_stand, nombres=nom_stand) 
total<-rbind(apellsec, apellsni) %>% distinct() 
total$apellidos<-str_replace_all(total$apellidos, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
total$nombres<-str_replace_all(total$nombres, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
compuestos<-c("Y\\sDE\\sL[A|O]S",  "Y\\sDE\\sLA", "Y\\sDEL", "DE\\sL[A|O]S", "DE\\sLA",  "Y\\sDE", "V[A|O]N", "DEL", "DE", "Y")
compuestos<-paste("\\b", compuestos, "\\s\\w+", sep="") 
apellidos<-total %>% select(apellidos)
apellidos$compuestos<-NA
for(i in 1:length(compuestos)){
  apellidos$compuestos<-ifelse(!is.na(apellidos$compuestos), apellidos$compuestos, 
                               str_extract(apellidos$apellidos, compuestos[i]))
  apellidos$apellidos<-ifelse(!is.na(apellidos$compuestos), 
                              str_replace(apellidos$apellidos, compuestos[i], ""), apellidos$apellidos)
  apellidos$apellidos<-str_replace_all(apellidos$apellidos, "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$", "")
}
apellidos<-apellidos %>%  separate(apellidos, into=c("apell1", "apell2"), sep="\\s")
apell<-c(apellidos$apell1, apellidos$apell2, apellidos$compuestos) %>% 
  as.data.frame() 
colnames(apell)<-c("pattern")
apell<- apell %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_apell") %>%
  arrange(desc(n_apell))

nombres<-total %>% select(nombres)
nmcompues<-c("MARIA\\sDE\\sLOS\\s\\w+", "MARIA\\sDEL\\s\\w+", "MARIA\\sDE\\s\\w+", "\\w+\\DE\\sJESUS") %>%
  paste(collapse="|")
ncompues<-str_extract(total$nombres, nmcompues)
compuestos2<-c("DE\\sL[A|O]S", "DE\\sLA", "DEL")
compuestos2<-paste(compuestos2, "\\s", sep="") %>% 
  paste(collapse="|")
nombres$nombres<- str_replace_all(nombres$nombres, compuestos2, "")
nombres<-nombres %>%  separate(nombres, into=c("nom1", "nom2"), sep="\\s")
nom<-c(nombres$nom1, nombres$nom2, ncompues) %>% 
  as.data.frame()
colnames(nom)<-c("pattern")
nom<- nom %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_nom") %>%
  arrange(desc(n_nom))
patrones<-merge(apell, nom, by="pattern", all=TRUE) %>% subset(nchar(pattern)>2)
patrones[is.na(patrones)]<-0
patrones$type<-ifelse(patrones$n_apell>=(4*patrones$n_nom), "ape", 
                      ifelse(patrones$n_nom>=(4*patrones$n_apell), "nom", "und") 
)
patrones<- patrones %>% subset(type!="und") 
write_csv(patrones, "patrones2b.csv")

#####3.4 establishing scores####
secihtiund<-read_csv("secihti2und.csv")
secundtempa<-read_csv("secundtemp.csv") %>% distinct()
secundtempb<-read_csv("secundtemp2.csv") %>% select(-c(nombres, apellidos)) %>% 
  mutate(idenom=ifelse(pre19==1, "pre2019", "pos2019")) %>% select(-pre19) %>%
  distinct()
temp<-secundtempb  %>% 
  select(nombre, idenom) %>% distinct() %>% mutate(dum=1)
secundtemp<-secundtemp %>% left_join(temp) %>% subset(is.na(dum), select=-dum) %>% 
  rbind(secundtempb) %>% distinct()

secundtemp<-secundtemp %>% arrange(nombre, correct) %>% 
  mutate(dum=ifelse((correct==0&(is.na(lag(correct))|lag(correct)==1)&(lag(nombre)==nombre))|
                      (correct==0&(is.na(lead(correct))|lead(correct)==1)&(lead(nombre)==nombre)), 
                      1, 0))
secundtemp2 <- secundtemp %>% 
  mutate(nombres1=ifelse(dum==1, NA, nombres1),
         nombres2=ifelse(dum==1, NA, nombres2),
         apellidos1=ifelse(dum==1, NA, apellidos1),
         apellidos2=ifelse(dum==1, NA, apellidos2), 
         correct=ifelse(dum==1, NA, correct)) %>% distinct() %>%
  nest(data=-nombre) 
for(i in 1:nrow(secundtemp2)){
  if(nrow(secundtemp2$data[[i]])>1){
    secundtemp2$data[[i]]<-secundtemp2$data[[i]] %>%fill(nombres1, nombres2, apellidos1, apellidos2, correct, .direction="downup")
  }
}
secundtemp2<-secundtemp2 %>% unnest(cols=data) %>% 
  select(-dum) %>% distinct()
secundtemp2<-secundtemp2 %>% select(-c(idenom, correct)) %>% distinct()

patrones<-read_csv("patrones2b.csv")
names<-patrones %>% subset(type=="nom", select=pattern) 
names<-paste("\\b", names$pattern, "\\b", sep="") %>% 
  paste(collapse = "|")
lnames<-patrones %>% subset(type=="ape", select=pattern)
lnames<-paste("\\b", lnames$pattern, "\\b", sep="") %>% 
  paste(collapse = "|")
uncorrected4<-secihtiund %>% left_join(secundtemp2) %>% distinct()
uncorrected4<-uncorrected4 %>% 
  mutate(scorenom1=NA, scorenom2=NA, scoreape1=NA, scoreape2=NA)
for(i in 1:nrow(uncorrected4)){
  uncorrected4$scorenom1[i]<-str_count(uncorrected4$nombres1[i], names)/(str_count(uncorrected4$nombres1[i], names)+str_count(uncorrected4$nombres1[i], lnames))
  uncorrected4$scorenom2[i]<-str_count(uncorrected4$nombres2[i], names)/(str_count(uncorrected4$nombres2[i], names)+str_count(uncorrected4$nombres2[i], lnames))
  uncorrected4$scoreape1[i]<-str_count(uncorrected4$apellidos1[i], lnames)/(str_count(uncorrected4$apellidos1[i], names)+str_count(uncorrected4$apellidos1[i], lnames))
  uncorrected4$scoreape2[i]<-str_count(uncorrected4$apellidos2[i], lnames)/(str_count(uncorrected4$apellidos2[i], names)+str_count(uncorrected4$apellidos2[i], lnames))
  print(paste(i*100/nrow(uncorrected4), "%", sep=""))
}
write_csv(uncorrected4, "uncorrectemp.csv")
#####3.5 extracting 1#####
uncorrected<-read_csv("uncorrectemp.csv")
patrones<-read_csv("patrones2b.csv")
names<-patrones %>% subset(type=="nom", select=pattern) 
names<-paste("\\b", names$pattern, "\\b", sep="") %>% 
  paste(collapse = "|")
lnames<-patrones %>% subset(type=="ape", select=pattern)
lnames<-paste("\\b", lnames$pattern, "\\b", sep="") %>% 
  paste(collapse = "|")
uncorrected<-uncorrected %>% subset(select=c(nombre, idenom)) %>% distinct() %>%
  mutate(nombre=nombre %>% trimws(which="both"))
patterns<-c("(?<=\\bMC)\\s", "[-]") %>% paste(collapse = "|")
uncorrected$nombre2<-str_replace(uncorrected$nombre, "(?<=\\bMC)\\s", "") %>% toupper()
uncorrected$nombre2<-str_replace(uncorrected$nombre2, "[-]", " ")
uncorrected$nombre2<-str_replace(uncorrected$nombre2, "(?<=\\bDE)\\s(?=\\w)", "cone")
uncorrected$dum<-ifelse(((uncorrected$nombre2==lag(uncorrected$nombre2))&(uncorrected$idenom!=lag(uncorrected$idenom)))|
                            ((uncorrected$nombre2==lead(uncorrected$nombre2))&(uncorrected$idenom!=lead(uncorrected$idenom))), 
                              1, 0)
uncorrected$dum[is.na(uncorrected$dum)]<-0
uncorrected$spaces<-str_count(uncorrected$nombre2, "\\s")+1
uncorr1<-uncorrected %>% subset(idenom=="pre2019")
uncorr2<-uncorrected %>% subset(idenom!="pre2019")
uncorr1$apellidos<-ifelse(uncorr1$spaces==2, str_extract(uncorr1$nombre2, "^(\\w+)"), 
                           ifelse(uncorr1$spaces>=3, str_extract(uncorr1$nombre2, "^(\\w+)(\\s+)(\\w+)"), NA))
uncorr1$nombres<-ifelse(uncorr1$spaces==2, str_replace(uncorr1$nombre2, "^(\\w+)(\\s+)", ""),
                           ifelse(uncorr1$spaces>=3, str_replace(uncorr1$nombre2, "^(\\w+)(\\s+)(\\w+)(\\s+)", ""), NA))
uncorr2$apellidos<-ifelse(uncorr2$spaces==2, str_extract(uncorr2$nombre2, "(\\w+)$"), 
                           ifelse(uncorr2$spaces>=3, str_extract(uncorr2$nombre2, "(\\w+)(\\s+)(\\w+)$"), NA))
uncorr2$nombres<-ifelse(uncorr2$spaces==2, str_replace(uncorr2$nombre2, "(\\s+)(\\w+)$", ""),
                           ifelse(uncorr2$spaces>=3, str_replace(uncorr2$nombre2, "(\\s+)(\\w+)(\\s+)(\\w+)$", ""), NA))
uncorr<-rbind(uncorr1, uncorr2) 
uncorr$nombres<-str_replace(uncorr$nombres, "(?<=\\bDE)cone(?=\\w)", " ")
uncorr$apellidos<-str_replace(uncorr$apellidos, "(?<=\\bDE)cone(?=\\w)", " ")
uncorr$nombre2<-str_replace(uncorr$nombre2, "(?<=\\bDE)cone(?=\\w)", " ")
uncorr$scorenom<-NA
uncorr$scoreape<-NA
for(i in 1:nrow(uncorr)){
  uncorr$scorenom[i]<-str_count(uncorr$nombres[i], names)/(str_count(uncorr$nombres[i], names)+str_count(uncorr$nombres[i], lnames))
  uncorr$scoreape[i]<-str_count(uncorr$apellidos[i], lnames)/(str_count(uncorr$apellidos[i], names)+str_count(uncorr$apellidos[i], lnames))
  print(paste(i*100/nrow(uncorr), "%", sep=""))
}
uncorr$scorenom<-ifelse(is.na(uncorr$scorenom), 0, uncorr$scorenom)
uncorr$scoreape<-ifelse(is.na(uncorr$scoreape), 0, uncorr$scoreape)
uncorr$score<-(uncorr$scorenom+uncorr$scoreape)/2
uncorr$spacen<-str_count(uncorr$nombres, "\\s")
uncorr<-uncorr %>% nest(data=-nombre)
for(i in 1:nrow(uncorr)){
  if(nrow(uncorr$data[[i]])>1){
    uncorr$data[[i]]$nombres<-uncorr$data[[i]]$nombres[which(uncorr$data[[i]]$score==max(uncorr$data[[i]]$score))]
    uncorr$data[[i]]$apellidos<-uncorr$data[[i]]$apellidos[which(uncorr$data[[i]]$score==max(uncorr$data[[i]]$score))]
  }
}
uncorr<-uncorr %>% unnest(cols=data)
write_csv(uncorr, "uncorrtemp.csv")

#####3.6 extracting 2#####
uncorr<-read_csv("uncorrtemp.csv")
uncorr<-uncorr %>% 
  mutate(dum=ifelse(score>0.5&spacen<4&scorenom>0.4, 1, 0)) %>%
  arrange(nombre, apellidos, nombres) %>% distinct()
uncorra<-uncorr %>% filter(dum==1)
uncorrb<-uncorr %>% filter(dum!=1)

patrones0<-read_csv("patrones2b.csv")
total<-uncorra %>% subset(select=c("apellidos", "nombres")) %>% 
  distinct() 
total$apellidos<-str_replace_all(total$apellidos, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
total$nombres<-str_replace_all(total$nombres, "[^[:alpha:]\\s]|^(\\s)+|(?<=\\s)(\\s)+|(\\s)+$", "")
compuestos<-c("Y\\sDE\\sL[A|O]S",  "Y\\sDE\\sLA", "Y\\sDEL", "DE\\sL[A|O]S", "DE\\sLA",  "Y\\sDE", "V[A|O]N", "DEL", "DE", "Y")
compuestos<-paste("\\b", compuestos, "\\s\\w+", sep="") 
apellidos<-total %>% select(apellidos)
apellidos$compuestos<-NA
for(i in 1:length(compuestos)){
  apellidos$compuestos<-ifelse(!is.na(apellidos$compuestos), apellidos$compuestos, 
                               str_extract(apellidos$apellidos, compuestos[i]))
  apellidos$apellidos<-ifelse(!is.na(apellidos$compuestos), 
                              str_replace(apellidos$apellidos, compuestos[i], ""), apellidos$apellidos)
  apellidos$apellidos<-str_replace_all(apellidos$apellidos, "^(\\s+)|(\\s+)(?=(\\s))|(\\s+)$", "")
}
apellidos<-apellidos %>%  separate(apellidos, into=c("apell1", "apell2"), sep="\\s")
apell<-c(apellidos$apell1, apellidos$apell2, apellidos$compuestos) %>% 
  as.data.frame() 
colnames(apell)<-c("pattern")
apell<- apell %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_apell") %>%
  arrange(desc(n_apell))

nombres<-total %>% select(nombres)
nmcompues<-c("MARIA\\sDE\\sLOS\\s\\w+", "MARIA\\sDEL\\s\\w+", "MARIA\\sDE\\s\\w+", "\\w+\\DE\\sJESUS") %>%
  paste(collapse="|")
ncompues<-str_extract(total$nombres, nmcompues)
compuestos2<-c("DE\\sL[A|O]S", "DE\\sLA", "DEL")
compuestos2<-paste(compuestos2, "\\s", sep="") %>% 
  paste(collapse="|")
nombres$nombres<- str_replace_all(nombres$nombres, compuestos2, "")
nombres<-nombres %>%  separate(nombres, into=c("nom1", "nom2"), sep="\\s")
nom<-c(nombres$nom1, nombres$nom2, ncompues) %>% 
  as.data.frame()
colnames(nom)<-c("pattern")
nom<- nom %>% subset(!is.na(pattern)&!str_detect(pattern, "^\\s*$")) %>% count(pattern, name="n_nom") %>%
  arrange(desc(n_nom))
patrones<-merge(apell, nom, by="pattern", all=TRUE) %>% subset(nchar(pattern)>2)
patrones[is.na(patrones)]<-0
patrones$type<-ifelse(patrones$n_apell>=(4*patrones$n_nom), "ape", 
                      ifelse(patrones$n_nom>=(4*patrones$n_apell), "nom", "und") 
)
patrones<- patrones %>% subset(type!="und")  %>% rbind(patrones0)

names<-patrones %>% subset(type=="nom", select=pattern) 
names<-paste("\\b", names$pattern, "\\b", sep="") %>% 
  paste(collapse = "|")
lnames<-patrones %>% subset(type=="ape", select=pattern)
lnames<-paste("\\b", lnames$pattern, "\\b", sep="") %>% 
  paste(collapse = "|")

uncorrb<-uncorrb %>% subset(select=c(nombre, idenom)) %>% distinct()
patterns<-c("(?<=\\bMC)\\s", "[-]") %>% paste(collapse = "|")
uncorrb$nombre2<-str_replace(uncorrb$nombre, "(?<=\\bMC)\\s", "") %>% toupper()
uncorrb$nombre2<-str_replace(uncorrb$nombre2, "[-]", " ")
uncorrb$nombre2<-str_replace(uncorrb$nombre2, "(?<=\\bDE)\\s(?=\\w)", "cone")
uncorrb$dum<-ifelse(((uncorrb$nombre2==lag(uncorrb$nombre2))&(uncorrb$idenom!=lag(uncorrb$idenom)))|
                          ((uncorrb$nombre2==lead(uncorrb$nombre2))&(uncorrb$idenom!=lead(uncorrb$idenom))), 
                        1, 0)
uncorrb$dum[is.na(uncorrb$dum)]<-0
uncorrb$spaces<-str_count(uncorrb$nombre2, "\\s")+1
uncorr1<-uncorrb %>% subset(idenom!="pre2019")
uncorr2<-uncorrb %>% subset(idenom=="pre2019")
uncorr1$apellidos<-ifelse(uncorr1$spaces==2, str_extract(uncorr1$nombre2, "^(\\w+)"), 
                          ifelse(uncorr1$spaces>=3, str_extract(uncorr1$nombre2, "^(\\w+)(\\s+)(\\w+)"), NA))
uncorr1$nombres<-ifelse(uncorr1$spaces==2, str_replace(uncorr1$nombre2, "^(\\w+)(\\s+)", ""),
                        ifelse(uncorr1$spaces>=3, str_replace(uncorr1$nombre2, "^(\\w+)(\\s+)(\\w+)(\\s+)", ""), NA))
uncorr2$apellidos<-ifelse(uncorr2$spaces==2, str_extract(uncorr2$nombre2, "(\\w+)$"), 
                          ifelse(uncorr2$spaces>=3, str_extract(uncorr2$nombre2, "(\\w+)(\\s+)(\\w+)$"), NA))
uncorr2$nombres<-ifelse(uncorr2$spaces==2, str_replace(uncorr2$nombre2, "(\\s+)(\\w+)$", ""),
                        ifelse(uncorr2$spaces>=3, str_replace(uncorr2$nombre2, "(\\s+)(\\w+)(\\s+)(\\w+)$", ""), NA))
uncorrb<-rbind(uncorr1, uncorr2) 
uncorrb$nombres<-str_replace(uncorrb$nombres, "(?<=\\bDE)cone(?=\\w)", " ")
uncorrb$apellidos<-str_replace(uncorrb$apellidos, "(?<=\\bDE)cone(?=\\w)", " ")
uncorrb$nombre2<-str_replace(uncorrb$nombre2, "(?<=\\bDE)cone(?=\\w)", " ")
uncorrb$scorenom<-NA
uncorrb$scoreape<-NA

for(i in 1:nrow(uncorrb)){
  uncorrb$scorenom[i]<-str_count(uncorrb$nombres[i], names)/(str_count(uncorrb$nombres[i], names)+str_count(uncorrb$nombres[i], lnames))
  uncorrb$scoreape[i]<-str_count(uncorrb$apellidos[i], lnames)/(str_count(uncorrb$apellidos[i], names)+str_count(uncorrb$apellidos[i], lnames))
  print(paste(i*100/nrow(uncorrb), "%", sep=""))
}
uncorrb$scorenom<-ifelse(is.na(uncorrb$scorenom)|is.nan(uncorrb$scorenom), 0, uncorrb$scorenom)
uncorrb$scoreape<-ifelse(is.na(uncorrb$scoreape)|is.nan(uncorrb$scoreape), 0, uncorrb$scoreape)
uncorrb$score<-(uncorrb$scorenom+uncorrb$scoreape)/2
uncorrb<-uncorrb %>% mutate(dum=ifelse((scorenom>=0.5&scoreape==0)|(scoreape>=0.5), 1, 0) ) %>%
  arrange(nombre, apellidos, nombres) %>% distinct()
uncorrc<-uncorrb %>% subset(dum==1)
uncorrd<-uncorrb %>% subset(dum!=1)
uncorrf<-uncorra %>% select(-spacen) %>%
  rbind(uncorrc, uncorrd)
write_csv(uncorrf, "uncorrtempf.csv")
uncorrf<-read_csv("uncorrtempf.csv")
uncorrb<-uncorrf %>% subset(dum!=1)
uncorrb<-uncorrb %>% subset(select=c(nombre, idenom)) %>% distinct()
patterns<-c("(?<=\\bMC)\\s", "[-]") %>% paste(collapse = "|")
uncorrb$nombre2<-str_replace(uncorrb$nombre, "(?<=\\bMC)\\s", "") %>% toupper()
uncorrb$nombre2<-str_replace(uncorrb$nombre2, "[-]", " ")
uncorrb$nombre2<-str_replace(uncorrb$nombre2, "(?<=\\bDE)\\s(?=\\w)", "cone")
uncorrb$dum<-ifelse(((uncorrb$nombre2==lag(uncorrb$nombre2))&(uncorrb$idenom!=lag(uncorrb$idenom)))|
                      ((uncorrb$nombre2==lead(uncorrb$nombre2))&(uncorrb$idenom!=lead(uncorrb$idenom))), 
                    1, 0)
uncorrb$dum[is.na(uncorrb$dum)]<-0
uncorrb$spaces<-str_count(uncorrb$nombre2, "\\s")+1
uncorr1<-uncorrb %>% subset(idenom=="pre2019")
uncorr2<-uncorrb %>% subset(idenom!="pre2019")
uncorr1$apellidos<-ifelse(uncorr1$spaces==2, str_extract(uncorr1$nombre2, "^(\\w+)"), 
                          ifelse(uncorr1$spaces>=3, str_extract(uncorr1$nombre2, "^(\\w+)(\\s+)(\\w+)"), NA))
uncorr1$nombres<-ifelse(uncorr1$spaces==2, str_replace(uncorr1$nombre2, "^(\\w+)(\\s+)", ""),
                        ifelse(uncorr1$spaces>=3, str_replace(uncorr1$nombre2, "^(\\w+)(\\s+)(\\w+)(\\s+)", ""), NA))
uncorr2$apellidos<-ifelse(uncorr2$spaces==2, str_extract(uncorr2$nombre2, "(\\w+)$"), 
                          ifelse(uncorr2$spaces>=3, str_extract(uncorr2$nombre2, "(\\w+)(\\s+)(\\w+)$"), NA))
uncorr2$nombres<-ifelse(uncorr2$spaces==2, str_replace(uncorr2$nombre2, "(\\s+)(\\w+)$", ""),
                        ifelse(uncorr2$spaces>=3, str_replace(uncorr2$nombre2, "(\\s+)(\\w+)(\\s+)(\\w+)$", ""), NA))
uncorrb<-rbind(uncorr1, uncorr2) 
uncorrb$nombres<-str_replace(uncorrb$nombres, "(?<=\\bDE)cone(?=\\w)", " ")
uncorrb$apellidos<-str_replace(uncorrb$apellidos, "(?<=\\bDE)cone(?=\\w)", " ")
uncorrb$nombre2<-str_replace(uncorrb$nombre2, "(?<=\\bDE)cone(?=\\w)", " ")
uncorrb <- uncorrb %>% arrange(idenom, nombre2)
write_csv(uncorrb, "uncorrd.csv")
uncorrg<-read_csv("uncorrg.csv")
uncorrh<- uncorrf %>% subset(dum==1) %>% select(-c(scorenom, scoreape, score)) %>%
  rbind(uncorrg) %>%
  distinct() %>%
  arrange(nombre, idenom, apellidos, nombres) %>%
  mutate(dum=ifelse((nombre==lag(nombre))|(nombre==lead(nombre)), 1, 0))
write_csv(uncorrh, "uncorrh.csv")

#####3.7 together#####
catedras<-read_csv("../secihti/catedras/catedras.csv")
secihtiund<-read_csv("secihti2und.csv")
uncorr<-read_csv("uncorri.csv")
uncorr<- uncorr %>% select(nombre, nombres, apellidos) %>% distinct() %>% arrange(nombre) 
secihtiund<-secihtiund %>% select(-c(nombres, apellidos)) %>% distinct()
secihtiund2<-secihtiund %>% left_join(uncorr, by="nombre") %>% distinct()
secihtidef<-read_csv("secihti2def.csv")
catedras$convocatoria<-"catedras"
catedras$nivel<-"catedras"
catedras$program<-"catedras"
catedras$nombres<-catedras$nombres %>% stri_trans_general(id="Latin-ASCII") %>% str_to_upper()
catedras$apellido1<- catedras$apellido1 %>% stri_trans_general(id="Latin-ASCII") %>% str_to_upper()
catedras$apellido2<- catedras$apellido2 %>% stri_trans_general(id="Latin-ASCII") %>% str_to_upper()
catedras$institucion<- catedras$institucion %>% stri_trans_general(id="Latin-ASCII") %>% str_to_upper()
catedras$apellidos<-paste0(catedras$apellido1, " ", catedras$apellido2) %>% 
  str_replace_all("\\s+", " ") %>%  trimws(which="both")
catedras$nombre<-paste0(catedras$apellidos, ", ",catedras$nombres) %>% 
  str_replace_all("\\s+", " ") %>% trimws(which="both")
catedras$pais_entidad<-"MEXICO"
catedras$areasni<-"catedras"
catedras2<- catedras %>% select(intersect(colnames(catedras), colnames(secihtidef)))

secihti3<-rbind(secihtidef, secihtiund2) %>% 
  arrange(nombre, year, apellidos, nombres) %>% 
  select(-c(idenom)) %>% distinct() %>%
  plyr::rbind.fill(catedras2) %>%
  mutate(simpname=paste(apellidos, nombres, sep=", "))
length(unique(secihti3$simpname))
write_csv(secihti3, "secihti3.csv")

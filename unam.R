library(tidyverse)
library(stringi)
library(readxl)
library(readr)
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
####personal####
exceles<-list.files("../unam/personal", full.names = TRUE, recursive = TRUE)
exceles<- exceles[which(str_detect(exceles, "[.]xlsx"))]
dires<-str_extract(exceles, ".*(?=\\/)")
dires<-str_replace(dires, pattern = "personal", replacement = "personal2")
unicos<- unique(dires)
for(i in 1:length(unicos)){
  tryCatch(
    dir.create(unicos[i], recursive = TRUE),
    error = function(e) {
      message(paste("Error creating directory:", unicos[i]))
    }
  )
}
csvs<-list.files("../unam/personal", recursive = TRUE)
csvs<-csvs[which(str_detect(csvs, "[.]xlsx"))] %>%
  str_replace(pattern = ".xlsx", replacement = ".csv") %>%
  str_replace(pattern = ".*/", replacement = "") %>%
  paste0(dires,"/", .)
for(i in 1:length(exceles)){
  x<-read_excel(exceles[i], col_names = F, sheet = 1)
  write_csv(x, csvs[i], na="", col_names = F)  
}
####academicos####
exceles<-list.files("../unam/profesores", full.names = TRUE, recursive = TRUE)
exceles<- exceles[which(str_detect(exceles, "[.]xlsx"))]
dires<-str_extract(exceles, ".*(?=\\/)")
dires<-str_replace(dires, pattern = "profesores", replacement = "profesores2")
unicos<- unique(dires)
for(i in 1:length(unicos)){
  tryCatch(
    dir.create(unicos[i], recursive = TRUE),
    error = function(e) {
      message(paste("Error creating directory:", unicos[i]))
    }
  )
}
csvs<-list.files("../unam/profesores", recursive = TRUE)
csvs<-csvs[which(str_detect(csvs, "[.]xlsx"))] %>%
  str_replace(pattern = ".xlsx", replacement = ".csv") %>%
  str_replace(pattern = ".*/", replacement = "") %>%
  paste0(dires,"/", .)
for(i in 1:length(exceles)){
  x<-read_excel(exceles[i], col_names = F, sheet = 1)
  write_csv(x, csvs[i], na="", col_names = F)  
}
####second round####
#####personal#####
files<-list.files("../unam/personal2", full.names = TRUE, recursive = TRUE)
files<-files[which(str_detect(files, "[.]csv"))]
personal<-data.frame()
for(i in 1:length(files)){
  file<-read_csv(files[i])
  colnames(file)<-tolower(colnames(file)) %>% stri_trans_general("Latin-ASCII") %>% trimws(which = "both") %>%
    str_replace_all("\\s+", " ") 
  colnames(file)
  info<-c("ejercicio", "nombre", "apellido") %>% paste(collapse = "|")
  info<-colnames(file)[which(str_detect(colnames(file), info))]
  sexo<-colnames(file)[which(str_detect(colnames(file), "sexo"))][1]
  area<-c("cargo", "adscripcion") %>% paste(collapse = "|")
  area<-colnames(file)[which(str_detect(colnames(file), area))]
  monto<-colnames(file)[which(str_detect(colnames(file), "monto")&str_detect(colnames(file), "neta"))][1]
  if(is.na(monto)){
    monto<-colnames(file)[which(str_detect(colnames(file), "remuneracion")&str_detect(colnames(file), "neta"))][1]
  }
  interest<-c(info, sexo, area, monto)
  file<-file %>% select(all_of(interest))
  colnames(file)<-c("ejercicio", "nombre", "apellido1", "apellido2", "sexo","cargo", "area", "monto_neto")
  file$monto_neto<-as.numeric(str_replace_all(file$monto_neto, "[,$]", ""))
  personal<-rbind(personal, file)
}
tabla<-personal %>% group_by(ejercicio, nombre, apellido1, apellido2, sexo, cargo, area) %>%
  summarize(monto_neto=mean(monto_neto, na.rm=T)) %>%
  rename(denominacion=cargo)
tabla$estimulos<-0
write_csv(tabla, "personal.csv")

files2<-list.files("../unam/profesores2", full.names = TRUE, recursive = TRUE)
files2<-files2[which(str_detect(files2, "[.]csv"))]
profesores<-data.frame()
for(i in 1:length(files2)){
  file2<-read_csv(files2[i])
  colnames(file2)<-tolower(colnames(file2)) %>% stri_trans_general("Latin-ASCII") 
  colnames(file2)
  info<-c("ejercicio", "nombre", "apellido") %>% paste(collapse = "|")
  info<-colnames(file2)[which(str_detect(colnames(file2), info))]
  sexo<-colnames(file2)[which(str_detect(colnames(file2), "sexo"))][1]
  area<-c("unidad", "tipo\\so\\snivel") %>% paste(collapse = "|")
  area<-colnames(file2)[which(str_detect(colnames(file2), area))]
  monto<-colnames(file2)[which(str_detect(colnames(file2), "monto")&str_detect(colnames(file2), "neto"))][1]
  if(is.na(monto)){
    monto<-colnames(file2)[which(str_detect(colnames(file2), "monto")&str_detect(colnames(file2), "total"))][1]
  }
  estimulos<-colnames(file2)[which(str_detect(colnames(file2), "estimulo"))]
  if(is.na(sexo)){
    interest<-c(info,area, monto, estimulos)
    file2<-file2 %>% select(all_of(interest))
    colnames(file2)<-c("ejercicio", "nombre", "apellido1", "apellido2", "unidad", "tipo_nivel", "monto_neto", "estimulos")
    file2$sexo<-NA
  } else{
    interest<-c(info,sexo, area, monto, estimulos)  
    file2<-file2 %>% select(all_of(interest))
    colnames(file2)<-c("ejercicio", "nombre", "apellido1", "apellido2", "sexo","unidad", "tipo_nivel", "monto_neto", "estimulos")
  }
  file2$monto_neto<-as.numeric(str_replace_all(file2$monto_neto, "[,$]", ""))
  file2$estimulos<-as.numeric(str_replace_all(file2$estimulos, "[,$]", ""))
  profesores<-rbind(profesores, file2)
}
tabla2<-profesores %>% group_by(ejercicio, nombre, apellido1, apellido2, sexo, tipo_nivel, unidad) %>%
  summarize(monto_neto=mean(monto_neto, na.rm=T), estimulos=mean(estimulos, na.rm=T)) %>% 
  subset(!is.na(nombre))%>%
  rename(denominacion=tipo_nivel)
write_csv(tabla2, "profesores.csv")
#####consolidating#####
personal<-read_csv("personal.csv")
profesores<-read_csv("profesores.csv")
tabla<-personal %>% 
  select(ejercicio, nombre, apellido1, apellido2, sexo, monto_neto, estimulos, denominacion, area) %>%
  rename(universidad=area) %>%
  mutate(tipo="personal") %>%
  bind_rows(profesores %>% 
              select(ejercicio, nombre, apellido1, apellido2, sexo, monto_neto, estimulos, denominacion, unidad) %>%
              rename(universidad=unidad) %>%
              mutate(tipo="profesores")) %>%
  mutate(dump=ifelse(tipo=="profesores", 1, 0))  %>%
  group_by(nombre, apellido1, apellido2) %>%
  fill(sexo, .direction = "updown")
multi<- tabla %>%
  group_by(ejercicio, nombre, apellido1, apellido2) %>%
  summarize(dump=sum(dump)) %>% arrange(nombre, apellido1, apellido2, ejercicio)
tablab<-tabla %>% select(-dump) %>%
  left_join(multi) %>% 
  group_by(ejercicio, nombre, apellido1, apellido2) %>%
  mutate(deno2=denominacion[which(tipo=="personal")][1]) %>%
  mutate(denominacion=ifelse(!is.na(deno2), deno2, denominacion)) %>% select(-deno2) %>%
  group_by(ejercicio, nombre, apellido1, apellido2, sexo, denominacion, universidad) %>%
  summarize(monto_neto=sum(monto_neto, na.rm=T), estimulos=sum(estimulos)) %>%
  mutate(monto_total=monto_neto+estimulos)%>%
  arrange(nombre, apellido1, apellido2, sexo, ejercicio)
tablab$universidad<-paste("UNAM", tablab$universidad, sep = " - ")
write_csv(tablab, "tablaunam.csv")
unam<-read_csv("tablaunam.csv")
max(unam$ejercicio)

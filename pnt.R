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
selecterror<-function(df, columns){
  tryCatch(
    expr= {
      return(df %>% select(all_of(columns))) # If the columns are selected successfully, return TRUE
    },
    error = function(e) {
      return(data.frame(matrix(ncol=length(columns), nrow=0)))
    }
  )
}
  
####unzipping#####
#####first unzip#####
compri0<-list.files("../pnt/comprimidos")
for(i in 1:length(compri0)){
  zip<-paste0("../pnt/comprimidos/", compri0[i])
  tryCatch(
    unzip(zip, exdir = "../pnt/extraidos/extraidos0"),
    error = function(e) {
      message(paste("Error in file:", zip))
    }
  )
}
#####second unzip#####
compri1<-list.files("../pnt/extraidos/extraidos0")
dataset<-data.frame()
for(i in 1:length(compri1)){
  zip<-paste0("../pnt/extraidos/extraidos0/", compri1[i])
  tryCatch(
    unzip(zip, exdir = "../pnt/extraidos/extraidos1a"),
    error = function(e) {
      x<-data.frame(file=zip)
      dataset<- rbind(dataset, x)
      message(paste("Error in file:", zip))
    }
  )
}
write_csv(dataset, "errors1.csv")
#####third unzip#####
compri2<-list.files("../pnt/extraidos/extraidos1a")
compri2<-compri2[which(str_detect(compri2, "[.]zip"))]
dataset<-data.frame()
for(i in 1:length(compri2)){
  zip<-paste0("../pnt/extraidos/extraidos1a/", compri2[i])
  dire<-paste0("../pnt/extraidos/extraidos2a/", str_replace(compri2[i], pattern = ".zip", replacement = ""))
  tryCatch(
    unzip(zip, exdir=dire),
    error = function(e) {
      x<-data.frame(file=zip)
      dataset<- rbind(dataset, x)
      message(paste("Error in file:", zip))
    }
  )
}
#####collecting unzipped from before#####
######Jesus######
exceles<-list.files("../pnt/extraidos/extraidos1c", full.names = TRUE, recursive = TRUE)
exceles<- exceles[which(str_detect(exceles, "[.]xlsx"))]
dires<-str_extract(exceles, ".*(?=\\/)")
dires<-str_replace(dires, pattern = "extraidos1c", replacement = "extraidos2")
unicos<- unique(dires)
for(i in 1:length(unicos)){
  tryCatch(
    dir.create(unicos[i], recursive = TRUE),
    error = function(e) {
      message(paste("Error creating directory:", unicos[i]))
    }
  )
}
csvs<-list.files("../pnt/extraidos/extraidos1c", recursive = TRUE)
csvs<-csvs[which(str_detect(csvs, "[.]xlsx"))] %>%
  str_replace(pattern = ".xlsx", replacement = ".csv") %>%
  str_replace(pattern = ".*/", replacement = "") %>%
  paste0(dires,"/", .)
for(i in 1:length(exceles)){
  x<-read_excel(exceles[i], col_names = F, sheet = 1)
  write_csv(x, csvs[i], na="", col_names = F)  
}

######Aldo######
csvs2<-list.files("../pnt/extraidos/extraidos1b", recursive = TRUE)
csvs2<-csvs2[which(str_detect(csvs2, "[.]csv"))] 
nombres<-str_extract(csvs2, "^.*?\\/.*?(?=\\/)") %>%
  str_replace("\\/", "") %>% paste0("../pnt/extraidos/extraidos2/", .)
for(i in 1:length(unique(nombres))){
  dir.create(unique(nombres)[i])
}
csvs3<-str_replace(csvs2, "^.*\\/", "") %>% paste0(nombres, "/", .)
csvs2<-paste0("../pnt/extraidos/extraidos1b/", csvs2)
for(i in 1:length(csvs2)){
file.copy(csvs2[i], csvs3[i], overwrite = TRUE)
}

####reading the files#####
extracted<-list.files("../pnt/extraidos/extraidos2")
mensual<-data.frame()
estimulos<-data.frame()
errors<-c()
for(i in 1:length(extracted)){
  ######Getting general information#####
  dire<-paste0("../pnt/extraidos/extraidos2/", extracted[i])
  files<-list.files(dire)
  if(length(files) == 0){
    next
  }
  else{
    encod<-adiv_encod(paste0(dire, "/", files[which(!str_detect(files, "Tabla"))][1]))
    univers<-readLines(paste0(dire, "/", files[which(!str_detect(files, "Tabla"))][1]))[1] %>% 
      iconv(from=encod,to="UTF-8") %>%
      str_extract("(?<=[:],).*$") %>% str_trim()
    ####working the main table#####
    if(columnerror(paste0(dire, "/", files[which(!str_detect(files, "Tabla"))][1]), encod)==T){
      original<-files[which(!str_detect(files, "Tabla"))][1] %>% paste0(dire, "/",.)%>%
        read.csv(fileEncoding = encod, encoding = "UTF-8", skip=3, na.strings = c("NA", "N/A", ""))
      colnames(original)<-str_to_lower(colnames(original))
      denom<-colnames(original)[which(str_detect(colnames(original), "denominac"))][1]
      remu<-colnames(original)[which(str_detect(colnames(original), "mont[o|a]|remunerac")&str_detect(colnames(original), "net[o|a]")&str_detect(colnames(original), "mensu"))]
      if(length(remu) > 0){
        remu<-remu[1]
      }
      sex<-colnames(original)[which(str_detect(colnames(original), "sexo"))]
      nombre<-colnames(original)[which(str_detect(colnames(original), "nombre"))]
      if(length(sex)>1){
        if(length(unique(original[sex[1]]))>1){
          sex<-sex[1]
        } else{
          sex<-sex[2]
        }
      }
      if(length(nombre)>1){
        nombre<-nombre[which(!str_detect(nombre, "responsable"))]
      }
      nombre<-c(nombre, "apellido") %>%
        str_c(collapse = "|")
      nom<-colnames(original)[which(str_detect(colnames(original), nombre))]
      id<-colnames(original)[which(str_detect(colnames(original), "^id$"))]
      ejer<-colnames(original)[which(str_detect(colnames(original), "ejercicio"))]
      if(length(ejer) == 0){
        ejer<-colnames(original)[which(str_detect(colnames(original), "año"))][1]
      } else{
        ejer<-ejer[1]
      }
      prin<-selecterror(original, c(id, ejer, nom,denom, sex))
      if(nrow(prin) == 0){
        message(paste("Error reading main table in file:", files[which(!str_detect(files, "Tabla"))][1]))
      } else if(length(sex) == 0) {
        prin$sexo<-NA
        colnames(prin)<-c("id", "ejercicio", "nombre", "primerapellido","segundoapellido", "denominacion", "sexo")
        prin$id<- as.character(prin$id)
        prin<-prin %>%
          subset(!is.na(nombre)) 
      } else{
        colnames(prin)<-c("id", "ejercicio", "nombre", "primerapellido","segundoapellido", "denominacion", "sexo")
        prin$id<- as.character(prin$id)
        prin<-prin %>%
          subset(!is.na(nombre))  
      }
      if(nrow(prin) > 0& length(remu) > 0){
        remus<-original %>% select(all_of(c("id", remu))) %>% 
          distinct() 
        colnames(remus)<-c("id", "mensual_neta")
        remus$mensual_neta<- as.numeric(remus$mensual_neta)
        remus<-remus %>% subset(!is.na(id)&!is.na(mensual_neta)) %>% distinct()
        remus$id<- as.character(remus$id)
        remus<-remus %>% group_by(id) %>% summarize(mensual_neta = sum(mensual_neta, na.rm = TRUE)) 
        main<-prin %>% left_join(remus, by = "id")
        main$universidad<-univers
        ####working with the alternate tables#####
        detect<-files[which(str_detect(files, "Tabla"))]
        detect<-paste0(dire, "/", detect)
        encod2<-adiv_encod(detect[1])
        extras<-data.frame()
        for(j in 1:length(detect)){
          tablae<- columnerror(detect[j], encod2, skip=1)
          if(tablae == F){
            errors<-c(errors, detect[j])
            message(paste("Error reading file:", detect[j]))
            next
          }
          else{
            tabla<-read.csv(detect[j], fileEncoding = encod2, 
                            encoding = "UTF-8", skip=1, na.strings = c("NA", "N/A", ""))
            colnames(tabla)<-str_to_lower(colnames(tabla))
            monto<-colnames(tabla)[which(str_detect(colnames(tabla), "mont[o|a]|remunerac|ingres"))]
            if(length(monto) == 0){
              next
            } else{
              if(length(monto) > 1){
                monto<-colnames(tabla)[which(str_detect(colnames(tabla), "net[o|a]"))][1]
              }
              interest<-c("id", "perio") %>%    str_c(collapse = "|")
              interest<-colnames(tabla)[which(str_detect(colnames(tabla), interest))]
              interest<-c(interest, monto)
              tabla<-tabla %>% select(all_of(interest)) %>% distinct() 
              colnames(tabla)<-c("id", "periodo", "monto")
            }
            extras<- rbind(extras, tabla)
          }
          tabla<-data.frame()
        }
        colnames(extras)<-str_to_lower(colnames(extras))
        monto<-colnames(extras)[which(str_detect(colnames(extras), "mont[o|a]|remunerac|ingres"))]
        if(length(monto) > 1){
          monto<-colnames(extras)[which(str_detect(colnames(extras), "net[o|a]"))][1]
        }
        interest<-c("id", "perio") %>%    str_c(collapse = "|")
        interest<-colnames(extras)[which(str_detect(colnames(extras), interest))]
        interest<-c(interest, monto)
        extras<-extras %>% select(all_of(interest)) %>% distinct() 
        if(nrow(extras) == 0){
          next
        } else{
          colnames(extras)<-c("id", "periodo", "monto")
          extras$monto<- as.numeric(extras$monto)
          extras<- extras%>%
            group_by(id, periodo) %>% summarize(extra = sum(monto, na.rm = TRUE))
          extras$id<- as.character(extras$id)
          extras<-prin %>% left_join(extras, by = "id")
          extras$universidad<-univers  
        }
        ####consolidating#####
        mensual<-mensual %>% rbind(main)
        estimulos<-estimulos %>% rbind(extras)  
      } else{
        next
      }
    } else{
      next
    }
  }
  print(paste("Processed:", i, "of", length(extracted), "files."))
}
mensual$nombre<-stri_trans_general(mensual$nombre, "latin-ascii")
mensual$primerapellido<-mensual$primerapellido %>%stri_trans_general("latin-ascii")
mensual$segundoapellido<-mensual$segundoapellido %>%stri_trans_general("latin-ascii")
mensual$denominacion<-mensual$denominacion%>%stri_trans_general("latin-ascii")
estimulos$nombre<-stri_trans_general(estimulos$nombre, "latin-ascii")
estimulos$primerapellido<-estimulos$primerapellido %>%stri_trans_general("latin-ascii")
estimulos$segundoapellido<-estimulos$segundoapellido %>%stri_trans_general("latin-ascii")
estimulos$denominacion<-estimulos$denominacion%>%stri_trans_general("latin-ascii")
errors<-data.frame(errors=errors)
write_csv(errors, "errors.csv")
write_csv(mensual, "mensual.csv")
write_csv(estimulos, "estimulos.csv")

####consolidating####
salarios<-read_csv("mensual.csv")
estimulos<-read_csv("estimulos.csv")
estimulos<-estimulos %>% subset(!is.na(extra)&extra>0) %>%distinct()
estimulos$period<-str_to_lower(estimulos$periodo) %>% str_trim()
meses<-c("enero", "febrero", "marzo", "abril", "mayo", "junio", 
          "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre") %>% paste(collapse="|")
estimulos$dum<-str_detect(estimulos$period, meses)
estimulos$period<-str_replace_all(estimulos$period, pattern = meses, replacement = "")
estimulos$period<-str_replace(estimulos$period, pattern = "/", replacement = "")%>% str_trim()
multi<-estimulos %>% subset(dum==T)
colnames(multi)
multi2<-multi %>% 
  group_by(id, ejercicio, nombre, primerapellido, segundoapellido, sexo, denominacion, period, universidad) %>% 
  summarize(extra = mean(extra, na.rm = TRUE), .groups = "keep") %>%
  ungroup() %>% select(-id) %>%
  mutate(periodo=period)%>%
  distinct() 
estimulos2<-estimulos %>% subset(dum==F, select=-c(id, dum)) %>%
  distinct() %>% rbind(multi2)

anual<-c("anual", "1\\svez", "año", "una\\svez", "mes\\de", "nica\\s", "evento") %>% paste(collapse = "|")
semestral<-c("semestral", "2\\sveces") %>% paste(collapse = "|")
trimestral<-c("trimestral", "4\\sveces") %>% paste(collapse = "|")
bimestral<-c("bimestral", "6\\sveces") %>% paste(collapse = "|")
mensual<-c("mensual", "12\\sveces") %>% paste(collapse = "|")
quincenal<-c("quincenal", "bimensual") %>% paste(collapse = "|")
estimulos2$periodo<-str_to_lower(estimulos2$periodo) %>% str_trim()
estimulos2$extramensual<-ifelse(str_detect(estimulos2$period, anual), estimulos2$extra/12,
                                      ifelse(str_detect(estimulos2$period, semestral), estimulos2$extra/6,
                                             ifelse(str_detect(estimulos2$period, trimestral), estimulos2$extra/4,
                                                    ifelse(str_detect(estimulos2$period, bimestral), estimulos2$extra/2,
                                                           ifelse(str_detect(estimulos2$period, mensual), estimulos2$extra,
                                                                  ifelse(str_detect(estimulos2$period, quincenal), estimulos2$extra, 0))))))
estimulos2<- estimulos2 %>%
  group_by(ejercicio, nombre, primerapellido, segundoapellido, sexo, denominacion, period, universidad) %>%
  mutate(dumext2=n())

multi3<-estimulos2 %>% subset(dumext2>1) %>%
  select(-dumext2) %>% distinct()
estimulosuniq<-estimulos2 %>% subset(dumext2==1) %>%
  select(-dumext2) %>% distinct()

multi3<- multi3 %>%
  group_by(ejercicio, nombre, primerapellido, segundoapellido, sexo, denominacion, period, universidad) %>%
  mutate(dumext=ifelse(extramensual > median(extramensual)*0.9 & 
                         extramensual < median(extramensual)*1.1, 1, 0))

multi3<-multi3 %>% 
  group_by(ejercicio, nombre, primerapellido, segundoapellido, sexo, denominacion, period, universidad, dumext) %>%
  mutate(extramensual = ifelse(dumext==1, mean(extramensual, na.rm = TRUE), extramensual), 
         extra= ifelse(dumext==1, mean(extra, na.rm = TRUE), extra)) %>%
  select(-dumext) %>%
  distinct()

estimulos3<-rbind(estimulosuniq, multi3)

estimulos3$extramensual<-ifelse(estimulos3$extramensual > 450000|
                                  (estimulos3$extramensual > 250000 & str_detect(estimulos3$universidad, "NORTE\\sDE\\sAGUAS")), 
                                0, estimulos3$extramensual)
estimulos3<- estimulos3 %>% subset(extramensual>10) %>%
  mutate(nombre=tolower(nombre), primerapellido=tolower(primerapellido),segundoapellido=tolower(segundoapellido)) %>% 
  distinct()

salarios2<- salarios %>% select(-id) %>% 
  mutate(nombre=tolower(nombre), primerapellido=tolower(primerapellido),
         segundoapellido=tolower(segundoapellido), denominacion=str_to_lower(denominacion)) %>% 
  distinct()
tablaest<- estimulos3 %>% 
  group_by(ejercicio, universidad, nombre, primerapellido, segundoapellido, sexo, denominacion) %>%
  summarize(extramensual = sum(abs(extramensual), na.rm = TRUE)) %>%
  ungroup()
tablasal<-salarios2 %>% 
  group_by(ejercicio, universidad, nombre, primerapellido, segundoapellido, sexo, denominacion) %>%
  summarize(mensual_neta=mean(abs(mensual_neta), na.rm=T)) %>% ungroup()
tabla<-left_join(tablasal, tablaest)

tabla<- tabla %>%
  group_by(ejercicio, universidad, nombre, primerapellido, segundoapellido, sexo, denominacion) %>%
  summarize(monto_neto = sum(mensual_neta, na.rm = TRUE),
            estimulos = sum(extramensual, na.rm = TRUE), 
            .groups="keep") %>% 
  mutate(monto_total = monto_neto + estimulos) %>%
  distinct()
tabla$universidad<-tabla$universidad %>% stri_trans_general(id="LATIN-ASCII") %>% str_to_lower()
tabla$denominacion<-tabla$denominacion%>% stri_trans_general(id="LATIN-ASCII") %>% str_to_lower()
write_csv(tabla, "tablapnt.csv")
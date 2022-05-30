setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/fontes")
library(tidyr)
options(scipen=999)

######
##### DAP
#######                                   

## PARTE 001

dapJS21 = readxl::read_excel("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-DAP/Arquivos originais enviados via LAI pelo Secretaria AF/2021_09___Relatorio_DAP_PJ___Singular.xlsx",skip = 2)
colnames(dapJS21)
dapJS21A<-dapJS21%>%dplyr::select(5,8,6,9,7)%>%dplyr::mutate(tipo=NA)
colnames(dapJS21A)<-c("identificador_fornec","ibge","nome_fornec","municipio_fornec","uf_fornec","tipo")

dapJC21 = readxl::read_excel("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-DAP/Arquivos originais enviados via LAI pelo Secretaria AF/2021_09___Relatorio_DAP_PJ___Central.xlsx",skip = 2)
colnames(dapJC21)
dapJC21A<-dapJC21%>%dplyr::select(4,7,5,8,6)%>%dplyr::mutate(tipo=NA)
colnames(dapJC21A)<-c("identificador_fornec","ibge","nome_fornec","municipio_fornec","uf_fornec","tipo")

dapF21 = read.csv2("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-DAP/Arquivos originais enviados via LAI pelo Secretaria AF/cafdapativa.csv")
colnames(dapF21)
dapF21A<-dapF21%>%dplyr::select(12,10,13,7,6)
colnames(dapF21A)
dapF21B <- dapF21A %>% pivot_longer(!c(3:5), names_to = "Tipo", values_to = "identificador_fornec")
head(dapF21B)
nrow(dapF21A)
nrow(dapF21B)
dapF21C <- dapF21B %>% dplyr::mutate(ibge=NA)
colnames(dapF21C)
dapF21D <- dapF21C %>% dplyr::select(5,6,1,2,3,4)
colnames(dapF21D)<-c("identificador_fornec","ibge","nome_fornec","municipio_fornec","uf_fornec","tipo")
head(dapF21D)
nrow(dapJS21A)
nrow(dapJC21A)
nrow(dapF21D)
## > nrow(dapJS21A)
## [1] 7732
## > nrow(dapJC21A)
## [1] 36
## > nrow(dapF21D)
## [1] 4403288

colnames(dapJS21A)
colnames(dapJC21A)
colnames(dapF21D)

head(dapJS21A)
head(dapJC21A)
head(dapF21D)

identificador_001<-rbind(dapJS21A,dapJC21A, dapF21D)
save(identificador_001,file="identificador_001.Rda")

## PARTE 002

dap19 = readxl::read_excel("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-DAP/DAP19.xlsx",skip = 0)
dap17s = read.csv2("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-DAP/dap17_pj_singular.csv",sep = ";")
dap17c = read.csv2("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-DAP/dap17_pj_central.csv",sep = ";")
head(dap19)
head(dap17s)
nrow(dap17c)

# > nrow(dap19)
# [1] 7732
# > nrow(dap17s)
# [1] 5756
# > nrow(dap17c)
# [1] 10

dap19<-dap19%>%dplyr::select(1,4,2,5,3)%>%dplyr::mutate(tipo=NA)
colnames(dap19)<-c("identificador_fornec","ibge","nome_fornec","municipio_fornec","uf_fornec","tipo")
dap17s<-dap17s%>%dplyr::select(5,8,6,9,7,4)
colnames(dap17s)<-c("identificador_fornec","ibge","nome_fornec","municipio_fornec","uf_fornec","tipo")
dap17c<-dap17c%>%dplyr::select(4,7,5,8,6)%>%dplyr::mutate(tipo=NA)
colnames(dap17c)<-c("identificador_fornec","ibge","nome_fornec","municipio_fornec","uf_fornec","tipo")

identificador_002<-rbind(dap19, dap17s,dap17c)

save(identificador_002,file = "identificador_002.Rda")

## PARTES 001 e 002 IDENTIFICADOR

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")

load("identificador_001.Rda")
load("identificador_002.Rda")

head(identificador_001)

identificador<-rbind(identificador_001,identificador_002)
head(identificador)

nrow(identificador_001)
nrow(identificador_002)
nrow(identificador)

#remove acentuação e pontuação
identificador$municipio_fornec = stringi::stri_trans_general(str = identificador$municipio_fornec, id = "Latin-ASCII")
identificador$municipio_fornec = stringr::str_replace_all(identificador$municipio_fornec, "(?![_])[[:punct:]]", "")

#lowercase
identificador$municipio_fornec<-tolower(identificador$municipio_fornec)

#remove acentuação e pontuação
identificador$identificador_fornec = stringi::stri_trans_general(str = identificador$identificador_fornec, id = "Latin-ASCII")
identificador$identificador_fornec = stringr::str_replace_all(identificador$identificador_fornec, "(?![_])[[:punct:]]", "")





identificador<-unique(identificador)

colnames(identificador)<-c("identificador_fornec","ibge","nome_af","municipio_fornec","uf_fornec" )

save(identificador,file = "identificador.Rda")

######
#### FNDE
#########

load("todos.Rda")
fnde_af_lai <- todos %>% dplyr::select(2:3,5:7,8:11)
colnames(fnde_af_lai)<-c("ano","uf_eex","municipio_eex","identificador_fornec","nome_fornec","item","quant","unid","valor")

#corrige valor p/ formato com ponto
strip <- function(x){
  z <- gsub("[^0-9,.]", "", x)
  z <- gsub("\\.", "", z)
  gsub(",", ".", z)
}

fnde_af_lai$valor<-as.numeric(strip(fnde_af_lai$valor))

save(fnde_af_lai,file="fnde_af_lai.Rda")

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC")
load("fnde_af_lai.Rda")

fnde_af_lai_002<-fnde_af_lai

fnde_af_lai_2013 <-fnde_af_lai_002 %>% dplyr::filter(ano == 2013) 
fnde_af_lai_2014 <-fnde_af_lai_002 %>% dplyr::filter(ano == 2014) 
fnde_af_lai_2015 <-fnde_af_lai_002 %>% dplyr::filter(ano == 2015) 
fnde_af_lai_2016 <-fnde_af_lai_002 %>% dplyr::filter(ano == 2016) 
fnde_af_lai_2017 <-fnde_af_lai_002 %>% dplyr::filter(ano == 2017) 
fnde_af_lai_2018 <-fnde_af_lai_002 %>% dplyr::filter(ano == 2018) 
fnde_af_lai_2019 <-fnde_af_lai_002 %>% dplyr::filter(ano == 2019) 
fnde_af_lai_2020 <-fnde_af_lai_002 %>% dplyr::filter(ano == 2020) 

colnames(fnde_af_lai_2013)

fnde_af_lai_2013a	<-  fnde_af_lai_2013[,c(1,2,3,4,5,6,9)]
fnde_af_lai_2014a	<-	fnde_af_lai_2014[,c(1,2,3,4,5,6,9)]
fnde_af_lai_2015a	<-	fnde_af_lai_2015[,c(1,2,3,4,5,6,9)]
fnde_af_lai_2016a	<-	fnde_af_lai_2016[,c(1,2,3,4,5,6,9)]
fnde_af_lai_2017a	<-	fnde_af_lai_2017[,c(1,2,3,4,5,6,9)]
fnde_af_lai_2018a	<-	fnde_af_lai_2018[,c(1,2,3,4,5,6,9)]
fnde_af_lai_2019a	<-	fnde_af_lai_2019[,c(1,2,3,4,5,6,9)]
fnde_af_lai_2020a	<-	fnde_af_lai_2020[,c(1,2,3,4,5,6,9)]

head(fnde_af_lai_2013a)
head(fnde_af_lai_2014a)

fnde_af_lai_2013a$identificador_fornec<-as.numeric(fnde_af_lai_2013$identificador_fornec)
fnde_af_lai_2014a$identificador_fornec<-as.numeric(fnde_af_lai_2014$identificador_fornec)
fnde_af_lai_2015a$identificador_fornec<-as.numeric(fnde_af_lai_2015$identificador_fornec)
fnde_af_lai_2016a$identificador_fornec<-as.numeric(fnde_af_lai_2016$identificador_fornec)
fnde_af_lai_2017a$identificador_fornec<-as.numeric(fnde_af_lai_2017$identificador_fornec)
fnde_af_lai_2018a$identificador_fornec<-as.numeric(fnde_af_lai_2018$identificador_fornec)
fnde_af_lai_2019a$identificador_fornec<-as.numeric(fnde_af_lai_2019$identificador_fornec)
fnde_af_lai_2020a$identificador_fornec<-as.numeric(fnde_af_lai_2020$identificador_fornec)

head(fnde_af_lai_2013a)

load("identificador.Rda")

head(identificador)
identificador <- identificador[,c(1:5)]
identificador$identificador_fornec<-as.numeric(identificador$identificador_fornec)

fnde_id_2013<- dplyr::left_join(fnde_af_lai_2013a,identificador, by = 'identificador_fornec')
fnde_id_2014<- dplyr::left_join(fnde_af_lai_2014a,identificador, by = 'identificador_fornec')
fnde_id_2015<- dplyr::left_join(fnde_af_lai_2015a,identificador, by = 'identificador_fornec')
fnde_id_2016<- dplyr::left_join(fnde_af_lai_2016a,identificador, by = 'identificador_fornec')
fnde_id_2017<- dplyr::left_join(fnde_af_lai_2017a,identificador, by = 'identificador_fornec')
fnde_id_2018<- dplyr::left_join(fnde_af_lai_2018a,identificador, by = 'identificador_fornec')
fnde_id_2019<- dplyr::left_join(fnde_af_lai_2019a,identificador, by = 'identificador_fornec')
fnde_id_2020<- dplyr::left_join(fnde_af_lai_2020a,identificador, by = 'identificador_fornec')

head(fnde_id_2013)

save(fnde_id_2013,file = "fnde_id_2013.Rda")
save(fnde_id_2014,file = "fnde_id_2014.Rda")
save(fnde_id_2015,file = "fnde_id_2015.Rda")
save(fnde_id_2016,file = "fnde_id_2016.Rda")
save(fnde_id_2017,file = "fnde_id_2017.Rda")
save(fnde_id_2018,file = "fnde_id_2018.Rda")
save(fnde_id_2019,file = "fnde_id_2019.Rda")
save(fnde_id_2020,file = "fnde_id_2020.Rda")

head(fnde_id_2013)

#identifica NAs -> retorna % de linhas/itens cujo município de origem está identificado 
1-((as.data.frame(sapply(fnde_id_2013, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2013))
1-((as.data.frame(sapply(fnde_id_2014, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2014))
1-((as.data.frame(sapply(fnde_id_2015, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2015))
1-((as.data.frame(sapply(fnde_id_2016, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2016))
1-((as.data.frame(sapply(fnde_id_2017, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2017))
1-((as.data.frame(sapply(fnde_id_2018, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2018))
1-((as.data.frame(sapply(fnde_id_2019, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2019))
1-((as.data.frame(sapply(fnde_id_2020, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2020))

# > 1-((as.data.frame(sapply(fnde_id_2013, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2013))
# [1] 0.6942023
# > 1-((as.data.frame(sapply(fnde_id_2014, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2014))
# [1] 0.6626401
# > 1-((as.data.frame(sapply(fnde_id_2015, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2015))
# [1] 0.7085365
# > 1-((as.data.frame(sapply(fnde_id_2016, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2016))
# [1] 0.6921722
# > 1-((as.data.frame(sapply(fnde_id_2017, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2017))
# [1] 0.6947031
# > 1-((as.data.frame(sapply(fnde_id_2018, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2018))
# [1] 0.618064
# > 1-((as.data.frame(sapply(fnde_id_2019, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2019))
# [1] 0.6368576
# > 1-((as.data.frame(sapply(fnde_id_2020, function(x) sum(length(which(is.na(x))))))[c(9),])/nrow(fnde_id_2020))
# [1] 0.7233448

#identifica NAs -> retorna % de linhas/itens cujo município de origem está identificado 

(sum((fnde_id_2013[!is.na(fnde_id_2013$uf_fornec),])$valor))/sum(fnde_id_2013$valor)
(sum((fnde_id_2014[!is.na(fnde_id_2014$uf_fornec),])$valor))/sum(fnde_id_2014$valor)
(sum((fnde_id_2015[!is.na(fnde_id_2015$uf_fornec),])$valor))/sum(fnde_id_2015$valor)
(sum((fnde_id_2016[!is.na(fnde_id_2016$uf_fornec),])$valor))/sum(fnde_id_2016$valor)
(sum((fnde_id_2017[!is.na(fnde_id_2017$uf_fornec),])$valor))/sum(fnde_id_2017$valor)
(sum((fnde_id_2018[!is.na(fnde_id_2018$uf_fornec),])$valor))/sum(fnde_id_2018$valor)
(sum((fnde_id_2019[!is.na(fnde_id_2019$uf_fornec),])$valor))/sum(fnde_id_2019$valor)
(sum((fnde_id_2020[!is.na(fnde_id_2020$uf_fornec),])$valor))/sum(fnde_id_2020$valor)

# > (sum((fnde_id_2013[!is.na(fnde_id_2013$uf_fornec),])$valor))/sum(fnde_id_2013$valor)
# [1] 0.7875299
# > (sum((fnde_id_2014[!is.na(fnde_id_2014$uf_fornec),])$valor))/sum(fnde_id_2014$valor)
# [1] 0.7619446
# > (sum((fnde_id_2015[!is.na(fnde_id_2015$uf_fornec),])$valor))/sum(fnde_id_2015$valor)
# [1] 0.7704653
# > (sum((fnde_id_2016[!is.na(fnde_id_2016$uf_fornec),])$valor))/sum(fnde_id_2016$valor)
# [1] 0.7907485
# > (sum((fnde_id_2017[!is.na(fnde_id_2017$uf_fornec),])$valor))/sum(fnde_id_2017$valor)
# [1] 0.8196545
# > (sum((fnde_id_2018[!is.na(fnde_id_2018$uf_fornec),])$valor))/sum(fnde_id_2018$valor)
# [1] 0.7227296
# > (sum((fnde_id_2019[!is.na(fnde_id_2019$uf_fornec),])$valor))/sum(fnde_id_2019$valor)
# [1] 0.7414739
# > (sum((fnde_id_2020[!is.na(fnde_id_2020$uf_fornec),])$valor))/sum(fnde_id_2020$valor)
# [1] 0.8982264


setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC")

load("fnde_id_2013.Rda")
load("fnde_id_2014.Rda")
load("fnde_id_2015.Rda")
load("fnde_id_2016.Rda")
load("fnde_id_2017.Rda")
load("fnde_id_2018.Rda")
load("fnde_id_2019.Rda")
load("fnde_id_2020.Rda")

nrow(fnde_id_2013)
nrow(fnde_id_2014)
nrow(fnde_id_2015)
nrow(fnde_id_2016)
nrow(fnde_id_2017)
nrow(fnde_id_2018)
nrow(fnde_id_2019)
nrow(fnde_id_2020)

colnames(fnde_id_2013)
colnames(fnde_id_2014)
colnames(fnde_id_2015)
colnames(fnde_id_2016)
colnames(fnde_id_2017)
colnames(fnde_id_2018)
colnames(fnde_id_2019)
colnames(fnde_id_2020)

fnde_id_total<-rbind(fnde_id_2013,fnde_id_2014,
                     fnde_id_2015,fnde_id_2016,
                     fnde_id_2017,fnde_id_2018,
                     fnde_id_2019,fnde_id_2020)

head(fnde_id_total)


save(fnde_id_total,file = "fnde_id_total.Rda")

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
load("fnde_id_total.Rda")


########
######## IDENFICAÇÃO DOS MUNICÍPIO DE ORIGEM
##########

#################
### PRIMEIRA RODADA | C/ BASE DISPONÍVEL
######################

library(tidyr)
geocod = readxl::read_excel("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-Dados AF disponíveis no site FNDE/geocod_mar2022.xlsx")#,skip = 2

head(geocod)

geocod_001<-geocod[,c(1,2,3,8)]
colnames(geocod_001)<-c("geocod","uf_eex_2","municipio_eex_2","municipio_eex")
geocod_002<-geocod_001
head(geocod_002)
nrow(geocod_002)

#remove acentuação e pontuação
geocod_002$municipio_eex = stringi::stri_trans_general(str = geocod_002$municipio_eex, id = "Latin-ASCII")
geocod_002$municipio_eex = stringr::str_replace_all(geocod_002$municipio_eex, "(?![_])[[:punct:]]", "")
geocod_002$municipio_eex_2 = stringi::stri_trans_general(str = geocod_002$municipio_eex_2, id = "Latin-ASCII")
geocod_002$municipio_eex_2 = stringr::str_replace_all(geocod_002$municipio_eex_2, "(?![_])[[:punct:]]", "")

#lowercase
geocod_002$municipio_eex<-tolower(geocod_002$municipio_eex)
geocod_002$municipio_eex_2<-tolower(geocod_002$municipio_eex_2)

head(geocod_002)
nrow(geocod_002)

geocod_002$eex2municipio_uf <- paste(geocod_002$municipio_eex_2,geocod_002$uf_eex_2,sep = "_")
geocod_002$eexmunicipio_uf <- paste(geocod_002$municipio_eex,geocod_002$uf_eex_2,sep = "_")

head(geocod_002)

geocod_002<-geocod_002[,c(1,5,6)]
nrow(geocod_002)

save(geocod_002, file = "geocod_002.Rda")

#dados fnde - informacoes eex e fornecedores 
fnde_id_total_a<-fnde_id_total%>%dplyr::select(1:3,6:8,10:11)

fnde_id_total_a_1<-fnde_id_total_a#[c(1:10000000),]
head(fnde_id_total_a_1)
fnde_id_total_a_1$eexmunicipio_uf <- paste(fnde_id_total_a_1$municipio_eex,fnde_id_total_a_1$uf_eex,sep = "_")
fnde_id_total_a_1$fornmunicipio_uf <- paste(fnde_id_total_a_1$municipio_fornec,fnde_id_total_a_1$uf_fornec,sep = "_")

fnde_id_total_a_2<-fnde_id_total_a_1[,c(9,10,6,1,4,5)]

head(fnde_id_total_a_2)

save(fnde_id_total_a_2,file="fnde_id_total_a_2.Rda")

load("fnde_id_total_a_2.Rda")

head(fnde_id_total_a_2)

###########

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
load("fnde_id_total.Rda")

load("geocod_002.Rda")

colnames(fnde_id_total)
fnde_id_total$municipio_eex<-tolower(fnde_id_total$municipio_eex)

head(geocod_002)
head(fnde_id_total_a_2)

fnde_id_total_a_2_1<-fnde_id_total_a_2[c(1:10000000),]
sigpc_geocod_1<-merge(fnde_id_total_a_2_1,geocod_002,by="eexmunicipio_uf")

fnde_id_total_a_2_2<-fnde_id_total_a_2[c(10000001:20000000),]
sigpc_geocod_2<-merge(fnde_id_total_a_2_2,geocod_002,by="eexmunicipio_uf")

nrow(fnde_id_total_a_2)-20000000

head(sigpc_geocod_2)

save(sigpc_geocod, file = "sigpc_geocod.Rda")

# write.csv(sigpc_geocod,"G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese/sigpc_geocod.csv")

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")

load("sigpc_geocod.Rda")

head(sigpc_geocod)

sigpc_geocod_select<-sigpc_geocod[,c(1,3,7:11)]

head(sigpc_geocod_select)
nrow(sigpc_geocod_select)

sigpc_geocod_001<-sigpc_geocod_select[c(1:5000000),]
sigpc_geocod_002<-sigpc_geocod_select[c(5000000:10000000),]
sigpc_geocod_003<-sigpc_geocod_select[c(10000000:12000000),]
sigpc_geocod_004<-sigpc_geocod_select[c(12000000:13000000),]
sigpc_geocod_005<-sigpc_geocod_select[c(13000000:15000000),]
sigpc_geocod_006<-sigpc_geocod_select[c(15000000:25000000),]
nrow(sigpc_geocod_select)-25000000

sigpc_geocod_unique_001<-unique(sigpc_geocod_001)
sigpc_geocod_unique_002<-unique(sigpc_geocod_002)
sigpc_geocod_unique_003<-unique(sigpc_geocod_003)
sigpc_geocod_unique_004<-unique(sigpc_geocod_004)
sigpc_geocod_unique_005<-unique(sigpc_geocod_005)
sigpc_geocod_unique_006<-unique(sigpc_geocod_006)

nrow(sigpc_geocod_select)
nrow(sigpc_geocod_unique_001)
nrow(sigpc_geocod_unique_002)
nrow(sigpc_geocod_unique_003)
nrow(sigpc_geocod_unique_004)
nrow(sigpc_geocod_unique_005)
nrow(sigpc_geocod_unique_006)

sigpc_geocod_unique<-rbind(sigpc_geocod_unique_001,sigpc_geocod_unique_002,sigpc_geocod_unique_003,sigpc_geocod_unique_004,
                           sigpc_geocod_unique_005,sigpc_geocod_unique_006)

sigpc_geocod_unique<-unique(sigpc_geocod_unique)

nrow(sigpc_geocod_unique)

head(sigpc_geocod_unique)

save(sigpc_geocod_unique, file = "sigpc_geocod_unique.Rda")

write.csv(sigpc_geocod_unique,
          "G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese/sigpc_geocod_unique.csv")

#################
### SEGUNDA RODADA | C/ RETIFICAÇÃO
######################

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
load("fnde_id_total.Rda")
nrow(fnde_id_total)

fnde_id_total$municipio_eex<-tolower(fnde_id_total$municipio_eex)
nrow(fnde_id_total)-30000000
fnde_id_total_1<-fnde_id_total[c(1:10000000),]
colnames(fnde_id_total_1)
fnde_id_total_2<-fnde_id_total_1 %>%
  dplyr::group_by(ano,uf_eex,municipio_eex,identificador_fornec,nome_fornec,item,
                  ibge,nome_af,municipio_fornec,uf_fornec) %>%
  dplyr::summarise_at(dplyr::vars(valor), dplyr::funs(sum="sum"(., na.rm=TRUE)))

head(fnde_id_total_2)

fnde_id_total_A<-fnde_id_total[c(10000000:20000000),]
colnames(fnde_id_total_A)
fnde_id_total_B<-fnde_id_total_A %>%
  dplyr::group_by(ano,uf_eex,municipio_eex,identificador_fornec,nome_fornec,item,
                  ibge,nome_af,municipio_fornec,uf_fornec) %>%
  dplyr::summarise_at(dplyr::vars(valor), dplyr::funs(sum="sum"(., na.rm=TRUE)))

head(fnde_id_total_B)

fnde_id_total_i<-fnde_id_total[c(20000000:30000000),]
colnames(fnde_id_total_i)
fnde_id_total_ii<-fnde_id_total_i %>%
  dplyr::group_by(ano,uf_eex,municipio_eex,identificador_fornec,nome_fornec,item,
                  ibge,nome_af,municipio_fornec,uf_fornec) %>%
  dplyr::summarise_at(dplyr::vars(valor), dplyr::funs(sum="sum"(., na.rm=TRUE)))

head(fnde_id_total_ii)

head(fnde_id_total_2)
head(fnde_id_total_B)
head(fnde_id_total_ii)

fnde_id_total_sum<-rbind(fnde_id_total_2,fnde_id_total_B,fnde_id_total_ii)

save(fnde_id_total_sum,file="fnde_id_total_sum.Rda")

###############
### MERGE 1
###################

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")

load("fnde_id_total_sum.Rda")
load("geocod_002.Rda")

colnames(fnde_id_total_sum)
head(fnde_id_total_sum)
nrow(fnde_id_total_sum)

fnde_id_total_sum$eexmunicipio_uf <- paste(fnde_id_total_sum$municipio_eex,fnde_id_total_sum$uf_eex,sep = "_")
fnde_id_total_sum$fornmunicipio_uf <- paste(fnde_id_total_sum$municipio_fornec,fnde_id_total_sum$uf_fornec,sep = "_")

colnames(fnde_id_total_sum)

fnde_id_total_sum_selec<-fnde_id_total_sum[,c(12,13,4,5,7,6,1,11)]
colnames(fnde_id_total_sum_selec)

save(fnde_id_total_sum_selec,file="fnde_id_total_sum_selec.Rda")
save(geocod_002,file="geocod_002.Rda")

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")

load("fnde_id_total_sum_selec.Rda")
load("geocod_002.Rda")

fnde_id_total_sum_1<-fnde_id_total_sum_selec#[c(1:1000000),]

fnde_origemforn <- merge(x=fnde_id_total_sum_1,y=geocod_002,by="eexmunicipio_uf",all.x=TRUE)

head(fnde_origemforn)
nrow(fnde_origemforn1)/nrow(fnde_origemforn)

fnde_origemforn1 <- fnde_origemforn %>% dplyr::filter(is.na(eex2municipio_uf))

head(fnde_origemforn)
fnde_origemforn_faltantes<-fnde_origemforn[,c(1,9,10)]
fnde_origemforn_faltantes_unique<-unique(fnde_origemforn_faltantes)
View(fnde_origemforn_faltantes_unique)
fnde_origemforn_faltantes_unique_1<-fnde_origemforn_faltantes_unique
write.csv(fnde_origemforn_faltantes_unique_1,
          "G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese/fnde_origemforn_faltantes_unique_1.csv")

#######################################
########MERGE 2
############################

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
load("fnde_id_total_sum_selec.Rda")
head(fnde_id_total_sum_selec)
load("geocod_002.Rda")
head(geocod_002)
View(geocod_002)
geocod_003 = read.csv2("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese/geocod_consolidado.csv",sep = ";")

fnde_id_total_sum_1<-fnde_id_total_sum[c(1:1000),]

fnde_origem <- merge(x=fnde_id_total_sum_selec,y=geocod_003,by="eexmunicipio_uf",all.x=TRUE)

save(fnde_origem,file="fnde_origem.Rda")

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")

colnames(fnde_origem)<-c("eexmunicipio_uf","fornmunicipio_uf",
                         "identificador_fornec",
                          "fornecnome","fornecgeocod","item",
                          "ano","sum","eexgeocod",
                          "eex2municipio_uf" )

load("fnde_origem.Rda")

head(fnde_origem)

View(fnde_origem)

sum(fnde_origem_na$sum)/sum(fnde_origem$sum)
# [1] 0.218651 valor sem município de origem

### checar se os identificadores não tem municipio e uf

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
load("identificador.Rda")

head(identificador)

fnde_origem_na<-fnde_origem[fnde_origem$fornmunicipio_uf == 'NA_NA',]

fnde_fornec_001 <- merge(x=fnde_origem_na,y=identificador,by="identificador_fornec",all.x=TRUE)
unique(fnde_fornec_001$municipio_fornec)
View(fnde_fornec_001)


#########
######## CLASSIFICAÇÃO DOS ITENS
##########

setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
load("fnde_origem.Rda")
setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
correspondencia = readxl::read_excel("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/resultados/correspondencia.xlsx")#,skip = 2

head(fnde_origem)
fnde_origem_002<-fnde_origem[c(1:100),]
#remove acentuação e pontuação
fnde_origem_002$item = stringi::stri_trans_general(str = fnde_origem_002$item, id = "Latin-ASCII")
fnde_origem_002$item = stringr::str_replace_all(fnde_origem_002$item, "(?![_])[[:punct:]]", "")

#lowercase
fnde_origem_002$item<-tolower(fnde_origem_002$item)

head(correspondencia)
correspondencia_002<-correspondencia#[c(1:100),]

head(fnde_origem_002)
head(correspondencia_002)

correspondencia_003<-correspondencia_002[,c(1:2)]
correspondencia_004<-unique(correspondencia_003)
library(tidyr)
correspondencia_005<-correspondencia_004%>%drop_na(item)
nrow(correspondencia_005)
head(correspondencia_005)
correspondencia_005a<-c(correspondencia_005$y)
correspondencia_005b<-c(correspondencia_005$item)
head(correspondencia_005a)
head(correspondencia_005b)
#novonome <- stringi::str_replace(basededados, de/original/from, para/destino/to)  
library(stringi)
fnde_origem_002$item_2 <- stringr::str_replace(fnde_origem_002$item,
                                        correspondencia_005a,
                                        correspondencia_005b)  
View(fnde_origem_002)

fnde_origem_002$item_2 <- df[unlist(sapply(patterns, grep, df$Letters, USE.NAMES = F)), ]#grepl("FOO", df$DESCRIPTION)






colnames(sigpc_geocod)

sigpc_geocod_item<-sigpc_geocod[,c(4,5)]
head(sigpc_geocod_item)
library(dplyr)
sigpc_geocod_item_A<-sigpc_geocod_item %>%
  dplyr::group_by(item) %>%
  dplyr::summarise_at(dplyr::vars(valor), dplyr::funs(sum="sum"(., na.rm=TRUE)))
nrow(sigpc_geocod_item_A)

colnames(sigpc_geocod_item_A)

sigpc_geocod_item_001<-sigpc_geocod_item_A
nrow(sigpc_geocod_item_001)

#lowercase
sigpc_geocod_item_001$item<-tolower(sigpc_geocod_item_001$item)

head(sigpc_geocod_item_001)

sigpc_geocod_item_002<-sigpc_geocod_item_001 %>%
  dplyr::group_by(item) %>%
  dplyr::summarise_at(dplyr::vars(sum), dplyr::funs(sum="sum"(., na.rm=TRUE)))
nrow(sigpc_geocod_item_A)
nrow(sigpc_geocod_item_002)

sigpc_geocod_item_B<-sigpc_geocod_item_002

save(sigpc_geocod_item_B,file="sigpc_geocod_item_B.Rda")
setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
load("sigpc_geocod_item_B.Rda")

head(sigpc_geocod_item_B)

#remove acentuação
sigpc_geocod_item_1<-sigpc_geocod_item_B[c(1:25000),]
sigpc_geocod_item_1$item<-rm_accent(sigpc_geocod_item_1$item)
sigpc_geocod_item_2<-sigpc_geocod_item_B[c(25000:50000),]
sigpc_geocod_item_2$item<-rm_accent(sigpc_geocod_item_2$item)
sigpc_geocod_item_3<-sigpc_geocod_item_B[c(50000:75000),]
sigpc_geocod_item_3$item<-rm_accent(sigpc_geocod_item_3$item)
sigpc_geocod_item_4<-sigpc_geocod_item_B[c(75000:100000),]
sigpc_geocod_item_4$item<-rm_accent(sigpc_geocod_item_4$item)
sigpc_geocod_item_5<-sigpc_geocod_item_B[c(100000:125000),]
sigpc_geocod_item_5$item<-rm_accent(sigpc_geocod_item_5$item)
sigpc_geocod_item_6<-sigpc_geocod_item_B[c(125000:150000),]
sigpc_geocod_item_6$item<-rm_accent(sigpc_geocod_item_6$item)

sigpc_geocod_item_002<-rbind(sigpc_geocod_item_1,
                             sigpc_geocod_item_2,
                             sigpc_geocod_item_3,
                             sigpc_geocod_item_4,
                             sigpc_geocod_item_5,
                             sigpc_geocod_item_6)

save(sigpc_geocod_item_002,file="sigpc_geocod_item_002.Rda")
setwd("G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese")
load("sigpc_geocod_item_002.Rda")

library(dplyr)
sigpc_geocod_item_003<-sigpc_geocod_item_002 %>%
  dplyr::group_by(item) %>%
  dplyr::summarise_at(dplyr::vars(sum), dplyr::funs(sum="sum"(., na.rm=TRUE)))

nrow(sigpc_geocod_item_002)
nrow(sigpc_geocod_item_003)

sigpc_geocod_item_004<-sigpc_geocod_item_003

#remove acentuação e pontuação
sigpc_geocod_item_004$item = stringi::stri_trans_general(str = sigpc_geocod_item_004$item, id = "Latin-ASCII")
sigpc_geocod_item_004$item = stringr::str_replace_all(sigpc_geocod_item_004$item, "[[:punct:]]", " ")
sigpc_geocod_item_004$item = stringr::str_replace_all(sigpc_geocod_item_004$item, "[:digit:]", " ")

#organiza espaços
sigpc_geocod_item_004$item = stringr::str_trim(str = sigpc_geocod_item_004$item)
sigpc_geocod_item_004$item <- gsub("\\s+"," ",sigpc_geocod_item_004$item)
sigpc_geocod_item_004$item<-stringr::str_squish(sigpc_geocod_item_004$item)

sigpc_geocod_item_005<-sigpc_geocod_item_004

sigpc_geocod_item_005$item <- gsub("ª", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("|", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("º", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("~", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("a ucar", "acucar", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub(">", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("<", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("³", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("+", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("=", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("´", "", sigpc_geocod_item_005$item)
sigpc_geocod_item_005$item <- gsub("'", "", sigpc_geocod_item_005$item)

View(sigpc_geocod_item_005)
library(dplyr)
sigpc_geocod_item_006<-sigpc_geocod_item_005 %>%
  dplyr::group_by(item) %>%
  dplyr::summarise_at(dplyr::vars(sum), dplyr::funs(sum="sum"(., na.rm=TRUE)))

View(sigpc_geocod_item_006)
head(sigpc_geocod_item_006)
summary(sigpc_geocod_item_006)

nrow(sigpc_geocod_item_004)
nrow(sigpc_geocod_item_006)

View(sigpc_geocod_item_006)

save(sigpc_geocod_item_006,file="sigpc_geocod_item_006.Rda") 

View(sigpc_geocod_item_006)

sigpc_geocod_item_007<-sigpc_geocod_item_006 %>% slice_max(sum, prop = 0.075)
sum(sigpc_geocod_item_007$sum)/sum(sigpc_geocod_item_006$sum)

nrow(sigpc_geocod_item_005)
nrow(sigpc_geocod_item_006)


head(sigpc_geocod_item_007)

save(sigpc_geocod_item_007,file="sigpc_geocod_item_007.Rda") 

sigpc_geocod_item_unico_novo<-sigpc_geocod_item_007

head(sigpc_geocod_item_unico_novo)

sigpc_geocod_item_unico_novo<-sigpc_geocod_item_unico_novo[,c(1)]

write.csv(sigpc_geocod_item_unico_novo,
          "G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese/sigpc_geocod_item_unico_novo.csv")

sigpc_geocod_002<-splitstackshape::cSplit(sigpc_geocod_item_unico_novo, "item", sep=" ")
nrow(sigpc_geocod_002)
sigpc_geocod_003<-data.frame(y=unlist(sigpc_geocod_002))
nrow(sigpc_geocod_003)
sigpc_geocod_item_decomposto<-as.data.frame(unique(sigpc_geocod_003))
nrow(sigpc_geocod_item_decomposto)
View(sigpc_geocod_item_decomposto)

write.csv(sigpc_geocod_item_decomposto,
          "G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/sintese/sigpc_geocod_item_decomposto.csv")



















#######################################
######## OUTRAS VISUALIZAÇÕES
#####################################



#Tabatinga (AM) e Caarapó (MS)
mun_selec<-dplyr::filter(fnde_id_total, grepl('TABATINGA|CAARAP',municipio_eex))
unique(mun_selec$municipio_eex)



write.csv(mun_selec,"G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/mun_selec.csv")



#identifica NAs por estado, aquisições de UF por UF
head(fnde_id_2019)
fnde_id_2019A<-reshape2::dcast(fnde_id_2019,uf_fornec~uf_eex,fun.aggregate = sum,value.var = "valor")


head(fnde_id_2019A)
write.csv(fnde_id_2019B,"G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/fnde_id_2019B.csv")

load("identificador_completo_unique.Rda")
head(identificador_completo_unique)
identificador_completo_i <- identificador_completo_unique[,c(1,5)]
nrow(identificador_completo_i)
identificador_completo_ii<-unique(identificador_completo_i)
nrow(identificador_completo_ii)

fnde_af_lai_2016_ii$identificador_fornec<-as.numeric(fnde_af_lai_2016_ii$identificador_fornec)
identificador_completo_ii$identificador_fornec<-as.numeric(identificador_completo_ii$identificador_fornec)

nrow(identificador_completo_ii)
nrow(fnde_af_lai_2016_ii)

head(identificador_completo_ii)
head(fnde_af_lai_2016_ii)

fnde_id_2016 <- dplyr::left_join(fnde_af_lai_2016_ii,identificador_completo_ii, by = 'identificador_fornec')
save(fnde_id_2016,file = "fnde_id_2016.Rda")

#identifica NAs
sapply(fnde_id_2016, function(x) sum(length(which(is.na(x)))))
1-(826903/nrow(fnde_id_2016))#[1] 0.5399178 das linhas  estão identificadas c/ município

fnde_id_2016$valor <- as.numeric(fnde_id_2016$valor)
fnde_id_na_2016 <- fnde_id_2016[is.na(fnde_id_2016$uf_fornec),]
fnde_id_soma<-sum(fnde_id_2016$valor, na.rm = TRUE)
fnde_id_na_soma<-sum(fnde_id_na_2016$valor, na.rm = TRUE)
1-(fnde_id_na_soma/(fnde_id_soma)) # [1] 0.7000714 do valor  tem identificação de município de origem

#identifica NAs por estado, aquisições de UF por UF
head(fnde_id_2016)
fnde_id_2016A<-fnde_id_2016%>%dplyr::select(1,3,4)
head(fnde_id_2016A)
fnde_id_2016B<-reshape2::dcast(fnde_id_2016A,uf_fornec~uf_eex,
                               fun.aggregate = sum,
                               value.var = "valor")

head(fnde_id_2016B)
write.csv(fnde_id_2016B,"G:/Meu Drive/NEPA/- Projeto PNAE Cebrap/Cebrap - Bases de dados/Cebrap-SiGPC/fnde_id_2016B.csv")

######
###### Funções
######

#++++++++++++++++++++++++++++++++++
# rm_accent() versao inicial retirada:
# - https://pt.stackoverflow.com/questions/46473/remover-acentos
#+++++++++++++++++++++++++++++++++++

# Remover acentos
rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}


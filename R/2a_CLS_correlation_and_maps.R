
# Credits -----------------------------------------------------------------

# Created by Jayme A. Prevedello and Luara Tourinho 
# (http://lattes.cnpq.br/7678496109580941)
# (https://github.com/luaratourinho)

# Last update: March 2021


#Loading library

library(sp)
library(rgdal)
library(raster)
library(reshape)
library(gdistance)
library(rgeos)
library(maptools)


dsn_shapelist <- paste0("./hex_SOSMA_per_species/", sp.names[1:71])

#Creating list with all landscape shapefiles
shapelist <- as.list(ogrListLayers(dsn_shapelist))

planilha_correlacao <- read.table(paste0("./Nodes_dists_cu/", sp.names[1:71], "/results_all_overall_indices.txt"))
head(planilha_correlacao)


nomes <- planilha_correlacao$V1
nomes_so_numeros <- NULL
for(i in 1:length(nomes)){
  nomes_reduzido <- (gsub('_cu.txt','',as.character(nomes[i])))
  nomes_so_numeros[i] <- nomes_reduzido
}


planilha_correlacao$numeros <- nomes_so_numeros
head(planilha_correlacao)

#ORDENAR AGORA -->
linhas.relevantes_Ocapu <- which(planilha_correlacao$V4=="PCnum")
planilha_correlacao <- planilha_correlacao[linhas.relevantes_Ocapu,]
head(planilha_correlacao)

plan_correl_order = planilha_correlacao
head(plan_correl_order) 
tail(plan_correl_order)
dim(plan_correl_order)

#salvar nova planilha de resultado

write.table(plan_correl_order, paste0("~/mestrado/Nodes_dists_cu/", sp.names[1:71], "/results_all_overall_indices_corrigido.txt"))

################# para o futuro

planilha_correlacao <- read.table(paste0("~/mestrado/Nodes_dists_fu/", sp.names[1:71], "/results_all_overall_indices.txt"))
head(planilha_correlacao)


nomes <- planilha_correlacao$V1
nomes_so_numeros <- NULL
for(i in 1:length(nomes)){
  nomes_reduzido <- (gsub('_cu.txt','',as.character(nomes[i])))
  nomes_so_numeros[i] <- nomes_reduzido
}
nomes_so_numeros

planilha_correlacao$numeros <- nomes_so_numeros
head(planilha_correlacao)

#ORDENAR AGORA -->
linhas.relevantes_Ocapu <- which(planilha_correlacao$V4=="PCnum")
planilha_correlacao <- planilha_correlacao[linhas.relevantes_Ocapu,]
head(planilha_correlacao)

plan_correl_order = planilha_correlacao

#salvar nova planilha de resultado
write.table(plan_correl_order, paste0("~/mestrado/Nodes_dists_fu/", sp.names[1:71], "/results_all_overall_indices_corrigido.txt"))


#abre planilha dos resultados
presente_Ocapu <- read.table(paste0("~/mestrado/Nodes_dists_cu/", sp.names[1:71], "/results_all_overall_indices_corrigido.txt"))

### presente -  - normalizar o pc

if(min(presente_Ocapu$V5, na.rm=T) == -Inf){# corrige o minimo, colocando como 0, caso ele seja -Inf
  infinito <- which(presente_Ocapu$V5==-Inf)
  presente_Ocapu$V5[infinito] <- 0
}

if(max(presente_Ocapu$V5, na.rm=T) == Inf){# corrige o max?
  infinito <- which(presente_Ocapu$V5==Inf)
  presente_Ocapu$V5[infinito] <- NA
}

minimo <- min(presente_Ocapu$V5, na.rm=T)
maximo <- max(presente_Ocapu$V5, na.rm=T)

normalizar <- function(x) {
  (x-minimo)/(maximo-minimo)}

norm <- normalizar(presente_Ocapu$V5)
presente_Ocapu$PC_norm <- norm

head(presente_Ocapu)

write.table(presente_Ocapu, paste0("~/mestrado/Nodes_dists_cu/", sp.names[1:71], "/results_all_overall_indices_norm.txt"))


#futuro dessa sp - normalizar o pc
futuro_Ocapu <- read.table(paste0("~/mestrado/Nodes_dists_fu/", sp.names[1:71], "/results_all_overall_indices_corrigido.txt"))

if(min(futuro_Ocapu$V5, na.rm=T) == -Inf){# corrige o minimo, colocando como 0, caso ele seja -Inf
  infinito <- which(futuro_Ocapu$V5==-Inf)
  futuro_Ocapu$V5[infinito] <- 0
}

if(max(futuro_Ocapu$V5, na.rm=T) == Inf){# corrige o max?
  infinito <- which(futuro_Ocapu$V5==Inf)
  futuro_Ocapu$V5[infinito] <- NA
}

minimo <- min(futuro_Ocapu$V5, na.rm=T)
maximo <- max(futuro_Ocapu$V5, na.rm=T)

normalizar <- function(x) {
  (x-minimo)/(maximo-minimo)}

norm <- normalizar(futuro_Ocapu$V5)
futuro_Ocapu$PC_norm <- norm

head(futuro_Ocapu)

write.table(futuro_Ocapu, paste0("~/mestrado/Nodes_dists_fu/", sp.names[1:71], "/results_all_overall_indices_norm.txt"))



# calculo quantidade de cobertura em cada hexagono onde o PC foi calculado

quantidade_cobertura <- read.table("./nodes_area/Quantidade_de_habitat.txt", header=T)
head(quantidade_cobertura)

#extrair numeros dos nomes dos hexagonos
nomes.cobertura <- quantidade_cobertura$nome
nomes_so_numeros.cobertura <- NULL
for(i in 1:length(nomes.cobertura)){
  nome_reduzido <- gsub('_MultipartToSinglepart1','',as.character(nomes.cobertura[i]))
  numero <- as.numeric(gsub('c','',as.character(nome_reduzido)))
  nomes_so_numeros.cobertura[i] <- numero
}
nomes_so_numeros.cobertura


#gerar novo arquivo com cobertura nos hexagonos que t?m valor de PC calculado
quantidade_cobertura_organizado <- NULL
for(i in 1:length(presente_Ocapu$nomes)){
  linha.correspondente <- which(nomes_so_numeros.cobertura == presente_Ocapu$nomes[i])
  if(sum(linha.correspondente)==0) {
    quantidade_cobertura_organizado[i] <- NA
  } else {quantidade_cobertura_organizado[i] <- quantidade_cobertura$porcentagem[linha.correspondente]}
}

quantidade_cobertura <- quantidade_cobertura_organizado
head(quantidade_cobertura)

paisagens_menos_30_porcento <- which(quantidade_cobertura <= 0.3)
paisagens_mais_30_porcento <- which(quantidade_cobertura > 0.3)


# Qnts dentro e fora de 30% -----------------------------------------------

#como presente eh a mesma qntidade que futuro, vejo so presente
str(presente_Ocapu$PC_norm[paisagens_mais_30_porcento])
str(presente_Ocapu$PC_norm[paisagens_menos_30_porcento])
str(presente_Ocapu$PC_norm)

paisagens_menos_50_porcento <- which(quantidade_cobertura <= 0.5)
paisagens_mais_50_porcento <- which(quantidade_cobertura > 0.5)
str(presente_Ocapu$PC_norm[paisagens_mais_50_porcento])
str(presente_Ocapu$PC_norm[paisagens_menos_50_porcento])


############ CORRELACAO GRAFICO 2CORES
##################### CORRELACAO ########################

#plot menor do que 30

par(new=TRUE)
par(mar=c(5,7,2,1), cex.axis=1.5, cex.lab=1.5, family="serif",mgp=c(3.5,0.5,0))

plot(presente_Ocapu$PC_norm[paisagens_menos_30_porcento], futuro_Ocapu$PC_norm[paisagens_menos_30_porcento], ylab="CLS futuro \nP. cruenta (<=30%)", xlab="CLS atual \nP. cruenta (<=30%)", las=1, bty="l", col="red", tcl=0, cex=1, ylim=c(0,1), xlim=c(0,1), family="serif")
correlacao <- cor.test(presente_Ocapu$PC_norm[paisagens_menos_30_porcento], futuro_Ocapu$PC_norm[paisagens_menos_30_porcento])
coef.de.correlacao <- correlacao$estimate #ver as estimativas da correlacao
text(paste("r =", round(coef.de.correlacao,3)), x=0.8, y=0.2, cex=1.5) #colocar o "r" no grafico
abline(lm(futuro_Ocapu$PC_norm[paisagens_menos_30_porcento]~presente_Ocapu$PC_norm[paisagens_menos_30_porcento])) #colocar a linha no grafico
regressao <- lm(futuro_Ocapu$PC_norm[paisagens_menos_30_porcento]~presente_Ocapu$PC_norm[paisagens_menos_30_porcento]) # calculo inclinacao da reta
inclinacao <- as.numeric(regressao$coefficients[2])
text(paste("b =", round(inclinacao,3)), x=0.8, y=0.3, cex=1.5) #colocar o "b" no grafico


##plot maior do que 30

par(mar=c(5,7,2,1), cex.axis=1.5, cex.lab=1.5, family="serif",mgp=c(3.5,0.5,0))

plot(presente_Ocapu$PC_norm[paisagens_mais_30_porcento], futuro_Ocapu$PC_norm[paisagens_mais_30_porcento], ylab="CLS futuro \nO. capueira (>30%)", xlab="CLS atual \nO. capueira (>30%)", las=1, bty="l", tcl=0, cex=1, ylim=c(0,1), xlim=c(0,1), family="serif")
correlacao <- cor.test(presente_Ocapu$PC_norm[paisagens_mais_30_porcento], futuro_Ocapu$PC_norm[paisagens_mais_30_porcento])
coef.de.correlacao <- correlacao$estimate #ver as estimativas da correlacao
text(paste("r =", round(coef.de.correlacao,3)), x=0.8, y=0.2, cex=1.5) #colocar o "r" no grafico
abline(lm(futuro_Ocapu$PC_norm[paisagens_mais_30_porcento]~presente_Ocapu$PC_norm[paisagens_mais_30_porcento])) #colocar a linha no grafico
regressao <- lm(futuro_Ocapu$PC_norm[paisagens_mais_30_porcento]~presente_Ocapu$PC_norm[paisagens_mais_30_porcento]) # calculo inclinacao da reta
inclinacao <- as.numeric(regressao$coefficients[2])
text(paste("b =", round(inclinacao,3)), x=0.8, y=0.3, cex=1.5) 


#plot geral

par(mar=c(5,7,2,1), cex.axis=1.5, cex.lab=1.5, family="serif",mgp=c(3,0.5,0))
plot(presente_Ocapu$PC_norm, futuro_Ocapu$PC_norm, ylab="Future CLS", xlab="Current CLS", las=1, bty="l", tcl=0, cex=1, ylim=c(0,1), xlim=c(0,1), family="serif")

par(mar=c(5,7,2,1), cex.axis=1.5, cex.lab=1.5, family="serif",mgp=c(3.5,0.5,0))
plot(presente_Ocapu$PC_norm, futuro_Ocapu$PC_norm, ylab="Future CLS \nP. cruentata", xlab="Current CLS \nP. cruentata", las=1, bty="l", tcl=0, cex=1, ylim=c(0,1), xlim=c(0,1), family="serif")
correlacao <- cor.test(presente_Ocapu$PC_norm, futuro_Ocapu$PC_norm)
coef.de.correlacao <- correlacao$estimate #ver as estimativas da correlacao
text(paste("r =", round(coef.de.correlacao,3)), x=0.8, y=0.2, cex=1.5) #colocar o "r" no grafico
abline(lm(futuro_Ocapu$PC_norm~presente_Ocapu$PC_norm)) #colocar a linha no grafico
regressao <- lm(futuro_Ocapu$PC_norm~presente_Ocapu$PC_norm) # calculo inclinacao da reta
inclinacao <- as.numeric(regressao$coefficients[2])
text(paste("b =", round(inclinacao,3)), x=0.8, y=0.3, cex=1.5) 

#pontos coloridos <=30%
par(new=TRUE)
par(mar=c(5,7,2,1), cex.axis=1.5, cex.lab=1.5, family="serif",mgp=c(3.5,0.5,0))
plot(presente_Ocapu$PC_norm[paisagens_menos_30_porcento], futuro_Ocapu$PC_norm[paisagens_menos_30_porcento], ylab="", xlab="", las=1, bty="l", col="red", tcl=0, cex=1, ylim=c(0,1), xlim=c(0,1), family="serif")



# MAPA PRESENTE E FUTURO --------------------------------------------------

### presente

cut$ids_corrigidos <- paisagens_dentro_buffer #adicionar novo campo no shape, com id de cada hexagono

PC_valores_para_plotar <- rep(NA, length(cut$ids_corrigidos))
for(i in 1:length(cut$ids_corrigidos)){
  id_hex <- cut$ids_corrigidos[i]
  linha_PC <- which(presente_Ocapu$nomes==id_hex)
  PC <- presente_Ocapu$PC_norm[linha_PC]
  if(length(PC)!=0){ 
    PC_valores_para_plotar[i] <- PC
  }
}

cut$PC_norm <- PC_valores_para_plotar
cut_Ocapu_cu <- cut

#dir.create(paste0("./Results_CLS_map"))
writeOGR(cut_Ocapu_cu, "E:/mestrado/Results_CLS_map", "Amazona_brasiliensis_cur", driver="ESRI Shapefile", overwrite=T)

### futuro

cut$ids_corrigidos <- paisagens_dentro_buffer #adicionar novo campo no shape, com id de cada hexagono

PC_valores_para_plotar <- rep(NA, length(cut$ids_corrigidos))
for(i in 1:length(cut$ids_corrigidos)){
  id_hex <- cut$ids_corrigidos[i]
  linha_PC <- which(futuro_Ocapu$nomes==id_hex)
  PC <- futuro_Ocapu$PC_norm[linha_PC]
  if(length(PC)!=0){ 
    PC_valores_para_plotar[i] <- PC
  }
}

cut$PC_norm <- PC_valores_para_plotar
cut_Ocapu_fu <- cut

writeOGR(cut_Ocapu_fu, "E:/mestrado/Results_CLS_map", "Amazona_brasiliensis_fut", driver="ESRI Shapefile", overwrite=T)


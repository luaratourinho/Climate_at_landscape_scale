
# Credits -----------------------------------------------------------------

# Created by Luara Tourinho and Jayme A. Prevedello 
# (https://github.com/luaratourinho)
# (http://lattes.cnpq.br/7678496109580941)

# Last update: 2019


#Loading library

library(maptools)
library(rgdal)


# Script under editing ----------------------------------------------------



# Sp1 ---------------------------------------------------------------------


#amaz <- readShapeSpatial("F:/modeling_land/Results_para_LE/Cut_CLS_results/amaz_bras/current.shp")
amaz <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/amaz_bras/current.shp")
amaz_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/amaz_bras/future.shp")
#plot(amaz)
#summary(amaz)
cents <- coordinates(amaz)
#plot(cents)
#plot(amaz)
#points(cents, add=T)

#amaz_table<-read.dbf(gsub(".shp", ".dbf", amaz), header=TRUE) 
amaz_table<-amaz@data
amaz_table<-data.frame(amaz_table)
#amaz_futtable<-read.dbf(gsub(".shp", ".dbf", amaz_fut), header=TRUE) 
amaz_futtable<-amaz_fut@data
amaz_futtable<-data.frame(amaz_futtable)

sp<- rep("amaz_bras",length(amaz))
table<- cbind(sp, amaz$ids_crr, cents, amaz$PC_norm, amaz_fut$PC_norm)
table_df <- as.data.frame(table, nrow=F)
names(table_df) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")
head(table_df)
table_df_noNA <- subset(table_df, CLS_curr !=0 & CLS_fut !=0)
dim(table_df_noNA)


# Other species -----------------------------------------------------------

bary <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/bary_rufi/current.shp")
bary_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/bary_rufi/future.shp")

cents_bary <- coordinates(bary)

bary_table<-bary@data
bary_table<-data.frame(bary_table)
bary_futtable<-bary_fut@data
bary_futtable<-data.frame(bary_futtable)

sp<- rep("bary_rufi",length(bary))
table_bary<- cbind(sp, bary$ids_crr, cents_bary, bary$PC_norm, bary_fut$PC_norm)
table_df_bary <- as.data.frame(table_bary, nrow=F)
names(table_df_bary) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_bary)
table_df_noNA__bary <- subset(table_df_bary,!is.na(table_df_bary$CLS_curr)&!is.na(table_df_bary$CLS_fut))
table_df_noNA_no0_bary <- subset(table_df_noNA__bary, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__bary)
tail(table_df_noNA__bary)
dim(table_df_noNA__bary)
dim(table_df_noNA_no0_bary)


# Sp3 ---------------------------------------------------------------------

camp <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/camp_robu/current.shp")
camp_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/camp_robu/future.shp")
plot(camp)

cents_camp <- coordinates(camp)

camp_table<-camp@data
camp_table<-data.frame(camp_table)
camp_futtable<-camp_fut@data
camp_futtable<-data.frame(camp_futtable)

sp<- rep("camp_robu",length(camp))
table_camp<- cbind(sp, camp$ids_crr, cents_camp, camp$PC_norm, camp_fut$PC_norm)
table_df_camp <- as.data.frame(table_camp, nrow=F)
names(table_df_camp) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_camp)
table_df_noNA__camp <- subset(table_df_camp,!is.na(table_df_camp$CLS_curr)&!is.na(table_df_camp$CLS_fut))
table_df_noNA_no0_camp <- subset(table_df_noNA__camp, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__camp)
tail(table_df_noNA__camp)
dim(table_df_noNA__camp)
dim(table_df_noNA_no0_camp)

# Sp4 ---------------------------------------------------------------------

drym <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/drym_gene/current.shp")
drym_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/drym_gene/future.shp")
plot(drym)

cents_drym <- coordinates(drym)

drym_table<-drym@data
drym_table<-data.frame(drym_table)
drym_futtable<-drym_fut@data
drym_futtable<-data.frame(drym_futtable)

sp<- rep("drym_gene",length(drym))
table_drym<- cbind(sp, drym$ids_crr, cents_drym, drym$PC_norm, drym_fut$PC_norm)
table_df_drym <- as.data.frame(table_drym, nrow=F)
names(table_df_drym) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_drym)
table_df_noNA__drym <- subset(table_df_drym,!is.na(table_df_drym$CLS_curr)&!is.na(table_df_drym$CLS_fut))
table_df_noNA_no0_drym <- subset(table_df_noNA__drym, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__drym)
tail(table_df_noNA__drym)
dim(table_df_noNA__drym)
dim(table_df_noNA_no0_drym)

# Sp5 ---------------------------------------------------------------------

jaca <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/jaca_trid/current.shp")
jaca_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/jaca_trid/future.shp")
plot(jaca)

cents_jaca <- coordinates(jaca)

jaca_table<-jaca@data
jaca_table<-data.frame(jaca_table)
jaca_futtable<-jaca_fut@data
jaca_futtable<-data.frame(jaca_futtable)

sp<- rep("jaca_trid",length(jaca))
table_jaca<- cbind(sp, jaca$ids_crr, cents_jaca, jaca$PC_norm, jaca_fut$PC_norm)
table_df_jaca <- as.data.frame(table_jaca, nrow=F)
names(table_df_jaca) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_jaca)
table_df_noNA__jaca <- subset(table_df_jaca,!is.na(table_df_jaca$CLS_curr)&!is.na(table_df_jaca$CLS_fut))
table_df_noNA_no0_jaca <- subset(table_df_noNA__jaca, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__jaca)
tail(table_df_noNA__jaca)
dim(table_df_noNA__jaca)
dim(table_df_noNA_no0_jaca)

# Sp6 ---------------------------------------------------------------------

glau <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/glau_dohr/current.shp")
glau_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/glau_dohr/future.shp")
plot(glau)

cents_glau <- coordinates(glau)

glau_table<-glau@data
glau_table<-data.frame(glau_table)
glau_futtable<-glau_fut@data
glau_futtable<-data.frame(glau_futtable)

sp<- rep("glau_dohr",length(glau))
table_glau<- cbind(sp, glau$ids_crr, cents_glau, glau$PC_norm, glau_fut$PC_norm)
table_df_glau <- as.data.frame(table_glau, nrow=F)
names(table_df_glau) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_glau)
table_df_noNA__glau <- subset(table_df_glau,!is.na(table_df_glau$CLS_curr)&!is.na(table_df_glau$CLS_fut))
table_df_noNA_no0_glau <- subset(table_df_noNA__glau, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__glau)
tail(table_df_noNA__glau)
dim(table_df_noNA__glau)
dim(table_df_noNA_no0_glau)

# Sp7 ---------------------------------------------------------------------

mion <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/mion_rufi/current.shp")
mion_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/mion_rufi/future.shp")
plot(mion)

cents_mion <- coordinates(mion)

mion_table<-mion@data
mion_table<-data.frame(mion_table)
mion_futtable<-mion_fut@data
mion_futtable<-data.frame(mion_futtable)

sp<- rep("mion_rufi",length(mion))
table_mion<- cbind(sp, mion$ids_crr, cents_mion, mion$PC_norm, mion_fut$PC_norm)
table_df_mion <- as.data.frame(table_mion, nrow=F)
names(table_df_mion) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_mion)
table_df_noNA__mion <- subset(table_df_mion,!is.na(table_df_mion$CLS_curr)&!is.na(table_df_mion$CLS_fut))
table_df_noNA_no0_mion <- subset(table_df_noNA__mion, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__mion)
tail(table_df_noNA__mion)
dim(table_df_noNA__mion)
dim(table_df_noNA_no0_mion)

# Sp8 ---------------------------------------------------------------------

neop <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/neop_chry/current.shp")
neop_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/neop_chry/future.shp")
plot(neop)

cents_neop <- coordinates(neop)

neop_table<-neop@data
neop_table<-data.frame(neop_table)
neop_futtable<-neop_fut@data
neop_futtable<-data.frame(neop_futtable)

sp<- rep("neop_chry",length(neop))
table_neop<- cbind(sp, neop$ids_crr, cents_neop, neop$PC_norm, neop_fut$PC_norm)
table_df_neop <- as.data.frame(table_neop, nrow=F)
names(table_df_neop) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_neop)
table_df_noNA__neop <- subset(table_df_neop,!is.na(table_df_neop$CLS_curr)&!is.na(table_df_neop$CLS_fut))
table_df_noNA_no0_neop <- subset(table_df_noNA__neop, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__neop)
tail(table_df_noNA__neop)
dim(table_df_noNA__neop)
dim(table_df_noNA_no0_neop)

# Sp9 ---------------------------------------------------------------------

odon <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/odon_capu/current.shp")
odon_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/odon_capu/future.shp")
plot(odon)

cents_odon <- coordinates(odon)

odon_table<-odon@data
odon_table<-data.frame(odon_table)
odon_futtable<-odon_fut@data
odon_futtable<-data.frame(odon_futtable)

sp<- rep("odon_capu",length(odon))
table_odon<- cbind(sp, odon$ids_crr, cents_odon, odon$PC_norm, odon_fut$PC_norm)
table_df_odon <- as.data.frame(table_odon, nrow=F)
names(table_df_odon) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_odon)
table_df_noNA__odon <- subset(table_df_odon,!is.na(table_df_odon$CLS_curr)&!is.na(table_df_odon$CLS_fut))
table_df_noNA_no0_odon <- subset(table_df_noNA__odon, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__odon)
tail(table_df_noNA__odon)
dim(table_df_noNA__odon)
dim(table_df_noNA_no0_odon)

# Sp10 ---------------------------------------------------------------------

phae <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/phae_eury/current.shp")
phae_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/phae_eury/future.shp")
plot(phae)

cents_phae <- coordinates(phae)

phae_table<-phae@data
phae_table<-data.frame(phae_table)
phae_futtable<-phae_fut@data
phae_futtable<-data.frame(phae_futtable)

sp<- rep("phae_eury",length(phae))
table_phae<- cbind(sp, phae$ids_crr, cents_phae, phae$PC_norm, phae_fut$PC_norm)
table_df_phae <- as.data.frame(table_phae, nrow=F)
names(table_df_phae) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_phae)
table_df_noNA__phae <- subset(table_df_phae,!is.na(table_df_phae$CLS_curr)&!is.na(table_df_phae$CLS_fut))
table_df_noNA_no0_phae <- subset(table_df_noNA__phae, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__phae)
tail(table_df_noNA__phae)
dim(table_df_noNA__phae)
dim(table_df_noNA_no0_phae)

# Sp11 ---------------------------------------------------------------------

proc <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/proc_nudi/current.shp")
proc_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/proc_nudi/future.shp")
plot(proc)

cents_proc <- coordinates(proc)

proc_table<-proc@data
proc_table<-data.frame(proc_table)
proc_futtable<-proc_fut@data
proc_futtable<-data.frame(proc_futtable)

sp<- rep("proc_nudi",length(proc))
table_proc<- cbind(sp, proc$ids_crr, cents_proc, proc$PC_norm, proc_fut$PC_norm)
table_df_proc <- as.data.frame(table_proc, nrow=F)
names(table_df_proc) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_proc)
table_df_noNA__proc <- subset(table_df_proc,!is.na(table_df_proc$CLS_curr)&!is.na(table_df_proc$CLS_fut))
table_df_noNA_no0_proc <- subset(table_df_noNA__proc, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__proc)
tail(table_df_noNA__proc)
dim(table_df_noNA__proc)
dim(table_df_noNA_no0_proc)

# Sp12 ---------------------------------------------------------------------

pyrr <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/pyrr_crue/current.shp")
pyrr_fut <- readOGR("F:/modeling_land/Results_para_LE/Cut_CLS_results/pyrr_crue/future.shp")
plot(pyrr)

cents_pyrr <- coordinates(pyrr)

pyrr_table<-pyrr@data
pyrr_table<-data.frame(pyrr_table)
pyrr_futtable<-pyrr_fut@data
pyrr_futtable<-data.frame(pyrr_futtable)

sp<- rep("pyrr_crue",length(pyrr))
table_pyrr<- cbind(sp, pyrr$ids_crr, cents_pyrr, pyrr$PC_norm, pyrr_fut$PC_norm)
table_df_pyrr <- as.data.frame(table_pyrr, nrow=F)
names(table_df_pyrr) <-c("sp","id_landscape","Lon","Lat","CLS_curr", "CLS_fut")

head(table_df_pyrr)
table_df_noNA__pyrr <- subset(table_df_pyrr,!is.na(table_df_pyrr$CLS_curr)&!is.na(table_df_pyrr$CLS_fut))
table_df_noNA_no0_pyrr <- subset(table_df_noNA__pyrr, CLS_curr !=0 & CLS_fut !=0)
head(table_df_noNA__pyrr)
tail(table_df_noNA__pyrr)
dim(table_df_noNA__pyrr)
dim(table_df_noNA_no0_pyrr)


# Sum of all species ------------------------------------------------------

###### ATENCAO ######
# phae esta sem o presente. resolver qnd chegar (sp10)

PC_11_sp <- rbind(table_df_noNA,table_df_noNA_no0_bary,table_df_noNA_no0_camp,table_df_noNA_no0_drym,table_df_noNA_no0_glau,table_df_noNA_no0_jaca,table_df_noNA_no0_mion,table_df_noNA_no0_neop,table_df_noNA_no0_odon,table_df_noNA_no0_proc,table_df_noNA_no0_pyrr)
write.csv(PC_11_sp,("F:/modeling_land/Results_para_LE/Cut_CLS_results/PC_11_sp.csv"),row.names = F)
write.csv(PC_11_sp,("C:/Users/Luara/Documents/modeling_land/PC_11_sp.csv"),row.names = F)


PC_11_sp<- read.csv("C:/Users/Luara/Documents/modeling_land/PC_11_sp.csv")

#Legend
#Amaz_bras,Mion_rufi,Neop_chry,Pyrr_cruea -> Omnivore (1)
#Glau_dohr,Odon_capu,Phae_eury,Proc_nudi -> Herbivorous(2)
#Bary_rufi,Camp_robu,Drym_gene,Jaca_trid -> Carnivorous(3)
PC_11_sp$Guild <- rep("1",13744)
PC_11_sp$Guild[which(PC_11_sp$sp=="bary_rufi" )] <-3
PC_11_sp$Guild[which(PC_11_sp$sp=="camp_robu" )] <-3
PC_11_sp$Guild[which(PC_11_sp$sp=="drym_gene" )] <-3
PC_11_sp$Guild[which(PC_11_sp$sp=="jaca_trid" )] <-3
PC_11_sp$Guild[which(PC_11_sp$sp=="glau_dohr" )] <-2
PC_11_sp$Guild[which(PC_11_sp$sp=="odon_capu" )] <-2
#PC_11_sp$Guild[which(PC_11_sp$sp=="phae_eury" )] <-2
PC_11_sp$Guild[which(PC_11_sp$sp=="proc_nudi" )] <-2
PC_11_sp$Guild[which(PC_11_sp$sp=="mion_rufi" )] <-1
PC_11_sp$Guild[which(PC_11_sp$sp=="neop_chry" )] <-1
PC_11_sp$Guild[which(PC_11_sp$sp=="pyrr_cruea" )] <-1
head(PC_11_sp)

#current time is 1, future time is 2
table_curr <- PC_11_sp[,c(1:5,7)]
table_curr$Time <- rep("1",13744)
colnames(table_curr)<-c("sp","id_landscape","Lon","Lat","CLS","Guild","Time")
table_curr$Lon <- table_curr$Lon[] +0.1

table_fut <- PC_11_sp[,c(1:4,6,7)]
table_fut$Time <- rep("2",13744)
colnames(table_fut)<-c("sp","id_landscape","Lon","Lat","CLS","Guild","Time")

table_general <- rbind(table_curr,table_fut)

amaz_bras <- table_general[table_general$sp ==c("amaz_bras"),] #60 
mion_rufi <- table_general[table_general$sp ==c("mion_rufi"),] #2998
neop_chry <- table_general[table_general$sp ==c("neop_chry"),] #1332
pyrr_crue <- table_general[table_general$sp ==c("pyrr_crue"),] #1140

glau_dohr<- table_general[table_general$sp ==c("glau_dohr"),] #700
odon_capu<- table_general[table_general$sp ==c("odon_capu"),] #4172
proc_nudi<- table_general[table_general$sp ==c("proc_nudi"),] #4270

drym_gene <- table_general[table_general$sp ==c("drym_gene"),] #906
jaca_trid <- table_general[table_general$sp ==c("jaca_trid"),] #2268
camp_robu <- table_general[table_general$sp ==c("camp_robu"),] #4754
bary_rufi <- table_general[table_general$sp ==c("bary_rufi"),] #4856

table_4_sp <- table_general[table_general$sp == c("amaz_bras","pyrr_crue","drym_gene","jaca_trid"),]
which(is.na(table_4_sp$Guild))

# m <- matrix(NA, nrow = 27488, ncol = 7)
# table_to_model <- if(i in m){
#   is.odd(i)
# }
# length(PC_11_sp$Guild[which(PC_11_sp$sp=="bary_rufi" )])
# sub(pattern = "O", replacement = "C", x = PC_11_sp$`Guild`[])
# PC_11_sp$Guild <- rep("C", which(PC_11_sp$sp=="bary_rufi" ))
# Lon_test <- if(table_general$Time==1){table_general$Lon[] +0.1}
# table_general_test = table_general
# table_general_test$Lon<-Lon_test
# head(table_general_test)

table_4_sp_1=subset(table_4_sp, Time=="1")
table_4_sp_2=subset(table_4_sp, Time=="2")

par(mfrow=c(1,2))
with(table_4_sp_1, hist(CLS))
with(table_4_sp_2, hist(CLS))

library(lme4)
library(nlme)
mod1<- lme(CLS~Time*Guild, random=~1|sp, correlation = corGaus(form = ~ Lat+Lon), data=table_4_sp)
AIC(mod1)
summary(mod1)
plot(mod1)

mod2<- lme(CLS~Time, random=~1|sp, correlation = corGaus(form = ~ Lat+Lon), data=table_4_sp)
AIC(mod2)
summary(mod2)
plot(mod2)

mod22_nule<- lme(log(CLS+1)~1, random=~1|sp, correlation = corGaus(form = ~ Lat+Lon), data=table_4_sp)
table_4_sp$resid=resid(mod22_nule)

hist(log(table_4_sp$resid))

mod22_gamma<- lmer(resid~Time*Guild +(1|sp), data=table_4_sp)
plot(mod22_gamma)

mod22<- lme(log(CLS+1)~Time*Guild, random=~1|sp, correlation = corGaus(form = ~ Lat+Lon), data=table_4_sp)
mod22b<- lme(log(CLS+1)~Time*Guild, random=~1|sp, data=table_4_sp)



AIC(mod22, mod22b)
summary(mod22)
plot(mod22)

mod3<- lme(CLS~Time*Guild, random=~1|sp, correlation = corAR1(form = ~ Lat+Lon), data=table_4_sp)
summary(mod3)
plot(mod3)

mod4<- lme(CLS~Time*Guild, random=~1|sp, correlation = corLin(form = ~ Lat+Lon), data=table_4_sp)
summary(mod4)
plot(mod4)

mod5<- lme(CLS~Time*Guild, random=~1|sp, correlation = corExp(form = ~ Lat+Lon), data=table_4_sp)
summary(mod5)
plot(mod5)

mod6<- lme(CLS~Time, random=~1|sp, correlation = corExp(form = ~ Lat+Lon), data=table_4_sp)
summary(mod6)
plot(mod6)

mod7=glm(CLS~Time, data=table_4_sp)
summary(mod7)

plot(resid(mod2))

# refazenda ---------------------------------------------------------------

brasil<-readOGR("F:/Brasil/Estados_do_Brasil/Brasil.shp")


amaz_bras <- PC_11_sp[1:46,]
bary_rufi <- subset(PC_11_sp, sp=="bary_rufi")

mod1<- lme(CLS_fut~CLS_curr, random=~1|sp, correlation = corGaus(form = ~ Lat+Lon), data=amaz_bras)
summary(mod1)

mod1<- gls(CLS_fut~CLS_curr, correlation = corGaus(form = ~ Lat+Lon), data=amaz_bras)
plot(mod1)
plot(resid(mod1))
hist(resid(mod1))

mod2<- gls(CLS_fut~CLS_curr, correlation = corLin(form = ~ Lat+Lon), data=amaz_bras)
mod3<- gls(CLS_fut~CLS_curr, correlation = corExp(form = ~ Lat+Lon), data=amaz_bras)
mod4<- gls(CLS_fut~CLS_curr, correlation = corRatio(form = ~ Lat+Lon), data=amaz_bras)
mod5<-gls(CLS_fut~CLS_curr, correlation = corSpher(form = ~ Lat+Lon), data=amaz_bras)
mod3nulo<- gls(CLS_fut~1, correlation = corExp(form = ~ Lat+Lon), data=amaz_bras)


library(MuMIn)
amaz_bras_aics<-model.sel(mod1,mod2,mod3,mod4,mod5)
hist(resid(mod3))
plot(mod3)
summary(mod3)
sqrt(rsquared(mod3)[5])

mod6<- gls(CLS_fut~CLS_curr, correlation = corGaus(form = ~ Lat+Lon), data=bary_rufi)
mod7<- gls(CLS_fut~CLS_curr, correlation = corLin(form = ~ Lat+Lon), data=bary_rufi)
mod8<- gls(CLS_fut~CLS_curr, correlation = corExp(form = ~ Lat+Lon), data=bary_rufi)
mod9<- gls(CLS_fut~CLS_curr, correlation = corRatio(form = ~ Lat+Lon), data=bary_rufi)
mod10<-gls(CLS_fut~CLS_curr, correlation = corSpher(form = ~ Lat+Lon), data=bary_rufi)

bary_rufi <- subset(bary_rufi, Lat=="bary_rufi")
nordeste <- raster("F:/Brasil/nordeste")
proj4string(nordeste) <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- CRS("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")
sudeste_sul <- projectRaster(nordeste, crs=crs.albers)
nordeste<- rasterToPolygons(nordeste, dissolve=FALSE)

point.in.polygon(nordeste,cents_bary)
bary_rufi_nordeste <- crop(amaz_wgs,sudeste_sul)

library(spsurvey)
albersgeod(bary_rufi$Lon, bary_rufi$Lat,sph="WGS84")

proj4string(amaz) <- CRS("+proj=aea +lat_1=-2 +lat_2=-22 +lat_0=-12 +lon_0=-54 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")




############################################################
##########################################################

brasil<-readOGR("F:/Brasil/Estados_do_Brasil/Brasil.shp")
plot(brasil@Sul)

r <- rasterize(brasil, field="UF")

group_1 <- spCbind(brasil, )
group_2 <- census$GEOID[6:8]

amaz_bras <- PC_11_sp[1:46,]
bary_rufi <- subset(PC_11_sp, sp=="bary_rufi")

mod1<- lme(CLS_fut~CLS_curr, random=~1|sp, correlation = corGaus(form = ~ Lat+Lon), data=amaz_bras)
summary(mod1)

mod1<- gls(CLS_fut~CLS_curr, correlation = corGaus(form = ~ Lat+Lon), data=amaz_bras)
plot(mod1)
plot(resid(mod1))
hist(resid(mod1))

mod2<- gls(CLS_fut~CLS_curr, correlation = corLin(form = ~ Lat+Lon), data=amaz_bras)
mod3<- gls(CLS_fut~CLS_curr, correlation = corExp(form = ~ Lat+Lon), data=amaz_bras)
mod4<- gls(CLS_fut~CLS_curr, correlation = corRatio(form = ~ Lat+Lon), data=amaz_bras)
mod5<-gls(CLS_fut~CLS_curr, correlation = corSpher(form = ~ Lat+Lon), data=amaz_bras)

library(MuMIn)
model.sel(mod1,mod2,mod3,mod4,mod5)
hist(resid(mod3))
plot(mod3)
summary(mod3)

mod1<- gls(CLS_fut~CLS_curr, correlation = corGaus(form = ~ Lat+Lon), data=bary_rufi)
mod2<- gls(CLS_fut~CLS_curr, correlation = corLin(form = ~ Lat+Lon), data=bary_rufi)
mod3<- gls(CLS_fut~CLS_curr, correlation = corExp(form = ~ Lat+Lon), data=bary_rufi)
mod4<- gls(CLS_fut~CLS_curr, correlation = corRatio(form = ~ Lat+Lon), data=bary_rufi)
mod5<-gls(CLS_fut~CLS_curr, correlation = corSpher(form = ~ Lat+Lon), data=bary_rufi)



# Continuando o GLM -------------------------------------------------------

mod1<- gls(CLS_fut~CLS_curr, correlation = corGaus(form = ~ Lat+Lon), data=mion_rufi)
plot(mod1)
plot(resid(mod1))
hist(resid(mod1))

mod2<- gls(CLS_fut~CLS_curr, correlation = corLin(form = ~ Lat+Lon), data=mion_rufi)
mod3<- gls(CLS_fut~CLS_curr, correlation = corExp(form = ~ Lat+Lon), data=mion_rufi)
mod4<- gls(CLS_fut~CLS_curr, correlation = corRatio(form = ~ Lat+Lon), data=mion_rufi)
mod5<-gls(CLS_fut~CLS_curr, correlation = corSpher(form = ~ Lat+Lon), data=mion_rufi)
mod4nulo<- gls(CLS_fut~1, correlation = corExp(form = ~ Lat+Lon), data=mion_rufi)

(camp_robu_aics<-model.sel(mod2,mod3,mod4,mod5,mod1))
hist(resid(mod4))
summary(mod4)
sqrt(rsquared(mod4)[5])


mod6<- gls(CLS_fut~CLS_curr, correlation = corGaus(form = ~ Lat+Lon), data=jaca_trid)
plot(mod1)
plot(resid(mod1))
hist(resid(mod1))

mod7<- gls(CLS_fut~CLS_curr, correlation = corLin(form = ~ Lat+Lon), data=jaca_trid)
mod8<- gls(CLS_fut~CLS_curr, correlation = corExp(form = ~ Lat+Lon), data=jaca_trid)
mod9<- gls(CLS_fut~CLS_curr, correlation = corRatio(form = ~ Lat+Lon), data=jaca_trid)
mod10<-gls(CLS_fut~CLS_curr, correlation = corSpher(form = ~ Lat+Lon), data=jaca_trid)
mod5nulo<- gls(CLS_fut~1, correlation = corExp(form = ~ Lat+Lon), data=jaca_trid)

(camp_robu_aics<-model.sel(mod7,mod8,mod9,mod10,mod6))
hist(resid(mod8))
summary(mod8)
sqrt(rsquared(mod8)[5])

mod11<- gls(CLS_fut~CLS_curr, correlation = corGaus(form = ~ Lat+Lon), data=pyrr_crue)
plot(mod1)
plot(resid(mod1))
hist(resid(mod1))

mod12<- gls(CLS_fut~CLS_curr, correlation = corLin(form = ~ Lat+Lon), data=pyrr_crue)
mod13<- gls(CLS_fut~CLS_curr, correlation = corExp(form = ~ Lat+Lon), data=pyrr_crue)
mod14<- gls(CLS_fut~CLS_curr, correlation = corRatio(form = ~ Lat+Lon), data=pyrr_crue)
mod15<-gls(CLS_fut~CLS_curr, correlation = corSpher(form = ~ Lat+Lon), data=pyrr_crue)
mod6nulo<- gls(CLS_fut~1, correlation = corExp(form = ~ Lat+Lon), data=pyrr_crue)

(camp_robu_aics<-model.sel(mod11,mod12,mod13,mod14,mod15))
hist(resid(mod14))
summary(mod14)
sqrt(rsquared(mod14)[5])

mod36<- gls(CLS_fut~CLS_curr, correlation = corGaus(form = ~ Lat+Lon), data=proc_nudi)
mod37<- gls(CLS_fut~CLS_curr, correlation = corLin(form = ~ Lat+Lon), data=proc_nudi)
mod38<- gls(CLS_fut~CLS_curr, correlation = corExp(form = ~ Lat+Lon), data=proc_nudi)
mod39<- gls(CLS_fut~CLS_curr, correlation = corRatio(form = ~ Lat+Lon), data=proc_nudi)
mod40<-gls(CLS_fut~CLS_curr, correlation = corSpher(form = ~ Lat+Lon), data=proc_nudi)
(camp_robu_aics<-model.sel(mod36,mod37,mod38,mod39,mod40))
mod16nulo<- gls(CLS_fut~1, correlation = corExp(form = ~ Lat+Lon), data=proc_nudi)
hist(resid(mod38))
summary(mod38)
sqrt(rsquared(mod38)[5])

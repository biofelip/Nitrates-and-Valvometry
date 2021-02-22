setwd("C:/Users/felip/Documents/PhD/Douda/experiments joanna/Second approach/Pruebabinomial/5fps")
library(tidyverse)
library(plotly)
library(lubridate)
library(caret)
library(pROC)
library(bbplot)
library(exact2x2)

filestoread=list.files(pattern = ".csv")
filestoread=filestoread[-grep("rois", filestoread)]
reslist=lapply(filestoread, read.csv)
names(reslist)=filestoread
for(i in 1:96){reslist[[i]]$action=filestoread[i]}
names(reslist)=gsub("resultados", "", names(reslist))
names(reslist)=gsub(".mp4.csv", "", names(reslist))


##plot side by side
names(reslist)=gsub("TAM4", "T4M4",names(reslist) )
names(valvom96)=gsub("TAM4", "T4M4",names(valvom96) ) 
##resulta que hay un grupo de 6 datos que no tienen equivalencia en la
##valvometria: NEGATIVE1-T6M4 o que no se cuales son porque les puse un 
##nombre estupido (probblemente algo asi pero estos no estan bien cuadrado)
#SYPHONLEVE1-T4M1<->NEGATIVE3-T4M3
#SYPHONLEVE1-T4M3<->NEGATIVE4-T4M3
#SYPHONLEVE2-T4M1<->NEGATIVE2-T4M1
#SYPHONLEVE2-T4M3<->NEGATIVE3-T4M1
#SYPHONLEVE3-T4M1<->NEGATIVE4-T4M1

reslist90=reslist[-grep("SYPHONLEVE", names(reslist))]
reslist90=reslist90[-grep("NEGATIVE1-T6M4",names(reslist90))]

valvom90=valvom96[!names(valvom96)%in% c("NEGATIVE3-T4M3", "NEGATIVE4-T4M3", 
                                         "NEGATIVE2-T4M1",
                                         "NEGATIVE3-T4M1", "NEGATIVE4-T4M1", 
                                         "NEGATIVE1-T6M4") ]
valvom90[["NA"]]=NULL
match( names(reslist90),names(valvom90))
valvom90[is.null()]
valvom90[["SYPHON1-T6M4"]]=NULL
reslist90[["SYPHON1-T6M4"]]=NULL
reslist90=reslist90[-(which(names(reslist90) %in% names(valvom90)==FALSE))]

  
##creation of the roc curves
##1sr create the df
varvid=do.call(rbind, reslist90) %>% group_by(action) %>%
  summarise(media=mean(Area/max(Area)),
            sd=sd(Area/max(Area)),
            rango=diff(range(Area/max(Area))),
            cv=sd(Area)/mean(Area),
            snr=mean(Area)/sd(Area),
            )


varvalv=do.call(rbind, valvom90) %>%  group_by(id) %>% 
  summarise(media=mean(MFI/max(MFI)),
            sd=sd(MFI/max(MFI)),
            rango=diff(range(MFI/max(MFI))),
            cv=sd(MFI)/mean(MFI),
            snr=mean(MFI)/sd(MFI))

varvid$class=1; varvid$class[grep("NEGATIVE", varvid$action)]=0

varvalv$class=1; varvalv$class[grep("NEGATIVE", varvalv$id)]=0

##2 calculate the rocs

rocvid=roc(varvid$class, varvid$cv, smooth = FALSE)
rocvalv=roc(varvalv$class, varvalv$cv, smooth = FALSE)


## find the best sen-spec point
variables=c("threshold","specificity", "sensitivity", "accuracy", "npv", "ppv",
            "tp", "tn", "fp", "fn")
bestv=coords(rocvid, x="best", best.method = "c", ret = variables, transpose = T)
bestvl=coords(rocvalv, x="best", best.method = "c",ret =variables, transpose = T)

##calculate the CI for said points

CIv=rbind(ci.sp(rocvid, unlist(bestv[["specificity"]]), conf.level = 0.95,
                boot.n = 10000), 
          ci.se(rocvid,unlist(bestv[["sensitivity"]]), boot.n = 10000))
CIvl=rbind(ci.sp(rocvalv, unlist(bestvl[["specificity"]]), conf.level = 0.95,
                 boot.n = 10000), 
           ci.se(rocvalv,unlist(bestvl[["sensitivity"]]), boot.n = 10000))


Civall=ci.coords(rocvid, x=unlist(bestv[["sensitivity"]]), 
                 input = "sensitivity", ret=variables)
Civlall=ci.coords(rocvalv, x=unlist(bestv[["sensitivity"]]), 
                 input = "sensitivity", ret=variables)
Civall=do.call(rbind, Civall); rownames(Civall)=variables
Civlall=do.call(rbind, Civlall); rownames(Civlall)=variables
Civall=cbind( bestv,Civall)
Civlall=cbind(bestvl, Civlall)

write.table(cbind(Civall, Civlall), "clipboard", sep = "\t")



lista=list(Valvometry=rocvalv, Video=rocvid)
names(lista)=c(paste("Valvometry",  "\n" ,"AUC: ", round(auc(rocvalv),4), sep = ""), 
               paste("Video","\n" ,"AUC: ", round(auc(rocvid),4), sep=""))

p=ggroc(lista, size=1.5)+
  theme_minimal()+geom_point(aes(x=unlist(bestv[["specificity"]]), 
                                 y=unlist(bestv[["sensitivity"]])), color="#00BFC4", size=4)+
  geom_point(aes(x=unlist(bestvl[["specificity"]]), 
                 y=unlist(bestvl[["sensitivity"]])), color="#F8766D", size=4)+
ylim(0.25,1)+xlim(0.10,1)+scale_x_reverse()+
  bbc_style()+xlab("Specificity")+ylab("Sensitivity")


ggsave("roc.jpg", p, width = 24, height = 18, units = "cm", dpi = 300)


##obtaining p values
#exosten tres metodos de comparcion, comparar las areas bajo la curva
#comparar las ROC como tal y comparar los valores de especificidad sensitividad
#para el trercer caso se calcula el si hay diferencia en la metrica opuesta a la que 
# se esta definiendo, por ejemplo so proveo una sensibilidad entonces calcula 
#diferencia para la especificidad y de la misma forma y en el sentido contrario :)

#comparando areas con metodo de boostrap 
roc.test(rocvalv, rocvid, alternative = "l", method = "bootstrap", paired = TRUE,
         boot.n = 10000, boot.stratified = FALSE)
# Bootstrap test for two correlated ROC curves
# 
# data:  rocvalv and rocvid
# D = -1.4143, boot.n = 10000, boot.stratified =
#   0, p-value = 0.07864
# alternative hypothesis: true difference in AUC is less than 0
# sample estimates:
#   AUC of roc1 AUC of roc2 
# 0.9618512   0.9968730 

#COMPARANDO AREAS CON METODO DE Delong

roc.test(rocvalv, rocvid, alternative = "l", method = "delong", paired = TRUE)

#DeLong's test for two correlated ROC curves

# data:  rocvalv and rocvid
# Z = -1.4187, p-value = 0.078
# alternative hypothesis: true difference in AUC is less than 0
# sample estimates:
# AUC of roc1 AUC of roc2 
#   0.9618512   0.9968730 

#COMPARANDO ROC CON METODO DE venkatraman

roc.test(rocvalv, rocvid, method = "venkatraman", paired = TRUE, boot.n = 10000  )

# Venkatraman's test for two paired ROC curves
# 
# data:  rocvalv and rocvid
# E = 88, boot.n = 10000, p-value = 0.3185
# alternative hypothesis: true difference in AUC is not equal to 0


##comparando specificifdad? 
#tengo que especificar un nivel de sensibilidad para que clalcule si hay dif
#en la especificidad entonces deberia usar los valores del mas pqequeñp?

roc.test(rocvalv, rocvid, method = "sensitivity", paired = TRUE, boot.n = 10000, 
         sensitivity = 0.97,alternative = "l",  boot.stratified = FALSE)

# Sensitivity test for two correlated ROC
# curves
# 
# data:  rocvalv and rocvid
# D = -0.18766, boot.n = 10000, boot.stratified
# = 0, p-value = 0.4256
# alternative hypothesis: true difference in AUC is less than 0
# sample estimates:
#   AUC of roc1 AUC of roc2 
# 0.9618512   0.9968730 

##comparando senitividad

a=roc.test(rocvalv, rocvid, method = "specificity", paired = TRUE, boot.n = 10000, 
         specificity = 0.92,alternative = "l")

##test de mcnamara para las otras proporciones espero que quede bien
##accuaracy:1=tp+tn  0=total-(tp+tn)=fp+fn
##negative predictive value: 1=tn, 0=fn=negative-tn
##positive predicte value: 1=tp, 0=positives-tp=fp

varvid$action=gsub("resultados|.mp4", "", varvid$action)
varvid$action=gsub("TAM4", "T4M4", varvid$action)
orden=match(varvid$action, varvalv$id)

mct=data.frame(id=varvid$action, gold=varvid$class,
               vidatt=ifelse(varvid$cv >= bestv[1], 1, 0),
               vlvat=ifelse(varvalv$cv >=bestvl[1],1,0))
mct$tpv=ifelse(mct$gold==mct$vidatt,1,0);mct$tpvl=ifelse(mct$gold==mct$vlvat,1,0)
table(mct$tpv, mct$tpvl)
with(filter(mct, gold==0), table(tpv, tpvl))
with(filter(mct, gold==1), table(tpv, tpvl))

mcnemar.exact(matrix(c(1,1,3,75), nrow = 2,ncol = 2, byrow = TRUE), conf.level = .95)
mcnemar.exact(matrix(c(1,0,2,36), nrow = 2,ncol = 2, byrow = TRUE), conf.level = .95)
mcnemar.exact(matrix(c(0,1,1,39), nrow = 2,ncol = 2, byrow = TRUE), conf.level = .95)

#nececito definir un t para esto por suerte gracias a que utilize el CV creo que
#puedo haver esto sin sufrif mucho, voy a utilizar el 

# par(mfrow=c(1,2))
# for (i in names(reslist90)){
#   plot(reslist90[[i]]$Area,type="l", col="blue", main=i)
#   plot(valvom90[[i]]$MAGNETIC.FIELD.INTENSITY, 
#        type="l", col="red", main=i)}
# 
# 
# for (i in names(reslist90)[grep("T2M2", names(reslist90))]){
#   fig1 <- plot_ly(reslist90[[i]], x = ~seq_along(Area), y = ~Area)
#   fig1 <- fig1 %>% add_lines(name = ~"Video")
#   fig2 <- plot_ly(valvom90[[i]], x = ~seq_along(X), y = ~MAGNETIC.FIELD.INTENSITY)
#   fig2 <- fig2 %>% add_lines(name = ~"Valvometry")
#   fig3 <- plot_ly(y=~Valvoml[[matrix(unlist(strsplit(i,"-")),nrow = 1, ncol=2, byrow = TRUE)[,2]]]$MAGNETIC.FIELD.INTENSITY, 
#                   x=~Valvoml[[matrix(unlist(strsplit(i,"-")),nrow = 1, ncol=2, byrow = TRUE)[,2]]]$TIME)
#   fig3=fig3 %>%  add_lines(name=~"valvometryfull")
#   fig <- subplot(fig1, fig2, fig3, nrows = 3) %>% layout(title=i)
#   print(fig)
# }
# ### time series clustering, 
# library(TSclust)
# library(ape)
# library(dendextend)
# library(circlize)
# library(cluster)
# Areas=lapply(reslist, function(x) x$Area/max(x$Area))
# names(Areas)=gsub("resultados","", names(Areas));names(Areas)=gsub(".mp4.csv","", names(Areas))
# names(Areas)=gsub("NEGATIVE", "N", names(Areas));names(Areas)=gsub("SYPHON", "S", names(Areas))
# names(Areas)=gsub("LEVE", "L", names(Areas)); names(Areas)=gsub("resultados", "", names(Areas))
# Circ=lapply(reslist, function(x) x$Circularity)
# Perimeter=list()
# for (i in 1:96){Perimeter[[i]]=sqrt(Circ[[i]]*Areas[[i]])}
# names(Circ)=names(Areas)
# Ecc=lapply(reslist, function(x) x$Eccentricity)
# names(Ecc)=names(Areas)
# 
# ###otro intento con machine learning
# ###datos del video
# cluster=as.integer(sds$action); names(cluster)=names(Areas)
# wtsc=cbind(cluster, sds[,-4])
# wtsc$cluster[wtsc$cluster==1]="1"
# wtsc$cluster[wtsc$cluster==2]="2"
# wtsc$cluster=as.factor(wtsc$cluster)
# ##datos de valvometria
# clusterv=as.integer(sdsv$action); names(cluster)=names(sdsv)
# wtscv=cbind(cluster, sds[,-4])
# wtsc$cluster[wtsc$cluster==1]="1"
# wtsc$cluster[wtsc$cluster==2]="2"
# wtsc$cluster=as.factor(wtsc$cluster)
# 
# library(party)
# set.seed(1992)
# index=sample(96, 32)
# indexv=sample(90, 30)
# ct <- ctree(action ~ Area, data=sds[index,],
#             controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5))
# pcluster <- predict(ct,newdata=sds[-index,])
# 
# table(sds[-index,]$action, pcluster)
# plot(ct, ip_args=list(pval=TRUE), ep_args=list(digits=0))
# ctv <- ctree(action ~ Percentage, data=sdsv[indexv,],
#             controls = ctree_control(minsplit=30, minbucket=10, maxdepth=5))
# pclusterv <- predict(ctv,newdata=sdsv[-indexv,])
# 
# table(sdsv[-indexv,]$action, pclusterv)
# ##prueba binomial como tal 
# library(DescTools)
# binomt=data.frame(Senc=c(34,32,24,35), Seni=c(1,3,11,0),SpeC=c(25,28,29,0), SpecI=c(4,1,0,29))
# rownames(binomt)=c("Area", "Eccentricity", "Perimeter", "Circularity")
# for(i in 1:4) {print( BinomCI(binomt$Senc[i],35, method = "wilson", sides = "l" ))
#   print( BinomCI(binomt$SpeC[i],29, method = "wilson", sides = "l" ))}
# for(i in 1:4) {print( binom.test(binomt$Senc[i],35, alternative  = "g" ))
#   print( binom.test(binomt$SpeC[i],29,alternative  = "g"  ))}
# 
# ## medida de distancia Dynamic time warping esta medida tiene el problema
# ## de que es insensible a la escala, algo que es importante en mis series de tiempo
# AreaDTW=lapply(list(Areas, Circ, Ecc, Perimeter), diss,  METHOD = "DTW")
# hcs=lapply(AreaDTW, hcut,hc_func="hclust" , method = "ward.D2", isdiss=TRUE,k=3)
# 
# cluster2=lapply(hcs, function(x) cutree(x, k=5))
# colors = c("blue", "green", "red", "orange")
# fviz_dend(hcs[[1]], k=2, label_cols = colors[cluster[hcs[[1]]$order]], main="Area")
# fviz_dend(hcs[[2]], k=2, label_cols = colors[cluster[hcs[[2]]$order]], main="Circ")
# fviz_dend(hcs[[3]], k=2, label_cols = colors[cluster[hcs[[3]]$order]], main="Ecc")
# fviz_dend(hcs[[4]], k=2, label_cols = colors[cluster[hcs[[4]]$order]], main="Perimeter")
# 
# 
# ###distancia CDM y NCD basadas en complejidad
# CDM=lapply(list(Areas, Circ, Ecc), diss,  METHOD = "CDM")
# hcscdm=lapply(CDM, hcut,hc_func="hclust" , method = "ward.D2", isdiss=TRUE, k=3)
# fviz_dend(hcscdm[[1]], k=2, label_cols = colors[cluster[hcscdm[[1]]$order]], main="Area")
# fviz_dend(hcscdm[[2]], k=2, label_cols = colors[cluster[hcscdm[[2]]$order]], main="Circ")
# fviz_dend(hcscdm[[3]], k=2, label_cols = colors[cluster[hcscdm[[3]]$order]], main="Ecc")
# 
# 
# 
# cluster=as.integer(sds$action); names(cluster)=names(Areas)
# hcscdmdf=cbind(cluster, hcscdm[[1]]$cluster,hcscdm[[2]]$cluster,hcscdm[[3]]$cluster)
# hcscdmdf=as.data.frame(hcscdmdf)
# colnames(hcscdmdf)=c("Correct", "Area", "Circularity", "Eccentricity")

#prueba binomial como tal
# library(DescTools)
# BinomCI(40, 41,method = "wilson", sides = "l")
# 
# 
# binomt=data.frame(Senc=c(34,32,24,35), Seni=c(1,3,11,0),SpeC=c(25,28,29,0), SpecI=c(4,1,0,29))
# rownames(binomt)=c("Area", "Eccentricity", "Perimeter", "Circularity")
# for(i in 1:4) {print( BinomCI(binomt$Senc[i],35, method = "wilson", sides = "l" ))
#   print( BinomCI(binomt$SpeC[i],29, method = "wilson", sides = "l" ))}
# for(i in 1:4) {print( binom.test(binomt$Senc[i],35, alternative  = "g" ))
#   print( binom.test(binomt$SpeC[i],29,alternative  = "g"  ))}


# roccomp=roc.test(varvalv$class, varvalv$cv, varvid$cv, method = "delong", 
#                  alternative="less", paired=TRUE, plot=TRUE,)
# roccomp2=roc.test(varvalv$class, varvalv$cv, varvid$cv, method = "b", 
#                   alternative="less", paired=TRUE, smooth=TRUE)
# roccomp3=roc.test(varvalv$class, varvalv$cv, varvid$cv, method = "v", 
#                   paired=TRUE)
# 
# coords(rocvid, x="best", best.method = "youden")
# ci.se(rocvalv)
# 
# 
# coords(rocvalv, x="best")
# roccomp4=roc.test(varvalv$class, varvalv$cv, varvid$cv, method = "specificity", 
#                   specificity=1, paired=TRUE)

# png("ROC.png", units = "px", width = 2700, height = 1350, res = 300)
# plot.roc(varvid$class, varvid$cv, auc.polygon=F,
#          grid=TRUE, print.auc=FALSE, 
#          auc.polygon.col=rgb(0.56,0.78,0.39, alpha=0.5),
#          smooth=F,  asp=0.47,xlim=c(1,-0.3),)
# plot.roc(varvalv$class, varvalv$cv, auc.polygon=F,
#          auc.polygon.col=rgb(0.39,0.56,0.78,alpha=0.5),
#          grid=TRUE, print.auc=FALSE, add=TRUE, smooth=F)
# 
# legend(0.0,0.6, legend =c(paste("Video",  "\n" ," AUC: ", round(auc(rocvid),2), sep = ""), 
#                           paste("Valvometry","\n" ," AUC: ", round(auc(rocvalv),2)), sep=""),
#        fill=c("#8FC76380", "#638FC780" ), text.font = 2, bty = "n",
#        x.intersp = 1, y.intersp = 1.5, cex = 1) 
# dev.off()
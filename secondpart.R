##este script busca extraer los datos de valvometria y analsisis de videos
##para todos los videos de Control, 250 y 500 mg para los 5 primeros minutos  
##posteriores a la adicion de los nitratos
setwd("C:/Users/felip/Documents/PhD/Douda/experiments joanna/Second approach/analisis  videos completo")
library(tidyverse)
library(ggplot2)
library(changepoint)
library(gganimate)
library(plotly)
completedata=read.csv("5obsseccompleto.csv", header = TRUE)
Valvom=completedata %>% filter(CONCENTRATION %in% c("250mg", "500mg") & TFE >= 0 & TFE <=180)
Valvoml=split(Valvom, f=list(Valvom$BIVALVE, Valvom$CONCENTRATION))
Valvoml=Valvoml[lapply(Valvoml, nrow)>0]
names(Valvoml)=c("T5M1-250", "T4M3-250","T6M4-250", "NA1","T4M2-250", "T4M5-250", "T6M1-250","NA2","T3M1-500","T5M3-500","NA3","T4M1-500","T3M3-500","T5M5-500","T6M5-500","T6M2-500")
Valvoml=Valvoml[-grep(pattern = "NA", names(Valvoml))]

valp=ggplot(Valvom, aes(x=TFE, y=Percentage))+geom_line()+facet_wrap(~CONCENTRATION+BIVALVE)
valp=valp + geom_point() +transition_reveal(TFE)
b=animate(valp, fps = 5, nframes=900, renderer = ffmpeg_renderer())
anim_save("allvalv.mp4", b)


###haciendo el video para el anexo vien aspero
valanex=Valvoml2[[2]]

plotvanex=ggplot(valanex, aes(x=index(valanex)+35, y=Percentage, col="Valvometry"))+geom_line()+
  geom_line(data = imagelist[[9]], aes(x=index(imagelist[[9]]), y=Area, col="Image analysis"))+
  labs(x="Index", y="Percentage")
plotvanex=plotvanex + transition_reveal(index(imagelist[[9]]))
plotanexb=animate(plotvanex, fps=5, nframes=900, renderer=ffmpeg_renderer())
anim_save("pltoanex.mp4", plotanexb)




setwd("CUTVIDEOS")
filenames=list.files(pattern = ".csv")
imagelist=lapply(filenames,read.csv)
names(imagelist)=filenames
imagelist=lapply(imagelist, function(x) x=x[,1:3])
for ( i in 1: length(imagelist)){imagelist[[i]]$TEF=1:nrow(imagelist[[i]])}
imagelist=lapply(imagelist, function(x) x=subset(x, TEF<= 900 ))
for ( i in 1: length(imagelist)){imagelist[[i]]$video=filenames[i]}
for ( i in 1: length(imagelist)){imagelist[[i]]$Area=imagelist[[i]]$Area/max(imagelist[[i]]$Area)}


##utlizando ´pltly para encontrar los puntos erroneos, el rollmena sirve para quitar los cambios
##fuerte que no representan  cambio real
for ( i in 1: length(imagelist)){imagelist[[i]]$Area=rollmean(imagelist[[i]]$Area,5,fill = c(mean(imagelist[[i]]$Area[1:300]),mean(imagelist[[i]]$Area[300:600]), mean(imagelist[[i]]$Area[600:900])))}
for ( i in 1: length(imagelist)){imagelist[[i]]$Eccentricity=rollmean(imagelist[[i]]$Eccentricity,5,fill = c(mean(imagelist[[i]]$Eccentricity[1:300]),mean(imagelist[[i]]$Eccentricity[300:600]), mean(imagelist[[i]]$Eccentricity[600:900])))}

p=do.call(rbind, imagelist[-c(1,2,3,6,10)]) %>% ggplot( aes(x=TEF, y=Area))+geom_line()+geom_line(aes(x=TEF, y=Eccentricity), color="red")+facet_wrap(~video)
p=p + geom_point() +transition_reveal(TEF)
a=animate(p, fps = 5, nframes=900, renderer = ffmpeg_renderer())
anim_save("all.mp4", a)
ggplotly(p)
##changepoint media 

cptmvars=lapply(imagelist[-c(1,2,3,6,10)], function(x) cpt.mean(x$Area, method = "PELT",penalty = "Manual", pen.value = 0.015 ))
for ( i in 1:length(cptmvars)){plot(cptmvars[[i]], main=names(cptmvars)[i])} 
cptvalv=lapply(Valvoml, function(x) cpt.mean(x$Percentage, method = "PELT",penalty = "Manual", pen.value = 0.015 ))
for ( i in 1:length(cptvalv)){plot(cptvalv[[i]], main=names(cptvalv)[i])} 
correccion=c(11,18,37,0,38,10,-161,17,11,-232,9,50,18)

##Because the algotithm is detecting some point that are not the
##ones we are interested in we will select the correct points from
##each set of changepoint to match the actual initial change
fstVid=c(1,2,4,1,1,2,1,1,1,1,3,1,1,2)
fstvalv=c(1,1,1,1,1,1,2,1,1,1,1,1,1,1)
firstovs=data.frame(matrix(nrow = 14, ncol = 2))
colnames(firstovs)=c("video", "Img")
for ( i in 1:length(cptmvars)){
  firstovs$video[i]=names(cptmvars)[i]
  firstovs$Img[i]=cpts(cptmvars[[i]])[fstVid[i]]}
firstovs$video=gsub("resultados","",firstovs$video); firstovs$video=gsub(".mp4.csv","",firstovs$video)

firstovs2=data.frame(matrix(nrow = 13, ncol = 2))
colnames(firstovs2)=c("video", "valv")
for ( i in 1:length(cptvalv)){
  firstovs2$video[i]=names(cptvalv)[i]
  firstovs2$valv[i]=cpts(cptvalv[[i]])[fstvalv[i]]+correccion[i]}
fobs=merge(firstovs, firstovs2, by="video", all.x = TRUE)
## erase the two  rowa that did not work properly or were not 
##represented in both data types  T4M2-250 and T5M4-500
fobs=fobs[-c(4,9),]
fobs$conc="500mg"
fobs$conc[grep("250", fobs$video)]="250mg"
fobs$valvs=fobs$valv*0.20;fobs$Imgs=fobs$Img*0.20


##comparison in reaction times
##tip for the future: the default device for plots in r studio renders
##the grpahed jagged and awfull the last line prevents this in the final
#plot by using the cairo device instead
P2=fobs %>%    select(-c(Img, valv)) %>%  pivot_longer(cols = -c(video, conc), names_to="variable", values_to="time")   %>%   
  ggplot( aes(x=variable, y=time, col=conc))+geom_point()+geom_line(aes(group=video), size=0.7)+facet_wrap(~conc)+ylab("Time to first reaction (seconds)")+
  scale_x_discrete("Detection method", labels=c("Image \n analysis", "Valvometry"))+theme(legend.position = "none")+
  ggsave("ttestcomp.png", type = "cairo")
               
## t test 250###
plot(lm(valvs-Imgs~c(1:5), data = filter(fobs, conc=="250mg")))
plot(lm(valvs-Imgs~c(1:7), data = filter(fobs, conc=="500mg")),which=4)
pt250=with(subset(fobs, conc=="250mg"), t.test(valvs,Imgs,  paired = TRUE))
pt500=with(subset(fobs, conc=="500mg"), t.test(valvs,Imgs, paired = TRUE))

table(fobs$conc)


correccion2=correccion[-c(7,10)]
Valvoml2=Valvoml[-c(7,10)]
for (i in 1:length(Valvoml2)){print(ymd_hms(Valvoml2[[i]]$TIME[fobs$valv[-c(7,10)][i]])-ymd_hms(Valvoml2[[i]]$TIME[1+correccion2[i]]))}


for (i in 1:length(Valvoml)){print(ymd_hms(Valvoml[[i]]$TIME[fobs$valv[i]])-ymd_hms(Valvoml[[i]]$TIME[correccion[i]]))}
diferencia=fobs$valvs-fobs$Imgs
tapply(diferencia[-8], fobs$conc[-8], sd)

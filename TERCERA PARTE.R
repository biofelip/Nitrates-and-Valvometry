source("C:/Users/felip/Documents/PhD/Douda/experiments joanna/SCRIPTS/cbindna.R")
source("C:/Users/felip/Documents/PhD/Douda/experiments joanna/SCRIPTS/rbindna.R")
setwd("C:/Users/felip/Documents/PhD/Douda/experiments joanna/Second approach/analisis  videos completo/controls/enhanced")
library(tidyverse)
library(zoo)
library(WaveletComp)
library(changepoint)
library(plotly)
filenames=list.files(pattern = ".csv")
listfiles=lapply(filenames, read.csv)
names(listfiles)=gsub(".csv", "", filenames)
listfiles=lapply(listfiles, function(x) x=x[,1:3])
for ( i in 1: length(listfiles)){listfiles[[i]]$id=gsub(".csv", "", filenames)[i]}
for ( i in 1: length(listfiles)){listfiles[[i]]$Area=listfiles[[i]]$Area/max(listfiles[[i]]$Area)}
for ( i in 1: length(listfiles)){listfiles[[i]]$X=index(listfiles[[i]])}
for ( i in 1: length(listfiles)){listfiles[[i]]$Area=rollmedian(listfiles[[i]]$Area,15,
                                                              fill = NA)}
listfiles=lapply(listfiles, function(x) x[complete.cases(x),])
for ( i in 1: length(listfiles)){listfiles[[i]]$diffarea=abs(diff(listfiles[[i]]$Area,lag=1))[1:length(listfiles[[i]]$Area)]}
for ( i in 1: length(listfiles)){listfiles[[i]]$X=listfiles[[i]]$X/5}



give_me_time_mf=function(x){minutes=x%/%60
                            seconds=format((x/60-minutes)*60, digits = 2, format="f")
                            tiempo=paste(minutes,":",seconds, sep = "")
                            return(tiempo)}
dfvideos=do.call(rbind, listfiles)

p=ggplot(data = dfvideos, aes(x=X, y=Area))+geom_line()+ facet_wrap(~id, scales = "free_x")
p
ggplotly(p)


setwd("C:/Users/felip/Documents/PhD/Douda/experiments joanna/Second approach/analisis  videos completo")
completedata=read.csv("5obsseccompleto.csv", header = TRUE)
Valvom=completedata %>% filter(CONCENTRATION == "CONTROL")
Valvom$id=with(Valvom, paste("L",LOGGER, "B", BIVALVE, sep = ""))
replacement=c("T1M1", "T1M4","T2M3","T2M2","T3M4","T3M2","T4M4")
names(replacement)=c("L18B1","L14B25","L15B18","L19B10","L14B24", "L16B20", "L18B6")
for (i in 1:7){Valvom$id[grep(names(replacement)[i], Valvom$id)]=replacement[i]}
Valvom=Valvom[-grep("T3M2", Valvom$id),]
Valvom=Valvom[-grep("T1M4", Valvom$id),]
##  hwere we are going to filter only the observations 1/2 hour before and 
#after of the exposure, meaning froom TFE -1800 to TFE 1800
Valvom=Valvom %>% filter(TFE >= -1800 & TFE <=1800)


### final data assempbly
valvoml=split(Valvom, Valvom$id)
## aligmemnt of the video number 4 to the left for some reason
##its misalinged for more than 9 minutes (this could be a mistake from 
##my part also but it fits better)
valvoml[[4]]=valvoml[[4]][2964:nrow(valvoml[[4]]),]
for ( i in 1:5){valvoml[[i]]=cbind.na(valvoml[[i]], listfiles[[i]]$Area)}
##we will also smooth the first series more applying another median filter
valvoml[[1]]$y=lowess(valvoml[[1]]$y, f = 3)$y
## finally we will turn the third series upside down T3m4
valvoml[[4]]$Percentage=1-valvoml[[4]]$Percentage
Valvom=do.call(rbind, valvoml)
Valvom=Valvom[complete.cases(Valvom),]
valvoml=split(Valvom, Valvom$id)
pv=ggplot(data = Valvom, aes(x=1800+TFE, y=Percentage))+geom_line()+geom_line(data = Valvom, aes(x=1800+TFE, y=y), color="green")+facet_wrap(~id, scales = "free")
pv
ggplotly(pv)
plot(lowess(valvoml[[1]]$y, f=1)$y, W = 0.01, type = "low", method = "ChebyshevI"), type="l")
plot(pass.filt(lowess(valvoml[[1]]$y, f=1)$y, W = 0.01, type = "high"), col="green")
lines(valvoml[[1]]$Percentage, col="red")
aver=ggplot(data = valvoml[[4]][2964:nrow(valvoml[[4]]),], aes(x=index(Percentage)/5, y=1-Percentage))+geom_line()+geom_line(data=valvoml[[4]], aes(x=index(y)/5, y=y), color="green")
ggplotly(aver)
## now we will calculate the wvt and do an estimation of the spectral
##density funciton according to https://rstudio-pubs-static.s3.amazonaws.com/9428_1197bd003ebd43c49b429f22ea4f36e5.html 
##  the  plitng of period agains power estimates this function
library(dplR)
### 
wave.out <- lapply(valvoml, function(x) morlet(y1 = x$y, x1 = index(x$y)/5,p2=11, dj = 0.1, siglvl = 0.95))
wave.avg <- lapply(wave.out, function(x) data.frame(power = apply(x$Power, 2, mean), period = (x$period)))
levsvid <- lapply(wave.out, function(x) quantile(x$Power, c(0, 0.25, 0.5, 0.75, 0.95, 1)))
setwd("C:/Users/felip/Documents/PhD/Douda/mis articulos/Valvometry paper")

#
for (i in 1){png(file = paste(names(wave.out)[i],"-videos",".png",  sep = ""))
  print(wavelet.plot(wave.out[[i]], wavelet.levels = levsvid[[i]], key.cols = hcl.colors(5, palette = "Zissou 1"), UseRaster=TRUE))
  dev.off()
  svg(file = paste(names(wave.out)[i],"-videos",".svg",  sep = ""))
  print(wavelet.plot(wave.out[[i]], wavelet.levels = levsvid[[i]], key.cols = hcl.colors(5, palette = "Zissou 1"), UseRaster=TRUE))
  dev.off()}

wave.outvalv=lapply(valvoml, function(x) morlet(y1 = x$Percentage, x1 = index(x$Percentage)/5,p2=11, dj = 0.1, siglvl = 0.95))
wave.avgvalv <- lapply(wave.outvalv, function(x) data.frame(power = apply(x$Power, 2, mean), period = (x$period)))
levs <- lapply(wave.outvalv, function(x) quantile(x$Power, c(0, 0.25, 0.5, 0.75, 0.95, 1)))
for (i in 4){png(file = paste(names(wave.outvalv)[i],"-valvs",".png",  sep = ""))
  print(wavelet.plot(wave.outvalv[[i]], wavelet.levels = levs[[i]], key.cols = hcl.colors(5, palette = "Zissou 1"), UseRaster=TRUE))
  dev.off()
  svg(file = paste(names(wave.outvalv)[i],"-valvs",".svg",  sep = ""))
  print(wavelet.plot(wave.outvalv[[i]], wavelet.levels = levs[[i]], key.cols = hcl.colors(5, palette = "Zissou 1"), UseRaster=TRUE))
  dev.off()}

## printing a plot of the main periods and powers for high frquency
##movements up to 32 seconds

for (i in seq(5)){wave.avg[[i]]$video=names(wave.avg)[i]
                  wave.avg[[i]]$method="video"
                  wave.avgvalv[[i]]$video=names(wave.avgvalv)[i]
                  wave.avgvalv[[i]]$method="valvometry"}

wave.avg.video=do.call(rbind, wave.avg)
wave.avg.valv=do.call(rbind, wave.avgvalv)
wave.avg.tot=rbind(wave.avg.valv, wave.avg.video)

ggplot(data = wave.avg.video, aes(x=period, y=power))+geom_line()+
  geom_line(data=wave.avg.valv, aes(x=period, y=power), color="blue")+
  facet_wrap(~video, scales = "free")
  
}aes(x=))
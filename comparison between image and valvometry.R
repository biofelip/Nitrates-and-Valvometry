setwd("C:/Users/felip/Documents/PhD/Douda/experiments joanna/Second approach/analisis  videos completo/scaled")
##Analsisi de la imagen
filelist=list.files(pattern = ".csv")
filelist=filelist[-6]
imanal=lapply(filelist, read.csv)
names(imanal)=filelist
for ( i in 1:length(imanal)){imanal[[i]]$Area[imanal[[i]]$Area==0]=NA}
for ( i in 1:length(imanal)){imanal[[i]]$Hu.moments1[imanal[[i]]$Hu.moments1>10^100]=NA}
for ( i in 1:length(imanal)){imanal[[i]]$Hu.moments2[imanal[[i]]$Hu.moments2>10^100]=NA}
for ( i in 1:length(imanal)){imanal[[i]]$Hu.moments3[imanal[[i]]$Hu.moments3>10^100]=NA}
## Valvometria
load("~/PhD/Douda/experiments joanna/Second approach/analisis  videos completo/scaled/ENVIROMENTS WITH COMPLETEDATA.RData")
completedata=completedata[seq(1, nrow(completedata),10),]
T4M1500=completedata[with( completedata, LOGGER==19 & CONCENTRATION == 500 & DAY==25 ),]
T4M1500=T4M1500[rowSums(is.na(T4M1500)) != ncol(T4M1500),]
T4M3250=completedata[with( completedata, LOGGER==14 & CONCENTRATION == 250 & DAY==25 ),]
T4M3250=T4M3250[rowSums(is.na(T4M3250)) != ncol(T4M3250),]
T1M1c=completedata[with( completedata, LOGGER==18 & CONCENTRATION == "CONTROL"  & DAY==24),]
T1M1c=T1M1c[rowSums(is.na(T1M1c)) != ncol(T1M1c),]
T1M250=completedata[with( completedata, LOGGER==16 & CONCENTRATION == 50  & DAY==24),]
T1M250=T1M250[rowSums(is.na(T1M250)) != ncol(T1M250),]
T2M4100=completedata[with( completedata, LOGGER==16 & CONCENTRATION == 100  & DAY==24),]
T2M4100=T2M4100[rowSums(is.na(T2M4100)) != ncol(T2M4100),]


##VECTOR DE SITIOS DE ADICION
ad=c(19677, 22272, 22673, 62679 ,58144)

T1M1c=T1M1c[sum(ad[1],-4500):sum(ad[1],4500),]
T1M250=T1M250[sum(ad[2],-4500):sum(ad[2],4500),]
T2M4100=T2M4100[sum(ad[3],-4500):sum(ad[3],4500),]
T4M1500=T4M1500[sum(ad[4],-4500):sum(ad[4],4500),]
T4M3250=T4M3250[sum(ad[5],-4500):sum(ad[5],4500),]

valvometria=list("T1M1c"= T1M1c,"T1M250"= T1M250, "T2M4100"=T2M4100, "T4M1500"=T4M1500, "T4M3250"=T4M3250)
plotnames=c("Control", "50 mg/L", "100 mg/L", "500 mg/L", "250 mg/L")

Areap=lapply(imanal, function(x) x$Area/max(x$Area, na.rm = TRUE))
Valvp=lapply(valvometria, function(x) x$MAGNETIC.FIELD.INTENSITY/max(x$MAGNETIC.FIELD.INTENSITY, na.rm = TRUE) )

##primera opcion variables escaldas
par(mfrow=c(3,2), cex=1)
for (i in 4){plot(x=1:length(imanal[[i]]$Area),y=scale(imanal[[i]]$Area), type = "l", main=plotnames[i], ylab="Scaled Area and Valve Aperture", xlab="Seconds", xaxt='n')
            axis(1,at=seq(1,length(imanal[[i]]$Area), by=1600),labels = seq(0,1800, by=320))
                          lines(x=1:length(valvometria[[i]]$MAGNETIC.FIELD.INTENSITY[-c(1:235)]), y=scale(valvometria[[i]]$MAGNETIC.FIELD.INTENSITY)[-c(1:235)], type = "l", col="red")
                          abline(v=4500, lty=6, col="blue", lwd=3)}

plot(x=1:length(imanal[[5]]$Area), scale(imanal[[5]]$Area), type = "l", main=plotnames[5], ylab=" Scaled Area and Valve Aperture", xlab="Seconds")
lines(x=1:length(valvometria[[i]]$MAGNETIC.FIELD.INTENSITY), scale(valvometria[[5]]$MAGNETIC.FIELD.INTENSITY), type = "l", col="red")
abline(v=4500, lty=6, col="blue", lwd=3)

##segunda opcion porcentajes

par(mfrow=c(2,2), cex=1)
for (i in 1:4){plot(x=1:length(Areap[[i]]), y=Areap[[i]], type = "l", main=plotnames[i], ylab="Area   and  magentic intensity (%)", xlab="Seconds")
  lines(x=1:length(Valvp[[i]][-c(1:235)]),y=Valvp[[i]][-c(1:235)], type = "l", col="red")
  abline(v=4500, lty=6, col="blue", lwd=3)}

plot(x=1:length(Areap[[5]]), y=Areap[[5]], type = "l", main=plotnames[5], ylab=" Scaled Area and magentic intensity", xlab="Seconds")
lines(x=1:length(Valvp[[5]][-c(1:235)]),y=Valvp[[5]][-c(1:235)], type = "l", col="red")
abline(v=4500, lty=6, col="blue", lwd=3)

##Comparando cuadro por cuadro con el video
for (i in 1:5){plot(x=1:length(imanal[[i]]$Area),y=scale(imanal[[i]]$Area), type = "l", main=plotnames[i], ylab="Scaled Area and Valve Aperture", xlab="Seconds")
  lines(x=1:length(valvometria[[i]]$MAGNETIC.FIELD.INTENSITY[-c(1:235)]), y=scale(valvometria[[i]]$MAGNETIC.FIELD.INTENSITY)[-c(1:235)], type = "l", col="red")
  abline(v=4500, lty=6, col="blue", lwd=3)}
identify(x=1:length(imanal[[1]]$Area),y=scale(imanal[[1]]$Area))
identify(x=1:length(imanal[[2]]$Area),y=scale(imanal[[2]]$Area))
identify(x=1:length(imanal[[3]]$Area),y=scale(imanal[[3]]$Area))
identify(x=1:length(imanal[[4]]$Area),y=scale(imanal[[4]]$Area))
identify(x=1:length(imanal[[5]]$Area),y=scale(imanal[[5]]$Area))

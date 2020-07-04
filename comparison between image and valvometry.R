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


for (i in 1:length(imanal)){plot(scale(imanal[[i]]$Area), type = "l", main=names(imanal)[i])
                          lines(scale(valvometria[[i]]$MAGNETIC.FIELD.INTENSITY), type = "l", col="red")}
seqplots=function(x){for ( i in 1:length(x$Hu.moments1)){
  jpeg(paste("plotfin",i,".jpg",sep = "" ))
  par(mfrow=c(2,2))
  plot(x=x$Area, main="Area", type = "b")
  abline(v=i, col="red")
  plot(x=x$Hu.moments1, main="Hu1", type = "b")
  abline(v=i, col="red")
  plot(x=x$Hu.moments2, main="Hu2", type = "b")
  abline(v=i, col="red")
  plot(x=x$Hu.moments3, main="Hu3", type = "b")
  abline(v=i, col="red")
  dev.off()}}

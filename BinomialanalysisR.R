pathh=choose.dir()
install.packages("rlist")
library(rlist)
setwd("C:\\Users\\felip\\Documents\\PhD\\Douda\\experiments joanna\\Second approach\\Pruebabinomial")
filestoread=list.files(pattern=".csv")
filestoread=filestoread[-49]
dataframes=lapply(filestoread, read.csv)
names(dataframes)=filestoread
plotnames=gsub(".mp4.csv", replacement = "", x = filestoread)
for ( i in 1:length(dataframes)){dataframes[[i]]$Hu.moments1[dataframes[[i]]$Hu.moments1>10^100]=NA}
for ( i in 1:length(dataframes)){dataframes[[i]]$Hu.moments2[dataframes[[i]]$Hu.moments2>10^100]=NA}
for ( i in 1:length(dataframes)){dataframes[[i]]$Hu.moments3[dataframes[[i]]$Hu.moments3>10^100]=NA}
#for ( i in 1:length(dataframes)){dataframes[[i]]$Area[dataframes[[i]]$Hu.moments>10^100]=NA}
Areas=unlist(lapply(dataframes, function (x) diff(range(x$Area))))
Hu1=unlist(lapply(dataframes, function (x) diff(range(x$Hu.moments1, na.rm = TRUE))))
Hu2=unlist(lapply(dataframes, function (x) diff(range(x$Hu.moments2, na.rm = TRUE))))
Hu3=unlist(lapply(dataframes, function (x) diff(range(x$Hu.moments3, na.rm = TRUE))))


for ( i in 1:length(dataframes)){plot(dataframes[[i]]$Area, main =plotnames[i], type = "l" )}

plot(x=dataframes[[i]]$X, y=dataframes[[i]]$Hu.moments, main =filestoread[i], type="l")}

dffinal=data.frame( Areas, Hu1, Hu2, Hu3)
dffinal=as.data.frame(apply(dffinal, 2, scale))
plot(dffinal$Areas, type = "p")
lines(dffinal$Hu1, type = "p", col="green")
lines(dffinal$Hu2, type = "p", col="blue")
lines(dffinal$Hu3, type = "p", col="red")
abline(v=24)

seqplots=function(x){for ( i in 1:length(x$X)){dir.create(paste(plotnames[i],"dir", sep = ""))
                                  jpeg(paste(plotnames[i],"dir", "/",plotnames[i],".jpg",sep = "" ))
                                  par(mfrow=c(2,2))
                                  plot(x$Area, main="Area")
                                  abline(v=i, col="red")
                                  plot(x$Hu1.moments1, main="Hu1")
                                  abline(v=i, col="red")
                                  plot(x$Hu1.moments2, main="Hu2")
                                  abline(v=i, col="red")
                                  plot(x$Hu1.moments3, main="Hu3")
                                  abline(v=i, col="red")
                                  dev.off()}}
seqplots(dataframes[[1]])
write.table(filestoread, 'clipboard')

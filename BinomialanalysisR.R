pathh=choose.dir()
install.packages("DescTools")
setwd("C:\\Users\\felip\\Documents\\PhD\\Douda\\experiments joanna\\Second approach\\Pruebabinomial")
filestoread=list.files(pattern=".csv")
filestoread=filestoread[-49]
dataframes=lapply(filestoread, read.csv)
names(dataframes)=filestoread

plotnames=gsub(".mp4.csv", replacement = "", x = filestoread)
for ( i in 1:length(dataframes)){dataframes[[i]]$X[]=plotnames[i]}
for ( i in 1:length(dataframes)){dataframes[[i]]$Hu.moments1[dataframes[[i]]$Hu.moments1>10^100]=NA}
for ( i in 1:length(dataframes)){dataframes[[i]]$Hu.moments2[dataframes[[i]]$Hu.moments2>10^100]=NA}
for ( i in 1:length(dataframes)){dataframes[[i]]$Hu.moments3[dataframes[[i]]$Hu.moments3>10^100]=NA}


#for ( i in 1:length(dataframes)){dataframes[[i]]$Area[dataframes[[i]]$Hu.moments>10^100]=NA}
Areas=unlist(lapply(dataframes, function (x) sd(x$Area)))
Hu1=unlist(lapply(dataframes, function (x) sd(x$Hu.moments1, na.rm = TRUE)))
Hu2=unlist(lapply(dataframes, function (x) sd(x$Hu.moments2, na.rm = TRUE)))
Hu3=unlist(lapply(dataframes, function (x) sd(x$Hu.moments3, na.rm = TRUE)))


for ( i in 1:length(dataframes)){plot(dataframes[[i]]$Area, main =plotnames[i], type = "l" )}

plot(x=dataframes[[i]]$X, y=dataframes[[i]]$Hu.moments, main =filestoread[i], type="l")

dffinal=data.frame( Areas, Hu1, Hu2, Hu3)
dffinal=as.data.frame(apply(dffinal, 2, scale))
plot(dffinal$Areas, type = "l")
lines(dffinal$Hu1, type = "l", col="green")
lines(dffinal$Hu2, type = "b", col="blue")
lines(dffinal$Hu3, type = "b", col="red")
abline(v=24)

library(ggplot2)
library(tidyr)
library(dplyr)
dataframesdf=do.call(rbind.data.frame, dataframes)
dataframesdf[,-1]=scale(dataframesdf[,-1])

df.summary <- dataframesdf  %>% group_by(X) %>% summarize(stdA = sd(Area, na.rm = TRUE),
                                                          stdH = sd(Hu.moments1, na.rm = TRUE),
            ymeanA = mean(Area, na.rm = TRUE),
            ymeanH = mean(Hu.moments1, na.rm = TRUE))
##plot area
ploteo=ggplot(df.summary, aes(x=1:48, y=ymeanA)) + 
  geom_point(size = 2, colour="red") +
  geom_errorbar(aes(ymin = ymeanA+stdA, ymax = ymeanA-stdA))
ploteo+geom_vline(xintercept = 24, linetype="dotted", 
                  color = "blue", size=1.5)+annotate("text", x=c(11,37), y=4, label=c("Negative", "Positive"))+
  labs(x="Videos", y="Mean ± SD")+theme_classic()+ggtitle("Area",)

##plot Hu momentos
ploteoH=ggplot(df.summary, aes(x=1:48, y=ymeanH)) + 
  geom_point(size = 2, colour="green") +
  geom_errorbar(aes(ymin = ymeanA+stdA, ymax = ymeanA-stdA))+geom_vline(xintercept = 24, linetype="dotted", 
                  color = "blue", size=1.5)+annotate("text", x=c(11,37), y=4, label=c("Negative", "Positive"))+
  labs(x="Videos", y="Mean ± SD")+theme_classic()+ggtitle("Hu moments")

seqplots=function(x){for ( i in 1:length(x$Area)){
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
seqplots(dataframes[["resultadosSYPHON5-T2M2.mp4.csv"]])
write.table(filestoread, 'clipboard')


### debido a que el calculo de intervalosd e confianza pra un proceso binimial
##sufre de  reduccion en el poder y la precision debdio a cambios especificos
##calculare un test exacto binomial para saber si difiere de 0.5 mi proporcion
##y luego utilizare el metodo de Agresti of Jefrey para calcular el intervalo
##de  confianza
binrNEG=read.csv(file = 'resultsbinomial.csv', header = TRUE)[1:24,]
binrPOS=read.csv(file = 'resultsbinomial.csv', header = TRUE)[25:48,]
binr=read.csv(file = 'resultsbinomial.csv', header = TRUE)
binr$Test="Negative"
binr$Test[25:48]="Positive"
sum(binrNEG$Area)
negtestA=binom.test(c(20,4), p=0.5, alternative = "g")
negtestH=binom.test(c(18,6), p=0.5, alternative = "g")
postestA=binom.test(c(22,2), p=0.5, alternative = "g")
postestH=binom.test(c(19,5), p=0.5, alternative = "g")
CInega=DescTools::BinomCI(20,24,sides = "l", method = "wilson")
CInegH=DescTools::BinomCI(18,24,sides = "l", method = "wilson")
CIposa=DescTools::BinomCI(22,24,sides = "l", method = "wilson")
CIposH=DescTools::BinomCI(19,24,sides = "l", method = "wilson")


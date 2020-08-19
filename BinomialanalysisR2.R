setwd("C:/Users/felip/Documents/PhD/Douda/experiments joanna/Second approach/Pruebabinomial/5fps")
filestoread=list.files(pattern = ".csv")
filestoread=filestoread[-97]
reslist=lapply(filestoread, read.csv)
names(reslist)=filestoread
for(i in 1:96){reslist[[i]]$action=filestoread[i]}
par(mfrow=c(3,2))
lapply(reslist, function(x) plot(x$Area, type = "l", main = unique(x$action)))
decompose=lapply(reslist, function(x) HoltWinters(ts(x$Area), gamma =  FALSE, beta = FALSE))
lapply(decompose, plot)


library(difR)
library(ltm)

data(verbal)
data<-verbal[colnames(verbal)!="Anger"]
attach(data)

########################
res <- difR::difRaju(data, group = "Gender", focal.name = 1,
                     model = "1PL",p.adjust.method = "BH")
res


#irtParam using ltm----------

## Pre-estimation of the item parameters (1PL model, "ltm" engine)

library(ltm)


nF<-sum(Gender)
nR<-nrow(data)-nF

data.ref<-data[,1:24][order(Gender),][1:nR,]
data.focal<-data[,1:24][order(Gender),][(nR+1):(nR+nF),]


item.1PL<-rbind(itemParEst(data.ref, model = "1PL"),
                itemParEst(data.focal, model = "1PL"))

result<- difR::difRaju(irtParam = item.1PL,same.scale = FALSE)
result

###########################################

r<- itemParEst(data.ref, model='1PL')
r

rtam <-  TAM::tam.mml(resp = as.matrix(data.ref))
rtam$xsi


#using TAM package----------
ref <-  TAM::tam.mml(resp = as.matrix(data.ref))
focal <-  TAM::tam.mml(resp = as.matrix(data.focal))

ref=ref$xsi
focal=focal$xsi

item.1PL<-rbind(ref,focal)

result<- difR::difRaju(irtParam = item.1PL,
                       p.adjust.method = "BH",
                       same.scale = FALSE)
result

plot(result)


## jamovi-----------------

library(dplyr)

ref=dplyr::filter(data, Gender==0)
ref.data=dplyr::select(ref, -Gender)
tam.ref <-  TAM::tam.mml(resp = as.matrix(ref.data))
ref1=tam.ref$xsi
ref1


focal=dplyr::filter(data, Gender==1)
focal.data=dplyr::select(focal, -Gender)
tam.focal<-  TAM::tam.mml(resp = as.matrix(focal.data))

focal1=tam.focal$xsi
focal1

item.1PL<-rbind(ref1,focal1)

result<- difR::difRaju(irtParam = item.1PL,same.scale = FALSE)
result


















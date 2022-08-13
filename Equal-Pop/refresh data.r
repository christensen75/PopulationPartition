
library(geosphere)

setwd("C:/Users/chip.christensen/Desktop/NewWork/Viz/EBC/globes/users/PopulousLocales")
d=read.csv("ergebnis.csv")[,c(4,5,1)]
a=read.csv("latlong.csv")
a$x=paste(a$Latitude,a$Longitude)

d1=d
dv=ceiling(200*d1$Value/max(d1$Value))
dv[dv<5]=5

for(i in 1:nrow(d1)){
a1=a[abs(a$Latitude-d1$Latitude[i])<5 & abs(a$Longitude-d1$Longitude[i])<5,1:2]
if(nrow(a1)>0){
#dist=distHaversine(d1[i,2:1],a1[,2:1])
dist=sqrt((a1$Latitude-d1$Latitude[i])**2+(a1$Longitude-d1$Longitude[i])**2)
dx=head(a1[order(dist),],100)
dxv=head(sort(dist),100)
dx$Value=floor(d1$Value[i]*(max(dxv)-dxv)*.1/max(dxv))
dxv=dxv[dx$Value>0]
dx=dx[dx$Value>0,]
if(nrow(dx)>dv[i]) dx=dx[sample(nrow(dx),dv[i],prob=(max(dxv)-dxv+1)),]
a=a[a$x%in%paste(dx$Latitude,dx$Longitude)==F,]
d=rbind(d,dx)
}}
d=d[is.na(d$Value)==F,]
d=d[order(-d$Value),]
d=d[!duplicated(d[,1:2]),]

h=read.table("htmltemplate.txt",sep="~",stringsAsFactor=F,quote="")[,1]
y1=colorRampPalette(c("white","#ffce00","black"))(50)
y2=colorRampPalette(y1[10:28])(nrow(d))
xxColor=paste(paste0("\"",y2[order(d$Value)],"\""),collapse=",")
xxValue=paste(ceiling(200*d$Value/max(d$Value)),collapse=",")
xxLongitude=paste(d$Longitude,collapse=",")
xxLatitude=paste(d$Latitude,collapse=",")
h=gsub("xxColor",xxColor,h)
h=gsub("xxValue",xxValue,h)
h=gsub("xxLatitude",xxLatitude,h)
h=gsub("xxLongitude",xxLongitude,h)


fileConn=file("index.html")
writeLines(h, fileConn)
close(fileConn)




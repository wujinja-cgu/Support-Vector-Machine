## Histogram
library(pastecs)
setwd("C:/Users/user/Desktop/model selection/644 death prediction model/data")
spb320=read.csv("test 20161004.csv",header=TRUE,sep=",")
attach(spb320)
scores=cbind(age,tmp,pulse,sbp,dbp,rr,spaO2,map,gcs,wbc,band,plt,bilt,aptt,pt,rdw,rbc,hb,mcv,alb,
             bun,inr,ast,mchc,eos,lactate,pct,crp,cre,c3_case,ca_case,uric_case,clchl_case,
             cortisol_case,inorP_case,ddimer_case,proteinc_case,fdp_case,Angiopoetin2,IL6,THF,
             sCD163,IL10,Pentraxin3,CD14,TREM1,CD64,ICAM1,Eselectin,Pselectin,VCAM1)
stat.desc(scores)
options(scipen=100)
options(digits=2)
stat.desc(scores)
windows()
hist(scores[,"wbc"], 
     main="Histogram for wbc", 
     xlab="wbc", 
     border="blue", 
     col="green", 
     xlim=c(0,55), 
     las=1, 
     breaks=40, 
     prob = TRUE)

## lowess plot
library(ggplot2)
setwd("C:/Users/user/Desktop/model selection/644 death prediction model/data")
mydata=read.csv("test 20161004.csv",header=TRUE,sep=",")
contivars=c("age","tmp","pulse","sbp","dbp","rr","spaO2","map","gcs","wbc","band",
            "plt","bilt","aptt","pt","rdw","rbc","hb","mcv","alb","bun","inr","ast",
            "mchc","eos","lactate","pct","crp","cre","fs","sugar","c3_case","ca_case",
            "uric_case","clchl_case","cortisol_case","inorP_case","ddimer_case",
            "proteinc_case","fdp_case")
categoryvars=c("res_infection","GU_infection","skin_infection","abdominal_infection","cns_infection","msk_infection","other_infection")
numericdata=mydata[contivars]
binarydata=mydata[categoryvars]
for(i in 1:ncol(numericdata)){
  numericdata[,i]=ifelse(is.na(numericdata[,i]),mean(numericdata[,i],na.rm=T),numericdata[,i])
}
for(i in 1:ncol(binarydata)){
  binarydata[,i]=ifelse(is.na(binarydata[,i]),0,binarydata[,i])
}
mydata=cbind(death=mydata$death_binary,binarydata,numericdata)
mydata=as.data.frame(mydata)
windows()
ggplot(mydata,aes(x=fdp_case,y=death))+
       geom_point(size=2,alpha=0.4)+
       stat_smooth(method="loess",colour="blue",size=1.5)+
       xlab("fdp_case")+ylab("death")+theme_bw()

### 3D plot
library(plotly)
setwd("C:/Users/user/Desktop/model selection/644 death prediction model/data")
mydata=read.csv("test 20161004.csv",header=TRUE,sep=",")
sapply(mydata,function(x) sum(is.na(x)))

contivars=c("age","tmp","pulse","sbp","dbp","rr","spaO2","map","gcs","wbc","band","plt","bilt","aptt","pt","rdw","rbc","hb","mcv","alb","bun","inr","ast","mchc","eos","lactate","pct","crp","cre","fs","sugar","c3_case","ca_case","uric_case","clchl_case","cortisol_case","inorP_case","ddimer_case","proteinc_case","fdp_case")
categoryvars=c("Gram","E","V","M","res_infection","GU_infection","skin_infection","abdominal_infection","cns_infection","msk_infection","other_infection")
numericdata=mydata[contivars]
binarydata=mydata[categoryvars]
for(i in 1:ncol(numericdata)){
  numericdata[,i]=ifelse(is.na(numericdata[,i]),mean(numericdata[,i],na.rm=T),numericdata[,i])
}
for(i in 1:ncol(binarydata)){
  binarydata[,i]=ifelse(is.na(binarydata[,i]),0,binarydata[,i])
}
mydata=cbind(binarydata,numericdata,death=mydata$death)
mydata=mydata[,c("death","map","cortisol_case","clchl_case","rdw","bun","ast","plt","pt","ddimer_case")]
windows()
x=mydata$cortisol_case
y=mydata$rdw
z=mydata$plt
death=as.factor(mydata$death)
# Plot
library(scatterplot3d)
scatterplot3d(x,y,z,pch=16)
## Change color by death
colors=c("red","#999999")
scatterplot3d(x,y,z,pch=16,color=colors[death],
              grid=TRUE,box=FALSE,xlab="Cortisol", 
              ylab="Rdw",zlab="Plt")

### 3-Dimensional (3-D) Visualization using R
list.of.packages <- c("rgl","ggplot2","knitr","rglwidget")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})
knit_hooks$set(webgl = hook_webgl)
setwd("C:/Users/user/Desktop/model selection/644 death prediction model/data")
mydata=read.csv("test 20161004.csv",header=TRUE,sep=",")
sapply(mydata,function(x) sum(is.na(x)))

contivars=c("age","tmp","pulse","sbp","dbp","rr","spaO2","map","gcs","wbc","band","plt","bilt","aptt","pt","rdw","rbc","hb","mcv","alb","bun","inr","ast","mchc","eos","lactate","pct","crp","cre","fs","sugar","c3_case","ca_case","uric_case","clchl_case","cortisol_case","inorP_case","ddimer_case","proteinc_case","fdp_case")
categoryvars=c("Gram","E","V","M","res_infection","GU_infection","skin_infection","abdominal_infection","cns_infection","msk_infection","other_infection")
numericdata=mydata[contivars]
binarydata=mydata[categoryvars]
for(i in 1:ncol(numericdata)){
                              numericdata[,i]=ifelse(is.na(numericdata[,i]),mean(numericdata[,i],na.rm=T),numericdata[,i])
}
for(i in 1:ncol(binarydata)){
                              binarydata[,i]=ifelse(is.na(binarydata[,i]),0,binarydata[,i])
}
mydata=cbind(binarydata,numericdata,death=mydata$death)
mydata=mydata[,c("death","map","cortisol_case","clchl_case","rdw","bun","ast","plt","pt","ddimer_case")]
str(mydata)
plot3d(mydata$map,mydata$cortisol_case,mydata$rdw,type="p",size=5,lit=FALSE,box=FALSE,col=c("red","blue","green"),expand=1,main="Map Vs Cortisol Vs Rdw",sub="3-D Plot",xlab="Map",ylab="Cortisol",zlab="Rdw")
rgl.bbox(color=c("#333377","black"),emission="#112233",
         specular="#1A2B3C", shininess=5, alpha=0.8, nticks = 3 ) 
aspect3d(1,1,1)



plot_ly(mydata, x = ~rdw, y = ~plt, z = ~ddimer_case, 
             marker = list(color=~death, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
                              add_markers() %>%
                              layout(scene = list(xaxis = list(title = 'rdw'),
                                                  yaxis = list(title = 'plt'),
                                                  zaxis = list(title = 'ddimer')),
                                     annotations = list(
                                                                   
                                     ))
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="scatter3d/colorscale")
chart_link


library(plotly)

mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E')) %>%
                              add_markers() %>%
                              layout(scene = list(xaxis = list(title = 'Weight'),
                                                  yaxis = list(title = 'Gross horsepower'),
                                                  zaxis = list(title = '1/4 mile time')))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="scatter3d/basic")
chart_link


## heatmap of correlation matrix
library(ggplot2)
library(reshape2)
setwd("C:/Users/user/Desktop/model selection/644 death prediction model/data")
mydata=read.csv("test 20161004.csv",header=TRUE,sep=",")
sapply(mydata,function(x) sum(is.na(x)))

contivars=c("age","tmp","pulse","sbp","dbp","rr","spaO2","map","gcs","wbc","band","plt","bilt","aptt","pt","rdw","rbc","hb","mcv","alb","bun","inr","ast","mchc","eos","lactate","pct","crp","cre","fs","sugar","c3_case","ca_case","uric_case","clchl_case","cortisol_case","inorP_case","ddimer_case","proteinc_case","fdp_case")
categoryvars=c("Gram","E","V","M","res_infection","GU_infection","skin_infection","abdominal_infection","cns_infection","msk_infection","other_infection")
numericdata=mydata[contivars]
binarydata=mydata[categoryvars]
for(i in 1:ncol(numericdata)){
                              numericdata[,i]=ifelse(is.na(numericdata[,i]),mean(numericdata[,i],na.rm=T),numericdata[,i])
}
for(i in 1:ncol(binarydata)){
                              binarydata[,i]=ifelse(is.na(binarydata[,i]),0,binarydata[,i])
}
mydata=cbind(binarydata,numericdata,death=mydata$death)
#"map","cortisol_case","clchl_case","rdw","bun","ast","plt","pt","ddimer_case"
mydata=mydata[,c("map","rr","pulse","plt","pt","ddimer_case","crp","pct","c3_case","proteinc_case","alb","lactate","ast","bun","wbc","rbc","rdw","clchl_case","ca_case","cortisol_case","gcs")]
M<-cor(mydata)
library(corrplot)
windows()
corrplot(M, method="circle")
corrplot(M, method="number")
plot(mydata)
windows()
qplot(x=Var1, y=Var2, data=melt(cor(mydata,use="p")), fill=value, geom="tile") +
                              scale_fill_gradient2(limits=c(-1, 1))

library(plotrix)
library(seriation)
library(MASS)
plotcor <- function(r, addtext=TRUE, atcex=NULL, incdiag=FALSE,
                    rorder=TRUE, plot=TRUE, ...) {
                              
                              # round to the nearest hundredth
                              rr <- round(r, 2)
                              
                              dimr <- dim(r)
                              sym <- isSymmetric(r)
                              
                              # get rid of diagonal numbers
                              if (!incdiag & sym) {
                                                            diag(rr) <- NA
                              }
                              
                              rrf <- format(rr)
                              rrf[grep("NA", rrf)] <- ""
                              rra <- abs(rr)
                              nx <- dimr[2]
                              ny <- dimr[1]
                              if (is.null(atcex)) {
                                                            atcex <- 8/max(nx, ny)
                              }
                              namzx <- dimnames(rr)[[2]]
                              namzy <- dimnames(rr)[[1]]
                              
                              # order rows/columns
                              ordx <- 1:nx
                              ordy <- 1:ny
                              if (rorder) {
                                                            # the seriate() function prints out % explained variance for method="PCA"
                                                            # I used capture.output to avoid having this print to the screen
                                                            dummy <- capture.output(ser <- seriate((1-r)/2, method="PCA"))
                                                            ordy <- rev(get_order(ser, 1))
                                                            ordx <- rev(get_order(ser, 2))
                              }
                              if (sym) {
                                                            ordx <- rev(ordy)
                              }
                              
                              if (plot) {
                                                            # categorize correlations from -1 to 1 by 0.01
                                                            brks <- seq(-1, 1, 0.01)
                                                            rcat <- apply(rr, 2, cut, breaks=brks, labels=FALSE)
                                                            
                                                            # assign colors on the cyan-magenta scale
                                                            colz <- apply(rcat, 2, function(x) cm.colors(length(brks))[x])
                                                            par(xaxs="i", yaxs="i", mar=c(0.1, 7, 7, 0.1), ...)
                                                            eqscplot(1, 1, type="n", xlim=c(0.5, nx+0.5), ylim=c(0.5, ny+0.5),
                                                                     xlab="", ylab="", axes=FALSE)
                                                            for(i in 1:nx) {
                                                                                          for(j in 1:ny) {
                                                                                                                        io <- ordx[i]
                                                                                                                        jo <- ordy[j]
                                                                                                                        draw.ellipse(i, j, a=0.5, b=0.5, col=colz[jo, io], border=NA)
                                                                                                                        draw.ellipse(i, j, a=0.5, b=(1-rr[jo, io]^2)/2,
                                                                                                                                     angle=45*c(-1, 1)[1+(rr[jo, io]>0)], col="white", border=NA)
                                                                                                                        if (addtext & !is.na(rra[jo, io])) {
                                                                                                                                                      text(i, j, rrf[jo, io], cex=atcex, col=rgb(0, 0, 0, alpha=rra[jo, io]))
                                                                                                                        }
                                                                                          }}
                                                            axis(3, at=1:nx, labels=namzx[ordx], las=2, tick=FALSE)
                                                            axis(2, at=1:ny, labels=namzy[ordy], las=2, tick=FALSE)
                              }
                              
                              list(rev(ordy), ordx)
}
windows()
plotcor(cor(mydata), mar=c(0.1, 4, 4, 0.1))


### 3D plot for fever up to 5 days
library(plotly)
setwd("C:/Users/user/Desktop/model selection/644 death prediction model/data")
mydata=read.csv("fever 5 up.csv",header=TRUE,sep=",")
sapply(mydata,function(x) sum(is.na(x)))

contivars=c("age","tmp","pulse","sbp","dbp","rr","spaO2","map","gcs","wbc","band","plt","bilt","aptt","pt","rdw","rbc","hb","mcv","alb","bun","inr","ast","mchc","eos","lactate","pct","crp","cre","fs","sugar","c3_case","ca_case","uric_case","clchl_case","cortisol_case","inorP_case","ddimer_case","proteinc_case","fdp_case")
categoryvars=c("Gram","E","V","M","res_infection","GU_infection","skin_infection","abdominal_infection","cns_infection","msk_infection","other_infection")
numericdata=mydata[contivars]
binarydata=mydata[categoryvars]
for(i in 1:ncol(numericdata)){
  numericdata[,i]=ifelse(is.na(numericdata[,i]),mean(numericdata[,i],na.rm=T),numericdata[,i])
}
for(i in 1:ncol(binarydata)){
  binarydata[,i]=ifelse(is.na(binarydata[,i]),0,binarydata[,i])
}
mydata=cbind(binarydata,numericdata,death=mydata$death)
mydata=mydata[,c("death","age","map","plt","rdw","rbc","alb","bun","lactate","c3_case")]
windows()
x=mydata$age
y=mydata$map
z=mydata$rdw
death=as.factor(mydata$death)
# Plot
library(scatterplot3d)
scatterplot3d(x,y,z,pch=16)
## Change color by death
colors=c("red","#999999")
scatterplot3d(x,y,z,pch=16,color=colors[death],
              grid=TRUE,box=FALSE,xlab="age", 
              ylab="map",zlab="rdw")

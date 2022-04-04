library(quantmod)
library(ggplot2)

#CALCOLO I LOG RETURNS DI OGNI TITOLO DI PORTFOLIO
NESN=diff(log(FOGLIO.COMPLETO[,1]))*100
ASML=diff(log(FOGLIO.COMPLETO[,2]))*100
ROG=diff(log(FOGLIO.COMPLETO[,3]))*100
NOVN=diff(log(FOGLIO.COMPLETO[,4]))*100
MC=diff(log(FOGLIO.COMPLETO[,5]))*100
ULVR=diff(log(FOGLIO.COMPLETO[,6]))*100
SAP=diff(log(FOGLIO.COMPLETO[,7]))*100
AZN=diff(log(FOGLIO.COMPLETO[,8]))*100
LIN=diff(log(FOGLIO.COMPLETO[,9]))*100
HSBA=diff(log(FOGLIO.COMPLETO[,10]))*100
NOVOB=diff(log(FOGLIO.COMPLETO[,11]))*100
FP=diff(log(FOGLIO.COMPLETO[,12]))*100
SIE=diff(log(FOGLIO.COMPLETO[,13]))*100
SAN=diff(log(FOGLIO.COMPLETO[,14]))*100
ALV=diff(log(FOGLIO.COMPLETO[,15]))*100
DGE=diff(log(FOGLIO.COMPLETO[,16]))*100
RIO=diff(log(FOGLIO.COMPLETO[,17]))*100
OR=diff(log(FOGLIO.COMPLETO[,18]))*100
SU=diff(log(FOGLIO.COMPLETO[,19]))*100
GSK=diff(log(FOGLIO.COMPLETO[,20]))*100
RDSA=diff(log(FOGLIO.COMPLETO[,21]))*100
BATS=diff(log(FOGLIO.COMPLETO[,22]))*100
BP=diff(log(FOGLIO.COMPLETO[,23]))*100
BAS=diff(log(FOGLIO.COMPLETO[,24]))*100
ENEL=diff(log(FOGLIO.COMPLETO[,25]))*100
AI=diff(log(FOGLIO.COMPLETO[,26]))*100
ADYEN=diff(log(FOGLIO.COMPLETO[,27]))*100
IBE=diff(log(FOGLIO.COMPLETO[,28]))*100
BNP=diff(log(FOGLIO.COMPLETO[,29]))*100
DAI=diff(log(FOGLIO.COMPLETO[,30]))*100
AIR=diff(log(FOGLIO.COMPLETO[,31]))*100
ADS=diff(log(FOGLIO.COMPLETO[,32]))*100
BHP=diff(log(FOGLIO.COMPLETO[,33]))*100
BAYN=diff(log(FOGLIO.COMPLETO[,34]))*100
ZURN=diff(log(FOGLIO.COMPLETO[,35]))*100
RB=diff(log(FOGLIO.COMPLETO[,36]))*100
DG=diff(log(FOGLIO.COMPLETO[,37]))*100
DTE=diff(log(FOGLIO.COMPLETO[,38]))*100
UBSG=diff(log(FOGLIO.COMPLETO[,39]))*100
PRX=diff(log(FOGLIO.COMPLETO[,40]))*100
ABI=diff(log(FOGLIO.COMPLETO[,41]))*100
ABBN=diff(log(FOGLIO.COMPLETO[,42]))*100
PRU=diff(log(FOGLIO.COMPLETO[,43]))*100
CS=diff(log(FOGLIO.COMPLETO[,44]))*100
VOD=diff(log(FOGLIO.COMPLETO[,45]))*100
SAF=diff(log(FOGLIO.COMPLETO[,46]))*100
REL=diff(log(FOGLIO.COMPLETO[,47]))*100
KEL=diff(log(FOGLIO.COMPLETO[,48]))*100
ISP=diff(log(FOGLIO.COMPLETO[,49]))*100
NG=diff(log(FOGLIO.COMPLETO[,50]))*100
#A QUESTO PUNTO HO I RENDIMENTI LOG DI OGNI TITOLO, DEVO SOLO CREARE LA MATRICE DATI DEL PORTFOLIO
PORTFOLIO.RETURNS=data.frame(nesn=NESN,asml=ASML,rog=ROG,novn=NOVN,mc=MC,ulvr=ULVR,sap=SAP,azn=AZN,lin=LIN,hsba=HSBA,novob=NOVOB,fp=FP,sie=SIE,san=SAN,alv=ALV,dge=DGE,rio=RIO,or=OR,su=SU,gsk=GSK,rdsa=RDSA,bats=BATS,bp=BP,bas=BAS,enel=ENEL,ai=AI,adyen=ADYEN,ibe=IBE,bnp=BNP,dai=DAI,air=AIR,ads=ADS,bhp=BHP,bayn=BAYN,zurn=ZURN,rb=RB,dg=DG,dte=DTE,ubsg=UBSG,prx=PRX,abi=ABI,abbn=ABBN,pru=PRU,cs=CS,vod=VOD,saf=SAF,rel=REL,kel=KEL,isp=ISP,ng=NG)
#qquesto è il nostro portafogli e al contempo la data matrix su cui effettuare la PCA

summary(PORTFOLIO.RETURNS)
covariation=cov(PORTFOLIO.RETURNS)
correlation=cor(PORTFOLIO.RETURNS)
PCA=princomp (PORTFOLIO.RETURNS, cor=TRUE)
names(PCA)
PCA$center
PCA$sdev
PCA$loadings
PCA$scale
PCA$n.obs
PCA$scores
PCA$call
summary(PCA)
#tra il PC8 e il PC 15


pr.sd = PCA$sdev^2
pr.sd
pve=pr.sd/sum(pr.sd)
pve
# scree plot
plot(PCA,type="lines")
# plot of variance explained
plot(pve,xlab="Principal Component",ylab="Proportion of Variance
Explained",ylim=c(0,1),type='b')
plot(cumsum(pve),xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0.5,1),type='b')
cor(PCA$scores[,1],PORTFOLIO.RETURNS)
cor(PCA$scores[,2],PORTFOLIO.RETURNS)
cor(PCA$scores[,3],PORTFOLIO.RETURNS)
cor(PCA$scores[,4],PORTFOLIO.RETURNS)
cor(PCA$scores[,5],PORTFOLIO.RETURNS)
cor(PCA$scores[,6],PORTFOLIO.RETURNS)
cor(PCA$scores[,7],PORTFOLIO.RETURNS)
cor(PCA$scores[,8],PORTFOLIO.RETURNS)

#AVENDO SCELTO 2 PC PROCEDO IN TAL SENSO per la costruzione del plot, anche se includero in esso tutte e 6 le PC
loadvec = as.vector(t(PCA$loadings[,1:8]))
var = rep(1:50,each=8)
comp = rep(1:8,50)
plot.data = data.frame(loadings =loadvec,var =var,PC = factor(comp))

p = ggplot(data =plot.data, aes(x=var,y=loadings,col=PC))
p =p +geom_line ()
p =p +scale_x_discrete (breaks =1 :50,labels= colnames(PORTFOLIO.RETURNS),
                        limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50")) +
  theme(axis.text.x = element_text(angle=45)) +
  labs(x ="Variables",y ="Loadings") +
  ggtitle("Component Pattern Profiles") +
  theme(plot.title = element_text(hjust =0.5))

library (devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(PCA)


library(quantmod)
getSymbols("BBCA.JK", from = "2020-04-01", to = "2022-04-01", src = "yahoo", adjust = TRUE)
getSymbols("PTBA.JK", from = "2010-01-01", to = "2020-01-01", src = "yahoo", adjust = TRUE)
getSymbols("KLBF.JK", from = "2010-01-01", to = "2020-01-01", src = "yahoo", adjust = TRUE)
getSymbols("EXCL.JK", from = "2010-01-01", to = "2020-01-01", src = "yahoo", adjust = TRUE)

closeBBCA <- as.numeric(BBCA.JK$BBCA.JK.Close)
closePTBA <- as.numeric(PTBA.JK$PTBA.JK.Close)
closeKLBF <- as.numeric(KLBF.JK$KLBF.JK.Close)
closeEXCL <- as.numeric(EXCL.JK$EXCL.JK.Close)

#ret = log(x_t / x_{t-1}) = log(x_t) - log(x_{t-1})
#mencari return saham 
BBCA.rt = diff(log(closeBBCA))
PTBA.rt = diff(log(closePTBA))
KLBF.rt = diff(log(closeKLBF))
EXCL.rt = diff(log(closeEXCL))

#Q-Q plot
qqnorm(BBCA.rt, main = "Normal Q-Q Plot of BBCA Return")
qqline(BBCA.rt)
qqnorm(PTBA.rt, main = "Normal Q-Q Plot of PTBA Return")
qqline(PTBA.rt)
qqnorm(KLBF.rt, main = "Normal Q-Q Plot of KLBF Return")
qqline(KLBF.rt)
qqnorm(EXCL.rt, main = "Normal Q-Q Plot of EXCL Return")
qqline(EXCL.rt)

library("fBasics")
StDf.BBCA <- basicStats(BBCA.rt);StDf.BBCA
StDf.PTBA <-basicStats(PTBA.rt);StDf.PTBA
StDf.KLBF <- basicStats(KLBF.rt);StDf.KLBF
StDf.EXCL <- basicStats(EXCL.rt);StDf.EXCL
summary(BBCA.rt)
summary(PTBA.rt)
summary(KLBF.rt)
summary(EXCL.rt)

skew.BBCA.rt<-basicStats(BBCA.rt)[15,]
T1<-length(BBCA.rt)
thitung1<-skew.BBCA.rt/sqrt(6/T1) # Skewness test
pvalue1<-2*(1-pnorm(thitung1)) #Hitung p-value
pvalue1 #Tolak H0 hipotesis simetris jika pvalue < alpha

skew.PTBA.rt<-basicStats(PTBA.rt)[15,]
T2<-length(PTBA.rt)
thitung2<-skew.PTBA.rt/sqrt(6/T2) # Skewness test
pvalue2<-2*(1-pnorm(thitung2)) #Hitung p-value
pvalue2 #Tolak H0 hipotesis simetris jika pvalue < alpha

skew.KLBF.rt<-basicStats(KLBF.rt)[15,]
T3<-length(KLBF.rt)
thitung3<-skew.KLBF.rt/sqrt(6/T3) # Skewness test
pvalue3<-2*(1-pnorm(thitung3)) #Hitung p-value
pvalue3 #Tolak H0 hipotesis simetris jika pvalue < alpha

skew.EXCL.rt<-basicStats(EXCL.rt)[15,]
T4<-length(EXCL.rt)
thitung4<-skew.EXCL.rt/sqrt(6/T4) # Skewness test
pvalue4<-2*(1-pnorm(thitung4)) #Hitung p-value
pvalue4 #Tolak H0 hipotesis simetris jika pvalue < alpha

#Mixture 
library(mclust)
#Equal
x<-BFIN.rt #repeat for other stocks
mod.BFIN<-densityMclust(x,G=2,modelNames="E") #equal variance
mix.BFIN = summary(mod.BFIN,parameters=T) ; mix.BFIN

x2<-INCO.rt #repeat for other stocks
mod.INCO<-densityMclust(x2,G=2,modelNames="E") #equal variance
mix.INCO = summary(mod.INCO,parameters=T) ; mix.INCO

x3<-MEDC.rt #repeat for other stocks
mod.MEDC<-densityMclust(x3,G=2,modelNames="E") #equal variance
mix.MEDC = summary(mod.MEDC,parameters=T) ; mix.MEDC

x4<-TBIG.rt #repeat for other stocks
mod.TBIG<-densityMclust(x4,G=2,modelNames="E") #equal variance
mix.TBIG = summary(mod.TBIG,parameters=T) ; mix.TBIG

#Diff
x<-BFIN.rt #repeat for other stocks
mod.BFIN.v<-densityMclust(x,G=2,modelNames="V") #diff variance
mix.BFIN.v = summary(mod.BFIN.v,parameters=T) ; mix.BFIN.v

x2<-INCO.rt #repeat for other stocks
mod.INCO.v<-densityMclust(x2,G=2,modelNames="V") #diff variance
mix.INCO.v = summary(mod.INCO.v,parameters=T) ; mix.INCO.v

x3<-MEDC.rt #repeat for other stocks
mod.MEDC.v<-densityMclust(x3,G=2,modelNames="V") #diff variance
mix.MEDC.v = summary(mod.MEDC.v,parameters=T) ; mix.MEDC.v

x4<-TBIG.rt #repeat for other stocks
mod.TBIG.v<-densityMclust(x4,G=2,modelNames="V") #diff variance
mix.TBIG.v = summary(mod.TBIG.v,parameters=T) ; mix.TBIG.v

#return = sum(bobot*mean)
BFIN.r=(mix.BFIN$pro[1]*mix.BFIN$mean[1]) + 
  (mix.BFIN$pro[2]*mix.BFIN$mean[2]);BFIN.r

INCO.r=(mix.INCO$pro[1]*mix.INCO$mean[1]) + 
  (mix.INCO$pro[2]*mix.INCO$mean[2]);INCO.r

MEDC.r=(mix.MEDC$pro[1]*mix.MEDC$mean[1]) + 
  (mix.MEDC$pro[2]*mix.MEDC$mean[2]);MEDC.r

TBIG.r=(mix.TBIG$pro[1]*mix.TBIG$mean[1]) + 
  (mix.TBIG$pro[2]*mix.TBIG$mean[2]);TBIG.r

ret.same.var= p5.w1*BFIN.r + p5.w2*INCO.r + p5.w3*MEDC.r + 
  p5.w4*TBIG.r ;ret.same.var

#risiko^2 = sum(bobot*(var+mean^2))-return^2
E.kuad.BFIN1 = mix.BFIN$variance[1]+(mix.BFIN$mean[1]^2)
E.kuad.BFIN2 = mix.BFIN$variance[1]+(mix.BFIN$mean[2]^2)
same.f1 = (mix.BFIN$pro[1]*E.kuad.BFIN1) + 
  (mix.BFIN$pro[2]*E.kuad.BFIN2)
ris.BFIN= same.f1-(BFIN.r^2) ;ris.BFIN

E.kuad.INCO1 = mix.INCO$variance[1]+(mix.INCO$mean[1]^2)
E.kuad.INCO2 = mix.INCO$variance[1]+(mix.INCO$mean[2]^2)
same.f2 = (mix.INCO$pro[1]*E.kuad.INCO1) + 
  (mix.INCO$pro[2]*E.kuad.INCO2)
ris.INCO = same.f2-(INCO.r^2) ;ris.INCO

E.kuad.MEDC1 = mix.MEDC$variance[1]+(mix.MEDC$mean[1]^2)
E.kuad.MEDC2 = mix.MEDC$variance[1]+(mix.MEDC$mean[2]^2)
same.f3 = (mix.MEDC$pro[1]*E.kuad.MEDC1) + 
  (mix.MEDC$pro[2]*E.kuad.MEDC2)
ris.MEDC= same.f3-(MEDC.r^2) ;ris.MEDC

E.kuad.TBIG1 = mix.TBIG$variance[1]+(mix.TBIG$mean[1]^2)
E.kuad.TBIG2 = mix.TBIG$variance[1]+(mix.TBIG$mean[2]^2)
same.f4= (mix.TBIG$pro[1]*E.kuad.TBIG1) + 
  (mix.TBIG$pro[2]*E.kuad.TBIG2)
ris.TBIG= same.f4-(TBIG.r^2) ;ris.TBIG

sig.same.var= p5.w1*ris.BFIN+p5.w2*ris.INCO+p5.w3*ris.MEDC+p5.w4*ris.TBIG ;sig.same.var^0.5

#different variance
#return = sum(bobot*mean)
BFIN.rv=(mix.BFIN.v$pro[1]*mix.BFIN.v$mean[1]) + 
  (mix.BFIN.v$pro[2]*mix.BFIN.v$mean[2])
INCO.rv=(mix.INCO.v$pro[1]*mix.INCO.v$mean[1]) + 
  (mix.INCO.v$pro[2]*mix.INCO.v$mean[2])
MEDC.rv=(mix.MEDC.v$pro[1]*mix.MEDC.v$mean[1]) + 
  (mix.MEDC.v$pro[2]*mix.MEDC.v$mean[2])
TBIG.rv=(mix.TBIG.v$pro[1]*mix.TBIG.v$mean[1]) + 
  (mix.TBIG.v$pro[2]*mix.TBIG.v$mean[2])
ret.diff.var= p5.w1*BFIN.rv + p5.w2*INCO.rv + p5.w3*MEDC.rv + p5.w4*TBIG.r ;ret.diff.var

#risiko^2 = sum(bobot*(var+mean^2))-return^2
E.kuad.BFIN1.v = mix.BFIN.v$variance[1]+(mix.BFIN.v$mean[1]^2)
E.kuad.BFIN2.v = mix.BFIN.v$variance[2]+(mix.BFIN.v$mean[2]^2)
diff.f1 = (mix.BFIN.v$pro[1]*E.kuad.BFIN1.v) + (mix.BFIN.v$pro[2]*E.kuad.BFIN2.v)
ris.BFIN.v= diff.f1-(BFIN.rv^2) ;ris.BFIN.v

E.kuad.INCO1.v = mix.INCO.v$variance[1]+(mix.INCO.v$mean[1]^2)
E.kuad.INCO2.v = mix.INCO.v$variance[2]+(mix.INCO.v$mean[2]^2)
diff.f2 = (mix.INCO.v$pro[1]*E.kuad.INCO1.v) + (mix.INCO.v$pro[2]*E.kuad.INCO2.v)
ris.INCO.v= diff.f2-(INCO.rv^2) ;ris.INCO.v

E.kuad.MEDC1.v = mix.MEDC.v$variance[1]+(mix.MEDC.v$mean[1]^2)
E.kuad.MEDC2.v = mix.MEDC.v$variance[2]+(mix.MEDC.v$mean[2]^2)
diff.f3 = (mix.MEDC.v$pro[1]*E.kuad.MEDC1.v) + (mix.MEDC.v$pro[2]*E.kuad.MEDC2.v)
ris.MEDC.v= diff.f3-(MEDC.rv^2) ;ris.MEDC.v

E.kuad.TBIG1.v = mix.TBIG.v$variance[1]+(mix.TBIG.v$mean[1]^2)
E.kuad.TBIG2.v = mix.TBIG.v$variance[2]+(mix.TBIG.v$mean[2]^2)
diff.f4= (mix.TBIG.v$pro[1]*E.kuad.TBIG1.v) + (mix.TBIG.v$pro[2]*E.kuad.TBIG2.v)
ris.TBIG.v= diff.f4-(TBIG.rv^2) ;ris.TBIG.v

sig.diff.var= p5.w1*ris.BFIN.v+p5.w2*ris.INCO.v+p5.w3*ris.MEDC.v+p5.w4*ris.TBIG.v ;sig.diff.var^0.5

#P5
#Plot mixture
ret.aset<-BFIN.rt 
mix = normalmixEM(ret.aset,k=2)
x <- mix$x
x <- data.frame(x)
ggplot2::ggplot(data.frame(x)) +
  ggplot2::geom_density(ggplot2::aes(x), color="grey", 
                        fill="grey") +
  ggplot2::stat_function(geom = "point", fun = 
                           plot_mix_comps,
                         args = list(mix$mu[1], 
                                     mix$sigma[1], lam =
                                       mix$lambda[1]),
                         colour = "black") +
  ggplot2::stat_function(geom = "line", fun = 
                           plot_mix_comps,
                         args = list(mix$mu[2], 
                                     mix$sigma[2], lam =
                                       mix$lambda[2]),
                         colour = "black") +
  theme(panel.background= element_rect(fill = 
                                         'white'),panel.grid =
          element_line(color = 'black'))+
  ggtitle('Mixture Assets BFIN')

ret.aset<-INCO.rt 
mix = normalmixEM(ret.aset,k=2)
x <- mix$x
x <- data.frame(x)

ggplot2::ggplot(data.frame(x)) +
  ggplot2::geom_density(ggplot2::aes(x), color="grey", 
                        fill="grey") +
  ggplot2::stat_function(geom = "point", fun = 
                           plot_mix_comps,
                         args = list(mix$mu[1], 
                                     mix$sigma[1], lam =
                                       mix$lambda[1]),
                         colour = "black") +
  ggplot2::stat_function(geom = "line", fun = 
                           plot_mix_comps,
                         args = list(mix$mu[2], 
                                     mix$sigma[2], lam =
                                       mix$lambda[2]),
                         colour = "black") +
  theme(panel.background= element_rect(fill = 
                                         'white'),panel.grid =
          element_line(color = 'black'))+
  ggtitle('Mixture Assets INCO')

ret.aset<-MEDC.rt 
mix = normalmixEM(ret.aset,k=2)
x <- mix$x
x <- data.frame(x)
ggplot2::ggplot(data.frame(x)) +
  ggplot2::geom_density(ggplot2::aes(x), color="grey", 
                        fill="grey") +
  ggplot2::stat_function(geom = "point", fun = 
                           plot_mix_comps,
                         args = list(mix$mu[1], 
                                     mix$sigma[1], lam =
                                       mix$lambda[1]),
                         colour = "black") +
  ggplot2::stat_function(geom = "line", fun = 
                           plot_mix_comps,
                         args = list(mix$mu[2], 
                                     mix$sigma[2], lam =
                                       mix$lambda[2]),
                         colour = "black") +
  theme(panel.background= element_rect(fill = 
                                         'white'),panel.grid =
          element_line(color = 'black'))+
  ggtitle('Mixture Assets MEDC')

ret.aset<-TBIG.rt 
mix = normalmixEM(ret.aset,k=2)
x <- mix$x
x <- data.frame(x)
ggplot2::ggplot(data.frame(x)) +
  ggplot2::geom_density(ggplot2::aes(x), color="grey", 
                        fill="grey") +
  ggplot2::stat_function(geom = "point", fun = 
                           plot_mix_comps,
                         args = list(mix$mu[1], 
                                     mix$sigma[1], lam =
                                       mix$lambda[1]),
                         colour = "black") +
  ggplot2::stat_function(geom = "line", fun = 
                           plot_mix_comps,
                         args = list(mix$mu[2], 
                                     mix$sigma[2], lam =
                                       mix$lambda[2]),
                         colour = "black") +
  theme(panel.background= element_rect(fill = 
                                         'white'),panel.grid =
          element_line(color = 'black'))+
  ggtitle('Mixture Assets TBIG')

#PLOT Portofolio MV
a = seq(from=1, to=-200, by=-0.075)
n.a = length(a)
z.mat = matrix(0, n.a, 4)
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m.vec)%*%sigma.mat%*%x.vec
for (i in 1:n.a) {
  z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
  mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
  sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px + 
    2*a[i]*(1-a[i])*sig.mx}
plot(sqrt(sig2.z), mu.z, type="b", ylim=c(0, 0.008), 
     xlim=c(0.01, 0.075),
     pch=16, col="red", cex = cex.val, 
     ylab=expression(mu[p]), xlab=expression(sigma[p]))
points(sd.vec, mu.vec, pch=16, cex=1.5, col="blue")
for (i in 1:length(x.BFIN)) {
  z.vec = c(x.BFIN[i], x.INCO[i], x.MEDC[i],x.TBIG[i])
  mu.p = crossprod(z.vec,mu.vec)
  sig.p = sqrt(t(z.vec)%*%sigma.mat%*%z.vec)
  points(sig.p, mu.p, pch=16, col="black", cex=0.75)
  }
points(sig.gmin, mu.gmin, pch=16, cex=1.15, col="Green")
points(sig.px,mu.px,pch=16,cex=1.15, col="yellow")
text(sig.px,mu.px,labels = "MV",pos = 3,cex = 0.5)
text(sig.gmin, mu.gmin, labels="GLOBAL MIN", pos=2, col="black", cex = 0.5)
text(sd.vec, mu.vec, labels=asset.names, pos=3, col="blue", cex = 0.5)

#MENYUSUN PORTOFOLIO OPTIMAL DENGAN MEAN VARIANCE
#matrix
######
p5.miu1=StDf.BFIN[7,]
p5.miu2=StDf.INCO[7,]
p5.miu3=StDf.MEDC[7,]
p5.miu4=StDf.TBIG[7,]
p5.A=matrix(data = c(2*(StDf.BFIN[14,]^2),2*p5.sig12,2*p5.sig13,2*p5.sig14,
                     p5.miu1,1,2*p5.sig12,2*(StDf.INCO[14,]^2),2*p5.sig23,
                     2*p5.sig24,p5.miu2,1,2*p5.sig13,2*p5.sig23,2*(StDf.MEDC[14,]^2),
                     2*p5.sig34,p5.miu3,1,2*p5.sig14,2*p5.sig24,2*p5.sig34,
                     2*(StDf.TBIG[14,]^2),p5.miu4,1,p5.miu1,p5.miu2,p5.miu3,
                     p5.miu4,0,0,1,1,1,1,0,0),6,6)
p5.miup = StDf.BFIN[7,] 
p5.b0 = matrix(c(0,0,0,0,p5.miup,1),6)
p5.weight <- solve(p5.A)%*%p5.b0 ;p5.weight
#Var 
ret.same.var+(qnorm(0.05)*sig.same.var)
ret.diff.var+(qnorm(0.05)*sig.diff.var)

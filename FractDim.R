FractDim<-function(Data,graphon=FALSE) {
  X=Data;N=length(X);
  jstart=10;jend=floor(10*(log10(N)-1));
  kvec=c(1:4,floor(2^(c(jstart:jend)/4)));
  indkend=length(kvec);
  k=c()
  AvgLmk=c()
  err=c()
  for(indk in 1:indkend)
  {
    k=kvec[indk]
    Xend=c()
    Xsum=c()
    Lmk=c()
    for(m in 1:k)
    {
      Xend=floor((N-m)/k)
      Xsum=sum(abs(X[m+c(1:Xend)*k]-c(0, X[m+c(1:(Xend-1))*k])))
      Lmk[m]=1/k*1/k*(N-1)/Xend*Xsum
    }
    AvgLmk[indk]=mean(Lmk)
    #  err[indk]=sd(log(Lmk))
  }
  x<-log(kvec);y<-log(AvgLmk);
  q<-lm(y~x);slope<-summary(q)$coefficients[2,1];
  yintcept<-summary(q)$coefficients[1,1];q<-NULL;
  yfit<-x*slope+yintcept
  FrDim=-slope
  if(graphon==TRUE)
  {
    plot(x,y,main="If linear then fractal, w/Fr. Dim = (-)slope",xlab="Ln(k)",ylab="Ln(length of curve with interval k)")
    z<-line(x,yfit);abline(coef(z),col='blue');z<-NULL
    #z<-line(x,y);abline(coef(z),col='blue');z<-NULL
  }
  #z<-line(x,y);qq=coef(z)
  #yintcept=qq[1]
  #FrDim=-qq[2]
  return(c(FrDim,yintcept))
}

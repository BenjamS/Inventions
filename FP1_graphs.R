library(ggplot2)
library(ggthemes)
library(pracma)

x <- c(-5, 6.2)
mu2 <- 0.6
sd2 <- 2


xseq <- seq(x[1], x[2], 0.1)
ydiff <- dnorm(xseq, 0, 1) - dnorm(xseq, mu2, sd2)
y_PDFint <- min(abs(ydiff))
indPDFint <- which(abs(ydiff) == y_PDFint)
y_adopt <- pnorm(xseq, 0, 1)[indPDFint]
x_adopt <- xseq[indPDFint]


gg <- ggplot(data = data.frame(x), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), geom = "area", fill = "green", alpha = 0.3) +
  scale_y_continuous(breaks = NULL)
gg <- gg + stat_function(fun = dnorm, n = 101, args = list(mean = mu2, sd = sd2), geom = "area", fill = "violet", alpha = 0.3) + ylab("")
# gg <- gg + geom_vline(xintercept = 0, linetype = "dashed", colour = "red")
# gg <- gg + geom_vline(xintercept = mu2, linetype = "dashed", colour = "brown")
gg <- gg + geom_vline(xintercept = x_adopt, colour = "red", size = 2)
gg <- gg + theme_wsj()
gg <- gg + theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())
gg

ydiff <- pnorm(xseq, 0, 1) - pnorm(xseq, mu2, sd2)
y_CDFint <- min(abs(ydiff))
indCDFint <- which(abs(ydiff) == y_CDFint)
y_adopt <- pnorm(xseq, 0, 1)[indCDFint]
x_CDFint <- xseq[indCDFint]

gg <- ggplot(data = data.frame(x), aes(x))
gg <- gg + stat_function(fun = pnorm, n = 101, args = list(mean = 0, sd = 1), geom = "area", fill = "green", alpha = 0.3)
#gg <- gg + scale_y_continuous(breaks = NULL)
gg <- gg + stat_function(fun = pnorm, n = 101, args = list(mean = mu2, sd = sd2), geom = "area", fill = "violet", alpha = 0.3)
# gg <- gg + geom_vline(xintercept = 0, linetype = "dashed", colour = "red")
# gg <- gg + geom_vline(xintercept = mu2, linetype = "dashed", colour = "brown")
gg <- gg + theme_wsj()
gg <- gg + theme(axis.title.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.ticks.x=element_blank())
gg <- gg + geom_hline(yintercept = y_adopt, color = "red", size = 1.5)
gg


#----------------
library(nleqslv)


rootfn <- function(sd2, mu2, xseq, WFtarg){
  ydiff <- pnorm(xseq, 0, 1) - pnorm(xseq, mu2, sd2)
  y_CDFint <- min(abs(ydiff))
  indCDFint <- which(abs(ydiff) == y_CDFint)
  indA <- c(indCDFint:length(xseq))
  xA <- xseq[indA]
  A1 <- trapz(xA, pnorm(xA, 0, 1))
  A2 <- trapz(xA, pnorm(xA, mu2, sd2))
  slack <- A1 - A2 - WFtarg
  return(slack)
} 


xseq <- seq(-5, 6.2, 0.1)
WFGvec <- seq(1, 2, 0.2)
mu2vec <- seq(0.2, 1.6, 0.2)
dflist <- list()
for(i in 1:length(WFGvec)){
  WFtarg <- WFGvec[i]
  check <- c()
  sd_out <- c()
  for(j in 1:length(mu2vec)){
    in_mu2 <- mu2vec[j]
    in_sd2 <- runif(1)
    #out <- uniroot(rootfn, )
    out <- uniroot(rootfn, c(0, 50), tol = 10^-8, in_mu2, xseq, WFtarg)
    #out <- nleqslv(in_sd2, rootfn, jac = NULL, in_mu2, xseq, WFtarg, method = "Broyden")
    sd_out[j] <- out$root
    check[j] <- out$f.root
  }
  dflist[[i]] <- data.frame(WFtarg, mu2 = mu2vec, sd2 = sd_out, f.root = check)
}
df <- do.call(rbind, dflist)
#--------------

gg <- ggplot(df, aes(x = ))













df <- round(df, 3)


for(i in 1:nrow(df)){
  str(rootfn(df$sd2[i], df$mu2[i], xseq, df$WFtarg[i]))
  
}








gg <- ggplot(data = data.frame(xseq, ydiff), aes(x = xseq, y = ydiff)) + geom_line()
# gg <- gg + scale_y_continuous(breaks = NULL)
gg <- gg + theme_wsj()
# gg <- gg + theme(axis.title.x=element_blank(),
#                  axis.text.x=element_blank(),
#                  axis.ticks.x=element_blank())
gg




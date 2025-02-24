#init
library(png)
library(mclust)
library(utils)

# First let's generate some 2D xmas samples
st='Hello !! \\ B2SLab'

png("test.png",width=400,height=700)
plot(1,1,col=0,axes=FALSE,xlab='',ylab='')
text(1,1,st,cex=5,font=2,ps=100)
dev.off()


I <- readPNG('test.png')
trues <- which(!I[,,1])
d <- data.frame(y = col(!I[,,1])[trues],
                x = -row(!I[,,1])[trues])
plot(d,pch='.')




library(RColorBrewer)

Ncomp=400
zinit <- unmap(sample(1:Ncomp, nrow(d), replace=T))
ms<-mstep(modelName="EEV",
          data=d,
          z=zinit)
system("rm tmp/*png")
png("tmp/xmas%03d.png")
Maxit <- 300

pb <- txtProgressBar(style = 3,max=Maxit)
for (i in c(1:Maxit)){
    es<-estep(modelName="EEV",
              data=d,
              parameters=ms$parameters,
              prior = priorControl())
    ms<-mstep(modelName="EEV",
              data=d,
              z=es$z,
              prior = priorControl())
    plot.densityMclust(ms,data=d,pch=' ',
                       what="density",
                       col=(brewer.pal(n = 11,
                                       name = "BuPu")),
                       axes=FALSE,
                       xlab='', ylab='',
                       type='image', nlevels=25,
                       transformation="none")
    
    setTxtProgressBar(pb, i)
}
close(bp)

dev.off()

unlink("xmas.mp4")
system("avconv -r 20 -i tmp/xmas%03d.png -b:v 1000k xmas.mp4")
system("mplayer xmas.mp4")
# :) 


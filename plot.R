


#Importing functions from the toolbox
source("toolbox.R")

##
exp<-read.csv2("data.csv")

words<-cbind(words,wx)

names(words)[3]<-"translation"
words$words
words$translation
words


me$econ_pos<-me$CA2-me$CA3-me$CA1
me$cult_pos<-me$CA2+me$CA1+me$CA3
levels(me$party)[7]<-"None"



w3<-as.data.frame(top_words(v1$CA$v,3,100))
w2<-as.data.frame(top_words(v1$CA$v,2 ,100))
w1<-as.data.frame(top_words(v1$CA$v,1 ,100))

w1$cult<-w1[,1]+w1[,2]+w1[,3]
w1$econ<-w1[,2]-w1[,1]-w1[,3]

w2$econ<-w2[,2]-w2[,1]-w2[,3]
w2$cult<-w2[,1]+w2[,2]+w2[,3]

w3$econ<-w3[,2]-w3[,1]-w3[,3]
w3$cult<-w3[,1]+w3[,2]+w3[,3]

w1$term<-rownames(w1)
w2$term<-rownames(w2)
w3$term<-rownames(w3)

words$term<-as.character(words$words)

w3<-merge(w3,words,by="term")
w2<-merge(w2,words,by="term")
w1<-merge(w1,words,by="term")

## change terms 


ccol<-c("dark blue","grey","black","light blue","yellow","green","purple","grey","red")  

ppch<-c("A","C","U","G","L","F","N","S")

pdf('.././plots/dim_de.pdf',width = 12, height = 12)
par(mfrow=c(2,1))
plot(me$cult_pos,me$econ_pos,col=pcols[me$party],ylab="Wirtschaftl. Links-Rechts",xlab="Kulturell Links-Rechts",pch=ppch[me$party])
legend(
  x="bottomright",
  legend=levels(me$party),
  col=pcols2,
  pch=ppch)

plot(me$cult_pos,me$econ_pos,col=pcols[me$party],ylab="Wirtschaftl. Links-Rechts",xlab="Kulturell Links-Rechts",ylim=c(min(w3$econ)-1,max(w1$econ)+1),xlim=c(min(w1$cult)-1,max(w3$cult)+1))
textplot(w1$cult,w1$econ,words =w1$term,cex=0.6,new=F,show.lines=F)
textplot(w2$cult,w2$econ,words=w2$term,cex=0.6,new=F,show.lines=F)
textplot(w3$cult,w3$econ,words=w3$term,cex=0.6,new=F,show.lines=F)
dev.off()


pdf('.././plots/vips.pdf',width = 12, height = 12)
prom<-c("Beatrix von Storch","Alice Weidel"
        ,"Jürgen Trittin"
        ,"Jens Spahn"
        ,"Lars Klingbeil"
        ,"Sahra Wagenknecht"
        ,"Dietmar Bartsch"
        ,"Johannes Kahrs"
        ,"Christian Lindner"
        ,"Verena Hartmann"
        ,"Karl Lauterbach"
        ,"Niels Annen")


plot(me$cult_pos,me$econ_pos,col=pcols[me$party],ylab="Wirtschaftl. Links-Rechts",xlab="Kulturell Links-Rechts",pch=19)
points(ger$galtan-4.5,ger$lrecon-4.5,col=ccol[ger$party],pch=19,cex=2.3)
legend(
  x="bottomright",
  legend=levels(me$party),
  col=pcols2,
  pch=ppch)

for(i in 1:length(prom)){
  t<-me[me$fullname==prom[i],]
  text(t$cult_pos,t$econ_pos,labels=t$fullname,col=pcols2[t$party])
  }

dev.off()


# set 2





#Importing functions from the toolbox
source("toolbox.R")


me$econ_pos<-me$CA2-me$CA3-me$CA1
me$cult_pos<-me$CA2+me$CA1+me$CA3
levels(me$party)[7]<-"None"

##
me<-read.csv2("data.csv")

ppch<-c("A","C","U","G","L","F","N","S")

pcols<-c('dark blue','black','light blue','green','purple','gold1','grey','red')


pdf('vips.pdf',width = 12, height = 12)
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


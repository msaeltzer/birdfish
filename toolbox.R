
if(!require("wordcloud")){install.packages("wordcloud")}
if(!require("wordcloud")){install.packages("ggplot2")}
if(!require("wordcloud")){install.packages("tidyverse")}
if(!require("wordcloud")){install.packages("quanteda")}

library(wordcloud)
library(quanteda)
library(ggplot2)



mult_plot<-function(allModelFrame,level_order=level_order,legend=F,sz=17,l=1){
  zp1 <- ggplot(allModelFrame, aes(colour = Model))
  zp1 <- zp1 + scale_color_manual(values=colscheme)
  zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2*l)
  zp1 <- zp1 + geom_linerange(aes(x = factor(Variable,levels=level_order), ymin = Coefficient - SE*interval1,
                                  ymax = Coefficient + SE*interval1),
                              lwd = 1*l, position = position_dodge(width = 1/2))
  zp1 <- zp1 + geom_pointrange(aes(x = factor(Variable,levels=level_order), y = Coefficient, ymin = Coefficient - SE*interval2,
                                   ymax = Coefficient + SE*interval2),
                               lwd = 1/2*l, position = position_dodge(width = 1/2),
                               shape = 19, fill = "WHITE")
  zp1 <- zp1 + theme_minimal()
  zp1 <- zp1 + coord_flip() +   theme(axis.text = element_text(size=sz,family='sans')
                                      ,axis.title = element_blank()
                                      ,axis.line.y = element_blank()
                                      ,axis.ticks = element_blank()
                                      ,legend.title = element_text(size=sz)
                                      ,legend.text = element_text(size=sz*0.75)
                                      ,panel.grid.major.y = element_blank()
                                      ,panel.grid.minor.y = element_blank()
                                      ,legend.position='bottom')
  if(legend==F){zp1<-zp1+theme(legend.position='none')}
  return(zp1)
}



stripURL2 = function(x) {
  gsub("www[^[:space:]]+|htt[^[:space:]]+|//t\\.[^[:space:]]+", " ", x)
}

kill_html<-function(x){
  x<-gsub('(<[^>]*>)','',x)
}

kill_html2<-function(x){
  x<-gsub('&.*;','',x)
}

ger_num<-function(x){
  x<-gsub('[0-9],[0-9]','',x)
}

map_plot_j<-function(x,d1,d2,meta=me,s=0.7,number=100,x_lab="Dimension 1",y_lab="Dimension 2",spread=1
                     ,t=T
                     ,party=F
                     ,leg=NULL
                     ,persons=NULL){
  w<-x
  x<-x$CA$u
  x<-x[order(rownames(x)),]
  meta<-meta[order(meta$screen_name),]
  data<-meta
  pcols2<-c('dark blue','black','light blue','green','purple','gold1','grey','red')
  w1<-as.data.frame(top_words(w$CA$v,d1,number))
  w2<-as.data.frame(top_words(w$CA$v,d2,number))
  w1$terms<-rownames(w1)
  w2$terms<-rownames(w2)
  
  ppch<-1
  if(t==T){
    plot(x[,d1],x[,d2]
         ,col=pcols2[as.factor(data$party)]
         ,cex=0.7
         ,ylim=c(min(w2[,d2])*sqrt(spread),max(w2[,d2]))*sqrt(spread)
         ,xlim=c(min(w1[,d1])*sqrt(spread),max(w1[,d1]))*sqrt(spread)
         ,ylab=y_lab
         ,xlab=x_lab
    )
    text(w1[,d1]*spread,w1[,d2]*spread,labels=w1$terms,cex=s,col="grey")
    text(w2[,d1]*spread,w2[,d2]*spread,labels=w2$terms,cex=s,col="grey")
  }else{plot(x[,d1],x[,d2]
             ,col=pcols2[as.factor(data$party)]
             ,pch=1
             ,cex=1
             ,ylab=y_lab
             ,xlab=x_lab)}
  
  if(is.null(leg)==F){
    legend(
      x=leg,
      legend=levels(data$party),
      col=pcols2,
      pch=ppch)
  }
  
  if(is.null(persons)==F){
    for(i in 1:length(persons)){mp<-persons[i]
    if(mp=="Verena Hartmann"){
      points(x[,d1][data$fullname==mp],x[,d2][data$fullname==mp],pch=19,col=pcols2[as.factor(data$party)[data$fullname==mp]],cex=1.7)
      text(x[,d1][data$fullname==mp],x[,d2][data$fullname==mp],labels = data$fullname[data$fullname==mp],pos=2,col=pcols2[as.factor(data$party)[data$fullname==mp]],font=4,cex=1.3)}
    if(nrow(data[data$fullname==mp,])>0){
      points(x[,d1][data$fullname==mp],x[,d2][data$fullname==mp],pch=19,col=pcols2[as.factor(data$party)[data$fullname==mp]],cex=1.1)
      text(x[,d1][data$fullname==mp],x[,d2][data$fullname==mp],labels = data$fullname[data$fullname==mp],pos=2,col=pcols2[as.factor(data$party)[data$fullname==mp]],cex=1.3,font=2)}
    }
  }  
}




add_names<-function(part,dim1="CA1",dim2="CA2",col1="black",data=m3){
  data$x<-data[,which(names(data)==dim1)]
  data$y<-data[,which(names(data)==dim2)]
  p1<-data[data$party==part,]
  text(p1$x[which(p1$x==min(p1$x))],p1$y[which(p1$x==min(p1$x))],labels=p1$fullname[which(p1$x==min(p1$x))],col=col1)
  text(p1$x[which(p1$x==max(p1$x))],p1$y[which(p1$x==max(p1$x))],labels=p1$fullname[which(p1$x==max(p1$x))],col=col1)
  text(p1$x[which(p1$y==min(p1$y))],p1$y[which(p1$y==min(p1$y))],labels=p1$fullname[which(p1$y==min(p1$y))],col=col1)
  text(p1$x[which(p1$y==max(p1$y))],p1$y[which(p1$y==max(p1$y))],labels=p1$fullname[which(p1$y==max(p1$y))],col=col1)
}

# es wäre gut wenn man die farbtiefe bei grün und lila tauschen könnte

# spd ggf helleres rot?


map_plot<-function(x,d1,d2,meta=me,s=0.7,number=100,x_lab="Dimension 1",y_lab="Dimension 2",spread=1
                   ,p=T #party letters as symbols
                   ,t=T
                   ,leg=NULL
                   ,en=F
                   ,trans=words
                   ,persons=NULL
                   ,conf=F
                   ,fuse=F){
  
  w<-x
  x<-x$CA$u
  x<-x[order(rownames(x)),]
  meta<-meta[order(meta$screen_name),]
  data<-meta
  pcols2<-c('dark blue','black','light blue','green','purple','gold1','grey','red')
  w1<-as.data.frame(top_words(w$CA$v,d1,number))
  w2<-as.data.frame(top_words(w$CA$v,d2,number))
  w1$terms<-rownames(w1)
  w2$terms<-rownames(w2)
  

  if(fuse==T){
  x[,d2]<-x[,d2]+x[,1]
  w2[,d2]<-w2[,d2]+w2[,1]
  w1[,d2]<-w1[,d2]+w1[,1]
  }
  
  if(conf==T){ces<-data$confidence}else{ces<-1}
  # merge, reorder
  if(en==T){
  
  w1$words<-row.names(w1)
  w1<-merge(w1,trans,by="words")
  w1<-w1[,-1]
  w1<-w1[order(w1[,1]),]
  w1$terms<-w1$translation
  
  w2$words<-row.names(w2)
  w2<-merge(w2,trans,by="words")
  w2<-w2[,-1]
  w2<-w2[order(w2[,2]),]
  names(w2)
  w2$terms<-w2$translation
  }
  ppch<-c(16,15,15,18,15,18,19,17)
  if(p==T){ppch<-c("A","C","U","G","L","F","P","S")}
  if(t==T){
  plot(x[,d1],x[,d2]
       ,col=pcols2[as.factor(data$party)]
       ,pch=ppch[as.factor(data$party)]
       ,cex=ces
       ,ylim=c(min(w2[,d2])*sqrt(spread),max(w2[,d2]))*sqrt(spread)
       ,xlim=c(min(w1[,d1])*sqrt(spread),max(w1[,d1]))*sqrt(spread)
       ,ylab=y_lab
       ,xlab=x_lab
  )
# call new doesn't work    
    textplot(w1[,d1],w1[,d2],w1$terms,new=F,cex=s,col="grey",show.lines = F)
    textplot(w2[,d1],w2[,d2],w2$terms,new=F,cex=s,col="grey",show.lines = F)
    
#text(w1[,d1]*spread,w1[,d2]*spread,labels=w1$terms,cex=0.5,col="grey")
#text(w2[,d1]*spread,w2[,d2]*spread,labels=w2$terms,cex=0.5,col="grey")
}else{plot(x[,d1],x[,d2]
       ,col=pcols2[as.factor(data$party)]
       ,pch=ppch[as.factor(data$party)]
       ,cex=ces
       ,ylab=y_lab
       ,xlab=x_lab)}

if(is.null(leg)==F){
  legend(
  x=leg,
  legend=levels(data$party),
  col=pcols2,
  pch=ppch)
}
  
if(is.null(persons)==F){
  for(i in 1:length(persons)){mp<-persons[i]
  if(nrow(data[data$fullname==mp,])>0){
  text(x[,d1][data$fullname==mp],x[,d2][data$fullname==mp],labels = data$fullname[data$fullname==mp],pos=4)}
  }
}  
    }











emph_plot<-function(x=v1,data=me,d1=1,d2=2,party,dim="social",x_lab="Dimension 1",ylab="Dimension 2"){
  
  x<-x$CA$u
  x<-x[order(rownames(x)),]
  data<-data[order(data$screen_name.y),]
  # whip, partygroup, minister
  
  # faction?
  if(dim=="social"){
  data$orientation<-data$social
  }else{data$orientation<-data$economic}

  ppch<-c("-","-","o","+","+")

plot(x[,d1],x[,d2]
       ,col=c("grey","black")[ifelse(data$party==party,2,1)]
       ,pch=ppch[as.factor(data$orientation)]
       ,ylab=y_lab
       ,xlab=x_lab
  )

#  add_names(part=party,d1,d2,m3)
      
}


prep_ca<-function(model,dfm,sparse=F){
  dat_ca <- data.frame(dim1 = coef(model, doc_dim = 1)$coef_document, 
                       dim2 = coef(model, doc_dim = 2)$coef_document,
                       dim3 = coef(model, doc_dim = 3)$coef_document,
                       dim4 = coef(model, doc_dim = 4)$coef_document,
                       screen_name=docvars(dfm,'screen_name'),
                       party=docvars(dfm,'party.y'),
                       fullname=docvars(dfm,'fullname'),
                       cons=docvars(dfm,'Won_District'),
                       pgl=docvars(dfm,'pgl'),
                       whip=docvars(dfm,'whip'),
                       faction=docvars(dfm,'faction'))
  
  dat_ca<-dat_ca[!is.infinite(dat_ca$dim1),]
  dat_ca<-dat_ca[!is.infinite(dat_ca$dim2),]
  return(dat_ca)
}

top_words<-function(x,dim,number,hash=T,at=T){
  n<-number/2
  if(at==F){x<-x[grepl('@',rownames(x))==F,]}
  if(hash==F){x<-x[grepl('#',rownames(x))==F,]}
  x<-x[order(x[,dim],decreasing=T),]
  top<-x[1:n,]
  bottom<-x[nrow(x):(nrow(x)-n),]
  names(bottom)<-gsub('CA','Dim',names(bottom))
  names(top)<-gsub('CA','Dim',names(top))
  return(rbind(bottom,top))
}

clear<-function(dfm_x,zoom=6,red=0){    
  
  if(red>0){dfm_x<-dfm_trim(dfm_x,min_termfreq = red, termfreq_type = "quantile")}
  
  tm<-textmodel_ca(dfm_x,sparse=T)
  
  dat_ca <- data.frame(dim1 = coef(tm, doc_dim = 1)$coef_document, 
                       dim2 = coef(tm, doc_dim = 2)$coef_document)
  dat_ca$screen_name<-rownames(dat_ca)
  x<-merge(dat_ca,f1,by='screen_name')
  
  x0<-x[is.infinite(x$dim1),]
  xz<-x[!is.infinite(x$dim1),]
  x1<-xz[xz$dim1>mean(xz$dim1)+zoom*sd(xz$dim1),]
  x2<-xz[xz$dim1<mean(xz$dim1)-zoom*sd(xz$dim1),]
  x3<-xz[xz$dim2>mean(xz$dim2)+zoom*sd(xz$dim2),]
  x4<-xz[xz$dim2<mean(xz$dim2)-zoom*sd(xz$dim2),]
  
  nonames<-c(x1$screen_name,x2$screen_name,x3$screen_name,x4$screen_name,x0$screen_name)
  dfm_r<-dfm_x
  for(i in 1:length(nonames)){
    dfm_r<-dfm_subset(dfm_r,screen_name!=nonames[i])
  }
  return(dfm_r)
}

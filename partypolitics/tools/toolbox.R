
if(!require("lubridate")){install.packages("lubridate")}
if(!require("vegan")){install.packages("vegan")}
if(!require("wordcloud")){install.packages("wordcloud")}
if(!require("quanteda")){install.packages("quanteda")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("stargazer")){install.packages("stargazer")}
if(!require("googleLanguageR")){install.packages("googleLanguageR")}


library(googleLanguageR)
library(lubridate)
library(vegan)
library(wordcloud)
library(quanteda)
library(ggplot2)
library(stargazer)

googleLanguageR::gl_auth("./tools/My First Project-c0ec0baa9709.json")

# defining color schemes
pcols2<-c('dark blue','black','light blue','green','purple','yellow','grey','red')
pcols<-c('dark blue','black','light blue','green','purple','yellow','grey','red')


# Coefficient Plot function
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


# Helpful functions for text cleaning
stripURL2 = function(x) {
  gsub("pic\\.[^[:space:]]+|www[^[:space:]]+|htt[^[:space:]]+|//t\\.[^[:space:]]+", " ", x)
 
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


top_words<-function(x,col=1,n=100){
  x<-v1$CA$v
  x<-x[,1:5]
  x<-as.data.frame(x)
  x<-x[order(x[,col]),] 
  x1<-x[1:(n/2),]
  x<-x[order(x[,col],decreasing = T),] 
  x2<-x[1:(n/2),]
  x<-rbind(x1,x2)
  return(x)
}

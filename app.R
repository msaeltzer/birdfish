

if(!require(wordcloud)){install.packages("wordcloud")}
if(!require(shiny)){install.packages("shiny")}

library(wordcloud)
library(shiny)

find<-function(df,x){df[,which(names(df)==x)]}

pcols<-c('dark blue','black','light blue','green','purple','gold1','grey','red')

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

me<-read.csv2("https://raw.githubusercontent.com/msaeltzer/birdfish/master/data.csv",stringsAsFactors = F,encoding = "utf8")
w1<-read.csv("https://raw.githubusercontent.com/msaeltzer/birdfish/master/words1.csv",stringsAsFactors = F)
w2<-read.csv("https://raw.githubusercontent.com/msaeltzer/birdfish/master/words2.csv",stringsAsFactors = F)

me$fullname<-iconv(me$fullname,from="latin1",to="UTF8")

me$party<-iconv(me$party,from="latin1",to="UTF8")


names(me)[20:25]<-c("Government_Opposition","Left_Right","Liberal_Conservative","Fourth Dimensions","Economic_LR","Cultural_LR")

me$party<-as.factor(me$party)

w1$English<-w1$translation
w1$German<-w1$term

w1$cult_pos<-w1$cult
w1$econ_pos<-w1$econ

w2$cult_pos<-w2$cult
w2$econ_pos<-w2$econ

w2$English<-w2$translation
w2$German<-w2$term


        ## Each shiny app needs a user interface and a server
        
        # user interface



        # we create a page with tabs: navbar page
ui_full<-fluidPage("Dimensions of Worduse on Twitter",
                                     titlePanel("Twitter Positions of German MPs"),    # give it a title
                                     
                  
                                     sidebarLayout(                           # we choose a sidebar layout 
                                       
                                       sidebarPanel(                           # control panel
                                         # We need three inputs so far: two scale selections and a on/off button
                                         # we choose from the scale varnames        
                                         selectInput('scale1', 'Dimension 1', names(me)[c(20:25)],selected=names(me)[24]),
                                         selectInput('scale2', 'Dimension 2', names(me)[c(20:25)],selected=names(me)[25]),
                                         selectInput('pols', 'Top Politicians',c("On","Off"),"Off"),
                                         selectInput("mps","Your MP:",me$fullname),
                                         sliderInput("wordcount","Display Words",min=0,max=0,value=0),
                                        selectInput('language', 'Language',c("German","English"),"German"),
                                         selectInput('legend', 'Legend',c("On","Off"),"On")
                                         ),                                       
                                       
                                       mainPanel(                              # plot panel
                                         plotOutput("scatter"),
                                         h6("For words to be plotted, please try to run the App locally! You can find it as app_2.R https://github.com/msaeltzer/birdfish"),
                                         
                                       ) 
                                       
                                     ) # end layout
        ) # end UI



        # we define what the server returns for input
        server_full <- function(input, output) {
          # plot tab 1
          output$scatter<-renderPlot({
        
            plot(find(me,input$scale1),find(me,input$scale2),col=pcols[me$party],ylab=input$scale2,xlab=input$scale1,pch=19)
            if(input$pols=="On"){
              t<-me[me$fullname %in% prom,]
              textplot(find(t,input$scale1),find(t,input$scale2),words=t$fullname,col=pcols[t$party],new=F,show.lines=F)
            }
            
         t1<-me[me$fullname==input$mps,]
         text(find(t1,input$scale1),find(t1,input$scale2),labels=t1$fullname,col=pcols[t1$party])
                  
                  
            if(input$wordcount>0){
            wo1<-w1[1:input$wordcount,]
            wo2<-w2[1:input$wordcount,]
            wo<-rbind(wo1,wo2)
            textplot(find(wo,input$scale1),find(wo,input$scale2),words =find(wo,input$language),cex=0.4,new=F,show.lines=F)
            }
         if(input$legend=="On"){
         legend(
                 x="topleft",
                 legend=levels(me$party),
                 col=pcols,
                 pch=19)}
         
          })
          
        }
        
        
        shinyApp(ui=ui_full,server=server_full)

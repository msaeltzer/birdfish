library(wordcloud)

#library(profvis)
#profvis(runApp())

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


        # dataset prep and import
        
me<-read.csv2("https://raw.githubusercontent.com/msaeltzer/birdfish/master/data.csv")
w1<-read.csv("https://raw.githubusercontent.com/msaeltzer/birdfish/master/words1.csv")
w2<-read.csv("https://raw.githubusercontent.com/msaeltzer/birdfish/master/words2.csv")

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
ui_full<-fluidPage("Twitter Positions of German MP's",
                                     titlePanel("Comparing Dimensions"),    # give it a title
                                     
                  
                                     sidebarLayout(                           # we choose a sidebar layout 
                                       
                                       sidebarPanel(                           # control panel
                                         # We need three inputs so far: two scale selections and a on/off button
                                         # we choose from the scale varnames        
                                         selectInput('scale1', 'Scale 1', names(me)[c(20:25)],selected=names(me)[20]),
                                         selectInput('scale2', 'Scale 2', names(me)[c(20:25)],selected=names(me)[20]),
                                         selectInput('pols', 'Top Politicians',c("On","Off"),"Off"),
                                         sliderInput("wordcount","Display Words",min=0,max=100,value=0),
                                         selectInput('language', 'Language',c("German","English"),"German")
                                         
                                       ),
                                       
                                       mainPanel(                              # plot panel
                                         plotOutput("scatter"),
                                       ) 
                                       
                                     ) # end layout
        ) # end UI

        # we define what the server returns for input
        server_full <- function(input, output) {
          # plot tab 1
          output$scatter<-renderPlot({
        
            plot(find(me,input$scale1),find(me,input$scale2),col=pcols[me$party],ylab=input$scale1,xlab=input$scale2,pch=19)
            if(input$pols=="On"){
              for(i in 1:length(prom)){
              t<-me[me$fullname==prom[i],]
              text(find(t,input$scale1),find(t,input$scale2),labels=t$fullname,col=pcols2[t$party])
              }
            }
            
            if(input$wordcount>0){
            wo1<-w1[1:input$wordcount,]
            wo2<-w2[1:input$wordcount,]
            wo<-rbind(wo1,wo2)
            textplot(find(wo,input$scale1),find(wo,input$scale2),words =find(wo,input$language),cex=0.4,new=F,show.lines=F)
            }
              
            

          })
          
        }
        
        
        shinyApp(ui=ui_full,server=server_full)

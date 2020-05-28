

if(!require(wordcloud)){install.packages("wordcloud")}
if(!require(shiny)){install.packages("shiny")}
if(!require(plotly)){install.packages("plotly")}


library(wordcloud)
library(shiny)
library(plotly)

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
w3<-read.csv("https://raw.githubusercontent.com/msaeltzer/birdfish/master/words3.csv",stringsAsFactors = F)

me$fullname<-iconv(me$fullname,from="latin1",to="UTF8")

me$party<-iconv(me$party,from="latin1",to="UTF8")


names(me)[20:25]<-c("Government_Opposition","Left_Right","Liberal_Conservative","Fourth Dimensions","Economic_LR","Cultural_LR")

me$party<-as.factor(me$party)

w1$English<-w1$translation
w1$German<-w1$term

w2$English<-w2$translation
w2$German<-w2$term

w2$cult_pos<-w2$cult
w2$econ_pos<-w2$econ

w1$cult_pos<-w2$cult
w1$econ_pos<-w2$econ


w3$English<-w3$translation
w3$German<-w3$term

w3$cult_pos<-w3$cult
w3$econ_pos<-w3$econ

names(w1)[3:6]<-c("Government_Opposition","Left_Right","Liberal_Conservative","Fourth Dimension")
names(w1)[480:481]<-c("Economic_LR","Cultural_LR")

names(w2)[3:6]<-c("Government_Opposition","Left_Right","Liberal_Conservative","Fourth Dimension")
names(w2)[480:481]<-c("Economic_LR","Cultural_LR")

names(w3)[3:6]<-c("Government_Opposition","Left_Right","Liberal_Conservative","Fourth Dimension")
names(w3)[480:481]<-c("Economic_LR","Cultural_LR")


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
                                   checkboxInput('pols', 'Top Politicians', value = FALSE),
                                   selectInput("mps","Your MP:",me$fullname)
                           ),                                       
                           
                           mainPanel(                              # plot panel
                                   plotlyOutput("scatter")                                  
                           ) 
                           
                   ) # end layout
) # end UI

input<-list()


input$scale1<-"Economic_LR"
input$scale2<-"Cultural_LR"
input$wordcount<-100

# we define what the server returns for input
server_full <- function(input, output) {
        # plot tab 1
        output$scatter<-renderPlotly({
            d <- data.frame(x = find(me,input$scale1), y = find(me,input$scale2), party = me$party, fullname = me$fullname, my_mp = ifelse(input$mps == me$fullname, input$mps, ""), top_mp = ifelse(me$fullname %in% prom, me$fullname, ""))
            if (!input$pols) {
                d$top_mp <- ""
            }
            pals <- c('dark blue','black','light blue','green','purple','gold1','grey','red')
            pals <- setNames(pals, levels(me$party))
            f <- list(
                size = 18,
                color = "#7f7f7f"
            )
            xlab <- list(title = input$scale1, titlefont = f)
            ylab <- list(title = input$scale2, titlefont = f)            
            fig <- plot_ly(
                d, x = ~x, y = ~ y, color = ~ party, text = ~ fullname, colors = pals
            ) %>% add_markers() %>% layout(xaxis = xlab, yaxis = ylab) %>% add_text(text = ~ my_mp, textposition = "top right") %>% add_text(text = ~ top_mp, textposition = "top right")
        })        
}


shinyApp(ui=ui_full,server=server_full)

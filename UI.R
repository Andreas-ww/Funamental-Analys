library(shiny)
Vis_historik <- function(Variabel){
  ggplot(data=data_tids , aes(y=data_tids[,Variabel] , x= ÅR , group=1))+
    geom_bar(stat="identity" , fill="bisque3" , col="black")+
    geom_smooth(method="lm", se=F , col="brown")+
    theme_bw()+
    scale_y_continuous(n.break=10,
                       expand = c(0.01,0.01))+
    labs(title=paste("Historik av", Variabel),
         x="ÅR" , y = Variabel)+
    geom_line(linetype=2, col="black", size=1)
}



## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Ladda upp .xlsx fil från Börsdata",
                  accept = c(".xslx")
        ),
        tags$hr(),
      ),
      mainPanel(
        tableOutput("contents"),
        plotOutput("plot1", click = "plot_click")
      )
    )
  )
  
  server <- function(input, output) {
    
    
    
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      library(readxl)
      data <- readxl::read_xlsx(path=inFile$datapath , sheet = "Year")
      #Ta bort tomma rader
      tomma_rader <- is.na(data$Report) 
      data <- data[!tomma_rader,]
      
      #Ändra namnen på kolumner
      names(data)[2] <- "Tal"
      names(data)[2] <- "Enhet"
      
      
      #Ta bort tomma kolumner
      tomma_kol <- c()
      for(i in 1:ncol(data)){
        if( colSums(is.na(data[,i]))==nrow(data[,i]) ) {
          tomma_kol <- c(tomma_kol,i)
        }
      }
      data <- data[,-tomma_kol]
      
      #Antal år med info (Sista är rullande 12 från Kvartalet)
      num_years<- ncol(data)-2
      years <- 3:(num_years+2)
      
      #Från tibble till dataframe
      data <- as.data.frame(data)
      
      #Ändrar lite namn tal
      data[data$Report=="Rörelseresultat",1] <- "Rörelsemarginal (EBIT)"
      data[data$Report=="Nettoskuld",1][2] <- "Nettoskuld %"
      
      
      ####      ---        Lägger till rader     ---     ###
      
      #Börsvärde
      B_varde <- as.vector(data[which(data$Report=="Aktiekurs Snitt"),years]*data[which(data$Report=="Antal Aktier"),years])
      data[nrow(data)+1,] <- c("Börsvärde" , "MSEK", B_varde)
      
      #EV
      E_V <- data[data$Report=="Börsvärde",years] +  data[data$Report=="Nettoskuld",years]
      data[nrow(data)+1,] <- c("EV" , "MSEK", E_V)
      
      
      ###     ---       Skapa tidsserie     ---     ###
      
      tad <- t(data)
      data_tids <- data.frame(tad[-(1:2),])
      colnames(data_tids) <- data$Report
      colnames(data_tids) <- make.unique(names(data_tids))
      
      #Skapar kolumn för år
      data_tids <- cbind((2020-num_years+1):2020, data_tids)
      names(data_tids)[1] <- "ÅR"
      data_tids$ÅR <- as.factor(data_tids$ÅR)
      
      
      #Gör om till numeric
      for(i in 2:ncol(data_tids)){
        data_tids[,i] <- as.numeric(data_tids[,i])
      }
      
      
      ###     ---       Välj ut viktiga kolumner     ---     ###
      
      
      
      ###     ---       Årlig Tillväxt     ---     ###
      
      
      
      
      for(j in 2:ncol(data_tids)){
        #Namn för ny kolumn för tillväxt
        temp_namn <- paste(colnames(data_tids)[j],"-tillväxt" , " %" , sep="")
        
        for (i in 2:num_years) {
          data_tids[i,temp_namn] <- ( ( data_tids[i,j] - data_tids[i-1,j] ) / data_tids[i-1,j] )*100
          
        }
      }
      return(data_tids)
    })
    
    output$plot1 <- renderPlot({
      library(ggplot2)
      Vis_historik(colnames(data_tids)[15])
      })
  }
  
  shinyApp(ui, server)
}
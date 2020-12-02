ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Ladda upp .xlsx fil från Börsdata",
                accept = c(".xslx")
      ),
      tags$hr(),
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      tableOutput("table")
    )
  )
)


server <- function(input, output, session) {
  library(readxl)
  dataset <- reactive({
    inFile <- input$file1
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
    data_tids
  })
  
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  output$table <- renderTable({
    dataset()
  })
}


shinyApp(ui, server)
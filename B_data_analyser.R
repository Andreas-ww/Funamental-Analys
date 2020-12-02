library(readxl)

pth <- "C:/Users/andre/Downloads/INSTAL-Instalco.xlsx"
data <- readxl::read_xlsx(path=pth , sheet = "Year")



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






###     ---     Visualisera historik      ---     ###
library(ggplot2)
library(plyr)

#round_any(101010, 100)
plot(data_tids$Omsättning , x=((2020-num_years+1):2020))

#Omsättning
ggplot(data=data_tids , aes(y=Omsättning , x= ÅR , group=1))+
  geom_bar(stat="identity" , fill="bisque3" , col="black")+
  geom_smooth(method="lm")+
  theme_bw()+
  scale_y_continuous(breaks = seq(0,max(data_tids$Omsättning),round_any(max(data_tids$Omsättning)/5 , 10) ),
                     limits = c(0,max(data_tids$Omsättning))*1.1,
                     expand = c(0,0))
  
  

#Funktion för visualisering
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


data_tids$`Nettoskuld %`
hehe <- which( colnames(data_tids)=="Nettoskuld %" )

Vis_historik(colnames(data_tids)[hehe])




###----- PROGNOSER --------###

#Skapar ny data.frame för prognoser med viktiga kolumner


data_prognos <- data_tids[,c(c(1,2,6,7,8,9,30,42,55,58,59,61),c(1,2,6,7,8,9,30,42,55,58,59,61)+73)]


prog_length <- 5 # År framåt prognos  
n <- 3 #Moving average

for (i in 1:prog_length) {
  #Skapar nya rader
  data_prognos[as.character(paste(i+year_before,"E")),1] <- NA
  
  #Fixar år
  levels(data_prognos$ÅR)[num_years+i] <- as.character(year_before+i)
  data_prognos$ÅR[num_years+i] <- as.character(year_before+i)
 
}  

#Omsättning tillväxt
#MA n år tillbaka
oms_ma <- mean(data_prognos$`Omsättning-tillväxt %`[(num_years-n+1):num_years])
oms_vec<- rep(oms_ma,prog_length)

#Omsättnig scaler
alpha <- 0.95

for (i in 2:length(oms_vec)){
  oms_vec[i] <- oms_vec[i-1]*alpha
}

data_prognos$`Omsättning-tillväxt %`[(num_years+1):(num_years+prog_length)] <- oms_vec

for (i in (num_years+1):(num_years+prog_length)) {
  data_prognos$Omsättning[i] <- data_prognos$Omsättning[i-1]*(1+data_prognos$`Omsättning-tillväxt %`[i]/100)
    
}
data_prognos$Omsättning <- 


data_tids[1,2:74]
 
  #Skapa dummy variabel PROGNOS
  
  #Moving average n steg tillbaka för tillväxt
  data_tids[num_years+i, 75:ncol(data_tids) ] <- apply(data_tids[(num_years+i-n):(num_years+i-1),-1] , 2 , mean)
  
  #Värden utifrån moving average 
  data_tids[num_years+i,c(-1,1:74)]
  
  
  #Corresponding %Tillväxt 


#TO DO 
#Prognose Gör kolumn med Prognos = SANT/FALSKT
#Välja ut kolumner?

data_tids[10,1] <- "2025"
levels(data_tids$ÅR)[num_years+1] <- "2025"
data_tids$ÅR[num_years+1] <- "2025"


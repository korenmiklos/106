library(data.table)
library(XLConnect)
library(tidyr)
library(stats)
library(ggplot2)

setwd("C:/Users/adasan01/OneDrive - ARM/Desktop/TacticalVoting/kerületi chartok")
wb <- loadWorkbook("vote_share_pred.xlsx")

data <- readWorksheet(wb, sheet = "in", header = TRUE)

data <- data.table(data)

data1 <- data[M.DK.F == "MSZP-P"]
data1[,c("M.DK.F")] <- NULL
data2 <- data[M.DK.F == "DK"]
data2[,c("M.DK.F")] <- NULL
data3 <- data[M.DK.F == "Független"]
data3[,c("M.DK.F")] <- NULL

data_long1 <- melt(data1, id.vars = c("OEVK"))
data_long1$variable <- gsub("Balodal", "MSZP-P", data_long1$variable)

data_long2 <- melt(data2, id.vars = c("OEVK"))
data_long2$variable <- gsub("Balodal", "DK", data_long2$variable)

data_long3 <- melt(data3, id.vars = c("OEVK"))
data_long3$variable <- gsub("Balodal", "Független", data_long3$variable)

data_long <- rbind(data_long1, data_long2)
data_long <- rbind(data_long, data_long3)

#data_long <- data_long[, value:= ifelse(value == 0, NA, value)]
#data_long <- data_long[value != is.na(value)]

szavazo <- unique(data_long$OEVK)
partok <- unique(data_long$variable)

szavazo
partok

## location of saved files:

setwd("C:/Users/adasan01/OneDrive - ARM/Desktop/TacticalVoting/kerületi chartok/output")


# loop to save all 106 results
for (korzet in szavazo){
  print(korzet)
  
  toplot <- data_long[OEVK == korzet]
  
  filename <- unique(toplot$OEVK)
  
  ggplot(toplot, aes(x = reorder(variable, -value), y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual("legend", values = c("Fidesz" = "orange",
                                           "MSZP-P" = "red",
                                           "LMP" = "green4",
                                           "Momentum" = "purple",
                                           "Együtt" = "black",
                                           "Jobbik" = "grey50",
                                           "MKKP" = "grey90",
                                           "Egyéb" = "yellow",
                                           "Független" = "lightpink2",
                                           "DK" = "darkblue")) + 
    theme_bw() + labs(x = "Pártok", y = "Szavazatarány", title = "2018-as egyéni eredmények") +
    geom_text(aes( label = scales::percent(value),
                   y= value), stat= "identity", vjust = -.5, size = 3) + 
    guides(fill=FALSE) 
  
  plotlyhoz <- ggplot(toplot, aes(x = reorder(variable, -value), y = value, fill = variable)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual("legend", values = c("Fidesz" = "orange",
                                           "MSZP-P" = "red",
                                           "LMP" = "green4",
                                           "Momentum" = "purple",
                                           "Együtt" = "black",
                                           "Jobbik" = "grey50",
                                           "MKKP" = "grey90",
                                           "Egyéb" = "yellow",
                                           "Független" = "lightpink2",
                                           "DK" = "darkblue")) + 
    theme_bw() + labs(x = "Pártok", y = "Szavazatarány", title = "2018-as egyéni eredmények") +
    geom_text(aes( label = scales::percent(value),
                   y= value), stat= "identity", vjust = -.5, size = 3) + 
    guides(fill=FALSE) 
  
  ggsave(paste(filename, "png", sep = "."), width = 15, height = 10, units = c("cm"))
  
}





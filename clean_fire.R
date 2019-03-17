library(plyr)
library(dplyr)
library(tidyr)
library(data.table)

getwd()
setwd("/Users/nicoshiro/Downloads/midterm-dukes-of-data-gh-pages_FINAL")

fire <- read.csv("Final_Fire_Dataset_Midterm.csv")

fire <- fire[order(fire$Neighborhooods...Analysis.Boundaries), ]
row.names(fire) <- 1:nrow(fire)
fire_Chinatown <- fire[1:5987,]
fire_Lone_Mountain <- fire[5988:9614,]
fire_Nob_Hill <- fire[9615:19518,]
fire_North_Beach <- fire[19519:26103,]
fire_Russian_Hill <- fire[26104:30595,]
fire_Tenderloin <- fire[30596:74489,]





###### Chinatown #######
fire_unique <- fire_Chinatown %>% distinct(Call.Number, .keep_all = TRUE)

fire_unique <- fire_unique[order(fire_unique$On.Scene.DtTm), ]
row.names(fire_unique) <- 1:nrow(fire_unique)
fire_unique <- fire_unique[393:2773,]

fire_unique <- fire_unique[order(fire_unique$Call.Number), ]
row.names(fire_unique) <- 1:nrow(fire_unique)

fus <- subset(fire_unique, select = c(Call.Number, Call.Date, Received.DtTm, On.Scene.DtTm, Call.Type.Group))

fus$Received.DtTm <- substr(fus$Received.DtTm, 12, 22)
fus$On.Scene.DtTm <- substr(fus$On.Scene.DtTm, 12, 22)


for (i in 1:2381) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "AM" && substr(fus$Received.DtTm[i], 1, 2) == "12"){
    fus$Received.DtTm[i] <- gsub("12", "00", fus$Received.DtTm[i])
  }

  if (substr(fus$On.Scene.DtTm[i], 10, 11) == "AM" && substr(fus$On.Scene.DtTm[i], 1, 2) == "12"){
    fus$On.Scene.DtTm[i] <- gsub("12", "00", fus$On.Scene.DtTm[i])
  }
}

for (i in 1:2381) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "PM"){
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)

    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")

    fus$Received.DtTm[i] <- as.integer(fus$Received.DtTm[i]) + 120000
    fus$On.Scene.DtTm[i] <- as.integer(fus$On.Scene.DtTm[i]) + 120000

  } else{
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)

    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
  }
}

for (i in 1:2381) {
  if (substr(fus$Received.DtTm[i], 1, 2) == "24"){
    fus$Received.DtTm[i] <- gsub("24", "12", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 1, 2) == "24"){
    fus$On.Scene.DtTm[i] <- gsub("24", "12", fus$On.Scene.DtTm[i])
  }
}

fus$Received.Hr <- substr(fus$Received.DtTm, 1, 2)
fus$Received.Mn <- substr(fus$Received.DtTm, 3, 4)
fus$Received.Sc <- substr(fus$Received.DtTm, 5, 6)

fus$On.Scene.Hr <- substr(fus$On.Scene.DtTm, 1, 2)
fus$On.Scene.Mn <- substr(fus$On.Scene.DtTm, 3, 4)
fus$On.Scene.Sc <- substr(fus$On.Scene.DtTm, 5, 6)

fus$Received.Time.New <- paste(fus$Received.Hr, fus$Received.Mn, fus$Received.Sc, sep = ":")
fus$On.Scene.Time.New <- paste(fus$On.Scene.Hr, fus$On.Scene.Mn, fus$On.Scene.Sc, sep = ":")

fus$Received.DateTime.New <- paste(fus$Call.Date, fus$Received.Time.New, sep = " ")
fus$On.Scene.DateTime.New <- paste(fus$Call.Date, fus$On.Scene.Time.New, sep = " ")



fus$Received.DtTm <- as.POSIXct(fus$Received.DateTime.New, format="%m/%d/%Y %H:%M:%S")
fus$On.Scene.DtTm <- as.POSIXct(fus$On.Scene.DateTime.New, format="%m/%d/%Y %H:%M:%S")

fus$Wait.Time <- difftime(fus$On.Scene.DtTm, fus$Received.DtTm, units = "mins")


fus <- fus[order(fus$Wait.Time), ]
row.names(fus) <- 1:nrow(fus)

fus <- fus[49:2381,]

fus <- fus[order(fus$Call.Number), ]
row.names(fus) <- 1:nrow(fus)

fus$Call.Month <- substr(fus$Call.Date, 1, 2)

fus_Jan <- fus[1:197,]
fus_Feb <- fus[198:400,]
fus_Mar <- fus[401:608,]
fus_Apr <- fus[609:786,]
fus_May <- fus[787:960,]
fus_Jun <- fus[961:1160,]
fus_Jul <- fus[1161:1330,]
fus_Aug <- fus[1331:1499,]
fus_Sep <- fus[1500:1710,]
fus_Oct <- fus[1711:1915,]
fus_Nov <- fus[1916:2126,]
fus_Dec <- fus[2127:2333,]

mean_fus_Jan <- mean(as.integer(fus_Jan$Wait.Time))
mean_fus_Feb <- mean(as.integer(fus_Feb$Wait.Time))
mean_fus_Mar <- mean(as.integer(fus_Mar$Wait.Time))
mean_fus_Apr <- mean(as.integer(fus_Apr$Wait.Time))
mean_fus_May <- mean(as.integer(fus_May$Wait.Time))
mean_fus_Jun <- mean(as.integer(fus_Jun$Wait.Time))
mean_fus_Jul <- mean(as.integer(fus_Jul$Wait.Time))
mean_fus_Aug <- mean(as.integer(fus_Aug$Wait.Time))
mean_fus_Sep <- mean(as.integer(fus_Sep$Wait.Time))
mean_fus_Oct <- mean(as.integer(fus_Oct$Wait.Time))
mean_fus_Nov <- mean(as.integer(fus_Nov$Wait.Time))
mean_fus_Dec <- mean(as.integer(fus_Dec$Wait.Time))

median_fus_Jan <- median(as.integer(fus_Jan$Wait.Time))
median_fus_Feb <- median(as.integer(fus_Feb$Wait.Time))
median_fus_Mar <- median(as.integer(fus_Mar$Wait.Time))
median_fus_Apr <- median(as.integer(fus_Apr$Wait.Time))
median_fus_May <- median(as.integer(fus_May$Wait.Time))
median_fus_Jun <- median(as.integer(fus_Jun$Wait.Time))
median_fus_Jul <- median(as.integer(fus_Jul$Wait.Time))
median_fus_Aug <- median(as.integer(fus_Aug$Wait.Time))
median_fus_Sep <- median(as.integer(fus_Sep$Wait.Time))
median_fus_Oct <- median(as.integer(fus_Oct$Wait.Time))
median_fus_Nov <- median(as.integer(fus_Nov$Wait.Time))
median_fus_Dec <- median(as.integer(fus_Dec$Wait.Time))

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
means_Chinatown <- c(mean_fus_Jan, mean_fus_Feb, mean_fus_Mar, mean_fus_Apr, mean_fus_May, mean_fus_Jun, mean_fus_Jul, mean_fus_Aug, mean_fus_Sep, mean_fus_Oct, mean_fus_Nov, mean_fus_Dec)
medians_Chinatown <- c(median_fus_Jan, median_fus_Feb, median_fus_Mar, median_fus_Apr, median_fus_May, median_fus_Jun, median_fus_Jul, median_fus_Aug, median_fus_Sep, median_fus_Oct, median_fus_Nov, median_fus_Dec)
counts_Chinatown <- c(nrow(fus_Jan), nrow(fus_Feb), nrow(fus_Mar), nrow(fus_Apr), nrow(fus_May), nrow(fus_Jun), nrow(fus_Jul), nrow(fus_Aug), nrow(fus_Sep), nrow(fus_Oct), nrow(fus_Nov), nrow(fus_Dec))
max_wait_time_Chinatown <- c(as.numeric(max(fus_Jan$Wait.Time)), as.numeric(max(fus_Feb$Wait.Time)), as.numeric(max(fus_Mar$Wait.Time)), as.numeric(max(fus_Apr$Wait.Time)), as.numeric(max(fus_May$Wait.Time)), as.numeric(max(fus_Jun$Wait.Time)), 
                             as.numeric(max(fus_Jul$Wait.Time)), as.numeric(max(fus_Aug$Wait.Time)), as.numeric(max(fus_Sep$Wait.Time)), as.numeric(max(fus_Oct$Wait.Time)), as.numeric(max(fus_Nov$Wait.Time)), as.numeric(max(fus_Dec$Wait.Time)))
min_wait_time_Chinatown <- c(as.numeric(min(fus_Jan$Wait.Time)), as.numeric(min(fus_Feb$Wait.Time)), as.numeric(min(fus_Mar$Wait.Time)), as.numeric(min(fus_Apr$Wait.Time)), as.numeric(min(fus_May$Wait.Time)), as.numeric(min(fus_Jun$Wait.Time)), 
                             as.numeric(min(fus_Jul$Wait.Time)), as.numeric(min(fus_Aug$Wait.Time)), as.numeric(min(fus_Sep$Wait.Time)), as.numeric(min(fus_Oct$Wait.Time)), as.numeric(min(fus_Nov$Wait.Time)), as.numeric(min(fus_Dec$Wait.Time)))





###### Lone Mountain #######
fire_unique <- fire_Lone_Mountain %>% distinct(Call.Number, .keep_all = TRUE)

fire_unique <- fire_unique[order(fire_unique$On.Scene.DtTm), ]
row.names(fire_unique) <- 1:nrow(fire_unique)
fire_unique <- fire_unique[232:1744,]

fire_unique <- fire_unique[order(fire_unique$Call.Number), ]
row.names(fire_unique) <- 1:nrow(fire_unique)

fus <- subset(fire_unique, select = c(Call.Number, Call.Date, Received.DtTm, On.Scene.DtTm, Call.Type.Group))

fus$Received.DtTm <- substr(fus$Received.DtTm, 12, 22)
fus$On.Scene.DtTm <- substr(fus$On.Scene.DtTm, 12, 22)


for (i in 1:1513) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "AM" && substr(fus$Received.DtTm[i], 1, 2) == "12"){
    fus$Received.DtTm[i] <- gsub("12", "00", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 10, 11) == "AM" && substr(fus$On.Scene.DtTm[i], 1, 2) == "12"){
    fus$On.Scene.DtTm[i] <- gsub("12", "00", fus$On.Scene.DtTm[i])
  }
}

for (i in 1:1513) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "PM"){
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
    
    fus$Received.DtTm[i] <- as.integer(fus$Received.DtTm[i]) + 120000
    fus$On.Scene.DtTm[i] <- as.integer(fus$On.Scene.DtTm[i]) + 120000
    
  } else{
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
  }
}

for (i in 1:1513) {
  if (substr(fus$Received.DtTm[i], 1, 2) == "24"){
    fus$Received.DtTm[i] <- gsub("24", "12", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 1, 2) == "24"){
    fus$On.Scene.DtTm[i] <- gsub("24", "12", fus$On.Scene.DtTm[i])
  }
}

fus$Received.Hr <- substr(fus$Received.DtTm, 1, 2)
fus$Received.Mn <- substr(fus$Received.DtTm, 3, 4)
fus$Received.Sc <- substr(fus$Received.DtTm, 5, 6)

fus$On.Scene.Hr <- substr(fus$On.Scene.DtTm, 1, 2)
fus$On.Scene.Mn <- substr(fus$On.Scene.DtTm, 3, 4)
fus$On.Scene.Sc <- substr(fus$On.Scene.DtTm, 5, 6)

fus$Received.Time.New <- paste(fus$Received.Hr, fus$Received.Mn, fus$Received.Sc, sep = ":")
fus$On.Scene.Time.New <- paste(fus$On.Scene.Hr, fus$On.Scene.Mn, fus$On.Scene.Sc, sep = ":")

fus$Received.DateTime.New <- paste(fus$Call.Date, fus$Received.Time.New, sep = " ")
fus$On.Scene.DateTime.New <- paste(fus$Call.Date, fus$On.Scene.Time.New, sep = " ")



fus$Received.DtTm <- as.POSIXct(fus$Received.DateTime.New, format="%m/%d/%Y %H:%M:%S")
fus$On.Scene.DtTm <- as.POSIXct(fus$On.Scene.DateTime.New, format="%m/%d/%Y %H:%M:%S")

fus$Wait.Time <- difftime(fus$On.Scene.DtTm, fus$Received.DtTm, units = "mins")


fus <- fus[order(fus$Wait.Time), ]
row.names(fus) <- 1:nrow(fus)

fus <- fus[23:1513,]

fus <- fus[order(fus$Call.Number), ]
row.names(fus) <- 1:nrow(fus)

fus$Call.Month <- substr(fus$Call.Date, 1, 2)

fus_Jan <- fus[1:123,]
fus_Feb <- fus[124:253,]
fus_Mar <- fus[254:409,]
fus_Apr <- fus[410:521,]
fus_May <- fus[522:655,]
fus_Jun <- fus[656:766,]
fus_Jul <- fus[767:885,]
fus_Aug <- fus[886:991,]
fus_Sep <- fus[992:1104,]
fus_Oct <- fus[1105:1229,]
fus_Nov <- fus[1230:1364,]
fus_Dec <- fus[1365:1491,]

mean_fus_Jan <- mean(as.integer(fus_Jan$Wait.Time))
mean_fus_Feb <- mean(as.integer(fus_Feb$Wait.Time))
mean_fus_Mar <- mean(as.integer(fus_Mar$Wait.Time))
mean_fus_Apr <- mean(as.integer(fus_Apr$Wait.Time))
mean_fus_May <- mean(as.integer(fus_May$Wait.Time))
mean_fus_Jun <- mean(as.integer(fus_Jun$Wait.Time))
mean_fus_Jul <- mean(as.integer(fus_Jul$Wait.Time))
mean_fus_Aug <- mean(as.integer(fus_Aug$Wait.Time))
mean_fus_Sep <- mean(as.integer(fus_Sep$Wait.Time))
mean_fus_Oct <- mean(as.integer(fus_Oct$Wait.Time))
mean_fus_Nov <- mean(as.integer(fus_Nov$Wait.Time))
mean_fus_Dec <- mean(as.integer(fus_Dec$Wait.Time))

median_fus_Jan <- median(as.integer(fus_Jan$Wait.Time))
median_fus_Feb <- median(as.integer(fus_Feb$Wait.Time))
median_fus_Mar <- median(as.integer(fus_Mar$Wait.Time))
median_fus_Apr <- median(as.integer(fus_Apr$Wait.Time))
median_fus_May <- median(as.integer(fus_May$Wait.Time))
median_fus_Jun <- median(as.integer(fus_Jun$Wait.Time))
median_fus_Jul <- median(as.integer(fus_Jul$Wait.Time))
median_fus_Aug <- median(as.integer(fus_Aug$Wait.Time))
median_fus_Sep <- median(as.integer(fus_Sep$Wait.Time))
median_fus_Oct <- median(as.integer(fus_Oct$Wait.Time))
median_fus_Nov <- median(as.integer(fus_Nov$Wait.Time))
median_fus_Dec <- median(as.integer(fus_Dec$Wait.Time))

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
means_USF <- c(mean_fus_Jan, mean_fus_Feb, mean_fus_Mar, mean_fus_Apr, mean_fus_May, mean_fus_Jun, mean_fus_Jul, mean_fus_Aug, mean_fus_Sep, mean_fus_Oct, mean_fus_Nov, mean_fus_Dec)
medians_USF <- c(median_fus_Jan, median_fus_Feb, median_fus_Mar, median_fus_Apr, median_fus_May, median_fus_Jun, median_fus_Jul, median_fus_Aug, median_fus_Sep, median_fus_Oct, median_fus_Nov, median_fus_Dec)
counts_USF <- c(nrow(fus_Jan), nrow(fus_Feb), nrow(fus_Mar), nrow(fus_Apr), nrow(fus_May), nrow(fus_Jun), nrow(fus_Jul), nrow(fus_Aug), nrow(fus_Sep), nrow(fus_Oct), nrow(fus_Nov), nrow(fus_Dec))
max_wait_time_USF <- c(as.numeric(max(fus_Jan$Wait.Time)), as.numeric(max(fus_Feb$Wait.Time)), as.numeric(max(fus_Mar$Wait.Time)), as.numeric(max(fus_Apr$Wait.Time)), as.numeric(max(fus_May$Wait.Time)), as.numeric(max(fus_Jun$Wait.Time)), 
                             as.numeric(max(fus_Jul$Wait.Time)), as.numeric(max(fus_Aug$Wait.Time)), as.numeric(max(fus_Sep$Wait.Time)), as.numeric(max(fus_Oct$Wait.Time)), as.numeric(max(fus_Nov$Wait.Time)), as.numeric(max(fus_Dec$Wait.Time)))
min_wait_time_USF <- c(as.numeric(min(fus_Jan$Wait.Time)), as.numeric(min(fus_Feb$Wait.Time)), as.numeric(min(fus_Mar$Wait.Time)), as.numeric(min(fus_Apr$Wait.Time)), as.numeric(min(fus_May$Wait.Time)), as.numeric(min(fus_Jun$Wait.Time)), 
                             as.numeric(min(fus_Jul$Wait.Time)), as.numeric(min(fus_Aug$Wait.Time)), as.numeric(min(fus_Sep$Wait.Time)), as.numeric(min(fus_Oct$Wait.Time)), as.numeric(min(fus_Nov$Wait.Time)), as.numeric(min(fus_Dec$Wait.Time)))









###### Nob Hill #######
fire_unique <- fire_Nob_Hill %>% distinct(Call.Number, .keep_all = TRUE)

fire_unique <- fire_unique[order(fire_unique$On.Scene.DtTm), ]
row.names(fire_unique) <- 1:nrow(fire_unique)
fire_unique <- fire_unique[822:4645,]

fire_unique <- fire_unique[order(fire_unique$Call.Number), ]
row.names(fire_unique) <- 1:nrow(fire_unique)

fus <- subset(fire_unique, select = c(Call.Number, Call.Date, Received.DtTm, On.Scene.DtTm, Call.Type.Group))

fus$Received.DtTm <- substr(fus$Received.DtTm, 12, 22)
fus$On.Scene.DtTm <- substr(fus$On.Scene.DtTm, 12, 22)


for (i in 1:3824) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "AM" && substr(fus$Received.DtTm[i], 1, 2) == "12"){
    fus$Received.DtTm[i] <- gsub("12", "00", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 10, 11) == "AM" && substr(fus$On.Scene.DtTm[i], 1, 2) == "12"){
    fus$On.Scene.DtTm[i] <- gsub("12", "00", fus$On.Scene.DtTm[i])
  }
}

for (i in 1:3824) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "PM"){
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
    
    fus$Received.DtTm[i] <- as.integer(fus$Received.DtTm[i]) + 120000
    fus$On.Scene.DtTm[i] <- as.integer(fus$On.Scene.DtTm[i]) + 120000
    
  } else{
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
  }
}

for (i in 1:3824) {
  if (substr(fus$Received.DtTm[i], 1, 2) == "24"){
    fus$Received.DtTm[i] <- gsub("24", "12", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 1, 2) == "24"){
    fus$On.Scene.DtTm[i] <- gsub("24", "12", fus$On.Scene.DtTm[i])
  }
}

fus$Received.Hr <- substr(fus$Received.DtTm, 1, 2)
fus$Received.Mn <- substr(fus$Received.DtTm, 3, 4)
fus$Received.Sc <- substr(fus$Received.DtTm, 5, 6)

fus$On.Scene.Hr <- substr(fus$On.Scene.DtTm, 1, 2)
fus$On.Scene.Mn <- substr(fus$On.Scene.DtTm, 3, 4)
fus$On.Scene.Sc <- substr(fus$On.Scene.DtTm, 5, 6)

fus$Received.Time.New <- paste(fus$Received.Hr, fus$Received.Mn, fus$Received.Sc, sep = ":")
fus$On.Scene.Time.New <- paste(fus$On.Scene.Hr, fus$On.Scene.Mn, fus$On.Scene.Sc, sep = ":")

fus$Received.DateTime.New <- paste(fus$Call.Date, fus$Received.Time.New, sep = " ")
fus$On.Scene.DateTime.New <- paste(fus$Call.Date, fus$On.Scene.Time.New, sep = " ")



fus$Received.DtTm <- as.POSIXct(fus$Received.DateTime.New, format="%m/%d/%Y %H:%M:%S")
fus$On.Scene.DtTm <- as.POSIXct(fus$On.Scene.DateTime.New, format="%m/%d/%Y %H:%M:%S")

fus$Wait.Time <- difftime(fus$On.Scene.DtTm, fus$Received.DtTm, units = "mins")


fus <- fus[order(fus$Wait.Time), ]
row.names(fus) <- 1:nrow(fus)

fus <- fus[139:3824,]

fus <- fus[order(fus$Call.Number), ]
row.names(fus) <- 1:nrow(fus)

fus$Call.Month <- substr(fus$Call.Date, 1, 2)

fus_Jan <- fus[1:346,]
fus_Feb <- fus[347:640,]
fus_Mar <- fus[641:1057,]
fus_Apr <- fus[1058:1398,]
fus_May <- fus[1399:1697,]
fus_Jun <- fus[1698:2039,]
fus_Jul <- fus[2040:2323,]
fus_Aug <- fus[2324:2610,]
fus_Sep <- fus[2611:2892,]
fus_Oct <- fus[2893:3185,]
fus_Nov <- fus[3186:3476,]
fus_Dec <- fus[3477:3686,]

mean_fus_Jan <- mean(as.integer(fus_Jan$Wait.Time))
mean_fus_Feb <- mean(as.integer(fus_Feb$Wait.Time))
mean_fus_Mar <- mean(as.integer(fus_Mar$Wait.Time))
mean_fus_Apr <- mean(as.integer(fus_Apr$Wait.Time))
mean_fus_May <- mean(as.integer(fus_May$Wait.Time))
mean_fus_Jun <- mean(as.integer(fus_Jun$Wait.Time))
mean_fus_Jul <- mean(as.integer(fus_Jul$Wait.Time))
mean_fus_Aug <- mean(as.integer(fus_Aug$Wait.Time))
mean_fus_Sep <- mean(as.integer(fus_Sep$Wait.Time))
mean_fus_Oct <- mean(as.integer(fus_Oct$Wait.Time))
mean_fus_Nov <- mean(as.integer(fus_Nov$Wait.Time))
mean_fus_Dec <- mean(as.integer(fus_Dec$Wait.Time))

median_fus_Jan <- median(as.integer(fus_Jan$Wait.Time))
median_fus_Feb <- median(as.integer(fus_Feb$Wait.Time))
median_fus_Mar <- median(as.integer(fus_Mar$Wait.Time))
median_fus_Apr <- median(as.integer(fus_Apr$Wait.Time))
median_fus_May <- median(as.integer(fus_May$Wait.Time))
median_fus_Jun <- median(as.integer(fus_Jun$Wait.Time))
median_fus_Jul <- median(as.integer(fus_Jul$Wait.Time))
median_fus_Aug <- median(as.integer(fus_Aug$Wait.Time))
median_fus_Sep <- median(as.integer(fus_Sep$Wait.Time))
median_fus_Oct <- median(as.integer(fus_Oct$Wait.Time))
median_fus_Nov <- median(as.integer(fus_Nov$Wait.Time))
median_fus_Dec <- median(as.integer(fus_Dec$Wait.Time))

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
means_Nob_Hill <- c(mean_fus_Jan, mean_fus_Feb, mean_fus_Mar, mean_fus_Apr, mean_fus_May, mean_fus_Jun, mean_fus_Jul, mean_fus_Aug, mean_fus_Sep, mean_fus_Oct, mean_fus_Nov, mean_fus_Dec)
medians_Nob_Hill <- c(median_fus_Jan, median_fus_Feb, median_fus_Mar, median_fus_Apr, median_fus_May, median_fus_Jun, median_fus_Jul, median_fus_Aug, median_fus_Sep, median_fus_Oct, median_fus_Nov, median_fus_Dec)
counts_Nob_Hill <- c(nrow(fus_Jan), nrow(fus_Feb), nrow(fus_Mar), nrow(fus_Apr), nrow(fus_May), nrow(fus_Jun), nrow(fus_Jul), nrow(fus_Aug), nrow(fus_Sep), nrow(fus_Oct), nrow(fus_Nov), nrow(fus_Dec))
max_wait_time_Nob_Hill <- c(as.numeric(max(fus_Jan$Wait.Time)), as.numeric(max(fus_Feb$Wait.Time)), as.numeric(max(fus_Mar$Wait.Time)), as.numeric(max(fus_Apr$Wait.Time)), as.numeric(max(fus_May$Wait.Time)), as.numeric(max(fus_Jun$Wait.Time)), 
                            as.numeric(max(fus_Jul$Wait.Time)), as.numeric(max(fus_Aug$Wait.Time)), as.numeric(max(fus_Sep$Wait.Time)), as.numeric(max(fus_Oct$Wait.Time)), as.numeric(max(fus_Nov$Wait.Time)), as.numeric(max(fus_Dec$Wait.Time)))
min_wait_time_Nob_Hill <- c(as.numeric(min(fus_Jan$Wait.Time)), as.numeric(min(fus_Feb$Wait.Time)), as.numeric(min(fus_Mar$Wait.Time)), as.numeric(min(fus_Apr$Wait.Time)), as.numeric(min(fus_May$Wait.Time)), as.numeric(min(fus_Jun$Wait.Time)), 
                            as.numeric(min(fus_Jul$Wait.Time)), as.numeric(min(fus_Aug$Wait.Time)), as.numeric(min(fus_Sep$Wait.Time)), as.numeric(min(fus_Oct$Wait.Time)), as.numeric(min(fus_Nov$Wait.Time)), as.numeric(min(fus_Dec$Wait.Time)))










###### North Beach #######
fire_unique <- fire_North_Beach %>% distinct(Call.Number, .keep_all = TRUE)

fire_unique <- fire_unique[order(fire_unique$On.Scene.DtTm), ]
row.names(fire_unique) <- 1:nrow(fire_unique)
fire_unique <- fire_unique[432:2957,]

fire_unique <- fire_unique[order(fire_unique$Call.Number), ]
row.names(fire_unique) <- 1:nrow(fire_unique)

fus <- subset(fire_unique, select = c(Call.Number, Call.Date, Received.DtTm, On.Scene.DtTm, Call.Type.Group))

fus$Received.DtTm <- substr(fus$Received.DtTm, 12, 22)
fus$On.Scene.DtTm <- substr(fus$On.Scene.DtTm, 12, 22)


for (i in 1:2526) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "AM" && substr(fus$Received.DtTm[i], 1, 2) == "12"){
    fus$Received.DtTm[i] <- gsub("12", "00", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 10, 11) == "AM" && substr(fus$On.Scene.DtTm[i], 1, 2) == "12"){
    fus$On.Scene.DtTm[i] <- gsub("12", "00", fus$On.Scene.DtTm[i])
  }
}

for (i in 1:2526) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "PM"){
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
    
    fus$Received.DtTm[i] <- as.integer(fus$Received.DtTm[i]) + 120000
    fus$On.Scene.DtTm[i] <- as.integer(fus$On.Scene.DtTm[i]) + 120000
    
  } else{
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
  }
}

for (i in 1:2526) {
  if (substr(fus$Received.DtTm[i], 1, 2) == "24"){
    fus$Received.DtTm[i] <- gsub("24", "12", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 1, 2) == "24"){
    fus$On.Scene.DtTm[i] <- gsub("24", "12", fus$On.Scene.DtTm[i])
  }
}

fus$Received.Hr <- substr(fus$Received.DtTm, 1, 2)
fus$Received.Mn <- substr(fus$Received.DtTm, 3, 4)
fus$Received.Sc <- substr(fus$Received.DtTm, 5, 6)

fus$On.Scene.Hr <- substr(fus$On.Scene.DtTm, 1, 2)
fus$On.Scene.Mn <- substr(fus$On.Scene.DtTm, 3, 4)
fus$On.Scene.Sc <- substr(fus$On.Scene.DtTm, 5, 6)

fus$Received.Time.New <- paste(fus$Received.Hr, fus$Received.Mn, fus$Received.Sc, sep = ":")
fus$On.Scene.Time.New <- paste(fus$On.Scene.Hr, fus$On.Scene.Mn, fus$On.Scene.Sc, sep = ":")

fus$Received.DateTime.New <- paste(fus$Call.Date, fus$Received.Time.New, sep = " ")
fus$On.Scene.DateTime.New <- paste(fus$Call.Date, fus$On.Scene.Time.New, sep = " ")



fus$Received.DtTm <- as.POSIXct(fus$Received.DateTime.New, format="%m/%d/%Y %H:%M:%S")
fus$On.Scene.DtTm <- as.POSIXct(fus$On.Scene.DateTime.New, format="%m/%d/%Y %H:%M:%S")

fus$Wait.Time <- difftime(fus$On.Scene.DtTm, fus$Received.DtTm, units = "mins")


fus <- fus[order(fus$Wait.Time), ]
row.names(fus) <- 1:nrow(fus)

fus <- fus[56:2526,]

fus <- fus[order(fus$Call.Number), ]
row.names(fus) <- 1:nrow(fus)

fus$Call.Month <- substr(fus$Call.Date, 1, 2)

fus_Jan <- fus[1:195,]
fus_Feb <- fus[196:396,]
fus_Mar <- fus[397:624,]
fus_Apr <- fus[625:812,]
fus_May <- fus[813:1021,]
fus_Jun <- fus[1022:1239,]
fus_Jul <- fus[1240:1438,]
fus_Aug <- fus[1439:1655,]
fus_Sep <- fus[1656:1845,]
fus_Oct <- fus[1846:2070,]
fus_Nov <- fus[2071:2279,]
fus_Dec <- fus[2280:2471,]

mean_fus_Jan <- mean(as.integer(fus_Jan$Wait.Time))
mean_fus_Feb <- mean(as.integer(fus_Feb$Wait.Time))
mean_fus_Mar <- mean(as.integer(fus_Mar$Wait.Time))
mean_fus_Apr <- mean(as.integer(fus_Apr$Wait.Time))
mean_fus_May <- mean(as.integer(fus_May$Wait.Time))
mean_fus_Jun <- mean(as.integer(fus_Jun$Wait.Time))
mean_fus_Jul <- mean(as.integer(fus_Jul$Wait.Time))
mean_fus_Aug <- mean(as.integer(fus_Aug$Wait.Time))
mean_fus_Sep <- mean(as.integer(fus_Sep$Wait.Time))
mean_fus_Oct <- mean(as.integer(fus_Oct$Wait.Time))
mean_fus_Nov <- mean(as.integer(fus_Nov$Wait.Time))
mean_fus_Dec <- mean(as.integer(fus_Dec$Wait.Time))

median_fus_Jan <- median(as.integer(fus_Jan$Wait.Time))
median_fus_Feb <- median(as.integer(fus_Feb$Wait.Time))
median_fus_Mar <- median(as.integer(fus_Mar$Wait.Time))
median_fus_Apr <- median(as.integer(fus_Apr$Wait.Time))
median_fus_May <- median(as.integer(fus_May$Wait.Time))
median_fus_Jun <- median(as.integer(fus_Jun$Wait.Time))
median_fus_Jul <- median(as.integer(fus_Jul$Wait.Time))
median_fus_Aug <- median(as.integer(fus_Aug$Wait.Time))
median_fus_Sep <- median(as.integer(fus_Sep$Wait.Time))
median_fus_Oct <- median(as.integer(fus_Oct$Wait.Time))
median_fus_Nov <- median(as.integer(fus_Nov$Wait.Time))
median_fus_Dec <- median(as.integer(fus_Dec$Wait.Time))

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
means_North_Beach <- c(mean_fus_Jan, mean_fus_Feb, mean_fus_Mar, mean_fus_Apr, mean_fus_May, mean_fus_Jun, mean_fus_Jul, mean_fus_Aug, mean_fus_Sep, mean_fus_Oct, mean_fus_Nov, mean_fus_Dec)
medians_North_Beach <- c(median_fus_Jan, median_fus_Feb, median_fus_Mar, median_fus_Apr, median_fus_May, median_fus_Jun, median_fus_Jul, median_fus_Aug, median_fus_Sep, median_fus_Oct, median_fus_Nov, median_fus_Dec)
counts_North_Beach <- c(nrow(fus_Jan), nrow(fus_Feb), nrow(fus_Mar), nrow(fus_Apr), nrow(fus_May), nrow(fus_Jun), nrow(fus_Jul), nrow(fus_Aug), nrow(fus_Sep), nrow(fus_Oct), nrow(fus_Nov), nrow(fus_Dec))
max_wait_time_North_Beach <- c(as.numeric(max(fus_Jan$Wait.Time)), as.numeric(max(fus_Feb$Wait.Time)), as.numeric(max(fus_Mar$Wait.Time)), as.numeric(max(fus_Apr$Wait.Time)), as.numeric(max(fus_May$Wait.Time)), as.numeric(max(fus_Jun$Wait.Time)), 
                             as.numeric(max(fus_Jul$Wait.Time)), as.numeric(max(fus_Aug$Wait.Time)), as.numeric(max(fus_Sep$Wait.Time)), as.numeric(max(fus_Oct$Wait.Time)), as.numeric(max(fus_Nov$Wait.Time)), as.numeric(max(fus_Dec$Wait.Time)))
min_wait_time_North_Beach <- c(as.numeric(min(fus_Jan$Wait.Time)), as.numeric(min(fus_Feb$Wait.Time)), as.numeric(min(fus_Mar$Wait.Time)), as.numeric(min(fus_Apr$Wait.Time)), as.numeric(min(fus_May$Wait.Time)), as.numeric(min(fus_Jun$Wait.Time)), 
                             as.numeric(min(fus_Jul$Wait.Time)), as.numeric(min(fus_Aug$Wait.Time)), as.numeric(min(fus_Sep$Wait.Time)), as.numeric(min(fus_Oct$Wait.Time)), as.numeric(min(fus_Nov$Wait.Time)), as.numeric(min(fus_Dec$Wait.Time)))











###### Russian Hill #######
fire_unique <- fire_Russian_Hill %>% distinct(Call.Number, .keep_all = TRUE)

fire_unique <- fire_unique[order(fire_unique$On.Scene.DtTm), ]
row.names(fire_unique) <- 1:nrow(fire_unique)
fire_unique <- fire_unique[354:1954,]

fire_unique <- fire_unique[order(fire_unique$Call.Number), ]
row.names(fire_unique) <- 1:nrow(fire_unique)

fus <- subset(fire_unique, select = c(Call.Number, Call.Date, Received.DtTm, On.Scene.DtTm, Call.Type.Group))

fus$Received.DtTm <- substr(fus$Received.DtTm, 12, 22)
fus$On.Scene.DtTm <- substr(fus$On.Scene.DtTm, 12, 22)


for (i in 1:1601) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "AM" && substr(fus$Received.DtTm[i], 1, 2) == "12"){
    fus$Received.DtTm[i] <- gsub("12", "00", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 10, 11) == "AM" && substr(fus$On.Scene.DtTm[i], 1, 2) == "12"){
    fus$On.Scene.DtTm[i] <- gsub("12", "00", fus$On.Scene.DtTm[i])
  }
}

for (i in 1:1601) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "PM"){
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
    
    fus$Received.DtTm[i] <- as.integer(fus$Received.DtTm[i]) + 120000
    fus$On.Scene.DtTm[i] <- as.integer(fus$On.Scene.DtTm[i]) + 120000
    
  } else{
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
  }
}

for (i in 1:1601) {
  if (substr(fus$Received.DtTm[i], 1, 2) == "24"){
    fus$Received.DtTm[i] <- gsub("24", "12", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 1, 2) == "24"){
    fus$On.Scene.DtTm[i] <- gsub("24", "12", fus$On.Scene.DtTm[i])
  }
}

fus$Received.Hr <- substr(fus$Received.DtTm, 1, 2)
fus$Received.Mn <- substr(fus$Received.DtTm, 3, 4)
fus$Received.Sc <- substr(fus$Received.DtTm, 5, 6)

fus$On.Scene.Hr <- substr(fus$On.Scene.DtTm, 1, 2)
fus$On.Scene.Mn <- substr(fus$On.Scene.DtTm, 3, 4)
fus$On.Scene.Sc <- substr(fus$On.Scene.DtTm, 5, 6)

fus$Received.Time.New <- paste(fus$Received.Hr, fus$Received.Mn, fus$Received.Sc, sep = ":")
fus$On.Scene.Time.New <- paste(fus$On.Scene.Hr, fus$On.Scene.Mn, fus$On.Scene.Sc, sep = ":")

fus$Received.DateTime.New <- paste(fus$Call.Date, fus$Received.Time.New, sep = " ")
fus$On.Scene.DateTime.New <- paste(fus$Call.Date, fus$On.Scene.Time.New, sep = " ")



fus$Received.DtTm <- as.POSIXct(fus$Received.DateTime.New, format="%m/%d/%Y %H:%M:%S")
fus$On.Scene.DtTm <- as.POSIXct(fus$On.Scene.DateTime.New, format="%m/%d/%Y %H:%M:%S")

fus$Wait.Time <- difftime(fus$On.Scene.DtTm, fus$Received.DtTm, units = "mins")


fus <- fus[order(fus$Wait.Time), ]
row.names(fus) <- 1:nrow(fus)

fus <- fus[49:1601,]

fus <- fus[order(fus$Call.Number), ]
row.names(fus) <- 1:nrow(fus)

fus$Call.Month <- substr(fus$Call.Date, 1, 2)

fus_Jan <- fus[1:122,]
fus_Feb <- fus[123:247,]
fus_Mar <- fus[248:369,]
fus_Apr <- fus[370:520,]
fus_May <- fus[521:659,]
fus_Jun <- fus[660:761,]
fus_Jul <- fus[762:915,]
fus_Aug <- fus[916:1032,]
fus_Sep <- fus[1033:1178,]
fus_Oct <- fus[1179:1287,]
fus_Nov <- fus[1288:1419,]
fus_Dec <- fus[1420:1553,]

mean_fus_Jan <- mean(as.integer(fus_Jan$Wait.Time))
mean_fus_Feb <- mean(as.integer(fus_Feb$Wait.Time))
mean_fus_Mar <- mean(as.integer(fus_Mar$Wait.Time))
mean_fus_Apr <- mean(as.integer(fus_Apr$Wait.Time))
mean_fus_May <- mean(as.integer(fus_May$Wait.Time))
mean_fus_Jun <- mean(as.integer(fus_Jun$Wait.Time))
mean_fus_Jul <- mean(as.integer(fus_Jul$Wait.Time))
mean_fus_Aug <- mean(as.integer(fus_Aug$Wait.Time))
mean_fus_Sep <- mean(as.integer(fus_Sep$Wait.Time))
mean_fus_Oct <- mean(as.integer(fus_Oct$Wait.Time))
mean_fus_Nov <- mean(as.integer(fus_Nov$Wait.Time))
mean_fus_Dec <- mean(as.integer(fus_Dec$Wait.Time))

median_fus_Jan <- median(as.integer(fus_Jan$Wait.Time))
median_fus_Feb <- median(as.integer(fus_Feb$Wait.Time))
median_fus_Mar <- median(as.integer(fus_Mar$Wait.Time))
median_fus_Apr <- median(as.integer(fus_Apr$Wait.Time))
median_fus_May <- median(as.integer(fus_May$Wait.Time))
median_fus_Jun <- median(as.integer(fus_Jun$Wait.Time))
median_fus_Jul <- median(as.integer(fus_Jul$Wait.Time))
median_fus_Aug <- median(as.integer(fus_Aug$Wait.Time))
median_fus_Sep <- median(as.integer(fus_Sep$Wait.Time))
median_fus_Oct <- median(as.integer(fus_Oct$Wait.Time))
median_fus_Nov <- median(as.integer(fus_Nov$Wait.Time))
median_fus_Dec <- median(as.integer(fus_Dec$Wait.Time))

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
means_Russian_Hill <- c(mean_fus_Jan, mean_fus_Feb, mean_fus_Mar, mean_fus_Apr, mean_fus_May, mean_fus_Jun, mean_fus_Jul, mean_fus_Aug, mean_fus_Sep, mean_fus_Oct, mean_fus_Nov, mean_fus_Dec)
medians_Russian_Hill <- c(median_fus_Jan, median_fus_Feb, median_fus_Mar, median_fus_Apr, median_fus_May, median_fus_Jun, median_fus_Jul, median_fus_Aug, median_fus_Sep, median_fus_Oct, median_fus_Nov, median_fus_Dec)
counts_Russian_Hill <- c(nrow(fus_Jan), nrow(fus_Feb), nrow(fus_Mar), nrow(fus_Apr), nrow(fus_May), nrow(fus_Jun), nrow(fus_Jul), nrow(fus_Aug), nrow(fus_Sep), nrow(fus_Oct), nrow(fus_Nov), nrow(fus_Dec))
max_wait_time_Russian_Hill <- c(as.numeric(max(fus_Jan$Wait.Time)), as.numeric(max(fus_Feb$Wait.Time)), as.numeric(max(fus_Mar$Wait.Time)), as.numeric(max(fus_Apr$Wait.Time)), as.numeric(max(fus_May$Wait.Time)), as.numeric(max(fus_Jun$Wait.Time)), 
                             as.numeric(max(fus_Jul$Wait.Time)), as.numeric(max(fus_Aug$Wait.Time)), as.numeric(max(fus_Sep$Wait.Time)), as.numeric(max(fus_Oct$Wait.Time)), as.numeric(max(fus_Nov$Wait.Time)), as.numeric(max(fus_Dec$Wait.Time)))
min_wait_time_Russian_Hill <- c(as.numeric(min(fus_Jan$Wait.Time)), as.numeric(min(fus_Feb$Wait.Time)), as.numeric(min(fus_Mar$Wait.Time)), as.numeric(min(fus_Apr$Wait.Time)), as.numeric(min(fus_May$Wait.Time)), as.numeric(min(fus_Jun$Wait.Time)), 
                             as.numeric(min(fus_Jul$Wait.Time)), as.numeric(min(fus_Aug$Wait.Time)), as.numeric(min(fus_Sep$Wait.Time)), as.numeric(min(fus_Oct$Wait.Time)), as.numeric(min(fus_Nov$Wait.Time)), as.numeric(min(fus_Dec$Wait.Time)))










###### Tenderloin #######
fire_unique <- fire_Tenderloin %>% distinct(Call.Number, .keep_all = TRUE)

fire_unique <- fire_unique[order(fire_unique$On.Scene.DtTm), ]
row.names(fire_unique) <- 1:nrow(fire_unique)
fire_unique <- fire_unique[3893:21821,]

fire_unique <- fire_unique[order(fire_unique$Call.Number), ]
row.names(fire_unique) <- 1:nrow(fire_unique)

fus <- subset(fire_unique, select = c(Call.Number, Call.Date, Received.DtTm, On.Scene.DtTm, Call.Type.Group))

fus$Received.DtTm <- substr(fus$Received.DtTm, 12, 22)
fus$On.Scene.DtTm <- substr(fus$On.Scene.DtTm, 12, 22)


for (i in 1:17929) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "AM" && substr(fus$Received.DtTm[i], 1, 2) == "12"){
    fus$Received.DtTm[i] <- gsub("12", "00", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 10, 11) == "AM" && substr(fus$On.Scene.DtTm[i], 1, 2) == "12"){
    fus$On.Scene.DtTm[i] <- gsub("12", "00", fus$On.Scene.DtTm[i])
  }
}

for (i in 1:17929) {
  if (substr(fus$Received.DtTm[i], 10, 11) == "PM"){
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
    
    fus$Received.DtTm[i] <- as.integer(fus$Received.DtTm[i]) + 120000
    fus$On.Scene.DtTm[i] <- as.integer(fus$On.Scene.DtTm[i]) + 120000
    
  } else{
    fus$Received.DtTm[i] <- substr(fus$Received.DtTm[i], 1, 8)
    fus$On.Scene.DtTm[i] <- substr(fus$On.Scene.DtTm[i], 1, 8)
    
    fus$Received.DtTm[i] <- paste(substr(fus$Received.DtTm[i], 1, 2), substr(fus$Received.DtTm[i], 4, 5), substr(fus$Received.DtTm[i], 7, 8), sep = "")
    fus$On.Scene.DtTm[i] <- paste(substr(fus$On.Scene.DtTm[i], 1, 2), substr(fus$On.Scene.DtTm[i], 4, 5), substr(fus$On.Scene.DtTm[i], 7, 8), sep = "")
  }
}

for (i in 1:17929) {
  if (substr(fus$Received.DtTm[i], 1, 2) == "24"){
    fus$Received.DtTm[i] <- gsub("24", "12", fus$Received.DtTm[i])
  }
  
  if (substr(fus$On.Scene.DtTm[i], 1, 2) == "24"){
    fus$On.Scene.DtTm[i] <- gsub("24", "12", fus$On.Scene.DtTm[i])
  }
}

fus$Received.Hr <- substr(fus$Received.DtTm, 1, 2)
fus$Received.Mn <- substr(fus$Received.DtTm, 3, 4)
fus$Received.Sc <- substr(fus$Received.DtTm, 5, 6)

fus$On.Scene.Hr <- substr(fus$On.Scene.DtTm, 1, 2)
fus$On.Scene.Mn <- substr(fus$On.Scene.DtTm, 3, 4)
fus$On.Scene.Sc <- substr(fus$On.Scene.DtTm, 5, 6)

fus$Received.Time.New <- paste(fus$Received.Hr, fus$Received.Mn, fus$Received.Sc, sep = ":")
fus$On.Scene.Time.New <- paste(fus$On.Scene.Hr, fus$On.Scene.Mn, fus$On.Scene.Sc, sep = ":")

fus$Received.DateTime.New <- paste(fus$Call.Date, fus$Received.Time.New, sep = " ")
fus$On.Scene.DateTime.New <- paste(fus$Call.Date, fus$On.Scene.Time.New, sep = " ")



fus$Received.DtTm <- as.POSIXct(fus$Received.DateTime.New, format="%m/%d/%Y %H:%M:%S")
fus$On.Scene.DtTm <- as.POSIXct(fus$On.Scene.DateTime.New, format="%m/%d/%Y %H:%M:%S")

fus$Wait.Time <- difftime(fus$On.Scene.DtTm, fus$Received.DtTm, units = "mins")


fus <- fus[order(fus$Wait.Time), ]
row.names(fus) <- 1:nrow(fus)

fus <- fus[376:17929,]

fus <- fus[order(fus$Call.Number), ]
row.names(fus) <- 1:nrow(fus)

fus$Call.Month <- substr(fus$Call.Date, 1, 2)

fus_Jan <- fus[1:1507,]
fus_Feb <- fus[1508:2957,]
fus_Mar <- fus[2958:4401,]
fus_Apr <- fus[4402:5782,]
fus_May <- fus[5783:7210,]
fus_Jun <- fus[7211:8616,]
fus_Jul <- fus[8617:10090,]
fus_Aug <- fus[10091:11627,]
fus_Sep <- fus[11628:13009,]
fus_Oct <- fus[13010:14474,]
fus_Nov <- fus[14475:15942,]
fus_Dec <- fus[15943:17554,]

mean_fus_Jan <- mean(as.integer(fus_Jan$Wait.Time))
mean_fus_Feb <- mean(as.integer(fus_Feb$Wait.Time))
mean_fus_Mar <- mean(as.integer(fus_Mar$Wait.Time))
mean_fus_Apr <- mean(as.integer(fus_Apr$Wait.Time))
mean_fus_May <- mean(as.integer(fus_May$Wait.Time))
mean_fus_Jun <- mean(as.integer(fus_Jun$Wait.Time))
mean_fus_Jul <- mean(as.integer(fus_Jul$Wait.Time))
mean_fus_Aug <- mean(as.integer(fus_Aug$Wait.Time))
mean_fus_Sep <- mean(as.integer(fus_Sep$Wait.Time))
mean_fus_Oct <- mean(as.integer(fus_Oct$Wait.Time))
mean_fus_Nov <- mean(as.integer(fus_Nov$Wait.Time))
mean_fus_Dec <- mean(as.integer(fus_Dec$Wait.Time))

median_fus_Jan <- median(as.integer(fus_Jan$Wait.Time))
median_fus_Feb <- median(as.integer(fus_Feb$Wait.Time))
median_fus_Mar <- median(as.integer(fus_Mar$Wait.Time))
median_fus_Apr <- median(as.integer(fus_Apr$Wait.Time))
median_fus_May <- median(as.integer(fus_May$Wait.Time))
median_fus_Jun <- median(as.integer(fus_Jun$Wait.Time))
median_fus_Jul <- median(as.integer(fus_Jul$Wait.Time))
median_fus_Aug <- median(as.integer(fus_Aug$Wait.Time))
median_fus_Sep <- median(as.integer(fus_Sep$Wait.Time))
median_fus_Oct <- median(as.integer(fus_Oct$Wait.Time))
median_fus_Nov <- median(as.integer(fus_Nov$Wait.Time))
median_fus_Dec <- median(as.integer(fus_Dec$Wait.Time))

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
means_Tenderloin <- c(mean_fus_Jan, mean_fus_Feb, mean_fus_Mar, mean_fus_Apr, mean_fus_May, mean_fus_Jun, mean_fus_Jul, mean_fus_Aug, mean_fus_Sep, mean_fus_Oct, mean_fus_Nov, mean_fus_Dec)
medians_Tenderloin <- c(median_fus_Jan, median_fus_Feb, median_fus_Mar, median_fus_Apr, median_fus_May, median_fus_Jun, median_fus_Jul, median_fus_Aug, median_fus_Sep, median_fus_Oct, median_fus_Nov, median_fus_Dec)
counts_Tenderloin <- c(nrow(fus_Jan), nrow(fus_Feb), nrow(fus_Mar), nrow(fus_Apr), nrow(fus_May), nrow(fus_Jun), nrow(fus_Jul), nrow(fus_Aug), nrow(fus_Sep), nrow(fus_Oct), nrow(fus_Nov), nrow(fus_Dec))
max_wait_time_Tenderloin <- c(as.numeric(max(fus_Jan$Wait.Time)), as.numeric(max(fus_Feb$Wait.Time)), as.numeric(max(fus_Mar$Wait.Time)), as.numeric(max(fus_Apr$Wait.Time)), as.numeric(max(fus_May$Wait.Time)), as.numeric(max(fus_Jun$Wait.Time)), 
                             as.numeric(max(fus_Jul$Wait.Time)), as.numeric(max(fus_Aug$Wait.Time)), as.numeric(max(fus_Sep$Wait.Time)), as.numeric(max(fus_Oct$Wait.Time)), as.numeric(max(fus_Nov$Wait.Time)), as.numeric(max(fus_Dec$Wait.Time)))
min_wait_time_Tenderloin <- c(as.numeric(min(fus_Jan$Wait.Time)), as.numeric(min(fus_Feb$Wait.Time)), as.numeric(min(fus_Mar$Wait.Time)), as.numeric(min(fus_Apr$Wait.Time)), as.numeric(min(fus_May$Wait.Time)), as.numeric(min(fus_Jun$Wait.Time)), 
                             as.numeric(min(fus_Jul$Wait.Time)), as.numeric(min(fus_Aug$Wait.Time)), as.numeric(min(fus_Sep$Wait.Time)), as.numeric(min(fus_Oct$Wait.Time)), as.numeric(min(fus_Nov$Wait.Time)), as.numeric(min(fus_Dec$Wait.Time)))





finalMatrix <- cbind(months, means_Chinatown, medians_Chinatown, counts_Chinatown, max_wait_time_Chinatown, min_wait_time_Chinatown,
                     means_USF, medians_USF, counts_USF, max_wait_time_USF, min_wait_time_USF,
                     means_Nob_Hill, medians_Nob_Hill, counts_Nob_Hill, max_wait_time_Nob_Hill, min_wait_time_Nob_Hill,
                     means_North_Beach, medians_North_Beach, counts_North_Beach, max_wait_time_North_Beach, min_wait_time_North_Beach,
                     means_Russian_Hill, medians_Russian_Hill, counts_Russian_Hill, max_wait_time_Russian_Hill, min_wait_time_Russian_Hill,
                     means_Tenderloin, medians_Tenderloin, counts_Tenderloin, max_wait_time_Tenderloin, min_wait_time_Tenderloin)


write.csv(finalMatrix, "midterm_fire_data.csv", row.names = FALSE)

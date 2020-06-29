library(lubridate)
library(plyr)
library(dplyr)

customers <- read.csv("customers.csv")
customers[,2] <- customers[,2] + 19110000
customers[,2] <- as.character(customers[,2])
customers[,2] <- as.Date(customers[,2], format = "%Y%m%d")
customers[,10] <- 2011 - year(customers[,2])
customers <- customers[,-11]
customers[which(customers[,10] <30),11] <- "<30"
customers[which(customers[,10] >=30 & customers[,10] <40),11] <- "30~39"
customers[which(customers[,10] >=40 & customers[,10] <50),11] <- "40~49"
customers[which(customers[,10] >=50 & customers[,10] <60),11] <- "50~59"
customers[which(customers[,10] >=60),11] <- ">=60"
customers[,11] <- factor(customers[,11])
customers[which(customers[,11] == "<30"),12] <- 1
customers[which(customers[,11] == "30~39"),12] <- 2
customers[which(customers[,11] == "40~49"),12] <- 3
customers[which(customers[,11] == "50~59"),12] <- 4
customers[which(customers[,11] == ">=60"),12] <- 5
customers[,12] <- factor(customers[,12])
customers <- customers[,c(1:2,10:12)]
trans <- read.csv("transactions.csv")
trans <- trans[order(trans[,2]),]
trans[,4] <- as.character(trans[,4])
trans[,4] <- as.Date(trans[,4], format = "%y%m%d")
trans[,4] <- trans[,4] %m+% years(11)
trans <- trans[!duplicated(trans[,1]),]
trans <- trans[,-c(3,5:10)]
trans <- trans[,1:4]

for (i in 1:562){
  trans[i,5] <- customers[which(customers[,1] == trans[i,2]),5]
}
for (i in 1:5){
  seg[i,3] <- round(mean(trans[which(trans[,5] == i),4]), digits = 0)
  seg[i,4] <- sd(trans[which(trans[,5] == i),4])^2
  seg[i,5] <- sd(trans[which(trans[,5] == i),4])
}

for (i in 1:200){
  customers[i,6] <- length(which(trans[,2] == customers[i,1]))
  customers[i,7] <- sum(trans[which(trans[,2] == customers[i,1]),4])
}

customers[,8] <- round(customers[,7]/customers[,6],digits = 0)
cc <- customers[which(customers[,6] >= 3),]

for (i in 1:62){
  cc[i,9] <- round(sd(trans[which(trans[,2] == cc[i,1]),4])^2, digits = 2)
  cc[i,10] <- round(sd(trans[which(trans[,2] == cc[i,1]),4]), digits = 2)
}

de_tr <- trans[which(trans[,2] %in% cc[,1]),]
seg <- data.frame("SEGMENT" = 1:5)
for (i in 1:5){
  seg[i,2] <- length(which(cc[,5] == i))
  seg[i,3] <- round(mean(de_tr[which(de_tr[,5] == i),4]),digits = 0)
  seg[i,4] <- round(sd(de_tr[which(de_tr[,5] == i),4])^2, digits = 2)
  seg[i,5] <- round(sd(de_tr[which(de_tr[,5] == i),4]), digits = 2)
}

for (i in 1:62){
  cc[i,11] <- seg[which(seg[,1] == cc[i,5]),3]
  cc[i,12] <- seg[which(seg[,1] == cc[i,5]),4]
  cc[i,13] <- seg[which(seg[,1] == cc[i,5]),5]
}
for (i in 1:62){
  cc[i,14] <- round(cc[i,12]/(cc[i,12] + (cc[i,9]/cc[i,6])), digits = 2)
  cc[i,15] <- round((cc[i,9]/cc[i,6])/(cc[i,12] + (cc[i,9]/cc[i,6])), digits = 2)
}
for (i in 1:62){
  cc[i,16] <- round(cc[i,14]*cc[i,8] + cc[i,15]*cc[i,11], digits = 0)
}

cc_1 <- cc[,c(1,5,6,8,9,11,12,14:16)]
for (i in 1:62){
  cc_1[i,11] <- round((cc_1[i,4]-cc_1[i,10])/(cc_1[i,4]-cc_1[i,6]), digits = 2)
}
library(openxlsx)
colnames(cc_1) <- c("ID", "Days", "Times ", "AVG Amount", "MLE", "WMLE", "CAI", "CRI")
write.xlsx(cc_1, "CRI.xlsx")

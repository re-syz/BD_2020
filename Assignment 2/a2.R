customers <- read.csv("customers.csv")
trans <- read.csv("transactions.csv")
trans <- trans[order(trans[,2]),]
trans[,4] <- as.character(trans[,4])
trans[,4] <- as.Date(trans[,4], format = "%y%m%d")
trans <- trans[!duplicated(trans[,1]),]
trans <- trans[,-c(3,5:10)]
trans <- trans[,1:4]
end_date <- as.Date("000101", format = "%y%m%d")
customers <- customers[order(customers[,1]),]
for (i in 1:561){
  if (trans[i + 1, 2] != trans[i,2]){
    customers[which(customers[,1] == trans[i,2]), 10] <- end_date - trans[i,3]
  }
}
customers[which(customers[,1] == trans[562,2]), 10] <- end_date - trans[562,3]
customers <- customers[,c(1,10)]
for (i in 1:200){
  customers[i,3] <- length(which(trans[,2] == customers[i,1]))
}
for (i in 1:200){
  customers[i,4] <- sum(trans[which(trans[,2] == customers[i,1]), 4])/customers[i,3]
}

customers[,4] <- round(customers[,4], digits = 0)
colnames(customers) <- c("ID", "Days", "Times ", "AVG Amount")
customers[,2] <- as.integer(customers[,2])
customers[,5] <- round((9-customers[,2]/100), digits = 0)
customers[,6] <- round(1.2*customers[,3], digits = 0)
customers[,7] <- round(customers[,4]/68, digits = 0)
customers[,8] <- customers[,5] + customers[,6] + customers[,7]
customers <- customers[order(-customers[,8]),]
customers[,9] <- c(1:200)
customers <- customers[order(customers[,2]),]
for (i in 1:40){
  customers[i,10] <- 5
}
for (i in 41:80){
  customers[i,10] <- 4
}
for (i in 81:120){
  customers[i,10] <- 3
}
for (i in 121:160){
  customers[i,10] <- 2
}
for (i in 161:200){
  customers[i,10] <- 1
}
customers <- customers[order(customers[,3]),]
for (i in 1:40){
  customers[i,11] <- 1
}
for (i in 41:80){
  customers[i,11] <- 2
}
for (i in 81:120){
  customers[i,11] <- 3
}
for (i in 121:160){
  customers[i,11] <- 4
}
for (i in 161:200){
  customers[i,11] <- 5
}
customers <- customers[order(customers[,4]),]
for (i in 1:40){
  customers[i,12] <- 1
}
for (i in 41:80){
  customers[i,12] <- 2
}
for (i in 81:120){
  customers[i,12] <- 3
}
for (i in 121:160){
  customers[i,12] <- 4
}
for (i in 161:200){
  customers[i,12] <- 5
}
customers[,13] <- customers[,10] + customers[,11] + customers[,12]
customers <- customers[order(-customers[,13]),]
customers[,14] <- 1:200
customers <- customers[order(customers[,1]),]
library(openxlsx)

t_sum <- 0

for (i in 1:200){
  t_sum <- 0
  if (customers[i, 3] != 1){
    for (j in 1:553){
      if (trans[j,2] == customers[i,1]){
        if (trans[j,2] == trans[j+1,2]){
          t_sum <- t_sum + (trans[j+1,3] - trans[j,3])
        }
        customers[i,15] <- round(t_sum/(customers[i,3] -1), digits = 0)
      }
    }
  } else{
    customers[i,15] <- NA
  }
}

t_w_sum <- 0
for (i in 1:200){
  t_w_sum <- 0
  if (customers[i, 3] != 1){
    for (j in 1:553){
      if (trans[j,2] == customers[i,1]){
        for (k in 1:(customers[i,3]-1)){
          t_w_sum <- t_w_sum + (trans[j+k,3] - trans[j+k-1,3])*k
        }
        customers[i,16] <- round(t_w_sum/sum(1:(customers[i,3]-1)), digits = 0)
        break
      }
    }
  } else{
    customers[i,16] <- NA
  }
}

for (i in 1:200){
  if (customers[i,3] != 1){
    customers[i,17] <- round(as.integer((customers[i,15]-customers[i,16]))/as.integer(customers[i,15]), digits = 4)
  }else{
    customers[i,17] <- NA
  }
}

for (i in 1:200){
  if (customers[i,3] != 1){
    if (customers[i,17] > 0){
      customers[i,18] <- "ACTIVE"
    } else if (customers[i,17] < 0){
      customers[i,18] <- "INACTIVE"
    } else if (customers[i,17] == 0){
      customers[i,18] <- "NEUTRAL"
    }
  }else{
    customers[i,18] <- NA
  }
}

customers <- customers[,c(1:4,15:18)]
colnames(customers) <- c("ID", "Days", "Times ", "AVG Amount", "MLE", "WMLE", "CAI", "STATUS")
library(openxlsx)
write.xlsx(customers, "CAI.xlsx")


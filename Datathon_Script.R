library(varhandle)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
data = read.csv("D:/xuexi/dasi(2)/datathon/DDCF.csv",header = T)
head(data)
dim(data)
plot(data$readmitted)
plot(data$A1Cresult)
na.omit(data)

# all the medicines that we have
medcines = colnames(data[,seq(15,36)])

# factor up down and steady to numeric
data[,seq(15,36)] = lapply(data[,seq(15,36)], factor, levels =c("No","Down","Up", "Steady"))
data[,seq(15,36)] = lapply(data[,seq(15,36)], as.numeric)
data[,c(3,4,5)] = lapply(data[,c(3,4,5)], factor)

# count number of 1, if = 22, use no medicine.
data$used = rowSums(data[,seq(15,36)])
data$used = rowSums(data[,seq(15,36)])
# filter out data with no extra medicine at all
data = data %>% filter(data$used != 22)
data = data[, seq(1,37)]


# number of medicine used
med = data[,seq(15,36)]
NumMed <- rowSums(med!= 1)
data = cbind(data, NumMed)

# remove those with only Insulin or Metformin
data[,"metformin"] = ifelse(data[,"metformin"] == 1, 0,1)
data[,"insulin"] = ifelse(data[,"insulin"] == 1, 0,1)
data = data %>% filter(
(data$NumMed - data[,"insulin"] - data[,"metformin"] != 0) & (data$NumMed - data[,"insulin"] != 0) &
  (data$NumMed - data[,"metformin"] != 0)
  ) 

data = data[, !(colnames(data) %in% c("metformin", "insulin"))]

data$readmitted = factor(data$readmitted, labels = c('<30','NO','>30'))

data[,seq(15,34)] = lapply(data[,seq(15,34)], factor, levels=c(1,2,3,4))
#########clean data ends##############

###############Prepare dummy variable#################
for (i in 15:34){
  contrasts(data[,i])
}
###########Fitting the Ologit model###############
m = polr(formula = data$readmitted ~ ., data = data[,-c(1,2,35,36)], Hess = TRUE)

ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
exp(coef(m))

ciL <- ctable[,1] - 3* ctable[,3]
ciU <- ctable[,1] + 3* ctable[,3]

exp(cbind(OR = ctable[,1], "2.5%" = ciL, "97.5%"=ciU))

# 0 if No, 1 if non Zero
# fun = function(x){
#   x <- ifelse(x == "No", 0, 1)
# }
# data[,seq(15,36)] = lapply(data[,seq(15,36)], fun)
# data[,37] = ifelse(data[,37] == "NO", 0, ifelse(data[,37] == ">30", 1, 2))
# data
# 
# 
# Meglitinides = c("Repaglinide","")

# data[,seq(15,36)] <- lapply(data[,seq(15,36)], factor, levels =c("No","Down","Up", "Steady"))
# data[,seq(15,36)] <- lapply(data[,seq(15,36)], as.numeric)

# data[which(count(data[,seq(15,36)] == 1) )]
# length(data[5,seq(15,36)] != 1)
# data[5, data[5,seq(15,36)] != 1]


write.csv(data, "2019 MMA Datathon Data Structure1.csv")

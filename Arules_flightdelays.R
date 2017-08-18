rm(list=ls(all=TRUE))

#install.packages("RCurl")
library(RCurl)
data=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth/Datasets/master/FlightDelays.csv"), header=T, sep=',')

str(data)

#Converting to factors
data[,c("CARRIER","DEST","ORIGIN","Weather","DAY_WEEK","Flight.Status")]=data.frame(apply(data[,c("CARRIER","DEST","ORIGIN","Weather","DAY_WEEK","Flight.Status")],2,as.factor))
str(data)

#Binning CRS_DEP_TIME into bins morning ,evening and night
summary(data$CRS_DEP_TIME)
for(i in 1:nrow(data)){
  if(data$CRS_DEP_TIME[i]>=600 & data$CRS_DEP_TIME[i]<1200){
    data$CRS_DEP_TIME[i]="Morning"
  }else if(data$CRS_DEP_TIME[i]>=1200 & data$CRS_DEP_TIME[i]<1800){
    data$CRS_DEP_TIME="Afternoon"
  }else if(data$CRS_DEP_TIME[i]>=1800 & data$CRS_DEP_TIME[i]<2359){
    data$CRS_DEP_TIME="Night"
  }else{
    data$CRS_DEP_TIME[i]="Morning"
  }
}
data$CRS_DEP_TIME=as.factor(data$CRS_DEP_TIME)
  str(data)

#Converting the data to transactional data.Since 
#arules requires data to be in transactional data format
transact_data=as(data,"transactions")  
 write(head(transact_data)) 
 
rules_all= apriori(transact_data)
inspect(rules_all)

# minimum support: supp=0.005
# minimum confidence: conf=0.8
# maximum length of rules: maxlen=
str(data)
# Rules with rhs containing lhs as LGA only
rules =apriori(transact_data,control = list(verbose=F),parameter = list(minlen=2, supp=0.005, conf=0.8),
                  appearance = list(lhs=c("Weather=0",
                                         "Weather=1"),
                                   default="rhs"))


# Keep three decimal places
quality(rules) <- round(quality(rules), digits=3)

# Order rules by lift
rules.sorted <- sort(rules, by="lift")

inspect(rules.sorted)

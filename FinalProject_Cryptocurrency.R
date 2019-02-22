library(rvest)
library(plotrix)
library(dplyr)
Sys.Date()
endDate=gsub("-","",Sys.Date())
endDate
#Bitcoin Data
url_cmc <- sprintf("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20130804&end=%s",endDate)
url_cmc %>%
  read_html() %>%
  html_nodes(css = "table") %>%
  html_table() %>%
  as.data.frame() -> "BitcoinData"
head(BitcoinData)
#Litecoin Data
url_cmc <- sprintf("https://coinmarketcap.com/currencies/litecoin/historical-data/?start=20130804&end=%s",endDate)
url_cmc %>%
  read_html() %>%
  html_nodes(css = "table") %>%
  html_table() %>%
  as.data.frame() -> "LitecoinData"
head(LitecoinData)
#Ripple Data
url_cmc <- sprintf("https://coinmarketcap.com/currencies/ripple/historical-data/?start=20130804&end=%s",endDate)
url_cmc %>%
  read_html() %>%
  html_nodes(css = "table") %>%
  html_table() %>%
  as.data.frame() -> "RippleData"
head(RippleData)
#considering only fisrt 5 columns in the data
BData<- BitcoinData[,1:5]
LData<- LitecoinData[,1:5]
RData<-RippleData[1:5]

BData$Change<- (BData$Close-BData$Open)/BData$Open
LData$Change<- (LData$Close-LData$Open)/LData$Open
RData$Change<- (RData$Close-RData$Open)/RData$Open
#Combining all the Change values into one new data frame
DailyReturns <- data.frame(BDate=BData$Date, BCoin=BData$Change,
                           LDate=LData$Date, LCoin=LData$Change, RDate=RData$Date,
                           RCoin=RData$Close)
#Declaring three new functions to pick the change values
getBCoinReturn = function(PickRow) {
  DailyReturns$BCoin[PickRow]
}
getLCoinReturn = function(PickRow) {
  DailyReturns$LCoin[PickRow]
}
getRCoinReturn = function(PickRow) {
  DailyReturns$RCoin[PickRow]
}

#eAmt1 for BitCoin Data
#eAmt2 for LiteCoin Data
#eAmt3 for Ripple Data

#Bootstrapping
bAmt = 1000
investment=data.frame(day=0,PercentBitCoin=0, PercentLiteCoin=0,PercentRippleCoin=0, eAmt1 = 0, eAmt2 = 0, eAmt3 = 0,eAmtFinal= 0)
investment=investment[0,]

eAmt = data.frame(bAmt=0, i=0, j=0, k=0, rBCoin=0, rLCoin=0, rRCoin=0,eAmt1=0, eAmt2=0, eAmt3=0, eAmtFinal=0)

for(i in seq(0,100,5)){
  for(j in seq(0,(100-i),5)){
    
    k=100-j-i
    
    eAmt = eAmt[0,]
    
    for(itrn in c(1:100)){
      
      PickRow = sample(1:nrow(DailyReturns),1)
      rBCoin = getBCoinReturn(PickRow)
      rLCoin = getLCoinReturn(PickRow)
      rRCoin = getRCoinReturn(PickRow)
      
      eAmt1 = (bAmt*i/100) * (1 + rBCoin)
      eAmt2 = (bAmt*j/100) * (1 + rLCoin)
      eAmt3 = (bAmt*k/100) * (1 + rRCoin)
      eAmtFinal= eAmt1+eAmt2+eAmt3
      
      eAmt = rbind(eAmt,data.frame(bAmt=bAmt, i=i, j=j, k=k, rBCoin=rBCoin, rLCoin=rLCoin, rRCoin=rRCoin,eAmt1=eAmt1, eAmt2=eAmt2, eAmt3=eAmt3, eAmtFinal=eAmtFinal))
      
      
    }
    mean_eAmtFinal = mean(eAmt$eAmtFinal)  
    mean_eAmt1= mean(eAmt$eAmt1)
    mean_eAmt2= mean(eAmt$eAmt2)
    mean_eAmt3=mean(eAmt$eAmt3)
    investment=rbind(investment,data.frame(day=1, PercentBitCoin=i/100,PercentLiteCoin=j/100,PercentRippleCoin=(100-j-i)/100, eAmt1 = mean_eAmt1, eAmt2 = mean_eAmt2,
                                           eAmt3 = mean_eAmt3, eAmtFinal= mean_eAmtFinal) )
    
  }
}

combination=1
for(d in c(2:30)){ #Model for predicting the returns for 30 days
  
  # bAmt should be the eAmt corresponding to each combination from the previous day
  
  for(i in seq(0,100,5)){
    for(j in seq(0,(100-i),5)){
      
      bAmt=investment[combination,8]
      combination=combination+1
      k=100-j-i
      eAmt = eAmt[0,]
      
      for(itrn in c(1:100)){
        
        PickRow = sample(1:nrow(DailyReturns),1)
        rBCoin = getBCoinReturn(PickRow)
        rLCoin = getLCoinReturn(PickRow)
        rRCoin = getRCoinReturn(PickRow)
        
        eAmt1 = (bAmt*i/100) * (1 + rBCoin)
        eAmt2 = (bAmt*j/100) * (1 + rLCoin)
        eAmt3 = (bAmt*k/100) * (1 + rRCoin)
        eAmtFinal= eAmt1+eAmt2+eAmt3
        
        eAmt = rbind(eAmt,data.frame(bAmt=bAmt, i=i, j=j, k=k, rBCoin=rBCoin, rLCoin=rLCoin, rRCoin=rRCoin,eAmt1=eAmt1, eAmt2=eAmt2, eAmt3=eAmt3,eAmtFinal=eAmtFinal))
         
        
      }
      mean_eAmtFinal = mean(eAmt$eAmtFinal)  
      mean_eAmt1= mean(eAmt$eAmt1)
      mean_eAmt2= mean(eAmt$eAmt2)
      mean_eAmt3=mean(eAmt$eAmt3)
      
      lastDay=d
      investment=rbind(investment,data.frame(day=d,PercentBitCoin=i/100,PercentLiteCoin=j/100, 
                                             PercentRippleCoin=(100-j-i)/100, eAmt1 = mean_eAmt1, eAmt2 = mean_eAmt2,
                                             eAmt3 = mean_eAmt3, eAmtFinal= mean_eAmtFinal) )
    }
  }
}

tail(investment)
last_day_investment=subset(investment,investment$day==lastDay)
which.max(last_day_investment$eAmtFinal)
last_day_investment[which.max(last_day_investment$eAmtFinal),]

write.csv(investment,file="C:/Users/shrut/Desktop/SDM Project/investment.csv")

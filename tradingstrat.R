
# install.packages("quantmod",
#                  repos = "https://cran.rstudio.com/")
# install.packages("PerformanceAnalytics",
#                  repos = "https://cran.rstudio.com/")
# install.packages("TTR",
#                  repos = "http://R-Forge.R-project.org")
# 
# install.packages("fTrading",
#                  repos = "https://cran.rstudio.com/")
# require(quantstrat)

# ===================== Strategy ==============================

require(quantmod)
require(PerformanceAnalytics)
require(TTR)
require(fTrading)

# ================= Reading the Data ==========================

data.spx = read.csv("SPX.csv",header = T)
head(data.spx)
attach(data.spx)
str(data.spx)

#===================== Inputs =================================

no.of.periods = 20 # Donchian Channel No of Periods
range.periods = 12 # ADX No of periods
range.level = 26 # ADX Level

#==============================================================

#=========== Functions for High, Low, Close etc================

hl= function(x) 
{
  high.low = x[,c("High","Low")]
  # high.low.close = x[,c("High","Low", "Adj.Close")]
  # close = x[,"Adj.Close"]
  return(high.low)
}
hlc = function(x)
{
  high.low.close = x[,c("High","Low", "Adj.Close")]
  return(high.low.close)
}
cl = function(x)
{
  close = x[,c('Adj.Close')]
  return(close)
}

hi = function(x)
  {
  hi = x[,c("High")]
  return(hi)
  }

lo = function(x)
{
  lo = x[,c("Low")]
  return(lo)
}

# ============================================================

# ============== Donchian Channel Function ===================

channel = function(x,no.of.periods)
{
  high.low = x[c('High','Low')]
  donchian = DonchianChannel(HL = high.low,
                             n = no.of.periods,
                             include.lag = T)
  return(donchian)
  }

#=============================================================

# ================= Directional Function =====================

direction = function(x,range.periods)
{
  high.low.close = hlc(x)
  direction = TTR::ADX(HLC = high.low.close,n = range.periods)
  return(direction)
}
# ============================================================

# ================= Function for Strategy ====================

#posn = array(posn,dim = c(nrow(x),1))
posn=0
strat = function(x, no.of.periods,range.periods,range.level) 
  {
    mychannel = channel(x,no.of.periods)
    mydirection = direction(x,range.periods)
    
    bsig = ifelse(xor((x[,c("Adj.Close")] > mychannel[,c("high")] & mydirection[,c('ADX')] > range.level) ,
                                 (x[,c("Low")] < mychannel[,c("low")] & mydirection[,c('ADX')] < range.level)
                              & x[,c("Adj.Close")] > mychannel[,c("low")]),1,0)
      
    bsig[is.na(bsig)] = 0
    
    #ssig = x[,c('Adj.Close')] < mychannel[,c('low')]            
    ssig = ifelse(xor((x[,c("Adj.Close")] < mychannel[,c("low")] & mydirection[,c('ADX')] > range.level) ,
                                (x[,c("High")] > mychannel[,c("high")] & mydirection[,c('ADX')] < range.level)
                                & x[,c("Adj.Close")] < mychannel[,c("high")]),-1,0)
      
    ssig[is.na(ssig)] = 0
  
    posn = bsig + ssig
  
    return(posn)
  }
  
#   for(i in 2:nrow(x)){
#   # buy signal positions
#   if ((bsig[i]=1) & (posn[i-1]=0)) {
#         posn[i-1] + 1
#                 }
#       else if ((bsig[i]=1) & (posn[i-1]=1)){
#               posn[i-1]
#               }
#           else if ((bsig[i]=1) & (posn[i-1]=-1)) {
#         posn[i-1] + 1
#         }
#   
# # sell signal positions
#   if ((ssig[i]=1) & (posn[i-1]=0)) {
#     posn[i-1] - 1}
# 
#     else if ((ssig[i]=1) & (posn[i-1]=1)){
#     posn[i-1] - 1}
#     
#   
#       else if ((ssig[i]=1) & (posn[i-1]=-1)) {
#       posn[i-1]
# }
  # }
#ret = ROC(Adj.Close)*posn
#eq <- exp(cumsum(ret[i]))

# ============================================================

# ============== Function for benchmark returns ==============

bmkreturns = function(x)
{
  #b.close = cl(x)
  benchmkreturns = ROC(cl(x))#dailyReturn(close,type = "arithmetic")
  return(benchmkreturns)
}

# ============ Function Inputs ======================

# mychannel = channel(data.spx,no.of.periods)
# mydirection = direction(data.spx,range.periods)
myposition = strat(data.spx,no.of.periods,range.periods,range.level)

#myposition = data.frame(myposition,row.names = NULL)
#myposition = na.omit(myposition)

myposition[is.na(myposition)] = 0
myposition = array(myposition)

# myposition[1] = 0
# for (pos in 2:231) {
#   if (myposition[pos-1] != 0){ 
#       
#       myposition[pos - 1] + myposition[pos]
#   
#     myposition[pos] }
#     }
#   #else {0}


bmkreturns.idx = bmkreturns(data.spx)
bmkreturns = na.omit(bmkreturns)
myreturns = bmkreturns*myposition
             
myreturns = data.frame(myreturns)
myreturns = na.omit(myreturns)
eq = exp(cumsum(myreturns))
eq
# ===================================================
#myreturns = bmkreturns*Lag(myposition,1)
#myreturns[1] = 0

# table.Drawdowns(myreturns, top=10)
# table.DownsideRisk(myreturns)
# charts.PerformanceSummary(myreturns)

# ---------- Performance Analytics -----------------
require(PerformanceAnalytics)
charts.PerformanceSummary(cbind(bmkreturns,myreturns))

Performance = function(x) {
  
  cumretn = Return.cumulative(x)
  annretn = Return.annualized(x, scale=252)
  sharpe.ratio = SharpeRatio.annualized(x, scale=252)
  win.pct = length(x[x > 0])/length(x[x != 0])
  ann.SD = sd.annualized(x, scale=252)
  
  DDs = findDrawdowns(x)
  maxDD = min(DDs$return)
  maxDD.period = max(DDs$length)
  
  Perf = c(cumretn, annretn, sharpe.ratio, win.pct, ann.SD, maxDD, maxDD.period)
  names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                  "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
  return(Perf)
}
cbind(Me=Performance(myreturns),spx=Performance(bmkreturns))

# 




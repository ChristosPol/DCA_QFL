# Part 1
rm(list = ls())
# Source functions
path_source <- "Source"
library(stringr)
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

interval <- 60
budget <- 655
minimum <- F
each_usd <- 20
n_orders <- 8 
grid <- c(0.025,0.05,0.075,0.1, 0.125, 0.15, 0.2, 0.25)


# Get all pairs
url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
all_pairs <- names(tb$result)
all_pairs <- data.table(PAIR = all_pairs, CUR=str_sub(all_pairs,start = -3))
all_pairs <- all_pairs[CUR%in%c("USD")]

# Get last price
url <- paste0("https://api.kraken.com/0/public/Ticker")
tb <- jsonlite::fromJSON(url)
price_info <- data.table(PAIR = names(tb$result),
                         PRICE = as.numeric(lapply(lapply(tb$result, "[[", 3), "[", 1)))
all_pairs <- merge(all_pairs,price_info, by = "PAIR", all.x = T)

# Get minimal order
url <- paste0("https://api.kraken.com/0/public/AssetPairs")
tb <- jsonlite::fromJSON(url)
min_deci_info <- data.table(PAIR = names(tb$result),
                         DECIMALS = lapply(tb$result, "[[", "pair_decimals"),
                         MIN = lapply(tb$result, "[[", "ordermin"))
all_pairs <- merge(all_pairs,min_deci_info, by = "PAIR", all.x = T)

minimums_calculated <- copy(all_pairs)
setnames(minimums_calculated, "PAIR", "COIN")
minimums_calculated$DECIMALS <- unlist(minimums_calculated$DECIMALS)
minimums_calculated$MIN <- unlist(minimums_calculated$MIN)

fwrite(minimums_calculated, file = paste0("/Users/christos.polysopoulos/Repositories/QFL_Bot/Data/minimums_calculated.csv"))

rem <- c("AUDUSD",
         "DAIUSD",
         "MSOLUSD",
         "TBTCUSD",
         "USDCUSD",
         "USDTZUSD",
         "WBTCUSD",
         "WAXLUSD",
         "ZEURZUSD",
         "ZGBPZUSD")
all_pairs <-all_pairs[!PAIR %in% rem]

# Select which pairs to trade
trade <-c("ADXUSD"  ,  "ALCXUSD",   "ALPHAUSD"  ,"ANKRUSD"  , "API3USD"  ,
          "AUDIOUSD",  "BADGERUSD", "BICOUSD" ,  "BOBAUSD" ,  "CFGUSD"   ,
          "DENTUSD"  , "EGLDUSD"  , "FARMUSD",   "IDEXUSD"  , "LSKUSD"   ,
          "PARAUSD" ,  "PERPUSD"   ,"PONDUSD" ,  "POWRUSD" ,  "QNTUSD"   ,
          "SPELLUSD" , "SUPERUSD")


all_pairs <- all_pairs[PAIR %in% trade]




trading_table <- data.frame(PAIR = rep(all_pairs$PAIR, each = n_orders),
                            VOL = unlist(rep(all_pairs$MIN, each = n_orders)),
                            DECIMAL= unlist(rep(all_pairs$DECIMALS, each = n_orders)))

pairs <- unique(trading_table$PAIR)

i <- 5
supports <- list()
for (i in 1:length(pairs)){
  msg <- tryCatch({
    df <- simple_OHLC(interval = interval, pair = pairs[i])
    df$week <- isoweek(as.Date(df$Date_POSIXct))
    df$weekday <- weekdays(as.Date(df$Date_POSIXct))
    weeks <- as.character(unique(df$week))
    weeks <- weeks[!weeks %in%names(which.min(table(df$week)))]
    
    SP <- c()
    sp_test <- list()
    rs_test <- list()
    RS <- c()
    j <-1
    # for(j in 1:length(unique(splits[, seq]))){
    for(j in 1:length(weeks)){
      # subdf <- df[splits[seq == j, idx], ]
      subdf <- df[week == weeks[j], ]
      SP[j] <- median(head(sort(subdf[, close]), 5))
      
      
      sp_test[[j]] <- c(min(head(sort(subdf[, close]), 5)), max(head(sort(subdf[, close]), 5)))
      rs_test[[j]] <- c(min(tail(sort(subdf[, close]), 5)), max(tail(sort(subdf[, close]), 5)))
      RS[j] <- median(tail(sort(subdf[, close]), 5))
    }
    
  }, error = function(e){
  })
  
  SP <- SP[SP<tail(df$close, 1)]
  needed <- n_orders-length(SP)
  
  if(needed == n_orders){
    grid_set <- tail(df$close, 1) - tail(df$close, 1)*grid
    SP <- grid_set
  } else{
    lowest <- min(SP)
    grid_selected <- grid[1:needed]
    grid_set <- lowest - lowest*grid_selected
    SP <- c(SP, grid_set)  
  }
  
  print(paste0("Pair ", pairs[i], " needed ", needed, " orders" ))
  # print(paste0("Sharpe Ratio for: ",  EUR_pairs[i]," ", round(unique(df$sharpe), 4))) 
  Sys.sleep(1.2)
  supports[[i]] <- sort(SP, decreasing = T)
}
setDT(trading_table)
trading_table[, PRICE_ENTER := unlist(supports)]
trading_table[, ORDER_BUY_ID:=NA]
trading_table[, STATUS_BUY:=NA]
trading_table[, index := 1:.N, by=PAIR]

if(minimum){
  trading_table[, VOL := round(each_usd/PRICE_ENTER, DECIMAL)]
}


trading_table[index == 1, PRICE_EXIT := PRICE_ENTER + PRICE_ENTER*0.01]

trading_table$PRICE_EXIT <- na.locf(trading_table$PRICE_EXIT)
trading_table$index <- NULL
trading_table[,ORDER_SELL_ID := NA]
trading_table[,STATUS_SELL := NA]

trading_table[, PRICE_ENTER := round(PRICE_ENTER, DECIMAL)]
trading_table[, PRICE_EXIT := round(PRICE_EXIT, DECIMAL)]
# trading_table[, per := (PRICE_EXIT-PRICE_ENTER)/PRICE_ENTER]
# trading_table$per <- NULL
trading_table[, DECIMAL := NULL]
trading_table[, BET_ENTER := 1.2*as.numeric(VOL)*PRICE_ENTER]
trading_table[, VOL:= 1.2*as.numeric(VOL)]
cumul <- trading_table[, list(SUM_BET = sum(BET_ENTER)), by = PAIR]
setorder(cumul, SUM_BET)
cumul[, CUM_BET := cumsum(SUM_BET)]

cumul <- cumul[CUM_BET < budget]
trading_table <- trading_table[PAIR %in% cumul$PAIR]



fwrite(trading_table, file = paste0("/Users/christos.polysopoulos/Repositories/QFL_Bot/Data/trading_table_BIND.csv"))

# Preamble ---------------------------------------------------------------------
rm(list = ls())

# I need to get also open orders from API to see the rate of catpured orders

# Source functions
path_source <- "Source"
files.sources = list.files(path_source, full.names = T)
invisible(sapply(files.sources, source))

csv_path <- paste0("Data/trading_table.csv")
orders <- fread(csv_path)

key = API_Key
secret = API_Sign

offset <- 0
url = "https://api.kraken.com/0/private/ClosedOrders"

# url = "https://api.kraken.com/0/private/OpenOrders"
# tt<- get_trade_history(url, key, secret, offset)
# length(tt$result$open)

i <- 1
trades_raw <- list()
while (offset <= 3000) {
  
  trades_raw[[i]] <- get_trade_history(url, key, secret, offset)
  
  offset <- offset + 50
  i <- i +1  
  Sys.sleep(3)
  print(offset)
}

dfs <- list()
namen <- list()
for(i in 1:length(trades_raw)){
  print(i)
  dfs[[i]] <- as.data.frame(do.call(rbind, trades_raw[[i]]$result$closed))
  namen[[i]] <- names(trades_raw[[i]]$result$closed)
}
df <- rbindlist(dfs, fill =T)
df$ids <- unlist(namen)
setDT(df)


df <- df[status == "closed"]
key <- c("ids", "opentm", "closetm", "vol", "vol_exec", "cost", "fee", "price")
df <- df[, ..key]


df_original <- copy(df)
colnames(df) <- paste0(colnames(df), "_BUY")
orders_upd <- merge(orders, df, by.x = "ORDER_BUY_ID", by.y = "ids_BUY",all.x =T) 
df <- copy(df_original)
colnames(df) <- paste0(colnames(df), "_SELL")
orders_upd1 <- merge(orders_upd, df, by.x = "ORDER_SELL_ID", by.y = "ids_SELL",all.x =T) 


orders_upd1[, opentm_BUY := anytime(as.numeric(as.character(opentm_BUY)))]
orders_upd1[, closetm_BUY := anytime(as.numeric(as.character(closetm_BUY)))]
orders_upd1[, opentm_SELL := anytime(as.numeric(as.character(opentm_SELL)))]
orders_upd1[, closetm_SELL := anytime(as.numeric(as.character(closetm_SELL)))]


orders_upd1_closed <- orders_upd1[STATUS_BUY == "CLOSED"]
orders_upd1_closed <- orders_upd1_closed[STATUS_SELL != "CLOSED"]


orders_upd1_closed[, cost_BUY := unlist(as.numeric(cost_BUY))]
# orders_upd1_closed[, cost_SELL := unlist(as.numeric(cost_SELL))]
orders_upd1_closed[, fee_BUY := unlist(as.numeric(fee_BUY))]
# orders_upd1_closed[, fee_SELL := unlist(as.numeric(fee_SELL))]
# orders_upd1_closed[, vol_SELL := unlist(as.numeric(vol_SELL))]
# orders_upd1_closed[, vol_exec_SELL := unlist(as.numeric(vol_exec_SELL))]
orders_upd1_closed[, cost_BUY_clean := cost_BUY + fee_BUY]


interval <- 1
pairs <- orders_upd1_closed[, unique(PAIR)]
prices <- c()
for (i in 1:length(pairs)){
  df <- simple_OHLC(interval = interval, pair = pairs[i])
  prices[i] <- tail(df$close, 1)
  Sys.sleep(1.5)
  print(i)
}
df <- data.table(price = prices, pair = pairs)
colnames(df)[1] <- "current_price"
orders_upd1_closed <- merge(orders_upd1_closed, df, by.x = "PAIR", by.y ="pair", all.x = T) 
orders_upd1_closed[, current_cost:= current_price*as.numeric(vol_exec_BUY)]
orders_upd1_closed[, result_per := (current_cost - cost_BUY_clean)/cost_BUY_clean*100]
orders_upd1_closed$vol_exec_BUY<-as.numeric(unlist(orders_upd1_closed$vol_exec_BUY))
f <- orders_upd1_closed[, list(quote_res = sum(current_cost - cost_BUY_clean),
                               avg_per = mean(result_per),
                               current_cost=sum(current_cost),
                               cost_BUY_clean = sum(cost_BUY_clean),
                               VOL= sum(vol_exec_BUY)), by = PAIR]

sum(f$quote_res)
mean(f$avg_per)

setorder(f, -quote_res)
f$PAIR <- factor(f$PAIR, levels = f$PAIR)
f[quote_res < 0, colour := "negative"]
f[quote_res > 0, colour := "positive"]
ggplot(data = f, aes(x = PAIR, y = quote_res, fill = colour))+
  geom_col()+theme_light()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_manual(values= c("red", "darkgreen"))
f[quote_res < 0, sum(quote_res)]
f[quote_res > 0, sum(quote_res)]

ggplot(data = f, aes(x = PAIR, y = avg_per, fill = colour))+
  geom_col()+theme_light()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
f1 <- merge(f,unique(orders_upd1_closed[, .(PAIR, PRICE_EXIT)]), by = "PAIR", all.x = T)

f1[, target := VOL*PRICE_EXIT]
View(f1)

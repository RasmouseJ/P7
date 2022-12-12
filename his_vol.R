library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)
library(lubridate)
r <- 0.00458/252
symbol <- "TSLA"
from_date <- lubridate::today()
### Functions -----------------------------------------------------------------
his_vol <- function(prices) {
  logreturn <- log(tail(prices, -1)) - log(head(prices, -1))
  res <- sqrt(60*7.5)*sqrt(1/(length(logreturn)-1)*sum((logreturn-mean(logreturn))^2))
  return(res)
}

gBSm <- function(s,r,ttm,K,sigma){
  d1 = 1/(sigma*sqrt(ttm))*(log(s/K)+(r+0.5*sigma^2)*(ttm))
  d2 = d1 - sigma*sqrt(ttm)
  
  call_price = s*pnorm(d1) - exp(-r*(ttm))*K*pnorm(d2)
  return(call_price)
}

get_calls <- function(x, options) {
  return(options[[x]]$calls)
}

get_share <- function(
  symbol, 
  from_date = "2022-11-01",
  to_date = "2022-11-16",
  multiplier = "5",
  interval = "minute"
  ) {
  stock_api <- paste0(
    "https://api.polygon.io/v2/aggs/ticker/",
    symbol, "/range/", multiplier, "/", interval, 
    "/", from_date, "/", to_date, 
    "?adjusted=true&sort=asc&limit=50000&",
    "apiKey=GI1r0nNP7O0RmLRt0PXkw1VnnMgSYnb4"
  )
  
  stock_raw <- jsonlite::fromJSON(stock_api)
  
  stock <- stock_raw$results %>% 
    dplyr::mutate(
      Start = lubridate::as_datetime(t/1000),
      Date = lubridate::date(Start),
      Hour = lubridate::hour(Start) + 1
    ) %>% 
    dplyr::select(
      Date,
      Hour,
      "Price" = c
    ) %>% group_by(across(c(-Price))) %>%   
    dplyr::summarise(MeanPrice = mean(Price)) %>% 
    dplyr::ungroup()
  
  return(stock)
}

### Get Data ------------------------------------------------------------------

options_list <- quantmod::getOptionChain(
  Symbols = symbol, 
  Exp = "2022"
)

calls_raw <- purrr::map_dfr(
  .x = names(options_list), 
  .f = get_calls,
  options = options_list
)

df <- readxl::read_excel("clean_data_no_interpolation.xlsx")

historical_vol <- his_vol(df$TSLA)
TSLA_price <- get_share("TSLA")
calls <- calls_raw %>% 
  dplyr::mutate(
    contract_name = rownames(.),
    Expiration = substr(
      contract_name,
      start = 5,
      stop = 10
    ) %>% lubridate::as_date(),
    TTM = lubridate::days(
      Expiration - from_date
    ) %>% lubridate::day(),
    Moneyness = last_price/Strike
  ) %>% magrittr::set_rownames(1:nrow(.)) %>% 
  dplyr::select(
    Expiration,
    TTM,
    Strike,
    "Price" = Last,
    Moneyness,
    "IV_orig" = IV,
    LastTradeTime
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    LastTradeTime >= lubridate::as_date("2022-11-01"),
    Strike <= last_price + 100,
    Strike > last_price - 100
  ) %>% 
  dplyr::mutate(
    Date = lubridate::date(LastTradeTime),
    Hour = lubridate::hour(LastTradeTime) + 1
  ) %>% 
  dplyr::left_join(TSLA_price, by = c("Date", "Hour")) %>% 
  dplyr::mutate(
    BS_price = gBSm(
      s = MeanPrice,
      r = r,
      ttm = TTM,
      K = Strike,
      sigma = historical_vol
    ),
    Error = Price - BS_price
  )

historical_vol_SPY <- his_vol(df$SPY)
SPY_price <- get_share("SPY")
options_list_SPY <- quantmod::getOptionChain(
  Symbols = "SPY", 
  Exp = "2022"
)

calls_raw_SPY <- purrr::map_dfr(
  .x = names(options_list_SPY), 
  .f = get_calls,
  options = options_list_SPY
)

calls_SPY <- calls_raw_SPY %>% 
  dplyr::mutate(
    contract_name = rownames(.),
    Expiration = substr(
      contract_name,
      start = 4,
      stop = 9
    ) %>% lubridate::as_date(),
    TTM = lubridate::days(
      Expiration - from_date
    ) %>% lubridate::day(),
    Moneyness = last_price/Strike
  ) %>% magrittr::set_rownames(1:nrow(.)) %>% 
  dplyr::select(
    Expiration,
    TTM,
    Strike,
    "Price" = Last,
    Moneyness,
    "IV_orig" = IV,
    LastTradeTime
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    LastTradeTime >= lubridate::as_date("2022-11-01"),
    Strike <= last_price_SPY + 100,
    Strike > last_price_SPY - 100
  ) %>% 
  dplyr::mutate(
    Date = lubridate::date(LastTradeTime),
    Hour = lubridate::hour(LastTradeTime)+1
  ) %>% 
  dplyr::left_join(SPY_price, by = c("Date", "Hour")) %>% 
  dplyr::mutate(
    BS_price = gBSm(
      s = MeanPrice,
      r = r,
      ttm = TTM,
      K = Strike,
      sigma = historical_vol_SPY
    ),
    Error = Price - BS_price
  )

Error_df <- data.frame(
  "Symbol" = rep("TSLA", nrow(calls)), 
  "Error" = calls$Error
) %>% rbind(data.frame(
  "Symbol" = rep("SPY", nrow(calls_SPY)),
  "Error" = calls_SPY$Error
))
mean_error <- Error_df %>% 
  dplyr::group_by(Symbol) %>% 
  dplyr::summarise(
    Mean = mean(Error),
    Median = median(Error),
    RMSE = sqrt(mean(Error^2))
  )

p<-ggplot(Error_df, aes(x=Error, color=Symbol, fill = Symbol)) +
  geom_histogram(alpha=0.2, position="identity") +
  geom_vline(data=mean_error, aes(xintercept=Mean, color=Symbol),
             linetype="dashed") + theme_bw() + 
  labs(title="Historical Volatility Pricing Error of SPY and TSLA",x="Error (USD)", y = "Count")
p


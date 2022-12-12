### Preliminaries -------------------------------------------------------------
library(httr)
library(rvest)
library(dplyr)
library(lubridate)
library(magrittr)
library(patchwork)
library(tidyr)

### Magic numbers -------------------------------------------------------------
r <- 0.0466/365 #Risk-free interest rate
K <- 100 #Strike Price
sigma <- 0.0458 #Volatility
maturity <- lubridate::as_datetime("2022-10-28")

#Parameters for API
contract <- "AAPL221028C00150000"
multiplier <- "1"
interval <- "minute"
from_date <- "2022-10-17"
to_date <- "2022-10-21"   

### Functions -----------------------------------------------------------------
delta <- function(ttm,s,r,K,sigma){
  d1 = 1/(sigma*sqrt(ttm))*(log(s/K)+(r+0.5*sigma^2)*(ttm))
  return(pnorm(d1))
}

gBSm <- function(s,r,ttm,K,sigma){
  
  d1 = 1/(sigma*sqrt(ttm))*(log(s/K)+(r+0.5*sigma^2)*(ttm))
  d2 = d1 - sigma*sqrt(ttm)
  
  call_price = s*pnorm(d1) - exp(-r*(ttm))*K*pnorm(d2)
  return(call_price)
}

### Get data ------------------------------------------------------------------
option_api <- paste0(
  "https://api.polygon.io/v2/aggs/ticker/",
  "O:",contract, "/range/", multiplier,
  "/", interval, "/", from_date, "/", to_date,
  "?adjusted=true&sort=asc&limit=50000&",
  "apiKey=GI1r0nNP7O0RmLRt0PXkw1VnnMgSYnb4"
)

stock_api <- paste0(
  "https://api.polygon.io/v2/aggs/ticker/",
  "AAPL/range/", multiplier, "/", interval, 
  "/", from_date, "/", to_date, 
  "?adjusted=true&sort=asc&limit=50000&",
  "apiKey=GI1r0nNP7O0RmLRt0PXkw1VnnMgSYnb4"
)

option_raw <- jsonlite::fromJSON(option_api)
stock_raw <- jsonlite::fromJSON(stock_api)

option <- option_raw$results %>% 
  dplyr::mutate(
    Start = lubridate::as_datetime(t/1000)
  ) %>% 
  dplyr::select(
    Start,
    "OptionPrice" = c
  )

stock <- stock_raw$results %>% 
  dplyr::mutate(
    Start = lubridate::as_datetime(t/1000)
  ) %>% 
  dplyr::select(
    Start,
    "Price" = c
  )

Data <- stock %>% 
  dplyr::left_join(option, by = "Start") %>% 
  tidyr::fill(OptionPrice, .direction = c("down")) %>% 
  dplyr::filter(!is.na(OptionPrice))

### Delta hedge ---------------------------------------------------------------
ttm <- lubridate::as_datetime(to_date) - lubridate::as_datetime(from_date) %>% 
  interval()
Initial_BS_price <- 



### Plot data -----------------------------------------------------------------
p1 <- ggplot(
  Data[,c("Start","Price")], 
  aes(x=1:nrow(Data), y=Price)
) +
  geom_line() + xlab("") + ylab("Price USD") + 
  labs(title = "AAPL Price and Option Price")

p2 <- ggplot(
  Data[,c("Start","OptionPrice")], 
  aes(x=1:nrow(Data), y=OptionPrice)) +
  geom_line() + xlab("") + ylab("")

p1+p2








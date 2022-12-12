### Preliminaries -------------------------------------------------------------
suppressPackageStartupMessages({
  library(quantmod)
  library(rvest)
  library(reshape2)
  library(magrittr)
  library(akima)
  library(plotly)
  library(dplyr)
  library(purrr)
  library(lubridate)
  library(oce)
  library(RND)
})

from_date <- lubridate::today() %>% lubridate::as_date()
symbol <- "SPY"
r <- 1.81746e-05

### Functions -----------------------------------------------------------------
get_calls <- function(x, options) {
  return(options[[x]]$calls)
}

gBSm <- function(s,r,ttm,K,sigma){
  
  d1 = 1/(sigma*sqrt(ttm))*(log(s/K)+(r+0.5*sigma^2)*(ttm))
  d2 = d1 - sigma*sqrt(ttm)
  
  call_price = s*pnorm(d1) - exp(-r*(ttm))*K*pnorm(d2)
  return(call_price)
}

funToOptim <- function(price,s,r,ttm,K,sigma){
  res <- abs(price - gBSm(s,r,ttm,K,sigma))
  return(res)
}

getIV <- function(price,ttm,strike,s,r){
  res <- optimize(
    funToOptim,
    interval = c(0,2),
    price = price,
    s = s,
    r = r,
    ttm = ttm,
    K = strike
  ) 
  return(res$minimum)
}

get_RND_IV <- function(price, ttm, strike){
  attempt <- try(
    RND::compute.implied.volatility(
      r=r, 
      te=ttm, 
      s0 = last_price, 
      k = strike, 
      y = 0, 
      call.price = price, 
      lower = 0, 
      upper = 0.5
    ),
    silent = TRUE
  )
  if (class(attempt) != "try-error"){
    return(attempt)
  }
  return(NA)
}

### Get Data ------------------------------------------------------------------
last_price <- quantmod::getQuote(symbol)$Last

options_list <- quantmod::getOptionChain(
  Symbols = symbol, 
  Exp = "2022"
)

calls_raw <- purrr::map_dfr(
  .x = names(options_list), 
  .f = get_calls,
  options = options_list
) 

calls <- calls_raw %>% 
  dplyr::mutate(
    contract_name = rownames(.),
    Expiration = substr(
      contract_name,
      start = nchar(symbol)+1,
      stop = nchar(symbol)+6
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
    "IV_orig" = IV
  ) %>%
  dplyr::filter(
    Moneyness <= 2,
    TTM > 0,
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    IV = getIV(
      price = Price, 
      ttm = TTM, 
      strike = Strike, 
      s = last_price, 
      r = 0.0011
    ) %>% round(5),
    IV_RND = get_RND_IV(
      Price, 
      TTM, 
      Strike
    ) %>% round(5)
  ) %>% ungroup()

calls <- calls %>% dplyr::filter(!is.na(IV_RND))

### RND Volatility smile ------------------------------------------------------
fil_df <- calls %>% dplyr::filter(TTM == 2, !is.na(IV_RND), IV < 0.2, Moneyness < 1.5)
price1 <- fil_df$Moneyness
price2 <- fil_df$Moneyness^2
plot(fil_df$Moneyness,fil_df$IV_RND)

model <- lm(IV_RND ~ price1 + price2, data = fil_df)
IV_predict <- predict(model, list(price_seq,price_seq^2))
lines(price1, IV_predict, col = "blue", lwd = 2)

smile <- ggplot2::ggplot(data = fil_df, aes(x = Moneyness, y = IV_RND)) +
  geom_point() +
  geom_smooth(color = "#4386FB") +
  theme_bw() + 
  labs(title = "Volatility smile of SPY", y = "Implied Volatility %")
smile
### Get IV Grid ---------------------------------------------------------------

IV_Grid_Calls <- acast(calls %>% dplyr::filter(!is.na(IV_RND)), TTM ~ Strike, value.var = "IV")

toInterpolate <- which(is.na(IV_Grid_Calls))
coords <- cbind(
  toInterpolate%%dim(IV_Grid_Calls)[1], 
  toInterpolate%/%dim(IV_Grid_Calls)[1] + 1
)
coords[coords[,1] == 0, 2] <- coords[coords[,1] == 0, 2] - 1 
coords[coords[,1] == 0, 1] <- dim(IV_Grid_Calls)[1]

#loop through NAs and interpolate
for(i in 1:nrow(coords)){
  #get the coordinates of a 10x10 area around the missing value
  x1 <- max(coords[i,1] - 10, 1)
  x2 <- min(coords[i,1] + 10, dim(IV_Grid_Calls)[1])
  y1 <- max(coords[i,2] - 10, 1)
  y2 <- min(coords[i,2] + 10, dim(IV_Grid_Calls)[2])
  
  #get the moneyness/time to mat combination of the missing value
  x0 <- as.numeric(rownames(IV_Grid_Calls)[coords[i,1]])
  y0 <- as.numeric(colnames(IV_Grid_Calls)[coords[i,2]])
  
  #get the part of the grid that is used to interpolate and remove all missing 
  #values that are present
  interpGrid <- IV_Grid_Calls[x1:x2,y1:y2]
  interpGrid <- melt(interpGrid)
  interpGrid <- na.omit(interpGrid)
  
  #interpolate linearly
  interpVal <- interp(
    x = interpGrid$Var1, 
    y = interpGrid$Var2, 
    z = interpGrid$value,
    xo = x0, 
    yo = y0,
    linear = TRUE, 
    extrap = TRUE)$z[1,1]
  
  #if linear interpolation doesnt yield a result, use spline interpolation
  if(is.na(interpVal)){
    interpVal <- interp(
      x = interpGrid$Var1, 
      y = interpGrid$Var2, 
      z = interpGrid$value,
      xo = x0, 
      yo = y0,
      linear = FALSE, 
      extrap = TRUE)$z[1,1]
  }
  
  #if the resulting value is clearly wrong, e.g. negative or way outside the values that are used to interpolate,
  #leave it as NA
  if(interpVal < 0 | interpVal > max(interpGrid$value * 1.5)){
    interpVal <- NA
  }
  
  #replace the value with the result of the interpolation
  IV_Grid_Calls[coords[i,1],coords[i,2]] <- interpVal
}
r_names <- rownames(IV_Grid_Calls)
c_names <- colnames(IV_Grid_Calls)

IV_Grid_Calls <- IV_Grid_Calls %>% 
  oce::matrixSmooth() %>% 
  oce::matrixSmooth() %>% 
  set_colnames(value = c_names) %>% 
  set_rownames(value = r_names)

### Plot IV Surface -----------------------------------------------------------
x_axis <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  title = "Strike"
)

y_axis <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  title = "Time To Maturity"
)

z_axis <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)',
  tickformat = ".2%",
  title = "Implied Volatility"
)

fig <- plotly::plot_ly(
  x = colnames(IV_Grid_Calls), 
  y =  rownames(IV_Grid_Calls), 
  z = IV_Grid_Calls
) %>% plotly::add_surface() %>% 
  plotly::layout(
    scene = list(
      xaxis=x_axis, 
      yaxis=y_axis, 
      zaxis = z_axis)
  ) %>% plotly::colorbar(
    title = "", 
    x = 0.9, 
    y = 0.75, 
    tickformat = ".2%"
  )
fig

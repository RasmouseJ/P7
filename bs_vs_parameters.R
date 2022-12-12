### Preliminaries -------------------------------------------------------------
library(httr)
library(rvest)
library(dplyr)
library(lubridate)
library(magrittr)
library(patchwork)
library(tidyr)
library(ggplot2)
library(cowplot)
library(gridExtra)

r <- 0.0466/365 #Risk-free interest rate
K <- 100 #Strike Price
sigma <- 0.05
t <- 0
s = 100

### Functions -----------------------------------------------------------------
gBSm <- function(s,r,t,K,sigma,type = "Call"){
  ttm <- 10-t
  d1 = 1/(sigma*sqrt(ttm))*(log(s/K)+(r+0.5*sigma^2)*(ttm))
  d2 = d1 - sigma*sqrt(ttm)
  
  price <- s*pnorm(d1) - exp(-r*(ttm))*K*pnorm(d2)
  if (type != "Call") {
    price <- pnorm(-d2)*K*exp(-r*ttm)-s*pnorm(-d1) 
  }
  return(price)
}

### Plots ---------------------------------------------------------------------

price_plot <- ggplot2::ggplot() +
  xlim(90, 120) +
  theme_bw() + 
  geom_function(
    fun = gBSm, 
    args = list(r = r, sigma= sigma, K=K, t=t), 
    color ="red"
  ) + xlab("(a) Price") + ylab("")

strike_plot <- ggplot2::ggplot() +
  xlim(90, 120) +
  theme_bw() + 
  geom_function(
    fun = gBSm, 
    args = list(r = r, sigma= sigma, s=s, t=t), 
    color ="red"
  ) + xlab("(b) Strike")+ ylab("")

sigma_plot <- ggplot2::ggplot() +
  xlim(0.01, 2) +
  theme_bw() + 
  geom_function(
    fun = gBSm, 
    args = list(r = r, K=K, s=s, t=t), 
    color ="red"
  ) + xlab("(c) Volatility")+ ylab("")

ttm_plot <- ggplot2::ggplot() +
  xlim(0, 9.99) +
  theme_bw() + 
  geom_function(
    fun = gBSm, 
    args = list(r = r, K=K, s=s, sigma= sigma), 
    color ="red"
  ) + xlab("(d) Time")+ ylab("")

grid.arrange(
  price_plot, 
  strike_plot, 
  sigma_plot, 
  ttm_plot, 
  top = "Black and Scholes Formula vs Parameters"
)
title <- ggdraw() + draw_label("                                                                        Black and Scholes Formula vs Parameters")
title2 <- ggdraw() + draw_label("", fontface='bold')


cowplot::plot_grid(
  title,
  title2,
  price_plot,
  strike_plot,
  sigma_plot,
  ttm_plot,
  align = "v",
  nrow = 3,
  ncol = 2,
  rel_heights=c(0.15, 1, 1)
)


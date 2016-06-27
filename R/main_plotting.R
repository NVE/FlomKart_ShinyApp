# Plotting function for the ShinyApp

# Plot function for server ------------------
plot4server  <- function(dat, param, distr.index = 1) {
  # plot fitted probability density function to estimated empirical pdf
  # Returns nothing, saves nothing
  xmax <- max(dat)*1.2
  x <- seq(0, xmax, xmax / 100)
  distr <- distr.name[distr.index]
  
  # Distribution specific y vector
  # PB: there is some logic erro with the NA management here. The app works, but this could be improved
  if(distr.index == 1 && all(is.na(param)) == FALSE)   y <- dgumbel(x, param[1], param[2])
  if(distr.index == 2 && all(is.na(param)) == FALSE)    y <- dgamma(x, param[1], param[2])
  if(distr.index == 3 && all(is.na(param)) == FALSE)      y <- evd::dgev(x, param[1], param[2], param[3]) 
  if(distr.index == 4 && all(is.na(param)) == FALSE)       y <- f.genlogis(x, param[1], param[2], param[3])
  if(distr.index == 5 && all(is.na(param)) == FALSE)  y <- f.gamma(x, param[1], param[2], param[3])
  
  ymax <- max( max(na.omit(y)), max(na.omit(density(dat)$y)) ) * 1.1
  
  # Plotting input dat, this is common to all distributions
  hist(dat, xlab = "Flood discharge (m3/s)",ylab = "Probability density",freq = FALSE, 
       breaks = seq(0, xmax, xmax / 15), col = "gray", main = NULL, xlim = c(0, xmax), ylim = c(0, ymax))  
  par(new = TRUE)
  plot(x, y, xlim = c(0, xmax), ylim = c(0, ymax), type = "l", lwd = 2, col = "black", xlab = "", ylab = "")
  par(new = TRUE)
  plot(density(dat), main = "Density distribution and data histogramm",
       xlim = c(0, xmax), ylim = c(0, ymax), lty = 1, lwd = 3, col = "blue", xlab = "", ylab = "")
  
  legend("topright", inset = .05, c("Model","Empirical" ), col = c("black","blue"),lty = c(1, 1),lwd=c(2, 3), 
         merge = TRUE, bg = "gray90")
}

plot4server_rlevel <- function(dat, param, distr.index = 1) {    
  # Plot return levels
  # Returns nothing, saves nothing  
  
  # Common to all distributions
  xmin <- min(dat)
  xmax <- max(dat)*1.5
  y <- seq(xmin, xmax, length = 100)
  empq <- sort(dat)
  
  # The x vector is distribution specific
  if(distr.index == 1) {
    x <- 1 / (1 - pgumbel(y, param[1], param[2]))
    # empT <- 1/(1-(seq(1:length(empq))-0.44)/(length(empq))+0.12) # Gringorten, optimized for the gumbel distribution
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr.index == 2) {
    x <- 1 / (1 - pgamma(y, param[1], param[2]))
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr.index == 3)  {
    x <- 1 / (1 - evd::pgev(y, param[1], param[2], param[3]))  # initially evd::pgev # also tried nsRFA::F.GEV
    # empT <- 1/(1-(seq(1:length(empq))-0.44)/(length(empq))+0.12) # Gringorten, optimized for the gumbel distribution
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr.index == 4)   { 
    x <- 1 / (1 - F.genlogis(y, param[1], param[2], param[3]))
    # empT <- 1/(1-(seq(1:length(empq))-0.35)/(length(empq)))  # APL
    empT <- 1/(1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  if(distr.index == 5) { 
    x <- 1/(1 - nsRFA::F.gamma(y, param[1], param[2], param[3]))
    empT <- 1 / (1 - (seq(1:length(empq)) - 0.50) / (length(empq)))   # Hazen, a traditional choice
  }
  
  # xaxt="n" is to not plot the x axis ticks, as I specify them later
  plot(log(log(x)), y, xlim = c(0, log(log(1000))), xaxt = "n", ylim = c(0, xmax),  
       main = "Return levels", xlab = "Return period (years)", ylab = "Flood discharge (m3/s)",type = "l",lwd = 2)
  tix <- c(5, 10, 20, 50, 100, 200, 500)
  axis(1, at = log(log(tix)), labels = tix)
  
  # plot empirical dat points    
  points(log(log(empT)), empq, pch = 16, col = "blue")
  grid(nx = 7, ny = 10, lwd = 2) # grid only in y-direction
  
}


plot4server_cdf  <- function(dat, param, distr = 1) {
  # Plot estimated and empirical cumulative distribution function
  # Returns nothing, saves nothing
  
  xmax <- max(dat)*1.2
  x <- seq(0, xmax, xmax / 100)
  
  # Distribution specific y vector
  if(distr == 1) y <- pgumbel(x, param[1], param[2])
  if(distr == 2)  y <- pgamma(x, param[1], param[2])
  if(distr == 3)    y <- evd::pgev(x, param[1], param[2], param[3])
  if(distr == 4)     y <- F.genlogis(x, param[1], param[2], param[3])
  if(distr == 5) y <- F.gamma(x, param[1], param[2], param[3])
  
  
  plot(ecdf(dat), main = "Cumulative density function", xlim = c(0, xmax), ylim = c(0, 1), 
       xlab = "", ylab = "", lty = 21, col = "blue")
  par(new = TRUE)
  plot(x, y, xlim = c(0, xmax), ylim = c(0, 1),
       type = "l",lwd = 2, col = "black", xlab = "Flood discharge (m3/s)", ylab = "Cumulative probability")
}  


plot4server_qq  <- function(dat, param, distr = 1) { 
  # QQ plot of empiricial against modelled 
  # Returns nothing, saves nothing
  
  # Compute plotting position 
  # pvalues <-(seq(1:length(dat))-0.35)/length(dat) # APL
  p.values <- (seq(1:length(dat)) - 0.5) / length(dat)   # Hazen, a traditional choice
  y <- sort(dat)
  
  if(distr == 1)  x <- sort(evd::rgumbel(p.values, param[1], param[2]))
  if(distr == 2) {
    # pvalues <- (seq(1:length(dat))-0.44)/(length(dat)+0.12) # Gringorten, optimized for the gumbel distribution
    x <- sort(stats::rgamma(p.values, param[1], param[2]))
  }
  if(distr == 3) {
    # pvalues <- (seq(1:length(dat))-0.44)/(length(dat)+0.12) # Gringorten, optimized for the gumbel distribution
    x <- sort(evd::rgev(p.values, param[1], param[2], param[3]))  # initially evd::rgev  # also tried nsRFA::invF.GEV
  }
  if(distr == 4)     x <- sort(invF.genlogis(p.values, param[1], param[2], param[3]))  # PB shouldnt it be rand.genlogis ?
  if(distr == 5) x <- sort(rand.gamma(p.values, param[1], param[2], param[3]))  
  
  if (length(x) == length(y)) {
    plot(x, y, ylab = "Empirical flood dischare (m3/s)", xlab = "Modelled flood dischare (m3/s)", 
         main = "Quantile-Quantile Plot", pch = 16, col = "blue")
    par(new = TRUE)
    abline(0, 1, lwd = 2, col = "black") 
  } else {
    plot(1,1)
    legend("topright", inset = .05, "Missing or wrong data for the record length", bty = "n", bg = "gray90", cex = 1.2)
  }
  
}
## Supporting functions for Shiny App

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

plot4server_gof  <- function(station, gof) { 
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {
      method <- method.name[m] 
      temp2plot[m, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1), 
                                   count = c(1, 1, 1, length(sampling_years), 1))
 
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ])) 
      
      
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      legend("topright", inset = .05, c("Maximum likelihood", "L-moments", "Ordinary moments" ), bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1), lwd=c(3, 3, 3), merge = TRUE, bg = "gray90", cex = 1.2)  
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
      
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
    
  }
  frame()  # We fill the 6th plot with a blank plot
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),
         lwd=c(3, 3, 3), merge = TRUE, bg = "gray90")    
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      temp2plot[d, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1), 
                                   count = c(1, 1, 1, length(sampling_years), 1))
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))     
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m == 1) {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
      
    } else {
      frame()
      frame()
      frame()
    }
  }
}

plot4server_gof.rlevels  <- function(station, gof, r.period) { 
  
  print(gof)
  if (gof == "BS") {
    r.period.index <- which(rperiods.bs == r.period)  
  } else {r.period.index <- which(return.periods == r.period)}
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {
      method <- method.name[m] 
      temp2plot[m, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1, 1)) 
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))   
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      
      legend("topright", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
  }
  frame()   # We fill the 6th plot with a blank plot
  #   legend("topleft", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
  #          col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      temp2plot[d, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1, 1)) 
      
    }
    if (all(is.na(temp2plot)) == FALSE) {
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))    
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m ==1) {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      #    legend("topright", inset = .05, distr.name, cex = 1.2, bty = "n",
      #              col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90")  
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
    } else {
      frame()
      frame()
      frame()
    }
  }
}

####################
plot4server_nt  <- function(station, gof, r.period) { 
  
  r.period.index <- which(rperiods.bs == r.period)  
  
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {
      method <- method.name[m] 
      temp2plot[m, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1)) 
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))   
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      
      legend("topright", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
  }
  frame()   # We fill the 6th plot with a blank plot
  #   legend("topleft", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
  #          col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      temp2plot[d, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1)) 
      
    }
    if (all(is.na(temp2plot)) == FALSE) {
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))    
      
      plot(temp2plot[1, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m ==1) {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:length(sampling_years)], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      #    legend("topright", inset = .05, distr.name, cex = 1.2, bty = "n",
      #              col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90")  
      tix <- sampling_years
      axis(1, at = c(1:length(sampling_years)), labels = tix)
    } else {
      frame()
      frame()
      frame()
    }
  }
}
###########################

gof_summary <- function(gof, station) {
  
  gof.table <- data.frame(gum = rep(NA,4), gam = rep(NA,4), gev = rep(NA,4), glo = rep(NA,4), pe3 = rep(NA,4))
  # gof.table <- matrix(NA,4,5)
  min.gof <- 100
  max.gof <- 0
  min.index <- c(NA, NA)
  max.index <- c(NA, NA)
  
  row.names(gof.table) <- method.name
  
  for (m in seq(along = method.name)) {
    for (d in seq(along = distr.name)) {
      gof.table[m,d] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 30, 1),
                                   count = c(1, 1, 1, 1, 1))  # in dataframe [row, column]
      if (!is.na(gof.table[m,d])) {
        if (gof.table[m,d] < min.gof) {
          min.gof <- gof.table[m,d]
          min.index <- c(m, d)
        }
        if (gof.table[m,d] > max.gof) {
          max.gof <- gof.table[m,d]
          max.index <- c(m, d)
        }
        
      }
      gof.table[m,d] <- round(gof.table[m,d], 4)
    }
  }
  
  min.gof <- round(min.gof, 4)
  
  max.gof <- round(max.gof, 4)
  
  results <- list(min_gof = min.gof, min_index = min.index, max_gof = max.gof, max_index = max.index, gof_table = gof.table)
  invisible(results)  
}


gof_summary_rperiods <- function(gof, station, r.period) {
  
  r.period.index <- which(return.periods == r.period)
  gof.table <- data.frame(gum = rep(NA,4), gam = rep(NA,4), gev = rep(NA,4), glo = rep(NA,4), pe3 = rep(NA,4))
  row.names(gof.table) <- method.name
  for (m in seq(along = method.name)) {
    for (d in seq(along = distr.name)) {
      gof.table[m,d] <- round(var.get.nc(gof_nc, gof, start = c(station, d, m, 30, 1, r.period.index),
                                         count = c(1, 1, 1, 1, 1, 1)), 0)  # in dataframe [row, column]
    }
  }
  invisible(gof.table)
  
}

# MAP OF NORWAY WITH COLOR CHANGING ACCORDING TO THE NUMBER OF DATA.
norway_map4server <- function(selected.station) {
  library(leaflet)
  library(magrittr)
  
  st.index <- which(station$number == selected.station)
  st.name <- station$name[st.index]
  st.long <- station$long[st.index]
  st.lat <- station$lat[st.index]
  st.length_rec <- station$length_rec[st.index]
  
  #   pal <- colorNumeric(
  #     palette = heat.colors(5),
  #     domain = c(0,30,60,90,120,150))
  # qpal <- colorQuantile("RdYlBu", length.bins, n = 5)
  
  my.colors <- c("black", "red", "orange", "green", "blue")
  
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c(0,30,60,90,120,150)
    color <- my.colors[trunc(x2plot/30)+1]
    invisible(color)
  }
  
  
  map <- leaflet() %>% addTiles()
  setView(map, 13, 64, zoom = 5) 
  
  addCircleMarkers(map, data = station, lng = ~ long, lat = ~ lat, 
                   popup = paste("Name:", as.character(station$name), "Number:", station$number,
                                 "Length of record:", station$length_rec, sep = " "), radius = 5, 
                   color = ~my.color.func(station$length_rec, my.colors), stroke = FALSE, fillOpacity = 0.5,
                   layerId = station$number) %>%
    
    addPopups(st.long, st.lat, paste("Name:", as.character(st.name), "Number:", selected.station,
                                     "Length of record:", st.length_rec, sep = " "),
              options = popupOptions(closeButton = FALSE, maxWidth = 100)) %>%
    
    addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
              title = "Length of flood record (years)",
              opacity = 1)
  
}


norway_map4groups <- function(group.index) {
  library(leaflet)
  library(magrittr)
  
  group.name <- station$name[group.index]
  group.nve_nb <- station$nve_nb[group.index]
  group.long <- station$long[group.index]
  group.lat <- station$lat[group.index]
  group.length_rec <- station$length_rec[group.index]
  
  my.colors <- c("black", "red", "orange", "green", "blue")
  
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c(0,30,60,90,120,150)
    color <- my.colors[trunc(x2plot/30)+1]
    invisible(color)
  }
  
  
  map <- leaflet() %>% addTiles()
  setView(map, 13, 64, zoom = 5) 
  
  addCircleMarkers(map, data = station, lng = ~ long, lat = ~ lat, 
                   popup = paste("Name:", as.character(station$name), "Number:", station$nve_nb,
                                 "Length of record:", station$length_rec, sep = " "),
                   radius = 5, 
                   color = ~my.color.func(station$length_rec, my.colors), stroke = FALSE, fillOpacity = 0.5) %>%
    
    addMarkers(group.long, group.lat, popup = paste("Name:", as.character(group.name), "Number:", group.nve_nb,
                                                    "Length of record:", group.length_rec, sep = " ")) %>%
    
    #     addPopups(group.long, group.lat, 
    #                paste("gev"),
    #               options = popupOptions(closeButton = FALSE, maxWidth = 1)) 
    
    addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
              title = "Length of flood record (years)",
              opacity = 1)
  
}


# Function to calculate the indexes of a group of common stations
bestgof.indexing <- function(gof, minmax) {
  
  bestgof.indexes <- matrix(NA,length(station$index),2)
  
  # Caution: station$index is required when reading the NetCDF files as we need to get back to the whole set of stations (445)
  for (st in seq(along = station$index)) {
    
    temp.list <- gof_summary(gof, station$index[st])
    if (minmax == "min") {
      bestgof.indexes[st,] <- temp.list$min_index
    }
    if (minmax == "max") {
      bestgof.indexes[st,] <- temp.list$max_index
    } 
    
    
  }
  invisible(bestgof.indexes)
}

# This function extracts from the matrix created above, the station indexes corresponding to the user's choices
station_group_indexes <- function(gof, distr, method, minmax) {
  
  group.indexes <- rep(NA,length(station$number))
  bestgof.indexes <- bestgof.indexing(gof, minmax)
  
  if (distr == "ALL" && method != "ALL") {
    m <- which(method.name == method)
    group.indexes <- intersect(seq(1,length(station$number)), which(bestgof.indexes[,1] == m))  
    
  }
  if (distr != "ALL" && method == "ALL") {
    d <- which(distr.name == distr)
    group.indexes <- intersect(seq(1,length(station$number)), which(bestgof.indexes[,2] == d))
  }
  if (distr == "ALL" && method == "ALL") {
    group.indexes <- seq(1,length(station$number))
  }
  if (distr != "ALL" && method != "ALL") {
    
    d <- which(distr.name == distr)
    m <- which(method.name == method)
    
    group.indexes <- intersect(which(bestgof.indexes[,2] == d), which(bestgof.indexes[,1] == m))  
  }
  invisible(group.indexes)
}

# Function for updating the df to put in the interactive table
group.dfmaker <- function(group.indexes) {
  
  group.df <- data.frame("Station name" = station$name[group.indexes], 
                         "NVE number" = station$nve_nb[group.indexes],
                         "Length of record" = station$length_rec[group.indexes])
  
  invisible(group.df)
}

stations2average <- function(min_years, max_years) {
  
  stations2ave <- c()
  print("entered the station2avergae function")
  print(min_years)
  print(max_years)
  
  print(class(max_years))
  
  min_years <- as.numeric(min_years)
  max_years <- as.numeric(max_years)
  
  print(class(max_years))
  
  for (st in seq(along = station$index)) {
    if (station$length_rec[st] >= min_years) {
      if (station$length_rec[st] <= max_years) {
        stations2ave <- c(stations2ave, st)
        print("prout1")
        
      }
    }
  }
  
  invisible(stations2ave)
}


# function like plot4server_gof but averaging over all stations filling the min/max years criteria
plot4server_gof_averaged  <- function(gof, min_years, max_years) { 
  
  # adding a new stations2ave index for the averaging
  stations2ave <- c()
  
  print("entered the plotting function")
  print(min_years)
  print(max_years)
  stations2ave <- stations2average(min_years, max_years)
  print(stations2ave)  # for debugging
  print("prout2")
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  #  maxindex2plot <- length(sampling_years) 
  maxindex2plot <- 15
  
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    temp2plot_raw <- array(NA, dim = c(length(stations2ave), 3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {  # To fix for Bayes
      method <- method.name[m] 
      
      # average over all stations that have more than 60 years of data
      #           temp2plot[m, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1), 
      #                                        count = c(1, 1, 1, length(sampling_years), 1))  
      
      
      for (st in seq(along = stations2ave)) {
        
        temp2plot_raw[st, m, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 1), 
                                             count = c(1, 1, 1, length(sampling_years), 1)) 
      }
      temp2plot[m, ] <- colMeans(temp2plot_raw[, m, ], na.rm = TRUE) 
      
      
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- maxindex2plot 
      
      
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      legend("topright", inset = .05, c("Maximum likelihood", "L-moments", "Ordinary moments" ), bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1), lwd=c(3, 3, 3), merge = TRUE, bg = "gray90", cex = 1.2)  
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
      
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
    rm(temp2plot)
    rm(temp2plot_raw)
  }
  frame()  # We fill the 6th plot with a blank plot
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),
         lwd=c(3, 3, 3), merge = TRUE, bg = "gray90")    
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    temp2plot_raw <- array(NA, dim = c(length(stations2ave), 5, length(sampling_years)))
    
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      #       
      #       temp2plot[d, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1), 
      #                                    count = c(1, 1, 1, length(sampling_years), 1))
      
      for (st in seq(along = stations2ave)) {
        temp2plot_raw[st, d, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 1), 
                                             count = c(1, 1, 1, length(sampling_years), 1)) 
      }
      
      temp2plot[d, ] <- colMeans(temp2plot_raw[, d, ], na.rm = TRUE) 
      
      # print(temp2plot)
      
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- maxindex2plot     
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m == 1) {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
      
    } else {
      frame()
      frame()
      frame()
    }
  }
}


plot4server_rlevels_coeffvar  <- function(station, gof, r.period) { 
  
  print(gof)
  print(station)
  print(r.period)
  maxindex2plot <- length(sampling_years)
  
  
  if (gof == "BS") {
    r.period.index <- which(rperiods.bs == r.period)  
  } else {r.period.index <- which(return.periods == r.period)}
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {
      method <- method.name[m] 
      # coefficient of variation = st_dev / mean
      temp2plot[m, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 2, r.period.index),
                                   count = c(1, 1, 1, length(sampling_years), 1, 1)) /
        var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index),
                   count = c(1, 1, 1, length(sampling_years), 1, 1))
    }
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))   
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      
      legend("topright", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
  }
  frame()   # We fill the 6th plot with a blank plot
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      # again coefficient of variation
      temp2plot[d, ] <- var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 2, r.period.index), 
                                   count = c(1, 1, 1, length(sampling_years), 1,1)) /
        var.get.nc(gof_nc, gof, start = c(station, d, m, 1, 1, r.period.index), 
                   count = c(1, 1, 1, length(sampling_years), 1,1))
      
    }
    if (all(is.na(temp2plot)) == FALSE) {
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- length(na.omit(temp2plot[1, ]))    
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m ==1) {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      #    legend("topright", inset = .05, distr.name, cex = 1.2, bty = "n",
      #              col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90")  
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
    } else {
      frame()
      frame()
      frame()
    }
  }
}


plot4server_rlevels_coeffvar_ave  <- function(gof, r.period, min_years, max_years) { 
  
  
  stations2ave <- stations2average(min_years, max_years)
  maxindex2plot <- 15
  
  print(gof)
  print(r.period)
  if (gof == "BS") {
    r.period.index <- which(rperiods.bs == r.period)  
  } else {r.period.index <- which(return.periods == r.period)}
  
  par(mfrow = c(3, 3), oma=c(0, 0, 0, 0), cex = 1.2)
  par(mar = c(4, 4, 2, 0))  # c(bottom, left, top, right)
  ## First subplot
  # d <- 1  # gumbel
  
  
  
  for (d in seq(along = distr.name)) {
    
    temp2plot <- array(NA, dim = c(3, length(sampling_years)))
    temp2plot_raw <- array(NA, dim = c(length(stations2ave), 3, length(sampling_years)))
    distr <- distr.name[d]
    
    for (m in 1:3) {  # TO FIX for Bayes
      method <- method.name[m] 
      
      for (st in seq(along = stations2ave)) {
        
        if (gof == "NT") {
          temp2plot_raw[st, m, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, r.period.index), 
                                               count = c(1, 1, 1, length(sampling_years), 1))
          
          
        } else {
          # coefficient of variation = st_dev / mean
          temp2plot_raw[st, m, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 2, r.period.index), 
                                               count = c(1, 1, 1, length(sampling_years), 1, 1)) /
            var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 1, r.period.index), 
                       count = c(1, 1, 1, length(sampling_years), 1, 1))
          
        }
        
        
      }
      temp2plot[m, ] <- colMeans(temp2plot_raw[, m, ], na.rm = TRUE) 
    }
    
    
    
    if (all(is.na(temp2plot)) == FALSE) {
      
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- maxindex2plot   
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      if (d == 1 || d == 4) {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "", main = paste(gof,"for", distr, sep = " "))
      } else {
        plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "", main = paste(gof,"for", distr, sep = " ")) 
      }
      
      legend("topright", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
             col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
    } else {
      frame()
      frame()
      frame()
      frame()
      frame()
    }
  }
  frame()   # We fill the 6th plot with a blank plot
  #   legend("topleft", inset = .05, c("Maximum likelihood","L-moments", "Ordinary moments" ), cex = 1.2, bty = "n",
  #          col = c("black", "red","blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  legend("center", inset = .05, distr.name, cex = 1.2, bty = "n",
         col = c("black", "red", "orange", "green", "blue"),lty = c(1, 1, 1),lwd=c(3, 3, 3), merge = TRUE, bg = "gray90") 
  
  
  ## Now we vary distr in each plot
  for (m in 1:3) {
    method <- method.name[m] 
    temp2plot <- array(NA, dim = c(5, length(sampling_years)))
    temp2plot_raw <- array(NA, dim = c(length(stations2ave), 5, length(sampling_years)))
    for (d in seq(along = distr.name)) {
      
      distr <- distr.name[d]
      # again coefficient of variation
      for (st in seq(along = stations2ave)) {
        
        if (gof == "NT") {
          
          temp2plot_raw[st, d, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, r.period.index), 
                                               count = c(1, 1, 1, length(sampling_years), 1)) 
          
        } else {
          temp2plot_raw[st, d, ] <- var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 2, r.period.index), 
                                               count = c(1, 1, 1, length(sampling_years), 1, 1)) /
            var.get.nc(gof_nc, gof, start = c(station$index[stations2ave[st]], d, m, 1, 1, r.period.index), 
                       count = c(1, 1, 1, length(sampling_years), 1, 1))     
          
        }
        
        
      }
      temp2plot[d, ] <- colMeans(temp2plot_raw[, d, ], na.rm = TRUE) 
    }
    
    if (all(is.na(temp2plot)) == FALSE) {
      ymin <- min(temp2plot, na.rm = TRUE) * 0.9
      ymax <- max(temp2plot, na.rm = TRUE) * 1.1
      xmax <- maxindex2plot    
      
      plot(temp2plot[1, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n',
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "black",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[2, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "red",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[3, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "orange",  ylab = "", xlab = "")
      par(new =TRUE)
      plot(temp2plot[4, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
           xlim = c(1, xmax), ylim = c(ymin, ymax), col = "green",  ylab = "", xlab = "")
      par(new =TRUE)
      if (m ==1) {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = gof, xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      } else {
        plot(temp2plot[5, 1:maxindex2plot], type = "l", lwd = 3, xaxt='n', 
             xlim = c(1, xmax), ylim = c(ymin, ymax), col = "blue",
             ylab = "", xlab = "Length of record", main = paste(gof,"for", method, sep = " "))
      }
      tix <- sampling_years[1:maxindex2plot]
      axis(1, at = c(1:maxindex2plot), labels = tix)
    } else {
      frame()
      frame()
      frame()
    }
  }
}


# length_dat <- vector(length = length(station.nb.vect))
# 
# for (i in seq(along = station.nb.vect)) {
#   sdat <- sdat_load(dat, station.nb.vect[i])
#   length_dat[i] <- length(sdat$flom_DOGN)
#   
# }
# hist(length_dat, breaks = 20 , xlab = "Lenth of flood record", ylab = "Number of stations", main= "", cex.lab = 1.5)
# 


########################################## TEST OF BOX PLOT FOR ALL STATIONS THAT HAVE MORE THAN N data
boxplot.param <- function(min_data, max_data) {
  
  
  
  flood_list <- list()
  station_list <- c()
  for (i in 1:length(station.nb.vect)) {
    # assign(paste("sdat",i,sep = ""), sdat_load(dat, station.nb.vect[i]) )
    if (!is.null(sdat_load(dat, station.nb.vect[i]))) {
      
      flood_list[[i]] <- sdat_load(dat, station.nb.vect[i])$flom_DOGN / mean(na.omit(sdat_load(dat, station.nb.vect[i])$flom_DOGN))
      station_list[i] <- as.character(sdat_load(dat, station.nb.vect[i])$name[1])
    } else {
      flood_list[[i]] <- NA
      station_list[i] <- NA
    }
  }
  to_plot <- c()
  k <- 0
  for (i in 1:length(station.nb.vect)) {
    
    if (length(flood_list[[i]]) > min_data && length(flood_list[[i]]) < max_data) {
      k <- k+1
      to_plot[k] <- i
    }
  }
  
  par(las = 1) # all axis labels horizontal
  boxplot(flood_list[to_plot], names = station_list[to_plot], horizontal = TRUE, outwex = TRUE)
  
  max_vect <- colMaxs(flood_list[to_plot])
  
  boxplot(sort(flood_list[to_plot], decreasing = TRUE), horizontal = TRUE)
  
}

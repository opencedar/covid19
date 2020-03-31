

###forecast function
forecast_forward <- function(x,b,l,p) {
  # x is forecast vector
  # b is beta vector
  # l is lag period
  # p is population
  for (i in 1:length(x)) {
    if(i==1) {
      forecast <- x[i]
      total <- x[i]
      next
        } else if(!is.na(x[i])) { #if there is a value in forecast, add to forecast (this is really actual) and skip forward
          forecast <- c(forecast, x[i])
          total <- total + x[i]
      next
          } else 
            {
      p_i <- total / p[i]
      al <- forecast[(i-l):(i-1)]  #new cases lag l
      b_this <- ifelse(b[i] - p_i > 0, b[i] - p_i, .01) # get elasticity
      adjuster <- seq(from = 1, to = (2^b_this)/2, length.out = l-1) # weight later cases down for smooth function
      adjuster <- 1/((2 ^ adjuster)-1)
      al <- mean(al * adjuster) #take mean of previous cases n cases adjusted down by predicted growth
      a <- al ^ b_this #growth in cases
      forecast <- c(forecast, a) #write to vector
      total <- total + a
    }
  }
  return(forecast)
}

###date projection function
date_forward <- function(x) {
  # x is days vector
  # d is date vector
  # l is lag period
  for (i in 1:length(x)) {
    if(!is.na(x[i])) { #if there is a value in date, add to date (this is really actual) and skip forward
      if(i==1) {date <- x[i]}  else {date <- c(date, x[i])} 
      next
    } else {
      next_date <- date[i-1] + 1 #next date
      date <- c(date, next_date) #write to vector
    }
  }
  return(date)
}
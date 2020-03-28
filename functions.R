

###forecast function
forecast_forward <- function(x,b,l) {
  # x is forecast vector
  # b is beta vector
  # l is lag period
  for (i in 1:length(x)) {
    if(!is.na(x[i])) { #if there is a value in forecast, add to forecast (this is really actual) and skip forward
      if(i==1) {forecast <- x[i]}  else {forecast <- c(forecast, x[i])} 
      next
    } else {
      al <- mean(forecast[(i-7):(i-1)])  #new cases lag l
      a <- al ^ b[i] #growth in cases
      forecast <- c(forecast, a) #write to vector
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
library(zoo) # for converting data to year month
library(ggplot2)
library(gridExtra)
library(forecast)

'Requires the following global variables:
- global.title
- global.xlab
- global.ylab
- global.freq
- global.pred.start
- global.pred.end
'
global.xlab = 'Time'
global.ylab = 'Value'

basictheme <- function() {
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(), 
        axis.text=element_text(color='black'),
        axis.title = element_text(colour = 'black'), 
        legend.text=element_text(),
        legend.title=element_text(), 
        legend.key = element_rect(colour = "black"))
}

plotts <- function(ts_data, colour="blue", title=NULL, ylab=NULL, xlab=NULL) {
  if (is.null(title)) {
    if (!is.null(names(ts_data))) {
      title <- names(ts_data)
    }
    else {
      title <- deparse(substitute(ts_data))
    }
  }
  ggplot(ts_data, aes(as.Date(time(ts_data)), as.matrix(ts_data))) + 
    geom_line(colour = colour)+
    ylab(global.ylab) +
    xlab(global.xlab) +
    ggtitle(title)+
    basictheme()
}


plotAcf2 <- function(ts_data, lag.max=NULL, plot=TRUE, title=NULL) {
  if (is.null(title)) {
    title <- deparse(substitute(ts_data))
  }
  acfPlot <- ggAcf(ts_data, lag.max = lag.max, plot = FALSE)
  pacfPlot <- ggPacf(ts_data, lag.max = lag.max, plot = FALSE)
  acfPlot$series <- title   # update title
  pacfPlot$series <- title  # update title
  
  if (plot) {
    acfPlot <- autoplot(acfPlot)
    pacfPlot <- autoplot(pacfPlot)
    
    grid.arrange(acfPlot, pacfPlot, ncol = 1)
  }
  else {
    newlist <- list(acfPlot, pacfPlot)
    return(newlist)
  }
  
}

plotmulti <- function(ts_all, ...) {
  pltdata <- data.frame(value=as.matrix(ts_all), date=as.Date(as.yearmon(time(ts_all))), model = "Base")
  args = list(...)
  if (!length(args) == 0) {
    for(i in seq(1, length(args), by=2)) {
      name = toString(args[[i+1]])
      ts_data = ts(args[[i]], start=c(global.start), end=c(global.end), frequency=global.freq)
      df <- data.frame(value=as.matrix(ts_data), date=as.Date(as.yearmon(time(ts_data))), model = name)
      pltdata <- rbind(pltdata, df)
    }
  }
  ggplot(pltdata, aes(date, value, colour = model)) +
    geom_line()+
    ylab(global.ylab)+
    xlab(global.xlab)+
    ggtitle(global.title)+
    theme(axis.line = element_line(), axis.text=element_text(color='black'),
          axis.title = element_text(colour = 'black'), legend.text=element_text(),
          legend.title=element_text(), legend.key = element_rect(colour = "black"))
}

plotforecast <- function(ts_all, ...) {
  pltdata <- data.frame(value=as.matrix(ts_all), date=as.Date(as.yearmon(time(ts_all))), model = "Base")
  args = list(...)
  if (!length(args) == 0) {
    for(i in seq(1, length(args), by=2)) {
      name = toString(args[[i+1]])
      forecast = ts(args[[i]], start=c(global.pred.start), end=c(global.pred.end), frequency=global.freq)
      df <- data.frame(value=as.matrix(forecast), date=as.Date(as.yearmon(time(forecast))), model = name)
      pltdata <- rbind(pltdata, df)
    }
  }
  ggplot(pltdata, aes(date, value, colour = model)) +
    geom_line()+
    geom_vline(aes(xintercept = as.numeric(date[length(ts_all)])),
               linetype = "longdash", color = "black")+
    ylab("")+
    xlab("")+
    ggtitle(global.title)+
    basictheme()
}
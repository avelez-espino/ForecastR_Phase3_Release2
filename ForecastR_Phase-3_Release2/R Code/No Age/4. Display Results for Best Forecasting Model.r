
###
### Histogram of Bootstrap Predictions: Best Model for Stock with No Age Information
###

# pred.int.individual.stock.naiveone.no.age
# pred.int.individual.stock.avgthree.no.age
# pred.int.individual.stock.avgfive.no.age
# pred.int.individual.stock.arima.no.age
# pred.int.individual.stock.expsmooth.no.age

###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### Come back here - January 2, 2014 

## best.index.no.age <- table.rank.rmse.results.no.age$index.min.avg.rank

plot.yboot.best.model.no.age <- function(pred.args, 
                                         best.index.no.age,
                                         stockabundance){

    .e = environment()

     ## pred.int.individual.stock.naiveone.no.age$sim

     y.star.boot.stacked <- pred.args[[best.index.no.age]]$sim
        
     tmplabel <- class(pred.args[[best.index.no.age]])
     
     if (tmplabel=="naiveone") {
              mylabel <- paste0(stockabundance,": ", "Naive Model (Previous Year)")
     } else if (tmplabel=="avgthree") {
              mylabel <- paste0(stockabundance,": ", "Naive Model (Average of Previous Three Years)")
     } else if (tmplabel=="avgfive") {
              mylabel <- paste0(stockabundance,": ", "Naive Model (Average of Previous Five Years)")
     } else if (tmplabel=="arima") {
              mylabel <- paste0(stockabundance,": ", "ARIMA Model")
     } else {
              mylabel <- paste0(stockabundance,": ", "Exponential Smoothing Model")
     }
     
        
    labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

    data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked, stringsAsFactors=FALSE)
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))


    usePackage("ggplot2")
    usePackage("scales")

    d <- data.stacked

    l <- levels(d$labels)

    h <- hist(data.stacked$y.star.boot, plot=FALSE, breaks = "Freedman-Diaconis")
    h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks))) 
    ## breaks <- h$breaks
    breaks <- h.tmp

    ## g <- ggplot(d, aes(x=residuals), environment=.e) +
     g <- ggplot(d, aes(x=y.star.boot), environment=.e) +
           geom_histogram(data=d, breaks=breaks, fill="wheat",colour="black") +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               ## expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Bootstrapped Point Forecasts"),labels=comma)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))



      tmp <- pred.args[[best.index.no.age]]$PI.ctr   ## extract point forecast produced by best forecasting model 
      clim <- round(tmp)


     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)


     #----
     
     x <- max(0,pred.args[[best.index.no.age]]$PI.lwr)
     xend <-  round(pred.args[[best.index.no.age]]$PI.upr)
     y <- 0
     yend <- 0

     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)

     return(g)
}



best.index.no.age <- table.rank.rmse.results.no.age$index.min.avg.rank

plot.yboot.best.model.no.age(pred.args,
                             best.index.no.age,
                             stockabundance)


#====================================================================================================================
#
# Empirical Probabilities for Stock With No Age Information: Best Model
#
#====================================================================================================================

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts
###

# pred.int.individual.stock.naiveone.no.age
# pred.int.individual.stock.avgthree.no.age,
# pred.int.individual.stock.avgfive.no.age,
# pred.int.individual.stock.arima.no.age,
# pred.int.individual.stock.expsmooth.no.age

empirical.probability.yboot.best.model.no.age <- function(pred.args,
                                                          best.index.no.age,
                                                          stockabundance){
 
    y.star.boot.stacked <- pred.args[[best.index.no.age]]$sim
     
    tmplabel <- class(pred.args[[best.index.no.age]])
    mylabel <- paste(stockabundance, ": ",  tmplabel)
        
    labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

    data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked, stringsAsFactors=FALSE)
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels)) 

    pfct <- pred.args[[best.index.no.age]]$PI.ctr ## point forecast of abundance

   
    ## cumulative probabilities evaluated for the endpoints of equal-sized bins covering the
    ## range of bootstrapped point forecasts

    x <- as.numeric(y.star.boot.stacked)
    usePackage("Hmisc")

    x.hist <- hist(x, breaks=20, plot=FALSE)
    x.hist.breaks <- x.hist$breaks

    my.ecdf <- ecdf(x)

    prob.less <- round(my.ecdf(x.hist.breaks),4)

    prob.greater <- round(1 - my.ecdf(x.hist.breaks),4)

    prob.less.percentage <- round(prob.less*100,2)

    prob.greater.percentage <- round(prob.greater*100,2)

    prob.threshold <- x.hist.breaks

    prob.thresholds <- data.frame(prob.threshold = prob.threshold,
                       prob.less.percentage = prob.less.percentage,
                       prob.greater.percentage = prob.greater.percentage)

    prob.thresholds

    ## cumulative probabilities evaluated for the point forecast of total abundance

    prob.less.pfct <- round(my.ecdf(pfct),4)
    prob.greater.pfct <- round(1 - my.ecdf(pfct),4)

    prob.less.percentage.pfct <- round(prob.less.pfct*100,2)

    prob.greater.percentage.pfct <- round(prob.greater.pfct*100,2)

    prob.threshold.pfct <- pfct

    prob.pfct <-  data.frame(prob.threshold = prob.threshold.pfct,
                             prob.less.percentage = prob.less.percentage.pfct,
                             prob.greater.percentage = prob.greater.percentage.pfct)


    prob.pfct

    probs = list(prob.thresholds=prob.thresholds,
                 prob.point.forecast=prob.pfct)

    probs

}


emp.prob.best.model.no.age <- empirical.probability.yboot.best.model.no.age(pred.args,
                                                                            best.index.no.age,
                                                                            stockabundance)




###
### Histogram of Bootstrap Predictions: All Models for Stock with No Age Information
###

# pred.int.individual.stock.naiveone.no.age
# pred.int.individual.stock.avgthree.no.age
# pred.int.individual.stock.avgfive.no.age
# pred.int.individual.stock.arima.no.age
# pred.int.individual.stock.expsmooth.no.age

plot.yboot.all.models.no.age <- function(pred.args,
                                         stockabundance){

    .e = environment()

    y.star.boot.stacked <- NULL
    labels.stacked <- NULL
    for (k in 1:length(pred.args)) {
    
        y.star.tmp <- pred.args[[k]]$sim
        y.star.boot.stacked <- c(y.star.boot.stacked, y.star.tmp)
    
         tmplabel <- class(pred.args[[k]])
         
         if (tmplabel=="naiveone") {
              mylabel <- paste0(stockabundance,": ", "Naive Model (Previous Year)")
         } else if (tmplabel=="avgthree") {
              mylabel <- paste0(stockabundance,": ", "Naive Model (Average of Previous Three Years)")
         } else if (tmplabel=="avgfive") {
              mylabel <- paste0(stockabundance,": ", "Naive Model (Average of Previous Five Years)")
         } else if (tmplabel=="arima") {
              mylabel <- paste0(stockabundance,": ", "ARIMA Model")
         } else {
              mylabel <- paste0(stockabundance,": ", "Exponential Smoothing Model")
         }
         
         
         labels.rep <- rep(mylabel, length(y.star.tmp)) 
         
         labels.stacked <- c(labels.stacked, labels.rep)
         
     
    }


    data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked, stringsAsFactors=FALSE)
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)
    
    ## l <- c(l[5], l[3], l[4], l[1], l[2])

    ## d$labels <- factor(d$labels, levels=l)
    
    ## levels(d$labels)

    data.stacked$labels <- factor(data.stacked$labels, levels=l)

    h <- hist(data.stacked$y.star.boot,plot=FALSE, breaks = "Freedman-Diaconis")
    h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks)))  
    ## breaks <- h$breaks
    breaks <- h.tmp

    ## g <- ggplot(d, aes(x=residuals), environment=.e) +
     g <- ggplot(d, aes(x=y.star.boot)) +
           geom_histogram(data=d, breaks=breaks, fill="wheat",colour="black") +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               ## expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Bootstrapped Point Forecasts"),labels=comma)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
      
     
     clim <- NULL
     for (k in 1:length(pred.args)) {
     
         clim.tmp <- pred.args[[k]]$PI.ctr
         clim.tmp <- round(clim.tmp)
         clim <- c(clim, clim.tmp)
     }
     
     
     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)


     x <- NULL
     xend <- NULL
     y <- NULL
     yend <- NULL 
     for (k in 1:length(pred.args)){
     
         x.tmp <- max(0,pred.args[[k]]$PI.lwr)
         x <- c(x,x.tmp)
         
         xend.tmp <- round(pred.args[[k]]$PI.upr)  
         xend <- c(xend,xend.tmp)
     
         y.tmp <- 0
         y <- c(y, y.tmp)
         
         yend.tmp <- 0
         yend <- c(yend,yend.tmp)
          
     }

   

     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)


     return(g)
}


plot.yboot.all.models.no.age(pred.args,
                             stockabundance)
                             
                             
###
### plot forecast vs. actual: best model (stock without age information)
###

plot.results.afe.individual.stock.retro.best.model.no.age <- function(retro.args,
                                                                      best.index.no.age){

    .e = environment()

    usePackage("stringr")

    forecasted.stacked <- NULL
    actual.stacked <- NULL
    title.stacked <- NULL
    labels.stacked <- NULL

    ## i <- best.index.no.age

    datatmp <- retro.args[[best.index.no.age]][[1]]   
  
    names(datatmp)[names(datatmp)=="a"] <- "Actual"
    names(datatmp)[names(datatmp)=="p"] <- "Forecast"
    names(datatmp)[names(datatmp)=="e"] <- "Error"

    ## r.sq <- summary(lm(Forecast ~ Actual, data=datatmp))$r.squared
    ## r.sq <-  sprintf("%.2f", r.sq*100)

    names.retro.args <-  names(retro.args)[best.index.no.age] 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args


    tmplabel <- names.retro.args  
    if (tmplabel=="naiveone") {
              mytitle <- "Naive Model (Previous Year)"
    } else if (tmplabel=="avgthree") {
              mytitle <- "Naive Model (Average of Previous 3 Years)"
    } else if (tmplabel=="avgfive") {
              mytitle <- "Naive Model (Average of Previous 5 Years)"
    } else if (tmplabel=="arima") {
              mytitle <- "ARIMA Model"
    } else {
              mytitle <- "Exponential Smoothing Model"
    }
          
    ## mytitle <- paste(mytitle, "\n","R-squared = ", r.sq , "%", sep="")

    usePackage("calibrate")
    labs <- substr(datatmp$cy,  # return year (or, equivalently, calendar year)
                      ## datatmp$cy - myage ,   # brood year
                   start=3, stop=4)


    forecasted.stacked <- c(forecasted.stacked, datatmp$Forecast)

    actual.stacked <- c(actual.stacked, datatmp$Actual)

    title.stacked <- c(title.stacked, rep(mytitle, length(datatmp$Actual)))
    labels.stacked <- c(labels.stacked, labs)


    data.stacked <- data.frame(forecasted=forecasted.stacked,
                               actual=actual.stacked,
                               title=title.stacked,
                               labels=labels.stacked, stringsAsFactors=FALSE)
                               
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))


    usePackage("ggplot2")
    usePackage("scales")

    ## environment=.e
    g <- ggplot(data.stacked, aes(actual,forecasted), environment=.e)  +
      ## g <- ggplot(data.stacked, aes(actual,forecasted)) +
       geom_abline(intercept=0,slope=1,colour="red",size=0.8) +
       geom_text(aes(label=labels),col="blue",size=3) +
            coord_fixed(ratio=1) +
              facet_wrap(~title, scales="fixed") +
                ### xlim(0, max(data.stacked$actual, data.stacked$forecasted, na.rm=TRUE)) + 
                ### ylim(0, max(data.stacked$actual, data.stacked$forecasted, na.rm=TRUE)) +
                ## expand_limits(x=0, y=0) +
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma) +
                   scale_y_continuous(paste("Forecasted", stockabundance, "Values"),labels=comma, 
                                      limits=c(0, max(data.stacked$actual, data.stacked$forecasted, na.rm=TRUE)), 
                                      breaks=pretty(c(0, max(data.stacked$actual, data.stacked$forecasted, na.rm=TRUE)))) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous(paste("Actual", stockabundance, "Values"),labels=comma, 
                                       limits=c(0, max(data.stacked$actual, data.stacked$forecasted, na.rm=TRUE)), 
                                       breaks=pretty(c(0, max(data.stacked$actual, data.stacked$forecasted, na.rm=TRUE)))) +
                   coord_fixed(ratio=1)


      # to expand plot axes, build the forecasted ranges in a data.frame and add a new layer to the plot using geom_blank

      ## g <- g + geom_rect(data = subset(data.stacked, title == levels(title)[1]), aes(fill = "grey"),
      ##     xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf, alpha = 0.05)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text = element_text(size=9))


      return(g)

}


plot.results.afe.individual.stock.retro.best.model.no.age(retro.args,
                                                          best.index.no.age)





#*******************************************************************************
# Time series plot of retrospectively forecasted vs. actual abundance (total age, expsmooth)
#
#*******************************************************************************

timeseries.plot.results.afe.individual.stock.retro.best.model.no.age <- function(retro.args,
                                                                                  best.index.no.age, 
                                                                                    stockabundance){

    .e = environment()
      
      
    usePackage("stringr")

    forecasted.stacked <- NULL
    actual.stacked <- NULL
    title.stacked <- NULL
    labels.stacked <- NULL

    ## i <- best.index.no.age

    datatmp <- retro.args[[best.index.no.age]][[1]]   
  
    names(datatmp)[names(datatmp)=="a"] <- "Actual"
    names(datatmp)[names(datatmp)=="p"] <- "Forecast"
    names(datatmp)[names(datatmp)=="e"] <- "Error"

    ## r.sq <- summary(lm(Forecast ~ Actual, data=datatmp))$r.squared
    ## r.sq <-  sprintf("%.2f", r.sq*100)

    names.retro.args <-  names(retro.args)[best.index.no.age] 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args


    tmplabel <- names.retro.args  
    if (tmplabel=="naiveone") {
              mytitle <- "Naive Model (Previous Year)"
    } else if (tmplabel=="avgthree") {
              mytitle <- "Naive Model (Average of Previous 3 Years)"
    } else if (tmplabel=="avgfive") {
              mytitle <- "Naive Model (Average of Previous 5 Years)"
    } else if (tmplabel=="arima") {
              mytitle <- "ARIMA Model"
    } else {
              mytitle <- "Exponential Smoothing Model"
    }
          
    ## mytitle <- paste(mytitle, "\n","R-squared = ", r.sq , "%", sep="")

    labs <- datatmp$cy  ## calendar year

    forecasted.stacked <- c(forecasted.stacked, datatmp$Forecast)

    actual.stacked <- c(actual.stacked, datatmp$Actual)

    title.stacked <- c(title.stacked, rep(mytitle, length(datatmp$Actual)))
    labels.stacked <- c(labels.stacked, labs)


    data.stacked <- data.frame(forecasted=forecasted.stacked,
                               actual=actual.stacked,
                               title=title.stacked,
                               labels=labels.stacked, stringsAsFactors=FALSE)
                               
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    data.stacked$labels  <- as.numeric(levels(data.stacked$labels))[data.stacked$labels] 

    ############################################################################
    
  
    ## data.stacked <- data.frame(forecasted=data$Forecast,
    ##                           actual=data$Actual, 
    ##                           labels=labs, 
    ##                           ## age=paste("Total ", stockabundance, ":  ", "R-squared = ",r.sq,"%",sep="")
    ##                           age=paste("Total ", stockabundance, sep="")
    ##                            )
    
    usePackage("scales")
    usePackage("ggplot2")
 
    g <- ggplot(data.stacked, aes(labels,actual), environment=.e) +
          ## ggplot(data.stacked, aes(labels,actual)) + 
          geom_line(aes(labels, actual, colour="Actual"),size=0.8) +
           geom_line(aes(labels, forecasted, colour="Forecasted"), size=0.8) + 
            geom_point(aes(labels, actual, colour="Actual"),size=2.5) +
             geom_point(aes(labels, forecasted, colour="Forecasted"), size=2.5) + 
              ## facet_wrap(~age, scales="free",ncol=1) +    
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) + 
                  scale_y_continuous(paste("Retrospectively Forecasted", stockabundance, "Values"),labels=comma,
                                     limits=c(0,max(data.stacked$actual,data.stacked$forecasted)), 
                                     breaks=pretty(c(0,max(data.stacked$actual,data.stacked$forecasted)))) +  
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                   scale_x_continuous("Return Year", breaks=seq(min(data.stacked$labels),max(data.stacked$labels),by=1)) + 
                  ## ggtitle(paste(stockname, "Stock")) +                                          
                 scale_color_manual(name=paste0(stockabundance), values=c("Actual"="blue2", "Forecasted"="red2")) +                            
                 theme_bw() +  
                  theme( plot.title=element_text(size=12, hjust=0.5),
                         axis.title.x=element_text(size=10,vjust=-0.5),
                         axis.title.y=element_text(size=10,vjust=1.5),
                         axis.text.y=element_text(size=8),
                         axis.text.x = element_text(angle = 90, vjust = 0.5,size=8), 
                         legend.position="top")
                           
      return(g)
  

}


timeseries.plot.results.afe.individual.stock.retro.best.model.no.age(retro.args,
                                                                      best.index.no.age, 
                                                                        stockabundance)


## results <- results.total.age.retro.predictive.performance.expsmooth
## timeseries.plot.results.afe.total.age.retro.expsmooth(results, stockabundance)




##===============================================================================================
## Bias Coefficient Plots 
##===============================================================================================


plot.bias.coefficients.retrospective.forecast.errors.individual.stock.best.model.no.age <- function(rmse.results.no.age.args,  retro.args, best.index.no.age){

    datatmp <- retro.args[[best.index.no.age]][[1]]   
  
    names(datatmp)[names(datatmp)=="a"] <- "Actual"
    names(datatmp)[names(datatmp)=="p"] <- "Forecast"
    names(datatmp)[names(datatmp)=="e"] <- "Error"

    names.retro.args <-  names(retro.args)[best.index.no.age] 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args

    tmplabel <- names.retro.args  
    if (tmplabel=="naiveone") {
              mytitle <- "Naive Model (Previous Year)"
    } else if (tmplabel=="avgthree") {
              mytitle <- "Naive Model (Average of Previous 3 Years)"
    } else if (tmplabel=="avgfive") {
              mytitle <- "Naive Model (Average of Previous 5 Years)"
    } else if (tmplabel=="arima") {
              mytitle <- "ARIMA Model"
    } else {
              mytitle <- "Exponential Smoothing Model"
    }
           

    data.stacked <- data.frame(errors=datatmp$Error,
                               labels=mytitle, stringsAsFactors=FALSE)
    data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

    d <- data.stacked
    l <- levels(d$labels)
    d$labels <- factor(d$labels, levels=l)


    par(mfrow=c(1,1), mar=c(2,2,2,2), cex.main=0.9)
 
    data <- d

    names(data)[names(data)=="errors"] <- "Error"

    error <- data$Error

    mre.error <- mre(error)

    ## plot.mre(mre.error)
        
    ## gamma <- Arg(mre.error)
        
    ## bias <- 1 - 4*gamma/pi
        
    ## k <- length(bias)

    bias.coeff.updated(mre.error, outplot=2)

    bias.coeff.afe.individual.stock.retro.best.model.no.age <<- bias.coeff.updated(mre.error, outplot=0)

    tmp <- paste(stockabundance, ": ", l, sep="")
        
    title(main=paste0(tmp))
     
    usePackage("gridGraphics")

    grid.echo()       
                                
    grid.grab() -> mapgrob

    return(mapgrob)
   
}

windows()
plot.bias.coefficients.retrospective.forecast.errors.individual.stock.best.model.no.age (
  rmse.results.no.age.args,  
  retro.args, 
  best.index.no.age)



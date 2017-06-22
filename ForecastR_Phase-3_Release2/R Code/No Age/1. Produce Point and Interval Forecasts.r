########################################################################################################
########################################################################################################
#
# Naive Time Series Forecasting (Average of Past 5 Years) - Forecasting Results for Youngest Age
#
########################################################################################################
########################################################################################################


datafile$Forecasting_Year[1] <- as.numeric(gsub("\\s", "", datafile$Forecasting_Year[1]))
datafile$Forecasting_Year <- as.numeric(datafile$Forecasting_Year)

datafile_original <- datafile


stockname <- datafile$Stock_Name[1]
stockabundance <- datafile$Stock_Abundance[1]
stockspecies <- datafile$Stock_Species[1]
forecastingyear <- datafile$Forecasting_Year[1]


                                            
#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for naive forecasting (average of past 5 years) ---------------------------------

datalist.no.age <- function(datafile, forecastingyear, stockabundance) {

    extr_abundance <- paste0("Average_",stockabundance)
    
    ## usePackage("stringr")
    
    extr_abundance <- stringr::str_replace_all(extr_abundance, " ", "_")
    
    ##
    myvars <- names(datafile)[names(datafile) %in% c("Run_Year",extr_abundance)]
    
    data <- subset(datafile, select=myvars)
    
    return(data)

}


datalist <- datalist.no.age(datafile, forecastingyear, stockabundance)  # CY refers to the T variable with highest age


#--------- plot data to be used for naive forecasting (average of past 5 years) (uses ggplot) ---------------------------

plot.data.no.age <- function(datalist){

     # par(mfrow=c(length(datalist),1), mar=c(4,6,2,2))

     x.stacked <- datalist[,1]
     
     y.stacked <- datalist[,2]

     age.stacked <-  rep(paste(stockabundance,"vs.","Run Year"), length(y.stacked))


     data.stacked <- data.frame(x=x.stacked,y=y.stacked,age=age.stacked)

     usePackage("ggplot2")
     usePackage("scales")

     ggplot(data.stacked, aes(x,y)) +
      geom_line(colour="dodgerblue3") +
        geom_point(col="dodgerblue3") +
          facet_wrap(~age, ncol=1, scales="free") +
            xlab("Run Year") +
              # scale_y_continuous("Terminal Run",labels=comma) +
               scale_y_continuous(paste(stockabundance),labels=comma) +
                theme_bw() +
                   theme(plot.title=element_text(size=12, hjust=0.5),
                         axis.title.x = element_text(size=10,vjust=-0.5),
                         axis.title.y = element_text(size=10,vjust=1),
                         axis.text.x = element_text(size=8),
                         axis.text.y = element_text(size=8)
                         )

}

## plot.data.no.age(datalist)


###############  naiveone #########################################################################################################################


#---------  fit naive model (previous year) -----------------------------------------

naiveone.model.no.age <- function(datalist){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "naiveone"  # create a new class

     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     ## usePackage("forecast")

     ## output$model <- forecast::rwf(series, h=1, drift=FALSE, level=0.80)

     output$model <- forecast::rwf(series, h=1, drift=FALSE)

     output$original.data <- datalist

     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}


if (noagemodelnaiveone) {  #naiveone 

      fit.naiveone.model.no.age <- naiveone.model.no.age(datalist)

}



#---------  Plot naive model (previous year) --------------------------------
# Plot fitted naive model (ggplot)
#-------------------------------------------------------------------------------

plot.fitted.naiveone.no.age <- function(fit){

    .e <- environment()

    naiveonefit <- fit
    CY <- naiveonefit$model.data[,"Run_Year"]

    naiveonemodel <- fit$model

    year <- CY
    actual <- as.numeric(naiveonemodel$x)
    fitted <- as.numeric(naiveonemodel$fitted)
    
    age.stacked <- rep("Naive Model (Previous Year)", length(fitted))

    year.stacked <- year
    actual.stacked <- actual
    fitted.stacked <- fitted

    data.stacked.1 <- data.frame(year=year.stacked, actual=actual.stacked)  
    data.stacked.2 <- data.frame(year=year.stacked, fitted=fitted.stacked)
    
    names(data.stacked.1) <- c("year","value")
    names(data.stacked.2) <- c("year","value")
    
    
    data.stacked <- rbind.data.frame(data.stacked.1, data.stacked.2)
    
    data.stacked$label <- c(rep("Actual Values",nrow(data.stacked.1)),
                            rep("Fitted Values",nrow(data.stacked.2)))
    data.stacked$age <- c(rep("Naive Model (Previous Year)",nrow(data.stacked)))                         
                             
   

    usePackage("ggplot2")
    usePackage("scales")

    ## usePackage("reshape")
    ## dd = melt(data.stacked, id=c("year"))
    ## dd$variable <- ifelse(dd$variable=="actual", "Actual Values",
    ##                       "Fitted Values")

    g <- ggplot(data.stacked) +
          facet_wrap(~age,ncol=1, scales="free_y") + 
          geom_line(aes(x=year, y=value, colour=label), size=0.6) +
                  scale_x_continuous("Return Year") +
             scale_y_continuous(paste(stockabundance),labels=comma) +
              theme_bw() +
               theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x = element_text(size=10,vjust=-0.5),
                axis.title.y = element_text(size=10,vjust=1.5),
                 axis.text.x = element_text(size=8),
                  axis.text.y = element_text(size=8),
                   legend.position = "top", legend.direction = "horizontal"
                    ) +
                    scale_colour_manual("",
                      values = c("dodgerblue3", "lightsalmon1"))

    # plot.title=element_text(family="Times", face="bold", size=20)
    # 


    g
}



## options(warn=-1)
## plot.fitted.naiveone.no.age(fit.naiveone.model.no.age)
## options(warn=0)


#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

point.forecast.naiveone.no.age <- function(datalist, fit, forecastingyear){

     PSY <- forecastingyear

     output <- list()

     fit$model

     model <- fit$model

     naiveonefit <- fit$model

     output$Model <- "Naive (Previous Year)"

     output$RY <- PSY

     # output[[j]]$p <- as.numeric(predict(model, h=1, level=0.80)$pred)

     output$p <-  as.numeric(naiveonefit$mean)

     return(output)
}



## point.forecast.naiveone.no.age(datalist, fit.naiveone.model.no.age, forecastingyear)

if (noagemodelnaiveone){  # naiveone

    tmp_list <- point.forecast.naiveone.no.age(datalist, fit.naiveone.model.no.age, forecastingyear)

    tmp_df <- do.call(cbind.data.frame, tmp_list)

    results.point.forecast.naiveone.no.age <- tmp_df

    results.point.forecast.naiveone.no.age$Model <- as.character(results.point.forecast.naiveone.no.age$Model)

}



#---- meboot2 function for bootstrapping positive time series ----------------------------------------

meboot2 <- function (x, reps = 9999, trim = 0.1, reachbnd = FALSE, expand.sd = FALSE,

    force.clt = FALSE, scl.adjustment = FALSE, sym = FALSE, elaps = FALSE,

    colsubj, coldata, coltimes, ...)

{

    if ("pdata.frame" %in% class(x)) {

        res <- meboot.pdata.frame(x, reps, trim, reachbnd, expand.sd,

            force.clt, scl.adjustment, sym, elaps, colsubj, coldata,

            coltimes, ...)

        return(res)

    }

    ptm1 <- proc.time()

    n <- length(x)

    xx <- sort(x)

    ordxx <- order(x)

    if (sym) {

        xxr <- rev(xx)

        xx.sym <- mean(xx) + 0.5 * (xx - xxr)

        xx <- xx.sym

    }

    z <- rowMeans(embed(xx, 2))

    dv <- abs(diff(as.numeric(x)))

    dvtrim <- mean(dv, trim = trim)

    xmin <- 0

    xmax <- xx[n] + dvtrim

    aux <- colSums(t(embed(xx, 3)) * c(0.25, 0.5, 0.25))

    desintxb <- c(0.75 * xx[1] + 0.25 * xx[2], aux, 0.25 * xx[n -

        1] + 0.75 * xx[n])

    ensemble <- matrix(x, nrow = n, ncol = reps)

    ensemble <- apply(ensemble, 2, meboot.part, n, z, xmin, xmax,

        desintxb, reachbnd)

    qseq <- apply(ensemble, 2, sort)

    ensemble[ordxx, ] <- qseq

    if (expand.sd)

        ensemble <- expand.sd(x = x, ensemble = ensemble, ...)

    if (force.clt)

        ensemble <- force.clt(x = x, ensemble = ensemble)

    if (scl.adjustment) {

        zz <- c(xmin, z, xmax)

        v <- diff(zz^2)/12

        xb <- mean(x)

        s1 <- sum((desintxb - xb)^2)

        uv <- (s1 + sum(v))/n

        desired.sd <- sd(x)

        actualME.sd <- sqrt(uv)

        if (actualME.sd <= 0)

            stop("actualME.sd<=0 Error")

        out <- desired.sd/actualME.sd

        kappa <- out - 1

        ensemble <- ensemble + kappa * (ensemble - xb)

    }

    else kappa <- NULL

    if (is.ts(x)) {

        ensemble <- ts(ensemble, frequency = frequency(x), start = start(x))

        dimnames(ensemble)[[2]] <- paste("Series", 1:reps)

    }

    ptm2 <- proc.time()

    elapsr <- elapsedtime(ptm1, ptm2)

    if (elaps)

        cat("\n  Elapsed time:", elapsr$elaps, paste(elapsr$units,

            ".", sep = ""), "\n")

    list(x = x, ensemble = ensemble, xx = xx, z = z, dv = dv,

        dvtrim = dvtrim, xmin = xmin, xmax = xmax, desintxb = desintxb,

        ordxx = ordxx, kappa = kappa, elaps = elapsr)

}


#*******************************************************************************************
#
#------------ compute prediction intervals for point forecasts of individual ages -----------
#
#*******************************************************************************************

## fit=fit.naiveone.model.no.age; level=80; npaths=B
                                                  
## meboot2 bootstrap for a specific age

forecast.naiveone.modified.no.age <- function(fit, bootmethod, level=80, npaths=B){

   series <- fit$model.data[,ncol(fit$model.data)]

   usePackage("meboot")
   usePackage("forecast")

   if (bootmethod=="meboot") {

      trim.values <- seq(0, 0.4, by=0.05)
     
      mean.series.meboot2 <- NULL
      for (k in 1:length(trim.values)) {
        set.seed(k)
        series.meboot2 <- meboot2(series, reps=npaths, trim=trim.values[k])$ensemble
        mean.series.meboot2 <- c(mean.series.meboot2, mean(series.meboot2))
    
      }
    
      diff.mean.series.meboot2 <- mean.series.meboot2 - mean(series)

      abs.diff.mean.series.meboot2 <- abs(diff.mean.series.meboot2)
    
      trim.optimal <- trim.values[which.min(abs.diff.mean.series.meboot2)]
    
      ## series.meboot2 <- meboot2(series, reps=npaths, trim=trim)$ensemble
    
      series.meboot2 <- meboot2(series, reps=npaths, trim=trim.optimal)$ensemble

      mean(series)
      mean(series.meboot2)

      series.boot.forecast.naiveone <- series.meboot2[nrow(series.meboot2),]  ## carry the last value of each bootstrapped time series forward
                                                                           ## to obtain the one-year ahead forecast
      y.paths <- series.boot.forecast.naiveone   ## one-year ahead forecasts

   }

   if (bootmethod=="stlboot") {

      series <- ts(series, 
                 start = min(fit$model.data$Run_Year), 
                 end = max(fit$model.data$Run_Year), 
                 frequency=1)
 
      series.stlboot <- stlboot(series, k=npaths, outplot=FALSE)
      
      series.boot.forecast.naiveone <- series.stlboot[nrow(series.stlboot),]  ## carry the last value of each bootstrapped time series forward
                                                                           ## to obtain the one-year ahead forecast
      y.paths <- series.boot.forecast.naiveone   ## one-year ahead forecasts

   }

   ## series.boot.forecast.naiveone <- NULL

   ## require(foreach)

   ## foreach (k=1:ncol(series.meboot2)) %do% {
   ##
   ##     ## print(k)
   ##     series.boot <- series.meboot2[,k]
   ##
   ##     ### series.boot <- tsclean(series.boot)
   ##     ## series.boot.forecast <- rwf(series.boot,h, drift=FALSE, level=level)$mean
   ##     series.boot.forecast <- series.boot[length(series.boot)]  ## carry the last value of each bootstrapped time series forward
   ##                                                               ## to obtain the one-year ahead forecast
   ##
   ##     series.boot.forecast.naiveone <- c(series.boot.forecast.naiveone, series.boot.forecast)
   ## }

  

   ## mean(series.boot.forecast.naiveone)

   ## rwf(series,h, drift=FALSE, level=level)$mean

   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   out <- NULL

   out$mean <-  as.numeric(forecast::rwf(series,h=1, drift=FALSE, level=level)$mean)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   
   if (bootmethod=="meboot") {
       out$ensemble <- series.meboot2
   } 
   
   if (bootmethod=="stlboot") {
       out$ensemble <- series.stlboot
   } 
   
   return(out)

}


## forecast.naiveone.modified.no.age(fit=fit.naiveone.model.no.age, bootmethod, level=80, npaths=B)


prediction.intervals.individual.stock.naiveone.no.age <- function(fit, bootmethod, level=80, npaths=B){

     ## h <- 1  # one step ahead forecasts

     naiveone.fit <- fit

     naiveoneboot <- forecast.naiveone.modified.no.age(naiveone.fit, bootmethod, level=level, npaths=npaths)

     naiveone.point.forecast <- naiveoneboot$mean

     fit$naiveone.point.forecast <- naiveone.point.forecast

     naiveone.lwr.forecast <- naiveoneboot$lower
     # avgfive.median.forecast <- median(avgfiveboot$sim)
     naiveone.upr.forecast <- naiveoneboot$upper

     fit$PI.ctr <- naiveone.point.forecast
     fit$PI.lwr <- naiveone.lwr.forecast
     fit$PI.upr <- naiveone.upr.forecast

     fit$sim <- naiveoneboot$sim
     fit$ensemble <- naiveoneboot$ensemble

     results <- fit

     return(results)
}


if (noagemodelnaiveone) {  # naiveone
   
   pred.int.individual.stock.naiveone.no.age <- prediction.intervals.individual.stock.naiveone.no.age(fit.naiveone.model.no.age, bootmethod, level=80, npaths=B)

}

##############  avgthree ##########################################################################################################################


###############  avgthree #########################################################################################################################


#--------- helper function for computing the average of the past 3 years of a time series -----

avgthree.no.age  <- function(series) {

     # n = length of time series
     # n must be strictly greater than 3!!

     n <- length(series)

     fitted <- rep(NA,n)
     residuals <- rep(NA,n)

     for (k in 4:n) {
          fitted[k] <- (series[k-3] + series[k-2] + series[k-1])/3
          residuals[k] <- series[k] - fitted[k]
     }

     fcast <- (series[n-2] + series[n-1] + series[n])/3   # point-forecast for series[n+1]

     out <- list()
     out$x <- series
     out$mean <- fcast
     out$fitted <- fitted
     out$residuals <- residuals

     return(out)

}


#---------  fit naive model (average of past 3 years) -----------------------------------------


avgthree.model.no.age <- function(datalist){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "avgthree"  # create a new class

     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     usePackage("forecast")

     usePackage("stringr")

     output$model <- avgthree.no.age(series)

     output$original.data <- datalist

     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}


if (noagemodelavgthree){  ## avgthree
 
   fit.avgthree.model.no.age <- avgthree.model.no.age(datalist)

}

#---------  Plot naive model (average of past 3 years) --------------------------------
# Plot fitted naive model (ggplot)
#-------------------------------------------------------------------------------

plot.fitted.avgthree.no.age <- function(fit){

    .e <- environment()

    avgthreefit <- fit
    CY <- avgthreefit$model.data[,"Run_Year"]

    avgthreemodel <- fit$model

    year <- CY
    actual <- as.numeric(avgthreemodel$x)
    fitted <- as.numeric(avgthreemodel$fitted)

    year.stacked <- year
    actual.stacked <- actual
    fitted.stacked <- fitted

    ## data.stacked <- data.frame(year=year.stacked, actual=actual.stacked, fitted=fitted.stacked)

    data.stacked.1 <- data.frame(year=year.stacked, actual=actual.stacked)
    data.stacked.2 <- data.frame(year=year.stacked, fitted=fitted.stacked)

    names(data.stacked.1) <- c("year","value")
    names(data.stacked.2) <- c("year","value")


    data.stacked <- rbind.data.frame(data.stacked.1, data.stacked.2)

    data.stacked$label <- c(rep("Actual Values",nrow(data.stacked.1)),
                            rep("Fitted Values",nrow(data.stacked.2)))
    data.stacked$age <- c(rep("Naive Model (Average of Previous Three Years)",nrow(data.stacked)))



    usePackage("ggplot2")
    usePackage("scales")

    ## usePackage("reshape")
    ## dd = melt(data.stacked, id=c("year"))
    ## dd$variable <- ifelse(dd$variable=="actual", "Actual Values",
    ##                      "Fitted Values")

    g <- ggplot(data.stacked, environment=.e) +
         facet_wrap(~age,ncol=1, scales="free_y") + 
          geom_line(aes(x=year, y=value, colour=label), size=0.6) +
                  scale_x_continuous("Return Year") +
             scale_y_continuous(paste(stockabundance),labels=comma) +
              theme_bw() +
               theme(plot.title=element_text(size=12, hjust=0.5),
               axis.title.x = element_text(size=10,vjust=-0.5),
                axis.title.y = element_text(size=10,vjust=1.5),
                 axis.text.x = element_text(size=8),
                  axis.text.y = element_text(size=8),
                   legend.position = "top", legend.direction = "horizontal"
                    ) +
                    scale_colour_manual("",
                      values = c("dodgerblue3", "lightsalmon1"))




    g
}

## options(warn=-1)
## plot.fitted.avgthree.no.age(fit.avgthree.model.no.age)
## options(warn=0)



#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

point.forecast.avgthree.no.age <- function(datalist, fit, forecastingyear){

     PSY <- forecastingyear

     output <- list()

     # fit$model

     # model <- fit$model

     avgthreefit <- fit$model

     output$Model <- "Naive (Average of Past 3 Years)"

     output$RY <- PSY

     # output[[j]]$p <- as.numeric(predict(model, h=1, level=0.80)$pred)

     output$p <-  as.numeric(avgthreefit$mean)

     return(output)
}



## point.forecast.avgthree.no.age(datalist, fit.avgthree.model.no.age, forecastingyear)

if (noagemodelavgthree){  ## avgthree 

    tmp_list <- point.forecast.avgthree.no.age(datalist, fit.avgthree.model.no.age, forecastingyear)

    tmp_df <- do.call(cbind.data.frame, tmp_list)

    results.point.forecast.avgthree.no.age <- tmp_df

    results.point.forecast.avgthree.no.age$Model <- as.character(results.point.forecast.avgthree.no.age$Model)

}



#*******************************************************************************************
#
#------------ compute prediction intervals for point forecasts of individual ages -----------
#
#*******************************************************************************************

## meboot2 bootstrap for a stock without age-specific data

forecast.avgthree.modified.no.age <- function(fit, bootmethod, level=80, npaths=B){

   series <- fit$model.data[,ncol(fit$model.data)]

   usePackage("meboot")
   usePackage("forecast")

   avgthreefcast  <- function(series) {

     n <- length(series)    # n > 3; length of time series

     fcast <- (series[n-2] + series[n-1] + series[n])/3   # point-forecast for series[n+1]

     return(fcast)

   }

   
   if (bootmethod=="meboot") {

      trim.values <- seq(0, 0.4, by=0.05)
        
      mean.series.meboot2 <- NULL
      for (k in 1:length(trim.values)) {
        set.seed(k)
        series.meboot2 <- meboot2(series, reps=npaths, trim=trim.values[k])$ensemble
        mean.series.meboot2 <- c(mean.series.meboot2, mean(series.meboot2))
      }
    
      diff.mean.series.meboot2 <- mean.series.meboot2 - mean(series)

      abs.diff.mean.series.meboot2 <- abs(diff.mean.series.meboot2)
    
      trim.optimal <- trim.values[which.min(abs.diff.mean.series.meboot2)]
    
      ## series.meboot2 <- meboot2(series, reps=npaths, trim=trim)$ensemble
    
      series.meboot2 <- meboot2(series, reps=npaths, trim=trim.optimal)$ensemble

      mean(series.meboot2)

      series.boot.forecast.avgthree <- apply(series.meboot2,2,avgthreefcast)  ## one-year ahead forecasts for all time series in the ensemble
      
      y.paths <- series.boot.forecast.avgthree   ## one-year ahead forecasts
   }


   if (bootmethod=="stlboot") {

      series <- ts(series,
                 start = min(fit$model.data$Run_Year),
                 end = max(fit$model.data$Run_Year),
                 frequency=1)

      series.stlboot <- stlboot(series, k=npaths, outplot=FALSE)
      
      series.boot.forecast.avgthree <- apply(series.stlboot,2,avgthreefcast)  ## one-year ahead forecasts for all time series in the ensemble
      
      y.paths <- series.boot.forecast.avgthree   ## one-year ahead forecasts

   }

   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   out <- NULL

   out$mean <-  as.numeric(avgthree.no.age(series)$mean)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   
   if (bootmethod=="meboot") {
       out$ensemble <- series.meboot2
   } 
   
   if (bootmethod=="stlboot") {
       out$ensemble <- series.stlboot
   }

   return(out)

}



## forecast.avgthree.modified.no.age(fit.avgthree.model.no.age, bootmethod, level=80, npaths=B)



prediction.intervals.individual.stock.avgthree.no.age <- function(fit, bootmethod, level=80, npaths=B){

     ## h <- 1  # one step ahead forecasts

     avgthree.fit <- fit

     avgthreeboot <- forecast.avgthree.modified.no.age(avgthree.fit, bootmethod, level=level, npaths=npaths)

     avgthree.point.forecast <- avgthreeboot$mean

     fit$avgthree.point.forecast <- avgthree.point.forecast

     avgthree.lwr.forecast <- avgthreeboot$lower
     # avgthree.median.forecast <- median(avgthreeboot$sim)
     avgthree.upr.forecast <- avgthreeboot$upper

     fit$PI.ctr <- avgthree.point.forecast
     fit$PI.lwr <- avgthree.lwr.forecast
     fit$PI.upr <- avgthree.upr.forecast

     fit$sim <- avgthreeboot$sim
     fit$ensemble <- avgthreeboot$ensemble

     results <- fit

     return(results)
}


if (noagemodelavgthree){  ## avgthree 

    pred.int.individual.stock.avgthree.no.age <- prediction.intervals.individual.stock.avgthree.no.age(fit.avgthree.model.no.age, bootmethod, level=80, npaths=B)

} 

## pred.int.individual.stock.avgthree.no.age

###############  avgfive #########################################################################################################################


#--------- helper function for computing the average of the past 5 years of a time series -----

avgfive.no.age  <- function(series) {

     # n = length of time series
     # n must be strictly greater than 5!!

     n <- length(series)

     fitted <- rep(NA,n)
     residuals <- rep(NA,n)

     for (k in 6:n) {
          fitted[k] <- (series[k-5] + series[k-4] + series[k-3] + series[k-2] + series[k-1])/5
          residuals[k] <- series[k] - fitted[k]
     }

     fcast <- (series[n-4]+ series[n-3] + series[n-2] + series[n-1] + series[n])/5   # point-forecast for series[n+1]

     out <- list()
     out$x <- series
     out$mean <- fcast
     out$fitted <- fitted
     out$residuals <- residuals

     return(out)

}




#---------  fit naive model (average of past 5 years) -----------------------------------------

avgfive.model.no.age <- function(datalist){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "avgfive"  # create a new class
     
     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     usePackage("forecast")

     usePackage("stringr")

     output$model <- avgfive.no.age(series)

     output$original.data <- datalist

     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}

if (noagemodelavgfive){  ## avgfive

   fit.avgfive.model.no.age <- avgfive.model.no.age(datalist)

}


#---------  Plot naive model (average of past 5 years) --------------------------------
# Plot fitted naive model (ggplot)
#-------------------------------------------------------------------------------

plot.fitted.avgfive.no.age <- function(fit){

    .e <- environment()

    avgfivefit <- fit
    CY <- avgfivefit$model.data[,"Run_Year"]

    avgfivemodel <- fit$model

    year <- CY
    actual <- as.numeric(avgfivemodel$x)
    fitted <- as.numeric(avgfivemodel$fitted)

    year.stacked <- year
    actual.stacked <- actual
    fitted.stacked <- fitted     

    data.stacked.1 <- data.frame(year=year.stacked, actual=actual.stacked)
    data.stacked.2 <- data.frame(year=year.stacked, fitted=fitted.stacked)

    names(data.stacked.1) <- c("year","value")
    names(data.stacked.2) <- c("year","value")


    data.stacked <- rbind.data.frame(data.stacked.1, data.stacked.2)

    data.stacked$label <- c(rep("Actual Values",nrow(data.stacked.1)),
                            rep("Fitted Values",nrow(data.stacked.2)))
    data.stacked$age <- c(rep("Naive Model (Average of Previous Five Years)",nrow(data.stacked)))

  

    usePackage("ggplot2")
    usePackage("scales")

    ## usePackage("reshape")
    ## dd = melt(data.stacked, id=c("year"))
    ## dd$variable <- ifelse(dd$variable=="actual", "Actual Values", 
    ##                      "Fitted Values")
    
    g <- ggplot(data.stacked, environment=.e) + 
          facet_wrap(~age,ncol=1, scales="free_y") +
          geom_line(aes(x=year, y=value, colour=label), size=0.6) +
                  scale_x_continuous("Return Year") +
             scale_y_continuous(paste(stockabundance),labels=comma) +
              theme_bw() +
               theme(plot.title=element_text(size=12, hjust=0.5),
               axis.title.x = element_text(size=10,vjust=-0.5),
                axis.title.y = element_text(size=10,vjust=1.5),
                 axis.text.x = element_text(size=8),
                  axis.text.y = element_text(size=8),
                   legend.position = "top", legend.direction = "horizontal"
                    ) + 
                    scale_colour_manual("",
                      values = c("dodgerblue3", "lightsalmon1"))
                    
                    


    g 
}

## options(warn=-1)
## plot.fitted.avgfive.no.age(fit.avgfive.model.no.age)
## options(warn=0)

#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

point.forecast.avgfive.no.age <- function(datalist, fit, forecastingyear){

     PSY <- forecastingyear

     output <- list()

     fit$model

     model <- fit$model

     avgfivefit <- fit$model

     output$Model <- "Naive (Average of Past 5 Years)"

     output$RY <- PSY

     # output[[j]]$p <- as.numeric(predict(model, h=1, level=0.80)$pred)

     output$p <-  as.numeric(avgfivefit$mean)

     return(output)
}


## point.forecast.avgfive.no.age(datalist, fit.avgfive.model.no.age, forecastingyear)

if (noagemodelavgfive) {   ## avgfive

    tmp_list <- point.forecast.avgfive.no.age(datalist, fit.avgfive.model.no.age, forecastingyear)

    tmp_df <- do.call(cbind.data.frame, tmp_list)

    results.point.forecast.avgfive.no.age <- tmp_df

    results.point.forecast.avgfive.no.age$Model <- as.character(results.point.forecast.avgfive.no.age$Model)

}

#---- meboot2 function for bootstrapping positive time series ----------------------------------------

meboot2 <- function (x, reps = 9999, trim = 0.1, reachbnd = FALSE, expand.sd = FALSE,

    force.clt = FALSE, scl.adjustment = FALSE, sym = FALSE, elaps = FALSE,

    colsubj, coldata, coltimes, ...)

{

    if ("pdata.frame" %in% class(x)) {

        res <- meboot.pdata.frame(x, reps, trim, reachbnd, expand.sd,

            force.clt, scl.adjustment, sym, elaps, colsubj, coldata,

            coltimes, ...)

        return(res)

    }

    ptm1 <- proc.time()

    n <- length(x)

    xx <- sort(x)

    ordxx <- order(x)

    if (sym) {

        xxr <- rev(xx)

        xx.sym <- mean(xx) + 0.5 * (xx - xxr)

        xx <- xx.sym

    }

    z <- rowMeans(embed(xx, 2))

    dv <- abs(diff(as.numeric(x)))

    dvtrim <- mean(dv, trim = trim)

    xmin <- 0

    xmax <- xx[n] + dvtrim

    aux <- colSums(t(embed(xx, 3)) * c(0.25, 0.5, 0.25))

    desintxb <- c(0.75 * xx[1] + 0.25 * xx[2], aux, 0.25 * xx[n -

        1] + 0.75 * xx[n])

    ensemble <- matrix(x, nrow = n, ncol = reps)

    ensemble <- apply(ensemble, 2, meboot.part, n, z, xmin, xmax,

        desintxb, reachbnd)

    qseq <- apply(ensemble, 2, sort)

    ensemble[ordxx, ] <- qseq

    if (expand.sd)

        ensemble <- expand.sd(x = x, ensemble = ensemble, ...)

    if (force.clt)

        ensemble <- force.clt(x = x, ensemble = ensemble)

    if (scl.adjustment) {

        zz <- c(xmin, z, xmax)

        v <- diff(zz^2)/12

        xb <- mean(x)

        s1 <- sum((desintxb - xb)^2)

        uv <- (s1 + sum(v))/n

        desired.sd <- sd(x)

        actualME.sd <- sqrt(uv)

        if (actualME.sd <= 0)

            stop("actualME.sd<=0 Error")

        out <- desired.sd/actualME.sd

        kappa <- out - 1

        ensemble <- ensemble + kappa * (ensemble - xb)

    }

    else kappa <- NULL

    if (is.ts(x)) {

        ensemble <- ts(ensemble, frequency = frequency(x), start = start(x))

        dimnames(ensemble)[[2]] <- paste("Series", 1:reps)

    }

    ptm2 <- proc.time()

    elapsr <- elapsedtime(ptm1, ptm2)

    if (elaps)

        cat("\n  Elapsed time:", elapsr$elaps, paste(elapsr$units,

            ".", sep = ""), "\n")

    list(x = x, ensemble = ensemble, xx = xx, z = z, dv = dv,

        dvtrim = dvtrim, xmin = xmin, xmax = xmax, desintxb = desintxb,

        ordxx = ordxx, kappa = kappa, elaps = elapsr)

}


#*******************************************************************************************
#
#------------ compute prediction intervals for point forecasts of individual ages -----------
#
#*******************************************************************************************

## meboot2 bootstrap for a stock without age-specific data

forecast.avgfive.modified.no.age <- function(fit, bootmethod, level=80, npaths=B){

   series <- fit$model.data[,ncol(fit$model.data)]

   usePackage("meboot")
   usePackage("forecast")

   avgfivefcast  <- function(series) {

     n <- length(series)    # n > 3; length of time series

     fcast <- (series[n-4] + series[n-3] + series[n-2] + series[n-1] + series[n])/5  # point-forecast for series[n+1]

     return(fcast)

   }
   
   
   if (bootmethod=="meboot") {
   
       trim.values <- seq(0, 0.4, by=0.05)
     
        mean.series.meboot2 <- NULL
        for (k in 1:length(trim.values)) {
            set.seed(k)
            series.meboot2 <- meboot2(series, reps=npaths, trim=trim.values[k])$ensemble
            mean.series.meboot2 <- c(mean.series.meboot2, mean(series.meboot2))
        }
    
        diff.mean.series.meboot2 <- mean.series.meboot2 - mean(series)

        abs.diff.mean.series.meboot2 <- abs(diff.mean.series.meboot2)
    
        trim.optimal <- trim.values[which.min(abs.diff.mean.series.meboot2)]
    
        ## series.meboot2 <- meboot2(series, reps=npaths, trim=trim)$ensemble
    
        series.meboot2 <- meboot2(series, reps=npaths, trim=trim.optimal)$ensemble

        mean(series)
        mean(series.meboot2)

        ## series.meboot2 <- meboot2(series, reps=npaths, trim=0)$ensemble
       
        series.boot.forecast.avgfive <- apply(series.meboot2,2,avgfivefcast)  ## one-year ahead forecasts for all time series in the ensemble
       
        y.paths <- series.boot.forecast.avgfive   ## one-year ahead forecasts

   } 
   
   
   if (bootmethod=="stlboot") {
      
      series <- ts(series,
                 start = min(fit$model.data$Run_Year),
                 end = max(fit$model.data$Run_Year),
                 frequency=1)

      series.stlboot <- stlboot(series, k=npaths, outplot=FALSE)

      series.boot.forecast.avgfive <- apply(series.stlboot,2,avgfivefcast)  ## one-year ahead forecasts for all time series in the ensemble
      
      y.paths <- series.boot.forecast.avgfive   ## one-year ahead forecasts

   }
   
   
   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   out <- NULL

   out$mean <-  as.numeric(avgfive.no.age(series)$mean)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   
   if (bootmethod=="meboot") {
       out$ensemble <- series.meboot2
   } 
   
   if (bootmethod=="stlboot") {
       out$ensemble <- series.stlboot
   }

   return(out)

}



## forecast.avgfive.modified.no.age(fit.avgfive.model.no.age, bootmethod, level=80, npaths=B)


prediction.intervals.individual.stock.avgfive.no.age <- function(fit, bootmethod, level=80, npaths=B){

     ## h <- 1  # one step ahead forecasts

     avgfive.fit <- fit

     avgfiveboot <- forecast.avgfive.modified.no.age(avgfive.fit, bootmethod, level=level, npaths=npaths)

     avgfive.point.forecast <- avgfiveboot$mean

     fit$avgfive.point.forecast <- avgfive.point.forecast

     avgfive.lwr.forecast <- avgfiveboot$lower
     # avgfive.median.forecast <- median(avgfiveboot$sim)
     avgfive.upr.forecast <- avgfiveboot$upper

     fit$PI.ctr <- avgfive.point.forecast
     fit$PI.lwr <- avgfive.lwr.forecast
     fit$PI.upr <- avgfive.upr.forecast

     fit$sim <- avgfiveboot$sim
     fit$ensemble <- avgfiveboot$ensemble

     results <- fit

     return(results)
}


if (noagemodelavgfive) {   ## avgfive

   pred.int.individual.stock.avgfive.no.age <- prediction.intervals.individual.stock.avgfive.no.age(fit.avgfive.model.no.age, bootmethod, level=80, npaths=B)

}


## pred.int.individual.ages.avgfive.no.age

## pred.int.individual.ages.avgfive.no.age$PI.ctr
## pred.int.individual.ages.avgfive.no.age$PI.lwr
## pred.int.individual.ages.avgfive.no.age$PI.upr
## pred.int.individual.ages.avgfive.no.age$sim


########################################################################################################
########################################################################################################
#
# ARIMA Forecasting  - Forecasting Results for Stock Without Age Information
#
########################################################################################################
########################################################################################################

#---------  fit time series ARIMA model -----------------------------------------

arima.model.no.age <- function(datalist, boxcoxtransform){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "arima"  # create a new class

     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     usePackage("forecast")

      if (boxcoxtransform==TRUE) {
              
              output$model <- auto.arima(series, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(series, method="guerrero"))
              
      } else {
              
              output$model <- auto.arima(series, allowmean=TRUE, allowdrift=FALSE)
      }
          
      ## output$model <- auto.arima(series, allowdrift=FALSE)

     output$original.data <- datalist

     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}

if (noagemodelarima) {   ## arima

    arima.model.fit.no.age  <- arima.model.no.age(datalist, boxcoxtransform)
    fit.arima.model.no.age <- arima.model.fit.no.age

}

#---------  Plot fitted time series ARIMA model --------------------------------
# Plot fit time series ARIMA model
#-------------------------------------------------------------------------------

plot.fitted.arima.no.age <- function(fit, boxcoxtransform){

    .e <- environment()

    ###
    arimafit <- fit
    CY <- arimafit$model.data[,"Run_Year"]

    sink("arimafit.txt")
    print(arimafit)
    sink()

    out <- readLines("arimafit.txt")
    usePackage("stringr")
    out.pattern <- str_detect(string=out, pattern="ARIMA")

    modelarima <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelarima <- str_trim(modelarima)

    if (boxcoxtransform==TRUE) {
          
          out.lambda <- str_detect(string=out, pattern="lambda")
          
          lambda <- out[out.lambda==TRUE]
          modellambda <- str_trim(lambda, side="right")
       
          modelarima <- paste0(modelarima, "; ", modellambda)
       
    } 

    arimamodel <- fit$model
    
    year <- CY

    actual <- as.numeric(arimamodel$x)
    fitted <- as.numeric(fitted(arimamodel))

    year.stacked <- c(year, year)
    actual.fitted.stacked <- c(actual, fitted)
    age.stacked <- c(rep(paste0(modelarima),length(actual)),rep(paste0(modelarima),length(fitted)))
    legend.stacked <- c(rep("Actual Values",length(actual)),rep("Fitted Values",length(fitted)))




    data.stacked <- data.frame(year=year.stacked, actual.fitted=actual.fitted.stacked, age=age.stacked, legend=legend.stacked)

    usePackage("ggplot2")
    usePackage("scales")

    # environment=.e
    g <- ggplot(data.stacked, aes(year.stacked, actual.fitted), environment=.e) +
    facet_wrap(~age,ncol=1, scales="free_y") +
    # geom_point(aes(shape = legend)) +
    geom_line(aes(colour = legend, group = legend),size=0.6) +
    ## labs(x = "Return Year", y = "Terminal Run", shape = "", colour = "") +
    labs(x = "Return Year", y = paste(stockabundance), shape = "", colour = "") +
    # scale_y_continuous("Terminal Run",labels=comma) +
    scale_y_continuous(paste(stockabundance),labels=comma) +
    theme_bw() +
    theme(plot.title=element_text(size=12, hjust=0.5),
          axis.title.x = element_text(size=10,vjust=-0.5),
          axis.title.y = element_text(size=10,vjust=1.5),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.position = "top", legend.direction = "horizontal"
          # , plot.margin=unit(c(0.5,1,0.5,1), "cm")
          )  +
    scale_colour_manual(values=c("dodgerblue3","lightsalmon1"))  +
    scale_linetype_manual(values=c(1,2))

    g 
}


## plot.fitted.arima.no.age(arima.model.fit.no.age, boxcoxtransform)



#-------------------------------------------------------------------------------
# Report ARIMA Model Results for Stock Without Age Information
#-------------------------------------------------------------------------------

arima.model.results.no.age <- function(fit){

       arimamodel <- fit$model
       age <- fit$age

       sink("arimamodel.txt")
       print(arimamodel)
       sink()

       out <- c(age, readLines("arimamodel.txt"))

       fn <- "arimamodel.txt"
       if (file.exists(fn)) file.remove(fn)

       return(out)

}


## arima.model.results.no.age(arima.model.fit.no.age)


#--------- point forecast for the stock without age information ---------------------------------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

point.forecast.arima.no.age <- function(datalist, fit, forecastingyear, boxcoxtransform){

     PSY <- forecastingyear

     output <- list()

     fit$model

     model <- fit$model

     ## Bella

     arimafit <- fit$model

     sink("arimafit.txt")
     print(arimafit)
     sink()

     out <- readLines("arimafit.txt")

     usePackage("stringr")

     out.pattern <- str_detect(string=out, pattern="ARIMA")

     modelarima <- out[out.pattern==TRUE]

     modelarima <- str_trim(modelarima)


     if (boxcoxtransform==TRUE){
     
            out.lambda <- str_detect(string=out, pattern="lambda")
     
            modellambda <- out[out.lambda==TRUE]
            modellambda <- str_trim(modellambda, side="right")
     
            modelarima <- paste0(modelarima, "; ", modellambda)
     
     }


     fn <- "arimafit.txt"
     if (file.exists(fn)) file.remove(fn)


     output$Model <- modelarima

     output$RY <- PSY

     ## output$p <-  round(as.numeric(forecast(arimafit, h=1, level=0.80, biasadj=FALSE)$mean))

     output$p <-  round(as.numeric(forecast(arimafit, h=1, biasadj=FALSE)$mean))

     return(output)
}


## point.forecast.arima.no.age(datalist, fit=arima.model.fit.no.age, forecastingyear, boxcoxtransform)


if (noagemodelarima) {   ## arima

    tmp_list <- point.forecast.arima.no.age(datalist, arima.model.fit.no.age, forecastingyear, boxcoxtransform)

    tmp_df <- do.call(cbind.data.frame, tmp_list)

    results.point.forecast.arima.no.age <- tmp_df

    results.point.forecast.arima.no.age$Model <- as.character(results.point.forecast.arima.no.age$Model)

}


#---- meboot2 function for bootstrapping positive time series ----------------------------------------

meboot2 <- function (x, reps = 9999, trim = 0.1, reachbnd = FALSE, expand.sd = FALSE,

    force.clt = FALSE, scl.adjustment = FALSE, sym = FALSE, elaps = FALSE,

    colsubj, coldata, coltimes, ...)

{

    if ("pdata.frame" %in% class(x)) {

        res <- meboot.pdata.frame(x, reps, trim, reachbnd, expand.sd,

            force.clt, scl.adjustment, sym, elaps, colsubj, coldata,

            coltimes, ...)

        return(res)

    }

    ptm1 <- proc.time()

    n <- length(x)

    xx <- sort(x)

    ordxx <- order(x)

    if (sym) {

        xxr <- rev(xx)

        xx.sym <- mean(xx) + 0.5 * (xx - xxr)

        xx <- xx.sym

    }

    z <- rowMeans(embed(xx, 2))

    dv <- abs(diff(as.numeric(x)))

    dvtrim <- mean(dv, trim = trim)

    xmin <- 0

    xmax <- xx[n] + dvtrim

    aux <- colSums(t(embed(xx, 3)) * c(0.25, 0.5, 0.25))

    desintxb <- c(0.75 * xx[1] + 0.25 * xx[2], aux, 0.25 * xx[n -

        1] + 0.75 * xx[n])

    ensemble <- matrix(x, nrow = n, ncol = reps)

    ensemble <- apply(ensemble, 2, meboot.part, n, z, xmin, xmax,

        desintxb, reachbnd)

    qseq <- apply(ensemble, 2, sort)

    ensemble[ordxx, ] <- qseq

    if (expand.sd)

        ensemble <- expand.sd(x = x, ensemble = ensemble, ...)

    if (force.clt)

        ensemble <- force.clt(x = x, ensemble = ensemble)

    if (scl.adjustment) {

        zz <- c(xmin, z, xmax)

        v <- diff(zz^2)/12

        xb <- mean(x)

        s1 <- sum((desintxb - xb)^2)

        uv <- (s1 + sum(v))/n

        desired.sd <- sd(x)

        actualME.sd <- sqrt(uv)

        if (actualME.sd <= 0)

            stop("actualME.sd<=0 Error")

        out <- desired.sd/actualME.sd

        kappa <- out - 1

        ensemble <- ensemble + kappa * (ensemble - xb)

    }

    else kappa <- NULL

    if (is.ts(x)) {

        ensemble <- ts(ensemble, frequency = frequency(x), start = start(x))

        dimnames(ensemble)[[2]] <- paste("Series", 1:reps)

    }

    ptm2 <- proc.time()

    elapsr <- elapsedtime(ptm1, ptm2)

    if (elaps)

        cat("\n  Elapsed time:", elapsr$elaps, paste(elapsr$units,

            ".", sep = ""), "\n")

    list(x = x, ensemble = ensemble, xx = xx, z = z, dv = dv,

        dvtrim = dvtrim, xmin = xmin, xmax = xmax, desintxb = desintxb,

        ordxx = ordxx, kappa = kappa, elaps = elapsr)

}




#*******************************************************************************************
#
#------------ compute prediction intervals for point forecasts of individual ages -----------
#
#*******************************************************************************************

##
## meboot2 bootstrap 
##

forecast.arima.modified.meboot.no.age <- function(fit, boxcoxtransform, level=80, npaths=B){

        series <- fit$model.data[,ncol(fit$model.data)]

        usePackage("meboot")
        usePackage("forecast")

        trim.values <- seq(0, 0.4, by=0.05)
     
        mean.series.meboot2 <- NULL
        for (k in 1:length(trim.values)) {
            set.seed(k)
            series.meboot2 <- meboot2(series, reps=npaths, trim=trim.values[k])$ensemble
            mean.series.meboot2 <- c(mean.series.meboot2, mean(series.meboot2))
    
        }
    
        diff.mean.series.meboot2 <- mean.series.meboot2 - mean(series)

        abs.diff.mean.series.meboot2 <- abs(diff.mean.series.meboot2)
    
        trim.optimal <- trim.values[which.min(abs.diff.mean.series.meboot2)]
    
        series.meboot2 <- meboot2(series, reps=npaths, trim=trim.optimal)$ensemble
        
        mean(series)
 
        mean(series.meboot2)

        ## series.meboot2 <- meboot2(series, reps=npaths, trim=0)$ensemble

        #----------------------------------------------------------------------------------------
   
         arimafit <- fit$model

         sink("arimafit.txt")
         print(arimafit)
         sink()

         out <- readLines("arimafit.txt")

         usePackage("stringr")
         
         out.allowmean <- grepl("with non-zero mean", out[2])
         out.pattern <- str_detect(string=out, pattern="ARIMA")
         modelarima <- out[out.pattern==TRUE]
         usePackage("stringr")
         modelarima <- str_trim(modelarima)

         modelorders <- as.numeric(unlist(strsplit(modelarima, "[^[:digit:]]")))
         
         modelorders <- modelorders[!is.na(modelorders)]

         p.0 <- modelorders[1]
         d.0 <- modelorders[2]
         q.0 <- modelorders[3]
         
         if (boxcoxtransform == TRUE) {  
             out.lambda <- str_detect(string=out, pattern="lambda") 
             mystr <- out[out.lambda==TRUE]
             lambda.char <- regmatches(mystr,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",mystr)) 
             lambda <- as.numeric(lambda.char)
         } else {
             lambda <- NULL 
         }

         fn <- "arimafit.txt"
         if (file.exists(fn)) file.remove(fn)

         ## need to apply an ARIMA model to a bootstrap series x, with orders of the model same as 
         ## the ones that were used for the original data series  
   
         arimafcast <- function(x, p.0, d.0, q.0, out.allowmean, lambda){
   
   
               series.boot <- x
             
               #### p0 <- sarima.for.modified(x, n.ahead=1, p=p.0, d=d.0, q=q.0, no.constant = out.allowmean)
               
              
               try.1 <- try(Arima(series.boot, order = c(p.0, d.0, q.0), 
                                  method="CSS-ML", lambda=lambda, include.mean=out.allowmean), silent=TRUE)
               try.2 <- try(Arima(series.boot, order = c(p.0, d.0, q.0), 
                                  method="ML", lambda=lambda, include.mean=out.allowmean), silent=TRUE)
               try.3 <- try(Arima(series.boot, order = c(p.0, d.0, q.0), 
                                  method="CSS", lambda=lambda, include.mean=out.allowmean), silent=TRUE)
                                  
               if (class(try.1)[1] !="try-error") {
               
                          model0 <- Arima(series.boot, order = c(p.0, d.0, q.0), 
                                          method="CSS-ML", lambda=lambda, include.mean=out.allowmean)
                                                            method0 <- "CSS-ML"
                          stop0 <- FALSE
                        
               } else if (class(try.2)[1] !="try-error") {

                       model0 <- Arima(series.boot, order = c(p.0, d.0, q.0), 
                                       method="ML", lambda=lambda, include.mean=out.allowmean)
                       method0 <- "ML"
                       stop0 <- FALSE 
                        
               } else if  (class(try.3)[1] !="try-error") {

                       model0 <- Arima(series.boot, order = c(p.0, d.0, q.0), 
                                       method="CSS", lambda=lambda, include.mean=out.allowmean)
                       method0 <- "CSS"
                       stop0 <- FALSE
                        
               } else {
               
                       stop0 <- TRUE
               
               }
          
               #### model0 <- arima(series.boot, order = c(p.0, d.0, q.0))
               #### p0 <- as.numeric(forecast::forecast(model0, h=1, level=0.80)$mean)
           
              if (stop0==FALSE){ 
                ## p0 <- as.numeric(forecast::forecast.Arima(model0, h=1, level=0.80, biasadj=FALSE)$mean)
                p0 <- as.numeric(forecast::forecast.Arima(model0, h=1, biasadj=FALSE)$mean)
                p0 <- round(p0)
              } else {
                p0 <- NA
              }
          
              return(p0)
   
        }

   
   ## one-year ahead forecasts for all time series in the ensemble
   series.boot.forecast.arima <- apply(series.meboot2, 2, arimafcast,p.0, d.0, q.0, out.allowmean, lambda)  
   ## series.boot.forecast.arima <- apply(series.meboot2, 2, arimafcast, p.0, d.0, q.0)


   y.paths <- series.boot.forecast.arima   ## one-year ahead forecasts

   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8, na.rm=TRUE))
   cat("lower = ", lower, "\n")
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8, na.rm=TRUE))
   cat("upper = ", upper, "\n")

   out <- NULL

   ## out$mean <-  as.numeric(forecast::forecast(arimafit, h=1, level=0.80, biasadj=FALSE)$mean)

   out$mean <-  as.numeric(forecast::forecast(arimafit, h=1, biasadj=FALSE)$mean)


   ## out$lower <- round(lower)
   ## out$upper <- round(upper)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   return(out)

}

## fit <- fit.arima.model.no.age
## forecast.arima.modified.meboot.no.age(fit.arima.model.no.age, boxcoxtransform, level=80, npaths=B)


##
## stlboot (loess) bootstrap 
##

forecast.arima.modified.stlboot.no.age <- function(fit, boxcoxtransform, level=80, npaths=B){

    series <- fit$model.data[,ncol(fit$model.data)]

    series <- ts(series, 
                 start = min(fit$model.data$Run_Year), 
                 end = max(fit$model.data$Run_Year), 
                 frequency=1)

    mean(series)

    ## require("TStools")
    usePackage("forecast")
        
    set.seed(1700) 
        
    series.stlboot <- stlboot(series, k=npaths, outplot=FALSE)
        
    mean(series.stlboot)
    
    #----------------------------------------------------------------------------------------
   
    arimafit <- fit$model

    sink("arimafit.txt")
    print(arimafit)
    sink()

    out <- readLines("arimafit.txt")

    usePackage("stringr")
         
    out.allowmean <- grepl("with non-zero mean", out[2])
    out.pattern <- str_detect(string=out, pattern="ARIMA")
    modelarima <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelarima <- str_trim(modelarima)

    modelorders <- as.numeric(unlist(strsplit(modelarima, "[^[:digit:]]")))
         
    modelorders <- modelorders[!is.na(modelorders)]

    p.0 <- modelorders[1]
    d.0 <- modelorders[2]
    q.0 <- modelorders[3]
         
    if (boxcoxtransform == TRUE) {  
             out.lambda <- str_detect(string=out, pattern="lambda") 
             mystr <- out[out.lambda==TRUE]
             lambda.char <- regmatches(mystr,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",mystr)) 
             lambda <- as.numeric(lambda.char)
    } else {
             lambda <- NULL 
    }

    fn <- "arimafit.txt"
    if (file.exists(fn)) file.remove(fn)

    
    ## need to apply an ARIMA model to a bootstrap series x, with orders of the model same as 
    ## the ones that were used for the original data series  
   
   arimafcast <- function(x, p.0, d.0, q.0, out.allowmean, lambda){
   
               series.boot <- x
             
               #### p0 <- sarima.for.modified(x, n.ahead=1, p=p.0, d=d.0, q=q.0, no.constant = out.allowmean)
               
              
               try.1 <- try(Arima(series.boot, order = c(p.0, d.0, q.0), 
                                  method="CSS-ML", lambda=lambda, include.mean=out.allowmean), silent=TRUE)
               try.2 <- try(Arima(series.boot, order = c(p.0, d.0, q.0), 
                                  method="ML", lambda=lambda, include.mean=out.allowmean), silent=TRUE)
               try.3 <- try(Arima(series.boot, order = c(p.0, d.0, q.0), 
                                  method="CSS", lambda=lambda, include.mean=out.allowmean), silent=TRUE)
                                  
               if (class(try.1)[1] !="try-error") {
               
                          model0 <- Arima(series.boot, order = c(p.0, d.0, q.0), 
                                          method="CSS-ML", lambda=lambda, include.mean=out.allowmean)
                                                            method0 <- "CSS-ML"
                          stop0 <- FALSE
                        
               } else if (class(try.2)[1] !="try-error") {

                       model0 <- Arima(series.boot, order = c(p.0, d.0, q.0), 
                                       method="ML", lambda=lambda, include.mean=out.allowmean)
                       method0 <- "ML"
                       stop0 <- FALSE 
                        
               } else if  (class(try.3)[1] !="try-error") {

                       model0 <- Arima(series.boot, order = c(p.0, d.0, q.0), 
                                       method="CSS", lambda=lambda, include.mean=out.allowmean)
                       method0 <- "CSS"
                       stop0 <- FALSE
                        
               } else {
               
                       stop0 <- TRUE
               
               }
          
         
              if (stop0==FALSE){ 
                ##  p0 <- as.numeric(forecast::forecast.Arima(model0, h=1, level=0.80, biasadj=FALSE)$mean)
                p0 <- as.numeric(forecast::forecast.Arima(model0, h=1, biasadj=FALSE)$mean)
                p0 <- round(p0)
              } else {
                p0 <- NA
              }
          
              return(p0)
   
    }

   
    series.stlboot <- as.data.frame(series.stlboot)  
        
    ## one-year ahead forecasts for all time series in the ensemble
    series.boot.forecast.arima <- apply(series.stlboot, 2, arimafcast, p.0, d.0, q.0, out.allowmean, lambda)   ## one-year ahead forecasts for all time series in the ensemble
                             
    ## series.boot.forecast.arima <- apply(series.meboot2, 2, arimafcast, p.0, d.0, q.0)
    
    y.paths <- series.boot.forecast.arima   ## one-year ahead forecasts


    lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
    upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

    out <- NULL

    ##  out$mean <- as.numeric(forecast::forecast(arimafit , h=1, lambda=lambda, level=0.80, biasadj=FALSE)$mean)
    
    out$mean <- as.numeric(forecast::forecast(arimafit , h=1, lambda=lambda, biasadj=FALSE)$mean)
    
    out$lower <- round(lower)
    out$upper <- round(upper)

    out$sim <- y.paths
    out$series <- out$series
    out$ensemble <- series.stlboot
   
    return(out)

}


## debug <- forecast.arima.modified.stlboot.no.age(fit.arima.model.no.age,  boxcoxtransform, level=80, npaths=B)

## debug$lower
## debug$mean
## debug$upper


#*******************************************************************************************
#
#------------ compute prediction interval for arima model -----------
#
#*******************************************************************************************

prediction.interval.no.age.arima <- function(fit.arima.model.no.age, boxcoxtransform, bootmethod, level=80, npaths=B){

     

     h <- 1  # one step ahead forecasts

     ## arima.fit <- fit$model

     usePackage("forecast")

     ## pred <- predict(arima.fit, n.ahead = h)$pred

     # sim stores the simulations obtained by re-sampling the fitted ARIMA time series

     if (bootmethod=="stlboot") {
           
           
              ## debug <- forecast.arima.modified.stlboot.no.age(fit.arima.model.no.age,  boxcoxtransform, level=80, npaths=B)
              
              ## cat("debug \n")
              ## print(debug$lower); print(debug$mean); print(debug$upper)
           
              #### arimaboot <<- forecast.arima.modified.stlboot.no.age(fit, boxcoxtransform, level=level, npaths=npaths)
              
              arimaboot <<- forecast.arima.modified.stlboot.no.age(fit.arima.model.no.age,  boxcoxtransform, level=level, npaths=B)
              
              cat("arimaboot \n")
              print(arimaboot$lower); print(arimaboot$mean); print(arimaboot$upper)
              
     }
          
     if (bootmethod=="meboot") {
              arimaboot <<- forecast.arima.modified.meboot.no.age(fit.arima.model.no.age, boxcoxtransform, level=level, npaths=npaths)
              
              str(arimaboot)
     }
          
     names(arimaboot)
                         
     arima.point.forecast <- as.numeric(arimaboot$mean)
 
     fit.arima.model.no.age$arima.point.forecast <- arima.point.forecast

     arima.lwr.forecast <- arimaboot$lower
     ## arima.median.forecast <- arimaboot$median.boot
     arima.upr.forecast <- arimaboot$upper

     fit.arima.model.no.age$PI.ctr <- arima.point.forecast
     fit.arima.model.no.age$PI.lwr <- arima.lwr.forecast
     fit.arima.model.no.age$PI.upr <- arima.upr.forecast
     ## fit$PI.median <- arima.median.forecast

     fit.arima.model.no.age$sim <- arimaboot$sim

     ## results <- fit

     ## return(results)
     
     return(fit.arima.model.no.age)
}


if (noagemodelarima) {   ## arima

   ## pred.int.individual.stock.arima.no.age <- prediction.interval.no.age.arima(fit.arima.model.no.age, boxcoxtransform, bootmethod, level=0.80, npaths=B)

   pred.int.individual.stock.arima.no.age <- prediction.interval.no.age.arima(fit.arima.model.no.age, boxcoxtransform, bootmethod, level=80, npaths=B)

}


########################################################################################################
########################################################################################################
#
# Exponential Smoothing - Forecasting Results for Stock without Age Information
#
########################################################################################################
########################################################################################################


#---------  fit exponential smoothing model -----------------------------------------

expsmooth.model.no.age <- function(datalist, boxcoxtransform){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "expsmooth"  # create a new class

     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     usePackage("forecast")
 
     if (boxcoxtransform==TRUE) {
                            
              output$model <- ets(series, model="ZZZ", lambda=BoxCox.lambda(series), damped=NULL )
              
     } else {
                            
              output$model <- ets(series, model="ZZZ", lambda=NULL, damped=NULL )
     }
 
     ## output$model <- ets(series)

     output$original.data <- datalist
          
     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}


if (noagemodelexpsmooth) {   ## expsmooth

   expsmooth.model.fit.no.age  <- expsmooth.model.no.age(datalist, boxcoxtransform)
   fit <- expsmooth.model.fit.no.age

   fit.expsmooth.model.no.age <- expsmooth.model.fit.no.age

}


#---------  Plot fitted exponential smoothing model --------------------------------
# Plot fitted exponential smoothing model (ggplot)
#-----------------------------------------------------------------------------------

plot.fitted.expsmooth.no.age <- function(fit, boxcoxtransform){
  
    .e <- environment()
   
    expsmoothfit <- fit
    CY <- expsmoothfit$model.data[,"Run_Year"]
       
    sink("expsmoothfit.txt")
    print(expsmoothfit)
    sink()

    out <- readLines("expsmoothfit.txt")
    usePackage("stringr")
    out.pattern <- str_detect(string=out, pattern="ETS")

    modelexpsmooth <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelexpsmooth <- str_trim(modelexpsmooth)
       
    if (boxcoxtransform==TRUE) {
          
          out.lambda <- str_detect(string=out, pattern="lambda")
          
          lambda <- out[out.lambda==TRUE]
          lambda <- lambda[2]
          
          modellambda <- str_trim(lambda, side="left")
          modellambda <- str_trim(lambda, side="right")
          modellambda <- str_trim(lambda, side="left")
       
          modelexpsmooth <- paste0(modelexpsmooth, "; ", modellambda)
       
    } 
       
    expsmoothmodel <- fit$model
       
    year <- CY 
    actual <- as.numeric(expsmoothmodel$x)
    fitted <- as.numeric(fitted(expsmoothmodel))
       
    year.stacked <- c(year, year)    
    actual.fitted.stacked <- c(actual, fitted)
    age.stacked <- c( rep(paste(modelexpsmooth),length(actual)),rep(paste(modelexpsmooth),length(actual)) )
    legend.stacked <- c(rep("Actual Values",length(actual)),rep("Fitted Values",length(fitted)))


    data.stacked <- data.frame(year=year.stacked, actual.fitted=actual.fitted.stacked, age=age.stacked, legend=legend.stacked)    

    usePackage("ggplot2")
    usePackage("scales")

    # environment=.e
    g <- ggplot(data.stacked, aes(year.stacked, actual.fitted), environment=.e) + 
    facet_wrap(~age,ncol=1, scales="free_y") + 
    # geom_point(aes(shape = legend)) + 
    geom_line(aes(colour = legend, group = legend),size=0.6) + 
    ## labs(x = "Return Year", y = "Terminal Run", shape = "", colour = "") + 
    labs(x = "Return Year", y = paste(stockabundance), shape = "", colour = "") + 
    # scale_y_continuous("Terminal Run",labels=comma) + 
    scale_y_continuous(paste(stockabundance),labels=comma) + 
    theme_bw() + 
    theme(plot.title=element_text(size=12, hjust=0.5),
          axis.title.x = element_text(size=10,vjust=-0.5),  
          axis.title.y = element_text(size=10,vjust=1.5),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.position = "top", legend.direction = "horizontal"
          ## , plot.margin=unit(c(0.5,1,1,1), "cm")
          )  + 
    scale_colour_manual(values=c("dodgerblue3","lightsalmon1"))  + 
    scale_linetype_manual(values=c(1,2))
       
    g   
}


## plot.fitted.expsmooth.no.age(expsmooth.model.fit.no.age, boxcoxtransform)



#-------------------------------------------------------------------------------
# Report Exponential Smoothing Model Results for A Specific Age Class
#-------------------------------------------------------------------------------

expsmooth.model.results.no.age <- function(fit){
    
       expsmoothmodel <- fit$model
      
 
       sink("expsmoothmodel.txt")
       print(expsmoothmodel)
       sink()

       out <- readLines("expsmoothmodel.txt")
       
       fn <- "expsmoothmodel.txt"
       if (file.exists(fn)) file.remove(fn)
         
       return(out)
    
}

## expsmooth.model.results.no.age(fit)



#--------- point forecast for the youngest age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

point.forecast.expsmooth.no.age <- function(datalist, fit, boxcoxtransform, forecastingyear){
    
     PSY <- forecastingyear
   
     output <- list()
          
     fit$model

     model <- fit$model
         
     ## Bella 
         
     expsmoothfit <- fit$model

     sink("expsmoothfit.txt")
     print(expsmoothfit)
     sink()

     out <- readLines("expsmoothfit.txt")

     usePackage("forecast")
     usePackage("stringr")

     out.pattern <- str_detect(string=out, pattern="ETS")

     modelexpsmooth <- out[out.pattern==TRUE]
         
 
     modelexpsmooth <- str_trim(modelexpsmooth)

     
     if (boxcoxtransform==TRUE){
     
            out.lambda <- str_detect(string=out, pattern="lambda")
     
            modellambda <- out[out.lambda==TRUE]
            modellambda <- modellambda[2]
            
            modellambda <- str_trim(modellambda, side="right")
            modellambda <- str_trim(modellambda, side="left")
            modellambda <- str_trim(modellambda, side="right")
            modellambda <- str_replace(modellambda, "lambda=", "lambda =")
     
     
            modelexpsmooth <- paste0(modelexpsmooth, "; ", modellambda)
     
     }


     fn <- "expsmoothfit.txt"
     if (file.exists(fn)) file.remove(fn)
         

     output$Model <- modelexpsmooth
           
     output$RY <- PSY 

     # output[[j]]$p <- as.numeric(predict(model, h=1, level=0.80)$pred)

     output$p <-  round(as.numeric(forecast(expsmoothfit, h=1, biasadj=FALSE)$mean))

     return(output)
}




if (noagemodelexpsmooth) {   ## expsmooth

    tmp_list <- point.forecast.expsmooth.no.age(datalist, expsmooth.model.fit.no.age, boxcoxtransform, forecastingyear)

    tmp_df <- do.call(cbind.data.frame, tmp_list)

    results.point.forecast.expsmooth.no.age <- tmp_df

    results.point.forecast.expsmooth.no.age$Model <- as.character(results.point.forecast.expsmooth.no.age$Model)

}




#*******************************************************************************************
#
#------------ compute prediction intervals for point forecasts of individual ages -----------
#
#*******************************************************************************************

## meboot2 bootstrap for a specific age

## fit <- expsmooth.model.fit.no.age


forecast.expsmooth.modified.meboot.no.age <- function(fit, boxcoxtransform, level=80, npaths=B){

    series <- fit$model.data[,ncol(fit$model.data)]

    mean(series)

    usePackage("meboot")
    usePackage("forecast")
    
    trim.values <- seq(0, 0.4, by=0.05)
     
    mean.series.meboot2 <- NULL
    for (k in 1:length(trim.values)) {
        set.seed(k)
        series.meboot2 <- meboot2(series, reps=npaths, trim=trim.values[k])$ensemble
        mean.series.meboot2 <- c(mean.series.meboot2, mean(series.meboot2))
    
    }
    
    diff.mean.series.meboot2 <- mean.series.meboot2 - mean(series)

    abs.diff.mean.series.meboot2 <- abs(diff.mean.series.meboot2)
    
    trim.optimal <- trim.values[which.min(abs.diff.mean.series.meboot2)]
    
    ## series.meboot2 <- meboot2(series, reps=npaths, trim=trim)$ensemble
    
    series.meboot2 <- meboot2(series, reps=npaths, trim=trim.optimal)$ensemble

    mean(series.meboot2)

    expsmoothfit <- fit$model

    sink("expsmoothfit.txt")
    print(expsmoothfit)
    sink()

    out <- readLines("expsmoothfit.txt")

    usePackage("stringr")

    out.pattern <- str_detect(string=out, pattern="ETS")

    modelexpsmooth <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelexpsmooth <- str_trim(modelexpsmooth)

    ## usePackage("stringr")
    
    ## modelexpsmooth <- modelexpsmooth
    
    #---
    
    modelexpsmooth.damped <- function(modelexpsmooth) {
    
        modelexpsmooth.damped.tmp <- strsplit(modelexpsmooth,",")[[1]][2]
    
        usePackage("stringi")
    
        modelexpsmooth.damped.tmp <- stri_detect_fixed(modelexpsmooth.damped.tmp,c("d"))

        damped <- modelexpsmooth.damped.tmp 
        
        #---
        
        modelexpsmooth <- str_replace_all(modelexpsmooth, "ETS", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, "\\(", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, "\\)", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, ",", "")

        usePackage("stringr")
        
        model <- modelexpsmooth
        model <- str_replace(model,"d","")
        model
    
        list(model=model, damped=damped)
    
    }
                                                             
    #---
    
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "ETS", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "\\(", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "\\)", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, ",", "")

    if (boxcoxtransform == TRUE) {  
    
             out.lambda <- str_detect(string=out, pattern="lambda") 
             mystr <- out[out.lambda==TRUE]
             mystr <- mystr[2]
             lambda.char <- regmatches(mystr,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",mystr)) 
             lambda <- as.numeric(lambda.char)
    
         } else {
    
             lambda <- NULL 
    
    }


    fn <- "expsmoothfit.txt"
    if (file.exists(fn)) file.remove(fn)

    ####

    ## usePackage("foreach")

    expsmoothfcast <- function(x, modelexpsmooth, lambda){
   
          if (!is.null(lambda)) {
              series.boot <- x
          } else {
          
             series.boot <- x
             series.boot[series.boot==0] <- 0.001 # add small constant to zero counts
          
          }
          
          ## model0 <- forecast::ets(series.boot, model=expsmoothfit)
          
          damped.flag <-  modelexpsmooth.damped(modelexpsmooth)$damped
          model <- modelexpsmooth.damped(modelexpsmooth)$model 
          
          model0 <- forecast::ets(series.boot, model=model, lambda=lambda, damped=damped.flag)
              
          
          ## p0 <- as.numeric(forecast::forecast(model0, h=1, level=0.80, lambda=lambda, biasadj=FALSE)$mean)
          p0 <- as.numeric(forecast::forecast(model0, h=1, biasadj=FALSE)$mean)
          p0 <- round(p0)
          return(p0)
   
    }
                         
   
   series.boot.forecast.expsmooth <- apply(series.meboot2, 2, expsmoothfcast, modelexpsmooth, lambda)  ## one-year ahead forecasts for all time series in the ensemble
                                  
   y.paths <- series.boot.forecast.expsmooth   ## one-year ahead forecasts


   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   out <- NULL

   ## out$mean <-  as.numeric(rwf(series,h=1, drift=FALSE, level=level)$mean)

   ## model00 <- forecast::ets(series, model=modelexpsmooth, lambda=lambda)
   out$mean <- as.numeric(forecast::forecast(expsmoothfit , h=1, lambda=lambda, biasadj=FALSE)$mean)
    
   out$lower <- round(lower)
   out$upper <- round(upper)

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   out$trim <- trim.optimal 

   return(out)

}


## debug <- forecast.expsmooth.modified.meboot.no.age(fit,  boxcoxtransform, level=80, npaths=B)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

forecast.expsmooth.modified.stlboot.no.age <- function(fit, boxcoxtransform, level=80, npaths=B){

    series <- fit$model.data[,ncol(fit$model.data)]

    series <- ts(series, 
                 start = min(fit$model.data$Run_Year), 
                 end = max(fit$model.data$Run_Year), 
                 frequency=1)

    mean(series)

    require("TStools")
    usePackage("forecast")
        
    series.stlboot <- stlboot(series, k=npaths, outplot=FALSE)
        
    mean(series.stlboot)
 
    expsmoothfit <- fit$model

    sink("expsmoothfit.txt")
    print(expsmoothfit)
    sink()

    out <- readLines("expsmoothfit.txt")

    usePackage("stringr")

    out.pattern <- str_detect(string=out, pattern="ETS")

    modelexpsmooth <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelexpsmooth <- str_trim(modelexpsmooth)

    modelexpsmooth.damped <- function(modelexpsmooth) {

        modelexpsmooth.damped.tmp <- strsplit(modelexpsmooth,",")[[1]][2]

        usePackage("stringi")

        modelexpsmooth.damped.tmp <- stri_detect_fixed(modelexpsmooth.damped.tmp,c("d"))

        damped <- modelexpsmooth.damped.tmp

        #---

        modelexpsmooth <- str_replace_all(modelexpsmooth, "ETS", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, "\\(", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, "\\)", "")
        modelexpsmooth <- str_replace_all(modelexpsmooth, ",", "")

        usePackage("stringr")

        model <- modelexpsmooth
        model <- str_replace(model,"d","")
        model

        list(model=model, damped=damped)

    }

    
    ## usePackage("stringr")
    
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "ETS", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "\\(", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, "\\)", "")
    ## modelexpsmooth <- str_replace_all(modelexpsmooth, ",", "")

    if (boxcoxtransform == TRUE) {  
    
             out.lambda <- str_detect(string=out, pattern="lambda") 
             mystr <- out[out.lambda==TRUE]
             mystr <- mystr[2]
             lambda.char <- regmatches(mystr,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",mystr)) 
             lambda <- as.numeric(lambda.char)
    
         } else {
    
             lambda <- NULL 
    
    }


    fn <- "expsmoothfit.txt"
    if (file.exists(fn)) file.remove(fn)

    ####

    ## usePackage("foreach")

    expsmoothfcast <- function(x, modelexpsmooth, lambda){
   
          if (!is.null(lambda)) {
 
              series.boot <- x
 
          } else {
          
             series.boot <- x
             series.boot[series.boot==0] <- 0.001 # add small constant to zero counts
          
          }
          
          ## model0 <- forecast::ets(series.boot, model=expsmoothfit)
   
          damped.flag <-  modelexpsmooth.damped(modelexpsmooth)$damped
          model <- modelexpsmooth.damped(modelexpsmooth)$model
          
          ## model0 <- forecast::ets(series.boot, model=modelexpsmooth, lambda=lambda)
             
          model0 <- forecast::ets(series.boot, model=model, lambda=lambda, damped=damped.flag)  
          
          ## p0 <- as.numeric(forecast::forecast(model0, h=1, level=0.80, lambda=lambda, biasadj=FALSE)$mean)
          p0 <- as.numeric(forecast::forecast(model0, h=1, biasadj=FALSE)$mean)
          p0 <- round(p0)
          return(p0)
   
    }                                                            
                         
   
   series.boot.forecast.expsmooth <- apply(series.stlboot, 2, expsmoothfcast, modelexpsmooth, lambda)  ## one-year ahead forecasts for all time series in the ensemble
                                  
   y.paths <- series.boot.forecast.expsmooth   ## one-year ahead forecasts


   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   out <- NULL

   ## out$mean <-  as.numeric(rwf(series,h=1, drift=FALSE, level=level)$mean)

   ## model00 <- forecast::ets(series, model=modelexpsmooth, lambda=lambda)
   out$mean <- as.numeric(forecast::forecast(expsmoothfit, h=1, lambda=lambda, biasadj=FALSE)$mean)
    
   out$lower <- round(lower)
   out$upper <- round(upper)

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.stlboot
   
   return(out)

}


## debug <- forecast.expsmooth.modified.stlboot.no.age(fit=expsmooth.model.fit.no.age, boxcoxtransform, level=80, npaths=B)


#*******************************************************************************************
#
#------------ compute prediction intervals for point forecasts of individual ages -----------
#
#*******************************************************************************************

# fit <- expsmooth.model.fit.no.age


prediction.interval.no.age.expsmooth <- function(fit, boxcoxtransform, bootmethod, level=80, npaths=B){
     
     h <- 1  # one step ahead forecasts 
                    
     # sim stores the simulations obtained by re-sampling the fitted model (?) 

     if (bootmethod=="stlboot") {
              expsmoothboot <- forecast.expsmooth.modified.stlboot.no.age(fit, boxcoxtransform, level=level, npaths=npaths)
     }
          
     if (bootmethod=="meboot") {
              expsmoothboot <- forecast.expsmooth.modified.meboot.no.age(fit, boxcoxtransform, level=level, npaths=npaths)
     }
        
     ## expsmoothboot <- forecast.expsmooth.meboot.no.age(fit, level=level, npaths=npaths)   
          
     names(expsmoothboot)

     expsmooth.point.forecast <- as.numeric(expsmoothboot$mean)
         
     fit$expsmooth.point.forecast <- expsmooth.point.forecast
          
     expsmooth.lwr.forecast <- expsmoothboot$lower 
     # expsmooth.median.forecast <- median(expsmoothboot$sim)
     expsmooth.upr.forecast <- expsmoothboot$upper
          
     fit$PI.ctr <- expsmooth.point.forecast
     fit$PI.lwr <- expsmooth.lwr.forecast
     fit$PI.upr <- expsmooth.upr.forecast
     # fits[[j]]$PI.median <- arima.median.forecast 
          
     fit$sim <- expsmoothboot$sim  
     
     results <- fit
     
     return(results)
}


if (noagemodelexpsmooth) {   ## expsmooth

   fit <- expsmooth.model.fit.no.age
   pred.int.individual.stock.expsmooth.no.age <- prediction.interval.no.age.expsmooth(fit, boxcoxtransform, bootmethod, level=80, npaths=B)

}







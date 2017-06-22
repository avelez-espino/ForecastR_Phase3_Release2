########################################################################################################
########################################################################################################
#
# Naive Time Series Forecasting (Average of Past 5 Years) - Forecasting Results for Youngest Age
#
########################################################################################################
########################################################################################################



cat("Youngest Age - Sibling Regression.R", "\n\n")

## datafile <- read.csv("SPR_thousands.csv", as.is=TRUE)

SIMPLESIBREG$datafile <- datafile_original

SIMPLESIBREG$datafilesub <- SIMPLESIBREG$datafile

SIMPLESIBREG$extract_ages <- sort(unique(SIMPLESIBREG$datafilesub$Age_Class))
SIMPLESIBREG$extract_names <- paste("T",SIMPLESIBREG$extract_ages,sep="")
SIMPLESIBREG$extract_names <- c("BY",SIMPLESIBREG$extract_names)

SIMPLESIBREG$tmpsub <- list()
for (i in 1:length(SIMPLESIBREG$extract_ages)){
     if (SIMPLESIBREG$stockabundance=="Terminal Run"){
     SIMPLESIBREG$tmpsub[[i]] <- subset(SIMPLESIBREG$datafilesub, Age_Class==SIMPLESIBREG$extract_ages[i])[,c("Brood_Year","Average_Terminal_Run")]
     } else if (SIMPLESIBREG$stockabundance=="Escapement") {
      SIMPLESIBREG$tmpsub[[i]] <- subset(SIMPLESIBREG$datafilesub, Age_Class==SIMPLESIBREG$extract_ages[i])[,c("Brood_Year","Average_Escapement")]
     } else if (SIMPLESIBREG$stockabundance=="Production") {
      SIMPLESIBREG$tmpsub[[i]] <- subset(SIMPLESIBREG$datafilesub, Age_Class==SIMPLESIBREG$extract_ages[i])[,c("Brood_Year","Average_Production")]
     }
}

SIMPLESIBREG$list.of.data.frames <- SIMPLESIBREG$tmpsub
SIMPLESIBREG$merged.data.frame = Reduce(function(...) merge(...,by="Brood_Year", all=T), SIMPLESIBREG$list.of.data.frames)

SIMPLESIBREG$datafile_new <- SIMPLESIBREG$merged.data.frame
names(SIMPLESIBREG$datafile_new) <- SIMPLESIBREG$extract_names

## SIMPLESIBREG$datafile <<- SIMPLESIBREG$datafile_new

SIMPLESIBREG$datafile <- SIMPLESIBREG$datafile_new


cat("Working data file is: ","\n")
print(SIMPLESIBREG$datafile)
cat("\n")


#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for naive forecasting (average of past 5 years) ---------------------------------

SIMPLESIBREG$datalist.avgfive <- function(datafile, forecastingyear) {


    cols <- colnames(datafile)

    data <- list()
    nms <- NULL
    for (i in 1:(length(cols)-1)) {

         pattern <- paste("c(",cols[1],",",cols[i+1],")",sep="")

         data[[i]] <- subset(datafile,
                             select=eval(parse(text=pattern)))


         data[[i]][data[[i]]<0] <- NA

         usePackage("stringr")
         age <- as.numeric(str_extract(cols[i+1],"[[:digit:]]+"))

         data[[i]]$CY <- data[[i]]$BY + age

         data[[i]] <- data[[i]][,c(3,1,2)]

         BYmax <- forecastingyear-age

         data[[i]] <- subset(data[[i]], BY <BYmax)    ## Added this on Dec. 15th to deal with Spring Creek data!


         paste("age",age, sep="")

         nms <- c(nms, paste("age",age, sep=""))


    }


    names(data) <- nms

    return(data)

}


SIMPLESIBREG$datalist <- SIMPLESIBREG$datalist.avgfive(SIMPLESIBREG$datafile, SIMPLESIBREG$forecastingyear)  # CY refers to the T variable with highest age

SIMPLESIBREG$datalist <- SIMPLESIBREG$datalist[[1]] ## retain only the data for the youngest ages


#--------- plot data to be used for naive forecasting (average of past 5 years) (uses ggplot) ---------------------------

SIMPLESIBREG$plot.data.avgfive.youngest <- function(datalist){

     # par(mfrow=c(length(datalist),1), mar=c(4,6,2,2))
     
     .e <- environment()

     x.stacked <- datalist[,"CY"]
     
     y.stacked <- datalist[,ncol(datalist)]

     age <- gsub("[^0-9]", "", names(datalist))

     age <- as.numeric(age[length(age)])

     age.stacked <-  rep(paste("Age",age), length(y.stacked))


     data.stacked <- data.frame(x=x.stacked,y=y.stacked,age=age.stacked)

    usePackage("ggplot2")
    usePackage("scales")

    ggplot(data.stacked, aes(x,y), environment=.e) +
      geom_line(colour="dodgerblue3") +
        geom_point(col="dodgerblue3") +
          facet_wrap(~age, ncol=1, scales="free") +
            xlab("Return Year") +
              # scale_y_continuous("Terminal Run",labels=comma) +
               scale_y_continuous(paste(SIMPLESIBREG$stockabundance),labels=comma) +
                theme_bw() +
                   theme(plot.title=element_text(size=12),
                         axis.title.x = element_text(size=10,vjust=-0.5),
                         axis.title.y = element_text(size=10,vjust=1),
                         axis.text.x = element_text(size=8),
                         axis.text.y = element_text(size=8)
                         )

}

SIMPLESIBREG$plot.data.avgfive.youngest(SIMPLESIBREG$datalist)


#--------- helper function for computing the average of the past 5 years of a time series -----

SIMPLESIBREG$avgfive.youngest  <- function(series) {

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

SIMPLESIBREG$avgfive.model.youngest <- function(datalist){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "avgfive"  # create a new class
     
     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     usePackage("forecast")

     usePackage("stringr")

     output$age <-  paste("Age ",
                          as.numeric(str_extract(names(tmpdata)[length(names(tmpdata))],"[[:digit:]]")),
                          sep="")

     output$model <- SIMPLESIBREG$avgfive.youngest(series)

     output$original.data <- datalist

     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}

SIMPLESIBREG$avgfive.model.fit.youngest  <- SIMPLESIBREG$avgfive.model.youngest(SIMPLESIBREG$datalist)

SIMPLESIBREG$fit <- SIMPLESIBREG$avgfive.model.fit.youngest

#---------  Plot naive model (average of past 5 years) --------------------------------
# Plot fitted naive model (ggplot)
#-------------------------------------------------------------------------------

SIMPLESIBREG$plot.fitted.avgfive.youngest <- function(fit){

    .e <- environment()

     avgfivefit <- fit
     CY <- avgfivefit$model.data[,"CY"]

     avgfivemodel <- fit$model

     age <- fit$age

     year <- CY
     actual <- as.numeric(avgfivemodel$x)
     fitted <- as.numeric(avgfivemodel$fitted)

     year.stacked <- c(year, year)
     actual.fitted.stacked <- c(actual, fitted)
     age.stacked <- c(   rep(paste(age),length(actual)),
                         rep(paste(age),length(fitted)) )
                         
     legend.stacked <- c( rep("Actual Values",length(actual)),
                          rep("Fitted Values Obtained via Naive Modeling",length(fitted)))


    data.stacked <- data.frame(year=year.stacked, actual.fitted=actual.fitted.stacked, age=age.stacked, legend=legend.stacked)


    usePackage("ggplot2")
    usePackage("scales")

    ## environment=.e
    g <- ggplot(data.stacked, aes(year.stacked, actual.fitted),environment=.e) +
    facet_wrap(~age,ncol=1, scales="free_y") +
    # geom_point(aes(shape = legend)) +
    geom_line(aes(colour = legend, group = legend),size=0.6) +
    ## labs(x = "Return Year", y = "Terminal Run", shape = "", colour = "") +
    labs(x = "Return Year", y = paste(SIMPLESIBREG$stockabundance), shape = "", colour = "") +
    # scale_y_continuous("Terminal Run",labels=comma) +
    scale_y_continuous(paste(SIMPLESIBREG$stockabundance),labels=comma) +
    theme_bw() +
    theme(axis.title.x = element_text(size=10,vjust=-0.5),
          axis.title.y = element_text(size=10,vjust=1.5),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.position = "top", legend.direction = "horizontal"
          ## , plot.margin=unit(c(0,0,0.5,1), "cm")
          )  +
    scale_colour_manual(values=c("dodgerblue3","lightsalmon1"))  +
    scale_linetype_manual(values=c(1,2))

    g 
}

SIMPLESIBREG$fit <- SIMPLESIBREG$avgfive.model.fit.youngest
SIMPLESIBREG$plot.fitted.avgfive.youngest(SIMPLESIBREG$fit)


#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

SIMPLESIBREG$point.forecast.avgfive.youngest <- function(datalist, fit){

     PSY <- datalist$CY[length(datalist$CY)] + 1

     output <- list()

     fit$model

     model <- fit$model

     avgfivefit <- fit$model

     output$Age <- fit$age

     output$Model <- "Naive (Average of Past 5 Years)"

     output$RY <- PSY

     # output[[j]]$p <- as.numeric(predict(model, h=1, level=80)$pred)

     output$p <-  as.numeric(avgfivefit$mean)

     return(output)
}


SIMPLESIBREG$point.forecast.avgfive.youngest(SIMPLESIBREG$datalist, SIMPLESIBREG$fit)

SIMPLESIBREG$tmp_list <- SIMPLESIBREG$point.forecast.avgfive.youngest(SIMPLESIBREG$datalist, SIMPLESIBREG$avgfive.model.fit.youngest)

SIMPLESIBREG$tmp_df <- do.call(cbind.data.frame, SIMPLESIBREG$tmp_list)

SIMPLESIBREG$results.point.forecast.avgfive.youngest <- SIMPLESIBREG$tmp_df

SIMPLESIBREG$results.point.forecast.avgfive.youngest$Model <- as.character(SIMPLESIBREG$results.point.forecast.avgfive.youngest$Model)

## results.point.forecast.avgfive.youngest

## str(results.point.forecast.avgfive.youngest)



#---- meboot2 function for bootstrapping positive time series ----------------------------------------

meboot2 <- function (x, reps = 999, trim = 0.1, reachbnd = FALSE, expand.sd = FALSE,

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

## meboot2 bootstrap for a specific age

SIMPLESIBREG$forecast.avgfive.modified.youngest <- function(fit, level=80, npaths=B){

   series <- fit$model.data[,ncol(fit$model.data)]

   usePackage("meboot")
   usePackage("forecast")

   series.meboot2 <- meboot2(series, reps=npaths, trim=0)$ensemble

    avgfivefcast  <- function(series) {

     n <- length(series)    # n > 3; length of time series

     fcast <- (series[n-4] + series[n-3] + series[n-2] + series[n-1] + series[n])/5  # point-forecast for series[n+1]

     return(fcast)

   }

   series.boot.forecast.avgfive <- apply(series.meboot2,2,avgfivefcast)  ## one-year ahead forecasts for all time series in the ensemble

   y.paths <- series.boot.forecast.avgfive   ## one-year ahead forecasts

   lower <- as.numeric(quantile(y.paths, (1-level/100)/2, type = 8))
   upper <- as.numeric(quantile(y.paths, (1-level/100)/2 + level/100, type = 8))

   out <- NULL

   out$mean <-  as.numeric(SIMPLESIBREG$avgfive.youngest(series)$mean)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   return(out)

}


SIMPLESIBREG$fit <- SIMPLESIBREG$avgfive.model.fit.youngest
## forecast.avgfive.modified.youngest(fit, level=80, npaths=B)


SIMPLESIBREG$prediction.intervals.individual.ages.avgfive.youngest <- function(fit, level=80, npaths=B){

     h <- 1  # one step ahead forecasts

     avgfive.fit <- fit

     avgfiveboot <- SIMPLESIBREG$forecast.avgfive.modified.youngest(avgfive.fit, level=level, npaths=npaths)

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


SIMPLESIBREG$fit <- SIMPLESIBREG$avgfive.model.fit.youngest
SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest <- SIMPLESIBREG$prediction.intervals.individual.ages.avgfive.youngest(SIMPLESIBREG$fit, level=80, npaths=SIMPLESIBREG$B)

SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest

SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.ctr
SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.lwr
SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.upr
SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$sim


########################################################################################################
########################################################################################################
#
# ARIMA Forecasting  - Forecasting Results for Youngest Age
#
########################################################################################################
########################################################################################################


#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for sibling regression ---------------------------------

SIMPLESIBREG$datalist.arima <- function(datafile, forecastingyear) {



    cols <- colnames(datafile)

    data <- list()
    nms <- NULL
    for (i in 1:(length(cols)-1)) {

         pattern <- paste("c(",cols[1],",",cols[i+1],")",sep="")

         data[[i]] <- subset(datafile,
                             select=eval(parse(text=pattern)))


         data[[i]][data[[i]]<0] <- NA

         usePackage("stringr")
         age <- as.numeric(str_extract(cols[i+1],"[[:digit:]]+"))

         data[[i]]$CY <- data[[i]]$BY + age

         data[[i]] <- data[[i]][,c(3,1,2)]

         BYmax <- forecastingyear-age

         data[[i]] <- subset(data[[i]], BY <BYmax)    ## Added this on Dec. 15th to deal with Spring Creek data!


         paste("age",age, sep="")

         nms <- c(nms, paste("age",age, sep=""))


    }


    names(data) <- nms

    return(data)

}


SIMPLESIBREG$datalist <- SIMPLESIBREG$datalist.arima(SIMPLESIBREG$datafile, SIMPLESIBREG$forecastingyear)  # CY refers to the T variable with highest age

SIMPLESIBREG$datalist <- SIMPLESIBREG$datalist[[1]]   # retain only the data for the youngest age

#---------  fit time series ARIMA model -----------------------------------------


SIMPLESIBREG$arima.model.youngest <- function(datalist, boxcoxtransform){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "arima"  # create a new class

     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     usePackage("forecast")

     usePackage("stringr")

     output$age <-  paste("Age ",  
                                    as.numeric(str_extract(names(tmpdata)[length(names(tmpdata))],"[[:digit:]]")),
                                    sep="")

     if (boxcoxtransform==TRUE) {
              
              output$model <- auto.arima(series, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(series, method="guerrero"))
              
          } else {
              
              output$model <- auto.arima(series, allowmean=TRUE, allowdrift=FALSE)
     }
          
     output$original.data <- datalist
          
     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}


## SIMPLESIBREG$boxcoxtransform <- SIMPLESIBREG$boxcoxtransform

if(is.null(SIMPLESIBREG$boxcoxtransform)) {SIMPLESIBREG$boxcoxtransform <- boxcoxtransform}

SIMPLESIBREG$arima.model.fit.youngest  <- SIMPLESIBREG$arima.model.youngest(SIMPLESIBREG$datalist, SIMPLESIBREG$boxcoxtransform)

SIMPLESIBREG$fit <- SIMPLESIBREG$arima.model.fit.youngest

#---------  Plot fitted time series ARIMA model --------------------------------
# Plot fit time series ARIMA model
#-------------------------------------------------------------------------------

SIMPLESIBREG$plot.fitted.arima.youngest <- function(fit, boxcoxtransform){

    .e <- environment()

    ###
    arimafit <- fit
    CY <- arimafit$model.data[,"CY"]

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

    age <- fit$age
    ###

    CY <- arimafit$model.data[,"CY"]
    
    year <- CY

    actual <- as.numeric(arimamodel$x)
    fitted <- as.numeric(fitted(arimamodel))

    year.stacked <- c(year, year)
    actual.fitted.stacked <- c(actual, fitted)
    age.stacked <- c(rep(paste0(age,": ", modelarima),length(actual)),rep(paste0(age,": ", modelarima),length(fitted)))
    legend.stacked <- c(rep("Actual Values",length(actual)),rep("Fitted Values Obtained via ARIMA Modeling",length(fitted)))

    data.stacked <- data.frame(year=year.stacked, actual.fitted=actual.fitted.stacked, age=age.stacked, legend=legend.stacked)

    usePackage("ggplot2")
    usePackage("scales")

    # environment=.e
    g <- ggplot(data.stacked, aes(year.stacked, actual.fitted), environment=.e) +
    facet_wrap(~age,ncol=1, scales="free_y") +
    # geom_point(aes(shape = legend)) +
    geom_line(aes(colour = legend, group = legend),size=0.6) +
    ## labs(x = "Return Year", y = "Terminal Run", shape = "", colour = "") +
    labs(x = "Return Year", y = paste(SIMPLESIBREG$stockabundance), shape = "", colour = "") +
    # scale_y_continuous("Terminal Run",labels=comma) +
    scale_y_continuous(paste(SIMPLESIBREG$stockabundance),labels=comma) +
    theme_bw() +
    theme(axis.title.x = element_text(size=10,vjust=-0.5),
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

SIMPLESIBREG$fit <- SIMPLESIBREG$arima.model.fit.youngest
SIMPLESIBREG$plot.fitted.arima.youngest(SIMPLESIBREG$fit, SIMPLESIBREG$boxcoxtransform)



#-------------------------------------------------------------------------------
# Report ARIMA Model Results for Youngest Age Class
#-------------------------------------------------------------------------------

SIMPLESIBREG$arima.model.results.youngest <- function(fit){

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

SIMPLESIBREG$arima.model.results.youngest(SIMPLESIBREG$fit)




#--------- point forecast for the youngest age ---------------------------------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

SIMPLESIBREG$point.forecast.arima.youngest <- function(datalist, fit){

     PSY <- datalist$CY[length(datalist$CY)] + 1

     output <- list()

     model <- fit$model

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


     output$Age <- fit$age

     output$Model <- modelarima

     output$RY <- PSY

     output$p <-  round(as.numeric(forecast(arimafit, h=1, level=80, biasadj=FALSE)$mean))

     return(output)
}


SIMPLESIBREG$point.forecast.arima.youngest(SIMPLESIBREG$datalist, SIMPLESIBREG$fit)



SIMPLESIBREG$tmp_list <- SIMPLESIBREG$point.forecast.arima.youngest(SIMPLESIBREG$datalist, SIMPLESIBREG$arima.model.fit.youngest)

SIMPLESIBREG$tmp_df <- do.call(cbind.data.frame, SIMPLESIBREG$tmp_list)

SIMPLESIBREG$results.point.forecast.arima.youngest <- SIMPLESIBREG$tmp_df


SIMPLESIBREG$results.point.forecast.arima.youngest$Model <- as.character(SIMPLESIBREG$results.point.forecast.arima.youngest$Model)

SIMPLESIBREG$results.point.forecast.arima.youngest

str(SIMPLESIBREG$results.point.forecast.arima.youngest)


#-----------------------------------------------------------------------------------------


SIMPLESIBREG$forecast.arima.modified.meboot <- function(fit, boxcoxtransform, level=80, npaths=B){

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
               #### p0 <- as.numeric(forecast::forecast(model0, h=1, level=80)$mean)
           
              if (stop0==FALSE){ 
                p0 <- as.numeric(forecast::forecast.Arima(model0, h=1, level=80, biasadj=FALSE)$mean)
                #   p0 <- sarima
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

   ## This produces strange results?
   ## lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8, na.rm=TRUE))
   ## cat("lower = ", lower, "\n")
   ## upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8, na.rm=TRUE))
   ## cat("upper = ", upper, "\n")

   ## alpha <- (100-level*100)/100

   ## lower <- sort(y.paths)[npaths*(1-alpha/2)]
   ## upper <- sort(y.paths)[npaths*alpha/2]
   
   lower <- quantile( y.paths, (1-level/100)/2, type=8) 
   upper <- quantile( y.paths, (1-level/100)/2 + level/100, type=8)


   out <- NULL

   ## out$mean <-  round(as.numeric(forecast::forecast(arimafit, h=1, level=80)$mean))

   out$mean <-  as.numeric(forecast::forecast(arimafit, h=1, level=80, biasadj=FALSE)$mean)


   ## out$lower <- round(lower)
   ## out$upper <- round(upper)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   return(out)

}

## SIMPLESIBREG$fit <- SIMPLESIBREG$arima.model.fits[[1]]
## debug <- SIMPLESIBREG$forecast.arima.modified.stlboot(SIMPLESIBREG$fit, SIMPLESIBREG$boxcoxtransform, level=80, npaths=SIMPLESIBREG$B)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SIMPLESIBREG$forecast.arima.modified.stlboot <- function(fit, boxcoxtransform, level=80, npaths=B){

    series <- fit$model.data[,ncol(fit$model.data)]

    series <- ts(series, 
                 start = min(fit$model.data$CY), 
                 end = max(fit$model.data$CY), 
                 frequency=1)

    mean(series)

    ## require("TStools")
    usePackage("forecast")
        
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
          
               #### model0 <- arima(series.boot, order = c(p.0, d.0, q.0))
               #### p0 <- as.numeric(forecast::forecast(model0, h=1, level=80)$mean)
           
              if (stop0==FALSE){ 
                p0 <- as.numeric(forecast::forecast.Arima(model0, h=1, level=80, biasadj=FALSE)$mean)
                #   p0 <- sarima
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

    ## This produces strange results? 
    ## lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
    ## upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

    #### alpha <- (100-level)/100

    ## alpha <-  (100-level*100)/100


    ## lower <- sort(y.paths)[npaths*(1-alpha/2)]
    ## upper <- sort(y.paths)[npaths*alpha/2]

    lower <- quantile( y.paths, (1-level/100)/2, type=8) 
    upper <- quantile( y.paths, (1-level/100)/2 + level/100, type=8)

    out <- NULL

    ## out$mean <-  as.numeric(rwf(series,h=1, drift=FALSE, level=level)$mean)

    ## model00 <- forecast::ets(series, model=modelexpsmooth, lambda=lambda)
    out$mean <- as.numeric(forecast::forecast(arimafit , h=1, lambda=lambda, level=80, biasadj=FALSE)$mean)
    
    out$lower <- round(lower)
    out$upper <- round(upper)

    out$sim <- y.paths
    out$series <- out$series
    out$ensemble <- series.stlboot
   
    return(out)

}

## fit <- arima.model.fits[[1]]
## debug <- forecast.arima.modified.stlboot(fit,  boxcoxtransform, level=80, npaths=B)


SIMPLESIBREG$prediction.interval.youngest.age.arima <- function(fit, boxcoxtransform, bootmethod="stlboot", level=80, npaths=B){
  
     out <- NULL
     
     ## arima.fit <- fit$model
          
     if (bootmethod=="stlboot") {
              ## expsmoothboot <- forecast.expsmooth.modified.stlboot(fit=fits[[j]], boxcoxtransform, level=level, npaths=npaths)
              arimaboot <- SIMPLESIBREG$forecast.arima.modified.stlboot(fit = fit, boxcoxtransform, level=level, npaths=B)
              
     }
          
     if (bootmethod=="meboot") {
              ## expsmoothboot <- forecast.expsmooth.modified.meboot(fit=fits[[j]], boxcoxtransform, level=level, npaths=npaths)
              arimaboot <- SIMPLESIBREG$forecast.arima.modified.meboot(fit=fit, boxcoxtransform, level=level, npaths=B)
     }
          
     arima.point.forecast <- round(as.numeric(arimaboot$mean))
         
     out$arima.point.forecast <- arima.point.forecast
          
     arima.lwr.forecast <- round(arimaboot$lower) 
     arima.upr.forecast <- round(arimaboot$upper)
        

     out$PI.ctr <- arima.point.forecast
     out$PI.lwr <- arima.lwr.forecast
     out$PI.upr <- arima.upr.forecast
         
     out$sim <- arimaboot$sim  
     
     results <- out
     
     return(results)
}


## ISSUES WITH lower and upper values being too close to each other!

SIMPLESIBREG$fit <- SIMPLESIBREG$arima.model.fit.youngest

## Hard-code prediction interval for youngest age (ARIMA) to use "stlboot", since stlboot had better performance than meboot for our test data sets
SIMPLESIBREG$pred.int.individual.ages.arima.youngest <- SIMPLESIBREG$prediction.interval.youngest.age.arima(SIMPLESIBREG$fit, 
                                                            SIMPLESIBREG$boxcoxtransform, 
                                                            bootmethod="stlboot", 
                                                            level=80, 
                                                            npaths=SIMPLESIBREG$B)


########################################################################################################
########################################################################################################
#
# Exponential Smoothing - Forecasting Results for Youngest Age
#
########################################################################################################
########################################################################################################


#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for sibling regression ---------------------------------


SIMPLESIBREG$datalist.expsmooth <- function(datafile, forecastingyear) {



    cols <- colnames(datafile)

    data <- list()
    nms <- NULL
    for (i in 1:(length(cols)-1)) {

         pattern <- paste("c(",cols[1],",",cols[i+1],")",sep="")

         data[[i]] <- subset(datafile,
                             select=eval(parse(text=pattern)))


         data[[i]][data[[i]]<0] <- NA

         usePackage("stringr")
         age <- as.numeric(str_extract(cols[i+1],"[[:digit:]]+"))

         data[[i]]$CY <- data[[i]]$BY + age

         data[[i]] <- data[[i]][,c(3,1,2)]

         BYmax <- forecastingyear-age

         data[[i]] <- subset(data[[i]], BY <BYmax)    ## Added this on Dec. 15th to deal with Spring Creek data!


         paste("age",age, sep="")

         nms <- c(nms, paste("age",age, sep=""))
         

    }


    names(data) <- nms

    return(data)

}


SIMPLESIBREG$datalist <-  SIMPLESIBREG$datalist.expsmooth(SIMPLESIBREG$datafile, SIMPLESIBREG$forecastingyear)  # CY refers to the T variable with highest age

SIMPLESIBREG$datalist <- SIMPLESIBREG$datalist[[1]]

#---------  fit exponential smoothing model -----------------------------------------

SIMPLESIBREG$expsmooth.model <- function(datalist, boxcoxtransform){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "expsmooth"  # create a new class

     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     usePackage("forecast")

     usePackage("stringr")

     output$age <-  paste("Age ",  
                                    as.numeric(str_extract(names(tmpdata)[length(names(tmpdata))],"[[:digit:]]")),
                                    sep="")

     if (boxcoxtransform==TRUE) {
     
         output$model <- ets(series, lambda=BoxCox.lambda(series, method="guerrero"))
     
     } else {
     
         output$model <- ets(series) 
     }



     # output[[j]]$formula <- form

     output$original.data <- datalist
          
     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}

SIMPLESIBREG$expsmooth.model.fit.youngest  <- SIMPLESIBREG$expsmooth.model(SIMPLESIBREG$datalist, SIMPLESIBREG$boxcoxtransform)

SIMPLESIBREG$fit <- SIMPLESIBREG$expsmooth.model.fit.youngest


#---------  Plot fitted exponential smoothing model --------------------------------
# Plot fitted exponential smoothing model (ggplot)
#-----------------------------------------------------------------------------------

SIMPLESIBREG$plot.fitted.expsmooth.youngest <- function(fit, boxcoxtransform){
  
    .e <- environment()
   
    expsmoothfit <- fit
    CY <- expsmoothfit$model.data[,"CY"]
       
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

          lambda <- out[out.lambda==TRUE][2]
          modellambda <- str_trim(lambda, side="right")
          modellambda <- str_trim(lambda, side="left")

          modelexpsmooth <- paste0(modelexpsmooth, "; ", modellambda)

    }
    
       
    expsmoothmodel <- fit$model
       
    age <- fit$age

    year <- CY 
    actual <- as.numeric(expsmoothmodel$x)
    fitted <- as.numeric(fitted(expsmoothmodel))
       
    year.stacked <- c(year, year)    
    actual.fitted.stacked <- c(actual, fitted)
    age.stacked <- c( rep(paste(age,": ",modelexpsmooth,sep=""),length(actual)),rep(paste(age,": ",modelexpsmooth,sep=""),length(actual)) )
    legend.stacked <- c(rep("Actual Values",length(actual)),rep("Fitted Values Obtained via Exponential Smoothing",length(fitted)))


    data.stacked <- data.frame(year=year.stacked, actual.fitted=actual.fitted.stacked, age=age.stacked, legend=legend.stacked)    

    usePackage("ggplot2")
    usePackage("scales")

    # environment=.e
    g <- ggplot(data.stacked, aes(year.stacked, actual.fitted), environment=.e) + 
    facet_wrap(~age,ncol=1, scales="free_y") + 
    # geom_point(aes(shape = legend)) + 
    geom_line(aes(colour = legend, group = legend),size=0.6) + 
    ## labs(x = "Return Year", y = "Terminal Run", shape = "", colour = "") + 
    labs(x = "Return Year", y = paste(SIMPLESIBREG$stockabundance), shape = "", colour = "") + 
    # scale_y_continuous("Terminal Run",labels=comma) + 
    scale_y_continuous(paste(SIMPLESIBREG$stockabundance),labels=comma) + 
    theme_bw() + 
    theme(axis.title.x = element_text(size=10,vjust=-0.5),  
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

SIMPLESIBREG$fit <- SIMPLESIBREG$expsmooth.model.fit.youngest

# plot.fitted.expsmooth.youngest(fit, boxcoxtransform)



#-------------------------------------------------------------------------------
# Report Exponential Smoothing Model Results for A Specific Age Class
#-------------------------------------------------------------------------------

SIMPLESIBREG$expsmooth.model.results.youngest <- function(fit){
    
       expsmoothmodel <- fit$model
       age <- fit$age
       
       sink("expsmoothmodel.txt")
       print(expsmoothmodel)
       sink()

       out <- c(age, readLines("expsmoothmodel.txt"))
       
       fn <- "expsmoothmodel.txt"
       if (file.exists(fn)) file.remove(fn)
         
       return(out)
    
}

## SIMPLESIBREG$expsmooth.model.results.youngest(SIMPLESIBREG$fit)



#--------- point forecast for the youngest age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

SIMPLESIBREG$point.forecast.expsmooth.youngest <- function(datalist, fit){
    
     PSY <- datalist$CY[length(datalist$CY)] + 1
   
     output <- list()
          
     fit$model

     model <- fit$model
         
     ## Bella 
         
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

     fn <- "expsmoothfit.txt"
     if (file.exists(fn)) file.remove(fn)
         

     output$Age <- fit$age
         
     output$Model <- modelexpsmooth
           
     output$RY <- PSY 

     # output[[j]]$p <- as.numeric(predict(model, h=1, level=0.80)$pred)

     output$p <-  as.numeric(forecast(expsmoothfit, h=1, level=80)$mean)

     return(output)
}


## point.forecast.expsmooth.youngest(datalist, fit)

SIMPLESIBREG$tmp_list <- SIMPLESIBREG$point.forecast.expsmooth.youngest(SIMPLESIBREG$datalist, SIMPLESIBREG$expsmooth.model.fit.youngest)

SIMPLESIBREG$tmp_df <- do.call(cbind.data.frame, SIMPLESIBREG$tmp_list)

SIMPLESIBREG$results.point.forecast.expsmooth.youngest <- SIMPLESIBREG$tmp_df

SIMPLESIBREG$results.point.forecast.expsmooth.youngest$Model <- as.character(SIMPLESIBREG$results.point.forecast.expsmooth.youngest$Model)

## results.point.forecast.expsmooth.youngest

## str(results.point.forecast.expsmooth.youngest)



############################################################################################
#===========================================================================================
#
# meboot bootstrapping for EXPSMOOTH
#
#
#===========================================================================================
############################################################################################

## fit <- expsmooth.model.fit.youngest

SIMPLESIBREG$forecast.expsmooth.modified.meboot <- function(fit, boxcoxtransform, level=80, npaths=B){

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

    usePackage("stringr")
    
    modelexpsmooth <- str_replace_all(modelexpsmooth, "ETS", "")
    modelexpsmooth <- str_replace_all(modelexpsmooth, "\\(", "")
    modelexpsmooth <- str_replace_all(modelexpsmooth, "\\)", "")
    modelexpsmooth <- str_replace_all(modelexpsmooth, ",", "")

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
          
          model0 <- forecast::ets(series.boot, model=modelexpsmooth, lambda=lambda)
              
          
          ## p0 <- as.numeric(forecast::forecast(model0, h=1, level=0.80, lambda=lambda, biasadj=FALSE)$mean)
          p0 <- as.numeric(forecast::forecast(model0, h=1, level=80, biasadj=FALSE)$mean)
          p0 <- round(p0)
          return(p0)
   
    }
                         
   
   series.boot.forecast.expsmooth <- apply(series.meboot2, 2, expsmoothfcast, modelexpsmooth, lambda)  ## one-year ahead forecasts for all time series in the ensemble
                                  
   y.paths <- series.boot.forecast.expsmooth   ## one-year ahead forecasts


   ## lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   ## upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   lower <- quantile( y.paths, (1-level/100)/2, type=8) 
   upper <- quantile( y.paths, (1-level/100)/2 + level/100, type=8)


   out <- NULL

   ## out$mean <-  as.numeric(rwf(series,h=1, drift=FALSE, level=level)$mean)

   ## model00 <- forecast::ets(series, model=modelexpsmooth, lambda=lambda)
   out$mean <- as.numeric(forecast::forecast(expsmoothfit , h=1, lambda=lambda, level=80, biasadj=FALSE)$mean)
    
   out$lower <- round(lower)
   out$upper <- round(upper)

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   out$trim <- trim.optimal 

   return(out)

}

## SIMPLESIBREG$forecast.expsmooth.modified.meboot(SIMPLESIBREG$fit, SIMPLESIBREG$boxcoxtransform, level=0.8, SIMPLESIBREG$npaths=B)


############################################################################################
#===========================================================================================
#
# stlboot bootstrapping for EXPSMOOTH 
#
#===========================================================================================
############################################################################################

## fit <- expsmooth.model.fit.youngest

SIMPLESIBREG$forecast.expsmooth.modified.stlboot <- function(fit, boxcoxtransform, level=80, npaths=B){

    series <- fit$model.data[,ncol(fit$model.data)]

    series <- ts(series, 
                 start = min(fit$model.data$CY), 
                 end = max(fit$model.data$CY), 
                 frequency=1)

    mean(series)

    ## require("TStools")
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

    usePackage("stringr")
    
    modelexpsmooth <- str_replace_all(modelexpsmooth, "ETS", "")
    modelexpsmooth <- str_replace_all(modelexpsmooth, "\\(", "")
    modelexpsmooth <- str_replace_all(modelexpsmooth, "\\)", "")
    modelexpsmooth <- str_replace_all(modelexpsmooth, ",", "")

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
          
          model0 <- forecast::ets(series.boot, model=modelexpsmooth, lambda=lambda)
              
          
          ## p0 <- as.numeric(forecast::forecast(model0, h=1, level=0.80, lambda=lambda, biasadj=FALSE)$mean)
          p0 <- as.numeric(forecast::forecast(model0, h=1, level=80, biasadj=FALSE)$mean)
          p0 <- round(p0)
          return(p0)
   
    }                                                            
                         
   
   series.boot.forecast.expsmooth <- apply(series.stlboot, 2, expsmoothfcast, modelexpsmooth, lambda)  ## one-year ahead forecasts for all time series in the ensemble
                                  
   y.paths <- series.boot.forecast.expsmooth   ## one-year ahead forecasts


   ## lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   ## upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   lower <- quantile( y.paths, (1-level/100)/2, type=8) 
   upper <- quantile( y.paths, (1-level/100)/2 + level/100, type=8)

   out <- NULL

   ## out$mean <-  as.numeric(rwf(series,h=1, drift=FALSE, level=level)$mean)

   ## model00 <- forecast::ets(series, model=modelexpsmooth, lambda=lambda)
   out$mean <- as.numeric(forecast::forecast(expsmoothfit , h=1, lambda=lambda, level=80, biasadj=FALSE)$mean)
    
   out$lower <- round(lower)
   out$upper <- round(upper)

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.stlboot
   
   return(out)

}



#*******************************************************************************************
#
#------------ compute prediction intervals for point forecasts of individual ages -----------
#
#*******************************************************************************************

SIMPLESIBREG$prediction.interval.youngest.age.expsmooth <- function(fit, boxcoxtransform, bootmethod, level=80, npaths=B){
     
     h <- 1  # one step ahead forecasts 
     
     expsmooth.fit <- fit
               
     # sim stores the simulations obtained by re-sampling the fitted model (?) 

     ## expsmoothboot <- forecast.ets.modified(expsmooth.fit)
     
     if (bootmethod=="stlboot") {
     
         expsmoothboot <- SIMPLESIBREG$forecast.expsmooth.modified.stlboot(fit, boxcoxtransform, level=80, npaths=B)
     
     }
     
     if (bootmethod=="meboot") {
     
         expsmoothboot <- SIMPLESIBREG$forecast.expsmooth.modified.meboot(fit, boxcoxtransform, level=80, npaths=B)
        
     } 
          
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




## fit <- expsmooth.model.fit.youngest 


## Hard code "stlboot" as the bootstrap method of choice for EXPSMOOTH, 
## since it worked better than meboot on the test data sets with considered 
## for stocks with age 

SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest <- SIMPLESIBREG$prediction.interval.youngest.age.expsmooth(fit = SIMPLESIBREG$expsmooth.model.fit.youngest, 
                                                 boxcoxtransform = SIMPLESIBREG$boxcoxtransform, 
                                                 bootmethod = "stlboot", 
                                                 level = 80, 
                                                 npaths = SIMPLESIBREG$B)


## SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest

## str(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest)


#########################################################################################################################
#########################################################################################################################
#
# Putting it all together to compute the Total Abundance
#
#########################################################################################################################
#########################################################################################################################

## SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest

## SIMPLESIBREG$pred.int.individual.ages.arima.youngest

## SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest

## SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression

#==========================================================================================================================
#
#==========================================================================================================================

## SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models 

SIMPLESIBREG$pred.total.age.avgfive.youngest <- list()
SIMPLESIBREG$pred.total.age.avgfive.youngest[[1]] <- SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$age 
SIMPLESIBREG$pred.total.age.avgfive.youngest[[2]] <- SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.ctr
SIMPLESIBREG$pred.total.age.avgfive.youngest[[3]] <- SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.lwr
SIMPLESIBREG$pred.total.age.avgfive.youngest[[4]] <- SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.upr
SIMPLESIBREG$pred.total.age.avgfive.youngest[[5]] <- SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$sim
names(SIMPLESIBREG$pred.total.age.avgfive.youngest) <- c("age","p","pi.lwr","pi.upr","sim")

SIMPLESIBREG$pred.total.age.arima.youngest <- list()
SIMPLESIBREG$pred.total.age.arima.youngest[[1]] <- SIMPLESIBREG$pred.int.individual.ages.arima.youngest$age 
SIMPLESIBREG$pred.total.age.arima.youngest[[2]] <- SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.ctr
SIMPLESIBREG$pred.total.age.arima.youngest[[3]] <- SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.lwr
SIMPLESIBREG$pred.total.age.arima.youngest[[4]] <- SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.upr
SIMPLESIBREG$pred.total.age.arima.youngest[[5]] <- SIMPLESIBREG$pred.int.individual.ages.arima.youngest$sim
names(SIMPLESIBREG$pred.total.age.arima.youngest) <- c("age","p","pi.lwr","pi.upr","sim")

SIMPLESIBREG$pred.total.age.expsmooth.youngest <- list()
SIMPLESIBREG$pred.total.age.expsmooth.youngest[[1]] <- SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$age 
SIMPLESIBREG$pred.total.age.expsmooth.youngest[[2]] <- SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.ctr
SIMPLESIBREG$pred.total.age.expsmooth.youngest[[3]] <- SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.lwr
SIMPLESIBREG$pred.total.age.expsmooth.youngest[[4]] <- SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.upr
SIMPLESIBREG$pred.total.age.expsmooth.youngest[[5]] <- SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$sim
names(SIMPLESIBREG$pred.total.age.expsmooth.youngest) <- c("age","p","pi.lwr","pi.upr","sim")


## str(pred.int.individual.ages.simple.sibling.regression[[1]])

SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models <- list()

## youngest age

SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$age <- SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$age 

## point forecasts for total abundance 

SIMPLESIBREG$p.total.age.avgfive.youngest.plus.oldest <-  round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.ctr) + 
                                 sum(unlist(lapply(SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression,
                                     function(xl) round(xl$p))))  # extract point predictions for older ages, 
                                                                  # as produced by simple sibling regression,
                                                                  # and sum them up

SIMPLESIBREG$p.total.age.arima.youngest.plus.oldest  <-  round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.ctr) + 
                                 sum(unlist(lapply(SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression,
                                     function(xl) round(xl$p))))  # extract point predictions for older ages, 
                                                                  # as produced by simple sibling regression,
                                                                  # and sum them up

SIMPLESIBREG$p.total.age.expsmooth.youngest.plus.oldest  <-  round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.ctr) + 
                                     sum(unlist(lapply(SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression,
                                         function(xl) round(xl$p))))  # extract point predictions for older ages, 
                                                                      # as produced by simple sibling regression,
                                                                      # and sum them up


SIMPLESIBREG$p.list <- list(avgfive = SIMPLESIBREG$p.total.age.avgfive.youngest.plus.oldest , 
               arima = SIMPLESIBREG$p.total.age.arima.youngest.plus.oldest , 
               expsmooth = SIMPLESIBREG$p.total.age.expsmooth.youngest.plus.oldest) 
      
      
SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$p <- SIMPLESIBREG$p.list
     

## simulated values from bootstrap for avgfive

SIMPLESIBREG$sim.total.age.avgfive.oldest <- lapply(SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression,
                                              function(xl) xl$y.star.boot)                                
SIMPLESIBREG$sim.total.age.avgfive.oldest <- Reduce("+", SIMPLESIBREG$sim.total.age.avgfive.oldest)
              
SIMPLESIBREG$sim.total.age.avgfive.youngest.plus.oldest <-  SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$sim + SIMPLESIBREG$sim.total.age.avgfive.oldest

## simulated values from bootstrap for arima

SIMPLESIBREG$sim.total.age.arima.oldest <- lapply(SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression,
                                              function(xl) xl$y.star.boot)                                
SIMPLESIBREG$sim.total.age.arima.oldest <- Reduce("+", SIMPLESIBREG$sim.total.age.arima.oldest)
              
SIMPLESIBREG$sim.total.age.arima.youngest.plus.oldest <-  SIMPLESIBREG$pred.int.individual.ages.arima.youngest$sim + SIMPLESIBREG$sim.total.age.arima.oldest

## simulated values from bootstrap for exponential smoothing

SIMPLESIBREG$sim.total.age.expsmooth.oldest <- lapply(SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression,
                                              function(xl) xl$y.star.boot)                                
SIMPLESIBREG$sim.total.age.expsmooth.oldest <- Reduce("+", SIMPLESIBREG$sim.total.age.expsmooth.oldest)
              
SIMPLESIBREG$sim.total.age.expsmooth.youngest.plus.oldest <-  SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$sim + SIMPLESIBREG$sim.total.age.expsmooth.oldest

SIMPLESIBREG$sim.list <- list(avgfive=SIMPLESIBREG$sim.total.age.avgfive.youngest.plus.oldest,
                 arima=SIMPLESIBREG$sim.total.age.arima.youngest.plus.oldest, 
                 expsmooth=SIMPLESIBREG$sim.total.age.expsmooth.youngest.plus.oldest)

SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$sim <- SIMPLESIBREG$sim.list

#======================================================================================================================

## str(SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models)
                            
#=======================================================================================================================
# Retrospective Evaluation of Point Forecasts for Total Age
#=======================================================================================================================

## SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models

## avgfive

level <- 80

## lower.total.age.avgfive <- as.numeric(quantile(sim.total.age.avgfive.youngest.plus.oldest, 0.5 - level/200, type = 8))
SIMPLESIBREG$lower.total.age.avgfive <- as.numeric(quantile(SIMPLESIBREG$sim.total.age.avgfive.youngest.plus.oldest, (1-level/100)/2, type = 8))

SIMPLESIBREG$lower.total.age.avgfive <- max(0, round(SIMPLESIBREG$lower.total.age.avgfive))

## upper.total.age.avgfive <- as.numeric(quantile(sim.total.age.avgfive.youngest.plus.oldest, 0.5 + level/200, type = 8))  
SIMPLESIBREG$upper.total.age.avgfive <- as.numeric(quantile(SIMPLESIBREG$sim.total.age.avgfive.youngest.plus.oldest, (1-level/100)/2 + level/100, type = 8))  
SIMPLESIBREG$upper.total.age.avgfive <- round(SIMPLESIBREG$upper.total.age.avgfive)

## arima

## lower.total.age.arima <- as.numeric(quantile(sim.total.age.arima.youngest.plus.oldest, 0.5 - level/200, type = 8))
SIMPLESIBREG$lower.total.age.arima <- as.numeric(quantile(SIMPLESIBREG$sim.total.age.arima.youngest.plus.oldest, (1-level/100)/2, type = 8))
SIMPLESIBREG$lower.total.age.arima <- max(0, round(SIMPLESIBREG$lower.total.age.arima))

## upper.total.age.arima <- as.numeric(quantile(sim.total.age.arima.youngest.plus.oldest, 0.5 + level/200, type = 8))  
SIMPLESIBREG$upper.total.age.arima <- as.numeric(quantile(SIMPLESIBREG$sim.total.age.arima.youngest.plus.oldest, (1 - level/100)/2 + level/100, type = 8)) 
SIMPLESIBREG$upper.total.age.arima <- round(SIMPLESIBREG$upper.total.age.arima)

## expsmooth

## lower.total.age.expsmooth <- as.numeric(quantile(sim.total.age.expsmooth.youngest.plus.oldest, 0.5 - level/200, type = 8))
SIMPLESIBREG$lower.total.age.expsmooth <- as.numeric(quantile(SIMPLESIBREG$sim.total.age.expsmooth.youngest.plus.oldest, (1 - level/100)/2, type = 8))
SIMPLESIBREG$lower.total.age.expsmooth <- max(0, round(SIMPLESIBREG$lower.total.age.expsmooth))

## upper.total.age.expsmooth <- as.numeric(quantile(sim.total.age.expsmooth.youngest.plus.oldest, 0.5 + level/200, type = 8))  
SIMPLESIBREG$upper.total.age.expsmooth <- as.numeric(quantile(SIMPLESIBREG$sim.total.age.expsmooth.youngest.plus.oldest, (1 - level/100)/2 + level/100, type = 8)) 
SIMPLESIBREG$upper.total.age.expsmooth <- round(SIMPLESIBREG$upper.total.age.expsmooth)

## add prediction intervals to pred.int.total.age.simple.sibling.regression.all.models

SIMPLESIBREG$lower.list <- list(avgfive=SIMPLESIBREG$lower.total.age.avgfive,
                   arima=SIMPLESIBREG$lower.total.age.arima, 
                   expsmooth=SIMPLESIBREG$lower.total.age.expsmooth)

SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$p.lwr <- SIMPLESIBREG$lower.list


SIMPLESIBREG$upper.list <- list(avgfive=SIMPLESIBREG$upper.total.age.avgfive,
                   arima=SIMPLESIBREG$upper.total.age.arima, 
                   expsmooth=SIMPLESIBREG$upper.total.age.expsmooth)

SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$p.upr <- SIMPLESIBREG$upper.list


###
###  Plot fitted values for the youngest age: avgfive, arima and expsmooth
### 

SIMPLESIBREG$plot.fitted.value.youngest <- function(avgfive.model.fit.youngest,
                                       arima.model.fit.youngest,
                                       expsmooth.model.fit.youngest){

     # avgfive.model.fit.youngest does NOT use Box Cox transformation!  
     gp.avgfive <- SIMPLESIBREG$plot.fitted.avgfive.youngest(avgfive.model.fit.youngest)
     
     # arima.model.fit.youngest can use either no transformation or Box Cox transformation 
     gp.arima <-   SIMPLESIBREG$plot.fitted.arima.youngest(arima.model.fit.youngest, boxcoxtransform)
     
     # expsmooth.model.fit.youngest can use either no transformation or Box Cox transformation 
     gp.expsmooth <- SIMPLESIBREG$plot.fitted.expsmooth.youngest(expsmooth.model.fit.youngest, boxcoxtransform)

     usePackage("gridExtra")
     gp <-  arrangeGrob(gp.avgfive, gp.arima, gp.expsmooth)

     gp
}

SIMPLESIBREG$plot.fitted.value.youngest(SIMPLESIBREG$avgfive.model.fit.youngest,
                                        SIMPLESIBREG$arima.model.fit.youngest,
                                        SIMPLESIBREG$expsmooth.model.fit.youngest)


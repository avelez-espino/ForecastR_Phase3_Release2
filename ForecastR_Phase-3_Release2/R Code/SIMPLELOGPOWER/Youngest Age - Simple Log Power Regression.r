########################################################################################################
########################################################################################################
#
# Naive Time Series Forecasting (Average of Past 5 Years) - Forecasting Results for Youngest Age
#
########################################################################################################
########################################################################################################

cat("Youngest Age - Simple Log Power Regression.R", "\n\n")

## datafile <- read.csv("SPR_thousands.csv", as.is=TRUE)

SIMPLELOGPOWER$datafile_original <- datafile_original

SIMPLELOGPOWER$datafile <- SIMPLELOGPOWER$datafile_original

SIMPLELOGPOWER$datafilesub <- SIMPLELOGPOWER$datafile

SIMPLELOGPOWER$extract_ages <- sort(unique(SIMPLELOGPOWER$datafilesub$Age_Class))
SIMPLELOGPOWER$extract_names <- paste("T",SIMPLELOGPOWER$extract_ages,sep="")
SIMPLELOGPOWER$extract_names <- c("BY",SIMPLELOGPOWER$extract_names)

SIMPLELOGPOWER$tmpsub <- list()
for (i in 1:length(SIMPLELOGPOWER$extract_ages)){
     if (SIMPLELOGPOWER$stockabundance=="Terminal Run"){
     SIMPLELOGPOWER$tmpsub[[i]] <- subset(SIMPLELOGPOWER$datafilesub, Age_Class==SIMPLELOGPOWER$extract_ages[i])[,c("Brood_Year","Average_Terminal_Run")]
     } else if (SIMPLELOGPOWER$stockabundance=="Escapement") {
      SIMPLELOGPOWER$tmpsub[[i]] <- subset(SIMPLELOGPOWER$datafilesub, Age_Class==SIMPLELOGPOWER$extract_ages[i])[,c("Brood_Year","Average_Escapement")]
     } else if (SIMPLELOGPOWER$stockabundance=="Production") {
      SIMPLELOGPOWER$tmpsub[[i]] <- subset(SIMPLELOGPOWER$datafilesub, Age_Class==SIMPLELOGPOWER$extract_ages[i])[,c("Brood_Year","Average_Production")]
     }
}

SIMPLELOGPOWER$list.of.data.frames <- SIMPLELOGPOWER$tmpsub
SIMPLELOGPOWER$merged.data.frame = Reduce(function(...) merge(...,by="Brood_Year", all=T), SIMPLELOGPOWER$list.of.data.frames)

SIMPLELOGPOWER$datafile_new <- SIMPLELOGPOWER$merged.data.frame
names(SIMPLELOGPOWER$datafile_new) <- SIMPLELOGPOWER$extract_names

SIMPLELOGPOWER$datafile <- SIMPLELOGPOWER$datafile_new



## cat("Working data file is: ","\n")
## print(datafile)
## cat("\n")


#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for naive forecasting (average of past 5 years) ---------------------------------

SIMPLELOGPOWER$datalist.avgfive <- function(datafile, forecastingyear) {


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


SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist.avgfive(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)  # CY refers to the T variable with highest age

SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist[[1]] ## retain only the data for the youngest ages


#--------- plot data to be used for naive forecasting (average of past 5 years) (uses ggplot) ---------------------------

SIMPLELOGPOWER$plot.data.avgfive.youngest <- function(datalist){

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
               scale_y_continuous(paste(SIMPLELOGPOWER$stockabundance),labels=comma) +
                theme_bw() +
                   theme(plot.title=element_text(size=12, hjust=0.5),
                         axis.title.x = element_text(size=10,vjust=-0.5),
                         axis.title.y = element_text(size=10,vjust=1),
                         axis.text.x = element_text(size=8),
                         axis.text.y = element_text(size=8)
                         )

}

SIMPLELOGPOWER$plot.data.avgfive.youngest(SIMPLELOGPOWER$datalist)


#--------- helper function for computing the average of the past 5 years of a time series -----

SIMPLELOGPOWER$avgfive.youngest  <- function(series) {

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

SIMPLELOGPOWER$avgfive.model.youngest <- function(datalist){

     output <- list() # start to build the S3 class storing the output

     class(output) <- "avgfive"  # create a new class

     tmpdata <- datalist[complete.cases(datalist),]

     series <- tmpdata[ ,ncol(tmpdata)]

     usePackage("forecast")

     usePackage("stringr")

     output$age <-  paste("Age ",
                          as.numeric(str_extract(names(tmpdata)[length(names(tmpdata))],"[[:digit:]]")),
                          sep="")

     output$model <- SIMPLELOGPOWER$avgfive.youngest(series)

     output$original.data <- datalist

     output$model.data <- datalist[complete.cases(datalist),]

     return(output)

}

SIMPLELOGPOWER$avgfive.model.fit.youngest  <- SIMPLELOGPOWER$avgfive.model.youngest(SIMPLELOGPOWER$datalist)

SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$avgfive.model.fit.youngest

#---------  Plot naive model (average of past 5 years) --------------------------------
# Plot fitted naive model (ggplot)
#-------------------------------------------------------------------------------

SIMPLELOGPOWER$plot.fitted.avgfive.youngest <- function(fit){

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
    labs(x = "Return Year", y = paste(SIMPLELOGPOWER$stockabundance), shape = "", colour = "") +
    # scale_y_continuous("Terminal Run",labels=comma) +
    scale_y_continuous(paste(SIMPLELOGPOWER$stockabundance),labels=comma) +
    theme_bw() +
    theme(plot.title=element_text(size=12, hjust=0.5), 
          axis.title.x = element_text(size=10,vjust=-0.5),
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

SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$avgfive.model.fit.youngest
SIMPLELOGPOWER$plot.fitted.avgfive.youngest(SIMPLELOGPOWER$fit)


#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

SIMPLELOGPOWER$point.forecast.avgfive.youngest <- function(datalist, fit){

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


SIMPLELOGPOWER$point.forecast.avgfive.youngest(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$fit)

SIMPLELOGPOWER$tmp_list <- SIMPLELOGPOWER$point.forecast.avgfive.youngest(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$avgfive.model.fit.youngest)

SIMPLELOGPOWER$tmp_df <- do.call(cbind.data.frame, SIMPLELOGPOWER$tmp_list)

SIMPLELOGPOWER$results.point.forecast.avgfive.youngest <- SIMPLELOGPOWER$tmp_df

SIMPLELOGPOWER$results.point.forecast.avgfive.youngest$Model <- as.character(SIMPLELOGPOWER$results.point.forecast.avgfive.youngest$Model)

SIMPLELOGPOWER$results.point.forecast.avgfive.youngest

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

SIMPLELOGPOWER$forecast.avgfive.modified.youngest <- function(fit, level=80, npaths=B){

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

   out$mean <-  as.numeric(SIMPLELOGPOWER$avgfive.youngest(series)$mean)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   return(out)

}


SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$avgfive.model.fit.youngest
SIMPLELOGPOWER$forecast.avgfive.modified.youngest(SIMPLELOGPOWER$fit, level=80, npaths=B)




SIMPLELOGPOWER$prediction.intervals.individual.ages.avgfive.youngest <- function(fit, level=80, npaths=B){

     h <- 1  # one step ahead forecasts

     avgfive.fit <- fit

     avgfiveboot <- SIMPLELOGPOWER$forecast.avgfive.modified.youngest(avgfive.fit, level=level, npaths=npaths)

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


SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$avgfive.model.fit.youngest
SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest <- SIMPLELOGPOWER$prediction.intervals.individual.ages.avgfive.youngest(SIMPLELOGPOWER$fit, level=80, npaths=B)

SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest

## pred.int.individual.ages.avgfive.youngest

## pred.int.individual.ages.avgfive.youngest$PI.ctr
## pred.int.individual.ages.avgfive.youngest$PI.lwr
## pred.int.individual.ages.avgfive.youngest$PI.upr
## pred.int.individual.ages.avgfive.youngest$sim


########################################################################################################
########################################################################################################
#
# ARIMA Forecasting  - Forecasting Results for Youngest Age
#
########################################################################################################
########################################################################################################


#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for sibling regression ---------------------------------

SIMPLELOGPOWER$datalist.arima <- function(datafile, forecastingyear) {



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


SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist.arima(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)  # CY refers to the T variable with highest age

SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist[[1]]   # retain only the data for the youngest age

#---------  fit time series ARIMA model -----------------------------------------


SIMPLELOGPOWER$arima.model.youngest <- function(datalist, boxcoxtransform){

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

SIMPLELOGPOWER$boxcoxtransform <- boxcoxtransform

SIMPLELOGPOWER$arima.model.fit.youngest  <- SIMPLELOGPOWER$arima.model.youngest(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$boxcoxtransform)

SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$arima.model.fit.youngest

#---------  Plot fitted time series ARIMA model --------------------------------
# Plot fit time series ARIMA model
#-------------------------------------------------------------------------------

SIMPLELOGPOWER$plot.fitted.arima.youngest <- function(fit, boxcoxtransform){

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
    labs(x = "Return Year", y = paste(SIMPLELOGPOWER$stockabundance), shape = "", colour = "") +
    # scale_y_continuous("Terminal Run",labels=comma) +
    scale_y_continuous(paste(SIMPLELOGPOWER$stockabundance),labels=comma) +
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

SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$arima.model.fit.youngest
SIMPLELOGPOWER$plot.fitted.arima.youngest(SIMPLELOGPOWER$fit, SIMPLELOGPOWER$boxcoxtransform)



#-------------------------------------------------------------------------------
# Report ARIMA Model Results for Youngest Age Class
#-------------------------------------------------------------------------------

SIMPLELOGPOWER$arima.model.results.youngest <- function(fit){

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

SIMPLELOGPOWER$arima.model.results.youngest(SIMPLELOGPOWER$fit)




#--------- point forecast for the youngest age ---------------------------------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

SIMPLELOGPOWER$point.forecast.arima.youngest <- function(datalist, fit){

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


SIMPLELOGPOWER$point.forecast.arima.youngest(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$fit)



SIMPLELOGPOWER$tmp_list <- SIMPLELOGPOWER$point.forecast.arima.youngest(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$arima.model.fit.youngest)

SIMPLELOGPOWER$tmp_df <- do.call(cbind.data.frame, SIMPLELOGPOWER$tmp_list)

SIMPLELOGPOWER$results.point.forecast.arima.youngest <- SIMPLELOGPOWER$tmp_df


SIMPLELOGPOWER$results.point.forecast.arima.youngest$Model <- as.character(SIMPLELOGPOWER$results.point.forecast.arima.youngest$Model)

## results.point.forecast.arima.youngest

## str(results.point.forecast.arima.youngest)


#-----------------------------------------------------------------------------------------


SIMPLELOGPOWER$forecast.arima.modified.meboot <- function(fit, boxcoxtransform, level=80, npaths=B){

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

## SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$arima.model.fits[[1]]
## SIMPLELOGPOWER$debug <- SIMPLELOGPOWER$forecast.arima.modified.stlboot(SIMPLELOGPOWER$fit, SIMPLELOGPOWER$boxcoxtransform, level=80, npaths=B)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SIMPLELOGPOWER$forecast.arima.modified.stlboot <- function(fit, boxcoxtransform, level=80, npaths=B){

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

## SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$arima.model.fits[[1]]
## SIMPLELOGPOWER$debug <- SIMPLELOGPOWER$forecast.arima.modified.stlboot(SIMPLELOGPOWER$fit,  SIMPLELOGPOWER$boxcoxtransform, level=80, npaths=B)


SIMPLELOGPOWER$prediction.interval.youngest.age.arima <- function(fit, boxcoxtransform, bootmethod="stlboot", level=80, npaths=B){

     out <- NULL

     ## arima.fit <- fit$model

     if (bootmethod=="stlboot") {
              ## expsmoothboot <- forecast.expsmooth.modified.stlboot(fit=fits[[j]], boxcoxtransform, level=level, npaths=npaths)
              arimaboot <- SIMPLELOGPOWER$forecast.arima.modified.stlboot(fit = fit, boxcoxtransform, level=level, npaths=B)

     }

     if (bootmethod=="meboot") {
              ## expsmoothboot <- forecast.expsmooth.modified.meboot(fit=fits[[j]], boxcoxtransform, level=level, npaths=npaths)
              arimaboot <- SIMPLELOGPOWER$forecast.arima.modified.meboot(fit=fit, boxcoxtransform, level=level, npaths=B)
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

SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$arima.model.fit.youngest

## Hard-code prediction interval for youngest age (ARIMA) to use "stlboot", since stlboot had better performance than meboot for our test data sets
SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest <- SIMPLELOGPOWER$prediction.interval.youngest.age.arima(SIMPLELOGPOWER$fit,
                                                              SIMPLELOGPOWER$boxcoxtransform,
                                                              bootmethod="stlboot", level=80, npaths=B)


#------------------------------------------------------------------------------------------
# IGNORE BELOW!!!!
#------------------------------------------------------------------------------------------

########################################################################################################
########################################################################################################
#
# Exponential Smoothing - Forecasting Results for Youngest Age
#
########################################################################################################
########################################################################################################


#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for sibling regression ---------------------------------


SIMPLELOGPOWER$datalist.expsmooth <- function(datafile, forecastingyear) {



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


SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist.expsmooth(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)  # CY refers to the T variable with highest age

SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist[[1]]

#---------  fit exponential smoothing model -----------------------------------------

SIMPLELOGPOWER$expsmooth.model <- function(datalist, boxcoxtransform){

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

SIMPLELOGPOWER$expsmooth.model.fit.youngest  <- SIMPLELOGPOWER$expsmooth.model(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$boxcoxtransform)

SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$expsmooth.model.fit.youngest


#---------  Plot fitted exponential smoothing model --------------------------------
# Plot fitted exponential smoothing model (ggplot)
#-----------------------------------------------------------------------------------

SIMPLELOGPOWER$plot.fitted.expsmooth.youngest <- function(fit, boxcoxtransform){

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
    labs(x = "Return Year", y = paste(SIMPLELOGPOWER$stockabundance), shape = "", colour = "") +
    # scale_y_continuous("Terminal Run",labels=comma) +
    scale_y_continuous(paste(SIMPLELOGPOWER$stockabundance),labels=comma) +
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

SIMPLELOGPOWER$fit <- SIMPLELOGPOWER$expsmooth.model.fit.youngest

SIMPLELOGPOWER$plot.fitted.expsmooth.youngest(SIMPLELOGPOWER$fit, SIMPLELOGPOWER$boxcoxtransform)



#-------------------------------------------------------------------------------
# Report Exponential Smoothing Model Results for A Specific Age Class
#-------------------------------------------------------------------------------

SIMPLELOGPOWER$expsmooth.model.results.youngest <- function(fit){

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

SIMPLELOGPOWER$expsmooth.model.results.youngest(SIMPLELOGPOWER$fit)



#--------- point forecast for the youngest age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

SIMPLELOGPOWER$point.forecast.expsmooth.youngest <- function(datalist, fit){

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

SIMPLELOGPOWER$tmp_list <- SIMPLELOGPOWER$point.forecast.expsmooth.youngest(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$expsmooth.model.fit.youngest)

SIMPLELOGPOWER$tmp_df <- do.call(cbind.data.frame, SIMPLELOGPOWER$tmp_list)

SIMPLELOGPOWER$results.point.forecast.expsmooth.youngest <- SIMPLELOGPOWER$tmp_df

SIMPLELOGPOWER$results.point.forecast.expsmooth.youngest$Model <- as.character(SIMPLELOGPOWER$results.point.forecast.expsmooth.youngest$Model)

SIMPLELOGPOWER$results.point.forecast.expsmooth.youngest

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

SIMPLELOGPOWER$forecast.expsmooth.modified.meboot <- function(fit, boxcoxtransform, level=80, npaths=B){

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

## SIMPLELOGPOWER$forecast.expsmooth.modified.meboot(SIMPLELOGPOWER$fit, SIMPLELOGPOWER$boxcoxtransform, level=0.8, npaths=B)


############################################################################################
#===========================================================================================
#
# stlboot bootstrapping for EXPSMOOTH
#
#===========================================================================================
############################################################################################

## fit <- expsmooth.model.fit.youngest

SIMPLELOGPOWER$forecast.expsmooth.modified.stlboot <- function(fit, boxcoxtransform, level=80, npaths=B){

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

SIMPLELOGPOWER$prediction.interval.youngest.age.expsmooth <- function(fit, boxcoxtransform, bootmethod, level=80, npaths=B){

     h <- 1  # one step ahead forecasts

     expsmooth.fit <- fit

     # sim stores the simulations obtained by re-sampling the fitted model (?)

     ## expsmoothboot <- forecast.ets.modified(expsmooth.fit)

     if (bootmethod=="stlboot") {

         expsmoothboot <- SIMPLELOGPOWER$forecast.expsmooth.modified.stlboot(fit, boxcoxtransform, level=80, npaths=B)

     }

     if (bootmethod=="meboot") {

         expsmoothboot <- SIMPLELOGPOWER$forecast.expsmooth.modified.meboot(fit, boxcoxtransform, level=80, npaths=B)

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


SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest <- SIMPLELOGPOWER$prediction.interval.youngest.age.expsmooth(
                                                 fit = SIMPLELOGPOWER$expsmooth.model.fit.youngest,
                                                 boxcoxtransform = SIMPLELOGPOWER$boxcoxtransform,
                                                 bootmethod = "stlboot",
                                                 level = 80,
                                                 npaths = B)


SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest

## str(pred.int.individual.ages.expsmooth.youngest)


#########################################################################################################################
#########################################################################################################################
#
# Putting it all together to compute the Total Abundance
#
#########################################################################################################################
#########################################################################################################################

SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest

SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest

SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest

SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression

#==========================================================================================================================
#
#==========================================================================================================================

## pred.int.total.age.simple.sibling.regression.all.models

SIMPLELOGPOWER$pred.total.age.avgfive.youngest <- list()
SIMPLELOGPOWER$pred.total.age.avgfive.youngest[[1]] <- SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$age
SIMPLELOGPOWER$pred.total.age.avgfive.youngest[[2]] <- SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.ctr
SIMPLELOGPOWER$pred.total.age.avgfive.youngest[[3]] <- SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.lwr
SIMPLELOGPOWER$pred.total.age.avgfive.youngest[[4]] <- SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.upr
SIMPLELOGPOWER$pred.total.age.avgfive.youngest[[5]] <- SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$sim
names(SIMPLELOGPOWER$pred.total.age.avgfive.youngest) <- c("age","p","pi.lwr","pi.upr","sim")

SIMPLELOGPOWER$pred.total.age.arima.youngest <- list()
SIMPLELOGPOWER$pred.total.age.arima.youngest[[1]] <- SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$age
SIMPLELOGPOWER$pred.total.age.arima.youngest[[2]] <- SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.ctr
SIMPLELOGPOWER$pred.total.age.arima.youngest[[3]] <- SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.lwr
SIMPLELOGPOWER$pred.total.age.arima.youngest[[4]] <- SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.upr
SIMPLELOGPOWER$pred.total.age.arima.youngest[[5]] <- SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$sim
names(SIMPLELOGPOWER$pred.total.age.arima.youngest) <- c("age","p","pi.lwr","pi.upr","sim")

SIMPLELOGPOWER$pred.total.age.expsmooth.youngest <- list()
SIMPLELOGPOWER$pred.total.age.expsmooth.youngest[[1]] <- SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$age
SIMPLELOGPOWER$pred.total.age.expsmooth.youngest[[2]] <- SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.ctr
SIMPLELOGPOWER$pred.total.age.expsmooth.youngest[[3]] <- SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.lwr
SIMPLELOGPOWER$pred.total.age.expsmooth.youngest[[4]] <- SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.upr
SIMPLELOGPOWER$pred.total.age.expsmooth.youngest[[5]] <- SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$sim
names(SIMPLELOGPOWER$pred.total.age.expsmooth.youngest) <- c("age","p","pi.lwr","pi.upr","sim")


## str(pred.int.individual.ages.simple.sibling.regression[[1]])

SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models <- list()

## youngest age

SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$age <- SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$age

## point forecasts for total abundance

SIMPLELOGPOWER$p.total.age.avgfive.youngest.plus.oldest <-  round(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.ctr) +
                                 sum(unlist(lapply(SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                     function(xl) round(xl$p))))  # extract point predictions for older ages,
                                                                  # as produced by simple sibling regression,
                                                                  # and sum them up

SIMPLELOGPOWER$p.total.age.arima.youngest.plus.oldest  <-  round(SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.ctr) +
                                 sum(unlist(lapply(SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                     function(xl) round(xl$p))))  # extract point predictions for older ages,
                                                                  # as produced by simple sibling regression,
                                                                  # and sum them up

SIMPLELOGPOWER$p.total.age.expsmooth.youngest.plus.oldest  <-  round(SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.ctr) +
                                     sum(unlist(lapply(SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                         function(xl) round(xl$p))))  # extract point predictions for older ages,
                                                                      # as produced by simple sibling regression,
                                                                      # and sum them up


SIMPLELOGPOWER$p.list <- list(avgfive = SIMPLELOGPOWER$p.total.age.avgfive.youngest.plus.oldest ,
               arima = SIMPLELOGPOWER$p.total.age.arima.youngest.plus.oldest ,
               expsmooth = SIMPLELOGPOWER$p.total.age.expsmooth.youngest.plus.oldest)


SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$p <- SIMPLELOGPOWER$p.list


## simulated values from bootstrap for avgfive

SIMPLELOGPOWER$sim.total.age.avgfive.oldest <- lapply(SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                              function(xl) xl$y.star.boot)
SIMPLELOGPOWER$sim.total.age.avgfive.oldest <- Reduce("+", SIMPLELOGPOWER$sim.total.age.avgfive.oldest)

SIMPLELOGPOWER$sim.total.age.avgfive.youngest.plus.oldest <-  SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$sim + SIMPLELOGPOWER$sim.total.age.avgfive.oldest

## simulated values from bootstrap for arima

SIMPLELOGPOWER$sim.total.age.arima.oldest <- lapply(SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                              function(xl) xl$y.star.boot)
SIMPLELOGPOWER$sim.total.age.arima.oldest <- Reduce("+", SIMPLELOGPOWER$sim.total.age.arima.oldest)

SIMPLELOGPOWER$sim.total.age.arima.youngest.plus.oldest <-  SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$sim + SIMPLELOGPOWER$sim.total.age.arima.oldest

## simulated values from bootstrap for exponential smoothing

SIMPLELOGPOWER$sim.total.age.expsmooth.oldest <- lapply(SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                              function(xl) xl$y.star.boot)
SIMPLELOGPOWER$sim.total.age.expsmooth.oldest <- Reduce("+", SIMPLELOGPOWER$sim.total.age.expsmooth.oldest)

SIMPLELOGPOWER$sim.total.age.expsmooth.youngest.plus.oldest <-  SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$sim + SIMPLELOGPOWER$sim.total.age.expsmooth.oldest

SIMPLELOGPOWER$sim.list <- list(avgfive=SIMPLELOGPOWER$sim.total.age.avgfive.youngest.plus.oldest,
                 arima=SIMPLELOGPOWER$sim.total.age.arima.youngest.plus.oldest,
                 expsmooth=SIMPLELOGPOWER$sim.total.age.expsmooth.youngest.plus.oldest)

SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$sim <- SIMPLELOGPOWER$sim.list


#=======================================================================================================================
# Retrospective Evaluation of Point Forecasts for Total Age
#=======================================================================================================================

level <- 80

## lower.total.age.avgfive <- as.numeric(quantile(sim.total.age.avgfive.youngest.plus.oldest, 0.5 - level/200, type = 8))
SIMPLELOGPOWER$lower.total.age.avgfive <- as.numeric(quantile(SIMPLELOGPOWER$sim.total.age.avgfive.youngest.plus.oldest, (1-level/100)/2, type = 8))

SIMPLELOGPOWER$lower.total.age.avgfive <- max(0, round(SIMPLELOGPOWER$lower.total.age.avgfive))

## upper.total.age.avgfive <- as.numeric(quantile(sim.total.age.avgfive.youngest.plus.oldest, 0.5 + level/200, type = 8))
SIMPLELOGPOWER$upper.total.age.avgfive <- as.numeric(quantile(SIMPLELOGPOWER$sim.total.age.avgfive.youngest.plus.oldest, (1-level/100)/2 + level/100, type = 8))
SIMPLELOGPOWER$upper.total.age.avgfive <- round(SIMPLELOGPOWER$upper.total.age.avgfive)

## arima

## lower.total.age.arima <- as.numeric(quantile(sim.total.age.arima.youngest.plus.oldest, 0.5 - level/200, type = 8))
SIMPLELOGPOWER$lower.total.age.arima <- as.numeric(quantile(SIMPLELOGPOWER$sim.total.age.arima.youngest.plus.oldest, (1-level/100)/2, type = 8))
SIMPLELOGPOWER$lower.total.age.arima <- max(0, round(SIMPLELOGPOWER$lower.total.age.arima))

## upper.total.age.arima <- as.numeric(quantile(sim.total.age.arima.youngest.plus.oldest, 0.5 + level/200, type = 8))
SIMPLELOGPOWER$upper.total.age.arima <- as.numeric(quantile(SIMPLELOGPOWER$sim.total.age.arima.youngest.plus.oldest, (1 - level/100)/2 + level/100, type = 8))
SIMPLELOGPOWER$upper.total.age.arima <- round(SIMPLELOGPOWER$upper.total.age.arima)

## expsmooth

## lower.total.age.expsmooth <- as.numeric(quantile(sim.total.age.expsmooth.youngest.plus.oldest, 0.5 - level/200, type = 8))
SIMPLELOGPOWER$lower.total.age.expsmooth <- as.numeric(quantile(SIMPLELOGPOWER$sim.total.age.expsmooth.youngest.plus.oldest, (1 - level/100)/2, type = 8))
SIMPLELOGPOWER$lower.total.age.expsmooth <- max(0, round(SIMPLELOGPOWER$lower.total.age.expsmooth))

## upper.total.age.expsmooth <- as.numeric(quantile(sim.total.age.expsmooth.youngest.plus.oldest, 0.5 + level/200, type = 8))
SIMPLELOGPOWER$upper.total.age.expsmooth <- as.numeric(quantile(SIMPLELOGPOWER$sim.total.age.expsmooth.youngest.plus.oldest, (1 - level/100)/2 + level/100, type = 8))
SIMPLELOGPOWER$upper.total.age.expsmooth <- round(SIMPLELOGPOWER$upper.total.age.expsmooth)

## add prediction intervals to pred.int.total.age.simple.sibling.regression.all.models

SIMPLELOGPOWER$lower.list <- list(avgfive=SIMPLELOGPOWER$lower.total.age.avgfive,
                   arima=SIMPLELOGPOWER$lower.total.age.arima,
                   expsmooth=SIMPLELOGPOWER$lower.total.age.expsmooth)

SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$p.lwr <- SIMPLELOGPOWER$lower.list


SIMPLELOGPOWER$upper.list <- list(avgfive=SIMPLELOGPOWER$upper.total.age.avgfive,
                   arima=SIMPLELOGPOWER$upper.total.age.arima,
                   expsmooth=SIMPLELOGPOWER$upper.total.age.expsmooth)

SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$p.upr <- SIMPLELOGPOWER$upper.list


###
###  Plot fitted values for the youngest age: avgfive, arima and expsmooth
###

SIMPLELOGPOWER$plot.fitted.value.youngest <- function(avgfive.model.fit.youngest,
                                       arima.model.fit.youngest,
                                       expsmooth.model.fit.youngest, boxcoxtransform){

     # avgfive.model.fit.youngest does NOT use Box Cox transformation!
     gp.avgfive <- SIMPLELOGPOWER$plot.fitted.avgfive.youngest(avgfive.model.fit.youngest)

     # arima.model.fit.youngest can use either no transformation or Box Cox transformation
     gp.arima <-   SIMPLELOGPOWER$plot.fitted.arima.youngest(arima.model.fit.youngest, boxcoxtransform)

     # expsmooth.model.fit.youngest can use either no transformation or Box Cox transformation
     gp.expsmooth <- SIMPLELOGPOWER$plot.fitted.expsmooth.youngest(expsmooth.model.fit.youngest, boxcoxtransform)

     usePackage("gridExtra")
     gp <-  arrangeGrob(gp.avgfive, gp.arima, gp.expsmooth)

     gp
}

SIMPLELOGPOWER$plot.fitted.value.youngest(SIMPLELOGPOWER$avgfive.model.fit.youngest,
                                          SIMPLELOGPOWER$arima.model.fit.youngest,
                                          SIMPLELOGPOWER$expsmooth.model.fit.youngest, 
                                          SIMPLELOGPOWER$boxcoxtransform)


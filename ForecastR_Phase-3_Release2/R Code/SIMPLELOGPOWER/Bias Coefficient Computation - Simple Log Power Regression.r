##########################################################################################
#
#  Bias coefficients of retrospective forecast errors - youngest age
#
##########################################################################################


SIMPLELOGPOWER$bias.coefficient.afe.youngest.age.retro.simplelogpower <- function(results, stockabundance, index.year){

    par(mfrow=c(1,1), mar=c(2,2,2,2))
    
    if (results$method == "naive forecasting (i.e., average of previous five years)"){
    
        datalist <- SIMPLELOGPOWER$datalist.avgfive(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)
        results.avgfive <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.avgfive.youngest(datalist, index.year)
        data <- cbind.data.frame(a = results.avgfive[[1]]$a,
                                 p = results.avgfive[[1]]$p,
                                 e = results.avgfive[[1]]$e)
                                 
        model <- "Naive Model (Average of Previous Five Years)"

    
    }
    
    if (results$method == "ARIMA forecasting"){

        datalist <- SIMPLELOGPOWER$datalist.arima(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)
        
        results.arima <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.arima.youngest(datalist, SIMPLELOGPOWER$forecastingyear, SIMPLELOGPOWER$boxcoxtransform, index.year)
        
        data <- cbind.data.frame(a = results.arima[[1]]$a,
                                 p = results.arima[[1]]$p,
                                 e = results.arima[[1]]$e)
                                 
         model <- "ARIMA Model"


    }
    
    if (results$method == "exponential smoothing forecasting"){
    
        datalist <- SIMPLELOGPOWER$datalist.expsmooth(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)

        results.expsmooth <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.expsmooth.youngest(datalist, SIMPLELOGPOWER$forecastingyear, SIMPLELOGPOWER$boxcoxtransform, index.year)

        data <- cbind.data.frame(a = results.expsmooth[[1]]$a,
                                 p = results.expsmooth[[1]]$p,
                                 e = results.expsmooth[[1]]$e)
                                 
         model <- "Exponential Smoothing Model"

    }




    error <- data$e

    mre.error <- mre(error)

    ## plot.mre(mre.error)

    ## gamma <- Arg(mre.error)

    ## bias <- 1 - 4*gamma/pi

    ## k <- length(bias)

    bias.coeff.updated(mre.error, outplot=2)

    ## bias.coeff.afe.youngest.age.retro.simplelogpower <<- bias.coeff.updated(mre.error, outplot=0)

    SIMPLELOGPOWER$bias.coeff.afe.youngest.age.retro.simplelogpower <<- bias.coeff.updated(mre.error, outplot=0)

    ## bias.coeff(mre.error, outplot=0)
    
    age.tmp <- names(results$retro$resjoin)[1]
    usePackage("stringr")
    
    age.tmp <- str_replace(age.tmp, "_", " ")

    tmp <- paste0(stockabundance, " at ", age.tmp, ": ", model)

    title(main=paste0(tmp))

    usePackage("gridGraphics")

    grid.echo()
    grid.grab() -> mapgrob

    return(mapgrob)

}





windows()

SIMPLELOGPOWER$results <- SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower

SIMPLELOGPOWER$bias.coefficient.afe.youngest.age.retro.simplelogpower(SIMPLELOGPOWER$results, SIMPLELOGPOWER$stockabundance, SIMPLELOGPOWER$index.year)

SIMPLELOGPOWER$bias.coeff.afe.youngest.age.retro.simplelogpower


## results$$retro$resjoin

## rm(results)

SIMPLELOGPOWER$results <- NULL 

##########################################################################################
#
#  Bias coefficients of retrospective forecast errors - older ages
#
##########################################################################################


SIMPLELOGPOWER$bias.coefficients.afe.older.ages.retro.simplelogpower <- function(fits, results, stockabundance){


    par(mfrow=c(length(fits),1), mar=c(2,2,2,2), cex.main=0.9)

    ## bias.coeff.afe.individual.ages.retro.simplelogpower <<- NULL

    SIMPLELOGPOWER$bias.coeff.afe.individual.ages.retro.simplelogpower <<- NULL 

    nms <- NULL
    for (i in 1:length(fits)) {

        data <- results[[i]]

        names(data)[names(data)=="a"] <- "Actual"
        names(data)[names(data)=="p"] <- "Forecast"
        names(data)[names(data)=="e"] <- "Error"

        error <- data$Error

        mre.error <- mre(error)

        ## plot.mre(mre.error)

        ## gamma <- Arg(mre.error)

        ## bias <- 1 - 4*gamma/pi

        ## k <- length(bias)


        bias.coeff.updated(mre.error, outplot=2)

        ## bias.coeff.afe.individual.ages.retro.simplelogpower <<- c(bias.coeff.afe.individual.ages.retro.simplelogpower,
        ##                                                    bias.coeff.updated(mre.error, outplot=0))
                                                            
        SIMPLELOGPOWER$bias.coeff.afe.individual.ages.retro.simplelogpower <<- c(SIMPLELOGPOWER$bias.coeff.afe.individual.ages.retro.simplelogpower,
                                                            bias.coeff.updated(mre.error, outplot=0))

        age.tmp <- formula(fits[[i]]$model)[[2]]
        
        age.tmp <- as.character(age.tmp)
        
        age.tmp <- strsplit(age.tmp, "\\(")[[2]]
        
        usePackage("stringr")
        age.tmp  <- str_replace(age.tmp, "_", " ")

        age.tmp <- paste(stockabundance, " at ", age.tmp, ": ", as.character(formula(fits[[i]]$model)), sep="")

        nms <- c(nms, fits[[i]]$age)

        title(main=paste0(age.tmp))


     }

     ## names(bias.coeff.afe.individual.ages.retro.simplelogpower) <<- nms

     names(SIMPLELOGPOWER$bias.coeff.afe.individual.ages.retro.simplelogpower) <<- nms

     usePackage("gridGraphics")

     grid.echo()
     grid.grab() -> mapgrob

     return(mapgrob)


}




SIMPLELOGPOWER$fits <- SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower  ## fits

SIMPLELOGPOWER$results <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.simplelogpower.regression.youngest(
                              SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression, 
                              SIMPLELOGPOWER$index.year)  # results


windows()
SIMPLELOGPOWER$bias.coefficients.afe.older.ages.retro.simplelogpower(SIMPLELOGPOWER$fits,
                                                     SIMPLELOGPOWER$results,
                                                     SIMPLELOGPOWER$stockabundance)

## rm(fits)
## rm(results)

SIMPLELOGPOWER$fits <- NULL 
SIMPLELOGPOWER$results <- NULL 

##########################################################################################
#
#  Bias coefficients of retrospective forecast errors - total age
#
##########################################################################################

SIMPLELOGPOWER$bias.coefficient.afe.total.age.retro.simplelogpower <- function(results, stockabundance){

    par(mfrow=c(1,1), mar=c(2,2,2,2))

    data <- cbind.data.frame(a = results$retro$a.total,
                             p = results$retro$p.total,
                             e = results$retro$e.total)


    error <- data$e

    mre.error <- mre(error)

    ## plot.mre(mre.error)

    ## gamma <- Arg(mre.error)

    ## bias <- 1 - 4*gamma/pi

    ## k <- length(bias)

    bias.coeff.updated(mre.error, outplot=2)

    ## bias.coeff.afe.total.age.retro.simplelogpower <<- bias.coeff.updated(mre.error, outplot=0)

    SIMPLELOGPOWER$bias.coeff.afe.total.age.retro.simplelogpower <<- bias.coeff.updated(mre.error, outplot=0)

    ## bias.coeff(mre.error, outplot=0)

    tmp <- paste("Total", stockabundance)

    title(main=paste0(tmp))

    usePackage("gridGraphics")

    grid.echo()
    grid.grab() -> mapgrob

    return(mapgrob)

}


windows()

SIMPLELOGPOWER$results <- SIMPLELOGPOWER$best.rmse.youngest.age

SIMPLELOGPOWER$bias.coefficient.afe.total.age.retro.simplelogpower(SIMPLELOGPOWER$results, SIMPLELOGPOWER$stockabundance)

SIMPLELOGPOWER$bias.coeff.afe.total.age.retro.simplelogpower

SIMPLELOGPOWER$results <- NULL 


##########################################################################################
#
# Time series plot of forecasted vs. actual abundance (older ages, simplelogpower)
#
##########################################################################################


#-----------------------------------------------------------------------------------------
# Time series plot of retrospectively forecasted and actual values of abundance (individual ages, avgfive)
#-----------------------------------------------------------------------------------------

SIMPLELOGPOWER$timeseries.plot.results.afe.individual.ages.retro.simplelogpower <- function(results, stockabundance){

    .e = environment()

    forecasted.stacked <- NULL
    actual.stacked <- NULL
    age.stacked <- NULL
    labels.stacked <- NULL

    for (i in 1:length(results$retro$resjoin)){

       usePackage("stringr")

       data <- results$retro$resjoin[[i]]
       names(data)[names(data)=="a"] <- "Actual"
       names(data)[names(data)=="p"] <- "Forecast"
       names(data)[names(data)=="e"] <- "Error"

       mytitle <- names(results$retro$resjoin)[i]

       ## mytitle <- names(results)[i]

       usePackage("stringr")
       mytitle <- str_replace_all(mytitle, pattern="_", replacement=" ")
       ## mytitle <- str_replace_all(mytitle, pattern="age", replacement="Age ")
       ## mytitle <- substr(mytitle, start=1, stop=5)


       labs <- data$cy

       forecasted.stacked <- c(forecasted.stacked, data$Forecast)
       actual.stacked <- c(actual.stacked, data$Actual)
       age.stacked <- c(age.stacked, rep(mytitle, length(data$Actual)))
       labels.stacked <- c(labels.stacked, labs)

    }

    data.stacked <- data.frame(forecasted=forecasted.stacked,
                               actual=actual.stacked,
                               age=age.stacked,
                               labels=labels.stacked)



    usePackage("ggplot2")
    usePackage("scales")

    g <- ggplot(data.stacked, aes(labels, actual), environment=.e)  +    # environment=.e
       geom_line(aes(labels, actual, colour="Actual"),size=0.8) +
       geom_line(aes(labels, forecasted, colour="Forecasted"), size=0.8) +
       geom_point(aes(labels, actual, colour="Actual"),size=2.5) +
       geom_point(aes(labels, forecasted, colour="Forecasted"), size=2.5) +
            coord_fixed(ratio=1) +
              facet_wrap(~age, scales="free",ncol=2) +   # free_y
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma) +
                   scale_y_continuous(paste("Actual and Retrospectively", "Forecasted", stockabundance, "Values"),labels=comma) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous("Return Year", breaks=seq(min(data.stacked$labels),max(data.stacked$labels),by=1)) +
                    ## ggtitle(paste(stockname, "Stock")) +
                   coord_fixed(ratio=1) +
                   scale_color_manual(name=paste0(stockabundance), values=c("Actual"="blue2", "Forecasted"="red2")) +
                   theme_bw() +
                   theme(plot.title=element_text(size=12, hjust=0.5), 
                         axis.title.x=element_text(size=10,vjust=-0.5),
                         axis.title.y=element_text(size=10,vjust=1.5),
                         axis.text.y=element_text(size=8),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),
                         legend.position="top")

      return(g)

}


SIMPLELOGPOWER$results <- SIMPLELOGPOWER$best.rmse.youngest.age
SIMPLELOGPOWER$timeseries.plot.results.afe.individual.ages.retro.simplelogpower(SIMPLELOGPOWER$results,
                                                             SIMPLELOGPOWER$stockabundance)

## rm(results)

SIMPLELOGPOWER$results <- NULL 

##########################################################################################
#
# Time series plot of forecasted vs. actual abundance (total age, simplelogpower)
#
##########################################################################################



SIMPLELOGPOWER$timeseries.plot.results.afe.total.age.retro.simplelogpower <- function(results, stockabundance){

    .e = environment()

    data <- cbind.data.frame(a = results$retro$a.total,
                             p = results$retro$p.total,
                             e = results$retro$e.total)

    names(data)[names(data)=="a"] <- "Actual"
    names(data)[names(data)=="p"] <- "Forecast"
    names(data)[names(data)=="e"] <- "Error"



    CY <- results$retro$resjoin[[1]]$cy

    ## data$data.retro[[1]]$cy

    ## usePackage("calibrate")
    ## labs <- substr(CY,
    ##           start=3, stop=4)

    ## usePackage("calibrate")
    labs <- CY

    ## r.sq <- summary(lm(data$Forecast ~ data$Actual))$r.squared
    ## r.sq <-  sprintf("%.2f", r.sq*100)

    data.stacked <- data.frame(forecasted=data$Forecast,
                               actual=data$Actual,
                               labels=labs,
                               ## age=paste("Total ", stockabundance, ":  ", "R-squared = ",r.sq,"%",sep="")
                               age=paste("Total ", stockabundance, sep="")
                               )
    usePackage("scales")
    usePackage("ggplot2")

    g <- ggplot(data.stacked, aes(labels,actual), environment=.e) +
          geom_line(aes(labels, actual, colour="Actual"),size=0.8) +
           geom_line(aes(labels, forecasted, colour="Forecasted"), size=0.8) +
            geom_point(aes(labels, actual, colour="Actual"),size=2.5) +
             geom_point(aes(labels, forecasted, colour="Forecasted"), size=2.5) +
              facet_wrap(~age, scales="free",ncol=1) +
                 coord_fixed(ratio=1)  +
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                  scale_y_continuous(paste("Retrospectively Forecasted", stockabundance, "Values"),labels=comma,
                                     limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                   scale_x_continuous("Return Year", breaks=seq(min(data.stacked$labels),max(data.stacked$labels),by=1)) +
                  ## ggtitle(paste(stockname, "Stock")) +
                   coord_fixed(ratio=1)   +
                 scale_color_manual(name=paste0(stockabundance), values=c("Actual"="blue2", "Forecasted"="red2")) +
                 theme_bw() +
                  theme( plot.title=element_text(size=12, hjust=0.5), 
                         axis.title.x=element_text(size=10,vjust=-0.5),
                         axis.title.y=element_text(size=10,vjust=1.5),
                         axis.text.y=element_text(size=8),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),   # hjust=1
                         legend.position="top")

      return(g)


}


SIMPLELOGPOWER$results <- SIMPLELOGPOWER$best.rmse.youngest.age

SIMPLELOGPOWER$timeseries.plot.results.afe.total.age.retro.simplelogpower(SIMPLELOGPOWER$results, SIMPLELOGPOWER$stockabundance)

## rm(results)

SIMPLELOGPOWER$results <- NULL 
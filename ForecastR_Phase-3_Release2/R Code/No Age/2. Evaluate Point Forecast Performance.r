
## need to refresh the datalist object, as it currently contains only the youngest age!!!
## datalist

##========================================================================================================
## stock without age information: naiveone
##========================================================================================================

individual.stock.retro.predictive.performance.naiveone.no.age <- function(datalist, forecastingyear, index){

      ## index <- 10

      index <- index 

      names(datalist)[names(datalist)=="Run_Year"] <- "CY"
      PSY <- forecastingyear

      subdata <- subset(datalist, CY < PSY)

      y <- subdata[,ncol(subdata)]
      cy <- subdata[,"CY"]

      usePackage("stringr")

      a <- NULL
      p <- NULL
      e <- NULL

      cy00 <- NULL

      p.bench <- NULL
      e.bench <- NULL

      no.age.retro.plot.info.naiveone <<- list()
      
      data0 <- NULL

      for (i in index:(length(y)-1)){

           usePackage("forecast")

           y0 <- y[1:i]
           cy0 <- cy[1:i]
           
           ## model0 <- rwf(y0, h=1, drift=FALSE, level=0.80)
           
           model0 <- rwf(y0, h=1, drift=FALSE)
           
           p0 <- as.numeric(model0$mean)

           f0 <- round(as.numeric(fitted(model0)))
                                         
           d0 <- data.frame(i=i, cy0, y0, f0, p0, psy=max(cy0)+1, a0=y[i+1])
               
           ## str(d0)
    
           data0 <- rbind.data.frame(data0, d0)  

           p <- c(p, p0)
           e0 <- y[i+1] - p0   # actual - predicted
           e <- c(e, e0)
           a <- c(a, y[i+1]) #actual

           cy00 <- c(cy00, cy[i+1])

           ## benchmark: naive forecasting (previous year)

           y0 <- y[1:i]

           usePackage("forecast")

           ## model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

           model0.bench <- rwf(y0,h=1,drift=FALSE)

           p0.bench <- as.numeric(model0.bench$mean)

           p.bench <- c(p.bench, p0.bench)
           e0.bench <- y[i+1] - p0.bench   # actual - predicted
           e.bench <- c(e.bench, e0.bench)

        }


        no.age.retro.plot.info.naiveone <<- data0

        result.naiveone.no.age <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)
       
        result.naiveone.no.age.output <- list() 
        result.naiveone.no.age.output[[1]]  <- result.naiveone.no.age
          
        result.naiveone.no.age.output


}

## individual.stock.retro.predictive.performance.naiveone.no.age(datalist, forecastingyear, index)


individual.stock.retro.plot.no.age.naiveone <- function(no.age.retro.plot.info.naiveone, stockabundance){

   .e <- environment()

   mydata <- no.age.retro.plot.info.naiveone

   tmp <- "Retrospective Modeling and Forecasting \n for the Naive Model (Previous Year)"

   ggplot(mydata, aes(cy0, y0), environment=.e) + 
    geom_line(data=mydata, aes(cy0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) + 
      geom_line(data=mydata, aes(cy0, f0), colour="red") + 
       geom_point(data=mydata, aes(psy, p0), colour="red") + 
        ylab(paste0(stockabundance)) + 
         xlab(paste0("Return Year")) + 
          ggtitle(paste0(tmp)) + 
           facet_wrap(~psy) + 
            theme_bw() + 
             scale_y_continuous(labels=scales::comma) + 
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}

## individual.stock.retro.plot.no.age.naiveone(no.age.retro.plot.info.naiveone, stockabundance)


##========================================================================================================
## stock without age information: avgthree
##========================================================================================================

individual.stock.retro.predictive.performance.avgthree.no.age <- function(datalist, forecastingyear, index){

      ## index <- 10

      index <- index

      names(datalist)[names(datalist)=="Run_Year"] <- "CY"
      PSY <- forecastingyear

      subdata <- subset(datalist, CY < PSY)

      y <- subdata[,ncol(subdata)]
      cy <- subdata[,"CY"]

      usePackage("stringr")

      a <- NULL
      p <- NULL
      e <- NULL

      cy00 <- NULL

      p.bench <- NULL
      e.bench <- NULL

      data0 <- NULL

      for (i in index:(length(y)-1)){

           y0 <- y[1:i]
           cy0 <- cy[1:i]
           
           model0 <- avgthree.no.age(y0)
           
           p0 <- as.numeric(model0$mean)
           f0 <- round(as.numeric(model0$fitted))

           d0 <- data.frame(i=i, cy0, y0, f0, p0, psy=max(cy0)+1, a0=y[i+1])
               
           ## str(d0)
               
           data0 <- rbind.data.frame(data0, d0)  

           p <- c(p, p0)
           e0 <- y[i+1] - p0   # actual - predicted
           e <- c(e, e0)
           a <- c(a, y[i+1]) #actual

           cy00 <- c(cy00, cy[i+1])

           ## benchmark: naive forecasting (previous year)

           y0 <- y[1:i]

           usePackage("forecast")

           ## model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

           model0.bench <- rwf(y0,h=1,drift=FALSE)

           p0.bench <- as.numeric(model0.bench$mean)

           p.bench <- c(p.bench, p0.bench)
           e0.bench <- y[i+1] - p0.bench   # actual - predicted
           e.bench <- c(e.bench, e0.bench)

        }

        no.age.retro.plot.info.avgthree <<- data0 

        result.avgthree.no.age <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)
       
        result.avgthree.no.age.output <- list() 
        result.avgthree.no.age.output[[1]]  <- result.avgthree.no.age
          
        result.avgthree.no.age.output


}

## individual.stock.retro.predictive.performance.avgthree.no.age(datalist, forecastingyear, index)

individual.stock.retro.plot.no.age.avgthree <- function(no.age.retro.plot.info.avgthree, stockabundance){

  .e <- environment()

   mydata <- no.age.retro.plot.info.avgthree

   tmp <- "Retrospective Modeling and Forecasting \n for the Naive Model (Average of Previous 3 Years)" 

   ggplot(mydata, aes(cy0, y0), environment=.e) + 
    geom_line(data=mydata, aes(cy0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) + 
      geom_line(data=mydata, aes(cy0, f0), colour="red") + 
       geom_point(data=mydata, aes(psy, p0), colour="red") + 
        ylab(paste0(stockabundance)) + 
         xlab(paste0("Return Year")) + 
          ggtitle(paste0(tmp)) + 
           facet_wrap(~psy) + 
            theme_bw() + 
             scale_y_continuous(labels=scales::comma) + 
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}

## individual.stock.retro.plot.no.age.avgthree(no.age.retro.plot.info.avgthree, stockabundance)

##========================================================================================================
## stock without age information: avgfive
##========================================================================================================

individual.stock.retro.predictive.performance.avgfive.no.age <- function(datalist, forecastingyear, index){

      ## index <- 10

      index <- index 

      names(datalist)[names(datalist)=="Run_Year"] <- "CY"
      PSY <- forecastingyear


      subdata <- subset(datalist, CY < PSY)

      y <- subdata[,ncol(subdata)]
      cy <- subdata[,"CY"]

      usePackage("stringr")

      a <- NULL
      p <- NULL
      e <- NULL

      cy00 <- NULL

      p.bench <- NULL
      e.bench <- NULL

      data0 <- NULL

      for (i in index:(length(y)-1)){

           y0 <- y[1:i]
           cy0 <- cy[1:i]
           
           model0 <- avgfive.no.age(y0)
           
           p0 <- as.numeric(model0$mean)

           f0 <- round(as.numeric(model0$fitted))
                                         
           d0 <- data.frame(i=i, cy0, y0, f0, p0, psy=max(cy0)+1, a0=y[i+1])
               
           ## str(d0)
               
           data0 <- rbind.data.frame(data0, d0)  

           p <- c(p, p0)
           e0 <- y[i+1] - p0   # actual - predicted
           e <- c(e, e0)
           a <- c(a, y[i+1]) #actual

           cy00 <- c(cy00, cy[i+1])

           ## benchmark: naive forecasting (previous year)

           y0 <- y[1:i]

           usePackage("forecast")

           ## model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

            model0.bench <- rwf(y0,h=1,drift=FALSE)

           p0.bench <- as.numeric(model0.bench$mean)

           p.bench <- c(p.bench, p0.bench)
           e0.bench <- y[i+1] - p0.bench   # actual - predicted
           e.bench <- c(e.bench, e0.bench)

        }

        no.age.retro.plot.info.avgfive <<- data0

        result.avgfive.no.age <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)
       
        result.avgfive.no.age.output <- list() 
        result.avgfive.no.age.output[[1]]  <- result.avgfive.no.age
          
        result.avgfive.no.age.output


}

individual.stock.retro.predictive.performance.avgfive.no.age(datalist, forecastingyear, index)



individual.stock.retro.plot.no.age.avgfive <- function(no.age.retro.plot.info.avgfive, stockabundance){

   .e <- environment()

   mydata <- no.age.retro.plot.info.avgfive

   tmp <- "Retrospective Modeling and Forecasting \n for the Naive Model (Average of Previous 5 Years)"

   ggplot(mydata, aes(cy0, y0), environment=.e) + 
    geom_line(data=mydata, aes(cy0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) + 
      geom_line(data=mydata, aes(cy0, f0), colour="red") + 
       geom_point(data=mydata, aes(psy, p0), colour="red") + 
        ylab(paste0(stockabundance)) + 
         xlab(paste0("Return Year")) + 
          ggtitle(paste0(tmp)) + 
           facet_wrap(~psy) + 
            theme_bw() + 
             scale_y_continuous(labels=scales::comma) + 
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}


## individual.stock.retro.plot.no.age.avgfive(no.age.retro.plot.info.avgfive, stockabundance)

##=========================================================
## stock without age information: arima
##========================================================================================================


individual.stock.retro.predictive.performance.arima.no.age <- function(datalist, forecastingyear, arima.model.fit.no.age, boxcoxtransform, index){


    fit <- arima.model.fit.no.age


    ## index <- 10

    index <- index

    names(datalist)[names(datalist)=="Run_Year"] <- "CY"
    ## CY <-  fit$model.data$Run_Year
    PSY <- forecastingyear    ## Come back here to make sure the forecasting year is specified correctly!

    
    subdata <- subset(datalist, CY < PSY)

    y <- subdata[,ncol(subdata)]

    cy <- subdata[,"CY"]

    arimafit <- fit$model

    sink("arimafit.txt")
    print(arimafit)
    sink()

    out <- readLines("arimafit.txt")
    
    usePackage("stringr")

    out.pattern <- str_detect(string=out, pattern="ARIMA")

    modelarima <- out[out.pattern==TRUE]
    usePackage("stringr")
    modelarima <- str_trim(modelarima)

    modelorders <- as.numeric(unlist(strsplit(modelarima, "[^[:digit:]]")))

    modelorders <- modelorders[!is.na(modelorders)]

    p.0 <- modelorders[1]
    d.0 <- modelorders[2]
    q.0 <- modelorders[3]

    fn <- "arimafit.txt"
    if (file.exists(fn)) file.remove(fn)

    a <- NULL
    p <- NULL
    e <- NULL

    cy00 <- NULL

    p.bench <- NULL
    e.bench <- NULL

    data0 <- NULL

    ## no.age.retro.plot.info <<- list()

    for (i in index:(length(y)-1)){

        y0 <- y[1:i]
        cy0 <- cy[1:i]

        usePackage("forecast")

        if (boxcoxtransform == TRUE) {
                 
                   ## y0[y0==0] <- 0.001  # add a small constant to zero counts
                  
                   model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(y0, method="guerrero"))
               
         } else {
               
                  model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE) 
               
         } 
        
        
         ## p0 <- round(as.numeric(forecast(model0, h=1, level=0.80, biasadj=FALSE)$mean))
         
         p0 <- round(as.numeric(forecast(model0, h=1, biasadj=FALSE)$mean))
               
         f0 <- round(as.numeric(fitted(model0, biasadj=FALSE)))
                                         
         d0 <- data.frame(i=i, cy0, y0, f0, p0, psy=max(cy0)+1, a0=y[i+1])
               
         ## str(d0)
               
         data0 <- rbind.data.frame(data0, d0)  
                                           
         ##---

         ## p0 <- as.numeric(forecast(model0, h=1, level=0.95)$mean)
         ## p0 <- round(p0)

         p <- c(p, p0)
         e0 <- y[i+1] - p0   # actual - predicted
         e <- c(e, e0)
         a <- c(a, y[i+1]) #actual

         cy00 <- c(cy00, cy[i+1])

         ## benchmark: naive forecasting (previous year)

         y0 <- y[1:i]

         usePackage("forecast")

         ## model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

         model0.bench <- rwf(y0,h=1,drift=FALSE)


         p0.bench <- as.numeric(model0.bench$mean)
         p0.bench <- round(p0.bench)
               
         p.bench <- c(p.bench, p0.bench)
         e0.bench <- y[i+1] - p0.bench   # actual - predicted
         e.bench <- c(e.bench, e0.bench)
         
    }

    no.age.retro.plot.info.arima <<- data0


    result.arima.no.age <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)

    result.arima.no.age  ## results of retrospective point forecast performance for 
                           ## stock without age information
    
    result.arima.no.age.output <- list() 
    result.arima.no.age.output[[1]]  <- result.arima.no.age
        
        
    result.arima.no.age.output

                           
}

## individual.stock.retro.predictive.performance.arima.no.age(datalist, forecastingyear, arima.model.fit.no.age, boxcoxtransform, index)


individual.stock.retro.plot.no.age.arima <- function(no.age.retro.plot.info.arima, stockabundance){

   .e <- environment()

   mydata <- no.age.retro.plot.info.arima

   tmp <- "Retrospective ARIMA Modeling and Forecasting"

   ggplot(mydata, aes(cy0, y0), environment=.e) + 
   ## ggplot(mydata, aes(cy0, y0)) + 
    geom_line(data=mydata, aes(cy0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) + 
      geom_line(data=mydata, aes(cy0, f0), colour="red") + 
       geom_point(data=mydata, aes(psy, p0), colour="red") + 
        ylab(paste0(stockabundance)) + 
         xlab(paste0("Return Year")) + 
          ggtitle(paste0(tmp)) + 
           facet_wrap(~psy) + 
            theme_bw() + 
             scale_y_continuous(labels=scales::comma) + 
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}


## individual.stock.retro.plot.no.age.arima(no.age.retro.plot.info.arima, stockabundance)


##========================================================================================================
## no age: expsmooth
##========================================================================================================

individual.stock.retro.predictive.performance.expsmooth.no.age <- function(datalist, forecastingyear, expsmooth.model.fit.no.age, boxcoxtransform, index){


    fit <- expsmooth.model.fit.no.age

    ## index <- 10

    index <- index

    names(datalist)[names(datalist)=="Run_Year"] <- "CY"
    ## CY <- fit$model.data$Run_Year 
    
    PSY <- forecastingyear    ## Come back here to make sure the forecasting year is specified correctly!

    subdata <- subset(datalist, CY < PSY)

    y <- subdata[,ncol(subdata)]

    cy <- subdata[,"CY"]

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


    a <- NULL
    p <- NULL
    e <- NULL

    cy00 <- NULL

    p.bench <- NULL
    e.bench <- NULL

    data0 <- NULL
     
    for (i in index:(length(y)-1)){

        y0 <- y[1:i]
        cy0 <- cy[1:i]

        usePackage("forecast")
       
        if (boxcoxtransform == TRUE) {
                  
                   y0[y0==0] <- 0.001  # add a small constant to zero counts
                  
                   ## model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(y0, method="guerrero"))
                   
                   model0 <- ets(y0, lambda=BoxCox.lambda(y0), damped=NULL)
               
        } else {
               
                  ## model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE) 
               
                  model0 <- ets(y0, damped=NULL) 
               
        } 
       
        ## model0 <- ets(y0,model=expsmoothfit)

        p0 <- as.numeric(forecast(model0, h=1, biasadj=FALSE)$mean)
              
        f0 <- round(as.numeric(fitted(model0, biasadj=FALSE)))
                                         
        d0 <- data.frame(i=i, cy0, y0, f0, p0, psy=max(cy0)+1, a0=y[i+1])
              
        ## str(d0)
               
        data0 <- rbind.data.frame(data0, d0)  

        ## p0 <- as.numeric(forecast(model0, h=1, level=80)$mean)
        ## p0 <- round(p0)

        p <- c(p, p0)
        e0 <- y[i+1] - p0   # actual - predicted
        e <- c(e, e0)
        a <- c(a, y[i+1]) #actual

        cy00 <- c(cy00, cy[i+1])

        ## benchmark: naive forecasting (previous year)

        y0 <- y[1:i]

        usePackage("forecast")

        ## model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)
        
        model0.bench <- rwf(y0,h=1,drift=FALSE)

        p0.bench <- as.numeric(model0.bench$mean)
        p0.bench <- round(p0.bench)

        p.bench <- c(p.bench, p0.bench)
        e0.bench <- y[i+1] - p0.bench   # actual - predicted
        e.bench <- c(e.bench, e0.bench)

    }

    no.age.retro.plot.info.expsmooth <<- data0

    ### result[[j]] <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)

    result.expsmooth.no.age <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)

    result.expsmooth.no.age  ## results of retrospective point forecast performance for
                           ## stock without age information

    result.expsmooth.no.age.output <- list()
    result.expsmooth.no.age.output[[1]]  <- result.expsmooth.no.age

    result.expsmooth.no.age.output

}

## individual.stock.retro.predictive.performance.expsmooth.no.age(datalist, forecastingyear, expsmooth.model.fit.no.age, boxcoxtransform, index)

individual.stock.retro.plot.no.age.expsmooth <- function(no.age.retro.plot.info.expsmooth, stockabundance){

   .e <- environment()

   mydata <- no.age.retro.plot.info.expsmooth

   tmp <- "Retrospective Exponential Smoothing Modeling and Forecasting" 

   ggplot(mydata, aes(cy0, y0), environment=.e) + 
   ## ggplot(mydata, aes(cy0, y0)) + 
    geom_line(data=mydata, aes(cy0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) + 
      geom_line(data=mydata, aes(cy0, f0), colour="red") + 
       geom_point(data=mydata, aes(psy, p0), colour="red") + 
        ylab(paste0(stockabundance)) + 
         xlab(paste0("Return Year")) + 
          ggtitle(paste0(tmp)) + 
           facet_wrap(~psy) + 
            theme_bw() + 
             scale_y_continuous(labels=scales::comma) + 
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}

## individual.stock.retro.plot.no.age.expsmooth(no.age.retro.plot.info.expsmooth, stockabundance)


##=========================================================================================================
## measures of retro performance - naiveone
##=========================================================================================================

no.age.retro.predictive.performance.naiveone <- function(results.retro.naiveone.no.age){

    resjoin <- results.retro.naiveone.no.age[[1]] 
	    
    a <- resjoin$a
    p <- resjoin$p
    e <- resjoin$e
    p.bench <- resjoin$p.bench
    e.bench <- resjoin$e.bench

    mre <- mean(e)

    mae <- mean(abs(e))

    mpe <- mean(e/a)

    mape <- mean(abs(e)/abs(a))
    
    ## mape <- mean(abs(e/a))*100

    num_mase <- mean(abs(e))
    denom_mase <- mean(abs(e.bench))
    
    mase <- num_mase/denom_mase

    rmse <- sqrt(sum(e^2)/length(e))

    out <- list(method="naiveone",
                resjoin=resjoin, 
                 a=a,
                 p=p,
                 e=e,
                 p.bench=p.bench,
                 e.bench=e.bench,
                 mre=mre,
                 mae=mae,
                 mpe=mpe,
                 mape=mape,
                 mase=mase,
                 rmse=rmse)
    return(out)
}


if (noagemodelnaiveone) {  # naiveone 
   
   results.retro.naiveone.no.age <- individual.stock.retro.predictive.performance.naiveone.no.age(datalist, forecastingyear, index)
   rmse.results.no.age.naiveone <- no.age.retro.predictive.performance.naiveone(results.retro.naiveone.no.age)
   
   
   all.retro.measures <- c("MRE","MAE","MPE","MAPE","MASE","RMSE")
   all.retro.measures <- tolower(all.retro.measures)
   flag.all.retro.measures <- c(retromeasureMRE,retromeasureMAE,retromeasureMPE,retromeasureMAPE,retromeasureMASE, retromeasureRMSE)
   user.selected.retro.measures <- all.retro.measures[flag.all.retro.measures==TRUE]

   rmse.results.no.age.naiveone.tmp1 <- rmse.results.no.age.naiveone[names(rmse.results.no.age.naiveone) %in% c("method", "resjoin", "a", "p", "e", "p.bench", "e.bench")]
   rmse.results.no.age.naiveone.tmp2 <- rmse.results.no.age.naiveone[names(rmse.results.no.age.naiveone) %in% user.selected.retro.measures]
   rmse.results.no.age.naiveone <- c(rmse.results.no.age.naiveone.tmp1, rmse.results.no.age.naiveone.tmp2)


}



##=========================================================================================================
## measures of retro performance - avgthree
##=========================================================================================================

no.age.retro.predictive.performance.avgthree <- function(results.retro.avgthree.no.age){

    resjoin <- results.retro.avgthree.no.age[[1]] 
	    
    a <- resjoin$a
    p <- resjoin$p
    e <- resjoin$e
    p.bench <- resjoin$p.bench
    e.bench <- resjoin$e.bench

    mre <- mean(e)

    mae <- mean(abs(e))

    mpe <- mean(e/a)

    mape <- mean(abs(e)/abs(a))

    num_mase <- mean(abs(e))
    denom_mase <- mean(abs(e.bench))
    
    mase <- num_mase/denom_mase

    rmse <- sqrt(sum(e^2)/length(e))

    out <- list(method="avgthree",
                resjoin=resjoin, 
                 a=a,
                 p=p,
                 e=e,
                 p.bench=p.bench,
                 e.bench=e.bench,
                 mre=mre,
                 mae=mae,
                 mpe=mpe,
                 mape=mape,
                 mase=mase,
                 rmse=rmse)
    return(out)
}


if (noagemodelavgthree) {  # avgthree 

results.retro.avgthree.no.age <- individual.stock.retro.predictive.performance.avgthree.no.age(datalist, forecastingyear, index)
rmse.results.no.age.avgthree <- no.age.retro.predictive.performance.avgthree(results.retro.avgthree.no.age)

all.retro.measures <- c("MRE","MAE","MPE","MAPE","MASE","RMSE")
all.retro.measures <- tolower(all.retro.measures)
flag.all.retro.measures <- c(retromeasureMRE,retromeasureMAE,retromeasureMPE,retromeasureMAPE,retromeasureMASE, retromeasureRMSE)
user.selected.retro.measures <- all.retro.measures[flag.all.retro.measures==TRUE]

rmse.results.no.age.avgthree.tmp1 <- rmse.results.no.age.avgthree[names(rmse.results.no.age.avgthree) %in% c("method", "resjoin", "a", "p", "e", "p.bench", "e.bench")]
rmse.results.no.age.avgthree.tmp2 <- rmse.results.no.age.avgthree[names(rmse.results.no.age.avgthree) %in% user.selected.retro.measures]
rmse.results.no.age.avgthree <- c(rmse.results.no.age.avgthree.tmp1, rmse.results.no.age.avgthree.tmp2)

} 

##=========================================================================================================
## measures of retro performance - avgfive
##=========================================================================================================

no.age.retro.predictive.performance.avgfive <- function(results.retro.avgfive.no.age){

    resjoin <- results.retro.avgfive.no.age[[1]] 
	    
    a <- resjoin$a
    p <- resjoin$p
    e <- resjoin$e
    p.bench <- resjoin$p.bench
    e.bench <- resjoin$e.bench

    mre <- mean(e)

    mae <- mean(abs(e))

    mpe <- mean(e/a)

    mape <- mean(abs(e)/abs(a))

    num_mase <- mean(abs(e))
    denom_mase <- mean(abs(e.bench))
    
    mase <- num_mase/denom_mase

    rmse <- sqrt(sum(e^2)/length(e))

    out <- list(method="avgfive",
                resjoin=resjoin, 
                 a=a,
                 p=p,
                 e=e,
                 p.bench=p.bench,
                 e.bench=e.bench,
                 mre=mre,
                 mae=mae,
                 mpe=mpe,
                 mape=mape,
                 mase=mase,
                 rmse=rmse)
    return(out)
}


if (noagemodelavgfive) {  # avgfive 

    results.retro.avgfive.no.age <- individual.stock.retro.predictive.performance.avgfive.no.age(datalist, forecastingyear, index)
    rmse.results.no.age.avgfive <- no.age.retro.predictive.performance.avgfive(results.retro.avgfive.no.age)
    
    all.retro.measures <- c("MRE","MAE","MPE","MAPE","MASE","RMSE")
    all.retro.measures <- tolower(all.retro.measures)
    flag.all.retro.measures <- c(retromeasureMRE,retromeasureMAE,retromeasureMPE,retromeasureMAPE,retromeasureMASE, retromeasureRMSE)
    user.selected.retro.measures <- all.retro.measures[flag.all.retro.measures==TRUE]

    rmse.results.no.age.avgfive.tmp1 <- rmse.results.no.age.avgfive[names(rmse.results.no.age.avgfive) %in% c("method", "resjoin", "a", "p", "e", "p.bench", "e.bench")]
    rmse.results.no.age.avgfive.tmp2 <- rmse.results.no.age.avgfive[names(rmse.results.no.age.avgfive) %in% user.selected.retro.measures]
    rmse.results.no.age.avgfive <- c(rmse.results.no.age.avgfive.tmp1, rmse.results.no.age.avgfive.tmp2)

}


##=========================================================================================================
## measures of retro performance - arima 
##=========================================================================================================


no.age.retro.predictive.performance.arima <- function(results.retro.arima.no.age){

    resjoin <- results.retro.arima.no.age[[1]]

    a <- resjoin$a
    p <- resjoin$p
    e <- resjoin$e
    p.bench <- resjoin$p.bench
    e.bench <- resjoin$e.bench

    mre <- mean(e)

    mae <- mean(abs(e))

    mpe <- mean(e/a)

    mape <- mean(abs(e)/abs(a))

    num_mase <- mean(abs(e))
    denom_mase <- mean(abs(e.bench))
    
    mase <- num_mase/denom_mase

    rmse <- sqrt(sum(e^2)/length(e))

    out <- list(method="arima",
                resjoin=resjoin, 
                 a=a,
                 p=p,
                 e=e,
                 p.bench=p.bench,
                 e.bench=e.bench,
                 mre=mre,
                 mae=mae,
                 mpe=mpe,
                 mape=mape,
                 mase=mase,
                 rmse=rmse)
    return(out)
	   
}



if (noagemodelarima) {  # arima 

    results.retro.arima.no.age <- individual.stock.retro.predictive.performance.arima.no.age(datalist, 
                                                                                         forecastingyear, 
                                                                                         arima.model.fit.no.age, 
                                                                                         boxcoxtransform, 
                                                                                         index)
                                                                                         
    rmse.results.no.age.arima <- no.age.retro.predictive.performance.arima(results.retro.arima.no.age)
    
    
    all.retro.measures <- c("MRE","MAE","MPE","MAPE","MASE","RMSE")
    all.retro.measures <- tolower(all.retro.measures)
    flag.all.retro.measures <- c(retromeasureMRE,retromeasureMAE,retromeasureMPE,retromeasureMAPE,retromeasureMASE, retromeasureRMSE)
    user.selected.retro.measures <- all.retro.measures[flag.all.retro.measures==TRUE]

    rmse.results.no.age.arima.tmp1 <- rmse.results.no.age.arima[names(rmse.results.no.age.arima) %in% c("method", "resjoin", "a", "p", "e", "p.bench", "e.bench")]
    rmse.results.no.age.arima.tmp2 <- rmse.results.no.age.arima[names(rmse.results.no.age.arima) %in% user.selected.retro.measures]
    rmse.results.no.age.arima <- c(rmse.results.no.age.arima.tmp1, rmse.results.no.age.arima.tmp2)

}

##=========================================================================================================
## measures of retro performance - expsmooth 
##=========================================================================================================

no.age.retro.predictive.performance.expsmooth <- function(results.retro.expsmooth.no.age){

    resjoin <- results.retro.expsmooth.no.age[[1]]

    a <- resjoin$a
    p <- resjoin$p
    e <- resjoin$e
    p.bench <- resjoin$p.bench
    e.bench <- resjoin$e.bench

    mre <- mean(e)

    mae <- mean(abs(e))

    mpe <- mean(e/a)

    mape <- mean(abs(e)/abs(a))

    num_mase <- mean(abs(e))
    denom_mase <- mean(abs(e.bench))
    
    mase <- num_mase/denom_mase

    rmse <- sqrt(sum(e^2)/length(e))

    out <- list(method="expsmooth",
                resjoin=resjoin, 
                 a=a,
                 p=p,
                 e=e,
                 p.bench=p.bench,
                 e.bench=e.bench,
                 mre=mre,
                 mae=mae,
                 mpe=mpe,
                 mape=mape,
                 mase=mase,
                 rmse=rmse)
    return(out)
 
}

if (noagemodelexpsmooth) {  # expsmooth 

    results.retro.expsmooth.no.age <- individual.stock.retro.predictive.performance.expsmooth.no.age(datalist, 
                                                forecastingyear, 
                                                expsmooth.model.fit.no.age, 
                                                boxcoxtransform, index)
                                                
    rmse.results.no.age.expsmooth <- no.age.retro.predictive.performance.expsmooth(results.retro.expsmooth.no.age)
    
    all.retro.measures <- c("MRE","MAE","MPE","MAPE","MASE","RMSE")
    all.retro.measures <- tolower(all.retro.measures)
    flag.all.retro.measures <- c(retromeasureMRE,retromeasureMAE,retromeasureMPE,retromeasureMAPE,retromeasureMASE, retromeasureRMSE)
    user.selected.retro.measures <- all.retro.measures[flag.all.retro.measures==TRUE]

    rmse.results.no.age.expsmooth.tmp1 <- rmse.results.no.age.expsmooth[names(rmse.results.no.age.expsmooth) %in% c("method", "resjoin", "a", "p", "e", "p.bench", "e.bench")]
    rmse.results.no.age.expsmooth.tmp2 <- rmse.results.no.age.expsmooth[names(rmse.results.no.age.expsmooth) %in% user.selected.retro.measures]
    rmse.results.no.age.expsmooth <- c(rmse.results.no.age.expsmooth.tmp1, rmse.results.no.age.expsmooth.tmp2)

}


##=========================================================================================================
## choosing the measure of retro performance (i.e., rmse) which is "best" for forecasting stock abundance
## among avgone, avgthree, avgfive, arima and expsmooth
##=========================================================================================================


#-------------------------------------------------------------------------------

all.args <- c("rmse.results.no.age.naiveone",
                                  "rmse.results.no.age.avgthree",
                                  "rmse.results.no.age.avgfive",
                                  "rmse.results.no.age.arima",
                                  "rmse.results.no.age.expsmooth")

rmse.results.no.age.args <- mget( ls( pattern = "^rmse.results.no.age.", env = .GlobalEnv ) , env = .GlobalEnv )

select.args <- names(rmse.results.no.age.args)

order.all.args <- all.args[all.args %in% select.args]

rmse.results.no.age.args <- rmse.results.no.age.args[order.all.args]

rmse.results.no.age.args$rmse.results.no.age.args <- NULL 

## rmse.results.no.age.args

## names(rmse.results.no.age.args)

## str(rmse.results.no.age.args)

#-------------------------------------------------------------------------------


retromeasure.args <- mget( ls( pattern = "^retromeasure", env = .GlobalEnv) , env = .GlobalEnv )

retromeasure.args <- Filter(isTRUE, retromeasure.args)

all.measures <- c("retromeasureMRE",
                  "retromeasureMAE",
                  "retromeasureMPE",
                  "retromeasureMAPE",
                  "retromeasureMASE",
                  "retromeasureRMSE")


retromeasure.args <-  retromeasure.args[all.measures]

## Source: https://stat.ethz.ch/pipermail/r-help/2006-August/111896.html
delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]
}

retromeasure.args <- delete.NULLs(retromeasure.args)

## names(retromeasure.args)

#-------------------------------------------------------------------------------


rmse.results.no.age <- function(rmse.results.no.age.args, retromeasure.args){
 
 
   ## names(retromeasure.args) 
 
   usePackage("stringr")
   
   measures <- str_replace_all(string=names(retromeasure.args), pattern="retromeasure", replacement="")
   measures <- tolower(measures)
 
   
   ## names(rmse.results.no.age.args) 
   models <- str_replace_all(string=names(rmse.results.no.age.args), pattern="rmse.results.no.age.", replacement="")
 
   
   retro.measures.no.age <- matrix(NA, nrow=length(models), ncol=length(measures)) 
   for (k in 1:length(measures)) {
        retro.measures.no.age[,k] <- unlist(sapply(rmse.results.no.age.args,'[[',measures[k]))
   }

   retro.measures.no.age <- round(retro.measures.no.age, 2)
   
   ## colnames(retro.measures.no.age) <- c("MRE", "MAE", "MPE", "MAPE", "MASE", "RMSE")
   colnames(retro.measures.no.age) <- toupper(measures) 
   
   ## rownames(retro.measures.no.age) <- c("naiveone","avgthree","avgfive","arima","expsmooth")
   rownames(retro.measures.no.age) <- models
   
   retro.measures.no.age
   
   
 }


table.rmse.results.no.age <- rmse.results.no.age(rmse.results.no.age.args, retromeasure.args)

 
##
## rank table of rmse (and other performance metrics) results
##
 
pred.args <- mget( ls( pattern = "^pred.int.individual.stock.", env = .GlobalEnv) , env = .GlobalEnv )

all.pred.args <- c("pred.int.individual.stock.naiveone.no.age",
                     "pred.int.individual.stock.avgthree.no.age",
                       "pred.int.individual.stock.avgfive.no.age",
                         "pred.int.individual.stock.arima.no.age",
                           "pred.int.individual.stock.expsmooth.no.age")

pred.args <-  pred.args[all.pred.args]

## Source: https://stat.ethz.ch/pipermail/r-help/2006-August/111896.html
delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]
}

pred.args <- delete.NULLs(pred.args)

## names(pred.args)
 
 
rank.rmse.results.no.age  <- function(table.rmse.results.no.age, pred.args){
    
     ## need to work with absolute values of MRE and MPE (should these be selected by users)
     
     if (length(pred.args)==1) {
         table.rmse.results.no.age <- as.data.frame(table.rmse.results.no.age)
     }
      
     table.rmse.results.no.age.tmp <- apply(table.rmse.results.no.age, 2, abs)
     
     if (length(pred.args)==1) {
         table.rmse.results.no.age.tmp <- as.data.frame(table.rmse.results.no.age.tmp)
     }
    
     pred.int.widths <- NULL 
     pred.int.models <- NULL 
     pred.int.lower.limit <- NULL 
     pred.int.upper.limit <- NULL 
     pred.int.center <- NULL 
     for (i in 1:length(pred.args)){
          pred.int.lower.limit.tmp <- round(pred.args[[i]]$PI.lwr)
          pred.int.upper.limit.tmp <- round(pred.args[[i]]$PI.upr)
          pred.int.width.tmp <- pred.int.upper.limit.tmp - pred.int.lower.limit.tmp 
          pred.int.widths <- c(pred.int.widths, pred.int.width.tmp)
          pred.int.models <- c(pred.int.models, class(pred.args[[i]]))
          pred.int.lower.limit <- c(pred.int.lower.limit, pred.int.lower.limit.tmp)
          pred.int.upper.limit <- c(pred.int.upper.limit, pred.int.upper.limit.tmp)
          
          pred.int.center.tmp <- round(pred.args[[i]]$PI.ctr)
          pred.int.center <- c(pred.int.center, pred.int.center.tmp)
     
     }

     pred.int <- data.frame(model=pred.int.models, center=pred.int.center,
                            lower=pred.int.lower.limit, upper=pred.int.upper.limit, 
                            width=pred.int.widths)

     if (ncol(table.rmse.results.no.age.tmp) > 1) {
         ranks <- apply(table.rmse.results.no.age.tmp,2,rank)
         avg.ranks <- round(apply(ranks,1,mean),2) 
         pred.int.and.avg.ranks <- cbind.data.frame(pred.int, avg.ranks) 
         ## index.min.avg.rank <- as.numeric(which(avg.ranks == min(avg.ranks))[1])
         index.min.avg.rank <- as.numeric(which(avg.ranks == min(avg.ranks)))
         index.min.avg.rank <- index.min.avg.rank[which(pred.int.widths[avg.ranks == min(avg.ranks)]==min(pred.int.widths[avg.ranks == min(avg.ranks)]))]  
         model.min.avg.rank <- rownames(table.rmse.results.no.age.tmp)[index.min.avg.rank]
     }

     ## when there is only one forecasting model, ranking is not necessary


     if (ncol(table.rmse.results.no.age.tmp)>1) {
        out <- list()
        ## out$ranking.method <- ranking.method
        out$ranking.measures <- colnames(table.rmse.results.no.age.tmp)
        out$measures <- table.rmse.results.no.age  ## should you have absolute values here for MRE and MPE?  
        out$ranks <- ranks
        out$avg.ranks <- avg.ranks
        out$pred.int.and.avg.ranks <- pred.int.and.avg.ranks
        out$index.min.avg.rank <- index.min.avg.rank
        out$model.min.avg.rank <- index.min.avg.rank
        out$all.models <- rownames(table.rmse.results.no.age.tmp)
        out$best.model <- rownames(table.rmse.results.no.age.tmp)[index.min.avg.rank] 
     }
     
     
     if(ncol(table.rmse.results.no.age.tmp)==1) {
        
        model.tmp <- names(pred.args)
        usePackage("stringr")
        model.tmp <- str_replace_all(model.tmp,"pred.int.individual.stock.","")
        model.tmp <- str_replace_all(model.tmp,".no.age","")
     
     
        out <- list()
        ## out$ranking.method <- ranking.method
        out$ranking.measures <- rownames(table.rmse.results.no.age.tmp)
        out$measures <- table.rmse.results.no.age  ## should you have absolute values here for MRE and MPE?  
        out$ranks <- 1
        out$avg.ranks <- 1
        tmpdat <- cbind.data.frame(pred.int, 1)
        colnames(tmpdat)[colnames(tmpdat)=="1"] <- "Rank"  
        out$pred.int.and.avg.ranks <- 
        out$index.min.avg.rank <- 1
        out$model.min.avg.rank <- 1
        out$all.models <- model.tmp
        out$best.model <- model.tmp 
     }
     

     return(out) 
     
}


table.rank.rmse.results.no.age <- rank.rmse.results.no.age(table.rmse.results.no.age, pred.args)

## print(table.rank.rmse.results.no.age$best.model)
## print(table.rank.rmse.results.no.age)


###
### Histogram of Retrospective Forecast Errors: No Age Information // naiveone 
###

#--------------------------------------------------------------------------------------------------


retro.args <- mget( ls( pattern = "^results.retro.", env = .GlobalEnv) , env = .GlobalEnv )

all.results.retro <- c("results.retro.naiveone.no.age",
                        "results.retro.avgthree.no.age",
                         "results.retro.avgfive.no.age",
                           "results.retro.arima.no.age", 
                             "results.retro.expsmooth.no.age")

retro.args <-  retro.args[all.results.retro]

## Source: https://stat.ethz.ch/pipermail/r-help/2006-August/111896.html
delete.NULLs  <-  function(x.list){   # delele null/empty entries in a list
    x.list[unlist(lapply(x.list, length) != 0)]
}

retro.args <- delete.NULLs(retro.args)

## names(retro.args)

#--------------------------------------------------------------------------------------------------


plot.hist.retrospective.forecast.errors.individual.stock <- function(retro.args){

    .e = environment()

    retro.args 

    usePackage("stringr")
    
    names.retro.args <-  names(retro.args) 
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args
    
    errors.stacked <- NULL 
    labels.stacked <- NULL 
    for (k in 1:length(retro.args)){
    
          tmperrors <- sapply(retro.args[[k]],'[[',"e")
          errors.stacked <- c(errors.stacked,  tmperrors )
  
          tmplabel <-  names.retro.args[k] 
          if (tmplabel=="naiveone") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Previous Year)",length(tmperrors)))
          } else if (tmplabel=="avgthree") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Three Years)",length(tmperrors)))
          } else if (tmplabel=="avgfive") {
              labels.stacked <- c(labels.stacked, rep("Naive Model (Average of Previous Five Years)",length(tmperrors)))
          } else if (tmplabel=="arima") {
              labels.stacked <- c(labels.stacked, rep("ARIMA Model",length(tmperrors)))
          } else {
              labels.stacked <- c(labels.stacked, rep("Exponential Smoothing Model",length(tmperrors)))
          }
          
          
          
          
    }
   
  


    data.stacked <- data.frame(errors=errors.stacked,
                               labels=labels.stacked)

    
    require(plyr)

    # data.stacked <- ddply(data.stacked, c('labels'))
    # data.stacked$labels <- as.factor(data.stacked$labels)
   
    usePackage("ggplot2")
    usePackage("scales")

    d <- data.stacked

    l <- unique(labels.stacked)
           
    str(d) 
    
    d$labels <- factor(d$labels, levels=l)       

    breaks <- NULL
    for (j in 1:length(l)){
        h <- hist(data.stacked$errors[data.stacked$labels==l[j]],plot=FALSE)
        breaks[[j]] <- h$breaks
    }

    #  environment=.e
    g <- ggplot(d, aes(x=errors), environment=.e) +
           mapply(function(d, b) {geom_histogram(data=d, aes(y=..count..), ,breaks=b, fill="lightblue",colour="black")},
            split(d, d$labels), breaks) +
             facet_wrap(~ labels,  scales="fixed", ncol=1) +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Retrospective Forecast Errors"),labels=comma)

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
                
     clim <- NULL
     for (j in 1:length(l)){
     
        clim <- c(clim, 0)

     }
     
     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(aes(xintercept = z), dummy2, linetype="dashed",col="red", size=0.8)
    

     return(g)


}


## plot.hist.retrospective.forecast.errors.individual.stock(retro.args)


#*******************************************************************************
#
# Gary's Plot: avgthree
#
#*******************************************************************************

## pred.int.individual.stock.naiveone.no.age



gary.plot.individual.stock.naiveone.no.age <- function(pred.int.individual.stock.naiveone.no.age,
                                              results.retro.naiveone.no.age,
                                              stockabundance,
                                              forecastingyear){

       ## start from here

       .e <- environment()

       results.retro <- results.retro.naiveone.no.age[[1]]

       method <- "naiveone Model"

       pred <- data.frame(PI.ctr = round(pred.int.individual.stock.naiveone.no.age$PI.ctr),
                          PI.lwr = round(pred.int.individual.stock.naiveone.no.age$PI.lwr),
                          PI.upr = round(pred.int.individual.stock.naiveone.no.age$PI.upr))

       retro <- data.frame(results.retro)
       years <- retro$cy

       retropointforecasts <- retro$e  ## these are forecast errors
       ## retropointforecasts <- round(retropointforecasts,2)
       dfretro <- data.frame(years,retropointforecasts)

       dfretropos <- subset(dfretro,retropointforecasts>=0)
       dfretroneg <- subset(dfretro,retropointforecasts<0)

       pointforecast <- pred$PI.ctr
       ## pointforecast <- round(pred$PI.ctr,2)
       lower <- pred$PI.lwr
       ## lower <- round(pred$PI.lwr,2)
       upper <- pred$PI.upr
       ## upper <- round(pred$PI.upr,2)
       lower <- max(0,lower)
       forecastingyear <-  forecastingyear

       dffor <- data.frame(forecastingyear,pointforecast,lower,upper)

       usePackage("ggplot2")
       usePackage("scales")


       if (pointforecast != lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Previous Year)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast == lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
         scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Previous Year)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8, angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast != lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(paste0(stockabundance, ": ", "Naive Model (Previous Year)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.y=element_text(size=8),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
      }


      if (pointforecast == lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=lower,
        #       label=paste(comma(round(lower)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=1) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=upper,
        #       label=paste(comma(round(upper)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Previous Year)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.y=element_text(size=8),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     ## print(gp)

     return(gp)


} # end gary.plot.complex.sibling.regression for individual ages




## gary.plot.individual.stock.naiveone.no.age(pred.int.individual.stock.naiveone.no.age,
##                                            results.retro.naiveone.no.age,
##                                            stockabundance,
##                                            forecastingyear)



                       
#*******************************************************************************
#
# Gary's Plot: avgthree
#
#*******************************************************************************

## pred.int.individual.stock.avgthree.no.age



gary.plot.individual.stock.avgthree.no.age <- function(pred.int.individual.stock.avgthree.no.age,
                                              results.retro.avgthree.no.age,
                                              stockabundance,
                                              forecastingyear){

       ## start from here

       .e <- environment()

       results.retro <- results.retro.avgthree.no.age[[1]]

       method <- "avgthree Model"

       pred <- data.frame(PI.ctr = round(pred.int.individual.stock.avgthree.no.age$PI.ctr),
                          PI.lwr = round(pred.int.individual.stock.avgthree.no.age$PI.lwr),
                          PI.upr = round(pred.int.individual.stock.avgthree.no.age$PI.upr))

       retro <- data.frame(results.retro)
       years <- retro$cy

       retropointforecasts <- retro$e  ## these are forecast errors
       ## retropointforecasts <- round(retropointforecasts,2)
       dfretro <- data.frame(years,retropointforecasts)

       dfretropos <- subset(dfretro,retropointforecasts>=0)
       dfretroneg <- subset(dfretro,retropointforecasts<0)

       pointforecast <- pred$PI.ctr
       ## pointforecast <- round(pred$PI.ctr,2)
       lower <- pred$PI.lwr
       ## lower <- round(pred$PI.lwr,2)
       upper <- pred$PI.upr
       ## upper <- round(pred$PI.upr,2)
       lower <- max(0,lower)
       forecastingyear <-  forecastingyear

       dffor <- data.frame(forecastingyear,pointforecast,lower,upper)

       usePackage("ggplot2")
       usePackage("scales")


       if (pointforecast != lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Average of Previous Three Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast == lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
         scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Average of Previous Three Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8, angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast != lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(paste0(stockabundance, ": ", "Naive Model (Average of Previous Three Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.y=element_text(size=8),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
      }


      if (pointforecast == lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=lower,
        #       label=paste(comma(round(lower)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=1) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=upper,
        #       label=paste(comma(round(upper)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Average of Previous Three Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.y=element_text(size=8),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     ## print(gp)

     return(gp)


} # end gary.plot.complex.sibling.regression for individual ages




## gary.plot.individual.stock.avgthree.no.age(pred.int.individual.stock.avgthree.no.age,
##                                            results.retro.avgthree.no.age,
##                                            stockabundance,
##                                             forecastingyear)


#*******************************************************************************
#
# Gary's Plot: avgfive
#
#*******************************************************************************

## pred.int.individual.stock.avgfive.no.age


gary.plot.individual.stock.avgfive.no.age <- function(pred.int.individual.stock.avgfive.no.age,
                                              results.retro.avgfive.no.age,
                                              stockabundance,
                                              forecastingyear){

       ## start from here

       .e <- environment()

       results.retro <- results.retro.avgfive.no.age[[1]]

       method <- "avgfive Model"

       pred <- data.frame(PI.ctr = round(pred.int.individual.stock.avgfive.no.age$PI.ctr),
                          PI.lwr = round(pred.int.individual.stock.avgfive.no.age$PI.lwr),
                          PI.upr = round(pred.int.individual.stock.avgfive.no.age$PI.upr))

       retro <- data.frame(results.retro)
       years <- retro$cy

       retropointforecasts <- retro$e  ## these are forecast errors
       ## retropointforecasts <- round(retropointforecasts,2)
       dfretro <- data.frame(years,retropointforecasts)

       dfretropos <- subset(dfretro,retropointforecasts>=0)
       dfretroneg <- subset(dfretro,retropointforecasts<0)

       pointforecast <- pred$PI.ctr
       ## pointforecast <- round(pred$PI.ctr,2)
       lower <- pred$PI.lwr
       ## lower <- round(pred$PI.lwr,2)
       upper <- pred$PI.upr
       ## upper <- round(pred$PI.upr,2)
       lower <- max(0,lower)
       forecastingyear <-  forecastingyear

       dffor <- data.frame(forecastingyear,pointforecast,lower,upper)

       usePackage("ggplot2")
       usePackage("scales")


       if (pointforecast != lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Average of Previous Five Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast == lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
         scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Average of Previous Five Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8, angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast != lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(paste0(stockabundance, ": ", "Naive Model (Average of Previous Five Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.y=element_text(size=8),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
      }


      if (pointforecast == lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=lower,
        #       label=paste(comma(round(lower)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=1) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=upper,
        #       label=paste(comma(round(upper)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "Naive Model (Average of Previous Five Years)")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.y=element_text(size=8),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     ## print(gp)

     return(gp)


} # end gary.plot.complex.sibling.regression for individual ages




## gary.plot.individual.stock.avgfive.no.age(pred.int.individual.stock.avgfive.no.age,
##                                            results.retro.avgfive.no.age,
##                                            stockabundance,
##                                            forecastingyear)



#*******************************************************************************
#
# Gary's Plot: arima
#
#*******************************************************************************

## pred.int.individual.stock.arima.no.age


gary.plot.individual.stock.arima.no.age <- function(pred.int.individual.stock.arima.no.age,
                                              results.retro.arima.no.age,
                                              stockabundance,
                                              forecastingyear){

       ## start from here

       .e <- environment()

       results.retro <- results.retro.arima.no.age[[1]]

       method <- "ARIMA Model"

       pred <- data.frame(PI.ctr = round(pred.int.individual.stock.arima.no.age$PI.ctr),
                          PI.lwr = round(pred.int.individual.stock.arima.no.age$PI.lwr),
                          PI.upr = round(pred.int.individual.stock.arima.no.age$PI.upr))

       retro <- data.frame(results.retro)
       years <- retro$cy

       retropointforecasts <- retro$e  ## these are forecast errors
       ## retropointforecasts <- round(retropointforecasts,2)
       dfretro <- data.frame(years,retropointforecasts)

       dfretropos <- subset(dfretro,retropointforecasts>=0)
       dfretroneg <- subset(dfretro,retropointforecasts<0)

       pointforecast <- pred$PI.ctr
       ## pointforecast <- round(pred$PI.ctr,2)
       lower <- pred$PI.lwr
       ## lower <- round(pred$PI.lwr,2)
       upper <- pred$PI.upr
       ## upper <- round(pred$PI.upr,2)
       lower <- max(0,lower)
       forecastingyear <-  forecastingyear

       dffor <- data.frame(forecastingyear,pointforecast,lower,upper)

       usePackage("ggplot2")
       usePackage("scales")


       if (pointforecast != lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        labs(title=paste0(stockabundance, ": ", "ARIMA Model")) +
        theme(plot.title=element_text(size=12,hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast == lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
         scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "ARIMA Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8, angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast != lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(paste0(stockabundance, ": ", "ARIMA Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.y=element_text(size=8),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
      }


      if (pointforecast == lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=lower,
        #       label=paste(comma(round(lower)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=1) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=upper,
        #       label=paste(comma(round(upper)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "ARIMA Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.y=element_text(size=8),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     ## print(gp)

     return(gp)


} # end gary.plot.complex.sibling.regression for individual ages




## gary.plot.individual.stock.arima.no.age(pred.int.individual.stock.arima.no.age,
##                                            results.retro.arima.no.age,
##                                            stockabundance,
##                                            forecastingyear)



#*******************************************************************************
#
# Gary's Plot: expsmooth
#
#*******************************************************************************

## pred.int.individual.stock.expsmooth.no.age


gary.plot.individual.stock.expsmooth.no.age <- function(pred.int.individual.stock.expsmooth.no.age,
                                              results.retro.expsmooth.no.age, 
                                              stockabundance,
                                              forecastingyear){

       ## start from here

       .e <- environment()
       
       results.retro <- results.retro.expsmooth.no.age[[1]]
       
       method <- "Exponential Smoothing Model"
 
       pred <- data.frame(PI.ctr = round(pred.int.individual.stock.expsmooth.no.age$PI.ctr),
                          PI.lwr = round(pred.int.individual.stock.expsmooth.no.age$PI.lwr),
                          PI.upr = round(pred.int.individual.stock.expsmooth.no.age$PI.upr))

       retro <- data.frame(results.retro)
       years <- retro$cy

       retropointforecasts <- retro$e  ## these are forecast errors
       ## retropointforecasts <- round(retropointforecasts,2)
       dfretro <- data.frame(years,retropointforecasts)

       dfretropos <- subset(dfretro,retropointforecasts>=0)
       dfretroneg <- subset(dfretro,retropointforecasts<0)

       pointforecast <- pred$PI.ctr
       ## pointforecast <- round(pred$PI.ctr,2)
       lower <- pred$PI.lwr
       ## lower <- round(pred$PI.lwr,2)
       upper <- pred$PI.upr
       ## upper <- round(pred$PI.upr,2)
       lower <- max(0,lower)
       forecastingyear <-  forecastingyear

       dffor <- data.frame(forecastingyear,pointforecast,lower,upper)

       usePackage("ggplot2")
       usePackage("scales")


       if (pointforecast != lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        labs(title=paste0(stockabundance, ": ", "Exponential Smoothing Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast == lower & pointforecast != upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
         scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "Exponential Smoothing Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8, angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     if (pointforecast != lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=pointforecast,
        #       label=paste(comma(round(pointforecast)))),
        #       colour="red",angle=0,size=3,hjust=0) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=1) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=2.5,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(paste0(stockabundance, ": ", "Exponential Smoothing Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
             axis.text.y=element_text(size=8),
             axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
      }


      if (pointforecast == lower & pointforecast == upper) {

        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower)
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper)

        ## environment=.e
        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
        geom_rect(data=dfretro,aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
        geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=0,vjust=0.2) +
        geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=2.5,hjust=1,vjust=0.2) +
        geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=2.5,hjust=0) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=lower,
        #       label=paste(comma(round(lower)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=1) +
        # geom_text(data=dffor,aes(x=forecastingyear+1/3,
        #       y=upper,
        #       label=paste(comma(round(upper)))),
        #       colour="red",angle=0,size=3,hjust=0,vjust=-0.4)  +
        geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
        geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=lower,xend=forecastingyear+1/4,yend=lower),colour="red",size=1) +
        geom_segment(data=dffor, aes(x=forecastingyear-1/4,y=upper,xend=forecastingyear+1/4,yend=upper),colour="red",size=1) +
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )
                        expand=c(0,0)
                        ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste0(stockabundance, ": ", "Exponential Smoothing Model")) +
        theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.y=element_text(size=8),
              axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
             axis.title.x=element_text(size=10,vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
     }


     ## print(gp)

     return(gp)


} # end gary.plot.complex.sibling.regression for individual ages




## gary.plot.individual.stock.expsmooth.no.age(pred.int.individual.stock.expsmooth.no.age,
##                                            results.retro.expsmooth.no.age, 
##                                            stockabundance,
##                                            forecastingyear)
                                             


##
## Table of results for candidate models for no age: avgfive, arima, expsmooth
##

# pred.int.individual.stock.naiveone.no.age
# pred.int.individual.stock.avgthree.no.age
# pred.int.individual.stock.avgfive.no.age
# pred.int.individual.stock.arima.no.age
# pred.int.individual.stock.expsmooth.no.age


## extract PI.ctr  ---  lapply(X = pred.args, FUN = `[[`, "PI.ctr")



table.results.individual.stock.all.models.no.age <- list()

names.pred.args <-  names(pred.args) 
names.pred.args <- str_replace_all(names.pred.args,"pred.int.individual.stock.","")
names.pred.args <- str_replace_all(names.pred.args,".no.age","")
## names.pred.args
    
labels.pred.args <- NULL       ## names of forecasting models

for (k in 1:length(pred.args)){
    
          tmplabel <-  names.pred.args[k] 
          if (tmplabel=="naiveone") {
              labels.pred.args <- c(labels.pred.args, "Naive Model (Previous Year)")
          } else if (tmplabel=="avgthree") {
              labels.pred.args <- c(labels.pred.args, "Naive Model (Average of Previous Three Years)")
          } else if (tmplabel=="avgfive") {
              labels.pred.args <- c(labels.pred.args, "Naive Model (Average of Previous Five Years)")
          } else if (tmplabel=="arima") {
              labels.pred.args <- c(labels.pred.args, "ARIMA Model")
          } else {
              labels.pred.args <- c(labels.pred.args, "Exponential Smoothing Model")
          }
}

table.results.individual.stock.all.models.no.age$Model <- labels.pred.args   ## names of forecasting models
                                                             
table.results.individual.stock.all.models.no.age$PI.ctr <-  unlist(lapply(X = pred.args, FUN = `[[`, "PI.ctr"))

table.results.individual.stock.all.models.no.age$PI.ctr <- round(as.numeric(table.results.individual.stock.all.models.no.age$PI.ctr))

table.results.individual.stock.all.models.no.age$PI.lwr <- unlist(lapply(X = pred.args, FUN = `[[`, "PI.lwr"))
                                                                 
table.results.individual.stock.all.models.no.age$PI.lwr <- round(as.numeric(table.results.individual.stock.all.models.no.age$PI.lwr))
                                                               
                                                               
table.results.individual.stock.all.models.no.age$PI.upr <- unlist(lapply(X = pred.args, FUN = `[[`, "PI.upr")) 
                                                               
table.results.individual.stock.all.models.no.age$PI.upr <- round(as.numeric(table.results.individual.stock.all.models.no.age$PI.upr))

table.results.individual.stock.all.models.no.age <- do.call(cbind.data.frame, table.results.individual.stock.all.models.no.age)

usePackage("scales")
table.results.individual.stock.all.models.no.age$PI.ctr <- comma(table.results.individual.stock.all.models.no.age$PI.ctr)
table.results.individual.stock.all.models.no.age$PI.lwr <- comma(table.results.individual.stock.all.models.no.age$PI.lwr)
table.results.individual.stock.all.models.no.age$PI.upr <- comma(table.results.individual.stock.all.models.no.age$PI.upr)



table.results.individual.stock.all.models.no.age$PI.int <- paste0(table.results.individual.stock.all.models.no.age$PI.lwr, 
                                                                   " - ", 
                                                                   table.results.individual.stock.all.models.no.age$PI.upr)

table.results.individual.stock.all.models.no.age <- subset(table.results.individual.stock.all.models.no.age, 
                                                            select=c(Model, PI.ctr,  PI.int))
names(table.results.individual.stock.all.models.no.age) <-  c("Model", "Point Forecast", "Interval Forecast")

## table.results.individual.stock.all.models.no.age



###
### plot forecast vs. actual: all models 
###

scatter.plot.results.afe.individual.stock.retro.all.models.no.age <- function(retro.args, stockabundance){

    .e = environment()

    retro.args

    usePackage("stringr")

    names.retro.args <-  names(retro.args)
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args


    forecasted.stacked <- NULL
    actual.stacked <- NULL
    title.stacked <- NULL
    labels.stacked <- NULL
    
    for (i in 1:length(retro.args)){

       usePackage("stringr")
       
       datatmp <- retro.args[[i]][[1]]

       names(datatmp)[names(datatmp)=="a"] <- "Actual"
       names(datatmp)[names(datatmp)=="p"] <- "Forecast"
       names(datatmp)[names(datatmp)=="e"] <- "Error"

       ## r.sq <- summary(lm(Forecast ~ Actual, data=datatmp))$r.squared
       ## r.sq <-  sprintf("%.2f", r.sq*100)

       ## R.squared <- c(R.squared, r.sq)

        tmplabel <-  names.retro.args[i]
        if (tmplabel=="naiveone") {
              mytitle <- "Naive Model \n (Previous Year)"
           } else if (tmplabel=="avgthree") {
              mytitle <- "Naive Model \n (Average of Previous 3 Years)" 
           } else if (tmplabel=="avgfive") {
             mytitle <-  "Naive Model \n (Average of Previous 5 Years)"
           } else if (tmplabel=="arima") {
             mytitle <-   "ARIMA Model"
           } else {
             mytitle <-   "Exponential Smoothing Model"
        }

       ## mytitle <- paste(mytitle, ": ", "Naive (Last Year)", sep="")
	     ## mytitle <- paste(mytitle, "\n","R-squared = ", r.sq , "%", sep="")

    	 usePackage("calibrate")
    	 labs <- substr(datatmp$cy,  # return year (or, equivalently, calendar year)
                      ## datatmp$cy - myage ,   # brood year
              start=3, stop=4)

       forecasted.stacked <- c(forecasted.stacked, datatmp$Forecast)
       
       actual.stacked <- c(actual.stacked, datatmp$Actual)
       
       title.stacked <- c(title.stacked, rep(mytitle, length(datatmp$Actual)))

       labels.stacked <- c(labels.stacked, labs)

    }

    data.stacked <- data.frame(forecasted=forecasted.stacked,
                               actual=actual.stacked,
                               title=title.stacked,
                               labels=labels.stacked)

    data.stacked$title
    
    data.stacked$title <- factor(data.stacked$title, levels=unique(data.stacked$title))
    
    levels(data.stacked$title)



    usePackage("ggplot2")
    usePackage("scales")

    ## environment=.e
    g <- ggplot(data.stacked, aes(actual,forecasted))  +
      ## g <- ggplot(data.stacked, aes(actual,forecasted)) +
       geom_abline(intercept=0,slope=1,colour="red",size=0.8) +
       geom_text(aes(label=labels),col="blue",size=3) +
            coord_fixed(ratio=1) +
              facet_wrap(~title, scales="fixed") +
                expand_limits(x=0, y=0) +
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma) +
                   scale_y_continuous(paste("Forecasted", stockabundance, "Values"),labels=comma) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous(paste("Actual", stockabundance, "Values"),labels=comma) +
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



## scatter.plot.results.afe.individual.stock.retro.all.models.no.age(retro.args, stockabundance)



#-----------------------------------------------------------------------------------------
# Time series plot of retrospectively forecasted and actual values 
#-----------------------------------------------------------------------------------------

timeseries.plot.results.afe.individual.stock.retro.all.models.no.age <- function(retro.args, stockabundance){
 
    .e = environment()
    
     retro.args

    usePackage("stringr")

    names.retro.args <-  names(retro.args)
    names.retro.args <- str_replace_all(names.retro.args,"results.retro.","")
    names.retro.args <- str_replace_all(names.retro.args,".no.age","")
    names.retro.args


    forecasted.stacked <- NULL
    actual.stacked <- NULL
    title.stacked <- NULL
    labels.stacked <- NULL
    
    for (i in 1:length(retro.args)){

       usePackage("stringr")
       
       datatmp <- retro.args[[i]][[1]]

       names(datatmp)[names(datatmp)=="a"] <- "Actual"
       names(datatmp)[names(datatmp)=="p"] <- "Forecast"
       names(datatmp)[names(datatmp)=="e"] <- "Error"

       ## r.sq <- summary(lm(Forecast ~ Actual, data=datatmp))$r.squared
       ## r.sq <-  sprintf("%.2f", r.sq*100)

       ## R.squared <- c(R.squared, r.sq)

        tmplabel <-  names.retro.args[i]
        if (tmplabel=="naiveone") {
              mytitle <- "Naive Model \n (Previous Year)"
           } else if (tmplabel=="avgthree") {
              mytitle <- "Naive Model \n (Average of Previous 3 Years)" 
           } else if (tmplabel=="avgfive") {
             mytitle <-  "Naive Model \n (Average of Previous 5 Years)"
           } else if (tmplabel=="arima") {
             mytitle <-   "ARIMA Model"
           } else {
             mytitle <-   "Exponential Smoothing Model"
        }

       ## mytitle <- paste(mytitle, ": ", "Naive (Last Year)", sep="")
	     ## mytitle <- paste(mytitle, "\n","R-squared = ", r.sq , "%", sep="")

    	 ## usePackage("calibrate")
    	 ## labs <- substr(datatmp$cy,  # return year (or, equivalently, calendar year)
       ##               ## datatmp$cy - myage ,   # brood year
       ##       start=3, stop=4)

       labs <- datatmp$cy

       forecasted.stacked <- c(forecasted.stacked, datatmp$Forecast)
       
       actual.stacked <- c(actual.stacked, datatmp$Actual)
       
       title.stacked <- c(title.stacked, rep(mytitle, length(datatmp$Actual)))

       labels.stacked <- c(labels.stacked, labs)

    }

    data.stacked <- data.frame(forecasted=forecasted.stacked,
                               actual=actual.stacked,
                               title=title.stacked,
                               labels=labels.stacked)

    data.stacked$title
    
    data.stacked$title <- factor(data.stacked$title, levels=unique(data.stacked$title))
    
    levels(data.stacked$title)
     
     
    usePackage("ggplot2")
    usePackage("scales")
    
    g <- ggplot(data.stacked, aes(labels, actual), environment=.e)  +    # environment=.e
       geom_line(aes(labels, actual, colour="Actual"),size=0.8) +
       geom_line(aes(labels, forecasted, colour="Forecasted"), size=0.8) +  
       geom_point(aes(labels, actual, colour="Actual"),size=2.5) +
       geom_point(aes(labels, forecasted, colour="Forecasted"), size=2.5) + 
            ## coord_fixed(ratio=1) +
              facet_wrap(~title, scales="free",ncol=2) + 
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


## results <- results.individual.ages.retro.predictive.performance.expsmooth
## timeseries.plot.results.afe.individual.ages.retro.expsmooth(results, stockabundance)

timeseries.plot.results.afe.individual.stock.retro.all.models.no.age(retro.args, stockabundance)

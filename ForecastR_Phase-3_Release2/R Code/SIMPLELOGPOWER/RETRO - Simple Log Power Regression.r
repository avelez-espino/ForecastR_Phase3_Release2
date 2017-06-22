cat("RETRO - Simple Log Power Regression.R", "\n\n")


## need to refresh the datalist object, as it currently contains only the youngest age!!!

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

SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist.avgfive(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)  # CY refers to the T variable with highest age

##========================================================================================================
## youngest age: avgfive
##========================================================================================================

SIMPLELOGPOWER$individual.ages.retro.predictive.performance.avgfive.youngest <- function(datalist, index){

      datalist <- datalist

      ## index <- 10

      PSY <- SIMPLELOGPOWER$forecastingyear

      subdata <- subset(datalist[[1]], CY < PSY)

      y <- subdata[,ncol(subdata)]
      cy <- subdata[,"CY"]

      broodyear <- subdata[,"BY"]

      usePackage("stringr")

      a <- NULL
      p <- NULL
      e <- NULL

      cy00 <- NULL

      by00 <- NULL

      data0 <- NULL

      p.bench <- NULL
      e.bench <- NULL

      for (i in index:(length(y)-1)){

           y0 <- y[1:i]

           cy0 <- cy[1:i]

           by0 <- broodyear[1:i]

           model0 <- SIMPLELOGPOWER$avgfive.youngest(y0)

           p0 <- as.numeric(model0$mean)

           f0 <- as.numeric(model0$fitted)


           d0 <- data.frame(i=i, cy0, by0, y0, f0, p0, psy=max(by0)+1, a0=y[i+1])

           ## cat("------ \n ")
           ## print(d0)
           ## cat("------ \n ")

           ## str(d0)

           data0 <- rbind.data.frame(data0, d0)

           p <- c(p, p0)
           e0 <- y[i+1] - p0   # actual - predicted
           e <- c(e, e0)
           a <- c(a, y[i+1]) #actual

           cy00 <- c(cy00, cy[i+1])

           by00 <- c(by00, broodyear[i+1])

           ## benchmark: naive forecasting (previous year)

           y0 <- y[1:i]

           usePackage("forecast")

           model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

           p0.bench <- as.numeric(model0.bench$mean)

           p.bench <- c(p.bench, p0.bench)
           e0.bench <- y[i+1] - p0.bench   # actual - predicted
           e.bench <- c(e.bench, e0.bench)

        }

        result.avgfive.youngest <- data.frame(cy=cy00, by=by00, a, p, e, p.bench, e.bench)


        result.avgfive.youngest.output <- list()
        result.avgfive.youngest.output[[1]]  <- result.avgfive.youngest

        youngestage <- as.numeric(gsub("[^\\d]+", "", names(datalist[[1]])[length(names(datalist[[1]]))], perl=TRUE))

        ## cat("====== \n")
        ## print(youngestage)
        ## cat("====== \n")

        names(result.avgfive.youngest.output) <- paste0("Age_",youngestage)

        ## youngest.age.retro.plot.info.avgfive <<- list(data0=data0,age0 = paste0("Age_",youngestage))

        SIMPLELOGPOWER$youngest.age.retro.plot.info.avgfive <- list(data0=data0,age0 = paste0("Age_",youngestage))

        result.avgfive.youngest.output


}


SIMPLELOGPOWER$index.year <- index.year

SIMPLELOGPOWER$result.avgfive.youngest <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.avgfive.youngest(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$index.year)

SIMPLELOGPOWER$youngest.age.retro.plot.info.avgfive



SIMPLELOGPOWER$youngest.age.retro.plot.avgfive <- function(youngest.age.retro.plot.info.avgfive, stockabundance){

   .e <- environment()

   mydata <- youngest.age.retro.plot.info.avgfive$data0

   tmpage <- youngest.age.retro.plot.info.avgfive$age0

   tmpage <- str_replace(tmpage, "_", " ")

   usePackage("ggplot2")

   ggplot(mydata, aes(by0, y0), environment=.e) +             # environment=.e
    geom_line(data=mydata, aes(by0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) +
      geom_line(data=mydata, aes(by0, f0), colour="red") +
       geom_point(data=mydata, aes(psy, p0), colour="red") +
        ylab(paste0(stockabundance)) +
         xlab(paste0("Brood Year")) +
          ggtitle(label=paste0(tmpage)) +
           facet_wrap(~psy) +
            theme_bw() +
             scale_y_continuous(labels=scales::comma) +
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8),
                   axis.text.y = element_text(vjust = 0.5, size=8),
                   strip.text.x = element_text(size = 9))

}

SIMPLELOGPOWER$youngest.age.retro.plot.avgfive(SIMPLELOGPOWER$youngest.age.retro.plot.info.avgfive, SIMPLELOGPOWER$stockabundance)


##========================================================================================================
## youngest age: arima
##========================================================================================================

SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist.arima(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)  # CY refers to the T variable with highest age


SIMPLELOGPOWER$arima.model <- function(datalist, boxcoxtransform){

     output <- vector("list", length(datalist)) # start to build the S3 class storing the output

     class(output) <- "arima"  # create a new class

     for (j in 1:length(datalist)) {

          # form <- as.formula(names(datalist)[j])

          # form

          # output[[j]]$model <- lm(form, data=datalist[[j]][complete.cases(datalist[[j]]),])


          tmpdata <- datalist[[j]][complete.cases(datalist[[j]]),]

          series <- tmpdata[ ,ncol(tmpdata)]

          usePackage("forecast")

          ## lambda <- BoxCox.lambda(series)

          ## transformed_series <- BoxCox(series, lambda)

          usePackage("stringr")

          output[[j]]$age <-  paste("Age ",
                                    as.numeric(str_extract(names(tmpdata)[length(names(tmpdata))],"[[:digit:]]")),
                                    sep="")


          if (boxcoxtransform==TRUE) {

              output[[j]]$model <- auto.arima(series, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(series, method="guerrero"))

          } else {

              output[[j]]$model <- auto.arima(series, allowmean=TRUE, allowdrift=FALSE)
          }


          ## output[[j]]$model <- auto.arima(series, allowmean=FALSE, allowdrift=FALSE)

          # output[[j]]$formula <- form

          output[[j]]$original.data <- datalist[[j]]

          output[[j]]$model.data <- datalist[[j]][complete.cases(datalist[[j]]),]

     }

     names(output) <- names(datalist)

     return(output)

}


## arima.model.fits  <- arima.model(datalist, boxcoxtransform)

SIMPLELOGPOWER$individual.ages.retro.predictive.performance.arima.youngest <- function(datalist, forecastingyear, boxcoxtransform, index){

    index <- index

    PSY <- forecastingyear    ## Come back here to make sure the forecasting year is specified correctly!

    subdata <- subset(datalist[[1]], CY < PSY)

    y <- subdata[,ncol(subdata)]

    cy <- subdata[,"CY"]

    broodyear <- subdata[,"BY"]

    usePackage("forecast")

    a <- NULL
    p <- NULL
    e <- NULL

    cy00 <- NULL

    by00 <- NULL

    data0 <- NULL

    p.bench <- NULL
    e.bench <- NULL

    for (i in index:(length(y)-1)){

        y0 <- y[1:i]

        cy0 <- cy[1:i]

        by0 <- broodyear[1:i]

        ##---

        if (boxcoxtransform==TRUE) {

                        model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(y0, method="guerrero"))
        } else {

                        model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE)
        }

        ##---

        p0 <- round(as.numeric(forecast(model0, h=1, level=0.80, biasadj=FALSE)$mean))

        f0 <- round(as.numeric(fitted(model0, biasadj=FALSE)))

        d0 <- data.frame(i=i, cy0, by0, y0, f0, p0, psy=max(by0)+1, a0=y[i+1])

        ## cat("------ \n ")
        ## print(d0)
        ## cat("------ \n ")

        ## str(d0)

        data0 <- rbind.data.frame(data0, d0)

        ##---

        p <- c(p, p0)
        e0 <- y[i+1] - p0   # actual - predicted
        e <- c(e, e0)
        a <- c(a, y[i+1]) #actual

        cy00 <- c(cy00, cy[i+1])

        by00 <- c(by00, broodyear[i+1])

        ## benchmark: naive forecasting (previous year)

        y0 <- y[1:i]

        usePackage("forecast")

        model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

        p0.bench <- as.numeric(model0.bench$mean)
        p0.bench <- round(p0.bench)

        p.bench <- c(p.bench, p0.bench)
        e0.bench <- y[i+1] - p0.bench   # actual - predicted
        e.bench <- c(e.bench, e0.bench)

    }


    result.arima.youngest <- data.frame(cy=cy00, by=by00, a, p, e, p.bench, e.bench)

    result.arima.youngest  ## results of retrospective point forecast performance for
                           ## youngest age

    result.arima.youngest.output <- list()
    result.arima.youngest.output[[1]]  <- result.arima.youngest

    youngestage <- as.numeric(gsub("[^\\d]+", "", names(datalist[[1]])[length(names(datalist[[1]]))], perl=TRUE))

    names(result.arima.youngest.output) <- paste0("Age_",youngestage)

    ## youngest.age.retro.plot.info.arima <<- list(data0=data0, age0 = paste0("Age_",youngestage))

    SIMPLELOGPOWER$youngest.age.retro.plot.info.arima <- list(data0=data0, age0 = paste0("Age_",youngestage))

    result.arima.youngest.output


}


SIMPLELOGPOWER$result.arima.simple.sibling.regression <-  SIMPLELOGPOWER$individual.ages.retro.predictive.performance.arima.youngest( SIMPLELOGPOWER$datalist,
                                                                 SIMPLELOGPOWER$forecastingyear,  SIMPLELOGPOWER$boxcoxtransform,  SIMPLELOGPOWER$index.year)



SIMPLELOGPOWER$youngest.age.retro.plot.arima <- function(youngest.age.retro.plot.info.arima, stockabundance){

   .e <- environment()

   mydata <- youngest.age.retro.plot.info.arima$data0

   tmpage <- youngest.age.retro.plot.info.arima$age0

   tmpage <- str_replace(tmpage, "_", " ")

   usePackage("ggplot2")

   ggplot(mydata, aes(by0, y0), environment=.e) +             # environment=.e
    geom_line(data=mydata, aes(by0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) +
      geom_line(data=mydata, aes(by0, f0), colour="red") +
       geom_point(data=mydata, aes(psy, p0), colour="red") +
        ylab(paste0(stockabundance)) +
         xlab(paste0("Return Year")) +
          ggtitle(label=paste0(tmpage)) +
           facet_wrap(~psy) +
            theme_bw() +
             scale_y_continuous(labels=scales::comma) +
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8),
                   axis.text.y = element_text(vjust = 0.5, size=8),
                   strip.text.x = element_text(size = 9))

}


SIMPLELOGPOWER$youngest.age.retro.plot.arima(SIMPLELOGPOWER$youngest.age.retro.plot.info.arima, SIMPLELOGPOWER$stockabundance)


##========================================================================================================
## youngest age: expsmooth
##========================================================================================================

SIMPLELOGPOWER$datalist <- SIMPLELOGPOWER$datalist.expsmooth(SIMPLELOGPOWER$datafile, SIMPLELOGPOWER$forecastingyear)


#---------  fit exponential smoothing model -----------------------------------------


SIMPLELOGPOWER$expsmooth.model <- function(datalist, boxcoxtransform){

     output <- vector("list", length(datalist)) # start to build the S3 class storing the output

     class(output) <- "expsmooth"  # create a new class

     for (j in 1:length(datalist)) {

          # form <- as.formula(names(datalist)[j])

          # form

          # output[[j]]$model <- lm(form, data=datalist[[j]][complete.cases(datalist[[j]]),])


          tmpdata <- datalist[[j]][complete.cases(datalist[[j]]),]

          series <- tmpdata[ ,ncol(tmpdata)]

          usePackage("forecast")

          usePackage("stringr")

          output[[j]]$age <-  paste("Age ",
                                    as.numeric(str_extract(names(tmpdata)[length(names(tmpdata))],"[[:digit:]]")),
                                    sep="")



          if (boxcoxtransform==TRUE) {

              ## output[[j]]$model <- auto.arima(series, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(series, method="guerrero"))

              output[[j]]$model <- ets(series, model="ZZZ", lambda=BoxCox.lambda(series))

          } else {

              ## output[[j]]$model <- auto.arima(series, allowmean=TRUE, allowdrift=FALSE)

              output[[j]]$model <- ets(series, model="ZZZ", lambda=NULL)
          }


          # output[[j]]$formula <- form

          output[[j]]$original.data <- datalist[[j]]

          output[[j]]$model.data <- datalist[[j]][complete.cases(datalist[[j]]),]

     }

     names(output) <- names(datalist)

     return(output)

}


## expsmooth.model.fits  <- expsmooth.model(datalist, boxcoxtransform)


SIMPLELOGPOWER$individual.ages.retro.predictive.performance.expsmooth.youngest <- function(datalist, forecastingyear, boxcoxtransform, index){

    index <- index


    PSY <- forecastingyear

    subdata <- subset(datalist[[1]], CY < PSY)

    y <- subdata[,ncol(subdata)]

    cy <- subdata[,"CY"]

    broodyear <- subdata[,"BY"]

    a <- NULL
    p <- NULL
    e <- NULL

    cy00 <- NULL

    by00 <- NULL

    data0 <- NULL

    p.bench <- NULL
    e.bench <- NULL

    for (i in index:(length(y)-1)){

        y0 <- y[1:i]

        cy0 <- cy[1:i]

        by0 <- broodyear[1:i]

        usePackage("forecast")

        if (boxcoxtransform==TRUE) {

            y0[y0==0] <- 0.001 ## add a small constant to zero counts

            model0 <- ets(y0, lambda=BoxCox.lambda(y0))

        } else {

            model0 <- ets(y0)
        }

        p0 <- round(as.numeric(forecast(model0, h=1, level=80, biasadj=FALSE)$mean))

        f0 <- round(as.numeric(fitted(model0, biasadj=FALSE)))

        #----

        d0 <- data.frame(i=i, cy0, by0, y0, f0, p0, psy=max(by0)+1, a0=y[i+1])

        data0 <- rbind.data.frame(data0, d0)

        #----

        p <- c(p, p0)
        e0 <- y[i+1] - p0   # actual - predicted
        e <- c(e, e0)
        a <- c(a, y[i+1]) #actual

        cy00 <- c(cy00, cy[i+1])

        by00 <- c(by00, broodyear[i+1])

        ## benchmark: naive forecasting (previous year)

        y0 <- y[1:i]

        usePackage("forecast")

        model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

        p0.bench <- as.numeric(model0.bench$mean)
        p0.bench <- round(p0.bench)

        p.bench <- c(p.bench, p0.bench)
        e0.bench <- y[i+1] - p0.bench   # actual - predicted
        e.bench <- c(e.bench, e0.bench)

    }

    result.expsmooth.youngest <- data.frame(cy=cy00,  by=by00, a, p, e, p.bench, e.bench)

    result.expsmooth.youngest  ## results of retrospective point forecast performance for
                               ## youngest age

    result.expsmooth.youngest.output <- list()
    result.expsmooth.youngest.output[[1]]  <- result.expsmooth.youngest

    youngestage <- as.numeric(gsub("[^\\d]+", "", names(datalist[[1]])[length(names(datalist[[1]]))], perl=TRUE))

    names(result.expsmooth.youngest.output) <- paste0("Age_",youngestage)

    ## youngest.age.retro.plot.info.expsmooth <<- list(data0=data0,age0 = paste0("Age_",youngestage))

    SIMPLELOGPOWER$youngest.age.retro.plot.info.expsmooth <- list(data0=data0,age0 = paste0("Age_",youngestage))

    result.expsmooth.youngest.output

}


SIMPLELOGPOWER$result.expsmooth.simple.sibling.regression <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.expsmooth.youngest(
                                                                  SIMPLELOGPOWER$datalist,
                                                                  SIMPLELOGPOWER$forecastingyear,
                                                                  SIMPLELOGPOWER$boxcoxtransform,
                                                                  SIMPLELOGPOWER$index.year)



SIMPLELOGPOWER$youngest.age.retro.plot.expsmooth <- function(youngest.age.retro.plot.info.expsmooth, stockabundance){

   .e <- environment()

   mydata <- youngest.age.retro.plot.info.expsmooth$data0

   tmpage <- youngest.age.retro.plot.info.expsmooth$age0

   tmpage <- str_replace(tmpage, "_", " ")

   usePackage("ggplot2")

   ggplot(mydata, aes(by0, y0), environment=.e) +             # environment=.e
    geom_line(data=mydata, aes(by0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) +
      geom_line(data=mydata, aes(by0, f0), colour="red") +
       geom_point(data=mydata, aes(psy, p0), colour="red") +
        ylab(paste0(stockabundance)) +
         xlab(paste0("Return Year")) +
          ggtitle(label=paste0(tmpage)) +
           facet_wrap(~psy) +
            theme_bw() +
             scale_y_continuous(labels=scales::comma) +
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8),
                   axis.text.y = element_text(vjust = 0.5, size=8),
                   strip.text.x = element_text(size = 9))

}


SIMPLELOGPOWER$youngest.age.retro.plot.expsmooth(SIMPLELOGPOWER$youngest.age.retro.plot.info.expsmooth, SIMPLELOGPOWER$stockabundance)


##=========================================================================================================
## Simple log power regression
##=========================================================================================================


SIMPLELOGPOWER$individual.ages.retro.predictive.performance.simplelogpower.regression.youngest <- function(pred.int.individual.ages.simplelogpower.regression, index){

     datalist.simplelogpower.regression <- pred.int.individual.ages.simplelogpower.regression

     ## index <- 10

     ## PSY <- forecastingyear    ## Come back here to make sure the forecasting year is specified correctly!

     result <- list()

     nms <- NULL

     SIMPLELOGPOWER$individual.ages.retro.plot.info.simplelogpower <- list()

     for (j in 1:length(datalist.simplelogpower.regression)){

          ## subdata <- subset(datalist[[j]]$model.data, CY < PSY)
          subdata <- datalist.simplelogpower.regression[[j]]$model.data
          submodelformula <- as.formula(datalist.simplelogpower.regression[[j]]$model.formula)

          usePackage("formula.tools")

          outcome <- lhs.vars(submodelformula)
          outcome <- attr(outcome,"term.labels")
          predictors <- rhs.vars(submodelformula)

          usePackage("stringr")

          if ( sum(str_count(predictors, "\\+")) >= 1 ) {

            predictors <- unlist(strsplit(predictors, " \\+ "))

            predictors <- gsub("log\\(", "log", x=predictors)

          }

          predictors


          predictors <- gsub("\\(|\\)", "", x=predictors)
          predictors <- gsub(pattern="log|log1p", replacement="", x=predictors)

          predictors


          outcome <- gsub("\\(|\\)", "", x=outcome)
          outcome <- gsub(pattern="log|log1p", replacement="", x=outcome)


     	    y <- subdata[,outcome]
          ## cy <- subdata[,"CY"]

          a <- NULL
          p <- NULL
          e <- NULL

          by00 <- NULL  ## work with brood year rather than calendar year (?)

          data0 <- NULL

          p.bench <- NULL
          e.bench <- NULL

          for (i in (index-j+0):(length(y)-1)){
          ## for (i in index:(length(y)-1)){
               ## y0 <- y[1:i]

               subdata0 <- subdata[1:i,]

               y0 <- subdata0[ ,outcome]
               by0 <- subdata0[,"Brood_Year"]

               model0 <- lm(submodelformula, data=subdata0)

               # model0 <- arima(y0, order = c(p.0, d.0, q.0))

               newdata0 = subset(subdata,select=predictors)
               newdata0 = subset(newdata0, rownames(newdata0)==(i+1))

               ## p0 <- as.numeric(forecast(model0, h=1, level=80)$mean)
               p0.log.scale <- predict(model0, newdata=newdata0, interval="prediction")[1]

               sigma0.ols <- summary(model0)$sigma
               n0 <- nrow(model.frame(model0))
               sigma0.squared.mle <- sigma0.ols^2 * ((n0-2)/n0)
               p0 <- round(exp(p0.log.scale + (sigma0.squared.mle/2)))

               p0 <- round(p0)

               ## f0 <- as.numeric(fitted(model0))

               f0.log.scale <- as.numeric(fitted(model0))
               f0 <- round(exp(f0.log.scale + (sigma0.squared.mle/2)))

               f0 <- round(f0)

               #----

               d0 <- data.frame(i=i, by0, y0, f0, p0, psy=max(by0)+1, a0=y[i+1])
               # note replacement of cy0 (calendar year) with by0 (brood year)

               ## str(d0)

               data0 <- rbind.data.frame(data0, d0)

               #----

               p <- c(p, p0)
               e0 <- y[i+1] - p0   # actual - predicted
               e <- c(e, e0)
               a <- c(a, y[i+1]) #actual

               by00 <- c(by00, subdata[i+1,"Brood_Year"])

               ## benchmark: naive forecasting (previous year)

               y0 <- y[1:i]

               usePackage("forecast")

               model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

               p0.bench <- round(as.numeric(model0.bench$mean))
               ## p0.bench <- round(p0.bench)

               p.bench <- c(p.bench, p0.bench)
               e0.bench <- y[i+1] - p0.bench   # actual - predicted
               e.bench <- c(e.bench, e0.bench)

          }

          nms <- c(nms,outcome)

          result[[j]] <- data.frame(by=by00, a, p, e, p.bench, e.bench)

          SIMPLELOGPOWER$individual.ages.retro.plot.info.simplelogpower[[j]] <- data0

     } # end for j loop

     nms

     names(result) <- nms

     names(SIMPLELOGPOWER$individual.ages.retro.plot.info.simplelogpower) <- nms

     for (j in 1:length(datalist.simplelogpower.regression)){
          result[[j]]$cy <- result[[j]]$by + as.numeric(gsub("[^\\d]+", "", nms[j], perl=TRUE))

     }


    ### combine all of the results: result.avgfive.youngest & result
    result

}


SIMPLELOGPOWER$result.simplelogpower.regression <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.simplelogpower.regression.youngest(
                                             SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                             SIMPLELOGPOWER$index.year)




#=====================================================================================================================


SIMPLELOGPOWER$individual.ages.retro.plot.simplelogpower <- function(individual.ages.retro.plot.info.simplelogpower, stockabundance, j){

   .e <- environment()

   mydata <- individual.ages.retro.plot.info.simplelogpower[[j]]

   ## mydata$psy <- factor(mydata$psy)


   tmpage <-  names(individual.ages.retro.plot.info.simplelogpower)[j]

   tmpage <- str_replace(tmpage, "_", " ")

   usePackage("ggplot2")

   ggplot(mydata, aes(by0, y0)) +             # environment=.e
    geom_line(data=mydata, aes(by0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) +
      geom_line(data=mydata, aes(by0, f0), colour="red") +
       geom_point(data=mydata, aes(psy, p0), colour="red") +
        ylab(paste0(stockabundance)) +
         xlab(paste0("Brood Year")) +
          ggtitle(paste0(tmpage)) +
           facet_wrap(~psy) +
            theme_bw() +
             scale_y_continuous(labels=scales::comma) +
             theme(plot.title=element_text(size=12, hjust=0.5), 
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8),
                   axis.text.y = element_text(vjust = 0.5, size=8),
                   strip.text.x = element_text(size = 9))

}


SIMPLELOGPOWER$individual.ages.retro.plot.simplelogpower(
     SIMPLELOGPOWER$individual.ages.retro.plot.info.simplelogpower,
     SIMPLELOGPOWER$stockabundance,
     j=1)




##=========================================================================================================
## measures of retro performance - avgfive + simplelogpower
##=========================================================================================================

SIMPLELOGPOWER$total.age.retro.predictive.performance.avgfive.plus.simplelogpower.regression <- function(result.avgfive.youngest,
                                                                                           res.avgfive.simplelogpower.regression){


    result <- res.avgfive.simplelogpower.regression

    ## data.table(cy=cy00, a, p, e, p.bench, e.bench, key="cy")
    usePackage("data.table")
    res.avgfive.simplelogpower.regression <- vector("list",length(result)+1)

    res.avgfive.simplelogpower.regression[[1]] <- data.table(result.avgfive.youngest[[1]], key="cy")

    for (k in 1:(length(res.avgfive.simplelogpower.regression)-1)){
        res.avgfive.simplelogpower.regression[[k+1]] <- data.table(result[[k]],key="cy")
    }

    names(res.avgfive.simplelogpower.regression) <- c(names(result.avgfive.youngest),names(result))


    resjoin <- vector("list",length(res.avgfive.simplelogpower.regression ))
    for (j in 1:length(resjoin)){

     		  DT <- setkey(res.avgfive.simplelogpower.regression[[j]], "cy")
      		DT1 <- DT[J(res.avgfive.simplelogpower.regression[[1]]$cy)]
     		  resjoin[[j]] <- DT1[complete.cases(DT1),]
    }

    ## nms <- NULL
    ## for (j in 1:length(datalist)) {
    ##       nms <- c(nms, paste("Age_",str_extract(names(datalist[j]),"[[:digit:]]+"),sep=""))
    ## }

    ## names(resjoin) <- nms

	  names(resjoin) <- c(names(result.avgfive.youngest),names(result))

    a.total <- apply(sapply(resjoin, "[[", "a"),1,sum)
    p.total <- apply(sapply(resjoin, "[[", "p"),1,sum)
    e.total <- apply(sapply(resjoin, "[[", "e"),1,sum)
    p.bench.total <- apply(sapply(resjoin, "[[", "p.bench"),1,sum)
    e.bench.total <- apply(sapply(resjoin, "[[", "e.bench"),1,sum)


    mre.total <- mean(e.total)

    mae.total <- mean(abs(e.total))

    mpe.total <- mean(e.total/a.total)

    mape.total <- mean(abs(e.total)/a.total)

    num_mase <- mean(abs(e.total))
    ## denom_mase <- mean(abs(e))
    denom_mase <- mean(abs(e.bench.total))
    mase.total <- num_mase/denom_mase

    rmse.total <- sqrt(sum(e.total^2)/length(e.total))

    out <- list(method="avgfive-sibreg",
                resjoin=resjoin,
                 a.total=a.total,
                 p.total=p.total,
                 e.total=e.total,
                 p.bench.total=p.bench.total,
                 e.bench.total=e.bench.total,
                 mre.total=mre.total,
                 mae.total=mae.total,
                 mpe.total=mpe.total,
                 mape.total=mape.total,
                 mase.total=mase.total,
                 rmse.total=rmse.total)
    return(out)
}



## need these before call to total.age.retro.predictive.performance.avgfive.plus.simplelogpower.regression
SIMPLELOGPOWER$result.avgfive.youngest <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.avgfive.youngest(SIMPLELOGPOWER$datalist, SIMPLELOGPOWER$index.year)

SIMPLELOGPOWER$res.avgfive.simplelogpower.regression <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.simplelogpower.regression.youngest(
                                                                        SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                                                        SIMPLELOGPOWER$index.year)


## call to total.age.retro.predictive.performance.avgfive.plus.simplelogpower.regression
SIMPLELOGPOWER$rmse.results.youngest.age.avgfive.plus.simplelogpower.regression <-
SIMPLELOGPOWER$total.age.retro.predictive.performance.avgfive.plus.simplelogpower.regression(SIMPLELOGPOWER$result.avgfive.youngest,
                                                                                 SIMPLELOGPOWER$res.avgfive.simplelogpower.regression)



##=========================================================================================================
## measures of retro performance - arima + sibreg
##=========================================================================================================

SIMPLELOGPOWER$total.age.retro.predictive.performance.arima.plus.simplelogpower.regression <- function(result.arima.youngest,
                                                                                           res.arima.simplelogpower.regression){

    result <- res.arima.simplelogpower.regression

    ## data.table(cy=cy00, a, p, e, p.bench, e.bench, key="cy")
    usePackage("data.table")
    res.arima.simplelogpower.regression <- vector("list",length(result)+1)
    res.arima.simplelogpower.regression[[1]] <- data.table(result.arima.youngest[[1]], key="cy")
    for (k in 1:(length(res.arima.simplelogpower.regression)-1)){
        res.arima.simplelogpower.regression[[k+1]] <- data.table(result[[k]],key="cy")
    }

    names(res.arima.simplelogpower.regression) <- c(names(result.arima.youngest),names(result))


    resjoin <- vector("list",length(res.arima.simplelogpower.regression ))
    for (j in 1:length(resjoin)){

     		  DT <- setkey(res.arima.simplelogpower.regression[[j]], "cy")
      		DT1 <- DT[J(res.arima.simplelogpower.regression[[1]]$cy)]
     		  resjoin[[j]] <- DT1[complete.cases(DT1),]
    }

    ## nms <- NULL
    ## for (j in 1:length(datalist)) {
    ##      nms <- c(nms, paste("Age_",str_extract(names(datalist[j]),"[[:digit:]]+"),sep=""))
    ## }

    ## names(resjoin) <- nms

	  names(resjoin) <- c(names(result.arima.youngest), names(result))

    a.total <- apply(sapply(resjoin, "[[", "a"),1,sum)
    p.total <- apply(sapply(resjoin, "[[", "p"),1,sum)
    e.total <- apply(sapply(resjoin, "[[", "e"),1,sum)
    p.bench.total <- apply(sapply(resjoin, "[[", "p.bench"),1,sum)
    e.bench.total <- apply(sapply(resjoin, "[[", "e.bench"),1,sum)


    mre.total <- mean(e.total)

    mae.total <- mean(abs(e.total))

    mpe.total <- mean(e.total/a.total)

    mape.total <- mean(abs(e.total)/a.total)

    num_mase <- mean(abs(e.total))
    ## denom_mase <- mean(abs(e))
    denom_mase <- mean(abs(e.bench.total))
    mase.total <- num_mase/denom_mase

    rmse.total <- sqrt(sum(e.total^2)/length(e.total))

    out <- list(method="arima-simplelogpower",
                resjoin=resjoin,
                 a.total=a.total,
                 p.total=p.total,
                 e.total=e.total,
                 p.bench.total=p.bench.total,
                 e.bench.total=e.bench.total,
                 mre.total=mre.total,
                 mae.total=mae.total,
                 mpe.total=mpe.total,
                 mape.total=mape.total,
                 mase.total=mase.total,
                 rmse.total=rmse.total)
    return(out)
}



## need these before call to total.age.retro.predictive.performance.arima.plus.simplelogpower.regression

SIMPLELOGPOWER$result.arima.youngest <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.arima.youngest(
                                       datalist = SIMPLELOGPOWER$datalist,
                                       forecastingyear = SIMPLELOGPOWER$forecastingyear,
                                       boxcoxtransform = SIMPLELOGPOWER$boxcoxtransform,
                                       index = SIMPLELOGPOWER$index.year)


SIMPLELOGPOWER$res.arima.simplelogpower.regression <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.simplelogpower.regression.youngest(
                                                                        SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                                                        SIMPLELOGPOWER$index.year)


## call to total.age.retro.predictive.performance.arima.plus.simplelogpower.regression
SIMPLELOGPOWER$rmse.results.youngest.age.arima.plus.simplelogpower.regression <-
SIMPLELOGPOWER$total.age.retro.predictive.performance.arima.plus.simplelogpower.regression(
      SIMPLELOGPOWER$result.arima.youngest,
      SIMPLELOGPOWER$res.arima.simplelogpower.regression)


##=========================================================================================================
## measures of retro performance - expsmooth + simplelogpower
##=========================================================================================================

SIMPLELOGPOWER$total.age.retro.predictive.performance.expsmooth.plus.simplelogpower.regression <- function(result.expsmooth.youngest,
                                                                                           res.expsmooth.simplelogpower.regression){

    result <- res.expsmooth.simplelogpower.regression

    ## data.table(cy=cy00, a, p, e, p.bench, e.bench, key="cy")
    usePackage("data.table")
    res.expsmooth.simplelogpower.regression <- vector("list",length(result)+1)
    res.expsmooth.simplelogpower.regression[[1]] <- data.table(result.expsmooth.youngest[[1]], key="cy")
    for (k in 1:(length(res.expsmooth.simplelogpower.regression)-1)){
        res.expsmooth.simplelogpower.regression[[k+1]] <- data.table(result[[k]],key="cy")
    }

    names(res.expsmooth.simplelogpower.regression) <- c(names(result.expsmooth.youngest),names(result))


    resjoin <- vector("list",length(res.expsmooth.simplelogpower.regression ))
    for (j in 1:length(resjoin)){

     		  DT <- setkey(res.expsmooth.simplelogpower.regression[[j]], "cy")
      		DT1 <- DT[J(res.expsmooth.simplelogpower.regression[[1]]$cy)]
     		  resjoin[[j]] <- DT1[complete.cases(DT1),]
    }

    ## nms <- NULL
    ## for (j in 1:length(datalist)) {
    ##      nms <- c(nms, paste("Age_",str_extract(names(datalist[j]),"[[:digit:]]+"),sep=""))
    ## }

    ## names(resjoin) <- nms

	  names(resjoin) <- c(names(result.expsmooth.youngest), names(result))

    a.total <- apply(sapply(resjoin, "[[", "a"),1,sum)
    p.total <- apply(sapply(resjoin, "[[", "p"),1,sum)
    e.total <- apply(sapply(resjoin, "[[", "e"),1,sum)
    p.bench.total <- apply(sapply(resjoin, "[[", "p.bench"),1,sum)
    e.bench.total <- apply(sapply(resjoin, "[[", "e.bench"),1,sum)


    mre.total <- mean(e.total)

    mae.total <- mean(abs(e.total))

    mpe.total <- mean(e.total/a.total)

    mape.total <- mean(abs(e.total)/a.total)

    num_mase <- mean(abs(e.total))
    ## denom_mase <- mean(abs(e))
    denom_mase <- mean(abs(e.bench.total))
    mase.total <- num_mase/denom_mase

    rmse.total <- sqrt(sum(e.total^2)/length(e.total))

    out <- list(method="expsmooth-simplelogpower",
                resjoin=resjoin,
                 a.total=a.total,
                 p.total=p.total,
                 e.total=e.total,
                 p.bench.total=p.bench.total,
                 e.bench.total=e.bench.total,
                 mre.total=mre.total,
                 mae.total=mae.total,
                 mpe.total=mpe.total,
                 mape.total=mape.total,
                 mase.total=mase.total,
                 rmse.total=rmse.total)
    return(out)
}



## need these before call to total.age.retro.predictive.performance.expsmooth.plus.simplelogpower.regression

SIMPLELOGPOWER$result.expsmooth.youngest <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.expsmooth.youngest(
                                    datalist = SIMPLELOGPOWER$datalist,
                                    forecastingyear = SIMPLELOGPOWER$forecastingyear,
                                    boxcoxtransform = SIMPLELOGPOWER$boxcoxtransform,
                                    index =  SIMPLELOGPOWER$index.year)


SIMPLELOGPOWER$res.expsmooth.simplelogpower.regression <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.simplelogpower.regression.youngest(
                                                                        SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression,
                                                                        SIMPLELOGPOWER$index.year)

## call to total.age.retro.predictive.performance.expsmooth.plus.simplelogpower.regression
SIMPLELOGPOWER$rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression <-
SIMPLELOGPOWER$total.age.retro.predictive.performance.expsmooth.plus.simplelogpower.regression(
      SIMPLELOGPOWER$result.expsmooth.youngest,
      SIMPLELOGPOWER$res.expsmooth.simplelogpower.regression)




##=========================================================================================================
## choosing the measure of retro performance (i.e., rmse) which is "best" for forecasting total age
## among avgfive + simplelogpower, arima + simplelogpower, expsmooth + simplelogpower
##=========================================================================================================

SIMPLELOGPOWER$best.rmse.results.youngest.age.simplelogpower <- function(rmse.results.youngest.age.avgfive.plus.simplelogpower.regression,
                                           rmse.results.youngest.age.arima.plus.simplelogpower.regression,
                                           rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression){

   rmse.total.age.avgfive <- rmse.results.youngest.age.avgfive.plus.simplelogpower.regression$rmse.total
   rmse.total.age.arima  <- rmse.results.youngest.age.arima.plus.simplelogpower.regression$rmse.total
   rmse.total.age.expsmooth  <- rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression$rmse.total


   rmse.total.age <- c(rmse.total.age.avgfive, rmse.total.age.arima, rmse.total.age.expsmooth)

   min.rmse.total.age <- min(rmse.total.age)

   index.min.rmse.total.age <- which(rmse.total.age %in% sort(unique(rmse.total.age))[1])
   index.min.rmse.total.age <- as.numeric(index.min.rmse.total.age)

   if (index.min.rmse.total.age==1){
       simplelogpower.youngest.age.method <- "naive forecasting (i.e., average of previous five years)"
       output <- rmse.results.youngest.age.avgfive.plus.simplelogpower.regression
   } else if (index.min.rmse.total.age==2) {
       simplelogpower.youngest.age.method <- "ARIMA forecasting"
       output <- rmse.results.youngest.age.arima.plus.simplelogpower.regression
   } else if (index.min.rmse.total.age==3) {
       simplelogpower.youngest.age.method <- "exponential smoothing forecasting"
       output <- rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression
   }

   myoutput <- list(method = simplelogpower.youngest.age.method,
                    retro = output,
                    min.rmse.total.age = min.rmse.total.age,
                    index.min.rmse.total.age = index.min.rmse.total.age)

   myoutput

 }


SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower <-  SIMPLELOGPOWER$best.rmse.results.youngest.age.simplelogpower(
                                                          SIMPLELOGPOWER$rmse.results.youngest.age.avgfive.plus.simplelogpower.regression,
                                                          SIMPLELOGPOWER$rmse.results.youngest.age.arima.plus.simplelogpower.regression,
                                                          SIMPLELOGPOWER$rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression)

SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower


## rmse.results.youngest.age.avgfive.plus.simplelogpower.regression = SIMPLELOGPOWER$rmse.results.youngest.age.avgfive.plus.simplelogpower.regression

## rmse.results.youngest.age.arima.plus.simplelogpower.regression = SIMPLELOGPOWER$rmse.results.youngest.age.arima.plus.simplelogpower.regression

## rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression = SIMPLELOGPOWER$rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression



###
###  "Cool" plots for the youngest age
###

SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$method   # "naive forecasting (i.e., average of previous five years)"
                                # "ARIMA forecasting"
                                # "exponential smoothing forecasting"

## youngest.age.retro.plot.avgfive(youngest.age.retro.plot.info.avgfive, stockabundance)
## youngest.age.retro.plot.arima(youngest.age.retro.plot.info.arima, stockabundance)
## youngest.age.retro.plot.expsmooth(youngest.age.retro.plot.info.expsmooth, stockabundance)



###
### Density Plot of Retrospective Forecast Errors: Individual Ages
###

SIMPLELOGPOWER$plot.density.retrospective.forecast.errors.individual.ages.simplelogpower.regression <- function(best.rmse.youngest.age.simplelogpower, stockabundance){

    .e = environment()

    best.rmse.youngest.age <- best.rmse.youngest.age.simplelogpower

    errors.stacked <- NULL
    labels.stacked <- NULL

      for (j in 1:length(names(best.rmse.youngest.age.simplelogpower$retro$resjoin))){

           errors.stacked <- c(errors.stacked, best.rmse.youngest.age.simplelogpower$retro$resjoin[[j]]$e )
           mylabel <-  as.character(names(best.rmse.youngest.age.simplelogpower$retro$resjoin)[j])
           mylabel <- str_replace_all(mylabel,"_"," ")
           mylabel <- paste0(stockabundance, " at ", mylabel)
           labels.stacked <- c( labels.stacked, rep(mylabel, length(best.rmse.youngest.age.simplelogpower$retro$resjoin[[j]]$e)))

          }

    data.stacked <- data.frame(errors=errors.stacked,
                               labels=labels.stacked)

    require(plyr)

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)

    ## breaks <- NULL
    ## for (j in 1:length(names(best.rmse.youngest.age.simplelogpower$retro$resjoin))){
    ##    h <- hist(data.stacked$errors[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE)
    ##    breaks[[j]] <- h$breaks
    ## }

    #  environment=.e
    g <- ggplot(d, aes(x=errors), environment=.e) +
           geom_density(data=d,fill="lightblue",colour="black") +
             facet_wrap(~ labels,  scales="free", ncol=1) +
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
     for (j in 1:length(names(best.rmse.youngest.age.simplelogpower$retro$resjoin))){

        clim <- c(clim, 0)

     }

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(aes(xintercept = z), dummy2, linetype="dashed",col="red", size=0.8)


     return(g)
}


SIMPLELOGPOWER$plot.density.retrospective.forecast.errors.individual.ages.simplelogpower.regression(
      SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower,
      SIMPLELOGPOWER$stockabundance)





###
### Density Plot of Retrospective Forecast Errors: Total Age
###

SIMPLELOGPOWER$plot.density.retrospective.forecast.errors.total.age.simplelogpower.regression <- function(best.rmse.youngest.age.simplelogpower, stockabundance){

    .e = environment()

    best.rmse.youngest.age <- best.rmse.youngest.age.simplelogpower

    errors.stacked <- best.rmse.youngest.age$retro$e.total
    mylabel <- paste0("Total ",stockabundance)
    ## labels.stacked <- rep(mylabel, length(best.rmse.youngest.age$retro$resjoin[[j]]$e))
    labels.stacked <- rep(mylabel, length(errors.stacked))


    data.stacked <- data.frame(errors=errors.stacked,
                               labels=labels.stacked)

    require(plyr)

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)

    ## h <- hist(data.stacked$errors,plot=FALSE)
    ## breaks <- h$breaks


    #  environment=.e
    g <- ggplot(d, aes(x=errors), environment=.e) +
           geom_density(data=d, fill="lightblue", colour="black") +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Retrospective Forecast Errors"),labels=comma)  +
                    ggtitle(paste(mylabel))

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     clim <- 0

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(aes(xintercept = z), dummy2, linetype="dashed",col="red", size=0.8)


     return(g)
}


SIMPLELOGPOWER$plot.density.retrospective.forecast.errors.total.age.simplelogpower.regression(
     SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower,
     SIMPLELOGPOWER$stockabundance)





################################################################################

SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower

## best.rmse.youngest.age <-  best.rmse.results.youngest.age(rmse.results.youngest.age.avgfive.plus.simple.sibling.regression,
##                                                          rmse.results.youngest.age.arima.plus.simple.sibling.regression,
##                                                          rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression)
##
## best.rmse.youngest.age




#*******************************************************************************
#
# Gary's Plot for Individual Ages: Youngest + Older
#
#*******************************************************************************

SIMPLELOGPOWER$gary.plot.individual.ages.simplelogpower.regression <- function(best.rmse.youngest.age.simplelogpower,
                                                                 pred.int.individual.ages.avgfive.youngest,   # youngest age prediction: avgfive
                                                                 pred.int.individual.ages.arima.youngest,     # youngest age prediction: arima
                                                                 pred.int.individual.ages.expsmooth.youngest, # youngest age prediction: expsmooth
                                                                 pred.int.individual.ages.simplelogpower.regression, # older ages prediction: log power regression (best model)
                                                                 forecastingyear, j){

       ## start from here

       .e <- environment()

       best.rmse.youngest.age <- best.rmse.youngest.age.simplelogpower

       results.retro <- best.rmse.youngest.age$retro$resjoin

       if (j==1) {

          model.youngest <- best.rmse.youngest.age$method  # model classed used for forecasting abundance for youngest age

          if (model.youngest=="naive forecasting (i.e., average of previous five years)"){
            method.youngest <- "Naive (Average of Previous Five Years)"
            pred.youngest <- data.frame(PI.ctr=round(pred.int.individual.ages.avgfive.youngest$PI.ctr),
                                     PI.lwr=pred.int.individual.ages.avgfive.youngest$PI.lwr,
                                     PI.upr=pred.int.individual.ages.avgfive.youngest$PI.upr)

          } else if (model.youngest=="ARIMA forecasting") {
            method.youngest <- "ARIMA"
            pred.youngest <- data.frame(PI.ctr=round(pred.int.individual.ages.arima.youngest$PI.ctr),
                                     PI.lwr=pred.int.individual.ages.arima.youngest$PI.lwr,
                                     PI.upr=pred.int.individual.ages.arima.youngest$PI.upr)
          } else if (model.youngest=="exponential smoothing forecasting") {
            method.youngest <- "Exponential Smoothing"
            pred.youngest <-  data.frame(PI.ctr=round(pred.int.individual.ages.expsmooth.youngest$PI.ctr),
                                     PI.lwr=pred.int.individual.ages.expsmooth.youngest$PI.lwr,
                                     PI.upr=pred.int.individual.ages.expsmooth.youngest$PI.upr)
          }

          pred <- pred.youngest

          retro <- data.frame(results.retro[[j]])

          age <- names(results.retro)[j]
          usePackage("stringr")

          age <- str_replace(age, "_"," ")

          age

          ## numeric_age <- as.numeric(gsub("[^\\d]+", "", age, perl=TRUE))

          ## years <- retro$data.retro$by + numeric_age

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

       } # end for (youngest age)

       if (j>1){

          method.oldest <- "Sibling Regression (Best Model)"

          pred.oldest <- pred.int.individual.ages.simplelogpower.regression[[j-1]]
          pred <- data.frame(PI.ctr = round(pred.oldest$p),
                             PI.lwr = round(pred.oldest$p.lwr),
                             PI.upr = round(pred.oldest$p.upr))

          retro <- data.frame(results.retro[[j]])

          age <- names(results.retro)[j]
          usePackage("stringr")

          age <- str_replace(age, "_"," ")

          age

          ## numeric_age <- as.numeric(gsub("[^\\d]+", "", age, perl=TRUE))

          ## years <- retro$data.retro$by + numeric_age

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


       }  # end for (older ages)


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
        labs(title=paste(SIMPLELOGPOWER$stockabundance,"at",age)) +
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
        labs(title=paste(SIMPLELOGPOWER$stockabundance,"at",age)) +
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
        labs(title=paste(SIMPLELOGPOWER$stockabundance, "at",age)) +
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
        labs(title=paste(SIMPLELOGPOWER$stockabundance, "at",age)) +
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


} # end gary.plot.simplelogpower.regression for individual ages


## COME BACK HERE - November 4, 2014

## j <- 3

SIMPLELOGPOWER$gary.plot.individual.ages.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower,
                                                       SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest,   # youngest age prediction: avgfive
                                                       SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest,     # youngest age prediction: arima
                                                       SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest, # youngest age prediction: expsmooth
                                                       SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression, # older ages prediction: log power regression (best model)
                                                       SIMPLELOGPOWER$forecastingyear,
                                                       j=3)




#*******************************************************************************
#
# Gary's Plot for Total Age:
#
#*******************************************************************************


SIMPLELOGPOWER$gary.plot.total.age.simplelogpower.regression <- function(best.rmse.youngest.age.simplelogpower,
                                                    pred.int.total.age.simplelogpower.regression.all.models,  # total age prediction
                                                    forecastingyear){

      ## start from here

      .e <- environment()

      best.rmse.youngest.age <- best.rmse.youngest.age.simplelogpower

      results.retro <- data.frame(cy = best.rmse.youngest.age$retro$resjoin[[1]]$cy,
                                   a   = best.rmse.youngest.age$retro$a.total,
                                   p   = best.rmse.youngest.age$retro$p.total,
                                   e   = best.rmse.youngest.age$retro$e.total)



      model.youngest <- best.rmse.youngest.age$method  # model classed used for forecasting abundance for youngest age

      if (model.youngest=="naive forecasting (i.e., average of previous five years)"){
            method.youngest <- "Naive (Average of Previous Five Years)"
            pred.total <- data.frame(PI.ctr=pred.int.total.age.simplelogpower.regression.all.models$p$avgfive,
                                     PI.lwr=pred.int.total.age.simplelogpower.regression.all.models$p.lwr$avgfive,
                                     PI.upr=pred.int.total.age.simplelogpower.regression.all.models$p.upr$avgfive)

        } else if (model.youngest=="ARIMA forecasting") {
            method.youngest <- "ARIMA"
            pred.total <- data.frame(PI.ctr=pred.int.total.age.simplelogpower.regression.all.models$p$arima,
                                     PI.lwr=pred.int.total.age.simplelogpower.regression.all.models$p.lwr$arima,
                                     PI.upr=pred.int.total.age.simplelogpower.regression.all.models$p.upr$arima)
        } else if (model.youngest=="exponential smoothing forecasting") {
            method.youngest <- "Exponential Smoothing"
            pred.total <-  data.frame(PI.ctr=pred.int.total.age.simplelogpower.regression.all.models$p$expsmooth,
                                     PI.lwr=pred.int.total.age.simplelogpower.regression.all.models$p.lwr$expsmooth,
                                     PI.upr=pred.int.total.age.simplelogpower.regression.all.models$p.upr$expsmooth)
                                     
            ## SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$p.upr
                                     
      }

      pred <- pred.total

      retro <- results.retro

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
        labs(title=paste("Total", SIMPLELOGPOWER$stockabundance)) +
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
        labs(title=paste("Total", stockabundance)) +
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
        labs(title=paste("Total", stockabundance)) +
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
        labs(title=paste("Total", stockabundance)) +
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


} # end gary.plot.simplelogpower.regression for total age



SIMPLELOGPOWER$gary.plot.total.age.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower,
                                                 SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models,  # total age prediction
                                                 SIMPLELOGPOWER$forecastingyear)




#===============================================================================================================================

##
## Table of results for candidate models for youngest age: avgfive, arima, expsmooth
##


SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest
SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest
SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest


SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest <- list()
SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$Model <- c("Naive (Average of Previous Five Years)",
                                                             "ARIMA",
                                                             "Exponential Smoothing")

SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.ctr <- c( round(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.ctr),
                                                                      round(SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.ctr),
                                                                      round(SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.ctr))

SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.lwr <- c( round(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.lwr),
                                                                     round(SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.lwr),
                                                                     round(SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.lwr))
SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.lwr <- as.numeric(SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.lwr)

SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.upr <- c(round(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.upr),
                                                               round(SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.upr),
                                                               round(SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.upr))
SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.upr <- as.numeric(SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.upr)

SIMPLELOGPOWER$rmse.total.age.avgfive <- SIMPLELOGPOWER$rmse.results.youngest.age.avgfive.plus.simplelogpower.regression$rmse.total
SIMPLELOGPOWER$rmse.total.age.arima  <- SIMPLELOGPOWER$rmse.results.youngest.age.arima.plus.simplelogpower.regression$rmse.total
SIMPLELOGPOWER$rmse.total.age.expsmooth  <- SIMPLELOGPOWER$rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression$rmse.total

SIMPLELOGPOWER$rmse.total.age <- c(SIMPLELOGPOWER$rmse.total.age.avgfive, SIMPLELOGPOWER$rmse.total.age.arima, SIMPLELOGPOWER$rmse.total.age.expsmooth)

SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$rmse.total.age <- SIMPLELOGPOWER$rmse.total.age  ## recall that this rmse is optimized for
                                                                                    ## prediction of total age!!!


SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest <- do.call(cbind.data.frame, SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest)

usePackage("scales")
SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.ctr <- comma(SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.ctr)
SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.lwr <- comma(SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.lwr)
SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.upr <- comma(SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.upr)

SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.int <- paste0(SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.lwr,
                                                                   " - ",
                                                                   SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest$PI.upr)

SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest <- subset(SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest,
                                                            select=c(Model, PI.ctr,  PI.int, rmse.total.age))
names(SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest) <-  c("Model", "Point Forecast", "Interval Forecast", "RMSE")

SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest



#=====================================================================================================================================
#
# Best RMSE
#
#######################################################################################################################################


SIMPLELOGPOWER$best.rmse.youngest.age <-  SIMPLELOGPOWER$best.rmse.results.youngest.age.simplelogpower(SIMPLELOGPOWER$rmse.results.youngest.age.avgfive.plus.simplelogpower.regression,
                                                          SIMPLELOGPOWER$rmse.results.youngest.age.arima.plus.simplelogpower.regression,
                                                          SIMPLELOGPOWER$rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression)

SIMPLELOGPOWER$total.index.simplelogpower <- SIMPLELOGPOWER$best.rmse.youngest.age$index.min.rmse.total.age



#========================================================================================================================================

###
### plot forecast vs. actual (individual ages, log power regression)
###

SIMPLELOGPOWER$plot.results.afe.individual.ages.retro.simplelogpower.regression <- function(best.rmse.youngest.age.simplelogpower){

    .e = environment()

    best.rmse.youngest.age <- best.rmse.youngest.age.simplelogpower

    forecasted.stacked <- NULL
    actual.stacked <- NULL
    age.stacked <- NULL
    labels.stacked <- NULL

    ## R.squared <- NULL

    for (i in 1:length(best.rmse.youngest.age$retro$resjoin)){

       usePackage("stringr")

       datatmp <- best.rmse.youngest.age$retro$resjoin[[i]]
       datatmp <- as.data.frame(datatmp)

       print(names(datatmp))

       names(datatmp)[names(datatmp)=="a"] <- "Actual"
       names(datatmp)[names(datatmp)=="p"] <- "Forecast"
       names(datatmp)[names(datatmp)=="e"] <- "Error"

       ## r.sq <- summary(lm(Forecast ~ Actual, data=datatmp))$r.squared
       ## r.sq <-  sprintf("%.2f", r.sq*100)

       ## R.squared <- c(R.squared, r.sq)

       mytitle <- names(best.rmse.youngest.age$retro$resjoin)[i]

       myage <- as.numeric(str_extract(mytitle,"[[:digit:]]+"))

       usePackage("stringr")
       mytitle <- str_replace_all(mytitle, pattern="_", replacement=" ")
       ## mytitle <- substr(mytitle, start=1, stop=5)

	     ## mytitle <- paste(mytitle, ":  ","R-squared = ", r.sq , "%", sep="")

    	 usePackage("calibrate")
    	 labs <- substr(datatmp$cy,  # return year (or, equivalently, calendar year)
                      ## datatmp$cy - myage ,   # brood year
              start=3, stop=4)



       forecasted.stacked <- c(forecasted.stacked, datatmp$Forecast)
       actual.stacked <- c(actual.stacked, datatmp$Actual)
       age.stacked <- c(age.stacked, rep(mytitle, length(datatmp$Actual)))
       labels.stacked <- c(labels.stacked, labs)

    }

    data.stacked <- data.frame(forecasted=forecasted.stacked,
                               actual=actual.stacked,
                               age=age.stacked,
                               labels=labels.stacked)



    usePackage("ggplot2")
    usePackage("scales")

    ## environment=.e
    g <- ggplot(data.stacked, aes(actual,forecasted), environment=.e)  +
      ## g <- ggplot(data.stacked, aes(actual,forecasted)) +
       geom_abline(intercept=0,slope=1,colour="red",size=0.8) +
       geom_text(aes(label=labels),col="blue",size=3) +
            coord_fixed(ratio=1) +
              facet_wrap(~age, scales="free",ncol=2) +
                expand_limits(x=0, y=0) +
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma) +
                   scale_y_continuous(paste("Forecasted", SIMPLELOGPOWER$stockabundance, "Values"),labels=comma) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous(paste("Actual", SIMPLELOGPOWER$stockabundance, "Values"),labels=comma) +
                   coord_fixed(ratio=1)

      ## print(g)

      # to expand plot axes, build the forecasted ranges in a data.frame and add a new layer to the plot using geom_blank

      # pick an actual value in the range of your actual values in data.stacked
      actual.pick = 0

      forecasted.pick.1 <- as.numeric(by(data.stacked$forecasted,data.stacked$age,max))
      forecasted.pick.2 <- as.numeric(by(data.stacked$actual,data.stacked$age,max))
      forecasted.pick.12 <- rbind(forecasted.pick.1,forecasted.pick.2)
      forecasted.pick <- apply(forecasted.pick.12,2,max)


      # Say you want to expand the y-axis ranges for the different subpanels to be (-5, 5), (-4, 4), (-2, 2).
      # If you simply plot at this point the y limits are roughly ~(-1.5, 1.5) for each plot
      upper_y = data.frame(actual = 0, forecasted = forecasted.pick,  age = levels(data.stacked$age))
      lower_y = data.frame(actual = 0, forecasted = rep(0,nlevels(data.stacked$age)),  age = levels(data.stacked$age))

      y_ranges = rbind(lower_y, upper_y)

      ## print(y_ranges)

      usePackage("reshape")
      y_ranges_m = melt(y_ranges, id.vars = c("actual", "age"))

      g = g + geom_blank(data = y_ranges_m, aes(x = actual, y = value))


      ##

      actual.pick <- forecasted.pick
      forecasted.pick <- 0

      upper_x = data.frame(actual = actual.pick, forecasted = forecasted.pick,  age = levels(data.stacked$age))
      lower_x = data.frame(actual = rep(0,nlevels(data.stacked$age)), forecasted = forecasted.pick,  age = levels(data.stacked$age))

      x_ranges = rbind(lower_x, upper_x)

      ## print(x_ranges)
      usePackage("reshape")
      x_ranges_m = melt(x_ranges, id.vars = c("forecasted", "age"))

      g = g + geom_blank(data = x_ranges_m, aes(x = value,y = forecasted))

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8))


      return(g)

}




SIMPLELOGPOWER$plot.results.afe.individual.ages.retro.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower)


#==============================================================================================================================

###
### plot forecasted vs. actual (total age, log power regression)
###

SIMPLELOGPOWER$plot.results.afe.total.age.retro.simplelogpower.regression <- function(best.rmse.youngest.age.simplelogpower, stockabundance){

    .e = environment()

    best.rmse.youngest.age <- best.rmse.youngest.age.simplelogpower

    usePackage("stringr")

    datatmp <- data.frame(cy = best.rmse.youngest.age$retro$resjoin[[1]]$cy,
                          a = best.rmse.youngest.age$retro$a.total,
                          p = best.rmse.youngest.age$retro$p.total,
                          e = best.rmse.youngest.age$retro$e.total
                          )

    names(datatmp)[names(datatmp)=="a"] <- "Actual"
    names(datatmp)[names(datatmp)=="p"] <- "Forecast"
    names(datatmp)[names(datatmp)=="e"] <- "Error"

    ## r.sq <- summary(lm(Forecast ~ Actual, data=datatmp))$r.squared
    ## r.sq <-  sprintf("%.2f", r.sq*100)

    ## R.squared <- c(R.squared, r.sq)

    ## myage <- as.numeric(str_extract(mytitle,"[[:digit:]]+"))

    usePackage("stringr")
    ## mytitle <- str_replace_all(mytitle, pattern="_", replacement=" ")
    ## mytitle <- substr(mytitle, start=1, stop=5)

    mytitle <- paste0("Total ", SIMPLELOGPOWER$stockabundance)

    ## mytitle <- paste(mytitle, ": ","R-squared = ", r.sq , "%", sep="")

    usePackage("calibrate")
    labs <- substr(datatmp$cy ,
                   start=3, stop=4)

    forecasted.stacked <- datatmp$Forecast
    actual.stacked <- datatmp$Actual
    age.stacked <- rep(mytitle, length(datatmp$Actual))
    labels.stacked <- labs

    data.stacked <- data.frame(forecasted=forecasted.stacked,
                               actual=actual.stacked,
                               labels=labels.stacked,
                               age = age.stacked)

    usePackage("ggplot2")
    usePackage("scales")

    ## environment=.e
    g <- ggplot(data.stacked, aes(actual,forecasted), environment=.e)  +
      ## g <- ggplot(data.stacked, aes(actual,forecasted)) +
       geom_abline(intercept=0,slope=1,colour="red",size=0.8) +
       geom_text(aes(label=labels),col="blue",size=3) +
            coord_fixed(ratio=1) +
              facet_wrap(~age, scales="free",ncol=2) +
                expand_limits(x=0, y=0) +
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma) +
                   scale_y_continuous(paste("Forecasted Total", SIMPLELOGPOWER$stockabundance, "Values"),labels=comma) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous(paste("Actual Total", SIMPLELOGPOWER$stockabundance, "Values"),labels=comma) +
                   coord_fixed(ratio=1)

    ## print(g)

    # to expand plot axes, build the forecasted ranges in a data.frame and add a new layer to the plot using geom_blank

    # pick an actual value in the range of your actual values in data.stacked
    actual.pick = 0

    forecasted.pick.1 <- as.numeric(by(data.stacked$forecasted,data.stacked$age,max))
    forecasted.pick.2 <- as.numeric(by(data.stacked$actual,data.stacked$age,max))
    forecasted.pick.12 <- rbind(forecasted.pick.1,forecasted.pick.2)
    forecasted.pick <- apply(forecasted.pick.12,2,max)


    # Say you want to expand the y-axis ranges for the different subpanels to be (-5, 5), (-4, 4), (-2, 2).
    # If you simply plot at this point the y limits are roughly ~(-1.5, 1.5) for each plot
    upper_y = data.frame(actual = 0, forecasted = forecasted.pick,  age = levels(data.stacked$age))
    lower_y = data.frame(actual = 0, forecasted = rep(0,nlevels(data.stacked$age)),  age = levels(data.stacked$age))

    y_ranges = rbind(lower_y, upper_y)

    ## print(y_ranges)

    usePackage("reshape")
    y_ranges_m = melt(y_ranges, id.vars = c("actual", "age"))

    g = g + geom_blank(data = y_ranges_m, aes(x = actual, y = value))

    actual.pick <- forecasted.pick
    forecasted.pick <- 0

    upper_x = data.frame(actual = actual.pick, forecasted = forecasted.pick,  age = levels(data.stacked$age))
    lower_x = data.frame(actual = rep(0,nlevels(data.stacked$age)), forecasted = forecasted.pick,  age = levels(data.stacked$age))

    x_ranges = rbind(lower_x, upper_x)

    ## print(x_ranges)
    usePackage("reshape")
    x_ranges_m = melt(x_ranges, id.vars = c("forecasted", "age"))

    g = g + geom_blank(data = x_ranges_m, aes(x = value,y = forecasted))

    g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5), 
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8))


    return(g)

}

SIMPLELOGPOWER$plot.results.afe.total.age.retro.simplelogpower.regression(
    SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower, stockabundance)




cat("RETRO - Sibling Regression.R", "\n\n")


## need to refresh the datalist object, as it currently contains only the youngest age!!!

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
     } else if (stockabundance=="Production") {
      SIMPLESIBREG$tmpsub[[i]] <- subset(SIMPLESIBREG$datafilesub, Age_Class==SIMPLESIBREG$extract_ages[i])[,c("Brood_Year","Average_Production")]
     }
}

SIMPLESIBREG$list.of.data.frames <- SIMPLESIBREG$tmpsub
SIMPLESIBREG$merged.data.frame = Reduce(function(...) merge(...,by="Brood_Year", all=T), SIMPLESIBREG$list.of.data.frames)

SIMPLESIBREG$datafile_new <- SIMPLESIBREG$merged.data.frame
names(SIMPLESIBREG$datafile_new) <- SIMPLESIBREG$extract_names

## SIMPLESIBREG$datafile <<- SIMPLESIBREG$datafile_new

SIMPLESIBREG$datafile <- SIMPLESIBREG$datafile_new

SIMPLESIBREG$datalist <- SIMPLESIBREG$datalist.avgfive(SIMPLESIBREG$datafile, SIMPLESIBREG$forecastingyear)  # CY refers to the T variable with highest age

##========================================================================================================
## youngest age: avgfive
##========================================================================================================

SIMPLESIBREG$individual.ages.retro.predictive.performance.avgfive.youngest <- function(datalist, index){

      datalist <- datalist

      ## index <- 10

      PSY <- SIMPLESIBREG$forecastingyear

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
           
           model0 <- SIMPLESIBREG$avgfive.youngest(y0)
           
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
        
        SIMPLESIBREG$youngest.age.retro.plot.info.avgfive <<- list(data0=data0,age0 = paste0("Age_",youngestage)) 
        
        
        result.avgfive.youngest.output


}


SIMPLESIBREG$index.year <- index.year

SIMPLESIBREG$result.avgfive.youngest <- SIMPLESIBREG$individual.ages.retro.predictive.performance.avgfive.youngest(SIMPLESIBREG$datalist, SIMPLESIBREG$index.year)

SIMPLESIBREG$youngest.age.retro.plot.info.avgfive



SIMPLESIBREG$youngest.age.retro.plot.avgfive <- function(youngest.age.retro.plot.info.avgfive, stockabundance){

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
             theme(plot.title=element_text(hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}

SIMPLESIBREG$youngest.age.retro.plot.avgfive(SIMPLESIBREG$youngest.age.retro.plot.info.avgfive, SIMPLESIBREG$stockabundance)


##========================================================================================================
## youngest age: arima
##========================================================================================================

SIMPLESIBREG$datalist <- SIMPLESIBREG$datalist.arima(SIMPLESIBREG$datafile, SIMPLESIBREG$forecastingyear)  # CY refers to the T variable with highest age


SIMPLESIBREG$arima.model <- function(datalist, boxcoxtransform){

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


## arima.model.fits  <- SIMPLESIBREG$arima.model(SIMPLESIBREG$datalist, SIMPLESIBREG$boxcoxtransform)

SIMPLESIBREG$individual.ages.retro.predictive.performance.arima.youngest <- function(datalist, forecastingyear, boxcoxtransform, index){

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
        
    SIMPLESIBREG$youngest.age.retro.plot.info.arima <<- list(data0=data0, age0 = paste0("Age_",youngestage))     
        
    result.arima.youngest.output

                           
}


SIMPLESIBREG$result.arima.simple.sibling.regression <- SIMPLESIBREG$individual.ages.retro.predictive.performance.arima.youngest(SIMPLESIBREG$datalist, 
                                                          SIMPLESIBREG$forecastingyear, 
                                                          SIMPLESIBREG$boxcoxtransform, 
                                                          SIMPLESIBREG$index.year)



SIMPLESIBREG$youngest.age.retro.plot.arima <- function(youngest.age.retro.plot.info.arima, stockabundance){

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
        ylab(paste0(SIMPLESIBREG$stockabundance)) + 
         xlab(paste0("Return Year")) + 
          ggtitle(label=paste0(tmpage)) + 
           facet_wrap(~psy) + 
            theme_bw() + 
             scale_y_continuous(labels=scales::comma) + 
             theme(plot.title=element_text(hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}


SIMPLESIBREG$youngest.age.retro.plot.arima(SIMPLESIBREG$youngest.age.retro.plot.info.arima, stockabundance)


##========================================================================================================
## youngest age: expsmooth
##========================================================================================================

SIMPLESIBREG$datalist <- SIMPLESIBREG$datalist.expsmooth(SIMPLESIBREG$datafile, SIMPLESIBREG$forecastingyear)


#---------  fit exponential smoothing model -----------------------------------------


SIMPLESIBREG$expsmooth.model <- function(datalist, boxcoxtransform){

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


## SIMPLESIBREG$expsmooth.model.fits  <- SIMPLESIBREG$expsmooth.model(SIMPLESIBREG$datalist, SIMPLESIBREG$boxcoxtransform)
 

SIMPLESIBREG$individual.ages.retro.predictive.performance.expsmooth.youngest <- function(datalist, forecastingyear, boxcoxtransform, index){

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

    SIMPLESIBREG$youngest.age.retro.plot.info.expsmooth <<- list(data0=data0,age0 = paste0("Age_",youngestage))

    result.expsmooth.youngest.output

}


SIMPLESIBREG$result.expsmooth.simple.sibling.regression <- SIMPLESIBREG$individual.ages.retro.predictive.performance.expsmooth.youngest(
                                                               SIMPLESIBREG$datalist, SIMPLESIBREG$forecastingyear, SIMPLESIBREG$boxcoxtransform, SIMPLESIBREG$index.year)


SIMPLESIBREG$youngest.age.retro.plot.expsmooth <- function(youngest.age.retro.plot.info.expsmooth, stockabundance){

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
             theme(plot.title=element_text(hjust=0.5), 
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}


SIMPLESIBREG$youngest.age.retro.plot.expsmooth(SIMPLESIBREG$youngest.age.retro.plot.info.expsmooth, SIMPLESIBREG$stockabundance)




##=========================================================================================================
## simple sibling regression
##=========================================================================================================

SIMPLESIBREG$individual.ages.retro.predictive.performance.simple.sibling.regression.youngest <- function(pred.int.individual.ages.simple.sibling.regression, index){ 

     datalist.simple.sibling.regression <- pred.int.individual.ages.simple.sibling.regression

     ## index <- 10

     ## PSY <- forecastingyear    ## Come back here to make sure the forecasting year is specified correctly!

     result <- list()

     nms <- NULL

     SIMPLESIBREG$individual.ages.retro.plot.info.simplesib <<- list()

     for (j in 1:length(datalist.simple.sibling.regression)){

          ## subdata <- subset(datalist[[j]]$model.data, CY < PSY)
          subdata <- datalist.simple.sibling.regression[[j]]$model.data
          submodelformula <- as.formula(datalist.simple.sibling.regression[[j]]$model.formula)

          usePackage("formula.tools")

          outcome <- lhs.vars(submodelformula)
          predictors <- rhs.vars(submodelformula)

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
               by0 <- subdata0[ ,"Brood_Year"]

               model0 <- lm(submodelformula, data=subdata0)

               # model0 <- arima(y0, order = c(p.0, d.0, q.0))

               newdata0 = subset(subdata,select=predictors)
               newdata0 = subset(newdata0, rownames(newdata0)==(i+1))

               ## p0 <- as.numeric(forecast(model0, h=1, level=80)$mean)
               p0 <- predict(model0, newdata=newdata0, interval="prediction")[1]
               p0 <- round(p0)

               f0 <- as.numeric(fitted(model0))
               
               #-----
               
               d0 <- data.frame(i=i, by0, y0, f0, p0, psy=max(by0)+1, a0=y[i+1])   # note replacement of cy0 (calendar year) 
                                                                                   # with by0 (brood year)

               ## str(d0)
               
               data0 <- rbind.data.frame(data0, d0)  
              
               #-----

               p <- c(p, p0)
               e0 <- y[i+1] - p0   # actual - predicted
               e <- c(e, e0)
               a <- c(a, y[i+1]) #actual

               by00 <- c(by00, subdata[i+1,"Brood_Year"])

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

          nms <- c(nms,outcome)

          result[[j]] <- data.frame(by=by00, a, p, e, p.bench, e.bench)
          
          SIMPLESIBREG$individual.ages.retro.plot.info.simplesib[[j]] <<- data0


     } # end for j loop

     nms

     names(result) <- nms

     names(SIMPLESIBREG$individual.ages.retro.plot.info.simplesib) <<- nms

     for (j in 1:length(datalist.simple.sibling.regression)){
     result[[j]]$cy <- result[[j]]$by + as.numeric(gsub("[^\\d]+", "", nms[j], perl=TRUE))
     }
      
     
    ### combine all of the results: result.avgfive.youngest & result
    result

}


SIMPLESIBREG$result.simple.sibling.regression <- SIMPLESIBREG$individual.ages.retro.predictive.performance.simple.sibling.regression.youngest(
                                                     SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression, 
                                                     SIMPLESIBREG$index.year)


SIMPLESIBREG$individual.ages.retro.plot.simplesib <- function(individual.ages.retro.plot.info.simplesib, stockabundance, j){

   .e <- environment()

   mydata <- individual.ages.retro.plot.info.simplesib[[j]]

   ## mydata$psy <- factor(mydata$psy)


   tmpage <-  names(individual.ages.retro.plot.info.simplesib)[j]

   tmpage <- str_replace(tmpage, "_", " ")

   usePackage("ggplot2")

   ggplot(mydata, aes(by0, y0)) +             # environment=.e
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
             theme(plot.title=element_text(hjust=0.5), 
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}


SIMPLESIBREG$individual.ages.retro.plot.simplesib(SIMPLESIBREG$individual.ages.retro.plot.info.simplesib, SIMPLESIBREG$stockabundance, j=1)


##=========================================================================================================
## measures of retro performance - avgfive + sibreg
##=========================================================================================================

SIMPLESIBREG$total.age.retro.predictive.performance.avgfive.plus.simple.sibling.regression <- function(result.avgfive.youngest, 
                                                                                           result.simple.sibling.regression){

    result <- result.simple.sibling.regression

    ## data.table(cy=cy00, a, p, e, p.bench, e.bench, key="cy")
    usePackage("data.table")
    
    res.avgfive.simple.sibling.regression <- vector("list",length(result)+1)
    
    
    res.avgfive.simple.sibling.regression[[1]] <- data.table(result.avgfive.youngest[[1]], key="cy")
    
    ## for (k in 1:(length(res.avgfive.simple.sibling.regression)-1)){
    for (k in 1:(length(res.avgfive.simple.sibling.regression)-1)){
        res.avgfive.simple.sibling.regression[[k+1]] <- data.table(result[[k]],key="cy")
    }

    names(res.avgfive.simple.sibling.regression) <- c(names(result.avgfive.youngest),names(result))
    

    resjoin <- vector("list",length(res.avgfive.simple.sibling.regression ))
    
    for (j in 1:length(resjoin)){

     		  DT <- setkey(res.avgfive.simple.sibling.regression[[j]], "cy")
      		DT1 <- DT[J(res.avgfive.simple.sibling.regression[[1]]$cy)]
     		  resjoin[[j]] <- DT1[complete.cases(DT1),]
    }

    ## nms <- NULL
    ## for (j in 1:length(datalist)) {
    ##      nms <- c(nms, paste("Age_",str_extract(names(datalist[j]),"[[:digit:]]+"),sep=""))
    ## }
                                   
    ## names(resjoin) <- names(res.avgfive.simple.sibling.regression)
	    
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



## need these before call to total.age.retro.predictive.performance.avgfive.plus.simple.sibling.regression
SIMPLESIBREG$result.avgfive.simple.sibling.regression <- SIMPLESIBREG$individual.ages.retro.predictive.performance.simple.sibling.regression.youngest(
                                                                        SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression, index.year)
## call to total.age.retro.predictive.performance.avgfive.plus.simple.sibling.regression
SIMPLESIBREG$rmse.results.youngest.age.avgfive.plus.simple.sibling.regression <- 
SIMPLESIBREG$total.age.retro.predictive.performance.avgfive.plus.simple.sibling.regression(SIMPLESIBREG$result.avgfive.youngest, 
                                                                               SIMPLESIBREG$result.simple.sibling.regression)




##=========================================================================================================
## measures of retro performance - arima + sibreg
##=========================================================================================================


SIMPLESIBREG$total.age.retro.predictive.performance.arima.plus.simple.sibling.regression <- function(result.arima.youngest, 
                                                                                         result.simple.sibling.regression){

    result <- result.simple.sibling.regression

    ## data.table(cy=cy00, a, p, e, p.bench, e.bench, key="cy")
    usePackage("data.table")
    res.arima.simple.sibling.regression <- vector("list",length(result)+1)
    res.arima.simple.sibling.regression[[1]] <- data.table(result.arima.youngest[[1]], key="cy")
    for (k in 1:(length(res.arima.simple.sibling.regression)-1)){
        res.arima.simple.sibling.regression[[k+1]] <- data.table(result[[k]],key="cy")
    }

    names(res.arima.simple.sibling.regression) <- c(names(result.arima.youngest),names(result))
    

    resjoin <- vector("list",length(res.arima.simple.sibling.regression ))
    for (j in 1:length(resjoin)){

     		  DT <- setkey(res.arima.simple.sibling.regression[[j]], "cy")
      		DT1 <- DT[J(res.arima.simple.sibling.regression[[1]]$cy)]
     		  resjoin[[j]] <- DT1[complete.cases(DT1),]
    }

    ## nms <- NULL
    ## for (j in 1:length(datalist)) {
    ##      nms <- c(nms, paste("Age_",str_extract(names(datalist[j]),"[[:digit:]]+"),sep=""))
    ## }

    ## names(resjoin) <- nms
	    
    ## names(resjoin) <- names(result.simple.sibling.regression)
	    
	  names(resjoin) <- c(names(result.arima.youngest),names(result))  
	    
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

    out <- list(method="arima-sibreg",
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



## need these before call to total.age.retro.predictive.performance.arima.plus.simple.sibling.regression


SIMPLESIBREG$result.arima.youngest <- SIMPLESIBREG$individual.ages.retro.predictive.performance.arima.youngest(SIMPLESIBREG$datalist, 
                                        SIMPLESIBREG$forecastingyear, 
                                        SIMPLESIBREG$boxcoxtransform, 
                                        SIMPLESIBREG$index.year)

SIMPLESIBREG$res.arima.simple.sibling.regression <- SIMPLESIBREG$individual.ages.retro.predictive.performance.simple.sibling.regression.youngest(
                                                                        SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression, index.year)
## call to total.age.retro.predictive.performance.arima.plus.simple.sibling.regression
SIMPLESIBREG$rmse.results.youngest.age.arima.plus.simple.sibling.regression <- 
SIMPLESIBREG$total.age.retro.predictive.performance.arima.plus.simple.sibling.regression(SIMPLESIBREG$result.arima.youngest, 
                                                                               SIMPLESIBREG$result.simple.sibling.regression)




##=========================================================================================================
## measures of retro performance - expsmooth + sibreg
##=========================================================================================================


SIMPLESIBREG$total.age.retro.predictive.performance.expsmooth.plus.simple.sibling.regression <- function(result.expsmooth.youngest, 
                                                                                             result.simple.sibling.regression){

    result <- result.simple.sibling.regression

    ## data.table(cy=cy00, a, p, e, p.bench, e.bench, key="cy")
    usePackage("data.table")
    res.expsmooth.simple.sibling.regression <- vector("list",length(result)+1)
    res.expsmooth.simple.sibling.regression[[1]] <- data.table(result.expsmooth.youngest[[1]], key="cy")
    for (k in 1:(length(res.expsmooth.simple.sibling.regression)-1)){
        res.expsmooth.simple.sibling.regression[[k+1]] <- data.table(result[[k]],key="cy")
    }

    names(res.expsmooth.simple.sibling.regression) <- c(names(result.expsmooth.youngest),names(result))
    

    resjoin <- vector("list",length(res.expsmooth.simple.sibling.regression ))
    for (j in 1:length(resjoin)){

     		  DT <- setkey(res.expsmooth.simple.sibling.regression[[j]], "cy")
      		DT1 <- DT[J(res.expsmooth.simple.sibling.regression[[1]]$cy)]
     		  resjoin[[j]] <- DT1[complete.cases(DT1),]
    }

    ## nms <- NULL
    ## for (j in 1:length(datalist)) {
    ##      nms <- c(nms, paste("Age_",str_extract(names(datalist[j]),"[[:digit:]]+"),sep=""))
    ## }

    ## names(resjoin) <- nms
	    
	  ## names(resjoin) <- names(result.simple.sibling.regression)  
	    
	  names(resjoin) <- c(names(result.expsmooth.youngest),names(result))  
	    
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

    out <- list(method="expsmooth-sibreg",
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



## need these before call to total.age.retro.predictive.performance.expsmooth.plus.simple.sibling.regression
SIMPLESIBREG$result.expsmooth.youngest <- SIMPLESIBREG$individual.ages.retro.predictive.performance.expsmooth.youngest(SIMPLESIBREG$datalist, 
                                                       SIMPLESIBREG$forecastingyear, 
                                                       SIMPLESIBREG$boxcoxtransform, 
                                                       SIMPLESIBREG$index.year)

SIMPLESIBREG$res.expsmooth.simple.sibling.regression <- SIMPLESIBREG$individual.ages.retro.predictive.performance.simple.sibling.regression.youngest(
                                                                        SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression, 
                                                                        SIMPLESIBREG$index.year)

## call to total.age.retro.predictive.performance.expsmooth.plus.simple.sibling.regression
SIMPLESIBREG$rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression <- 
SIMPLESIBREG$total.age.retro.predictive.performance.expsmooth.plus.simple.sibling.regression(SIMPLESIBREG$result.expsmooth.youngest, 
                                                                                 SIMPLESIBREG$result.simple.sibling.regression)



##=========================================================================================================
## choosing the measure of retro performance (i.e., rmse) which is "best" for forecasting total age 
## among avgfive + sibreg, arima + sibreg, expsmooth + sibreg
##=========================================================================================================

SIMPLESIBREG$best.rmse.results.youngest.age <- function(rmse.results.youngest.age.avgfive.plus.simple.sibling.regression,
                                           rmse.results.youngest.age.arima.plus.simple.sibling.regression,
                                           rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression){
 
   rmse.total.age.avgfive <- rmse.results.youngest.age.avgfive.plus.simple.sibling.regression$rmse.total
   rmse.total.age.arima  <- rmse.results.youngest.age.arima.plus.simple.sibling.regression$rmse.total
   rmse.total.age.expsmooth  <- rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression$rmse.total                                           
 
   
   rmse.total.age <- c(rmse.total.age.avgfive, rmse.total.age.arima, rmse.total.age.expsmooth)
   
   min.rmse.total.age <- min(rmse.total.age)
           
   index.min.rmse.total.age <- which(rmse.total.age %in% sort(unique(rmse.total.age))[1])                    
   index.min.rmse.total.age <- as.numeric(index.min.rmse.total.age)
 
   if (index.min.rmse.total.age==1){
       sibreg.youngest.age.method <- "naive forecasting (i.e., average of previous five years)"
       output <- rmse.results.youngest.age.avgfive.plus.simple.sibling.regression
   } else if (index.min.rmse.total.age==2) {
       sibreg.youngest.age.method <- "ARIMA forecasting"
       output <- rmse.results.youngest.age.arima.plus.simple.sibling.regression
   } else if (index.min.rmse.total.age==3) {
       sibreg.youngest.age.method <- "exponential smoothing forecasting"
       output <- rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression
   }
   
   myoutput <- list(method = sibreg.youngest.age.method, 
                    retro = output, 
                    min.rmse.total.age = min.rmse.total.age,
                    index.min.rmse.total.age = index.min.rmse.total.age)
   
 }

 
SIMPLESIBREG$best.rmse.youngest.age <-  SIMPLESIBREG$best.rmse.results.youngest.age(
                                                     SIMPLESIBREG$rmse.results.youngest.age.avgfive.plus.simple.sibling.regression,
                                                     SIMPLESIBREG$rmse.results.youngest.age.arima.plus.simple.sibling.regression,
                                                     SIMPLESIBREG$rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression)
 

SIMPLESIBREG$best.rmse.youngest.age



###
###  "Cool" plots for the youngest age 
###

SIMPLESIBREG$best.rmse.youngest.age$method   # "naive forecasting (i.e., average of previous five years)"
                                # "ARIMA forecasting"
                                # "exponential smoothing forecasting"

## youngest.age.retro.plot.avgfive(youngest.age.retro.plot.info.avgfive, stockabundance)
## youngest.age.retro.plot.arima(youngest.age.retro.plot.info.arima, stockabundance)
## youngest.age.retro.plot.expsmooth(youngest.age.retro.plot.info.expsmooth, stockabundance)


###
### Density Plot of Retrospective Forecast Errors: Individual Ages
###

SIMPLESIBREG$plot.dens.retrospective.forecast.errors.individual.ages.simple.sibling.regression <- function(best.rmse.youngest.age, stockabundance){

    .e = environment()

    errors.stacked <- NULL
    labels.stacked <- NULL

      for (j in 1:length(names(best.rmse.youngest.age$retro$resjoin))){

           errors.stacked <- c(errors.stacked, best.rmse.youngest.age$retro$resjoin[[j]]$e )
           mylabel <-  as.character(names(best.rmse.youngest.age$retro$resjoin)[j])
           mylabel <- str_replace_all(mylabel,"_"," ") 
           mylabel <- paste0(stockabundance, " at ", mylabel)
           labels.stacked <- c( labels.stacked, rep(mylabel, length(best.rmse.youngest.age$retro$resjoin[[j]]$e)))

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
    ## for (j in 1:length(names(best.rmse.youngest.age$retro$resjoin))){
    ##    h <- hist(data.stacked$errors[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE, breaks = "Freedman-Diaconis")
    ##    ## breaks[[j]] <- h$breaks
    ##    h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks)))
    ##    breaks[[j]] <- h.tmp
    ## }

    #  environment=.e
    g <- ggplot(d, aes(x=errors), environment=.e) +
           ## mapply(function(d, b) {geom_histogram(data=d, breaks=b, fill="lightblue",colour="black")},
            ## split(d, d$labels), breaks) +
            geom_density(data=d, fill="lightblue",colour="black") +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Retrospective Forecast Errors"),labels=comma)

      g = g + theme_bw() +
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
                
     clim <- NULL
     for (j in 1:length(names(best.rmse.youngest.age$retro$resjoin))){
     
        clim <- c(clim, 0)

     }
     
     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(aes(xintercept = z), dummy2, linetype="dashed",col="red", size=0.8)
    

     return(g)
}


SIMPLESIBREG$plot.dens.retrospective.forecast.errors.individual.ages.simple.sibling.regression(
    SIMPLESIBREG$best.rmse.youngest.age, 
    SIMPLESIBREG$stockabundance)




###
### Density Plot of Retrospective Forecast Errors: Total Age
###

SIMPLESIBREG$plot.dens.retrospective.forecast.errors.total.age.simple.sibling.regression <- function(best.rmse.youngest.age, stockabundance){

    .e = environment()

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
           ## geom_histogram(data=d, breaks=breaks, fill="lightblue",colour="black") + 
           geom_density(data=d, fill="lightblue",colour="black") + 
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Retrospective Forecast Errors"),labels=comma)  + 
                    ggtitle(label=paste(mylabel))

      g = g + theme_bw() +
          theme(plot.title=element_text(hjust=0.5), 
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


SIMPLESIBREG$plot.dens.retrospective.forecast.errors.total.age.simple.sibling.regression(
    SIMPLESIBREG$best.rmse.youngest.age, 
    SIMPLESIBREG$stockabundance)




SIMPLESIBREG$best.rmse.youngest.age <-  SIMPLESIBREG$best.rmse.results.youngest.age(
                                              SIMPLESIBREG$rmse.results.youngest.age.avgfive.plus.simple.sibling.regression,
                                              SIMPLESIBREG$rmse.results.youngest.age.arima.plus.simple.sibling.regression,
                                              SIMPLESIBREG$rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression)

SIMPLESIBREG$best.rmse.youngest.age


##
## R-Squared Values from Retrospective Evaluation of Point Forecast Performance
## 
  

SIMPLESIBREG$r.squared.retro.simple.sibling.regression <- function(best.rmse.youngest.age){

    component <- NULL
    r.squared <- NULL

    for (i in 1:length(best.rmse.youngest.age$retro$resjoin)){

       usePackage("stringr")

       datatmp <- best.rmse.youngest.age$retro$resjoin[[i]]
       datatmp <- as.data.frame(datatmp)

       names(datatmp)[names(datatmp)=="a"] <- "Actual"
       names(datatmp)[names(datatmp)=="p"] <- "Forecast"
       names(datatmp)[names(datatmp)=="e"] <- "Error"

       r.sq <- summary(lm(Forecast ~ Actual, data=datatmp))$r.squared
       r.sq <-  sprintf("%.2f", r.sq*100)

       ## R.squared <- c(R.squared, r.sq)

       mytitle <- names(best.rmse.youngest.age$retro$resjoin)[i]

       ## usePackage("stringr")
       ## myage <- as.numeric(str_extract(mytitle,"[[:digit:]]+"))

       mytitle <- str_replace_all(mytitle, pattern="_", replacement=" ")
       
       component <- c(component, mytitle)
       r.squared <- c(r.squared, r.sq)

    }
    
    model <- rep("Sibling Regression (Best Model)",length(best.rmse.youngest.age$retro$resjoin))
    
    tmpmodel <- best.rmse.youngest.age$method     
    
    if (tmpmodel=="naive forecasting (i.e., average of previous five years)"){
         tmpmodelnew <- "Naive (Average of Previous Five Years)"
    } else if (tmpmodel=="ARIMA forecasting") {
         tmpmodelnew <- "ARIMA"
    } else if (tmpmodel=="exponential smoothing forecasting") {
         tmpmodelnew <- "Exponential Smoothing"
    }
    
    model[1] <- tmpmodelnew  

    data.stacked <- data.frame(component=component,
                               model=model,
                               r.squared=paste0(r.squared,"%"))

    rm(datatmp)
    
    datatmp <- data.frame(cy = best.rmse.youngest.age$retro$resjoin[[1]]$cy,
                             a = best.rmse.youngest.age$retro$a.total,
                             p = best.rmse.youngest.age$retro$p.total,
                             e = best.rmse.youngest.age$retro$e.total
                             )

    names(datatmp)[names(datatmp)=="a"] <- "Actual"
    names(datatmp)[names(datatmp)=="p"] <- "Forecast"
    names(datatmp)[names(datatmp)=="e"] <- "Error"

    r.sq.total <- summary(lm(Forecast ~ Actual, data=datatmp))$r.squared
    r.sq.total <-  sprintf("%.2f", r.sq.total*100)

    data.stacked.total <- data.frame(component="Total", model=paste0(model[1]," + ",model[2]),r.squared=paste0(r.sq.total,"%"))

    data.stacked.final <- rbind.data.frame(data.stacked, data.stacked.total)

    data.stacked.final

}

## SIMPLESIBREG$r.squared.retro.simple.sibling.regression(best.rmse.youngest.age)


#*******************************************************************************
#
# Gary's Plot for Individual Ages: Youngest + Older
#
#*******************************************************************************



SIMPLESIBREG$gary.plot.individual.ages.simple.sibling.regression <- function(best.rmse.youngest.age,
                                                                 pred.int.individual.ages.avgfive.youngest,   
                                                                 # youngest age prediction: avgfive
                                                                 pred.int.individual.ages.arima.youngest,     
                                                                 # youngest age prediction: arima
                                                                 pred.int.individual.ages.expsmooth.youngest, 
                                                                 # youngest age prediction: expsmooth
                                                                 pred.int.individual.ages.simple.sibling.regression, 
                                                                 # older ages prediction: sibling regression (best model)
                                                                 forecastingyear, j){

       ## start from here

       .e <- environment()
       
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

          pred.oldest <- pred.int.individual.ages.simple.sibling.regression[[j-1]]
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
        geom_rect(aes(xmax=years-1/3,
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
        labs(title=paste(SIMPLESIBREG$stockabundance,"at",age)) +
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
        geom_rect(aes(xmax=years-1/3,
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
        labs(title=paste(SIMPLESIBREG$stockabundance,"at",age)) +
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
        geom_rect(aes(xmax=years-1/3,
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
        labs(title=paste(SIMPLESIBREG$stockabundance, "at",age)) +
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
        geom_rect(aes(xmax=years-1/3,
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
        labs(title=paste(SIMPLESIBREG$stockabundance, "at",age)) +
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


} # end gary.plot.simple.sibling.regression for individual ages


## j <- 1

SIMPLESIBREG$gary.plot.individual.ages.simple.sibling.regression( SIMPLESIBREG$best.rmse.youngest.age,
                                                                 SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,   
                                                                # youngest age prediction: avgfive
                                                                  SIMPLESIBREG$pred.int.individual.ages.arima.youngest,     
                                                                 # youngest age prediction: arima
                                                                  SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest, 
                                                                 # youngest age prediction: expsmooth
                                                                  SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression, 
                                                                 # older ages prediction: sibling regression (best model)
                                                                  SIMPLESIBREG$forecastingyear, j=1)
                                                                 


#*******************************************************************************
#
# Gary's Plot for Total Age:
#
#*******************************************************************************



SIMPLESIBREG$gary.plot.total.age.simple.sibling.regression <- function(best.rmse.youngest.age,
                                                           pred.int.total.age.simple.sibling.regression.all.models,  # total age prediction
                                                           forecastingyear){

      ## start from here

      .e <- environment()

      results.retro <- data.frame(cy = best.rmse.youngest.age$retro$resjoin[[1]]$cy,
                                   a   = best.rmse.youngest.age$retro$a.total,
                                   p   = best.rmse.youngest.age$retro$p.total,
                                   e   = best.rmse.youngest.age$retro$e.total)



      model.youngest <- best.rmse.youngest.age$method  # model classed used for forecasting abundance for youngest age

      if (model.youngest=="naive forecasting (i.e., average of previous five years)"){
            method.youngest <- "Naive (Average of Previous Five Years)"
            pred.total <- data.frame(PI.ctr=pred.int.total.age.simple.sibling.regression.all.models$p$avgfive,
                                     PI.lwr=pred.int.total.age.simple.sibling.regression.all.models$p.lwr$avgfive,
                                     PI.upr=pred.int.total.age.simple.sibling.regression.all.models$p.upr$avgfive)

        } else if (model.youngest=="ARIMA forecasting") {
            method.youngest <- "ARIMA"
            pred.total <- data.frame(PI.ctr=pred.int.total.age.simple.sibling.regression.all.models$p$arima,
                                     PI.lwr=pred.int.total.age.simple.sibling.regression.all.models$p.lwr$arima,
                                     PI.upr=pred.int.total.age.simple.sibling.regression.all.models$p.upr$arima)
        } else if (model.youngest=="exponential smoothing forecasting") {
            method.youngest <- "Exponential Smoothing"
            pred.total <-  data.frame(PI.ctr=pred.int.total.age.simple.sibling.regression.all.models$p$expsmooth,
                                     PI.lwr=pred.int.total.age.simple.sibling.regression.all.models$p.lwr$expsmooth,
                                     PI.upr=pred.int.total.age.simple.sibling.regression.all.models$p.upr$expsmooth)
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
        geom_rect(aes(xmax=years-1/3,
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
        ## labs(title=paste("Total", stockabundance)) +
        ggtitle(label=paste("Total", SIMPLESIBREG$stockabundance)) + 
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
        geom_rect(aes(xmax=years-1/3,
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
        ## labs(title=paste("Total", stockabundance)) +
        ggtitle(label=paste("Total", SIMPLESIBREG$stockabundance)) + 
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
        geom_rect(aes(xmax=years-1/3,
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
        ## labs(title=paste("Total", stockabundance)) +
        ggtitle(label=paste("Total", SIMPLESIBREG$stockabundance)) + 
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
        geom_rect(aes(xmax=years-1/3,
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
        ## labs(title=paste("Total", stockabundance)) +
        ggtitle(label=paste("Total", SIMPLESIBREG$stockabundance)) + 
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


} # end gary.plot.simple.sibling.regression for total age



SIMPLESIBREG$gary.plot.total.age.simple.sibling.regression(SIMPLESIBREG$best.rmse.youngest.age,
                                               SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,  # total age prediction
                                               SIMPLESIBREG$forecastingyear)



##
## Table of results for candidate models for youngest age: avgfive, arima, expsmooth
##

SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest
SIMPLESIBREG$pred.int.individual.ages.arima.youngest
SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest


SIMPLESIBREG$table.results.individual.ages.all.models.youngest <- list()
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$Model <- c("Naive (Average of Previous Five Years)", 
                                                             "ARIMA", 
                                                             "Exponential Smoothing")
                                                             
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.ctr <- c( round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.ctr),
                                                               round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.ctr),
                                                               round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.ctr))                                                             

SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.lwr<- c( round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.lwr),
                                                               round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.lwr),
                                                               round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.lwr))
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.lwr <- as.numeric(SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.lwr)
                                                               
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.upr <- c(round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.upr),
                                                               round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.upr),
                                                               round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.upr))                                                             
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.upr <- as.numeric(SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.upr)

SIMPLESIBREG$rmse.total.age.avgfive <- SIMPLESIBREG$rmse.results.youngest.age.avgfive.plus.simple.sibling.regression$rmse.total
SIMPLESIBREG$rmse.total.age.arima  <- SIMPLESIBREG$rmse.results.youngest.age.arima.plus.simple.sibling.regression$rmse.total
SIMPLESIBREG$rmse.total.age.expsmooth  <- SIMPLESIBREG$rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression$rmse.total                                           
 
SIMPLESIBREG$rmse.total.age <- c(SIMPLESIBREG$rmse.total.age.avgfive, SIMPLESIBREG$rmse.total.age.arima, SIMPLESIBREG$rmse.total.age.expsmooth)
   
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$rmse.total.age <- SIMPLESIBREG$rmse.total.age  ## recall that this rmse is optimized for 
                                                                                    ## prediction of total age!!!
   

SIMPLESIBREG$table.results.individual.ages.all.models.youngest <- do.call(cbind.data.frame, SIMPLESIBREG$table.results.individual.ages.all.models.youngest)

usePackage("scales")
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.ctr <- comma(SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.ctr)
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.lwr <- comma(SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.lwr)
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.upr <- comma(SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.upr)

SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.int <- paste0(SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.lwr, 
                                                                   " - ", 
                                                                   SIMPLESIBREG$table.results.individual.ages.all.models.youngest$PI.upr)

SIMPLESIBREG$table.results.individual.ages.all.models.youngest <- subset(SIMPLESIBREG$table.results.individual.ages.all.models.youngest, 
                                                            select=c(Model, PI.ctr,  PI.int, rmse.total.age))
names(SIMPLESIBREG$table.results.individual.ages.all.models.youngest) <-  c("Model", "Point Forecast", "Interval Forecast", "RMSE")

## SIMPLESIBREG$table.results.individual.ages.all.models.youngest


#=====================================================================================================================================
#
# Best RMSE
#
#######################################################################################################################################


SIMPLESIBREG$best.rmse.youngest.age <-  SIMPLESIBREG$best.rmse.results.youngest.age(SIMPLESIBREG$rmse.results.youngest.age.avgfive.plus.simple.sibling.regression,
                                                          SIMPLESIBREG$rmse.results.youngest.age.arima.plus.simple.sibling.regression,
                                                          SIMPLESIBREG$rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression)
                                                          
SIMPLESIBREG$total.index <- SIMPLESIBREG$best.rmse.youngest.age$index.min.rmse.total.age



###
### plot forecast vs. actual (individual ages, simple sibling regression)
###

SIMPLESIBREG$plot.results.afe.individual.ages.retro.simple.sibling.regression <- function(best.rmse.youngest.age){

    .e = environment()


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

       #### R.squared <- c(R.squared, r.sq)

       mytitle <- names(best.rmse.youngest.age$retro$resjoin)[i]

       myage <- as.numeric(str_extract(mytitle,"[[:digit:]]+"))

       usePackage("stringr")
       mytitle <- str_replace_all(mytitle, pattern="_", replacement=" ")
       #### mytitle <- substr(mytitle, start=1, stop=5)

       #### mytitle <- paste(mytitle, ": ", "Naive (Last Year)", sep="")
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
                   scale_y_continuous(paste("Forecasted", SIMPLESIBREG$stockabundance, "Values"),labels=comma) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous(paste("Actual", SIMPLESIBREG$stockabundance, "Values"),labels=comma) +
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
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8))


      return(g)

}




SIMPLESIBREG$plot.results.afe.individual.ages.retro.simple.sibling.regression(SIMPLESIBREG$best.rmse.youngest.age)



###
### plot forecasted vs. actual (total age, simple sibling regression)
###

SIMPLESIBREG$plot.results.afe.total.age.retro.simple.sibling.regression <- function(best.rmse.youngest.age, stockabundance){

    .e = environment()

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

    #### R.squared <- c(R.squared, r.sq)

    #### myage <- as.numeric(str_extract(mytitle,"[[:digit:]]+"))

    usePackage("stringr")
    ## mytitle <- str_replace_all(mytitle, pattern="_", replacement=" ")
    ## mytitle <- substr(mytitle, start=1, stop=5)

    mytitle <- paste0("Total ", stockabundance)

    #### mytitle <- paste(mytitle, ": ", "Naive (Last Year)", sep="")
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
                   scale_y_continuous(paste("Forecasted Total", stockabundance, "Values"),labels=comma) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous(paste("Actual Total", stockabundance, "Values"),labels=comma) +
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
          theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8))


    return(g)

}

SIMPLESIBREG$plot.results.afe.total.age.retro.simple.sibling.regression(SIMPLESIBREG$best.rmse.youngest.age, SIMPLESIBREG$stockabundance)

##

########################################################################################################################################



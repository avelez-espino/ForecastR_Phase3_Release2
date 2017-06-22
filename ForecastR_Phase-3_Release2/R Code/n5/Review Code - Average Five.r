########################################################################################################
# Naive Time Series Forecasting (Average of Past 5 Years) - Forecasting Results
########################################################################################################


n5$datafile <- datafile_original

n5$datafilesub <- n5$datafile

n5$stockabundance <- n5$datafile$Stock_Abundance[1]
n5$stockabundance <- gsub("[[:space:]]", "_", n5$stockabundance)

n5$stockname <- n5$datafile$Stock_Name[1]
n5$stockspecies <- n5$datafile$Stock_Species[1]
n5$forecastingyear <- n5$datafile$Forecasting_Year[1]


usePackage("stringr")
n5$forecastingyear <- str_replace_all(n5$forecastingyear, "\n","")
n5$forecastingyear <- as.numeric(n5$forecastingyear)



## datafile <- data.frame(BY, T2, T3, T4, T5)   # need to come back here to automate the creation of this data frame!!! [January 13, 2014]

n5$extract_ages <- sort(unique(n5$datafilesub$Age_Class))
n5$extract_names <- paste("T",n5$extract_ages,sep="")
n5$extract_names <- c("BY",n5$extract_names)


n5$tmpsub <- list()
for (i in 1:length(n5$extract_ages)){
    n5$tmpsub[[i]] <- subset(n5$datafilesub, Age_Class==n5$extract_ages[i])[,c("Brood_Year",paste0("Average","_",n5$stockabundance))]
}


n5$list.of.data.frames <- n5$tmpsub
n5$merged.data.frame = Reduce(function(...) merge(...,by="Brood_Year", all=T), n5$list.of.data.frames)

n5$datafile_new <- n5$merged.data.frame
names(n5$datafile_new) <- n5$extract_names

## datafile <<- datafile_new

n5$datafile <- n5$datafile_new

#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for naive forecasting (average of past 5 years) ---------------------------------

n5$datalist.avgfive <- function(datafile, forecastingyear) {


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


n5$datalist <- n5$datalist.avgfive(n5$datafile, n5$forecastingyear)  # CY refers to the T variable with highest age


#--------- prepare data table for reporting --------------------------------------------------

n5$datafile.report <-  n5$datafile

n5$datafile.report[n5$datafile.report <0] <- "NA"



#--------- plot data to be used for naive forecasting (average of past five years) (uses ggplot) ---------------------------

n5$plot.data.avgfive   <- function(datalist){

     # par(mfrow=c(length(datalist),1), mar=c(4,6,2,2))

     x.stacked <- NULL
     y.stacked <- NULL
     age.stacked <- NULL
     for (i in 1:length(datalist)) {

          x <- datalist[[i]][,"CY"]

          x.stacked <- c(x.stacked, x)

          y <- datalist[[i]][,ncol(datalist[[i]])]

          y.stacked <- c(y.stacked, y)

          age <- gsub("[^0-9]", "", names(datalist[[i]]))

          age <- as.numeric(age[length(age)])

          age.stacked <- c(age.stacked, rep(paste("Age",age), length(y)))

    }

    data.stacked <- data.frame(x=x.stacked,y=y.stacked,age=age.stacked)

    usePackage("ggplot2")
    usePackage("scales")

    ggplot(data.stacked, aes(x,y)) +
      geom_line(colour="dodgerblue3") +
        geom_point(col="dodgerblue3") +
          facet_wrap(~age, ncol=1, scales="free") +
            xlab("Return Year") +
              # scale_y_continuous("Terminal Run",labels=comma) +
               scale_y_continuous(paste(n5$stockabundance),labels=comma) +
                theme_bw() +
                   theme(plot.title=element_text(size=12, hjust=0.5),
                         axis.title.x = element_text(size=10,vjust=-0.5),
                         axis.title.y = element_text(size=10,vjust=1),
                         axis.text.x = element_text(size=8),
                         axis.text.y = element_text(size=8)
                         )

}


n5$plot.data.avgfive(n5$datalist)


#--------- helper function for computing the average of the past five years of a time series -----


#--------- helper function for computing the average of the past 5 years of a time series -----

n5$avgfive  <- function(series) {

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

n5$avgfive.model <- function(datalist){

     output <- vector("list", length(datalist)) # start to build the S3 class storing the output

     class(output) <- "avgfive"  # create a new class

     for (j in 1:length(datalist)) {

          tmpdata <- datalist[[j]][complete.cases(datalist[[j]]),]

          series <- tmpdata[ ,ncol(tmpdata)]

          usePackage("forecast")

          usePackage("stringr")

          output[[j]]$age <-  paste("Age ",
                                    as.numeric(str_extract(names(tmpdata)[length(names(tmpdata))],"[[:digit:]]")),
                                    sep="")

          output[[j]]$model <- n5$avgfive(series)

          # output[[j]]$formula <- form

          output[[j]]$original.data <- datalist[[j]]

          output[[j]]$model.data <- datalist[[j]][complete.cases(datalist[[j]]),]

     }

     names(output) <- names(datalist)

     return(output)

}

n5$avgfive.model.fits  <- n5$avgfive.model(n5$datalist)

n5$fits <- n5$avgfive.model.fits



#---------  Plot naive model (average of past 5 years) --------------------------------
# Plot fitted naive model (ggplot)
#-------------------------------------------------------------------------------

n5$plot.fitted.avgfive <- function(fits){

    .e <- environment()

    year.stacked <- NULL
    actual.fitted.stacked <- NULL
    age.stacked <- NULL
    legend.stacked <- NULL

    for (j in 1:length(fits)) {

       avgfivefit <- fits[[j]]
       CY <- avgfivefit$model.data[,"CY"]

       avgfivemodel <- fits[[j]]$model

       age <- fits[[j]]$age

       year <- CY
       actual <- as.numeric(avgfivemodel$x)
       fitted <- as.numeric(avgfivemodel$fitted)

       year.stacked <- c(year.stacked, year, year)
       actual.fitted.stacked <- c(actual.fitted.stacked, actual, fitted)
       age.stacked <- c(age.stacked, rep(paste(age),length(actual)),rep(paste(age),length(fitted)))
       legend.stacked <- c(legend.stacked, rep("Actual Values",length(actual)),rep("Fitted Values Obtained via Naive Time Series Modeling",length(fitted)))

    }

    data.stacked <- data.frame(year=year.stacked, actual.fitted=actual.fitted.stacked, age=age.stacked, legend=legend.stacked)


    usePackage("ggplot2")
    usePackage("scales")

    ggplot(data.stacked, aes(year.stacked, actual.fitted), environment=.e) +
    facet_wrap(~age,ncol=1, scales="free_y") +
    # geom_point(aes(shape = legend)) +
    geom_line(aes(colour = legend, group = legend),size=0.6) +
    labs(x = "Return Year", y = paste(n5$stockabundance) , shape = "", colour = "") +
    scale_y_continuous(paste(n5$stockabundance),labels=comma) +
    theme_bw() +
    theme(plot.title=element_text(size=12, hjust=0.5),
    axis.title.x = element_text(vjust=-0.5),  axis.title.y = element_text(vjust=1.5),legend.position = "top", legend.direction = "horizontal")  +
    scale_colour_manual(values=c("dodgerblue3","lightsalmon1"))  +
    scale_linetype_manual(values=c(1,2))

}

n5$fits <- n5$avgfive.model.fits
n5$plot.fitted.avgfive(n5$fits)





#-------------------------------------------------------------------------------
# Model Diagnostics for Age-Specific Naive Time Series Model  (Average of Past 5 Years)
#-------------------------------------------------------------------------------


n5$diagnostics.avgfive.model.fit  <- function(fits,i){

    .e <- environment()

    usePackage("forecast")
    usePackage("portes")

    avgfivefit <- fits[[i]]

    avgfivemodel <- avgfivefit$model
    avgfiveresiduals <- avgfivemodel$residuals

    CY <- avgfivefit$model.data[,"CY"]

    modelavgfive <- "Naive Model (Average of Previous 5 Years)"

    Age <- avgfivefit$age

    ## avgfiveresiduals <- avgfiveresiduals[-c(1,2,3,4,5)]  # first five residuals are in fact missing values
    ## CY <- CY[-c(1,2,3,4,5)]  # retain only calendar years for which the residuals are NOT missing values


    ## environment=.e

    g1 <- ggplot(data=data.frame(avgfiveresiduals=as.numeric(avgfiveresiduals),CY=CY),
           aes(CY, avgfiveresiduals),environment=.e) +
    		geom_point() +
    		 geom_line() +
    		  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    		   labs(x="Return Year") +
    		    labs(y="Residuals")+
    		     ggtitle(paste("Terminal Run at ", Age, ": ", modelavgfive, sep="")) +
    			theme_bw() +
          		 theme(plot.title=element_text(size=12, hjust=0.5), 
                   axis.title.x=element_text(size=10,vjust=-0.5),
                	  axis.title.y=element_text(size=10,vjust=1.5),
                	   axis.text.x=element_text(size=8),
                	    axis.text.y=element_text(size=8),
                	     strip.text.x = element_text(size = 8, colour = "black", angle = 0))

    ## g2: ACF plot
    
    tmp <- Acf(avgfiveresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1

    lag <- 1:lagmax
    
    acf <-  Acf(avgfiveresiduals,lag.max=lagmax, plot=FALSE)$acf[-1]
    data.stacked <- data.frame(lag=lag, acf=acf)

    g2 <- ggplot(data.stacked, aes(lag,acf),environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=acf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("ACF of Residuals"),labels=comma) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g2 = g2 + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


        acftmp <- Acf(avgfiveresiduals, plot=FALSE)

        ci <- 0.95 # Indicates 95% confidence level
        clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
        clim <- clim0

      g2 = g2 + geom_hline(aes(yintercept=0))
      g2 = g2 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g2 = g2 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")


    ## g3: PACF plot

    # lag <- acf(avgfiveresiduals,type="partial",plot=FALSE)$lag
    # pacf <-  acf(avgfiveresiduals,type="partial",plot=FALSE)$acf
    # data.stacked <- data.frame(lag=lag, pacf=pacf)
    
    
    tmp <- Acf(avgfiveresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1
    lag <- 1:lagmax

    pacf <-  Pacf(avgfiveresiduals,lag.max=lagmax, plot=FALSE)$acf
    

    g3 <- ggplot(data.stacked, aes(lag,pacf),environment=.e) +
           geom_linerange(aes(x=lag, ymin=0, ymax=pacf), colour="red", size=0.3) +
           ## geom_hline(yintercept=2*mean(data.stacked$leverage)) +   ## doesn't work
          # geom_text(aes(label=labels),col="blue",size=3) +
            # coord_fixed(ratio=1) +
              # facet_wrap(~labels, scales="free", ncol = 1) +
                expand_limits(x=1, y=0) +
                  scale_y_continuous(paste("PACF of Residuals"),labels=comma) +
                    scale_x_continuous(paste("Lag"),breaks=0:length(unique(data.stacked$lag)))
                   ## coord_fixed(ratio=1)

      ## print(g)

      g3 = g3 + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


       acftmp <- Acf(avgfiveresiduals, plot=FALSE)

       ci <- 0.95 # Indicates 95% confidence level
       clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
       clim <- clim0

      g3 = g3 + geom_hline(aes(yintercept=0))
      g3 = g3 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g3 = g3 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")


    ## g4: Plot of p-values from Ljung-Box test

    ## lags <- acf(avgfiveresiduals,plot=FALSE)$lag[-1]

    tmp <- Acf(avgfiveresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1
    lags <- 1:lagmax


    Ljung.Box.Test <- LjungBox(avgfiveresiduals[!is.na(avgfiveresiduals)], lags=lags)
    
    Ljung.Box.Test <- data.frame(lags= Ljung.Box.Test[,"Lags"], pvalue=Ljung.Box.Test[,ncol(Ljung.Box.Test)])


    g4 <- ggplot(data= Ljung.Box.Test, aes(lags, pvalue),environment=.e) +
           geom_point() +
            geom_line()
    g4 <- g4 + geom_hline(aes(yintercept=0.05), linetype="dashed", colour="steelblue")
    g4 <- g4 + labs(x="Lag") + labs(y="P-value for Ljung-Box Test")
    g4 <- g4 + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
    g4 <- g4 + geom_text(data = NULL, x=Ljung.Box.Test$lags[length(Ljung.Box.Test$lags)-1], y=0.10, label=paste("alpha","= 0.05"),size=2.5)

    usePackage("gridExtra")

    ## g <- arrangeGrob(g1, g2, g3, g4, ncol=1)
    
    g <- grid.arrange(g1, g2, g3, g4, ncol=1)

    g
}

n5$diagnostics.avgfive.model.fit(n5$fits,i)



#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

n5$point.forecast.avgfive <- function(datalist, fits){

     PSY <- datalist[[1]]$CY[length(datalist[[1]]$CY)] + 1

     output <- vector("list",length(fits))

     nms <- NULL
     for (j in 1:length(fits)) {

         fits[[j]]$model

         model <- fits[[j]]$model

         avgfivefit <- fits[[j]]$model

         output[[j]]$Age <- fits[[j]]$age

         output[[j]]$Model <- "Naive (Average of Previous 5 Years)"

         output[[j]]$RY <- PSY

         # output[[j]]$p <- as.numeric(predict(model, h=1, level=0.80)$pred)

         output[[j]]$p <-  as.numeric(avgfivefit$mean)

         nms <- c(nms, output[[j]]$Age)

     }

     names(output) <- nms

     return(output)
}


n5$point.forecast.avgfive(n5$datalist, n5$fits)




n5$results.point.forecast.avgfive <- NULL
for (j in 1:length(n5$avgfive.model.fits)){

       n5$tmp_list <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)[[j]]

       # list2 <- unlist(list1)

       # point.pred.asslr <- rbind(point.pred.asslr, list2)

       n5$tmp_df <- do.call(cbind.data.frame, n5$tmp_list)

       n5$results.point.forecast.avgfive <- rbind(n5$results.point.forecast.avgfive, n5$tmp_df)

}

n5$results.point.forecast.avgfive$Model <- as.character(n5$results.point.forecast.avgfive$Model)

## results.point.forecast.avgfive

## str(results.point.forecast.avgfive)





#--------- Naive Model (Average of Past 5 Years): retrospective evaluation for each individual age ----------------------------------

n5$individual.ages.retro.predictive.performance.avgfive <- function(datalist, index){

     # index <- 10

     PSY <- datalist[[1]]$CY[length(datalist[[1]]$CY)] + 1    ## Come back here to make sure the forecasting year is specified correctly!

     result <- list()

     nms <- NULL
     
     n5$individual.ages.retro.plot.info.avgfive <<- list()

     for (j in 1:length(datalist)){

          subdata <- subset(datalist[[j]], CY < PSY)

     	    y <- subdata[,ncol(subdata)]

          cy <- subdata[,"CY"]

          avgfivefit <- n5$fits[[j]]$model

          usePackage("stringr")

          a <- NULL
          p <- NULL
          e <- NULL

          cy00 <- NULL
          
          data0 <- NULL

          p.bench <- NULL
          e.bench <- NULL

          for (i in index:(length(y)-1)){

               y0 <- y[1:i]

               cy0 <- cy[1:i]

               model0 <- n5$avgfive(y0)

               p0 <- as.numeric(model0$mean)
               f0 <- as.numeric(model0$fitted)

               d0 <- data.frame(i=i, cy0, y0, f0, p0, psy=max(cy0)+1, a0=y[i+1])
                
               ## str(d0)
               
               data0 <- rbind.data.frame(data0, d0)  
              
               #----
               p <- c(p, p0)
               e0 <- y[i+1] - p0   # actual - predicted
               e <- c(e, e0)
               a <- c(a, y[i+1]) #actual

               cy00 <- c(cy00, cy[i+1])

               ## benchmark: naive forecasting (previous year)

               y0 <- y[1:i]

               usePackage("forecast")

               model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

               p0.bench <- as.numeric(model0.bench$mean)

               p.bench <- c(p.bench, p0.bench)
               e0.bench <- y[i+1] - p0.bench   # actual - predicted
               e.bench <- c(e.bench, e0.bench)


          }



          nms <- c(nms,names(datalist[j]))

          result[[j]] <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)

           n5$individual.ages.retro.plot.info.avgfive[[j]] <<- data0

     } # end for j loop

     nms

     names(result) <- nms

     names(n5$individual.ages.retro.plot.info.avgfive) <<- nms

     result

     res <- vector("list",length(result))

     for (j in 1:length(result)) {

          a <- as.numeric(result[[j]]$a)
          p <- as.numeric(result[[j]]$p)
          e <- as.numeric(result[[j]]$e)

          p.bench <- as.numeric(result[[j]]$p.bench)
          e.bench <- as.numeric(result[[j]]$e.bench)


          mre <- mean(e)

          mae <- mean(abs(e))

          mpe <- mean(e/a)

          mape <- mean(abs(e)/a)

          num_mase <- mean(abs(e))
          # denom_mase <- mean(abs(e))
          denom_mase <- mean(abs(e.bench))    # e.bench contains retrospective forecast errors
                                              # from naive forecast method (previous year)
          mase <- num_mase/denom_mase

          rmse <- sqrt(sum(e^2)/length(e))

          res[[j]]$a.retro <- a
          res[[j]]$p.retro <- p
          res[[j]]$e.retro <- e
          res[[j]]$mre.retro <- mre
          res[[j]]$mae.retro <- mae
          res[[j]]$mpe.retro <- mpe
          res[[j]]$mape.retro <- mape
          res[[j]]$mase.retro <- mase
          res[[j]]$rmse.retro <- rmse

          res[[j]]$data.retro <- result[[j]]

     }

     names(res) <- nms

     return(res)

}



n5$results.individual.ages.retro.predictive.performance.avgfive  <-
     n5$individual.ages.retro.predictive.performance.avgfive(n5$datalist, n5$index.year)

## results.individual.ages.retro.predictive.performance.avgfive

## names(results.individual.ages.retro.predictive.performance.avgfive)



#---------------------------------------------------------------------------------------

n5$individual.ages.retro.plot.avgfive <- function(individual.ages.retro.plot.info.avgfive, stockabundance, j){

   .e <- environment()

   mydata <- individual.ages.retro.plot.info.avgfive[[j]]

   tmpage <- names(individual.ages.retro.plot.info.avgfive)[j]

   tmpage <- str_replace(tmpage, "age", "Age ")

   ggplot(mydata, aes(cy0, y0), environment=.e) + 
    geom_line(data=mydata, aes(cy0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) + 
      geom_line(data=mydata, aes(cy0, f0), colour="red") + 
       geom_point(data=mydata, aes(psy, p0), colour="red") + 
        ylab(paste0(stockabundance)) + 
         xlab(paste0("Return Year")) + 
          ggtitle(paste0(tmpage)) + 
           facet_wrap(~psy) + 
            theme_bw() + 
             scale_y_continuous(labels=scales::comma) + 
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}





### report retrospective performance measures for individual ages in a nicer format

n5$measures.individual.ages.retro.avgfive <- function(results){


       Model <- c("Naive (Average of Past 5 Years)", rep("",5))

       Perf.Meas <- c("MRE","MAE","MPE","MAPE","MASE","RMSE")

       tmp_df <- data.frame(Model, Measure=Perf.Meas)


       for (j in 1:length(results)){

            results_tmp <- results[[j]]

            Perf.Meas.Value <- c(results_tmp$mre.retro,
                                 results_tmp$mae.retro,
                                 results_tmp$mpe.retro,
                                 results_tmp$mape.retro,
                                 results_tmp$mase.retro,
                                 results_tmp$rmse.retro)

            usePackage("stringr")

            Age <- paste("Age",str_extract(names(results[j]), "[[:digit:]]+"))

            tmp_df$Value <- Perf.Meas.Value

            names(tmp_df)[names(tmp_df)=="Value"] <- Age

            tmp_df

            usePackage("scales")
            tmp_df[,Age] <- comma(round(tmp_df[,Age] ,2))

            tmp_df

       }

       tmp_df

}

n5$MIA <- n5$measures.individual.ages.retro.avgfive(n5$results.individual.ages.retro.predictive.performance.avgfive)

## MIA


##
## Total Age
##


#---------  Naive Forecast (Average of Past 5 Years): retrospective evaluation for the total age ----------------------------------------

n5$total.age.retro.predictive.performance.avgfive <- function(datalist, index){

      index <- 10

      PSY <- datalist[[1]]$CY[length(datalist[[1]]$CY)] + 1

      result <- list()
	    res <- list()
	    individual.ages.retro.plot.info <- list() 
	    
	    for (j in 1:length(datalist)){

          subdata <- subset(datalist[[j]], CY < PSY)

     		  y <- subdata[,ncol(subdata)]
     		  cy <- subdata[,"CY"]

          avgfivefit <- n5$fits[[j]]$model

          usePackage("stringr")

     		  a <- NULL
     		  p <- NULL
     		  e <- NULL

          cy00 <- NULL

          data0 <- NULL
          
          p.bench <- NULL
          e.bench <- NULL


     		  for (i in (index-j+1):(length(y)-1)){

          		y0 <- y[1:i]
              cy0 <- cy[1:i]

   		        model0 <- n5$avgfive(y0)
     		      
              p0 <- as.numeric(model0$mean)
              f0 <- as.numeric(model0$fitted)

              d0 <- data.frame(i=i, cy0, y0, f0, p0, psy=max(cy0)+1, a0=y[i+1]) 
              
              # str(d0)
               
              data0 <- rbind.data.frame(data0, d0)  
              
              ##--- 
              
          		p <- c(p, p0)
          		e0 <- y[i+1] - p0   # actual - predicted
          		e <- c(e, e0)
          		a <- c(a, y[i+1]) #actual

          		cy00 <- c(cy00, cy[i+1])

          		## benchmark: naive forecasting (previous year)

              y0 <- y[1:i]

              usePackage("forecast")

              model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)

              p0.bench <- as.numeric(model0.bench$mean)

              p.bench <- c(p.bench, p0.bench)
              e0.bench <- y[i+1] - p0.bench   # actual - predicted
              e.bench <- c(e.bench, e0.bench)

    		   }

     		   result[[j]] <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)

           individual.ages.retro.plot.info[[j]] <- data0

     		   usePackage("data.table")

     		   res[[j]] <- data.table(cy=cy00, a, p, e, p.bench, e.bench, key="cy")

       }
       
      names(individual.ages.retro.plot.info) <- names(datalist)
       
      leftjoin <- individual.ages.retro.plot.info[[1]][,-1] 
      for (j in 2:length(individual.ages.retro.plot.info)) {
      leftjoin <- dplyr::left_join(leftjoin, 
                    individual.ages.retro.plot.info[[j]][,-1], by=c("cy0","psy"))
      }
        
        
      head(leftjoin)

      usePackage("dplyr")
      usePackage("magrittr")
      
      names(leftjoin) <- make.names(names=names(leftjoin), unique=TRUE, allow_ = TRUE)

      head(leftjoin)

      # actual values extracted retrospectively
      y0 <- leftjoin %>% 
           dplyr::select(matches("y0.")) %>% 
             rowSums(na.rm=FALSE)   # na.rm=TRUE

      ## nrow(leftjoin)
      ## length(y0.sum)

      # fitted values computed retrospectively 
      f0 <- leftjoin %>% 
           dplyr::select(matches("f0.")) %>% 
             rowSums(na.rm=FALSE)   # na.rm=TRUE

      # forecasts computed retrospectively 
      p0 <- leftjoin %>% 
           dplyr::select(matches("p0.")) %>% 
             rowSums(na.rm=FALSE)   # na.rm=T

      # actual value to be compared against forecasted value  
      a0 <- leftjoin %>% 
            dplyr::select(matches("a0.")) %>% 
             rowSums(na.rm=FALSE)   # na.rm=T

      leftjoin.new <- cbind.data.frame(leftjoin[,c("cy0","psy")], y0, f0, p0, a0)

      head(leftjoin.new)

       n5$total.age.retro.plot.info.avgfive <<- leftjoin.new
   
      #------------------------------------------------------------------------ 

      resjoin <- vector("list",length(res))
	    ## res[[1]] <-  res[[1]][-c(1:9), ]  # discard first 10 years of data
      ##                                    # (an adjustment is made, which is why we have 9 here)

	    for (j in 1:length(resjoin)){

     		  DT <- setkey(res[[j]], "cy")
      		DT1 <- DT[J(res[[1]]$cy)]
     		  resjoin[[j]] <- DT1[complete.cases(DT1),]

	    }

      nms <- NULL
      for (j in 1:length(datalist)) {
          nms <- c(nms, paste("T",str_extract(names(datalist[j]),"[[:digit:]]+"),sep=""))
      }

      names(resjoin) <- nms

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


      return(list(a.total.retro = a.total,
                  p.total.retro = p.total,
                  e.total.retro = e.total,
                  mre.total.retro = mre.total,
                  mae.total.retro = mae.total,
                  mpe.total.retro = mpe.total,
                  mape.total.retro = mape.total,
                  mase.total.retro = mase.total,
                  rmse.total.retro = rmse.total,
                  data.retro = resjoin))

}


n5$results.total.age.retro.predictive.performance.avgfive <-
      n5$total.age.retro.predictive.performance.avgfive(n5$datalist, n5$index.year)

n5$results.total.age.retro.predictive.performance.avgfive

## names(results.total.age.retro.predictive.performance.avgfive)



#-------------------------------------------------------------------------------


n5$total.age.retro.plot.avgfive <- function(total.age.retro.plot.info.avgfive, stockabundance){

   .e <- environment()
   
   usePackage("ggplot2")

   mydata <- total.age.retro.plot.info.avgfive

   ggplot(mydata, aes(cy0, y0), environment=.e) + 
   ## ggplot(mydata, aes(cy0, y0)) +    
    geom_line(data=mydata, aes(cy0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) + 
      geom_line(data=mydata, aes(cy0, f0), colour="red") + 
       geom_point(data=mydata, aes(psy, p0), colour="red") + 
        ylab(paste0("Total ", stockabundance)) + 
         xlab(paste0("Return Year")) + 
          ggtitle(paste0("Total ", stockabundance)) + 
           facet_wrap(~psy) + 
            theme_bw() + 
             scale_y_continuous(labels=scales::comma) + 
             theme(plot.title=element_text(size=12, hjust=0.5),
                   axis.text.x = element_text(angle = 90, vjust=0.5, size=8), 
                   axis.text.y = element_text(vjust = 0.5, size=8), 
                   strip.text.x = element_text(size = 9)) 

}


n5$total.age.retro.plot.avgfive(n5$total.age.retro.plot.info.avgfive, n5$stockabundance)

#-------------------------------------------------------------------------------------

### report retrospective performance measures for total age in a nicer format

n5$measures.total.age.retro.avgfive  <- function(results){

       Model <- c("Naive Model (Average of Past 5 Years)", rep("",5))
       Perf.Meas <- c("MRE","MAE","MPE","MAPE","MASE","RMSE")
       Perf.Meas.Value <- c(results$mre.total.retro,
                            results$mae.total.retro,
                            results$mpe.total.retro,
                            results$mape.total.retro,
                            results$mase.total.retro,
                            results$rmse.total.retro)

       tmp_df <- data.frame(Model, Measure=Perf.Meas, Total=Perf.Meas.Value)

       usePackage("scales")
       tmp_df$Total <- comma(round(tmp_df$Total,2))

       tmp_df

       usePackage("data.table")
       tmp_df <- data.table(tmp_df, key="Measure")

       tmp_df$Model[1:length(tmp_df$Model)] <- "Naive Model (Average of Past 5 Years)"

       tmp_df

       return(tmp_df)

}

n5$MTA <- n5$measures.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive)

## M <- merge(MIA, MTA, by = intersect(names(MIA), names(MTA)), sort=FALSE)

n5$M <- merge(n5$MIA, n5$MTA, by = c("Measure"), sort=FALSE)

n5$M <- subset(n5$M, select=-Model.y)

## M

names(n5$M)[names(n5$M)=="Model.x"] <- "Model"

## M

n5$M.avgfive <- n5$M




### report actual, forecasted and error values for total age in a nicer format

n5$afe.total.age.retro.avgfive <- function(results){

     afe.results.total.age.retro.predictive.performance.avgfive <-
     data.frame(CY=results$data.retro[[1]]$cy,
               Actual=results$a.total.retro,
               Forecast=results$p.total.retro,
               Error=results$e.total.retro)

    return(afe.results.total.age.retro.predictive.performance.avgfive)

}

n5$results.afe.total.age.retro.avgfive <-
 n5$afe.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive)

n5$results.afe.total.age.retro.avgfive


## usePackage("scales")
## cbind(results.afe.total.age.retro.avgfive[,1],
##      comma(round(results.afe.total.age.retro.avgfive[,"Actual"])),
##      comma(round(results.afe.total.age.retro.avgfive[,"Forecast"])),
##      comma(round(results.afe.total.age.retro.avgfive[,"Error"]))
##)



#---- meboot2 function for bootstrapping positive time series ----------------------------------------

meboot2 <- function (x, reps = B, trim = 0.1, reachbnd = FALSE, expand.sd = FALSE,

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


n5$forecast.avgfive.modified.meboot <- function(fit, level=80, npaths=B){

   series <- fit$model.data[,ncol(fit$model.data)]

   usePackage("meboot")
   usePackage("forecast")

   #-----
   
   # series.meboot2 <- meboot2(series, reps=npaths, trim=0)$ensemble

   #----
    
   ## series.meboot2 <- meboot2(series, reps=npaths, trim=0)$ensemble   
   
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
   
   #----

    avgfivefcast  <- function(series) {

     n <- length(series)    # n > 5; length of time series

     fcast <- (series[n-4] + series[n-3] + series[n-2] + series[n-1] + series[n])/5  # point-forecast for series[n+1]

     return(fcast)

   }

   series.boot.forecast.avgfive <- apply(series.meboot2,2,avgfivefcast)  ## one-year ahead forecasts for all time series in the ensemble

   y.paths <- series.boot.forecast.avgfive   ## one-year ahead forecasts

   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

   out <- NULL

   out$mean <-  as.numeric(n5$avgfive(series)$mean)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   return(out)

}


## fit <- avgfive.model.fits[[1]]
## forecast.avgfive.modified.meboot(fit, level=80, npaths=999)


n5$forecast.avgfive.modified.stlboot <- function(fit, level=80, npaths=B){
 
   series <- fit$model.data[,ncol(fit$model.data)]
   
   usePackage("forecast")
  
   series <- ts(series, 
                 start = min(fit$model.data$CY), 
                 end = max(fit$model.data$CY), 
                 frequency=1)

   mean(series)
 
   avgfivefcast  <- function(series) {

     n <- length(series)    # n > 5; length of time series

     fcast <- (series[n-4] + series[n-3] + series[n-2] + series[n-1] + series[n])/5  # point-forecast for series[n+1]

     return(fcast)

   }
        
   series.stlboot <- stlboot(series, k=npaths, outplot=FALSE)
           
   #### carry the last value of each bootstrapped time series forward
   #### to obtain the one-year ahead forecast 
   ## series.boot.forecast.avgfive <- series.stlboot[nrow(series.stlboot),]  
  
      #---   handle situations where stlboot produces huge bootstrapped point forecasts 
   
   series.stlboot.range <- diff(range(series.stlboot))
   series.range <- diff(range(series)) 
   
   if (series.stlboot.range > 5*series.range ) {
   
       usePackage("meboot")
       usePackage("forecast")
   
       # series.meboot2 <- meboot2(series, reps=npaths, trim=0)$ensemble
   
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
        
        ## series.boot.forecast.naiveone <- series.meboot2[nrow(series.meboot2),] 
   
        ## series.stlboot <- series.boot.forecast.naiveone
        
        ## y.paths <- series.boot.forecast.naiveone # one-year ahead forecasts 
        
        ## one-year ahead forecasts for all time series in the ensemble
        series.boot.forecast.avgfive <- apply(series.meboot2,2,avgfivefcast)  
        
        y.paths <- series.boot.forecast.avgfive   ## one-year ahead forecasts    
              
   } else {
   
         ## one-year ahead forecasts for all time series in the ensemble
         series.boot.forecast.avgfive <- apply(series.stlboot,2,avgfivefcast)  
                                                                           
         y.paths <- series.boot.forecast.avgfive   ## one-year ahead forecasts 
   
   }
           
   #--- end handling of situation  
  
 
   ## mean(series.boot.forecast.naiveone)
   
   ## rwf(series,h, drift=FALSE, level=level)$mean
    
   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8)) 
    
   out <- NULL 
   
   ## out$mean <-  as.numeric(rwf(series,h=1, drift=FALSE, level=level)$mean)
   out$mean <-  as.numeric(n5$avgfive(series)$mean) 
   
   out$lower <- lower
   out$upper <- upper 
  
   out$sim <- y.paths
   out$series <- out$series 
   out$ensemble <- series.stlboot 
   
   return(out)
   
}


#-----------------------------------------------------------------------------------

n5$prediction.intervals.individual.ages.avgfive <- function(fits, bootmethod, level=80, npaths=B){

     h <- 1  # one step ahead forecasts

     for (j in 1:length(fits)){

          avgfive.fit <- fits[[j]]

          ## avgfiveboot <- forecast.avgfive.modified(avgfive.fit, level=level, npaths=npaths)
          
          if (bootmethod=="stlboot") {
              
              avgfiveboot <- n5$forecast.avgfive.modified.stlboot(avgfive.fit, level=level, npaths=npaths)
              
          }
          
          if (bootmethod=="meboot") {
              
              avgfiveboot <- n5$forecast.avgfive.modified.meboot(avgfive.fit, level=level, npaths=npaths)
          }


          avgfive.point.forecast <- avgfiveboot$mean

          fits[[j]]$avgfive.point.forecast <- avgfive.point.forecast

          avgfive.lwr.forecast <- avgfiveboot$lower
          # avgfive.median.forecast <- median(avgfiveboot$sim)
          avgfive.upr.forecast <- avgfiveboot$upper


          fits[[j]]$PI.ctr <- avgfive.point.forecast
          fits[[j]]$PI.lwr <- avgfive.lwr.forecast
          fits[[j]]$PI.upr <- avgfive.upr.forecast
          # fits[[j]]$PI.median <- avgfive.median.forecast

          fits[[j]]$sim <- avgfiveboot$sim

          fits[[j]]$ensemble <- avgfiveboot$ensemble
     }


     results <- fits

     return(results)
}



n5$fits <- n5$avgfive.model.fits
n5$pred.int.individual.ages.avgfive <- n5$prediction.intervals.individual.ages.avgfive(n5$fits, n5$bootmethod, level=80, npaths=n5$B)

## pred.int.individual.ages.avgfive



#----------------------------------------------------------------------------

n5$PI.ctr <- NULL
n5$PI.lwr <- NULL
n5$PI.upr <- NULL
# PI.med <- NULL
n5$PI.sim <- NULL
n5$nms <- NULL

for (k in 1:length(n5$pred.int.individual.ages.avgfive)){

     n5$PI.ctr <- c(n5$PI.ctr,
                 n5$pred.int.individual.ages.avgfive[[k]]$PI.ctr)

     n5$PI.lwr <- c(n5$PI.lwr,
                 n5$pred.int.individual.ages.avgfive[[k]]$PI.lwr)

     n5$PI.upr <- c(n5$PI.upr,
                 n5$pred.int.individual.ages.avgfive[[k]]$PI.upr)

     # PI.med <- c(PI.med,
     #            pred.int.individual.ages.avgfive)[[k]]$PI.median)

     n5$PI.sim <- cbind(n5$PI.sim, n5$pred.int.individual.ages.avgfive[[k]]$sim)

     n5$nms <- c(n5$nms, n5$pred.int.individual.ages.avgfive[[k]]$age)

}

colnames(n5$PI.sim) <- n5$nms


n5$PI.lwr[n5$PI.lwr < 0] <- 0
n5$PI.upr[n5$PI.upr < 0] <- 0
## PI.med[PI.med < 0] <- 0

n5$PI.ctr <- round(n5$PI.ctr)
n5$PI.lwr <- round(n5$PI.lwr)
n5$PI.upr <- round(n5$PI.upr)
## PI.med <- round(PI.med)


n5$PI.individual.ages.avgfive <- data.frame(PI.ctr=n5$PI.ctr, PI.lwr=n5$PI.lwr, PI.upr=n5$PI.upr)

n5$PI.individual.ages.avgfive.no.comma <- data.frame(PI.ctr=n5$PI.ctr, PI.lwr=n5$PI.lwr, PI.upr=n5$PI.upr)


## PI.individual.ages.avgfive

usePackage("scales")

n5$PI.individual.ages.avgfive <- comma(n5$PI.individual.ages.avgfive)

## PI.individual.ages.avgfive

n5$PI.individual.ages.avgfive.sim <- n5$PI.sim





n5$plot.distribution.bootstrapped.point.forecasts.individual.ages.avgfive <- function(PI.individual.ages.avgfive.sim,
                                                                                   PI.individual.ages.avgfive.no.comma, stockabundance){

    .e = environment()
        
    y.star.boot.stacked <- NULL
    labels.stacked <- NULL
    for (i in 1:ncol(PI.individual.ages.avgfive.sim)) {
    
        y.star.boot.stacked <- c(y.star.boot.stacked,  PI.individual.ages.avgfive.sim[,i])
        mylabel <- paste(stockabundance,"at",colnames(PI.individual.ages.avgfive.sim)[i])
        labels.stacked <-  c(labels.stacked, rep(mylabel, length(PI.individual.ages.avgfive.sim[,i])))

    }

    data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)
                               

    usePackage("ggplot2")
    usePackage("scales")

        
    d <- data.stacked

    l <- levels(d$labels)



    ## myBinwidth <- NULL
    myBreaks <- NULL 
    for (j in 1:length(l)){
        ## h <- hist(data.stacked$y.star.boot[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE)
        hdata <- subset(d, labels==l[j])$y.star.boot
        h <- hist(hdata, plot=FALSE, breaks = "Freedman-Diaconis")
        h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks)))
        #### myBinwidth <-  c(myBinwidth, unique(diff(h$breaks)))
        ## myBreaks[[j]] <- h$breaks
        myBreaks[[j]] <- h.tmp 
    }
    
    
    
    ## g <- ggplot(d, aes(x=y.star.boot),environment=.e) +
    g <- ggplot(d, aes(x=y.star.boot), environment=.e) +
             facet_wrap(~ labels,  scales="free", ncol=1) +
              mapply(function(d, b) {geom_histogram(data=d, breaks=b, fill="wheat",colour="black")},
                     split(d, d$labels), myBreaks)  +  
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Bootstrapped Point Forecasts"),labels=comma, breaks = scales::pretty_breaks(n = 10))


      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))

     clim <- NULL
     for (j in 1:length(l)){

        tmp <- PI.individual.ages.avgfive.no.comma[j,"PI.ctr"]
        tmp <- round(tmp)
        clim <- c(clim, tmp)

     }

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)


     x <- NULL
     xend <- NULL
     y <- NULL
     yend <- NULL
     for (j in 1:length(l)){
         x <- c(x, max(0,PI.individual.ages.avgfive.no.comma[j,"PI.lwr"]))
         xend <- c(xend, PI.individual.ages.avgfive.no.comma[j,"PI.upr"])
         y <- c(y,0)
         yend <- c(yend,0)
     }

     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)

     return(g)
}



## plot.distribution.bootstrapped.point.forecasts.individual.ages.avgfive(PI.individual.ages.avgfive.sim,
##                                                                        PI.individual.ages.avgfive.no.comma, stockabundance)


n5$plot.distribution.bootstrapped.point.forecasts.individual.ages.avgfive(n5$PI.individual.ages.avgfive.sim,
                                                                       n5$PI.individual.ages.avgfive.no.comma, 
                                                                       n5$stockabundance)



############################################################################################
#*******************************************************************************************
#
#------------ compute prediction interval for point forecast of total age       -----------
#
#*******************************************************************************************

n5$avgfive.sim.total.age <- NULL
n5$nms <- NULL
n5$avgfive.PI.ctr.total.age <- 0
for (k in 1:length(n5$pred.int.individual.ages.avgfive)){
     n5$avgfive.sim.total.age <- cbind(n5$avgfive.sim.total.age, n5$pred.int.individual.ages.avgfive[[k]]$sim)
     n5$nms <- c(n5$nms, n5$pred.int.individual.ages.avgfive[[k]]$age)
     n5$avgfive.PI.ctr.total.age <- n5$avgfive.PI.ctr.total.age + n5$pred.int.individual.ages.avgfive[[k]]$PI.ctr
}

colnames(n5$avgfive.sim.total.age) <- n5$nms

n5$PI.total.age.avgfive <- NULL


n5$sim <- apply(n5$avgfive.sim.total.age, 1, sum)

n5$PI.total.age.avgfive$PI.ctr <- n5$avgfive.PI.ctr.total.age
## PI.total.age.avgfive$PI.med <- quantile(sim, 0.500)
n5$PI.total.age.avgfive$PI.lwr <- quantile(n5$sim, 0.10, type=8)  # need to automate this!
n5$PI.total.age.avgfive$PI.upr <- quantile(n5$sim, 0.90, type=8)  # need to automate this!

n5$PI.total.age.avgfive <- data.frame(n5$PI.total.age.avgfive)

## names(PI.total.age.avgfive) <- c("PI.ctr","PI.med","PI.lwr","PI.upr")
names(n5$PI.total.age.avgfive) <- c("PI.ctr","PI.lwr","PI.upr")

## PI.total.age.avgfive

rownames(n5$PI.total.age.avgfive) <- NULL

n5$PI.total.age.avgfive


#-----------------------------------------------------------------------------------------------


n5$PI.total.age.avgfive$PI.lwr[n5$PI.total.age.avgfive$PI.lwr < 0] <- 0
n5$PI.total.age.avgfive$PI.upr[n5$PI.total.age.avgfive$PI.upr < 0] <- 0
## PI.total.age.avgfive$PI.med[PI.total.age.avgfive$PI.med < 0] <- 0

n5$PI.total.age.avgfive$PI.ctr <- round(n5$PI.total.age.avgfive$PI.ctr)
n5$PI.total.age.avgfive$PI.lwr <- round(n5$PI.total.age.avgfive$PI.lwr)
n5$PI.total.age.avgfive$PI.upr <- round(n5$PI.total.age.avgfive$PI.upr)
## PI.total.age.avgfive$PI.med <- round(PI.total.age.avgfive$PI.med)

usePackage("scales")

n5$PI.total.age.avgfive.no.comma <- n5$PI.total.age.avgfive

n5$PI.total.age.avgfive$PI.ctr <- comma(n5$PI.total.age.avgfive$PI.ctr)

n5$PI.total.age.avgfive$PI.lwr <- comma(n5$PI.total.age.avgfive$PI.lwr)

n5$PI.total.age.avgfive$PI.upr <- comma(n5$PI.total.age.avgfive$PI.upr)

## PI.total.age.avgfive$PI.med <- comma(PI.total.age.avgfive$PI.med)

## PI.total.age.avgfive

n5$PI.total.age.avgfive.sim <- n5$sim



##########################################################################################
#
#  Plot distribution of bootstrapped point forecasts - total age
#
##########################################################################################



###
### Histogram of Bootstrap Predictions
###

n5$plot.distribution.bootstrapped.point.forecasts.total.age.avgfive <- function(PI.total.age.avgfive.sim, PI.total.age.avgfive.no.comma, stockabundance){

    .e = environment()


    # naive model (previous year)

    y.star.boot.stacked <- PI.total.age.avgfive.sim
    mylabel <- paste("Total", stockabundance)
    labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

    data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)


    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)

    h <- hist(data.stacked$y.star.boot,plot=FALSE, breaks = "Freedman-Diaconis")
    h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks)))
    ## breaks <- h$breaks
    breaks <- h.tmp 

    ## g <- ggplot(d, aes(x=residuals), environment=.e) +
    g <- ggplot(d, aes(x=y.star.boot), environment=.e) +
           geom_histogram(data=d, breaks=breaks, fill="wheat",colour="black") +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               ## expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Bootstrapped Point Forecasts"),labels=comma, breaks = scales::pretty_breaks(n = 10))

    g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


     ## point forecast annotation 
     
     tmp <- PI.total.age.avgfive.no.comma[["PI.ctr"]]
     clim <- round(tmp)
      

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)


     ## interval forecast annotation

     x <- max(0,PI.total.age.avgfive.no.comma[["PI.lwr"]])
     xend <-  round(PI.total.age.avgfive.no.comma[["PI.upr"]])
     y <- 0
     yend <- 0

     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)



     return(g)
}



n5$plot.distribution.bootstrapped.point.forecasts.total.age.avgfive(n5$PI.total.age.avgfive.sim, 
                                                                  n5$PI.total.age.avgfive.no.comma, 
                                                                  n5$stockabundance)


#-----------------------------------------------------------------------------------------
# Scatter plot of retrospectively forecasted versus actual values of abundance (individual ages, avgfive)
#-----------------------------------------------------------------------------------------


n5$scatter.plot.results.afe.individual.ages.retro.avgfive <- function(results){

    .e = environment()

    forecasted.stacked <- NULL
    actual.stacked <- NULL
    age.stacked <- NULL
    labels.stacked <- NULL

    ## R.squared <- NULL

    for (i in 1:length(results)){

       ## avgthreefit <- fits[[i]]$model

       usePackage("stringr")

       data <- results[[i]]
       names(data)[names(data)=="a.retro"] <- "Actual"
       names(data)[names(data)=="p.retro"] <- "Forecast"
       names(data)[names(data)=="e.retro"] <- "Error"

       ## r.sq <- summary(lm(Forecast ~ Actual, data=data))$r.squared
       ## r.sq <-  sprintf("%.2f", r.sq*100)

       ## R.squared <- c(R.squared, r.sq)

       mytitle <- names(results)[i]

       usePackage("stringr")
       mytitle <- str_replace_all(mytitle, pattern="age", replacement="Age ")
       mytitle <- substr(mytitle, start=1, stop=5)

       #### mytitle <- paste(mytitle, ": ", "Naive (Last Year)", sep="")
	     ## mytitle <- paste(mytitle, ":  ","R-squared = ", r.sq , "%", sep="")

    	 usePackage("calibrate")
    	 labs <- substr(data$data.retro$cy,
               start=3, stop=4)



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

    g <- ggplot(data.stacked, aes(actual,forecasted),environment=.e)  +
       geom_abline(intercept=0,slope=1,colour="red",size=0.8) +
       geom_text(aes(label=labels),col="blue",size=3) +
            coord_fixed(ratio=1) +
              facet_wrap(~age, scales="free",ncol=2) +
                expand_limits(x=0, y=0) +
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma) +
                   scale_y_continuous(paste("Forecasted", n5$stockabundance, "Values"),labels=comma) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous(paste("Actual", n5$stockabundance, "Values"),labels=comma) +
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

n5$results <- n5$results.individual.ages.retro.predictive.performance.avgfive

## scatter.plot.results.afe.individual.ages.retro.avgfive(
##   ## avgfive.model.fits,
##   results.individual.ages.retro.predictive.performance.avgfive
##   )



#-----------------------------------------------------------------------------------------
# Time series plot of retrospectively forecasted and actual values of abundance (individual ages, avgthree)
#-----------------------------------------------------------------------------------------

n5$scatter.plot.results.afe.total.age.retro.avgfive <- function(results){

    .e = environment()

    data <- results
    names(data)[names(data)=="a.total.retro"] <- "Actual"
    names(data)[names(data)=="p.total.retro"] <- "Forecast"
    names(data)[names(data)=="e.total.retro"] <- "Error"

    CY <- data$data.retro[[1]]$cy

    usePackage("calibrate")
    labs <- substr(CY,
               start=3, stop=4)

    ## r.sq <- summary(lm(data$Forecast ~ data$Actual))$r.squared
    ## r.sq <-  sprintf("%.2f", r.sq*100)

    data.stacked <- data.frame(forecasted=data$Forecast,
                               actual=data$Actual,
                               labels=labs,
                               # age=paste("Total ", stockabundance, ":  ", "R-squared = ",r.sq,"%",sep="")
                               age=paste("Total ", n5$stockabundance, sep=""))
                               
    usePackage("scales")
    usePackage("ggplot2")

    g <- ggplot(data.stacked, aes(actual,forecasted), environment=.e) +
       geom_abline(intercept=0,slope=1,colour="red",size=0.8) +
       geom_text(aes(label=labels),col="blue",size=3) +
              facet_wrap(~age, scales="free",ncol=2) +
                expand_limits(x=0, y=0) +
                 coord_fixed(ratio=1)  +
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                  scale_y_continuous(paste("Forecasted", n5$stockabundance, "Values"),labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                  scale_x_continuous(paste("Actual", n5$stockabundance, "Values"),labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                   coord_fixed(ratio=1)   +
                 theme_bw() +
                 # theme(axis.title.x=element_text(vjust=-0.5),axis.title.y=element_text(vjust=1.5))
                theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8))
      return(g)


}


n5$results <- n5$results.total.age.retro.predictive.performance.avgfive
n5$scatter.plot.results.afe.total.age.retro.avgfive(n5$results)



#-----------------------------------------------------------------------------------------
# Time series plot of retrospectively forecasted and actual values of abundance (individual ages, avgfive)
#-----------------------------------------------------------------------------------------

n5$timeseries.plot.results.afe.individual.ages.retro.avgfive <- function(results, stockabundance){
 
    .e = environment()
 
    forecasted.stacked <- NULL
    actual.stacked <- NULL
    age.stacked <- NULL
    labels.stacked <- NULL
    
    for (i in 1:length(results)){

       usePackage("stringr")

       data <- results[[i]]
       names(data)[names(data)=="a.retro"] <- "Actual"       
       names(data)[names(data)=="p.retro"] <- "Forecast"  
       names(data)[names(data)=="e.retro"] <- "Error"
      
       mytitle <- names(results)[i]
       
       usePackage("stringr")
       mytitle <- str_replace_all(mytitle, pattern="age", replacement="Age ")
       mytitle <- substr(mytitle, start=1, stop=5)
       
	     ## mytitle <- paste(mytitle, ":  ","R-squared = ", r.sq , "%", sep="") 

    	 ## usePackage("calibrate") 
    	 ## labs <- substr(data$data.retro$cy, 
       ##        start=3, stop=4)
       labs <- data$data.retro$cy
       
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


## results <- results.individual.ages.retro.predictive.performance.avgfive
## timeseries.plot.results.afe.individual.ages.retro.avgfive(results, stockabundance)


n5$timeseries.plot.results.afe.individual.ages.retro.avgfive(n5$results.individual.ages.retro.predictive.performance.avgfive, 
                                                          n5$stockabundance)



#*******************************************************************************
# Time series plot of forecasted vs. actual abundance (total age, avgfive)
#
#*******************************************************************************


n5$timeseries.plot.results.afe.total.age.retro.avgfive <- function(results, stockabundance){

    .e = environment()
      
    data <- results
    names(data)[names(data)=="a.total.retro"] <- "Actual"       
    names(data)[names(data)=="p.total.retro"] <- "Forecast"  
    names(data)[names(data)=="e.total.retro"] <- "Error"
   
    CY <- data$data.retro[[1]]$cy

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
                  theme(plot.title=element_text(size=12, hjust=0.5),
                         axis.title.x=element_text(size=10,vjust=-0.5),
                         axis.title.y=element_text(size=10,vjust=1.5),
                         axis.text.y=element_text(size=8),
                         axis.text.x = element_text(angle = 90, vjust = 0.5, size=8),   # hjust=1 
                         legend.position="top")
                           
      return(g)
  

}


## results <- results.total.age.retro.predictive.performance.avgfive
## timeseries.plot.results.afe.total.age.retro.avgfive(results, stockabundance)

n5$timeseries.plot.results.afe.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive, 
                                                    n5$stockabundance)




#*******************************************************************************
# Histogram of Retrospective Forecast Errors - Individual Ages
#*******************************************************************************

###
### Histogram of Retrospective Forecast Errors: Individual Ages
###

# hist.results.afe.individual.ages.retro.avgfive

n5$dens.results.afe.individual.ages.retro.avgfive <- function(fits, results){

    .e = environment()
  

    errors.stacked <- NULL
    labels.stacked <- NULL

      for (j in 1:length(results)){
       
            data <- results[[j]]
            names(data)[names(data)=="a.retro"] <- "Actual"       
            names(data)[names(data)=="p.retro"] <- "Forecast"  
            names(data)[names(data)=="e.retro"] <- "Error"

            agelab <- names(fits)[j]
            usePackage("stringr")
            
            agelab <- str_replace(string=agelab,pattern="age",replacement="Age ")

            errors.stacked <- c(errors.stacked, data$Error)
            mylabel <- agelab
            mylabel <- paste0(n5$stockabundance, " at ", mylabel)
            labels.stacked <- c( labels.stacked, rep(mylabel, length(data$Error)))

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
    ## for (j in 1:length(fits)){
    ##    h <- hist(data.stacked$errors[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE, breaks = "Freedman-Diaconis")
    ##    breaks[[j]] <- h$breaks
    ##    
    ## }

    #  environment=.e
    g <- ggplot(d, aes(x=errors), environment=.e) +
           ## mapply(function(d, b) {geom_histogram(data=d, breaks=b, fill="lightblue",colour="black")},
           ## split(d, d$labels), breaks) +
           geom_density(data=d, fill="lightblue",colour="black") +
             facet_wrap(~ labels,  scales="free", ncol=1) +
               expand_limits(x=0, y=0) +
                  scale_y_continuous(paste("Frequency"),labels=comma) +
                   scale_x_continuous(paste("Retrospective Forecast Errors"),labels=comma, breaks = scales::pretty_breaks(n = 10))

      g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
                
     clim <- NULL
     for (j in 1:length(fits)){
     
        clim <- c(clim, 0)

     }
     
     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(aes(xintercept = z), dummy2, linetype="dashed",col="red", size=0.8)
    

     return(g)
}

## fits <- avgfive.model.fits
## results <- results.individual.ages.retro.predictive.performance.avgfive


## par(mfrow=c(4,1), mar=c(4,4,3,2))
## hist.results.afe.individual.ages.retro.avgfive(
##    avgfive.model.fits,
##    results.individual.ages.retro.predictive.performance.avgfive
## )


## dens.results.afe.individual.ages.retro.avgfive(avgfive.model.fits, 
## results.individual.ages.retro.predictive.performance.avgfive)

#*******************************************************************************
#
# histogram of retrospective forecast errors (total age)
#
#*******************************************************************************

# hist.results.afe.total.age.retro.avgfive

n5$dens.results.afe.total.age.retro.avgfive <- function(results){

  .e <- environment()

	## usePackage("KernSmooth")
	usePackage("ggplot2")
	usePackage("scales")
	
	# results$age <- "Total Terminal Run"
	results$age <- paste("Total", n5$stockabundance) 

	## b <- dpih(results$Error)

	g <- ggplot(results, aes(Error), environment=.e) + 
  	      ## geom_histogram(fill="peachpuff",colour="black", binwidth=b) + 
  	      geom_density(fill="lightblue",colour="black") + 
  	       geom_vline(xintercept=0,colour="red",linetype=2,size=1.2) + 
              facet_wrap(~age, ncol=1) + 
    	         scale_y_continuous("Frequency") + 
    	          scale_x_continuous("Retrospective Forecast Error", labels=comma, 
                   limits=c(-max(abs(results$Error)),max(abs(results$Error))), breaks = scales::pretty_breaks(n = 10)) + 
    	           # scale_x_continuous("Retrospective Forecast Error", labels=comma,
    	             #   limits=c(-max(abs(results$Error)),max(abs(results$Error)))) + 
    	             theme_bw() +
    	             theme(plot.title=element_text(size=12, hjust=0.5),
                         axis.text.x=element_text(size=8),
                         axis.text.y=element_text(size=8),
                         axis.title.x=element_text(size=10),
                         axis.title.y=element_text(size=10))

      return(g)

}

## dens.results.afe.total.age.retro.avgfive(results.afe.total.age.retro.avgfive)


## results <- results.afe.total.age.retro.avgfive

## hist.results.afe.total.age.retro.avgfive(results)



#*******************************************************************************
#  barplot forecasted values (specific ages)
#
#*******************************************************************************


n5$barplot.forecasted.values.individual.ages.avgfive <- function(fits, pointforecasts, i){

        .e <- environment()

        ages <- names(fits)

        ### starts here

        age <- ages[i]

        usePackage("stringr")

        age <- str_replace(age, "age","Age ")

        age

        years <- fits[[i]]$model.data$CY

        retropointforecasts <-  fits[[i]]$model.data[,3]  # historic data ?

        p <-  as.numeric(pointforecasts[[i]]$p)

        ## p <- round(p)

        pointforecast <- round(p,2)

        forecastingyear <- pointforecasts[[i]]$RY


        dfretro <- data.frame(years,retropointforecasts)

        dffor <- data.frame(forecastingyear,pointforecast)


        gp1 <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(dfretro$retropointforecasts)))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        # scale_y_continuous(expand=c(0.01, 0.5)) +
        geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear-1/3,
                xmin=forecastingyear+1/3,
                ymax=pointforecast,
                ymin=0),fill="violetred2") +
        geom_text(data=dffor,aes(x=forecastingyear,
               y=pointforecast,
               label=paste(comma(round(dffor$pointforecast)))),
               colour="violetred2",angle=90,size=2.5,hjust=0,vjust=0.2) +
          geom_segment(data=dfretro,aes(x = years[1]-0.5,
                     y = mean(dfretro$retropointforecasts),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retropointforecasts)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), size=2.5, colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(n5$stockabundance),label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        labs(title=paste(age)) +
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
            panel.background = element_rect(fill='white',colour="grey50"))  +
             expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retropointforecasts),environment=.e) +
      geom_line(data=dfretro, aes(years,retropointforecasts),col="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(round(pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
      scale_y_continuous(paste(n5$stockabundance),labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), size=2.5,colour="grey15",hjust=0,vjust=0) +
     labs(title=paste(age)) +
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
            panel.background = element_rect(fill='white',colour="grey50")) +
     expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))

     ## print(gp2)

     usePackage("grid")
     usePackage("gridExtra")
     usePackage("ggplot2")

     #### plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     #### do.call("grid.arrange", c(plotlist,ncol=1))

     ## p <- arrangeGrob(gp1,gp2) 
     p <- grid.arrange(gp1,gp2) 
     
     p  

     ## }

     ## require(gridExtra)
     ## do.call("grid.arrange", c(list_graphs, ncol=1))

}


n5$fits <- n5$avgfive.model.fits

n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)

## barplot.forecasted.values.individual.ages.avgfive(fits, pointforecasts,i=1)


#*******************************************************************************
#  barplot forecasted values (total age)
#
#*******************************************************************************

n5$barplot.forecasted.values.total.age.avgfive <- function(results, pointforecasts){

      .e <- environment()

      tabledata <- data.frame(cy = results$data.retro[[1]]$cy,
                             a = results$a.total.retro,
                             p = results$p.total.retro,
                             e = results$e.total.retro)

      p <- NULL
      for (k in 1:length(pointforecasts)){
          p <- c(p, round(pointforecasts[[k]]$p))
      }


       years <- tabledata$cy

       retropointforecasts <-  tabledata$a   ## historic values

       pointforecast <- round(sum(p),2)


       ## cy <- pointforecasts[[1]]$CY

       forecastingyear <- pointforecasts[[1]]$RY


        dfretro <- data.frame(years,retropointforecasts)

        dffor <- data.frame(forecastingyear,pointforecast)


        gp1 <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
             geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="dodgerblue3") +
        geom_text(data=dfretro,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(dfretro$retropointforecasts))),
               colour="dodgerblue3",angle=90,size=2.5,hjust=0,vjust=0.2) +
        # scale_y_continuous(expand=c(0.01, 0.5)) +
        geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear-1/3,
                xmin=forecastingyear+1/3,
                ymax=pointforecast,
                ymin=0),fill="violetred2") +
        geom_text(data=dffor,aes(x=forecastingyear,
               y=pointforecast,
               label=paste(comma(dffor$pointforecast))),
               colour="violetred2",angle=90,size=2.5,hjust=0,vjust=0.2) +
          geom_segment(data=dfretro,aes(x = years[1]-0.5,
                     y = mean(dfretro$retropointforecasts),
                     xend = forecastingyear+0.5,
                     yend = mean(dfretro$retropointforecasts)),
                     colour="grey15",linetype=1) +
        annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), size=2.5,colour="grey15",hjust=0,vjust=0) +
        scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
        ## scale_y_continuous("Terminal Run",label=comma,breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        scale_y_continuous(paste(n5$stockabundance),label=comma,breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        ## labs(title="Total Terminal Run") +
        labs(title=paste("Total", n5$stockabundance)) +
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
            panel.background = element_rect(fill='white',colour="grey50"))  +
              expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))


      gp2 <- ggplot(data=dfretro, aes(years,retropointforecasts),environment=.e) +
      geom_line(data=dfretro, aes(years,retropointforecasts),colour="dodgerblue3") +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="red",linetype=2,size=0.7) +
      geom_point(data=dfretro, aes(years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear,
                   y=pointforecast,
                   label=paste(comma(pointforecast))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=90,hjust=-0.5,vjust=0,size=2.5) +
      geom_segment(data=dfretro,aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      ## scale_y_continuous("Terminal Run",labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
      scale_y_continuous(paste(n5$stockabundance),labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +
      annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), size=2.5,colour="grey15",hjust=0,vjust=0) +
     ## labs(title="Total Terminal Run") +
     labs(title=paste("Total", n5$stockabundance)) +
     theme(plot.title=element_text(size=12, hjust=0.5),
           axis.text.x=element_text(size=8,angle=90,hjust=1,vjust=0.5),
           axis.text.y=element_text(size=8),
           axis.title.x=element_text(size=10, vjust=-0.5),
           axis.title.y=element_text(size=10, vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50")) +
      expand_limits(y=1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast)))

     ## print(gp2)

     usePackage("grid")
     usePackage("gridExtra")
     usePackage("ggplot2")

     ## plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     ## do.call("grid.arrange", c(plotlist,ncol=1))

     ## p <- arrangeGrob(gp1,gp2) 
     p <- grid.arrange(gp1,gp2) 
     
     p  

}



n5$results <- n5$results.total.age.retro.predictive.performance.avgfive

n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)

## barplot.forecasted.values.total.age.avgfive(results, pointforecasts)




#*******************************************************************************
#
# Gary's Plot for Individual Ages
#
#*******************************************************************************


n5$gary.plot.individual.ages <- function(results.retro, results.pred, j){

       ## start from here

       .e <- environment()

       retro <- results.retro[[j]]
       pred <- results.pred[[j]]

       age <- names(results.retro)[j]
       usePackage("stringr")

       age <- str_replace(age, "age","Age ")

       age

       years <- retro$data.retro$cy
       retropointforecasts <- retro$e.retro  ## these are forecast errors
       retropointforecasts <- round(retropointforecasts,2)
       dfretro <- data.frame(years,retropointforecasts)

       dfretropos <- subset(dfretro,retropointforecasts>=0)
       dfretroneg <- subset(dfretro,retropointforecasts<0)


       pointforecast <- round(pred$PI.ctr,2)
       lower <- round(pred$PI.lwr,2)
       upper <- round(pred$PI.upr,2)
       lower <- max(0,lower)
       forecastingyear <-  max(years) + 1    #### Come back here to automate!!!

       dffor <- data.frame(forecastingyear,pointforecast,lower,upper)

       usePackage("ggplot2")
       usePackage("scales")


      if (pointforecast != lower & pointforecast != upper) {

        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
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
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=3) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",label=comma,
                        limits=c( 1.4*min(min(dfretro$retropointforecasts,dffor$forpointforecast)),
                                   1.4*max(max(dfretro$retropointforecasts,dffor$forpointforecast)) ) ,
                        expand=c(0,0) ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste(n5$stockabundance,"at",age)) +
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

        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
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
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=3) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",label=comma,
                        limits=c( 1.4*min(min(dfretro$retropointforecasts,dffor$forpointforecast)),
                                   1.4*max(max(dfretro$retropointforecasts,dffor$forpointforecast)) ) ,
                        expand=c(0,0) ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste(n5$stockabundance,"at",age)) +
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

        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
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
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=3) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",label=comma,
                        limits=c( 1.4*min(min(dfretro$retropointforecasts,dffor$forpointforecast)),
                                   1.4*max(max(dfretro$retropointforecasts,dffor$forpointforecast)) ) ,
                        expand=c(0,0) ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste(n5$stockabundance, "at",age)) +
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

        gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts), environment=.e) +
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
        geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=3) +
        geom_hline(yintercept=0,colour="grey90") +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
        scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",label=comma,
                        limits=c( 1.4*min(min(dfretro$retropointforecasts,dffor$forpointforecast)),
                                   1.4*max(max(dfretro$retropointforecasts,dffor$forpointforecast)) ) ,
                        expand=c(0,0) ) +
        ## labs(title=paste("Terminal Run at",age)) +
        labs(title=paste(n5$stockabundance, "at",age)) +
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




     return(gp)





} # end gary.plot


n5$results.retro <- n5$results.individual.ages.retro.predictive.performance.avgfive
## names(results.retro)

n5$results.pred <- n5$pred.int.individual.ages.avgfive

## par(mfrow=c(1,1))
## j <- 1

n5$gary.plot.individual.ages(n5$results.retro, n5$results.pred, j=1)


#*******************************************************************************
#
# Gary's Plot for "Total Age"
#
#*******************************************************************************


n5$gary.plot.total.age <- function(results.retro, results.pred){

       .e <- environment()

       retro <- results.retro
       pred <- results.pred


       years <- retro$data.retro[[1]]$cy
       retropointforecasts <- retro$e.total.retro  ## these are forecast errors
       retropointforecasts <- round(retropointforecasts,2)
       dfretro <- data.frame(years,retropointforecasts)

       dfretropos <- subset(dfretro,retropointforecasts>=0)
       dfretroneg <- subset(dfretro,retropointforecasts<0)


       pointforecast <- round(as.numeric(pred$PI.ctr),2)
       lower <- round(as.numeric(pred$PI.lwr),2)
       upper <- round(as.numeric(pred$PI.upr),2)
       lower <- max(0,lower)
       forecastingyear <-  max(years) + 1    #### Come back here to automate!!!

       dffor <- data.frame(forecastingyear,pointforecast,lower,upper)

       usePackage("ggplot2")
       usePackage("scales")

       if (pointforecast != lower & pointforecast != upper) {
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
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=lower,xend=forecastingyear+1/3,yend=lower),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=upper,xend=forecastingyear+1/3,yend=upper),colour="red",size=1) +
          geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=3) +
          geom_hline(yintercept=0,colour="grey90") +
          scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
          scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",label=comma,
                        limits=c( 1.3*min(min(dfretro$retropointforecasts,dffor$forpointforecast)),
                                   1.3*max(max(dfretro$retropointforecasts,dffor$forpointforecast)) ) ,  expand=c(0,0) ) +
          ## labs(title=paste("Total Terminal Run")) +
          labs(title=paste("Total",n5$stockabundance)) +
          theme(plot.title=element_text(size=12, hjust=0.5),
              axis.text.x=element_text(size=8, angle=90,hjust=1,vjust=0.5),
              axis.text.y=element_text(size=8),
              axis.title.x=element_text(size=10, vjust=-0.5),
             axis.title.y=element_text(size=10,vjust=1.5),
              panel.grid = element_blank(),
              panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text=element_text(colour="black"),
            panel.background = element_rect(fill='white',colour="grey50"))
        }


        if (pointforecast == lower & pointforecast != upper) {
          gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
          geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
          geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=3,hjust=0,vjust=0.2) +
          geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=3,hjust=1,vjust=0.2) +
          # geom_text(data=dffor,aes(x=forecastingyear+1/3,
          #     y=pointforecast,
          #     label=paste(comma(round(pointforecast)))),
          #     colour="red",angle=0,size=3,hjust=0) +
          geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=3,hjust=0,vjust=1) +
          geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=3,hjust=0,vjust=-0.4)  +
          geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
          geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=lower,xend=forecastingyear+1/3,yend=lower),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=upper,xend=forecastingyear+1/3,yend=upper),colour="red",size=1) +
          geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=3) +
          geom_hline(yintercept=0,colour="grey90") +
          scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
          scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",label=comma,
                        limits=c( 1.3*min(min(dfretro$retropointforecasts,dffor$forpointforecast)),
                                   1.3*max(max(dfretro$retropointforecasts,dffor$forpointforecast)) ) ,  expand=c(0,0) ) +
          ## labs(title=paste("Total Terminal Run")) +
          labs(title=paste("Total", n5$stockabundance)) +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.text.x=element_text(size=8, angle=90,hjust=1,vjust=0.5),
                axis.text.x=element_text(size=8),
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
          gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
          geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
          geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=3,hjust=0,vjust=0.2) +
          geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=3,hjust=1,vjust=0.2) +
          # geom_text(data=dffor,aes(x=forecastingyear+1/3,
          #     y=pointforecast,
          #     label=paste(comma(round(pointforecast)))),
          #     colour="red",angle=0,size=3,hjust=0) +
          geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=lower,
               label=paste(comma(round(lower)))),
               colour="red",angle=0,size=3,hjust=0,vjust=1) +
          geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=upper,
               label=paste(comma(round(upper)))),
               colour="red",angle=0,size=3,hjust=0,vjust=-0.4)  +
          geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
          geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=lower,xend=forecastingyear+1/3,yend=lower),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=upper,xend=forecastingyear+1/3,yend=upper),colour="red",size=1) +
          geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=3) +
          geom_hline(yintercept=0,colour="grey90") +
          scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
          scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",label=comma,
                        limits=c( 1.3*min(min(dfretro$retropointforecasts,dffor$forpointforecast)),
                                   1.3*max(max(dfretro$retropointforecasts,dffor$forpointforecast)) ) ,  expand=c(0,0) ) +
          ## labs(title=paste("Total Terminal Run")) +
          labs(title=paste("Total", n5$stockabundance)) +
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


        if (pointforecast == lower & pointforecast == upper) {
          gp <- ggplot(data=dfretro, aes(x=years, y=retropointforecasts),environment=.e) +
          geom_rect(aes(xmax=years-1/3,
                xmin=years+1/3,
                ymax=retropointforecasts,
                ymin=0),fill="darkgrey") +
          geom_text(data=dfretropos,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=3,hjust=0,vjust=0.2) +
          geom_text(data=dfretroneg,aes(x=years,
               y=retropointforecasts,
               label=paste(comma(round(retropointforecasts)))),
               colour="blue",angle=90,size=3,hjust=1,vjust=0.2) +
          geom_text(data=dffor,aes(x=forecastingyear+1/3,
               y=pointforecast,
               label=paste(comma(round(pointforecast)))),
               colour="red",angle=0,size=3,hjust=0) +
          # geom_text(data=dffor,aes(x=forecastingyear+1/3,
          #     y=lower,
          #     label=paste(comma(round(lower)))),
          #     colour="red",angle=0,size=3,hjust=0) +
          # geom_text(data=dffor,aes(x=forecastingyear+1/3,
          #     y=upper,
          #     label=paste(comma(round(upper)))),
          #     colour="red",angle=0,size=3,hjust=0)  +
          geom_vline(xintercept=dffor$forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) +
          geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=lower,xend=forecastingyear+1/3,yend=lower),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=upper,xend=forecastingyear+1/3,yend=upper),colour="red",size=1) +
          geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=3) +
          geom_hline(yintercept=0,colour="grey90") +
          scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
          scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",label=comma,
                        limits=c( 1.3*min(min(dfretro$retropointforecasts,dffor$forpointforecast)),
                                   1.3*max(max(dfretro$retropointforecasts,dffor$forpointforecast)) ) ,  expand=c(0,0) ) +
          # labs(title=paste("Total Terminal Run")) +
          labs(title=paste("Total", n5$stockabundance)) +
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


     return(gp)

} # end gary.plot


n5$results.retro <- n5$results.total.age.retro.predictive.performance.avgfive
## names(results.retro)
n5$results.pred <- n5$PI.total.age.avgfive.no.comma

n5$gary.plot.total.age(n5$results.retro, n5$results.pred)



###################################################################################################################################
#**********************************************************************************************************************************
#
#---- plot forecasted values & forecast intervals:  scatterplot (individual ages) -------------------------------------------------
#
#**********************************************************************************************************************************
###################################################################################################################################

n5$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.avgfive <- function(fits, pointforecasts, intervalforecasts,i){

     .e <- environment()

     ages <- names(fits)

     ## par(mfrow=c(length(fits),1),mar=c(4,4,3,2), oma=c(2,2,2,6),cex.main=1.1)

     ## for (i in 1:length(fits)) {

        ### starts here

        age <- ages[i]

        usePackage("stringr")

        age <- str_replace(age, "age","Age ")

        age

        years <- fits[[i]]$model.data$CY

        retropointforecasts <-  fits[[i]]$model.data[,3]  # historic data ?

        p <-  as.numeric(pointforecasts[[i]]$p)

        ## p <- round(p)

        pointforecast <- round(p,2)

        forecastingyear <- pointforecasts[[i]]$RY


        dfretro <- data.frame(years,retropointforecasts)


        upper <- as.numeric(intervalforecasts[i,"PI.lwr"])
        lower <- as.numeric(intervalforecasts[i,"PI.upr"])

        dffor <- data.frame(forecastingyear,pointforecast,upper,lower)


    gp <- ggplot(data=dfretro, aes(years,retropointforecasts), environment=.e) +
      geom_line(colour="dodgerblue3") +
      geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
                xmin=forecastingyear-1/3,
                ymax=upper,
                ymin=lower),fill="lightgrey") +
      geom_segment(data=dffor, aes(x = forecastingyear,
                     y = lower,
                     xend = forecastingyear,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = lower,
                     xend = forecastingyear+1/3,
                     yend = lower),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = upper,
                     xend = forecastingyear+1/3,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dfretro, aes(x = dfretro$years[length(dfretro$years)],
                     y = dfretro$retropointforecasts[length(dfretro$retropointforecasts)],
                     xend = dffor$forecastingyear,
                     yend = dffor$pointforecast),
                     colour="violetred2",linetype=3,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,
                   y=pointforecast,
                   ## label=paste(comma(sprintf("%.2f",pointforecast)))),
                   label=paste(comma(round(pointforecast)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   ## label=paste(comma(sprintf("%.2f",lower)))),
                   label=paste(comma(round(lower)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   ## label=paste(comma(sprintf("%.2f",upper)))),
                   label=paste(comma(round(upper)))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=dffor$forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") +
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(n5$stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      labs(title=paste(age)) +
      theme( plot.title=element_text(size=12, hjust=0.5),
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

     return(gp)





}


n5$fits <- n5$avgfive.model.fits
n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)
n5$intervalforecasts <-   n5$PI.individual.ages.avgfive.no.comma

## i <- 1
n5$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.avgfive(n5$fits, n5$pointforecasts, n5$intervalforecasts,i=1)


#------------------------------------------------------------------------------------------------------------------

#---- plot forecasted values & forecast intervals:  scatterplot (total age) ----------------------------------------

# ----------------------------------------------------------------------------------------------------------------

n5$scatterplot.forecasted.values.and.forecast.intervals.total.age.avgfive <- function(results, pointforecasts, intervalforecasts){

     .e <- environment()

     intervals <- intervalforecasts

     ## intervals <- subset(intervals, select=-PI.med)

     usePackage("stringr")

     intervals <- transform(intervals, PI.ctr=str_replace(PI.ctr,",",""))
     intervals <- transform(intervals, PI.ctr=as.numeric(PI.ctr))

     intervals <- transform(intervals, PI.lwr=str_replace(PI.lwr,",",""))
     intervals <- transform(intervals, PI.lwr=as.numeric(PI.lwr))

     intervals <- transform(intervals, PI.upr=str_replace(PI.upr,",",""))
     intervals <- transform(intervals, PI.upr=as.numeric(PI.upr))
     intervals

     ## colnames(intervals) <- c("Centre", "Lower","Upper")


     tabledata <- data.frame(cy = results$data.retro[[1]]$cy,
                             a = results$a.total.retro,
                             p = results$p.total.retro,
                             e = results$e.total.retro)



       years <- tabledata$cy
       retropointforecasts <- tabledata$a  ## these are historic values
       retropointforecasts <- round(retropointforecasts,2)
       dfretro <- data.frame(years,retropointforecasts)

       dfretropos <- subset(dfretro,retropointforecasts>=0)
       dfretroneg <- subset(dfretro,retropointforecasts<0)


       pointforecast <- as.numeric(intervals["PI.ctr"])
       lower <- as.numeric(intervals["PI.lwr"])
       upper <- as.numeric(intervals["PI.upr"])
       lower <- max(0,lower)
       forecastingyear <-  max(years) + 1    #### Come back here to automate!!!

       dffor <- data.frame(forecastingyear,pointforecast,lower,upper)

       usePackage("ggplot2")
       usePackage("scales")

      gp <- ggplot(data=dfretro, aes(years,retropointforecasts), environment=.e) +
      geom_line(colour="dodgerblue3") +
      geom_rect(data=dffor, aes(x=NULL, y=NULL, xmax=forecastingyear+1/3,
                xmin=forecastingyear-1/3,
                ymax=upper,
                ymin=lower),fill="lightgrey") +
      geom_segment(data=dffor, aes(x = forecastingyear,
                     y = lower,
                     xend = forecastingyear,
                     yend = upper),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = lower,
                     xend = forecastingyear+1/3,
                     yend = lower),
                     colour="violetred2",linetype=1,size=0.7) +
      geom_segment(data=dffor, aes(x = forecastingyear-1/3,
                     y = upper,
                     xend = forecastingyear+1/3,
                     yend = upper),
                     colour="red",linetype=1,size=0.7) +
      geom_segment(data=dfretro, aes(x = years[length(years)],
                     y = retropointforecasts[length(retropointforecasts)],
                     xend = forecastingyear,
                     yend = pointforecast),
                     colour="violetred2",linetype=2,size=0.7) +
      geom_point(data=dfretro,aes(x=years,y=retropointforecasts),colour="dodgerblue3",size=2) +
      geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="violetred2",size=2) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,
                   y=pointforecast,
                   label=paste(comma(round(dffor$pointforecast)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   label=paste(comma(round(dffor$lower)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=0,hjust=0,vjust=1,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   label=paste(comma(round(dffor$upper)))),
                   # colour="red",angle=90,vjust=2,size=3) +
                   colour="violetred2",angle=0,hjust=0,vjust=-0.4,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(dfretro$retropointforecasts),
                       xend=forecastingyear,
                       yend=mean(dfretro$retropointforecasts)), colour="grey15") +
      # scale_y_continuous("Terminal Run",labels=comma) +
       scale_y_continuous(paste(n5$stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear),
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) +
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      # labs(title="Total Terminal Run") +
      labs(title=paste("Total", n5$stockabundance)) +
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

     return(gp)



}



n5$results <- n5$results.total.age.retro.predictive.performance.avgfive

n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)

n5$intervalforecasts <-  n5$PI.total.age.avgfive.no.comma


n5$scatterplot.forecasted.values.and.forecast.intervals.total.age.avgfive(
   n5$results, 
   n5$pointforecasts, 
   n5$intervalforecasts)



  
##########################################################################################
#
#  Bias coefficients of retrospective forecast errors - individual ages
#
##########################################################################################


n5$bias.coefficients.afe.individual.ages.retro.avgfive <- function(fits, results, stockabundance){

   
    par(mfrow=c(length(fits),1), mar=c(2,2,2,2), cex.main=0.9)

     n5$bias.coeff.afe.individual.ages.retro.avgfive <<- NULL 

    nms <- NULL 
    for (i in 1:length(fits)) {

        data <- results[[i]]

        names(data)[names(data)=="a.retro"] <- "Actual"
        names(data)[names(data)=="p.retro"] <- "Forecast"
        names(data)[names(data)=="e.retro"] <- "Error"

        error <- data$Error

        mre.error <- n5$mre(error)

        ## plot.mre(mre.error)
        
        ## gamma <- Arg(mre.error)
        
        ## bias <- 1 - 4*gamma/pi
        
        ## k <- length(bias)


        n5$bias.coeff.updated(mre.error, outplot=2)

        n5$bias.coeff.afe.individual.ages.retro.avgfive <<- c(n5$bias.coeff.afe.individual.ages.retro.avgfive, 
                                                               n5$bias.coeff.updated(mre.error, outplot=0))
        
        age.tmp <- paste(stockabundance, " at ", fits[[i]]$age,": ","Naive Model (Average of Previous 3 Years)", sep="")
        
        nms <- c(nms, fits[[i]]$age)
        
        title(main=paste0(age.tmp))


     }
     
     names(n5$bias.coeff.afe.individual.ages.retro.avgfive) <<- nms
     
     usePackage("gridGraphics")

     grid.echo()
     grid.grab() -> mapgrob

     return(mapgrob)


}


n5$fits <- n5$avgfive.model.fits
n5$results <- n5$results.individual.ages.retro.predictive.performance.avgfive

windows()
n5$bias.coefficients.afe.individual.ages.retro.avgfive(n5$avgfive.model.fits,
                                                    n5$results.individual.ages.retro.predictive.performance.avgfive,
                                                    n5$stockabundance)
                                                        
n5$bias.coeff.afe.individual.ages.retro.avgfive
                                                        
##########################################################################################
#
#  Bias coefficients of retrospective forecast errors - total age
#
##########################################################################################

n5$bias.coefficient.afe.total.age.retro.avgfive <- function(results, stockabundance){
   
    par(mfrow=c(1,1), mar=c(2,2,2,2))

    data <- results

    error <- data$e.total.retro

    mre.error <- n5$mre(error)
 
    ## plot.mre(mre.error)
        
    ## gamma <- Arg(mre.error)
        
    ## bias <- 1 - 4*gamma/pi
        
    ## k <- length(bias)

    n5$bias.coeff.updated(mre.error, outplot=2)
    
    n5$bias.coeff.afe.total.age.retro.avgfive <<-  n5$bias.coeff.updated(mre.error, outplot=0)  

    ## bias.coeff(mre.error, outplot=0)
        
    tmp <- paste("Total", stockabundance)
        
    title(main=paste0(tmp))
     
    usePackage("gridGraphics")

    grid.echo()
    grid.grab() -> mapgrob

    return(mapgrob)

}


n5$results <- n5$results.total.age.retro.predictive.performance.avgfive

windows()
n5$bias.coefficient.afe.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive,
                                                n5$stockabundance)
                                                        
n5$bias.coeff.afe.total.age.retro.avgfive


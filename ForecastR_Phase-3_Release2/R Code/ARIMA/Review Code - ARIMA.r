######################################################################################################
# ARIMA Time Series - Prediction Results
# Date Stamp: June 25, 2014
######################################################################################################

## Predictive Analytics: Microsoft Excel
## By Conrad Carlberg
## Page 259

ARIMA$datafile <- datafile_original

ARIMA$stockabundance <- ARIMA$datafile$Stock_Abundance[1]
ARIMA$stockabundance <- gsub("[[:space:]]", "_", ARIMA$stockabundance)

ARIMA$stockname <- ARIMA$datafile$Stock_Name[1]
ARIMA$stockspecies <- ARIMA$datafile$Stock_Species[1]
ARIMA$forecastingyear <- ARIMA$datafile$Forecasting_Year[1]


usePackage("stringr")
ARIMA$forecastingyear <- str_replace_all(ARIMA$forecastingyear, "\n","")
ARIMA$forecastingyear <- as.numeric(ARIMA$forecastingyear)


ARIMA$datafilesub <- ARIMA$datafile

## function for installing and/or loading R packages 
## usePackage <- function(p) {
##    if (!is.element(p, installed.packages()[,1]))
##        install.packages(p, dep = TRUE)
##    require(p, character.only = TRUE)
## }


#######################################################################################################

ARIMA$extract_ages <- sort(unique(ARIMA$datafilesub$Age_Class))
ARIMA$extract_names <- paste("T",ARIMA$extract_ages,sep="")
ARIMA$extract_names <- c("BY",ARIMA$extract_names)


ARIMA$tmpsub <- list()
for (i in 1:length(ARIMA$extract_ages)){
    ARIMA$tmpsub[[i]] <- subset(ARIMA$datafilesub, Age_Class==ARIMA$extract_ages[i])[,c("Brood_Year",paste0("Average","_",ARIMA$stockabundance))]
}


ARIMA$list.of.data.frames <- ARIMA$tmpsub
ARIMA$merged.data.frame = Reduce(function(...) merge(...,by="Brood_Year", all=T), ARIMA$list.of.data.frames)

ARIMA$datafile_new <- ARIMA$merged.data.frame
names(ARIMA$datafile_new) <- ARIMA$extract_names

## datafile <<- datafile_new
ARIMA$datafile <- ARIMA$datafile_new


#-----  add calendar year to age extracted from name of --------------------------
#-----  response variable for sibling regression ---------------------------------

ARIMA$datalist.arima <- function(datafile, forecastingyear) {



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


ARIMA$datalist <- ARIMA$datalist.arima(ARIMA$datafile, ARIMA$forecastingyear)  # CY refers to the T variable with highest age


#--------- prepare data table for reporting --------------------------------------------------

ARIMA$datafile.report <-  ARIMA$datafile

ARIMA$datafile.report[ARIMA$datafile.report <0] <- "NA"


#--------- plot data to be used for time series arima modeling ---------------------------


# https://rstudio-pubs-static.s3.amazonaws.com/126854_e21376aaeb324fbc802efc98ad17201c.html

#--------- plot data to be used for arima forecasting (uses ggplot) ---------------------------

ARIMA$plot.data.arima   <- function(datalist){

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
               scale_y_continuous(paste(ARIMA$stockabundance),labels=comma) + 
                theme_bw() + 
                   theme(plot.title=element_text(size=12, hjust=0.5),
                         axis.title.x = element_text(size=10,vjust=-0.5),  
                         axis.title.y = element_text(size=10,vjust=1),
                         axis.text.x = element_text(size=8),
                         axis.text.y = element_text(size=8)
                         )
    
}


ARIMA$plot.data.arima(ARIMA$datalist)



#---------  fit time series ARIMA model -----------------------------------------

ARIMA$arima.model <- function(datalist, boxcoxtransform){

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

if (is.null(ARIMA$boxcoxtransform)) {ARIMA$boxcoxtransform <- boxcoxtransform}

ARIMA$arima.model.fits  <- ARIMA$arima.model(ARIMA$datalist, ARIMA$boxcoxtransform)

ARIMA$fits <- ARIMA$arima.model.fits

#---------  Plot fitted time series ARIMA model --------------------------------
# Plot fit time series ARIMA model
#-------------------------------------------------------------------------------


ARIMA$plot.fitted.arima <- function(fits, boxcoxtransform){
  
    .e <- environment()
    
    year.stacked <- NULL
    actual.fitted.stacked <- NULL
    age.stacked <- NULL
    legend.stacked <- NULL
    
    for (j in 1:length(fits)) {   
       ###
       arimafit <- fits[[j]]
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
       
       
       arimamodel <- fits[[j]]$model
       
       age <- fits[[j]]$age
       ### 
    
      
       CY <- arimafit$model.data[,"CY"]
       
       year <- CY 
       
       actual <- as.numeric(arimamodel$x)
       fitted <- as.numeric(fitted(arimamodel))  ## returns one-step forecasts 
                                                 ## for the data used in fitting the ARIMA model
       
       # actual <- actual
       # fitted <- fitted
       
       year.stacked <- c(year.stacked,year,year)    
       actual.fitted.stacked <- c(actual.fitted.stacked, actual, fitted)
       age.stacked <- c(age.stacked, rep(paste0(age,": ", modelarima),length(actual)),
                                     rep(paste0(age,": ", modelarima),length(fitted)))
       legend.stacked <- c(legend.stacked, rep("Actual Values",length(actual)),
                                           rep("Fitted Values Obtained via ARIMA Modeling",length(fitted)))

    }

    data.stacked <- data.frame(year=year.stacked, actual.fitted=actual.fitted.stacked, age=age.stacked, legend=legend.stacked)    


    usePackage("ggplot2") 
    usePackage("scales")
    
    ggplot(data.stacked, aes(year.stacked, actual.fitted), environment=.e) + 
    facet_wrap(~age,ncol=1, scales="free_y") + 
    # geom_point(aes(shape = legend)) + 
    geom_line(aes(colour = legend, group = legend),size=0.6) + 
    ## labs(x = "Return Year", y = "Terminal Run", shape = "", colour = "") + 
    labs(x = "Return Year", y = paste(ARIMA$stockabundance), shape = "", colour = "") + 
    # scale_y_continuous("Terminal Run",labels=comma) + 
    scale_y_continuous(paste(ARIMA$stockabundance),labels=comma) + 
    theme_bw() + 
    theme(plot.title=element_text(size=12, hjust=0.5),
          axis.title.x = element_text(size=10,vjust=-0.5),  
          axis.title.y = element_text(size=10,vjust=1.5),
          axis.text.x = element_text(size=8),
          axis.text.y = element_text(size=8),
          legend.position = "top", legend.direction = "horizontal")  + 
    scale_colour_manual(values=c("dodgerblue3","lightsalmon1"))  + 
    scale_linetype_manual(values=c(1,2))
    
}

## fits <- arima.model.fits
## plot.fitted.arima(fits)

ARIMA$plot.fitted.arima(ARIMA$arima.model.fits, ARIMA$boxcoxtransform)

#-------------------------------------------------------------------------------
# Report ARIMA Model Results for A Specific Age Class
#-------------------------------------------------------------------------------

ARIMA$arima.model.results <- function(fits,j){
    
       arimamodel <- fits[[j]]$model
       age <- fits[[j]]$age
       
       sink("arimamodel.txt")
       print(arimamodel)
       sink()

       out <- c(age, readLines("arimamodel.txt"))
       
       fn <- "arimamodel.txt"
       if (file.exists(fn)) file.remove(fn)
         
       return(out)
    
}

## j <- 1
## arima.model.results(fits,j=1)


#-------------------------------------------------------------------------------
# Model Diagnostics for a Specific Arima Model
#-------------------------------------------------------------------------------


ARIMA$diagnostics.arima.model.fit  <- function(fits, boxcoxtransform, i){

    .e <- environment()

    usePackage("forecast")
    usePackage("portes")
    usePackage("gridExtra")

    arimafit <- fits[[i]]
    arimamodel <- arimafit$model
    arimaresiduals <- residuals(arimamodel)
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
   
        out.lambda <-  str_detect(string=out, pattern="lambda")
       
        modellambda <- out[out.lambda==TRUE] 
        modellambda <- str_trim(modellambda, side="right")
        
        modelarima <- paste0(modelarima, "; \n", modellambda)
        
    }

   
    Age <- arimafit$age

    ## environment=.e
    g1 <- ggplot(data=data.frame(arimaresiduals=as.numeric(arimaresiduals),CY=CY),
           aes(CY, arimaresiduals), environment=.e) +
    		geom_point() +
    		 geom_line() +
    		  geom_hline(aes(yintercept=0), colour="red", linetype="dashed") +
    		   labs(x="Return Year") +
    		    labs(y="Residuals")+
    		     ggtitle(paste("Terminal Run at ", Age, ": ", modelarima, sep="")) +
    			theme_bw() +
          		 theme(plot.title=element_text(size=12, hjust=0.5), 
                   axis.title.x=element_text(size=10,vjust=-0.5),
                	  axis.title.y=element_text(size=10,vjust=1.5),
                	   axis.text.x=element_text(size=8),
                	    axis.text.y=element_text(size=8),
                	     strip.text.x = element_text(size = 8, colour = "black", angle = 0))

    ## g2: ACF plot
    
    tmp <- Acf(arimaresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1

    lag <- 1:lagmax
    
    acf <-  Acf(arimaresiduals,lag.max=lagmax, plot=FALSE)$acf[-1]
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


        acftmp <- Acf(arimaresiduals, plot=FALSE)

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
    
    
    tmp <- Acf(arimaresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1
    lag <- 1:lagmax

    pacf <-  Pacf(arimaresiduals,lag.max=lagmax, plot=FALSE)$acf
    

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


       acftmp <- Acf(arimaresiduals, plot=FALSE)

       ci <- 0.95 # Indicates 95% confidence level
       clim0 <- qnorm((1 + ci)/2)/sqrt(acftmp$n.used)
       clim <- clim0

      g3 = g3 + geom_hline(aes(yintercept=0))
      g3 = g3 + geom_hline(aes(yintercept=clim),linetype="dashed",colour="blue")
      g3 = g3 + geom_hline(aes(yintercept=-clim),linetype="dashed",colour="blue")


    ## g4: Plot of p-values from Ljung-Box test

    ## lags <- acf(avgfiveresiduals,plot=FALSE)$lag[-1]

    tmp <- Acf(arimaresiduals,plot=FALSE)$lag
    lagmax <- max(tmp)+1
    lags <- 1:lagmax


    Ljung.Box.Test <- LjungBox(arimaresiduals[!is.na(arimaresiduals)], lags=lags)
    
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

    # g <- arrangeGrob(g1, g2, g3, g4, ncol=1)
    g <- grid.arrange(g1, g2, g3, g4, ncol=1)

    g
}

## diagnostics.arima.model.fit(fits,i)

ARIMA$diagnostics.arima.model.fit(ARIMA$fits, ARIMA$boxcoxtransform, i=1)



#--------- point forecast for each individual age and for the total age ----------------------

## http://science.nature.nps.gov/im/datamgmt/statistics/r/advanced/ReproducibleReporting.cfm

ARIMA$point.forecast.arima <- function(datalist, fits, boxcoxtransform){
    
     PSY <- datalist[[1]]$CY[length(datalist[[1]]$CY)] + 1
   
     output <- vector("list",length(fits))
          
     nms <- NULL
     for (j in 1:length(fits)) { 

         fits[[j]]$model

         
         model <- fits[[j]]$model
         
         ## Bella 
         
         arimafit <- fits[[j]]$model

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

         ## mystr <- out[out.lambda==TRUE]
         
         ## lambda.char <- regmatches(mystr,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",mystr)) 
         
         ## lambda <- as.numeric(lambda.char)
       

         fn <- "arimafit.txt"
         if (file.exists(fn)) file.remove(fn)
         

         output[[j]]$Age <- fits[[j]]$age
         
         output[[j]]$Model <- modelarima
           
         output[[j]]$RY <- PSY 

         # output[[j]]$p <- as.numeric(predict(model, h=1, level=0.80)$pred)

         output[[j]]$p <-  round(as.numeric(forecast(arimafit, h=1, level=0.80, biasadj=FALSE)$mean))

         nms <- c(nms, output[[j]]$Age)          

     }

     names(output) <- nms

     return(output)
}


ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$fits, ARIMA$boxcoxtransform)


ARIMA$results.point.forecast.arima <- NULL
for (j in 1:length(ARIMA$arima.model.fits)){

       ARIMA$tmp_list <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)[[j]]

       # list2 <- unlist(list1)

       # point.pred.asslr <- rbind(point.pred.asslr, list2)

       ARIMA$tmp_df <- do.call(cbind.data.frame, ARIMA$tmp_list)

       ARIMA$results.point.forecast.arima <- rbind(ARIMA$results.point.forecast.arima, ARIMA$tmp_df)

}

ARIMA$results.point.forecast.arima$Model <- as.character(ARIMA$results.point.forecast.arima$Model)

## results.point.forecast.arima

## str(results.point.forecast.arima)




#--------- retrospective evaluation for each individual age ----------------------------------

ARIMA$individual.ages.retro.predictive.performance.arima <- function(datalist, boxcoxtransform, index){

     ## index <- 10

     index <- index

     PSY <- datalist[[1]]$CY[length(datalist[[1]]$CY)] + 1    ## Come back here to make sure the forecasting year is specified correctly!
    
     result <- list()
   
     nms <- NULL 
      
     ARIMA$individual.ages.retro.plot.info <<- list()
     
     for (j in 1:length(datalist)){

          subdata <- subset(datalist[[j]], CY < PSY)

     	    y <- subdata[,ncol(subdata)]

          cy <- subdata[,"CY"]
          
          ## arimafit <- fits[[j]]$model

          ## sink("arimafit.txt")
          ## print(arimafit)
          ## sink()

          ## out <- readLines("arimafit.txt")

          ## usePackage("stringr")

          ## out.allowmean <- grepl("with non-zero mean", out[2])

          ## out.pattern <- str_detect(string=out, pattern="ARIMA")

          ## modelarima <- out[out.pattern==TRUE]
          ## usePackage("stringr")
          ## modelarima <- str_trim(modelarima)

          ## modelorders <- as.numeric(unlist(strsplit(modelarima, "[^[:digit:]]")))

          ## modelorders <- modelorders[!is.na(modelorders)]

          ## p.0 <- modelorders[1]                 
          ## d.0 <- modelorders[2]    
          ## q.0 <- modelorders[3] 
          
          ## out.lambda <- str_detect(string=out, pattern="lambda")
          
          ## mystr <- out[out.lambda==TRUE]
         
          ## lambda.char <- regmatches(mystr,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",mystr)) 
         
          ## lambda <- as.numeric(lambda.char)

                    
          ## fn <- "arimafit.txt"
          ## if (file.exists(fn)) file.remove(fn)   
          
          usePackage("forecast")

          a <- NULL
          p <- NULL 
          e <- NULL
        
          cy00 <- NULL
          
          p.bench <- NULL
          e.bench <- NULL 
        
          ## method <- NULL 
          
          data0 <- NULL
        
          for (i in index:(length(y)-1)){
          	   
               y0 <- y[1:i]
               cy0 <- cy[1:i]
               
               ## print(i) 
               
               ##---                       
               
               if (boxcoxtransform == TRUE) {
                  
                   ## y0[y0==0] <- 0.001  # add a small constant to zero counts
                  
                   model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(y0, method="guerrero"))
               
               } else {
               
                  model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE) 
               
               } 
               
               p0 <- round(as.numeric(forecast(model0, h=1, level=0.80, biasadj=FALSE)$mean))
               
               f0 <- round(as.numeric(fitted(model0, biasadj=FALSE)))
                                         
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
               
               model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)
               
               p0.bench <- round(as.numeric(model0.bench$mean))
               
               p.bench <- c(p.bench, p0.bench)
               e0.bench <- y[i+1] - p0.bench   # actual - predicted  
               e.bench <- c(e.bench, e0.bench)
               
          }     
          
          nms <- c(nms,names(datalist[j])) 

          result[[j]] <- data.frame(cy=cy00, a, p, e, p.bench, e.bench)

          ARIMA$individual.ages.retro.plot.info[[j]] <<- data0
                    
     } # end for j loop

     # nms 
     
     names(ARIMA$individual.ages.retro.plot.info) <<- nms

     names(result) <- nms

     # result

     res <- vector("list",length(result))

     for (j in 1:length(result)) {
    
          a <- as.numeric(result[[j]]$a)
          p <- round(as.numeric(result[[j]]$p))
          e <- as.numeric(result[[j]]$e)

          p.bench <- as.numeric(result[[j]]$p.bench)
          e.bench <- as.numeric(result[[j]]$e.bench)

          mre <- mean(e,na.rm=TRUE) 
 
          mae <- mean(abs(e), na.rm=TRUE)
     
          mpe <- mean(e/a, na.rm=TRUE)
 
          mape <- mean(abs(e)/a, na.rm=TRUE)
     
          num_mase <- mean(abs(e), na.rm=TRUE)
          # denom_mase <- mean(abs(e))
          denom_mase <- mean(abs(e.bench), na.rm=TRUE)    # e.bench contains retrospective forecast errors
                                              # from naive forecast method (previous year)
          mase <- num_mase/denom_mase 
     
          rmse <- sqrt(sum(e^2, na.rm=TRUE)/length(e))

          res[[j]]$a.retro <- a
          res[[j]]$p.retro <- p
          res[[j]]$e.retro <- e
          res[[j]]$mre.retro <- mre
          res[[j]]$mae.retro <- mae
          res[[j]]$mpe.retro <- mpe
          res[[j]]$mape.retro <- mape
          res[[j]]$mase.retro <- mase
          res[[j]]$rmse.retro <- rmse
          
          ## res[[j]]$method <- method
          
          res[[j]]$data.retro <- result[[j]]

     }

     names(res) <- nms

     return(res)     
                            
} 



ARIMA$results.individual.ages.retro.predictive.performance.arima  <- 
     ARIMA$individual.ages.retro.predictive.performance.arima(ARIMA$datalist, boxcoxtransform=ARIMA$boxcoxtransform, index=ARIMA$index.year) 


ARIMA$individual.ages.retro.plot <- function(individual.ages.retro.plot.info, stockabundance, j){

   .e <- environment()

   mydata <- individual.ages.retro.plot.info[[j]]

   tmpage <- names(individual.ages.retro.plot.info)[j]

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


ARIMA$individual.ages.retro.plot(ARIMA$individual.ages.retro.plot.info, ARIMA$stockabundance, j=1)


### report retrospective performance measures for individual ages in a nicer format

ARIMA$measures.individual.ages.retro.arima <- function(results){


       Model <- c("ARIMA Model", rep("",5))
       
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

            # tmp_df

            usePackage("scales")
            tmp_df[,Age] <- comma(round(tmp_df[,Age] ,2))        

            # tmp_df   

       }

       return(tmp_df)

}

ARIMA$MIA <- ARIMA$measures.individual.ages.retro.arima(results = ARIMA$results.individual.ages.retro.predictive.performance.arima)

## MIA


##
## Total Age
##

#--------- retrospective evaluation for the total age ----------------------------------------

ARIMA$total.age.retro.predictive.performance.arima <- function(datalist, boxcoxtransform, index){

      ## index <- 10

      index <- index

      # PSY <- 2012
      PSY <- datalist[[1]]$CY[length(datalist[[1]]$CY)] + 1


      result <- list()
	    res <- list()
	    
	    individual.ages.retro.plot.info <- list()   
   
      for (j in 1:length(datalist)){

          subdata <- subset(datalist[[j]], CY < PSY)
 
     		  y <- subdata[,ncol(subdata)]
     		  cy <- subdata[,"CY"]

          ## arimafit <- fits[[j]]$model

          ## sink("arimafit.txt")
          ## print(arimafit)
          ## sink()

          ## out <- readLines("arimafit.txt")

          ## usePackage("stringr")

          ## out.pattern <- str_detect(string=out, pattern="ARIMA")

          ## out.allowmean <- grepl("with non-zero mean", out[2])

          ## modelarima <- out[out.pattern==TRUE]
          ## usePackage("stringr")
          ## modelarima <- str_trim(modelarima)

          ## modelorders <- as.numeric(unlist(strsplit(modelarima, "[^[:digit:]]")))

          ## modelorders <- modelorders[!is.na(modelorders)]

          ## p.0 <- modelorders[1]                 
          ## d.0 <- modelorders[2]    
          ## q.0 <- modelorders[3] 
          
          ## out.lambda <- str_detect(string=out, pattern="lambda")
          
          ## mystr <- out[out.lambda==TRUE]
         
          ## lambda.char <- regmatches(mystr,gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?",mystr)) 
         
          ## lambda <- as.numeric(lambda.char)

          ## fn <- "arimafit.txt"
          ## if (file.exists(fn)) file.remove(fn)   

          usePackage("forecast")

     		  a <- NULL
     		  p <- NULL 
     		  e <- NULL
     		  cy00 <- NULL
     		  
          p.bench <- NULL 
          e.bench <- NULL 
          
          ## method <- NULL
     		  
          data0 <- NULL
            
          for (i in (index-j+1):(length(y)-1)){
          		
          		y0 <- y[1:i]
              cy0 <- cy[1:i]
          				 
     				  if (boxcoxtransform == TRUE) {
              
                  y0[y0==0] <- 0.001  # add a small constant to zero counts 
                  
                  model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE, lambda=BoxCox.lambda(y0, method="guerrero"))
               
              } else {
              
                  model0 <- auto.arima(y0, allowmean=TRUE, allowdrift=FALSE) 
              
              }
               
              p0 <- round(as.numeric(forecast(model0, h=1, level=0.80, biasadj=FALSE)$mean))
                    
              f0 <- round(as.numeric(fitted(model0, biasadj=FALSE)))
                                         
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
               
              model0.bench <- rwf(y0,h=1,drift=FALSE,level=0.80)
               
              p0.bench <- round(as.numeric(model0.bench$mean))
               
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
        
        
      # head(leftjoin)

      usePackage("dplyr")
      usePackage("magrittr")

      names(leftjoin) <- make.names(names=names(leftjoin), unique=TRUE, allow_ = TRUE)

      # head(leftjoin)

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

      # head(leftjoin.new)

      ARIMA$total.age.retro.plot.info <<- leftjoin.new
  
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
   
      a.total <- apply(sapply(resjoin, "[[", "a"),1,sum, na.rm=TRUE)
      p.total <- apply(sapply(resjoin, "[[", "p"),1,sum, na.rm=TRUE)
      e.total <- apply(sapply(resjoin, "[[", "e"),1,sum, na.rm=TRUE)  
       
      p.bench.total <- apply(sapply(resjoin, "[[", "p.bench"),1,sum, na.rm=TRUE)
      e.bench.total <- apply(sapply(resjoin, "[[", "e.bench"),1,sum, na.rm=TRUE)  

  
      mre.total <- mean(e.total, na.rm=TRUE) 

      mae.total <- mean(abs(e.total), na.rm=TRUE)
     
      mpe.total <- mean(e.total/a.total, na.rm=TRUE)
 
      mape.total <- mean(abs(e.total)/a.total, na.rm=TRUE)

      num_mase <- mean(abs(e.total), na.rm=TRUE)
      ## denom_mase <- mean(abs(e))
      denom_mase <- mean(abs(e.bench.total), na.rm=TRUE)
      mase.total <- num_mase/denom_mase 

      rmse.total <- sqrt(sum(e.total^2, na.rm=TRUE)/length(e.total))
    

      return(list(a.total.retro = a.total, 
                  p.total.retro = p.total,
                  e.total.retro = e.total,
                  mre.total.retro = mre.total,
                  mae.total.retro = mae.total, 
                  mpe.total.retro = mpe.total,
                  mape.total.retro = mape.total,
                  mase.total.retro = mase.total,
                  rmse.total.retro = rmse.total,
                  ## method = method, 
                  data.retro = resjoin))

}


ARIMA$results.total.age.retro.predictive.performance.arima <- 
      ARIMA$total.age.retro.predictive.performance.arima(ARIMA$datalist, ARIMA$boxcoxtransform, ARIMA$index.year) 

## results.total.age.retro.predictive.performance.arima

## names(results.total.age.retro.predictive.performance.arima)

### report retrospective performance measures for total age in a nicer format



ARIMA$total.age.retro.plot <- function(total.age.retro.plot.info, stockabundance){

   .e <- environment()

   mydata <- total.age.retro.plot.info

   ggplot(mydata, aes(cy0, y0), environment=.e) + 
   ## ggplot(mydata, aes(cy0, y0)) +    
    geom_line(data=mydata, aes(cy0, y0), colour=colors()[434]) +
     geom_point(data=mydata, aes(psy, a0), colour=colors()[434]) + 
      geom_line(data=mydata, aes(cy0, f0), colour="red") + 
       geom_point(data=mydata, aes(psy, p0), colour="red") + 
        ylab(paste0(stockabundance)) + 
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


ARIMA$total.age.retro.plot(ARIMA$total.age.retro.plot.info, ARIMA$stockabundance)


ARIMA$measures.total.age.retro.arima  <- function(results){

       Model <- c("ARIMA Model", rep("",5))
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

       # tmp_df   

       usePackage("data.table")
       tmp_df <- data.table(tmp_df, key="Measure")

       tmp_df$Model <- c("ARIMA Model", rep("",5)) 

       # tmp_df

       return(tmp_df)

}

ARIMA$MTA <- ARIMA$measures.total.age.retro.arima(ARIMA$results.total.age.retro.predictive.performance.arima) 

## M <- merge(MIA, MTA, by = intersect(names(MIA), names(MTA)), sort=FALSE)

ARIMA$M <- merge(ARIMA$MIA, ARIMA$MTA, by = c("Measure"), sort=FALSE)

ARIMA$M <- subset(ARIMA$M, select=-Model.y)

## M

names(ARIMA$M)[names(ARIMA$M)=="Model.x"] <- "Model"

## M

ARIMA$M.arima <- ARIMA$M 


### report actual, forecasted and error values for total age in a nicer format 

ARIMA$afe.total.age.retro.arima <- function(results){

     afe.results.total.age.retro.predictive.performance.arima <- 
     data.frame(CY=results$data.retro[[1]]$cy,
               Actual=results$a.total.retro,
               Forecast=results$p.total.retro,
               Error=results$e.total.retro)

    return(afe.results.total.age.retro.predictive.performance.arima)

}

ARIMA$results.afe.total.age.retro.arima <-  
 ARIMA$afe.total.age.retro.arima(ARIMA$results.total.age.retro.predictive.performance.arima)

## results.afe.total.age.retro.arima


## usePackage("scales")
## cbind(results.afe.total.age.retro.arima[,1],
##      comma(round(results.afe.total.age.retro.arima[,"Actual"])),
##      comma(round(results.afe.total.age.retro.arima[,"Forecast"])),
##      comma(round(results.afe.total.age.retro.arima[,"Error"]))
## )

###########################################################################################
# 
# Modified version of forecast.Arima
# 
###########################################################################################



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


## meboot2 bootstrap for a specific age

# forecast.arima.modified.meboot

ARIMA$forecast.arima.modified.meboot <- function(fit, boxcoxtransform, level=80, npaths=B){

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
                p0 <- as.numeric(forecast::forecast.Arima(model0, h=1, level=0.80, biasadj=FALSE)$mean)
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

   lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8, na.rm=TRUE))
   cat("lower = ", lower, "\n")
   upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8, na.rm=TRUE))
   cat("upper = ", upper, "\n")

   out <- NULL

   ## out$mean <-  round(as.numeric(forecast::forecast(arimafit, h=1, level=0.80)$mean))

   out$mean <-  as.numeric(forecast::forecast(arimafit, h=1, level=0.80, biasadj=FALSE)$mean)


   ## out$lower <- round(lower)
   ## out$upper <- round(upper)

   out$lower <- lower
   out$upper <- upper

   out$sim <- y.paths
   out$series <- out$series
   out$ensemble <- series.meboot2

   return(out)

}

## fit <- arima.model.fits[[1]]
## debug <- forecast.arima.modified.stlboot(fit, boxcoxtransform, level=80, npaths=B)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ARIMA$forecast.arima.modified.stlboot <- function(fit, boxcoxtransform, level=80, npaths=B){

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
               #### p0 <- as.numeric(forecast::forecast(model0, h=1, level=0.80)$mean)
           
              if (stop0==FALSE){ 
                p0 <- as.numeric(forecast::forecast.Arima(model0, h=1, level=0.80, biasadj=FALSE)$mean)
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


    lower <- as.numeric(quantile(y.paths, 0.5 - level/200, type = 8))
    upper <- as.numeric(quantile(y.paths, 0.5 + level/200, type = 8))

    out <- NULL

    ## out$mean <-  as.numeric(rwf(series,h=1, drift=FALSE, level=level)$mean)

    ## model00 <- forecast::ets(series, model=modelexpsmooth, lambda=lambda)
    out$mean <- as.numeric(forecast::forecast(arimafit , h=1, lambda=lambda, level=0.80, biasadj=FALSE)$mean)
    
    out$lower <- round(lower)
    out$upper <- round(upper)

    out$sim <- y.paths
    out$series <- out$series
    out$ensemble <- series.stlboot
   
    return(out)

}

## fit <- arima.model.fits[[1]]
## debug <- forecast.arima.modified.stlboot(fit,  boxcoxtransform, level=80, npaths=B)


#*******************************************************************************************
#
#------------ compute prediction intervals for point forecasts of individual ages -----------
#
#*******************************************************************************************

ARIMA$prediction.intervals.individual.ages.arima <- function(fits, boxcoxtransform, bootmethod, level=80, npaths=npaths){
     
     ## h <- 1  # one step ahead forecasts 
     
     ## PI.ctr <- NULL
     ## PI.lwr <- NULL 
     ## PI.upr <- NULL 
     ## PI.median <- NULL 

     out <- vector("list", length=length(fits))
     for (j in 1:length(fits)){
      
          arima.fit <- fits[[j]]$model
          
          # arima.forecast.results <- forecast(arima.fit, h=1, bootstrap=TRUE, level=level, npaths=npaths)

          ## pred <- predict(arima.fit, n.ahead = h)$pred
 

          # if (bootstrap) {
          #     
          #    sim <- matrix(NA, nrow = npaths, ncol = h)
          #    
          #    for (i in 1:npaths) sim[i, ] <- simulate.Arima(object=arima.fit, nsim = h)
          #   
          # }

          # sim stores the simulations obtained by re-sampling the fitted ARIMA time series 

          if (bootmethod=="stlboot") {
              ## expsmoothboot <- forecast.expsmooth.modified.stlboot(fit=fits[[j]], boxcoxtransform, level=level, npaths=npaths)
              arimaboot <- ARIMA$forecast.arima.modified.stlboot(fit=fits[[j]], boxcoxtransform, level=level, npaths=npaths)
              
          }
          
          if (bootmethod=="meboot") {
              ## expsmoothboot <- forecast.expsmooth.modified.meboot(fit=fits[[j]], boxcoxtransform, level=level, npaths=npaths)
              arimaboot <- ARIMA$forecast.arima.modified.meboot(fit=fits[[j]], boxcoxtransform, level=level, npaths=npaths)
          }
          

          names(arimaboot)

          arima.point.forecast <- round(as.numeric(arimaboot$mean))
         
          out[[j]]$arima.point.forecast <- arima.point.forecast
          
          # arima.lwr.forecast <- as.numeric(arima.forecast.results[5]$lower)
          # arima.upr.forecast <- as.numeric(arima.forecast.results[6]$upper)

          arima.lwr.forecast <- round(arimaboot$lower) 
          arima.upr.forecast <- round(arimaboot$upper)
          
          ## PI.ctr <- c(PI.ctr, 
          ##             arima.point.forecast) 

          ## PI.lwr <- c(PI.lwr, 
          ##             arima.lwr.forecast) 
       
          ## PI.upr <- c(PI.upr,
          ##              arima.upr.forecast)
                        
          ## PI.median <- c(PI.median, 
          ##              arima.median.forecast)    

          out[[j]]$PI.ctr <- arima.point.forecast
          out[[j]]$PI.lwr <- arima.lwr.forecast
          out[[j]]$PI.upr <- arima.upr.forecast
          ## out[[j]]$PI.median <- arima.median.forecast 
          
          out[[j]]$sim <- arimaboot$sim  
     }


     results <- out
     
     return(results)
}


ARIMA$fits <- ARIMA$arima.model.fits 
ARIMA$pred.int.individual.ages.arima <- ARIMA$prediction.intervals.individual.ages.arima(ARIMA$fits, ARIMA$boxcoxtransform, ARIMA$bootmethod, level=80, npaths=ARIMA$B)

ARIMA$pred.int.individual.ages.arima

#----------------------------------------------------------------------------

ARIMA$PI.ctr <- NULL
ARIMA$PI.lwr <- NULL
ARIMA$PI.upr <- NULL 
## PI.med <- NULL
ARIMA$PI.sim <- NULL
ARIMA$nms <- NULL

for (k in 1:length(ARIMA$pred.int.individual.ages.arima)){

     ARIMA$PI.ctr <- c(ARIMA$PI.ctr, 
                 ARIMA$pred.int.individual.ages.arima[[k]]$PI.ctr) 

     ARIMA$PI.lwr <- c(ARIMA$PI.lwr, 
                 ARIMA$pred.int.individual.ages.arima[[k]]$PI.lwr) 
       
     ARIMA$PI.upr <- c(ARIMA$PI.upr,
                 ARIMA$pred.int.individual.ages.arima[[k]]$PI.upr)
                 
     ## PI.med <- c(PI.med,
     ##            pred.int.individual.ages.arima[[k]]$PI.median)            

     ARIMA$PI.sim <- cbind(ARIMA$PI.sim, ARIMA$pred.int.individual.ages.arima[[k]]$sim)
     
     ARIMA$nms <- c(ARIMA$nms, ARIMA$pred.int.individual.ages.arima[[k]]$age)
     
}

colnames(ARIMA$PI.sim) <- ARIMA$nms


ARIMA$PI.lwr[ARIMA$PI.lwr < 0] <- 0 
ARIMA$PI.upr[ARIMA$PI.upr < 0] <- 0 
## PI.med[PI.med < 0] <- 0 

ARIMA$PI.ctr <- round(ARIMA$PI.ctr)
ARIMA$PI.lwr <- round(ARIMA$PI.lwr)
ARIMA$PI.upr <- round(ARIMA$PI.upr)



## PI.med <- round(PI.med)

ARIMA$PI.individual.ages.arima <- data.frame(PI.ctr=ARIMA$PI.ctr, ## PI.med=PI.med, 
                                       PI.lwr=ARIMA$PI.lwr, PI.upr=ARIMA$PI.upr)

ARIMA$PI.individual.ages.arima.no.comma <- data.frame(PI.ctr=ARIMA$PI.ctr, ## PI.med=PI.med, 
                                                PI.lwr=ARIMA$PI.lwr, PI.upr=ARIMA$PI.upr)


## PI.individual.ages.arima

usePackage("scales")

ARIMA$PI.individual.ages.arima <- comma(ARIMA$PI.individual.ages.arima)

## PI.individual.ages.arima

ARIMA$PI.individual.ages.arima.sim <- ARIMA$PI.sim

##########################################################################################
#
#  Plot distribution of bootstrapped point forecasts - individual ages 
#
##########################################################################################

# plot.distribution.bootstrapped.point.forecasts.individual.ages.arima  <- function(PI.individual.ages.arima.sim,
#                                                                                   PI.individual.ages.arima.no.comma)


ARIMA$plot.distribution.bootstrapped.point.forecasts.individual.ages.arima <- function(PI.individual.ages.arima.sim,
                                                                                   PI.individual.ages.arima.no.comma, 
                                                                                   fits, boxcoxtransform, 
                                                                                   extract_ages, 
                                                                                   stockabundance){

    .e = environment()
    
     modelarima.name <- NULL 
     
     for (i in 1:length(fits)) {

      ## begin ARIMA fit
      
      arimafit <- fits[[i]]$model

      sink("arimafit.txt")
      print(arimafit)
      sink()

      out <- readLines("arimafit.txt")

      usePackage("stringr")

      out.pattern <- str_detect(string=out, pattern="ARIMA")
      modelarima <- out[out.pattern==TRUE]
      usePackage("stringr")
      modelarima <- str_trim(modelarima)
      
      modelarima.name <- c(modelarima.name, paste0(modelarima))
      
      if (boxcoxtransform==TRUE) {
      
          out.lambda <- str_detect(string=out, pattern="lambda")
          modellambda <- out[out.lambda==TRUE] 
          modellambda <- str_trim(modellambda, side="right")
          
          modelarima.name <- c(modelarima.name, paste0(modelarima,"; ", modellambda))

      } 
      
      ## end ARIMA fit

    

      fn <- "arimafit.txt"
      if (file.exists(fn)) file.remove(fn)

    }
     
     
    colnames(PI.individual.ages.arima.sim)  <- paste0("Age ", extract_ages)
        
    y.star.boot.stacked <- NULL
    labels.stacked <- NULL
    for (i in 1:ncol(PI.individual.ages.arima.sim)) {
    
        y.star.boot.stacked <- c(y.star.boot.stacked,  PI.individual.ages.arima.sim[,i])
        mylabel <- paste0(stockabundance," at ",colnames(PI.individual.ages.arima.sim)[i], ": ", modelarima.name[i])
        labels.stacked <-  c(labels.stacked, rep(mylabel, length(PI.individual.ages.arima.sim[,i])))

    }



    data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)
                               

    usePackage("ggplot2")
    usePackage("scales")

        
    d <- data.stacked

    l <- levels(d$labels)



    ## myBinwidth <- NULL
    myBreaks <- NULL  
    ## n <- NULL 
    for (j in 1:length(l)){
        ## h <- hist(data.stacked$y.star.boot[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE)
        hdata <- subset(d, labels==l[j])$y.star.boot
        h <- hist(hdata, plot=FALSE, breaks = "Freedman-Diaconis")
        ## myBinwidth <-  c(myBinwidth, unique(diff(h$breaks)))
        ## h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks))/2)
        ## h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks))) 
        h.tmp <- h$breaks
        myBreaks[[j]] <- h.tmp
        ## n <- c(n, length(diff(h$breaks)))
    }
    
    
    ## ggplot(d, aes(x=x)) +
    ## mapply(function(d, b) {geom_histogram(data=d, breaks=b)}, 
    ##     split(d, d$par), breaks) +
    ## facet_wrap(~ par,  scales="free_x")
    
    
    ## g <- ggplot(d, aes(x=y.star.boot),environment=.e) +
    g <- ggplot(d, aes(x=y.star.boot), environment=.e) +
              mapply(function(d, b) {geom_histogram(data=d, breaks=b, fill="wheat",colour="black")},
                     split(d, d$labels), myBreaks)  + 
              facet_wrap(~ labels,  scales="free", ncol=1) +
               expand_limits(x=0, y=0) +
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
     for (j in 1:length(l)){

        tmp <- PI.individual.ages.arima.no.comma[j,"PI.ctr"]
        ## tmp <- round(tmp)
        clim <- c(clim, tmp)

     }

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))

     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)


     x <- NULL
     xend <- NULL
     y <- NULL
     yend <- NULL
     for (j in 1:length(l)){
         x <- c(x, max(0,PI.individual.ages.arima.no.comma[j,"PI.lwr"]))
         xend <- c(xend, PI.individual.ages.arima.no.comma[j,"PI.upr"])
         y <- c(y,0)
         yend <- c(yend,0)
     }

     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)

     return(g)
}




ARIMA$plot.distribution.bootstrapped.point.forecasts.individual.ages.arima(ARIMA$PI.individual.ages.arima.sim,
                                                                       ARIMA$PI.individual.ages.arima.no.comma,
                                                                       ARIMA$arima.model.fits,  
                                                                       ARIMA$boxcoxtransform, 
                                                                       ARIMA$extract_ages, 
                                                                       ARIMA$stockabundance)



############################################################################################
#*******************************************************************************************
#
#------------ compute prediction interval for point forecast of total age       -----------
#
#*******************************************************************************************

ARIMA$arima.sim.total.age <- NULL 
ARIMA$nms <- NULL
ARIMA$arima.PI.ctr.total.age <- 0
for (k in 1:length(ARIMA$pred.int.individual.ages.arima)){
     ARIMA$arima.sim.total.age <- cbind(ARIMA$arima.sim.total.age, ARIMA$pred.int.individual.ages.arima[[k]]$sim)
     ARIMA$nms <- c(ARIMA$nms, ARIMA$pred.int.individual.ages.arima[[k]]$age)
     ARIMA$arima.PI.ctr.total.age <- ARIMA$arima.PI.ctr.total.age + ARIMA$pred.int.individual.ages.arima[[k]]$PI.ctr
}

colnames(ARIMA$arima.sim.total.age) <- ARIMA$nms



ARIMA$PI.total.age.arima <- NULL
  
ARIMA$sim <- apply(ARIMA$arima.sim.total.age, 1, sum)

ARIMA$PI.total.age.arima$PI.ctr <- ARIMA$arima.PI.ctr.total.age
## PI.total.age.arima$PI.med <- quantile(sim, 0.500)  
ARIMA$PI.total.age.arima$PI.lwr <- quantile(ARIMA$sim, 0.10)  # need to automate this!
ARIMA$PI.total.age.arima$PI.upr <- quantile(ARIMA$sim, 0.90)  # need to automate this!

# PI.total.age.arima <- unlist(PI.total.age.arima)

ARIMA$PI.total.age.arima <- data.frame(ARIMA$PI.total.age.arima)

names(ARIMA$PI.total.age.arima) <- c("PI.ctr", ## "PI.med",
                               "PI.lwr","PI.upr")

## PI.total.age.arima 

rownames(ARIMA$PI.total.age.arima) <- NULL

## PI.total.age.arima 

ARIMA$PI.total.age.arima.no.comma <- ARIMA$PI.total.age.arima

# hist(sim)

#-----------------------------------------------------------------------------------------------


ARIMA$PI.total.age.arima$PI.lwr[ARIMA$PI.total.age.arima$PI.lwr < 0] <- 0 
ARIMA$PI.total.age.arima$PI.upr[ARIMA$PI.total.age.arima$PI.upr < 0] <- 0 
## PI.total.age.arima$PI.med[PI.total.age.arima$PI.med < 0] <- 0 

ARIMA$PI.total.age.arima$PI.ctr <- round(ARIMA$PI.total.age.arima$PI.ctr)
ARIMA$PI.total.age.arima$PI.lwr <- round(ARIMA$PI.total.age.arima$PI.lwr)
ARIMA$PI.total.age.arima$PI.upr <- round(ARIMA$PI.total.age.arima$PI.upr)
## PI.total.age.arima$PI.med <- round(PI.total.age.arima$PI.med)

usePackage("scales")


ARIMA$PI.total.age.arima$PI.ctr <- comma(ARIMA$PI.total.age.arima$PI.ctr)

ARIMA$PI.total.age.arima$PI.lwr <- comma(ARIMA$PI.total.age.arima$PI.lwr)

ARIMA$PI.total.age.arima$PI.upr <- comma(ARIMA$PI.total.age.arima$PI.upr)

## PI.total.age.arima$PI.med <- comma(PI.total.age.arima$PI.med)

## PI.total.age.arima

ARIMA$PI.total.age.arima.sim <- ARIMA$sim


##########################################################################################
#
#  Plot distribution of bootstrapped point forecasts - total age 
#
##########################################################################################

## plot.distribution.bootstrapped.point.forecasts.total.age.arima  <- function(PI.total.age.arima.sim,
##                                                                            PI.total.age.arima.no.comma)


ARIMA$plot.distribution.bootstrapped.point.forecasts.total.age.arima <- function(PI.total.age.arima.sim, 
                                                                           PI.total.age.arima.no.comma, stockabundance){

    .e = environment()


    # naive model (previous year)

    y.star.boot.stacked <- PI.total.age.arima.sim
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
                   scale_x_continuous(paste("Bootstrapped Point Forecasts"),labels=comma)

    g = g + theme_bw() +
          theme(plot.title=element_text(size=12, hjust=0.5),
                axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))


     ## point forecast annotation 
     
     tmp <- PI.total.age.arima.no.comma[["PI.ctr"]]
     clim <- round(tmp)
      

     dummy2 <- data.frame(z = clim, labels = levels(data.stacked$labels))
     g = g + geom_vline(aes(xintercept = z), data=dummy2, linetype="dashed",col="red", size=1)


     ## interval forecast annotation

     x <- max(0,PI.total.age.arima.no.comma[["PI.lwr"]])
     xend <-  round(PI.total.age.arima.no.comma[["PI.upr"]])
     y <- 0
     yend <- 0

     dummy3 <- data.frame(x=x,xend=xend,y=y,yend=yend, labels = levels(data.stacked$labels))
     g = g + geom_segment(aes(x=x,y=y,xend=xend,yend=yend), data=dummy3, linetype="solid",col="blue", size=1)



     return(g)
}


ARIMA$plot.distribution.bootstrapped.point.forecasts.total.age.arima(ARIMA$PI.total.age.arima.sim, 
                                                               ARIMA$PI.total.age.arima.no.comma, 
                                                               ARIMA$stockabundance)


#-----------------------------------------------------------------------------------------
# plot forecast vs. actual (individual ages, arima)
#-----------------------------------------------------------------------------------------


ARIMA$scatter.plot.results.afe.individual.ages.retro.arima <- function(results, stockname, stockabundance){
 
    .e = environment()
 
    forecasted.stacked <- NULL
    actual.stacked <- NULL
    age.stacked <- NULL
    labels.stacked <- NULL

    ## R.squared <- NULL
    
    for (i in 1:length(results)){

       ## arimafit <- fits[[i]]$model

       usePackage("stringr")

       data <- results[[i]]
       names(data)[names(data)=="a.retro"] <- "Actual"       
       names(data)[names(data)=="p.retro"] <- "Forecast"  
       names(data)[names(data)=="e.retro"] <- "Error"
      
       ## r.sq <- summary(lm(Forecast ~ Actual, data=data))$r.squared
       ## r.sq <-  sprintf("%.2f", r.sq*100)

       mytitle <- names(results)[i]
       
       usePackage("stringr")
       mytitle <- str_replace_all(mytitle, pattern="age", replacement="Age ")
       mytitle <- substr(mytitle, start=1, stop=5)
       
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
                   scale_y_continuous(paste("Retrospectively", "Forecasted", stockabundance, "Values", "for the", stockname, "Stock"),labels=comma) + 
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma) +
                    scale_x_continuous(paste("Actual", stockabundance, "Values", "for the", stockname, "Stock"),labels=comma) +
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


ARIMA$results <- ARIMA$results.individual.ages.retro.predictive.performance.arima
ARIMA$scatter.plot.results.afe.individual.ages.retro.arima(ARIMA$results, ARIMA$stockname, ARIMA$stockabundance)


#-----------------------------------------------------------------------------------------
# Time series plot of retrospectively forecasted and actual values (individual ages, arima)
#-----------------------------------------------------------------------------------------


ARIMA$timeseries.plot.results.afe.individual.ages.retro.arima <- function(results, stockabundance){
 
    .e = environment()
 
    forecasted.stacked <- NULL
    actual.stacked <- NULL
    age.stacked <- NULL
    labels.stacked <- NULL

    ## R.squared <- NULL
    
    for (i in 1:length(results)){

       ## arimafit <- fits[[i]]$model

       usePackage("stringr")

       data <- results[[i]]
       names(data)[names(data)=="a.retro"] <- "Actual"       
       names(data)[names(data)=="p.retro"] <- "Forecast"  
       names(data)[names(data)=="e.retro"] <- "Error"
      
       ## r.sq <- summary(lm(Forecast ~ Actual, data=data))$r.squared
       ## r.sq <-  sprintf("%.2f", r.sq*100)

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
              facet_wrap(~age, scales="free_y",ncol=2) + 
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


ARIMA$results <- ARIMA$results.individual.ages.retro.predictive.performance.arima
ARIMA$timeseries.plot.results.afe.individual.ages.retro.arima(ARIMA$results, ARIMA$stockabundance)



#*******************************************************************************
# scatter plot of retrospectively forecasted vs. actual abundance (total age, arima)
#
#*******************************************************************************

ARIMA$scatter.plot.results.afe.total.age.retro.arima <- function(results, stockabundance){

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
                               ## age=paste("Total ", stockabundance, ":  ", "R-squared = ",r.sq,"%",sep="")
                               age=paste("Total ", stockabundance, sep="")
                               )
    usePackage("scales")
    usePackage("ggplot2")
 
    g <- ggplot(data.stacked, aes(actual,forecasted), environment=.e) + 
       geom_abline(intercept=0,slope=1,colour="red",size=0.8) +
       geom_text(aes(label=labels),col="blue",size=3) +
              facet_wrap(~age, scales="free",ncol=2) + 
                expand_limits(x=0, y=0) +
                 coord_fixed(ratio=1)  +       
                  # scale_y_continuous("Forecasted Terminal Run Values",labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) + 
                  scale_y_continuous(paste("Retrospectively Forecasted", stockabundance, "Values"),labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +  
                   # scale_x_continuous("Actual Terminal Run Values",labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +
                  scale_x_continuous(paste("Actual", stockabundance, "Values"),labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) + 
                  ## ggtitle(paste(stockname, "Stock")) + 
                   coord_fixed(ratio=1)   +                              
                     scale_color_manual(name=paste0(stockabundance), values=c("Actual"="blue2", "Forecasted"="red2")) + 
                   theme_bw() + 
                   theme(plot.title=element_text(size=12, hjust=0.5),
                         axis.title.x=element_text(size=10,vjust=-0.5),
                         axis.title.y=element_text(size=10,vjust=1.5),
                         axis.text.y=element_text(size=8),
                         axis.text.x = element_text(size=8))
                  
      return(g)
  

}


ARIMA$results <- ARIMA$results.total.age.retro.predictive.performance.arima

ARIMA$scatter.plot.results.afe.total.age.retro.arima(ARIMA$results, ARIMA$stockabundance)


#*******************************************************************************
# Time series plot of retrospectively forecasted vs. actual abundance (total age, arima)
#
#*******************************************************************************

ARIMA$timeseries.plot.results.afe.total.age.retro.arima <- function(results, stockabundance){

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
                  scale_y_continuous(paste("Retrospectively Forecasted", stockabundance, "Values"),labels=comma,limits=c(0,max(data.stacked$actual,data.stacked$forecasted))) +  
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


ARIMA$results <- ARIMA$results.total.age.retro.predictive.performance.arima

ARIMA$timeseries.plot.results.afe.total.age.retro.arima(ARIMA$results, ARIMA$stockabundance)


#*******************************************************************************
# Density of Retrospective Forecast Errors - Individual Ages
#*******************************************************************************

# fits <- arima.model.fits
# results <- results.individual.ages.retro.predictive.performance.arima 

# hist.results.afe.individual.ages.retro.arima(arima.model.fits, 
#                                              results.individual.ages.retro.predictive.performance.arima)


ARIMA$dens.results.afe.individual.ages.retro.arima <- function(fits, boxcoxtransform, results){

    
    data.stacked  <- NULL 
    
    for (i in 1:length(fits)) {

      data <- results[[i]]
      names(data)[names(data)=="a.retro"] <- "Actual"       
      names(data)[names(data)=="p.retro"] <- "Forecast"  
      names(data)[names(data)=="e.retro"] <- "Error"
         
      # mytitle <- names(data)[i]

      error.stacked <- data$Error         

      ## begin ARIMA fit

      arimafit <- fits[[i]]$model

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
          modellambda <- out[out.lambda==TRUE] 
          modellambda <- str_trim(modellambda, side="right")
          
          modelarima <- paste0(modelarima,"; ", modellambda)

      } 

      ## end ARIMA fit 
                  
      age.tmp <- paste(ARIMA$stockabundance, " at ", fits[[i]]$age,": ",modelarima, sep="")

      age.stacked <- rep(age.tmp, length(data$Error))
            
      fn <- "arimafit.txt"
      if (file.exists(fn)) file.remove(fn)

   
      data.stacked <- rbind.data.frame(data.stacked, data.frame(errors = error.stacked,  labels = factor(age.stacked)))

      }


    usePackage("plyr")

    data.stacked <- ddply(data.stacked, c('labels'))
    data.stacked$labels <- as.factor(data.stacked$labels)

    usePackage("ggplot2")
    usePackage("scales")


    d <- data.stacked

    l <- levels(d$labels)


    ## breaks <- NULL
    ## for (j in 1:length(fits)){
    ##    h <- hist(data.stacked$errors[data.stacked$labels==levels(data.stacked$labels)[j]],plot=FALSE, breaks = "Freedman-Diaconis")
    ##    #### breaks[[j]] <- h$breaks
    ##    h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks)))
    ##    ## h.tmp <- seq(from=min(h$breaks), to=max(h$breaks), by = unique(diff(h$breaks))/2)
    ##    breaks[[j]] <- h.tmp
    ## }

    .e <- environment()

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





ARIMA$dens.results.afe.individual.ages.retro.arima(ARIMA$arima.model.fits, 
                                                   ARIMA$boxcoxtransform,  
                                                   ARIMA$results.individual.ages.retro.predictive.performance.arima)



## results <- results.individual.ages.retro.predictive.performance.arima


## par(mfrow=c(4,1), mar=c(4,4,3,2))
## hist.results.afe.individual.ages.retro.arima(
##    arima.model.fits, 
##    results.individual.ages.retro.predictive.performance.arima
## )


#*******************************************************************************
#
# Density of retrospective forecast errors (total age, arima)
#
#*******************************************************************************


ARIMA$dens.results.afe.total.age.retro.arima <- function(results){

  .e <- environment()

	## usePackage("KernSmooth")
	usePackage("ggplot2")
	usePackage("scales")
	

	# results$age <- "Total Terminal Run"
	results$age <- paste("Total", ARIMA$stockabundance) 

	## b <- dpih(results$Error)

  ## g <- ggplot(results, aes(Error), environment=.e)
	g <- ggplot(results, aes(Error), environment=.e) + 
  	      ## geom_histogram(fill="wheat",colour="black", binwidth=b) + 
          geom_density(fill="lightblue",colour="black") +
  	       geom_vline(xintercept=0,colour="red",linetype=2,size=1.2) + 
              facet_wrap(~age, ncol=1) + 
    	         scale_y_continuous("Frequency") + 
    	          scale_x_continuous("Retrospective Forecast Error", labels=comma, 
                   limits=c(-max(abs(results$Error)),max(abs(results$Error))) ) + 
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

## results <- results.afe.total.age.retro.arima

## hist.results.afe.total.age.retro.arima(results)

## dens.results.afe.total.age.retro.arima(results.afe.total.age.retro.arima)

#*******************************************************************************
#  barplot forecasted values (specific ages)
#
#*******************************************************************************

ARIMA$barplot.forecasted.values.individual.ages.arima <- function(fits, boxcoxtransform, pointforecasts, i){
    
        .e <- environment()
        
        arimafit <- fits[[i]]$model

        sink("arimafit.txt")
        print(arimafit)
        sink()

        out <- readLines("arimafit.txt")

        usePackage("stringr")

        out.pattern <- str_detect(string=out, pattern="ARIMA")

        modelarima <- out[out.pattern==TRUE]
        usePackage("stringr")
        modelarima <- str_trim(modelarima) 
    
        if (boxcoxtransform == TRUE) {
    
            out.lambda <- str_detect(string=out, pattern="lambda")

            modellambda <- out[out.lambda==TRUE]
            modellambda <- str_trim(modellambda, side="right")  
    
            modelarima <- paste0(modelarima, "; \n", modellambda)
    
        }
    
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
        scale_y_continuous(paste(ARIMA$stockabundance),label=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        labs(title=paste0(age,": ", modelarima)) +  
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
      scale_y_continuous(paste(ARIMA$stockabundance),labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +  
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) + 
         ## geom_text(data=dfretro,aes(x=years[1],
         ##              y=max(dfretro$retropointforecasts),
         ##              label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts),2)))),
         ##          colour="burlywood4",size=3,hjust=-1.2,vjust=1) +        
     annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), size=2.5,colour="grey15",hjust=0,vjust=0) +
     labs(title=paste0(age,": ", modelarima)) +
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
     
     ## plotlist <- gList(ggplotGrob(gp1),ggplotGrob(gp2))
     ## do.call("grid.arrange", c(plotlist,ncol=1))
      
     # p <- arrangeGrob(gp1,gp2) 
     
     p <- grid.arrange(gp1,gp2)
     
     p   
      
        
     ## }
     
     ## require(gridExtra)
     ## do.call("grid.arrange", c(list_graphs, ncol=1))

}


ARIMA$fits <- ARIMA$arima.model.fits

ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)

ARIMA$barplot.forecasted.values.individual.ages.arima(ARIMA$fits, ARIMA$boxcoxtransform, ARIMA$pointforecasts,i=1)


#*******************************************************************************
#  barplot forecasted values (total age)
#
#*******************************************************************************


ARIMA$barplot.forecasted.values.total.age.arima <- function(results, pointforecasts){
  
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
        scale_y_continuous(paste(ARIMA$stockabundance),label=comma,breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +
        ## labs(title="Total Terminal Run") +  
        labs(title=paste("Total", ARIMA$stockabundance)) +  
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
      scale_y_continuous(paste(ARIMA$stockabundance),labels=comma, breaks=pretty(c(0,1.4*max(max(dfretro$retropointforecasts,dffor$pointforecast))))) +   
         scale_x_continuous("Return Year", breaks=c(dfretro$years,dffor$forecastingyear)) +         
      annotate("text",x=dfretro$years[1],y=1.3*max(max(dfretro$retropointforecasts,dffor$pointforecast)),
               label=paste("Historical Average =",comma(round(mean(dfretro$retropointforecasts)))), size=2.5,colour="grey15",hjust=0,vjust=0) + 
     ## labs(title="Total Terminal Run") +
     labs(title=paste("Total", ARIMA$stockabundance)) +
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
       
     # p <- arrangeGrob(gp1,gp2) 
     
     p <- grid.arrange(gp1,gp2) 
     
     p    
       
}



ARIMA$results <- ARIMA$results.total.age.retro.predictive.performance.arima

ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)

ARIMA$barplot.forecasted.values.total.age.arima(ARIMA$results, ARIMA$pointforecasts)


#*******************************************************************************
#
# plot forecasted values:  scatterplot (individual ages) 
#
#*******************************************************************************

ARIMA$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.arima <- function(fits, boxcoxtransform, pointforecasts, intervalforecasts,i){
  
     .e <- environment()
    
     ## arima fit information 
        
     arimafit <- fits[[i]]$model

     sink("arimafit.txt")
     print(arimafit)
     sink()

     out <- readLines("arimafit.txt")

     usePackage("stringr")

     out.pattern <- str_detect(string=out, pattern="ARIMA")

     modelarima <- out[out.pattern==TRUE]
     usePackage("stringr")
     modelarima <- str_trim(modelarima) 
    
     if (boxcoxtransform == TRUE){
     
        out.lambda <- str_detect(string=out, pattern="lambda")

        modellambda <- out[out.lambda==TRUE]
        modellambda <- str_trim(modellambda, side="right")
     
        modelarima <- paste0(modelarima, "; \n", modellambda) 
    
     }
     
     ## end arima fit information 
     
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
        
        ## pointforecast <- round(p,2)
        
         pointforecast <- p

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
                   label=paste(comma(pointforecast))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # lower end annotation
                   y=lower,
                   ## label=paste(comma(sprintf("%.2f",lower)))),
                   label=paste(comma(lower))),
                   colour="violetred2",angle=0,hjust=0,size=2.5)+
      geom_text(data=dffor, aes(x=forecastingyear+1/2,     # upper end annotation
                   y=upper,
                   ## label=paste(comma(sprintf("%.2f",upper)))),
                   label=paste(comma(upper))),
                   colour="violetred2",angle=0,hjust=0,size=2.5) +
      geom_segment(data=dfretro, aes(x=years[1],
                       y=mean(retropointforecasts),
                       xend=dffor$forecastingyear,
                       yend=mean(retropointforecasts)), colour="grey15") + 
      # scale_y_continuous("Terminal Run",labels=comma) +
      scale_y_continuous(paste(ARIMA$stockabundance),labels=comma) +
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear), 
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) + 
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      labs(title=paste0(age,": ",modelarima)) +
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


ARIMA$fits <- ARIMA$arima.model.fits
ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)
ARIMA$intervalforecasts <-   ARIMA$PI.individual.ages.arima.no.comma

## i <- 1
ARIMA$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.arima(ARIMA$fits, ARIMA$boxcoxtransform, ARIMA$pointforecasts, ARIMA$intervalforecasts,i=1)




#---- plot forecasted values:  scatterplot (total age) ------------------------------------------------------


ARIMA$scatterplot.forecasted.values.and.forecast.intervals.total.age.arima <- function(results, pointforecasts, intervalforecasts){
  
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
       scale_y_continuous(paste(ARIMA$stockabundance),labels=comma) + 
        scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear), 
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+4), expand=c(0,0)) + 
         geom_text(data=dfretro, aes(x=years[1]+1,
                       y=1.1*max(dfretro$retropointforecasts,dffor$upper),
                       label=paste("Historical Average = ",comma(round(mean(dfretro$retropointforecasts))))),
                   colour="grey15",size=2.5,hjust=0.1,vjust=0.1) +
      # labs(title="Total Terminal Run") +
      labs(title=paste("Total", ARIMA$stockabundance)) + 
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



ARIMA$results <- ARIMA$results.total.age.retro.predictive.performance.arima

ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)

ARIMA$intervalforecasts <-  ARIMA$PI.total.age.arima.no.comma


ARIMA$scatterplot.forecasted.values.and.forecast.intervals.total.age.arima(ARIMA$results, ARIMA$pointforecasts, ARIMA$intervalforecasts)  


#*******************************************************************************
#
# Gary's Plot for Individual Ages
#
#*******************************************************************************


ARIMA$gary.plot.individual.ages <- function(results.retro, results.pred, j){

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
      
        lim_neg <- 1.4* min(dfretroneg$retropointforecasts, lower) 
        lim_pos <- 1.4* max(dfretropos$retropointforecasts, upper) 
      
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
        geom_vline(data=dffor, xintercept=forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) + 
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
        labs(title=paste(ARIMA$stockabundance,"at",age)) +
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
        geom_vline(data=dffor, xintercept=forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) + 
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
        labs(title=paste(ARIMA$stockabundance,"at",age)) +
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
        geom_vline(data=dffor, xintercept=forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) + 
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
        labs(title=paste(ARIMA$stockabundance, "at",age)) +
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
        geom_vline(data=dffor, xintercept=forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) + 
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
        labs(title=paste(ARIMA$stockabundance, "at",age)) +
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
       
          
} # end gary.plot for individual ages


ARIMA$results.retro <- ARIMA$results.individual.ages.retro.predictive.performance.arima 
names(ARIMA$results.retro)

ARIMA$results.pred <- ARIMA$pred.int.individual.ages.arima
 
## par(mfrow=c(1,1))
## j <- 1
ARIMA$gary.plot.individual.ages(ARIMA$results.retro, ARIMA$results.pred, j=1)


#*******************************************************************************
#
# Gary's Plot for "Total Age"
#
#*******************************************************************************

ARIMA$gary.plot.total.age <- function(results.retro, results.pred){
          
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
          
          lim_neg <- 1.3* min(dfretroneg$retropointforecasts, lower) 
          lim_pos <- 1.3* max(dfretropos$retropointforecasts, upper) 
          
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
          geom_vline(data=dffor, xintercept=forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) + 
          geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=lower,xend=forecastingyear+1/3,yend=lower),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=upper,xend=forecastingyear+1/3,yend=upper),colour="red",size=1) +
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
          labs(title=paste("Total",ARIMA$stockabundance)) +
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
          
          lim_neg <- 1.3* min(dfretroneg$retropointforecasts, lower) 
          lim_pos <- 1.3* max(dfretropos$retropointforecasts, upper) 
          
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
          geom_vline(data=dffor, xintercept=forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) + 
          geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=lower,xend=forecastingyear+1/3,yend=lower),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=upper,xend=forecastingyear+1/3,yend=upper),colour="red",size=1) +
          geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
          geom_hline(yintercept=0,colour="grey90") +  
          scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear), 
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
          scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )   
                        expand=c(0,0) 
                        ) + 
          labs(title=paste("Total", ARIMA$stockabundance)) +
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
          
          lim_neg <- 1.3* min(dfretroneg$retropointforecasts, lower) 
          lim_pos <- 1.3* max(dfretropos$retropointforecasts, upper) 
          
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
          geom_vline(data=dffor, xintercept=forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) + 
          geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=lower,xend=forecastingyear+1/3,yend=lower),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=upper,xend=forecastingyear+1/3,yend=upper),colour="red",size=1) +
          geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
          geom_hline(yintercept=0,colour="grey90") +  
          scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear), 
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
          scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )   
                        expand=c(0,0) 
                        ) + 
          labs(title=paste("Total", ARIMA$stockabundance)) +
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
            
          lim_neg <- 1.3* min(dfretroneg$retropointforecasts, lower) 
          lim_pos <- 1.3* max(dfretropos$retropointforecasts, upper) 
          
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
          geom_vline(data=dffor, xintercept=forecastingyear-1/2,linetype=2,colour="turquoise",size=0.8) + 
          geom_segment(data=dffor, aes(x=forecastingyear,y=lower,xend=forecastingyear,yend=upper),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=lower,xend=forecastingyear+1/3,yend=lower),colour="red",size=1) +
          geom_segment(data=dffor, aes(x=forecastingyear-1/3,y=upper,xend=forecastingyear+1/3,yend=upper),colour="red",size=1) +
          geom_point(data=dffor, aes(x=forecastingyear,y=pointforecast),colour="red",size=2.5) +
          geom_hline(yintercept=0,colour="grey90") +  
          scale_x_continuous("Return Year",breaks=c(dfretro$years,dffor$forecastingyear), 
                         limits=c(min(dfretro$years)-1, max(dffor$forecastingyear)+3), expand=c(0,0) ) +
          scale_y_continuous("Retrospective Forecast Errors & Forecast Interval",
                        breaks=pretty(c(lim_neg, lim_pos)),
                        limits=c(lim_neg, lim_pos),
                        labels=comma,
                        # limits=c( 1.3*min(c(min(dfretro$retropointforecasts),min(dffor$pointforecast))),
                        #           1.3*max(c(max(dfretro$retropointforecasts),max(dffor$pointforecast))) )   
                        expand=c(0,0) 
                        ) + 
          # labs(title=paste("Total Terminal Run")) +
          labs(title=paste("Total", ARIMA$stockabundance)) +
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




ARIMA$results.retro <- ARIMA$results.total.age.retro.predictive.performance.arima 
names(ARIMA$results.retro)
ARIMA$results.pred <- ARIMA$PI.total.age.arima.no.comma

## str(results.retro)
## str(results.pred)

ARIMA$gary.plot.total.age(ARIMA$results.retro, ARIMA$results.pred)


#=============================================================================================

##########################################################################################
#
#  Bias coefficients of retrospective forecast errors - individual ages
#
##########################################################################################

## fits <- arima.model.fits

ARIMA$bias.coefficients.afe.individual.ages.retro.arima <- function(fits, results, boxcoxtransform, stockabundance){

   
    par(mfrow=c(length(fits),1), mar=c(2,2,2,2), cex.main=0.9)

    ARIMA$bias.coeff.afe.individual.ages.retro.arima <<- NULL 

    for (i in 1:length(fits)) {

        data <- results[[i]]

        names(data)[names(data)=="a.retro"] <- "Actual"
        names(data)[names(data)=="p.retro"] <- "Forecast"
        names(data)[names(data)=="e.retro"] <- "Error"

        error <- data$Error

        mre.error <- ARIMA$mre(error)

        ## plot.mre(mre.error)
        
        ## gamma <- Arg(mre.error)
        
        ## bias <- 1 - 4*gamma/pi
        
        ## k <- length(bias)


        ARIMA$bias.coeff.updated(mre.error, outplot=2)

        ARIMA$bias.coeff.afe.individual.ages.retro.arima <<- c(ARIMA$bias.coeff.afe.individual.ages.retro.arima, ARIMA$bias.coeff.updated(mre.error, outplot=0))

        ## bias.coeff(mre.error, outplot=0)

                ## begin exp smooth fit
      
        arimafit <- fits[[i]]

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
          modellambda <- out[out.lambda==TRUE] 
         
          ## modellambda <- modellambda[2] 
          
          modellambda <- str_trim(modellambda, side="right")
          modellambda <- str_trim(modellambda, side="left")
          modellambda <- str_trim(modellambda, side="right")
          modellambda <- str_replace(modellambda, "lambda=", "lambda =")
          
          modelarima <- paste0(modelarima,"; ", modellambda)

      } 

        ## end exp smooth fit
        
        age.tmp <- paste(stockabundance, " at ", fits[[i]]$age,": ",modelarima, sep="")
        
        title(main=paste0(age.tmp))


     }
     
     usePackage("gridGraphics")

     grid.echo()
     grid.grab() -> mapgrob

     return(mapgrob)


}


ARIMA$fits <- ARIMA$arima.model.fits
ARIMA$results <- ARIMA$results.individual.ages.retro.predictive.performance.arima

windows()
ARIMA$bias.coefficients.afe.individual.ages.retro.arima(ARIMA$arima.model.fits,
                                                       ARIMA$results.individual.ages.retro.predictive.performance.arima,
                                                        ARIMA$boxcoxtransform, 
                                                        ARIMA$stockabundance)
                                                        
ARIMA$bias.coeff.afe.individual.ages.retro.arima
                                                        
##########################################################################################
#
#  Bias coefficients of retrospective forecast errors - total age
#
##########################################################################################

ARIMA$bias.coefficient.afe.total.age.retro.arima <- function(results, stockabundance){
   
    par(mfrow=c(1,1), mar=c(2,2,2,2))

    data <- results

    ## error <- data$Error

    error <- data$e.total.retro

    mre.error <- ARIMA$mre(error)
 
    ## plot.mre(mre.error)
        
    ## gamma <- Arg(mre.error)
        
    ## bias <- 1 - 4*gamma/pi
        
    ## k <- length(bias)

    ARIMA$bias.coeff.updated(mre.error, outplot=2)
    
    ARIMA$bias.coeff.afe.total.age.retro.arima <<- ARIMA$bias.coeff.updated(mre.error, outplot=0)  

    ## bias.coeff(mre.error, outplot=0)
        
    tmp <- paste("Total", stockabundance)
        
    title(main=paste0(tmp))
     
    usePackage("gridGraphics")

    grid.echo()
    grid.grab() -> mapgrob

    return(mapgrob)

}


windows()


ARIMA$results <- ARIMA$results.total.age.retro.predictive.performance.arima

ARIMA$bias.coefficient.afe.total.age.retro.arima(ARIMA$results,ARIMA$stockabundance)
                                                        
ARIMA$bias.coeff.afe.total.age.retro.arima

cat("========================================================","\n\n")
cat("ARIMA$bias.coeff.afe.total.age.retro.arima = ", ARIMA$bias.coeff.afe.total.age.retro.arima, "\n\n")
cat("========================================================","\n\n")

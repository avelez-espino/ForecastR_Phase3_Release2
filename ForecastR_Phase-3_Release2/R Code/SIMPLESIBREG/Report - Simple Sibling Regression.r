#========================================================================================================
# CoverPage
#========================================================================================================

usePackage("ReporteRs")

SIMPLESIBREG$fits <-  SIMPLESIBREG$sibling_regression_model_fits(SIMPLESIBREG$data_and_model_formulas)


SIMPLESIBREG$pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 40) )
SIMPLESIBREG$my.pars = set_of_paragraphs(SIMPLESIBREG$pot1)
doc = addParagraph( doc, value = SIMPLESIBREG$my.pars, stylename="Normal" )

SIMPLESIBREG$pot1 <- NULL 
SIMPLESIBREG$my.pars <- NULL 

SIMPLESIBREG$pot1 = pot(" ", textProperties(font.weight="bold", font.size = 20) )
SIMPLESIBREG$my.pars = set_of_paragraphs(SIMPLESIBREG$pot1)
doc = addParagraph( doc, value = SIMPLESIBREG$my.pars, stylename="Normal" )

SIMPLESIBREG$pot1 <- NULL 
SIMPLESIBREG$my.pars <- NULL 

SIMPLESIBREG$pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(SIMPLESIBREG$stockname), textProperties(font.size = 20) )
SIMPLESIBREG$my.pars = set_of_paragraphs(SIMPLESIBREG$pot2)
doc = addParagraph( doc, value = SIMPLESIBREG$my.pars, stylename="Normal" )

SIMPLESIBREG$pot2 <- NULL 
SIMPLESIBREG$my.pars <- NULL 

SIMPLESIBREG$pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(SIMPLESIBREG$stockspecies), textProperties(font.size = 20) )
SIMPLESIBREG$my.pars = set_of_paragraphs(SIMPLESIBREG$pot3)
doc = addParagraph( doc, value = SIMPLESIBREG$my.pars, stylename="Normal" )

SIMPLESIBREG$pot3 <- NULL 
SIMPLESIBREG$my.pars <- NULL 

SIMPLESIBREG$pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(SIMPLESIBREG$stockabundance), textProperties(font.size = 20) )
SIMPLESIBREG$my.pars = set_of_paragraphs(SIMPLESIBREG$pot4)
doc = addParagraph( doc, value = SIMPLESIBREG$my.pars, stylename="Normal" )

SIMPLESIBREG$pot4 <- NULL 
SIMPLESIBREG$my.pars <- NULL 

SIMPLESIBREG$pot5 =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(SIMPLESIBREG$forecastingyear), textProperties(font.size = 20) )
SIMPLESIBREG$my.pars = set_of_paragraphs(SIMPLESIBREG$pot5)
doc = addParagraph( doc, value = SIMPLESIBREG$my.pars, stylename="Normal" )

SIMPLESIBREG$pot5 <- NULL 
SIMPLESIBREG$my.pars <- NULL 

SIMPLESIBREG$pot6 =  pot("Forecasting Model: ", textProperties(font.weight="bold", font.size = 20)) + 
        pot(paste("Simple Sibling Regression Model Without Environmental Covariates"), textProperties(font.size = 20) ) 
SIMPLESIBREG$my.pars = set_of_paragraphs(SIMPLESIBREG$pot6)
doc = addParagraph( doc, value = SIMPLESIBREG$my.pars, stylename="Normal" )
               
SIMPLESIBREG$pot6 <- NULL 
SIMPLESIBREG$my.pars <- NULL 

SIMPLESIBREG$pot13 =  pot("Date: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(Sys.Date()), textProperties(font.size = 20) )
SIMPLESIBREG$my.pars = set_of_paragraphs(SIMPLESIBREG$pot13)
doc = addParagraph( doc, value = SIMPLESIBREG$my.pars, stylename="Normal" )
     
SIMPLESIBREG$pot13 <- NULL 
SIMPLESIBREG$my.pars <- NULL 

doc = addPageBreak(doc)


#========================================================================================================
# Compute Summary of Forecasting Results for Complex Sibling Regression
#========================================================================================================

## Measures of Retrospective Forecast Performance


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


SIMPLESIBREG$best.rmse.youngest.age <-  SIMPLESIBREG$best.rmse.results.youngest.age(SIMPLESIBREG$rmse.results.youngest.age.avgfive.plus.simple.sibling.regression,
                                                          SIMPLESIBREG$rmse.results.youngest.age.arima.plus.simple.sibling.regression,
                                                          SIMPLESIBREG$rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression)
 

SIMPLESIBREG$best.rmse.youngest.age

SIMPLESIBREG$summary.forecast.results.simple.sibling.regression <-
   SIMPLESIBREG$retro.measures.all.ages.simple.sibling(SIMPLESIBREG$best.rmse.youngest.age)

data.frame(lapply(SIMPLESIBREG$summary.forecast.results.simple.sibling.regression, as.character), stringsAsFactors=FALSE)

SIMPLESIBREG$summary.forecast.results.simple.sibling.regression[,-1]  <-
   comma(SIMPLESIBREG$summary.forecast.results.simple.sibling.regression[,-1])



SIMPLESIBREG$summary.forecast.results.simple.sibling.regression <-
    data.frame(lapply(SIMPLESIBREG$summary.forecast.results.simple.sibling.regression, as.character), stringsAsFactors=FALSE)

## r.squared.retro.simple.sibling.regression(best.rmse.youngest.age)

## summary.forecast.results.simple.sibling.regression <- rbind.data.frame(
##       summary.forecast.results.simple.sibling.regression,
##       c("R-squared",
##         as.character(r.squared.retro.simple.sibling.regression(best.rmse.youngest.age)$r.squared))
##       )


names(SIMPLESIBREG$summary.forecast.results.simple.sibling.regression)[1] <- "Item"

## Point and Interval Forecasts

# this function deals only with forecasts produced by best sibling regression models for older ages
SIMPLESIBREG$table_interval_forecasts <- function(results_best_fitting_model_for_each_age_class,
                                     PI.individual.ages.simple.sibling.regression.no.comma,
                                                           forecastingyear, stockabundance){

    point_forecast_results <-
      SIMPLESIBREG$point.forecast.best.fitting.model.for.each.age.class(results_best_fitting_model_for_each_age_class,
                                                           forecastingyear, SIMPLESIBREG$datafile_variables)

    interval_forecast_results <- PI.individual.ages.simple.sibling.regression.no.comma

    mytable <- matrix(NA, nrow=length(point_forecast_results), ncol=5)  # add a 5th column for interval forecast
    mytable <- as.data.frame(mytable)
    names(mytable)[-1] <- c("Best Model","Forecasting Year","Point Forecast","Interval Forecast")
    names(mytable)[1] <- paste(stockabundance)
    mytable

    for (i in 1:length(point_forecast_results)){

         mytable[i,1] <- names(point_forecast_results)[i]
         mytable[i,"Best Model"] <- point_forecast_results[[i]]$Model
         mytable[i,"Forecasting Year"] <- forecastingyear
         pfcst <- point_forecast_results[[i]]$PointForecast
         pfcst <- ifelse(pfcst>0,pfcst,0)
         pfcst <- round(pfcst)
         require(scales)
         pfcst <- as.character(comma(pfcst))
         mytable[i,"Point Forecast"] <- pfcst
         mytable[i,"Interval Forecast"] <- paste0(comma(interval_forecast_results[i,"PI.lwr"])," - ",comma(interval_forecast_results[i,"PI.upr"]))

    }

    usePackage("stringr")

    for (i in 1:length(point_forecast_results)){
       mytable[i,1] <- str_replace(mytable[i,1], "_", " ")
    }
    mytable

}

SIMPLESIBREG$total.index <- SIMPLESIBREG$best.rmse.youngest.age$index.min.rmse.total.age # index of best model used
##                                                               # to forecast abundance for
##                                                               # youngest age, where
##                                                               # index is 1 for naive (avg. of 5 years),
##                                                               #          2 for arima
##                                                               #          3 for exponential smoothing

SIMPLESIBREG$total.index


## usePackage("Hmisc")

capitalize  <- function (string) 
{
    capped <- grep("^[^A-Z]*$", string, perl = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped], 
        1, 1))
    return(string)
}


SIMPLESIBREG$youngest.age <- names(SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin)[1]
SIMPLESIBREG$youngest.age.capitalized <- capitalize(SIMPLESIBREG$youngest.age)


if (SIMPLESIBREG$total.index==1) {   # naive model for youngest age

   usePackage("scales")

   SIMPLESIBREG$ttrow <- c(SIMPLESIBREG$youngest.age.capitalized,
           "Naive Model (Average of Previous 5 Years)", paste(SIMPLESIBREG$forecastingyear),
           comma(round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.ctr)),
           paste0(comma(round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.lwr)),
                  " - ",
                  comma(round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.upr)))
           )
}

if (SIMPLESIBREG$total.index==2) {  # arima model for youngest age

    SIMPLESIBREG$arimafit <- SIMPLESIBREG$arima.model.fit.youngest
    SIMPLESIBREG$arimamodel <- SIMPLESIBREG$arimafit$model
      
    sink("arimafit.txt")
    print(SIMPLESIBREG$arimamodel)
    sink()

    SIMPLESIBREG$out <- readLines("arimafit.txt")
    
    usePackage("stringr")
    SIMPLESIBREG$out.pattern <- str_detect(string=SIMPLESIBREG$out, pattern="ARIMA")

    SIMPLESIBREG$modelarima <- SIMPLESIBREG$out[SIMPLESIBREG$out.pattern==TRUE]
    SIMPLESIBREG$modelarima <- str_trim(SIMPLESIBREG$modelarima)

    if (SIMPLESIBREG$boxcoxtransform==TRUE) {
          
          SIMPLESIBREG$out.lambda <- str_detect(string=SIMPLESIBREG$out, pattern="lambda")
          
          SIMPLESIBREG$lambda <- SIMPLESIBREG$out[SIMPLESIBREG$out.lambda==TRUE]
          SIMPLESIBREG$modellambda <- str_trim(SIMPLESIBREG$lambda, side="right")
       
          SIMPLESIBREG$modelarima <- paste0(SIMPLESIBREG$modelarima, "; ", SIMPLESIBREG$modellambda)
       
    } 

    usePackage("scales")

    SIMPLESIBREG$ttrow <- c(SIMPLESIBREG$youngest.age.capitalized,
               SIMPLESIBREG$modelarima, paste(SIMPLESIBREG$forecastingyear),
               comma(round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.ctr)),
                paste0(comma(round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.lwr)),
                  " - ",
                  comma(round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.upr))))
}

if (SIMPLESIBREG$total.index==3) { # exponential smoothing model for youngest age

    SIMPLESIBREG$expsmoothfit <- SIMPLESIBREG$expsmooth.model.fit.youngest
    SIMPLESIBREG$expsmoothmodel <- SIMPLESIBREG$expsmoothfit$model

    sink("expsmoothfit.txt")
    print(SIMPLESIBREG$expsmoothfit)
    sink()

    SIMPLESIBREG$out <- readLines("expsmoothfit.txt")
    
    usePackage("stringr")
    usePackage("scales")
    
    SIMPLESIBREG$out.pattern <- str_detect(string=SIMPLESIBREG$out, pattern="ETS")

    SIMPLESIBREG$modelexpsmooth <- SIMPLESIBREG$out[SIMPLESIBREG$out.pattern==TRUE]
    SIMPLESIBREG$modelexpsmooth <- str_trim(SIMPLESIBREG$modelexpsmooth)
    
    if (SIMPLESIBREG$boxcoxtransform==TRUE) {

          SIMPLESIBREG$out.lambda <- str_detect(string=SIMPLESIBREG$out, pattern="lambda")

          SIMPLESIBREG$lambda <- SIMPLESIBREG$out[SIMPLESIBREG$out.lambda==TRUE][2]
          SIMPLESIBREG$modellambda <- str_trim(SIMPLESIBREG$lambda, side="right")
          SIMPLESIBREG$modellambda <- str_trim(SIMPLESIBREG$lambda, side="left")

          SIMPLESIBREG$modelexpsmooth <- paste0(SIMPLESIBREG$modelexpsmooth, "; ", SIMPLESIBREG$modellambda)

    }
    

    SIMPLESIBREG$ttrow <- c(SIMPLESIBREG$youngest.age.capitalized,
               SIMPLESIBREG$modelexpsmooth, paste(SIMPLESIBREG$forecastingyear),
               comma(round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.ctr)),
                paste0(comma(round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.lwr)),
                  " - ",
                  comma(round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.upr))))
}




SIMPLESIBREG$tt <- rbind.data.frame(SIMPLESIBREG$ttrow,
                       SIMPLESIBREG$table_interval_forecasts(SIMPLESIBREG$results_best_fitting_model_for_each_age_class,
                                     SIMPLESIBREG$PI.individual.ages.simple.sibling.regression.no.comma,
                                     SIMPLESIBREG$forecastingyear, SIMPLESIBREG$stockabundance)
                       )


## pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]]


SIMPLESIBREG$tt[nrow(SIMPLESIBREG$tt)+1,] <- c("Total","-",paste(SIMPLESIBREG$forecastingyear),
                              comma(SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$p[[SIMPLESIBREG$total.index]]),
                              paste0(comma(SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$p.lwr[[SIMPLESIBREG$total.index]]),
                                    " - ",
                                    comma(SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$p.upr[[SIMPLESIBREG$total.index]]))
                              )

SIMPLESIBREG$point.and.interval.forecasts.simple.sibling.regression <- SIMPLESIBREG$tt


SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression <- list()

SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression$Return_Year <-
c("Return Year",
  SIMPLESIBREG$point.and.interval.forecasts.simple.sibling.regression[,"Forecasting Year"])


SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression$Best_Model  <-
c("Best Model",
   SIMPLESIBREG$point.and.interval.forecasts.simple.sibling.regression[,"Best Model"])

SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression$Point_Forecast <-
c("Point Forecast",
  SIMPLESIBREG$point.and.interval.forecasts.simple.sibling.regression[,"Point Forecast"])

SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression$Interval_Forecast <-
c("Interval Forecast",
  SIMPLESIBREG$point.and.interval.forecasts.simple.sibling.regression[,"Interval Forecast"])


SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression <- do.call(rbind.data.frame, 
                SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression)

SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression <- as.data.frame(SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression)


## Combine retrospective measures of forecast performance with
## point and interval forecast results

colnames(SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression)   <-
     colnames(SIMPLESIBREG$summary.forecast.results.simple.sibling.regression)

## rownames(summary.point.and.interval.forecasts.simple.sibling.regression) <- NULL
## rownames(summary.forecast.results.simple.sibling.regression) <- NULL


SIMPLESIBREG$summary.results.simple.sibling.regression <-
rbind.data.frame(SIMPLESIBREG$summary.point.and.interval.forecasts.simple.sibling.regression,
                 SIMPLESIBREG$summary.forecast.results.simple.sibling.regression)

SIMPLESIBREG$summary.results.simple.sibling.regression

SIMPLESIBREG$summary.results.simple.sibling.regression  <-
   data.frame(lapply(SIMPLESIBREG$summary.results.simple.sibling.regression, as.character), stringsAsFactors=FALSE)

#========================================================================================================
# Include Summary of Forecasting Results in Word Report
#========================================================================================================

doc = addTitle(doc, "Summary of Forecasting Results", level=1)


SIMPLESIBREG$tablecaption <- paste0("Summary of forecasting results for the ",
                      SIMPLESIBREG$forecastingyear, " ",
                      "age-specific and total ",
                      paste0(tolower(SIMPLESIBREG$stockabundance),"s"),
                      " associated with the ",
                      SIMPLESIBREG$stockname, " ",
                      SIMPLESIBREG$stockspecies,
                      " stock.")

doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

SIMPLESIBREG$tt <- SIMPLESIBREG$summary.results.simple.sibling.regression

usePackage("stringr")
names(SIMPLESIBREG$tt) <- str_replace_all(names(SIMPLESIBREG$tt),"_"," ")

## tt[,-1] <- comma(tt[,-1])

SIMPLESIBREG$my_ft <- FlexTable( data = SIMPLESIBREG$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))

# overwrites some paragraph formatting properties
SIMPLESIBREG$my_ft[, 1:ncol(SIMPLESIBREG$tt)] = parProperties(text.align = "right")



doc = addFlexTable(doc, flextable=SIMPLESIBREG$my_ft)

SIMPLESIBREG$my_ft <- NULL 
SIMPLESIBREG$tt <- NULL 

#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

SIMPLESIBREG$empirical.probability.yboot.simple.sibling.regression.total.age <- function(pred.int.total.age.simple.sibling.regression.all.models,
                                                               total.index, stockabundance){


    if (total.index==1) {   # naive model for youngest age (average of previous 5 years)

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]] ## point forecast of total abundance

    }

    if (total.index==2) {   # arima model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]] ## point forecast of total abundance

    }

    if (total.index==3) {   # exponential smoothing model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]] ## point forecast of total abundance

    }

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
    
    prob.interval.percentage <- c(NA, diff(prob.less.percentage))
    
    prob.interval.percentage <- round(prob.interval.percentage, 2)

    prob.threshold <- x.hist.breaks

    prob.thresholds <- data.frame(prob.threshold = prob.threshold,
                       prob.less.percentage = prob.less.percentage,
                       prob.greater.percentage = prob.greater.percentage, 
                       prob.interval.percentage = prob.interval.percentage)

    prob.thresholds

    ## cumulative probabilities evaluated for the point forecast of total abundance

    prob.less.pfct <- round(my.ecdf(pfct),4)
    prob.greater.pfct <- round(1 - my.ecdf(pfct),4)

    prob.less.percentage.pfct <- round(prob.less.pfct*100,2)

    prob.greater.percentage.pfct <- round(prob.greater.pfct*100,2)

    prob.threshold.pfct <- pfct
    
    prob.interval.percentage.pfct <- NA

    prob.pfct <-  data.frame(prob.threshold = prob.threshold.pfct,
                             prob.less.percentage = prob.less.percentage.pfct,
                             prob.greater.percentage = prob.greater.percentage.pfct, 
                             prob.interval.percentage = prob.interval.percentage.pfct)


    prob.pfct

    probs = list(prob.thresholds=prob.thresholds,
                 prob.point.forecast=prob.pfct)

    probs

}


SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age <- SIMPLESIBREG$empirical.probability.yboot.simple.sibling.regression.total.age(
                                                               SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,
                                                               SIMPLESIBREG$total.index, SIMPLESIBREG$stockabundance)




SIMPLESIBREG$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(SIMPLESIBREG$stockabundance)," ",
                       "value yet to be observed in ",
                       SIMPLESIBREG$forecastingyear, " for the ",
                       SIMPLESIBREG$stockname," ",
                       SIMPLESIBREG$stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(SIMPLESIBREG$stockabundance), ".")

doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

SIMPLESIBREG$tt_1 <- SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.thresholds

SIMPLESIBREG$tt_2 <- SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.point.forecast

SIMPLESIBREG$tt_1_and_2 <- rbind.data.frame(SIMPLESIBREG$tt_1, SIMPLESIBREG$tt_2)

usePackage("plyr")

## SIMPLESIBREG$tt_arrange <- arrange(SIMPLESIBREG$tt_1_and_2, SIMPLESIBREG$prob.threshold)

## SIMPLESIBREG$tt_arrange <- arrange(SIMPLESIBREG$tt_1_and_2, SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.point.forecast$prob.threshold)

SIMPLESIBREG$tt_arrange <- SIMPLESIBREG$tt_1_and_2[order(SIMPLESIBREG$tt_1_and_2$prob.threshold),]

### tt_arrange$prob.interval.percentage.updated <- c(NA, diff(tt_arrange$prob.less.percentage))

### tt_arrange$prob.interval.percentage <- NULL



## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2)

SIMPLESIBREG$from_tmp = which(SIMPLESIBREG$tt_arrange[,1] == SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.point.forecast$prob.threshold)

SIMPLESIBREG$tt_arrange[SIMPLESIBREG$from_tmp, 4] <- SIMPLESIBREG$tt_arrange[SIMPLESIBREG$from_tmp + 1, 4]

SIMPLESIBREG$tt_arrange[,1] <- comma(SIMPLESIBREG$tt_arrange[,1])
SIMPLESIBREG$tt_arrange[,2] <- paste0(sprintf("%.1f", SIMPLESIBREG$tt_arrange[,2]),"%")
SIMPLESIBREG$tt_arrange[,3] <- paste0(sprintf("%.1f", SIMPLESIBREG$tt_arrange[,3]),"%")
SIMPLESIBREG$tt_arrange[,4] <- paste0(sprintf("%.1f", SIMPLESIBREG$tt_arrange[,4]),"%")

names(SIMPLESIBREG$tt_arrange)[1] <- "Threshold"

names(SIMPLESIBREG$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(SIMPLESIBREG$tt_arrange)[3] <- "Prob(Actual > Threshold)"

## names(tt_arrange)[4] <- "Prob(Previous Threshold < Actual <= Current Threshold)"

names(SIMPLESIBREG$tt_arrange)[4] <- "Interval Probability"
SIMPLESIBREG$tt_arrange[1,4] <- "-"

SIMPLESIBREG$my_ft <- FlexTable( data = SIMPLESIBREG$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right"),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=2))

# overwrites some paragraph formatting properties
SIMPLESIBREG$my_ft[, 1:ncol(SIMPLESIBREG$tt_arrange)] = parProperties(text.align = "right")



SIMPLESIBREG$my_ft = spanFlexTableRows(SIMPLESIBREG$my_ft, j=4, from = SIMPLESIBREG$from_tmp, to = SIMPLESIBREG$from_tmp + 1)

SIMPLESIBREG$my_ft[SIMPLESIBREG$tt_arrange$Threshold %in% comma(SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

doc = addFlexTable(doc, flextable=SIMPLESIBREG$my_ft)


SIMPLESIBREG$my_ft_emp_prob_total <- SIMPLESIBREG$my_ft
SIMPLESIBREG$tablecaption_emp_prob_total <- SIMPLESIBREG$tablecaption

doc = addPageBreak(doc)

SIMPLESIBREG$my_ft <- NULL 
SIMPLESIBREG$tt_arrange <- NULL 

#========================================================================================================
# Introduction
#========================================================================================================


doc = addTitle(doc, "Introduction", level=1)


SIMPLESIBREG$sometext <- paste("In this report, we forecast the",
                  SIMPLESIBREG$forecastingyear,
                  "age-specific and total",
                  paste(tolower(SIMPLESIBREG$stockabundance),collapse="s",sep=""),
                  "for the",
                  SIMPLESIBREG$stockname,
                  SIMPLESIBREG$stockspecies,
                  "stock",
                  "using simple sibling regression models without environmental covariates.")

doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")


#=========================================================================================================
# Data used to model stock abundance for each age class
#=========================================================================================================

addTitle(doc, "Data Used for Simple Sibling Regression Modeling", level=1)


## doc = addPageBreak(doc)

is.even <- function(x) {x %% 2 == 0}
is.odd <- function(x) {x %% 2 != 0}

for (i in 1:length(SIMPLESIBREG$data_and_model_formulas$model_data)){

    SIMPLESIBREG$age <- names(SIMPLESIBREG$best.fits)[i]
    SIMPLESIBREG$age  <- tolower(SIMPLESIBREG$age)

    SIMPLESIBREG$tablecaption <- paste0("Data used for simple sibling regression modeling in connection with forecasting the ",
                          SIMPLESIBREG$age,
                          " ",
                          tolower(SIMPLESIBREG$stockabundance),
                          " for the ",
                          SIMPLESIBREG$stockname,
                          " ",
                          SIMPLESIBREG$stockspecies,
                          " stock."
                          )

    doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

    SIMPLESIBREG$tt <- SIMPLESIBREG$data_and_model_formulas$model_data[[i]]
    SIMPLESIBREG$ll <- 1:length(names(SIMPLESIBREG$tt))

    usePackage("scales")

    SIMPLESIBREG$tt[,names(SIMPLESIBREG$tt) %in%  names(SIMPLESIBREG$tt)[is.odd(SIMPLESIBREG$ll)][-1]] <- 
        comma(SIMPLESIBREG$tt[,names(SIMPLESIBREG$tt) %in%  names(SIMPLESIBREG$tt)[is.odd(SIMPLESIBREG$ll)][-1]])

    # define flex table and set default formatting properties
    SIMPLESIBREG$MyFTable = FlexTable(data=SIMPLESIBREG$tt, header.columns = FALSE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))

    # names(tt)[is.even(ll)]
    # names(tt)[is.odd(ll)][-1]

    names(SIMPLESIBREG$tt)[is.even(SIMPLESIBREG$ll)] <- substr(names(SIMPLESIBREG$tt)[is.even(SIMPLESIBREG$ll)],1,8)

    names(SIMPLESIBREG$tt)[is.odd(SIMPLESIBREG$ll)][-1] <- SIMPLESIBREG$stockabundance


    usePackage("stringr")
    names(SIMPLESIBREG$tt) <- str_replace_all(names(SIMPLESIBREG$tt),"_"," ")

    SIMPLESIBREG$ttt <- SIMPLESIBREG$data_and_model_formulas$model_data[[i]]
    SIMPLESIBREG$lll <- 1:length(names(SIMPLESIBREG$ttt))
    usePackage("stringr")
    names(SIMPLESIBREG$ttt) <- str_replace_all(names(SIMPLESIBREG$ttt),"_"," ")

    # add a first header row
    SIMPLESIBREG$MyFTable = addHeaderRow(SIMPLESIBREG$MyFTable,
                            value = c("",names(SIMPLESIBREG$ttt)[is.odd(SIMPLESIBREG$lll)][-1]),
                            colspan=c(1,rep(2,length(names(SIMPLESIBREG$tt)[is.odd(SIMPLESIBREG$ll)][-1]))),
                            par.properties=parProperties(text.align="center"))

    # add a second header row
    SIMPLESIBREG$MyFTable = addHeaderRow(SIMPLESIBREG$MyFTable, value = names(SIMPLESIBREG$tt))

    ## overwrite text formatting properties for first column
    ## MyFTable[,1] <- parProperties(text.align="center")


    ## overwrite text formatting properties for first row
    ## MyFTable[1,] <- parProperties(text.align="center")

    # format the table grid borders
    # MyFTable = setFlexTableBorders( MyFTable,
    #           inner.vertical = borderProperties( style = "none" ),
    #           inner.horizontal = borderProperties( ),
    #           outer.vertical = borderProperties( width = 0 ),
    #           outer.horizontal = borderProperties( width = 4 ),
    #           footer = FALSE)

    doc = addFlexTable(doc, SIMPLESIBREG$MyFTable)

    doc = addPageBreak(doc)
    
    SIMPLESIBREG$MyFTable <- NULL 
    
}


#=========================================================================================================
# Modeling: Older Ages (Simple Sibling Regression)
#=========================================================================================================

## doc = addPageBreak(doc)

addTitle(doc, "Simple Sibling Regression Modeling Results for the Older Age Components", level=1)


## youngest.age <- names(best.rmse.youngest.age$retro$resjoin)[1]

usePackage("stringr")
SIMPLESIBREG$youngest.age <- tolower(str_replace(SIMPLESIBREG$youngest.age,"_"," "))


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The simple sibling regression methodology is applicable only to the older age components of ",
                    # forecastingyear, " ",
                    SIMPLESIBREG$stockname, " ",
                    SIMPLESIBREG$stockspecies, " stock ",
                    tolower(SIMPLESIBREG$stockabundance), ". ",
                    "This methodology is not applicable to the youngest age represented in the available historical ",
                    tolower(SIMPLESIBREG$stockabundance), 
                    " data",
                    " (i.e., ",
                    SIMPLESIBREG$youngest.age,
                    "). ", 
                    "For the youngest age, naive modeling (i.e., average of previous five years) and ", 
                    "time series modeling (i.e., ARIMA and exponential smoothing) are utilized instead to identify the best forecasting model of ", 
                     tolower(SIMPLESIBREG$stockabundance), 
                    ".")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("For each of the older age components of ",
                    SIMPLESIBREG$stockname, " ",
                    SIMPLESIBREG$stockspecies, " stock ",
                    tolower(SIMPLESIBREG$stockabundance), ", ", 
                   "the simple sibling regression methodology involves fitting a simple linear regression model without intercept ",
                   "to the corresponding historical ", 
                    tolower(SIMPLESIBREG$stockabundance), " data ", 
                    "and using that model to forecast ", 
                    tolower(SIMPLESIBREG$stockabundance),
                    " for that age component in the year ", SIMPLESIBREG$forecastingyear, ". ",  
                    "The fitted model relates the older age component (Age k) to the younger age component (Age k - 1) of ",
                    "stock ",  tolower(SIMPLESIBREG$stockabundance), 
                    ". ", 
                    "The table below displays the simple linear regression models considered for the older age components of the stock.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

## insert candidate models here


SIMPLESIBREG$tablecaption <- paste0("Simple sibling regression models considered for the older age components of the stock. ",
                      "The -1 notation appearing in each model formula denotes the fact that the model lacks an intercept term. ")
doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

SIMPLESIBREG$tt <- SIMPLESIBREG$model_weights
SIMPLESIBREG$tt <- subset(SIMPLESIBREG$tt, select=c(Age_Class, Model))

usePackage("stringr")
names(SIMPLESIBREG$tt) <- str_replace_all(names(SIMPLESIBREG$tt),"_"," ")

SIMPLESIBREG$MyFTable = FlexTable(data=SIMPLESIBREG$tt, header.columns=TRUE) # create flexTable

## MyFTable = spanFlexTableRows(MyFTable, j="Age_Class", runs=as.character(tt[,"Age_Class"]))

SIMPLESIBREG$MyFTable = spanFlexTableRows(SIMPLESIBREG$MyFTable, j="Age Class", runs=as.character(SIMPLESIBREG$tt[,"Age Class"]))

doc = addFlexTable(doc, SIMPLESIBREG$MyFTable,
                    layout.properties=tableProperties(header.text=textProperties(font.size=10, font.weight="bold"),
                                                 data.text=textProperties(font.size=10)))

## doc = addTable(doc, data=tt,
##               layout.properties=tableProperties(header.text=textProperties(font.size=10, font.weight="bold"),
##                                                 data.text=textProperties(font.size=10)))

## rm(tablecaption)
## rm(tt)

SIMPLESIBREG$tt <- NULL 
SIMPLESIBREG$tablecaption <- NULL 

doc = addParagraph(doc, value=" ", stylename="Normal")


SIMPLESIBREG$paragraph <- paste("The output associated with the simple sibling regression models considered for the older age components of the",  
                   SIMPLESIBREG$stockname, SIMPLESIBREG$stockspecies, "stock", 
                   "is reported in the tables below.")

doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

for (i in 1:length(SIMPLESIBREG$results_best_fitting_model_for_each_age_class)) {
  
  SIMPLESIBREG$tablecaption <- paste0("Output associated with the simple sibling regression model considered for the ",
                        tolower( names(SIMPLESIBREG$best.fits))[i], " ",
                        "component of the ",
                        tolower(SIMPLESIBREG$stockabundance), " for the ",
                        SIMPLESIBREG$stockname, " ",
                        SIMPLESIBREG$stockspecies, " stock.")
  doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")
  SIMPLESIBREG$tt <- SIMPLESIBREG$format_results_best_fitting_model_for_each_age_class(SIMPLESIBREG$results_best_fitting_model_for_each_age_class,i)
  SIMPLESIBREG$ft_tt <- FlexTable(data=SIMPLESIBREG$tt, header.columns=TRUE)
  doc = addFlexTable(doc, flextable=SIMPLESIBREG$ft_tt,
                 layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
                                                   data.cell=cellProperties(border.color="black")))
  doc = addParagraph(doc, value=" ", stylename="Normal")
  
  SIMPLESIBREG$tablecaption <- NULL 
  SIMPLESIBREG$ft_tt <- NULL 
  SIMPLESIBREG$tt <- NULL 
  
}


#%%%
#---
#%%%


SIMPLESIBREG$paragraph <- paste("Model fit information pertaining to the simple sibling regression models considered for the older age components of the",  
                   SIMPLESIBREG$stockname, SIMPLESIBREG$stockspecies, "stock ", 
                   "is reported in the table below. ", 
                   "This information includes the following quantities: ", 
                   "i) standard error of regression (sigma),  ii) R-squared (Rsq),  iii) adjusted R-squared (Adj_Rsq),",  
                   " iv) log-likelihood (Log_Lik) and v) corrected Akaike Information Criterion (AICc) value.")

doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$tablecaption <- paste0("Model fit information pertaining to the simple sibling regression models considered for the older age components of the ",  
                      toupper(SIMPLESIBREG$stockname),
                      " stock.")
doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

SIMPLESIBREG$tt_res <- SIMPLESIBREG$model_selection_table_updated(SIMPLESIBREG$results_model_selection)

SIMPLESIBREG$tt_res$Num_Param <- NULL

SIMPLESIBREG$MyFTable = FlexTable(data=SIMPLESIBREG$tt_res, header.columns=TRUE) # create flexTable

doc = addFlexTable(doc, SIMPLESIBREG$MyFTable,
                    layout.properties=tableProperties(header.text=textProperties(font.size=10, font.weight="bold"),
                                                 data.text=textProperties(font.size=10)))

doc = addParagraph(doc, value=" ", stylename="Normal")

## rm(tt_res)

SIMPLESIBREG$tt_res <- NULL 
SIMPLESIBREG$MyFTable <- NULL 
SIMPLESIBREG$tablecaption <- NULL 

##
## Best Model Fits:  Effect Visualization
##

# doc = addPageBreak(doc)

SIMPLESIBREG$pots <- pot("To help visualize the effects estimated by the simple sibling regression models for the older ages, we resort to ") +
        pot("effect displays ", textProperties(font.style="italic")) +
        pot("(Fox, 1987, 2003).")
SIMPLESIBREG$pots.text <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.text, stylename="Normal", 
                   par.properties=parProperties(text.align="justify", padding=10))

SIMPLESIBREG$pots <- NULL 
SIMPLESIBREG$pots.text <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <- pot("Effect displays are graphs of fitted values computed from an estimated linear regression model ") + 
        pot("that allow us to see how the expected value of the response variable changes with the values of each of the predictors in the model. ") +
        pot("As such, effect displays are an alternative to interpreting linear regression models directly from the estimated coefficients.")
SIMPLESIBREG$pots.text <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.text, stylename="Normal",
                   par.properties=parProperties(text.align="justify", padding=10))

SIMPLESIBREG$pots.text <- NULL 
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <- pot("In this report, all effect displays are constructed under the assumption that the sole predictor in the intercept-free, simple linear regression model of interest has a linear effects on the response variable. ")
SIMPLESIBREG$pots.text <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.text, stylename="Normal", 
                   par.properties=parProperties(text.align="justify", padding=10))

SIMPLESIBREG$pots.text <- NULL 
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <- pot("Because each of the regression models considered in this report includes a single predictor with an assumed linear effect, that model will generate a single effect display which will reveal the linear effect of that predictor on the response variable.") +
        pot(" This display consists of a straight line, whose slope captures the direction and strength of the relationship between the predictor and the response. ") +
        pot("A positive slope indicates a positive effect, whereas a negative slope indicates a negative effect. ") +
        pot("On the other hand, a small slope corresponds to a small effect while a large slope corresponds to a large effect.")
SIMPLESIBREG$pots.text <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.text, stylename="Normal", 
                   par.properties=parProperties(text.align="justify", padding=10))

SIMPLESIBREG$pots <- NULL 
SIMPLESIBREG$pots.text <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <- pot("All effect displays included in this report are accompanied by 95% (pointwise) confidence bands,") +
        pot(" which are derived by exploiting the fact that ") +
        pot(" effect displays are collections of fitted values.  This makes it straightforward to estimate the standard errors of the effects. ") +
        pot("The width of the 95% confidence band associated with an effect plot reflects the precision of estimation of the effect displayed in the plot. ") +
        pot("The narrower this width, the more precise the estimated linear effect. ")
SIMPLESIBREG$pots.text <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.text, stylename="Normal", 
                  par.properties=parProperties(text.align="justify", padding=10))

SIMPLESIBREG$pots.text <- NULL
SIMPLESIBREG$pots <- NULL


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <-  pot("The effect displays in this report also include residuals.")
SIMPLESIBREG$pots.text <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.text, stylename="Normal", 
                  par.properties=parProperties(text.align="justify", padding=10))

SIMPLESIBREG$pots <- NULL 
SIMPLESIBREG$pots.text <- NULL 

for (i in 1:length(SIMPLESIBREG$results_best_fitting_model_for_each_age_class)){

    doc = addPageBreak(doc)

    SIMPLESIBREG$current_age <- all.vars(SIMPLESIBREG$results_best_fitting_model_for_each_age_class[[i]]$formula)[1]
    usePackage("stringr")
    SIMPLESIBREG$current_age <- str_replace(SIMPLESIBREG$current_age,"_"," ")
    SIMPLESIBREG$current_age <- tolower(SIMPLESIBREG$current_age)

    SIMPLESIBREG$plotlegend <- paste0("Effect display for the simple sibling regression model corresponding to the ",
                         SIMPLESIBREG$current_age, " component of the ",
                         tolower(SIMPLESIBREG$stockabundance), " ",
                         "for the ",
                         SIMPLESIBREG$stockname, " ",
                         SIMPLESIBREG$stockspecies, " ",
                         "stock.")


    SIMPLESIBREG$myplot <- SIMPLESIBREG$my.effect.plot.multiple.predictor(
                             SIMPLESIBREG$results_best_fitting_model_for_each_age_class, 
                             SIMPLESIBREG$stockabundance, i)

    doc = addPlot(doc,
            fun = print,
            x = SIMPLESIBREG$myplot,
            width = plotwidth, height=plotheight)

    doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")

    ## rm(myplot)

    SIMPLESIBREG$myplot <- NULL 

}


#=========================================================================================================
# Model Diagnostics
#=========================================================================================================

## Residuals vs. Fitted Values: Best Fitting Models

# Mamic

SIMPLESIBREG$best.fits

doc = addPageBreak(doc)

doc = addTitle(doc, paste("Model Diagnostics for the Simple Sibling Regression Models"), level=1)


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("When fitting a simple sibling regression model to a particular data set, many problems may occur. ",
                    "Most common among these are the following:")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- c("   1. Non-linearity of the response-predictor relationship(s);",
              "   2. Correlation of the model errors;",
              "   3. Non-constant variance of the error terms;",
              "   4. Non-normality of error terms;",
              "   5. Outliers;",
              "   6. High-leverage points;",
              "   7. Collinearity.")
doc = addParagraph(doc, value=SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
SIMPLESIBREG$paragraph <- paste0("Identifying and overcoming these problems is as much an art as it is a science.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("For each of the older age components, ",
                    "the corresponding simple sibling regression model fits the underlying data well ",
                    "if the observations in the plot of the residuals versus the fitted values are randomly scattered ",
                    "about the horizontal line going through zero. ",
                    "Any systematic pattern seen in this plot (e.g., funnel shape, non-linear shape) ",
                    "and/or any unusual features (e.g., outliers, gaps) ",
                    "are indicative of an inadequate model fit and warrant further investigation.")

doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

##
## Correlation of Model Errors
##

pots <- pot("Correlation of Model Errors", textProperties(font.weight="bold", font.style="italic"))
pots.par <- set_of_paragraphs(pots)
doc = addParagraph(doc, value=pots.par, stylename="Normal")

SIMPLESIBREG$pots.par <- NULL
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("An important assumption for simple sibling regression models is that ",
                    "their error terms are uncorrelated. What does this mean? For example, if",
                    " the errors are uncorrelated, then the fact that the error term corresponding to one of ",
                    "the model observations is positive provides little or no information about the sign",
                    " of the error term corresponding to the next model observation.",
                    " If the error terms are correlated and we ignore this at the modeling and forecasting stage, ",
                    "we may have an unwarranted sense of confidence in the results produced by the model.",
                    " In particular, prediction intervals may be narrower than they should be.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("Correlations among the model errors tend to occur when ",
                                 "the data utilized in the model were collected over time (e.g., on an annual basis).")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In order to detect the presence of correlation among the errors ",
                    "associated with a simple sibling regression model, we need to plot the residuals from the model as a function of time, ",
                    "obtaining a so-called time series plot of residuals. If the model errors are uncorrelated, ",
                    "then there should be no discernable pattern in this plot. ",
                    "On the other hand, if the residuals are positively correlated, then we may see tracking in the residuals ",
                    "- that is, adjacent residuals may have similar values.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("Other graphical tools for detecting correlation among the errors associated ",
                    "with a simple sibling regression model include the autocorrelation and partial autocorrelation plots. ",
                    "If the errors are uncorrelated, all of the autocorrelations and partial autocorrelations ",
                    "displayed in these plots would be expected to lie within the ",
                    " 95% confidence bands. ",
                    "The following table provides further guidance on how to interpret the autocorrelation and partial autocorrelation plots corresponding",
                    " to various underlying processes that may have generated the errors.")

doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

doc = addPageBreak(doc)

SIMPLESIBREG$cortable <- matrix(NA, nrow=5, ncol=3)
SIMPLESIBREG$cortable[1,] <- c("Nonstationary",
                  "Autocorrelations do not die out; they remain large or diminish approximately linearly","")
SIMPLESIBREG$cortable[2,] <- c("Stationary",
                  "After the first few lags, autocorrelations die out (i.e., they collapse toward 0 in some combination of exponential decay or damped oscillation)",
                  "")

SIMPLESIBREG$cortable[3,] <- c("AR(p)",
                  "Autocorrelations die out",
                  "Partial autocorrelations cut off after the first p lags")

SIMPLESIBREG$cortable[4,] <- c("MA(q)",
                  "Autocorrelations cut off after the first q lags",
                  "Partial autocorrelations die out")

SIMPLESIBREG$cortable[5,] <- c("ARMA(p,q)",
                  "Autocorrelations die out after first q-p lags",
                  "Partial autocorrelations die out after first p-q lags")

SIMPLESIBREG$cortable <- as.data.frame(SIMPLESIBREG$cortable)

names(SIMPLESIBREG$cortable) <- c("Process","Autocorrelation Function","Partial Autocorrelation Function")

doc = addParagraph(doc, value=paste("Guidelines for interpreting the autocorrelation and partial autocorrelation functions."), stylename="rTableLegend")

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

# Create a FlexTable with data.frame dataset
SIMPLESIBREG$MyFTable = FlexTable( data = SIMPLESIBREG$cortable,
                      body.cell.props = baseCellProp,
                      header.cell.props = baseCellProp,
                      header.par.props = parProperties(text.align = "left" )
)


doc = addFlexTable(doc, SIMPLESIBREG$MyFTable)

SIMPLESIBREG$cortable <- NULL 
SIMPLESIBREG$MFTable <- NULL 

paragraph <- paste0(" ")
doc = addParagraph(doc, value=paragraph, stylename="Normal")

##
## Non-Constant Variance of Error Terms
##

paragraph <- paste0(" ")

SIMPLESIBREG$pots <- pot("Non-Constant Variance of Error Terms", textProperties(font.weight="bold", font.style="italic"))
SIMPLESIBREG$pots.par <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.par, stylename="Normal")

SIMPLESIBREG$pots.par <- NULL 
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("Another important assumption for simple sibling regression models is that their error terms have a constant variance. ",
                    "However, in practice it is often the case that the variances of the error terms are non-constant.  For instance, ",
                    "the variances of the error terms may increase with the value of the response variable included in the model.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("We can identify non-constant variances in the errors, or heteroscedasticity, ",
                                 "from the presence of a funnel shape in the plot of residuals versus fitted values.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("When faced with the problem of heteroscedasticity, ",
                    "one possible solution is to log-transform the response variable.  Such a transformation ",
                    "tends to result in a greater amount of shrinkage of the larger responses, leading to a reduction in heteroscedasticity.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <- pot("Non-Normality of Error Terms", textProperties(font.weight="bold", font.style="italic"))
SIMPLESIBREG$pots.par <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.par, stylename="Normal")

SIMPLESIBREG$pots.par <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("When the error terms in a simple sibling regression model are not normally distributed, least squares estimates produced by the model ",
                    "may not be optimal.  They will still be best linear unbiased estimates, but other robust estimators may be more effective. ",
                    "Also, the p-values of the tests of statistical significance of the effects represented in the model are no longer exact. ")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("Non-normality of the model errors can be diagnosed by examining histogram and density plots of the model residuals along with",
                    " normal probability plots of the model residuals.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("When non-normality of the model errors is found, the resolution depends on the type of problem found:")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- c("For short-tailed error distributions, the consequences of non-normality are not serious and can reasonably be ignored.",
           "For skewed error distributions, a transformation of the response may solve the problem.",
           "For long-tailed error distributions, we might just accept the non-normality and base the inference and/or prediction on",
           "resampling methods such as the bootstrap."
           )
doc = addParagraph( doc, value = SIMPLESIBREG$sometext, stylename="BulletList" )

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <- pot("Outliers", textProperties(font.weight="bold", font.style="italic"))
SIMPLESIBREG$pots.par <- set_of_paragraphs(SIMPLESIBREG$pots)                                                                                  
doc = addParagraph(doc, value=SIMPLESIBREG$pots.par, stylename="Normal")

SIMPLESIBREG$pots.par <- NULL 
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In a simple sibling regression model, an outlier is a regression observation whose corresponding response value is far from the ",
                    "value predicted by the model.  Observations deemed to be outliers need to be investigated further as they have the potential to influence the ",
                    "model fit. ",
                    "(An observation is said to influence the model fit if omitting that observation from the model significantly changes the model fit.)")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("To identify outliers, we can use the studentized residuals. ",
                     "Studentized residuals are computed by dividing each raw residual by its estimated standard error.",
                     " Observations whose studentized residuals are ",
                     "greater than 3 in absolute value are possible outliers.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <- pot("High-Leverage Observations", textProperties(font.weight="bold", font.style="italic"))
SIMPLESIBREG$pots.par <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.par, stylename="Normal")

SIMPLESIBREG$pots.par <- NULL 
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In a simple sibling regression model, high-leverage observations are regression observations ",
                    "that are relatively far from the center of the predictor space,",
                    " taking into account the correlational pattern among the predictors. ",
                    " Such observations could have a large impact on the regression model fit so they need to be identified and investigated further.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("High leverage observations can be identified on the basis of ",
                    "the index plot of leverage (or hat) values. Any observation whose reported leverage value ",
                    "exceeds twice the average leverage of all observations is deemed to have high-leverage. ",
                    "(The leverage value quantifies how extreme an observation is in the predictor space when compared to all other observations.)")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots <- pot("Influence", textProperties(font.weight="bold", font.style="italic"))
SIMPLESIBREG$pots.par <- set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, value=SIMPLESIBREG$pots.par, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("A simple sibling regression model observation that is both outlying and has high leverage exerts ",
                    "influence on the regression coefficients, in the ",
                    "sense that if the observation is removed, the coefficients change considerably.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The most common measure of influence of an observation is Cook's distance, expressed as a product of two factors. ",
                    "The first factor represents a measure of the outlyingness of the observation and the second factor represents ",
                    "a measure of leverage of the observation. ",
                    "Observations with a large Cook's distance are potentially influential cases.  In this report, we deem a Cook's distance value to be large if ",
                    "it exceeds the threshold 4/n, where n is the number of observations included in the model.")
doc = addParagraph(doc, value=SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

doc = addPageBreak(doc)


SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.residuals.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)

if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x = SIMPLESIBREG$myplot,
            width=plotwidth, 
            height = SIMPLESIBREG$plotheight.tmp
            )
            
doc = addParagraph(doc, value=paste("Plots of residuals versus fitted values corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotheight.tmp <- NULL 

## Histogram of Residuals: Best Fitting Models


SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.histresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun = print,
            x = SIMPLESIBREG$myplot,
            width = plotwidth, 
            height = SIMPLESIBREG$plotheight.tmp)

doc = addParagraph(doc, value=paste("Histograms of residuals corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotheight.tmp <- NULL 

## Density Plot of Residuals: Best Fitting Models


SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.densresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x = SIMPLESIBREG$myplot,
            width = plotwidth, 
            height = SIMPLESIBREG$plotheight.tmp)

doc = addParagraph(doc, value=paste("Density plots of residuals corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotheight.tmp <- NULL 

## Time Series Plots of Residuals: Best Fitting Models

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.timeresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun = print,
            x = SIMPLESIBREG$myplot,
            width = plotwidth, 
            height = SIMPLESIBREG$plotheight.tmp)

doc = addParagraph(doc, value=paste("Time series plots of residuals corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL
SIMPLESIBREG$plotheight.tmp <- NULL

## ACF Plots of Residuals: Best Fitting Models


SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.acfresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)

if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun = print,
            x = SIMPLESIBREG$myplot,
            width = plotwidth, 
            height = SIMPLESIBREG$plotheight.tmp)

doc = addParagraph(doc, value=paste("ACF plots of residuals corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotheight.tmp <- NULL 

## PACF Plots of Residuals: Best Fitting Models

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.pacfresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLESIBREG$myplot,
            width=plotwidth, 
            height=SIMPLESIBREG$plotheight.tmp)

doc = addParagraph(doc, value=paste("PACF plots of residuals corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL
SIMPLESIBREG$plotheight.tmp <- NULL 

## Index Plots of Studentized Residuals: Best Fitting Models

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.studentresid.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLESIBREG$myplot,
            width=plotwidth, 
            height=SIMPLESIBREG$plotheight.tmp)

doc = addParagraph(doc, value=paste("Index plots of studentized residuals corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotheight.tmp <- NULL 

## Index Plots of Leverage Values: Best Fitting Models

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.hatvalues.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLESIBREG$myplot,
            width=plotwidth, 
            height=SIMPLESIBREG$plotheight.tmp)

doc = addParagraph(doc, value=paste("Index plots of leverage values corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotheight.tmp <- NULL 

## Index Plots of Cook's Distances: Best Fitting Models

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.cooks.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x = SIMPLESIBREG$myplot,
            width=plotwidth, 
            height=SIMPLESIBREG$plotheight.tmp)

doc = addParagraph(doc, value=paste("Index plots of Cook's distances corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotheight.tmp <- NULL 

## Influence Plots:  Best Fitting Models

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.cooks.bubble.simple.sibling.regression.best.fitting.models(SIMPLESIBREG$best.fits)


if (length(SIMPLESIBREG$best.fits)==4) {
   SIMPLESIBREG$plotheight.tmp <- plotheight + 1
} else if (length(SIMPLESIBREG$best.fits)==3) {
   SIMPLESIBREG$plotheight.tmp <- plotheight 
} else if (length(SIMPLESIBREG$best.fits)==2) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 1
} else if (length(SIMPLESIBREG$best.fits)==1) {
   SIMPLESIBREG$plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun = print,
            x = SIMPLESIBREG$myplot,
            width = plotwidth, 
            height = SIMPLESIBREG$plotheight.tmp)
doc = addParagraph(doc, value=paste("Influence plots corresponding to the simple sibling regression models."), stylename="rPlotLegend")

## rm(myplot)
## rm(plotheight.tmp)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotheight.tmp <- NULL 

#=========================================================================================================
# Modeling: Youngest Age  (RETURN HERE)
#=========================================================================================================

doc = addPageBreak(doc)

addTitle(doc, "Modeling Results for the Youngest Age Component", level=1)


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The sibling regression methodology cannot be used to produce the ",
                     SIMPLESIBREG$forecastingyear, " ",
                     "forecast of ",
                     tolower(SIMPLESIBREG$stockabundance), " for the ",
                     SIMPLESIBREG$youngest.age,
                     " component of the ",
                     SIMPLESIBREG$stockname, " ",
                     SIMPLESIBREG$stockspecies, " stock. ",
                    "To obtain this forecast, ",
                    " we need to use a different methodology.  The methodology involves five steps, which are described below.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph = pot('Step 1', textProperties(font.weight='bold', font.style = 'italic')) 
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In Step 1, we fit three different models to the ",
                    tolower(SIMPLESIBREG$stockabundance),
                    " time series available for the ",
                    SIMPLESIBREG$youngest.age, " component of the stock: ",
                    "1) a naive time series model (i.e., average of previous 5 years), ",
                    "2) an ARIMA time series model and ",
                    "3) an exponential smoothing model.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 

SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("We use automatic model selection based on the Akaike's Information Criterion (AIC) to determine the optimal ARIMA model for the ",
                    " (possibly Box-Cox transformed) ", 
                    tolower(SIMPLESIBREG$stockabundance),
                    " series corresponding to ", SIMPLESIBREG$youngest.age, ".")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In a similar fashion, we use Akaike's Information Criterion (AIC), corrected for small sample bias,",
                    " to select the optimal exponential smoothing model for the ",
                    " (possibly Box-Cox transformed) ", 
                    tolower(SIMPLESIBREG$stockabundance),
                    " series corresponding to ", SIMPLESIBREG$youngest.age, ".")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <-  paste0("If selected by the user, the Box-Cox transformation can be applied to the time series ", 
                     "available for the ",
                     SIMPLESIBREG$youngest.age, " component of the stock ", 
                    "prior to ", 
                    "carrying out the ARIMA and exponential smoothing modeling. ",
                    "The transformation can stabilize the variance of the original time series ", 
                    "(e.g., by removing large fluctuations in the series or ", 
                    "by making the patterns noticed in the series more consistent across the entire span of the series). ",
                    "As a result, the transformation can lead to simpler forecasting models which may produce more accurate forecasts. ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )                    
         
SIMPLESIBREG$paragraph <- NULL           


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The Box-Cox transformation encompasses a family of transformations that includes logarithms and power transformations. ", 
                    "Its application involves identifying an appropriate exponent (lambda) which indicates ", 
                    "the power to which all of the original time series values should be raised prior to modeling. ", 
                    "In this report, the optimal value of lambda for the Box-Cox transformation is found in accordance with the method proposed by ", 
                    "Guerrero in 1993.  This method identifies the lambda value which minimizes the coefficient of variation ", 
                    "for subseries of the original time series.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("Once the optimal value for the exponent lambda governing the Box-Cox transformation is determined, ", 
                    "the ARIMA and exponential smoothing models are applied to the Box-Cox transformed data in order to produce point forecasts of abundance ",
                    "for the youngest age component of the stock. ", 
                    "These point forecasts are back-transformed via a reverse Box-Cox transformation to obtain point forecasts on the original scale. ", 
                    "Further details about the Box-Cox transformation can be found at https://www.otexts.org/fpp/2/4.") 
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph = pot('Step 2', textProperties(font.weight='bold', font.style = 'italic')) 
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In Step 2, we use each of the three models (i.e., the naive model along with the optimal ARIMA",
                    " and optimal exponential smoothing models)",
                    "to compute retrospective point forecasts of ",
                    tolower(SIMPLESIBREG$stockabundance),
                    " for all of the historical return years preceding the forecasting year ",
                    SIMPLESIBREG$forecastingyear,
                    " except for the first ", SIMPLESIBREG$index.year, 
                    " historical return years.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLESIBREG$paragraph = pot('Step 3', textProperties(font.weight='bold', font.style = 'italic')) 
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL
 

SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In Step 3, for each model identified in the previous step, we add the ",
                    " retrospective point forecasts of ",
                    tolower(SIMPLESIBREG$stockabundance),
                    " for ", SIMPLESIBREG$youngest.age, " ",
                    " to the retrospective point forecasts of ",
                    tolower(SIMPLESIBREG$stockabundance),
                    " for the older ages which were produced by sibling regression. ",
                    "This enables us to compute retrospective point forecasts of total ",
                    tolower(SIMPLESIBREG$stockabundance)," for all of the historical return years preceding the forecasting year ",
                    SIMPLESIBREG$forecastingyear,
                    " except for the first ", SIMPLESIBREG$index.year, 
                    " historical return years.",
                    " Comparing these retrospective point forecasts against the actual total ",
                    tolower(SIMPLESIBREG$stockabundance),
                    " values yields the retrospective forecast errors.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph = pot('Step 4', textProperties(font.weight='bold', font.style = 'italic')) 
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In Step 4, we use the retrospective point forecasts of total ",
                    tolower(SIMPLESIBREG$stockabundance),
                    " along with the accompanying ",
                    "retrospective forecast errors ",
                    "to compute the RMSE measure corresponding to each of the naive, ARIMA and exponential smoothing models.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph = pot('Step 5', textProperties(font.weight='bold', font.style = 'italic')) 
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("In Step 5, we compare the naive, ARIMA and exponential smoothing models ", 
                    "used to forecast the youngest age component of the stock ", 
                    "based on the values of the RMSE measure and we retain the model with the lowest value for this measure. ", 
                    "The winning model (i.e., the model producing the lowest RMSE value) will be the one used to forecast ", 
                    "abundance for the youngest age component of the stock.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

##
## Fitted Values for Candidate Models for Youngest Age:
##

## options(warn=1)

SIMPLESIBREG$figurecaption <- paste0("Fitted values produced by the three candidate models used for forecasting ",
                         "the youngest age component of the ", SIMPLESIBREG$forecastingyear, " ",
                         tolower(SIMPLESIBREG$stockabundance),
                         " for the ",
                         SIMPLESIBREG$stockname, " ",
                         SIMPLESIBREG$stockspecies,
                         " stock.")

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.fitted.value.youngest(
                           SIMPLESIBREG$avgfive.model.fit.youngest,
                           SIMPLESIBREG$arima.model.fit.youngest,
                           SIMPLESIBREG$expsmooth.model.fit.youngest)

doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLESIBREG$myplot,
        width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

# rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

## options(warn=2)

##
## Model Fit for Youngest Age: ARIMA
##

isEmpty <- function(x) {return(identical(x, numeric(0)))}

SIMPLESIBREG$extract.arima <- function(model){

   extract.arima.fit <- model

   sink("arimafit.txt")
   print(extract.arima.fit)
   sink()
   
   out <- readLines("arimafit.txt")
   usePackage("stringr")
   out.pattern <- str_detect(string=out, pattern="ARIMA")
   
   out.coefficients <- str_detect(string=out, pattern="Coefficients")
   flag <- sum(out.coefficients)  # flag = 0 means no Coefficients are reported for ARIMA model for youngest age
                                  # flag = 1 means Coefficients are reported for ARIMA model for youngest age
      
   modelarima <- out[out.pattern==TRUE]
   require(stringr)
   modelarima <- str_trim(modelarima)

   ## flag <- grepl("ARIMA(0,0,0)", modelarima, fixed=TRUE)

   if (flag > 0) { # if Coefficients are reported for ARIMA model for youngest age 
   
      extract.arima.fit.coef.names <- attr(extract.arima.fit$coef,"names")
      extract.arima.fit.coef <- round(extract.arima.fit$coef,4)
      
      if (!isEmpty(extract.arima.fit$var.coef)) {
        
          extract.arima.fit.se <- round(sqrt(diag(extract.arima.fit$var.coef)),4)
        
          ## http://stats.stackexchange.com/questions/8868/how-to-calculate-the-p-value-of-parameters-for-arima-model-in-r

          if (length(extract.arima.fit$coef)==1){
              extract.arima.fit.z.statistic <- extract.arima.fit$coef/sqrt(extract.arima.fit$var.coef)
          } else if (length(extract.arima.fit$coef)>1) {
              extract.arima.fit.z.statistic <- extract.arima.fit$coef/sqrt(diag(extract.arima.fit$var.coef))
          }
      
          extract.arima.fit.p.value <- pnorm(abs(extract.arima.fit.z.statistic), lower.tail=FALSE)*2
          extract.arima.fit.p.value <- round(extract.arima.fit.p.value, 4)
        
          if (length(extract.arima.fit$coef)==1){
              extract.arima.fit.table <- data.frame(extract.arima.fit.coef.names,
                                            extract.arima.fit.coef,
                                            extract.arima.fit.se,
                                            round(extract.arima.fit.z.statistic,4),
                                            extract.arima.fit.p.value)
          } else if (length(extract.arima.fit$coef)>1) {
             extract.arima.fit.table <- cbind.data.frame(extract.arima.fit.coef.names,
                                            extract.arima.fit.coef,
                                            extract.arima.fit.se,
                                            round(extract.arima.fit.z.statistic,4),
                                            extract.arima.fit.p.value)
          } # end if else if 

      
          rownames(extract.arima.fit.table) <- extract.arima.fit.coef.names
          colnames(extract.arima.fit.table) <- c("Coefficient","Estimate","Standard Error","Z-statistic","P-value")

          extract.arima.fit.aic <- extract.arima.fit$aic
          extract.arima.fit.bic <-  extract.arima.fit$bic
          extract.arima.fit.aicc <-  extract.arima.fit$aicc

          extract.arima.fit.sigma2 <- extract.arima.fit$sigma2
          extract.arima.fit.loglik <- extract.arima.fit$loglik

          return(list(extract.arima.fit.model=modelarima,
               extract.arima.fit.table=extract.arima.fit.table,
               extract.arima.fit.aic=extract.arima.fit.aic,
               extract.arima.fit.bic=extract.arima.fit.bic,
               extract.arima.fit.aicc=extract.arima.fit.aicc,
               extract.arima.fit.sigma2=extract.arima.fit.sigma2,
               extract.arima.fit.loglik=extract.arima.fit.loglik))
                     
      
      } # end !isEmpty() 
     
     
      if (isEmpty(extract.arima.fit$var.coef)) {
        
          if (length(extract.arima.fit$coef)==1){
              extract.arima.fit.table <- data.frame(extract.arima.fit.coef.names,
                                            extract.arima.fit.coef)
          } else if (length(extract.arima.fit$coef)>1) {
             extract.arima.fit.table <- cbind.data.frame(extract.arima.fit.coef.names,
                                            extract.arima.fit.coef)
          } # end if else if 

      
          rownames(extract.arima.fit.table) <- extract.arima.fit.coef.names
          colnames(extract.arima.fit.table) <- c("Coefficient","Estimate")

          extract.arima.fit.aic <- extract.arima.fit$aic
          extract.arima.fit.bic <-  extract.arima.fit$bic
          extract.arima.fit.aicc <-  extract.arima.fit$aicc

          extract.arima.fit.sigma2 <- extract.arima.fit$sigma2
          extract.arima.fit.loglik <- extract.arima.fit$loglik

          return(list(extract.arima.fit.model=modelarima,
               extract.arima.fit.table=extract.arima.fit.table,
               extract.arima.fit.aic=extract.arima.fit.aic,
               extract.arima.fit.bic=extract.arima.fit.bic,
               extract.arima.fit.aicc=extract.arima.fit.aicc,
               extract.arima.fit.sigma2=extract.arima.fit.sigma2,
               extract.arima.fit.loglik=extract.arima.fit.loglik))
        
      } # end isEmpty()


    } # end flag > 0 

    if (flag==0) {  # if Coefficients are NOT reported for ARIMA model for youngest age 
    
           extract.arima.fit.aic <- extract.arima.fit$aic
           extract.arima.fit.bic <-  extract.arima.fit$bic
           extract.arima.fit.aicc <-  extract.arima.fit$aicc

           extract.arima.fit.sigma2 <- extract.arima.fit$sigma2
           extract.arima.fit.loglik <- extract.arima.fit$loglik

           return(list(extract.arima.fit.model=modelarima,
               extract.arima.fit.aic=extract.arima.fit.aic,
               extract.arima.fit.bic=extract.arima.fit.bic,
               extract.arima.fit.aicc=extract.arima.fit.aicc,
               extract.arima.fit.sigma2=extract.arima.fit.sigma2,
               extract.arima.fit.loglik=extract.arima.fit.loglik))
               
       
    } # end flag = 0 

}

SIMPLESIBREG$extract.arima.model.fit.youngest <- SIMPLESIBREG$extract.arima(SIMPLESIBREG$arima.model.fit.youngest$model)


doc = addPageBreak(doc)


doc = addParagraph(doc, value=paste("ARIMA modeling results for the youngest age."), stylename="rTableLegend")

options(stringsAsFactors=FALSE)

if (!is.null(SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.table)) {
   SIMPLESIBREG$tt <- SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.table 
   SIMPLESIBREG$tt.1 <-  apply(SIMPLESIBREG$tt,2,as.character)
   
   SIMPLESIBREG$tt.2 <- rbind.data.frame(colnames(SIMPLESIBREG$tt), SIMPLESIBREG$tt.1)

   names(SIMPLESIBREG$tt.2) <- names(SIMPLESIBREG$tt)
   SIMPLESIBREG$tt.2
   
   SIMPLESIBREG$tt <- SIMPLESIBREG$tt.2 
   
} else {

   SIMPLESIBREG$tt <- do.call(rbind, lapply(SIMPLESIBREG$extract.arima.model.fit.youngest, data.frame, stringsAsFactors=FALSE))
   
   SIMPLESIBREG$tt <- SIMPLESIBREG$tt[c("extract.arima.fit.model", 
              "extract.arima.fit.sigma2", 
              "extract.arima.fit.loglik", 
              "extract.arima.fit.aic",
              "extract.arima.fit.aicc", 
              "extract.arima.fit.bic"), ] 

   
   SIMPLESIBREG$tt[2] <- round(sqrt(as.numeric(SIMPLESIBREG$tt[2])),4)  # compute square root of "extract.arima.fit.sigma2"
   
   SIMPLESIBREG$tt <- cbind.data.frame(c("Model", 
                            "Estimated sigma", 
                            "Log-likelihood", 
                            "AIC", 
                            "AICc", 
                            "BIC"),SIMPLESIBREG$tt)


    SIMPLESIBREG$tt <- data.frame(lapply(SIMPLESIBREG$tt, as.character), stringsAsFactors=FALSE)
    

   colnames(SIMPLESIBREG$tt) <- NULL 
   
   for (k in 2:nrow(SIMPLESIBREG$tt)) {
   
        SIMPLESIBREG$tt[k,2] <- round(as.numeric(SIMPLESIBREG$tt[k,2]),4)
   }
   
}


options(stringsAsFactors=TRUE)

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )
# Create a FlexTable with tt data frame
SIMPLESIBREG$my_ft = FlexTable(data = SIMPLESIBREG$tt,
                  body.cell.props = baseCellProp,
                  header.cell.props = baseCellProp,
                  header.par.props = parProperties(text.align = "center"),
                  add.rownames = FALSE,
                  header.columns= FALSE)

if (!is.null(SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.table)) { 

    
    SIMPLESIBREG$my_ft = addHeaderRow(SIMPLESIBREG$my_ft, value=paste0(SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.model),
                     colspan=ncol(SIMPLESIBREG$tt),
                     text.properties=textBold())

    SIMPLESIBREG$my_ft = addFooterRow(SIMPLESIBREG$my_ft, 
                     value=paste0("sigma estimated as ",round(sqrt(SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.sigma2),4)),
                     colspan=ncol(SIMPLESIBREG$tt),
                     text.properties=textItalic())


    SIMPLESIBREG$my_ft = addFooterRow(SIMPLESIBREG$my_ft, value=paste0("log-likelihood = ",
                     round(SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.loglik,4)),
                     colspan=ncol(SIMPLESIBREG$tt),
                     text.properties=textItalic())

    SIMPLESIBREG$my_ft = addFooterRow(SIMPLESIBREG$my_ft, 
                     value=paste0("AIC = ",round(SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.aic,4)),
                     colspan=ncol(SIMPLESIBREG$tt),
                     text.properties=textItalic())

    SIMPLESIBREG$my_ft = addFooterRow(SIMPLESIBREG$my_ft, 
                     value=paste0("AICc = ",round(SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.aicc,4)),
                     colspan=ncol(SIMPLESIBREG$tt),
                     text.properties=textItalic())

    SIMPLESIBREG$my_ft = addFooterRow(SIMPLESIBREG$my_ft, 
                     value=paste0("BIC = ",round(SIMPLESIBREG$extract.arima.model.fit.youngest$extract.arima.fit.bic,4)),
                     colspan=ncol(SIMPLESIBREG$tt),
                     text.properties=textItalic())
                     
    ## SIMPLESIBREG$my_ft[1, ] = textProperties(color="black", font.weight = "bold" )
                     
    ## SIMPLESIBREG$my_ft <- NULL 

}


# overwrites some text formatting properties
## my_ft[, 1] = parProperties(font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, background.color = "lightgrey")

SIMPLESIBREG$my_ft[1, ] = textProperties(color="black", font.weight = "bold" )
# my_ft[, 2] = textProperties(color="black", font.weight = "bold" )


doc = addFlexTable(doc, SIMPLESIBREG$my_ft)

doc = addParagraph(doc, value=paste(" "), stylename="Normal")

SIMPLESIBREG$my_ft <- NULL 

##
## Model Fit for Youngest Age: Exponential Smoothing
##

SIMPLESIBREG$expsmoothfit <- SIMPLESIBREG$expsmooth.model.fit.youngest
sink("expsmoothfit.txt")
print(SIMPLESIBREG$expsmoothfit)
sink()
SIMPLESIBREG$out <- readLines("expsmoothfit.txt")

usePackage("stringr")

SIMPLESIBREG$out.pattern <- str_detect(string=SIMPLESIBREG$out, pattern="ETS")
SIMPLESIBREG$modelexpsmooth <- SIMPLESIBREG$out[SIMPLESIBREG$out.pattern==TRUE]

SIMPLESIBREG$modelexpsmooth <- str_trim(SIMPLESIBREG$modelexpsmooth)
SIMPLESIBREG$model_desc <- SIMPLESIBREG$modelexpsmooth
SIMPLESIBREG$model_fit <- SIMPLESIBREG$out

SIMPLESIBREG$hadley <- NULL
for (k in 1:length(SIMPLESIBREG$model_fit)){
  SIMPLESIBREG$hadley <- c(SIMPLESIBREG$hadley,
              sum(unlist(str_locate( SIMPLESIBREG$model_fit[k], "original.data")))
              )
}
SIMPLESIBREG$hadley <- ifelse(!is.na(SIMPLESIBREG$hadley), 1, 0)
SIMPLESIBREG$hadley.index <- which(SIMPLESIBREG$hadley==1) - 2
SIMPLESIBREG$model_fit_expsmooth_youngest <- SIMPLESIBREG$model_fit[6:SIMPLESIBREG$hadley.index]

doc = addParagraph(doc, value=paste("Exponential smoothing modeling results for the youngest age."), stylename="rTableLegend")

SIMPLESIBREG$tt <- data.frame(SIMPLESIBREG$model_fit_expsmooth_youngest)

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )
# Create a FlexTable with tt data frame
SIMPLESIBREG$my_ft = FlexTable(data = SIMPLESIBREG$tt,
                  body.cell.props = baseCellProp,
                  header.cell.props = baseCellProp,
                  header.par.props = parProperties(text.align = "center"),
                  add.rownames = FALSE
)

# overwrites some text formatting properties
## my_ft[, 1] = parProperties(font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, background.color = "lightgrey")

SIMPLESIBREG$my_ft[, 1] = textProperties(color="black", font.weight = "bold" )


doc = addFlexTable(doc, SIMPLESIBREG$my_ft)

SIMPLESIBREG$my_ft <- NULL 

## Sanda


##
## Results Produced by Candidate Models for Youngest Age
##


## doc = addPageBreak(doc)

## doc = addParagraph(doc, value=paste(" "), stylename="Normal")

doc = addTitle( doc, "Results Produced by the Candidate Models for the Youngest Age", level = 2)


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The results produced by the candidate models considered for forecasting ",
                    tolower(SIMPLESIBREG$stockabundance),
                    " for the youngest age component of the ",
                    SIMPLESIBREG$stockname,
                    " stock ",
                    "(i.e., ",
                    SIMPLESIBREG$youngest.age,
                    ") ",
                    "are provided below. ",
                    "They include the ",
                    SIMPLESIBREG$forecastingyear, " ",
                    "point forecast and interval forecast for the ",
                    SIMPLESIBREG$youngest.age, " ",
                    tolower(SIMPLESIBREG$stockabundance)," ",
                    "produced by each model, as well as the ",
                    "RMSE value obtained by retrospectively forecasting the total ",
                    tolower(SIMPLESIBREG$stockabundance),".")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <-  paste0("The retrospective forecasts of total ",
                    tolower(SIMPLESIBREG$stockabundance), " utilized in the computation of the reported RMSE values",
                    " were obtained by combining the retrospective forecasts of ",
                    SIMPLESIBREG$youngest.age, " ", tolower(SIMPLESIBREG$stockabundance), " ",
                    "produced by the candidate models",
                    " with the retrospective forecasts produced by the simple sibling regression models for the older age components.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 

SIMPLESIBREG$best.model.youngest.age <- NULL
SIMPLESIBREG$best.model.youngest.age[SIMPLESIBREG$best.rmse.youngest.age$index.min.rmse.total.age==1] <- "naive model based on the average of the previous five years"
SIMPLESIBREG$best.model.youngest.age[SIMPLESIBREG$best.rmse.youngest.age$index.min.rmse.total.age==2] <- "ARIMA model"
SIMPLESIBREG$best.model.youngest.age[SIMPLESIBREG$best.rmse.youngest.age$index.min.rmse.total.age==3] <- "exponential smoothing model"


doc = addParagraph(doc, value=paste("Results produced by the candidate models considered for the youngest age component."), stylename="rTableLegend")

usePackage("scales")

## DEBUG TABLE ON JUNE 17, 2016
SIMPLESIBREG$table.results.individual.ages.all.models.youngest <- NULL
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$Model <- c("Naive","ARIMA","Exponential Smoothing")
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$Forecating_Year <- rep(SIMPLESIBREG$forecastingyear, 3)
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$Point_Forecast <- 
  c(comma(round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.ctr)), 
    comma(round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.ctr)), 
    comma(round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.ctr)))
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$Interval_Forecast <- 
   c(paste0("(",comma(round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.lwr)), "; ", 
                comma(round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.upr)),")"),
     paste0("(",comma(round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.lwr)), "; ", 
                comma(round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.upr)),")"),
     paste0("(",comma(round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.lwr)), "; ", 
                comma(round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.upr)),")")
    )                      
SIMPLESIBREG$table.results.individual.ages.all.models.youngest$RMSE <- 
c(comma(round(SIMPLESIBREG$rmse.results.youngest.age.avgfive.plus.simple.sibling.regression$rmse.total)),
  comma(round(SIMPLESIBREG$rmse.results.youngest.age.arima.plus.simple.sibling.regression$rmse.total)),
  comma(round(SIMPLESIBREG$rmse.results.youngest.age.expsmooth.plus.simple.sibling.regression$rmse.total))
  )

SIMPLESIBREG$table.results.individual.ages.all.models.youngest <- as.data.frame(SIMPLESIBREG$table.results.individual.ages.all.models.youngest)   

    

SIMPLESIBREG$tt <- SIMPLESIBREG$table.results.individual.ages.all.models.youngest


# set cell padding defaut to 2
baseCellProp = cellProperties(padding = 2)
# Create a FlexTable with tt data frame
SIMPLESIBREG$my_ft = FlexTable(data = SIMPLESIBREG$tt,
                  body.cell.props = baseCellProp,
                  body.par.props = parProperties(text.align = "center"),   
                  header.cell.props = baseCellProp,
                  header.par.props = parProperties(text.align = "center"),
                  add.rownames = FALSE
)

# overwrites some text formatting properties
## my_ft[, 1] = parProperties(font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, background.color = "lightgrey")

SIMPLESIBREG$my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

## colour the row corresponding to the best model for youngest age
SIMPLESIBREG$my_ft[SIMPLESIBREG$best.rmse.youngest.age$index.min.rmse.total.age, ] = cellProperties(background.color = "yellow")

doc = addFlexTable(doc, SIMPLESIBREG$my_ft)


SIMPLESIBREG$my_ft <- NULL 
SIMPLESIBREG$tt <- NULL 

doc = addParagraph(doc, value=" ", stylename = "Normal" )

SIMPLESIBREG$paragraph <- paste0("When comparing the candidate models for the youngest age in terms of their associated RMSE values, ",
                    "it emerges that the best model identified for this age ",
                    "is the ",
                    SIMPLESIBREG$best.model.youngest.age,". ",
                    "This particular model leads to the lowest RMSE value when used in combination with the ",
                    " best simple sibling regression models for the older age components ",
                    "in order to forecast total ",
                    tolower(SIMPLESIBREG$stockabundance),
                    ".")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )


SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$figurecaption <- paste0("Model diagnostics for the naive model (i.e., average of previous five years) considered for forecasting ",
                         "the youngest age component of the ", SIMPLESIBREG$forecastingyear, " ",
                         tolower(SIMPLESIBREG$stockabundance),
                         " for the ",
                         SIMPLESIBREG$stockname, " ",
                         SIMPLESIBREG$stockspecies,
                         " stock.")

SIMPLESIBREG$myplot <- SIMPLESIBREG$diagnostics.avgfive.model.fit.youngest.age(SIMPLESIBREG$avgfive.model.fit.youngest)


doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight + 1
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

SIMPLESIBREG$figurecaption <- paste0("Model diagnostics for the ARIMA model considered for forecasting ",
                         "the youngest age component of the ", SIMPLESIBREG$forecastingyear, " ",
                         tolower(SIMPLESIBREG$stockabundance),
                         " for the ",
                         SIMPLESIBREG$stockname, " ",
                         SIMPLESIBREG$stockspecies,
                         " stock.")

SIMPLESIBREG$myplot <- SIMPLESIBREG$diagnostics.arima.model.fit.youngest.age(SIMPLESIBREG$arima.model.fit.youngest)

doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLESIBREG$myplot,
        width = plotwidth, 
        # height=plotheight
        height = plotheight + 1
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

SIMPLESIBREG$figurecaption <- paste0("Model diagnostics for the exponential smoothing model considered for forecasting ",
                         "the youngest age component of the ", SIMPLESIBREG$forecastingyear, " ",
                         tolower(SIMPLESIBREG$stockabundance),
                         " for the ",
                         SIMPLESIBREG$stockname, " ",
                         SIMPLESIBREG$stockspecies,
                         " stock. ", 
                         "If the model has additive errors, the residuals are computed as actual - fitted values. ",
                         "If the model has multiplicative errors, the residuals are computed as actual/(fitted values) - 1.")

SIMPLESIBREG$myplot <- SIMPLESIBREG$diagnostics.expsmooth.model.fit.youngest.age(SIMPLESIBREG$expsmooth.model.fit.youngest)


doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight + 1
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

## Stats Tutorial

# doc = addPageBreak(doc)

SIMPLESIBREG$pots = pot("Stats Tutorial:", textProperties(font.weight = "bold"))
SIMPLESIBREG$pars = set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, SIMPLESIBREG$pars, stylename = "Normal")

SIMPLESIBREG$pars <- NULL 
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots = pot("After fitting the naive, ARIMA and exponential smoothing models considered for forecasting ") +
       pot(paste("the youngest age component of the", SIMPLESIBREG$forecastingyear, tolower(SIMPLESIBREG$stockabundance))) +
       pot(paste(" for the", SIMPLESIBREG$stockname, SIMPLESIBREG$stockspecies,"stock, ")) +
       pot("we need to validate the model which emerged as best on the basis of the RMSE. ") +
       pot("We can do this by using diagnostic tests based on the residuals associated with the best model.")
SIMPLESIBREG$pars = set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, SIMPLESIBREG$pars, stylename = "Normal")


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots = pot("If the best model provides a good fit to the underlying data, ") +
       pot("then its residuals should exhibit no systematic patterns and no temporal dependence.")
SIMPLESIBREG$pars = set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, SIMPLESIBREG$pars, stylename = "Normal")

SIMPLESIBREG$pots <- NULL 
SIMPLESIBREG$pars <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots = pot("Useful diagnostic plots for verifying that the best model residuals exhibit no systematic patterns and no temporal dependence include:")
SIMPLESIBREG$pars = set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, SIMPLESIBREG$pars, stylename = "Normal")

SIMPLESIBREG$pars <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$texts = c("Time series plot of the model residuals;",
          "Autocorrelation plot of the model residuals;",
          "Partial autocorrelation plot of the model residuals;",
          "Plot of p-values associated with the Ljung-Box test applied to the model residuals.")
# add texts with stylename BulletList
doc = addParagraph(doc, value = SIMPLESIBREG$texts, stylename="BulletList")

SIMPLESIBREG$texts <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots = pot("The Ljung-Box test is a diagnostic tool used to test the lack of fit of the best model. ") +
       pot("The test is applied to the model residuals and examines the first ") +
       pot("m ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("autocorrelations of the residuals. ") +
       pot("If all of these autocorrelations are very small, we conclude that the model does not exhibit significant lack of fit.")
SIMPLESIBREG$pars = set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, SIMPLESIBREG$pars, stylename = "Normal")


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots = pot("The Ljung-Box test tests the following hypotheses ") +
       pot("Ho: The model does not exhibit lack of fit ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("versus ") +
       pot("Ha: The model exhibits lack of fit. ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("Small p-values for the Ljung-Box test lead to the rejection of the alternative hypothesis, suggesting that the model exhibits significant lack of fit. ") +
       pot("Conversely, large p-values suggest that the model does not exhibit significant lack of fit.")
SIMPLESIBREG$pars = set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, SIMPLESIBREG$pars, stylename = "Normal")

SIMPLESIBREG$pars <- NULL 
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots = pot("Since the choice of ") +
       pot("m ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("is important but somewhat arbitrary, ") +
       pot("in practice we perform the Ljung-Box test for several consecutive values of ") +
       pot("m ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("to see if the p-values it produces are large for all of these values. ") +
       pot("If they are, then we conclude that the model does not exhibit lack of fit.")
SIMPLESIBREG$pars = set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, SIMPLESIBREG$pars, stylename = "Normal")

SIMPLESIBREG$pars <- NULL 
SIMPLESIBREG$pots <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pots = pot("If a naive model provides a good fit to a univariate time series, then:")
SIMPLESIBREG$pars = set_of_paragraphs(SIMPLESIBREG$pots)
doc = addParagraph(doc, SIMPLESIBREG$pars, stylename = "Normal")

SIMPLESIBREG$pots <- NULL 
SIMPLESIBREG$pars <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$texts = c("The time series plot of the model residuals should exhibit no systematic patterns;",
          "The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals;",
          "The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;",
          "The p-values associated with the Ljung-Box test should be large for all values of m considered.")
doc = addParagraph( doc, value = SIMPLESIBREG$texts, stylename="BulletList" )

SIMPLESIBREG$texts <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

#=========================================================================================================
# Forecasting Results
#=========================================================================================================

doc = addPageBreak(doc)

addTitle(doc, "Forecasting Results", level=1)


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste("This section reports the forecasting results for the",
                   SIMPLESIBREG$stockname, SIMPLESIBREG$stockspecies,
                  "stock corresponding to the forecasting year",
                   paste0(SIMPLESIBREG$forecastingyear,"."),
                  "The results were produced via sibling regression models.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste("In what follows, forecasting results are reported numerically and visually for two types of forecasts:",
                  "1) point forecasts and 2) interval forecasts.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste("A point forecast is simply a number which represents our best guess of the future value of the age-specific or total",
                   tolower(SIMPLESIBREG$stockabundance),
                  "for the stock of interest based on available historical data.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste("An interval forecast is not a single number, rather it is a range of values",
                               "in which we expect the future value of an age-specific or total",
                               SIMPLESIBREG$stockabundance,
                               "series to fall with some (prespecified) probability.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste("A couple of remarks are in order in connection with an interval forecast:")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste("• The width of the interval forecast conveys information regarding forecast uncertainty",
                  " (the wider the interval forecast, the more uncertain the forecast);")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste("• The interval forecast conveys more information than the associated point forecast.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste0("Interval forecasts were obtained separately for the youngest age component and for the older age components. ", 
                   "All reported interval forecasts are 80% interval forecasts.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste0("The computation of the interval forecast for the youngest age component of the stock depended on the nature of the time series model ", 
                   "identified as best for that component. ", 
                   "If the naive model (average of previous five years) emerged as best for the youngest age component, ",
                   "maximum entropy bootstrapping was used for computing ", 
                   "the interval forecast. ", 
                   "If either the ARIMA model or the exponential smoothing model emerged as best, loess bootstrapping was used instead to compute the ", 
                   "interval forecast.  The maximum entropy and loess bootstrapping methods are described briefly below.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste0("The maximum entropy bootstrap refers to a method for bootstrapping dependent time series, which was introduced by ",
                   "Vinod and Lopez-de-Lacalle in 2009. In this method, the original time series of annual abundance values corresponding to the ",
                   "youngest age component of the stock ", 
                   "is used as a basis for creating B bootstrap replicates using an algorithm designed ", 
                   "to satisfy the ergodic theorem (i.e., the grand mean of all replicates is close to ", 
                   "the sample mean of the original time series). ", 
                   "The algorithm can accommodate both stationary and non-stationary time series. ", 
                   "The B bootstrap replicates retain the basic shape (i.e., local peaks and troughs) of the original time series. ",  
                   "They also retain the time dependence structure of the autocorrelation function (ACF) and the partial autocorrelation function (PACF)",
                   " of the original time series.") 
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <- paste0("The loess bootstrap is a time series bootstrapping method introduced by Bergmeir, Hyndman and Benitez in 2014 ", 
                   "in their working paper on bagging exponential smoothing methods using the STL decomposition and the Box-Cox transformation. ", 
                   "In this method, the original time series of annual abundance values corresponding to the youngest age component of the stock is first transformed ", 
                   "via a Box-Cox transformation. The transformed time series is then decomposed into its trend and remainder components using the loess method ", 
                   "(i.e., a smoothing method based on local linear regression). ", 
                   "Finally, the remainder component is bootstrapped using the moving block bootstrap (MBB), the trend and seasonal components are added back, ", 
                   "and the Box-Cox transformation is inverted. In this way, a random pool of B similar bootstrapped time series is generated from the original time series.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$sometext <-  paste0("The computation of the interval forecast for the older age components of the stock relied on model-based resampling ",  
                    "(but will be extended in the future to allow for normal-theory driven interval forecasts).  Note that this type of resampling ", 
                    "can generate negative values of abundance.")
doc = addParagraph(doc, SIMPLESIBREG$sometext, stylename="Normal")

SIMPLESIBREG$sometext <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


## doc = addPageBreak(doc)


addTitle(doc, "Point Forecasts", level=2)


SIMPLESIBREG$table_point_forecasts <- function(results_best_fitting_model_for_each_age_class,
                                                           forecastingyear, stockabundance){

    point_forecast_results <-
      SIMPLESIBREG$point.forecast.best.fitting.model.for.each.age.class(results_best_fitting_model_for_each_age_class,
                                                           forecastingyear, SIMPLESIBREG$datafile_variables)

    mytable <- matrix(NA, nrow=length(point_forecast_results), ncol=4)
    mytable <- as.data.frame(mytable)
    names(mytable)[-1] <- c("Best Model","Forecasting Year","Point Forecast")
    names(mytable)[1] <- paste(stockabundance)
    mytable

    usePackage("scales")

    for (i in 1:length(point_forecast_results)){

         mytable[i,1] <- names(point_forecast_results)[i]
         mytable[i,"Best Model"] <- point_forecast_results[[i]]$Model
         mytable[i,"Forecasting Year"] <- forecastingyear
         pfcst <- point_forecast_results[[i]]$PointForecast
         pfcst <- ifelse(pfcst>0,pfcst,0)
         pfcst <- round(pfcst)
         require(scales)
         pfcst <- as.character(comma(pfcst))
         mytable[i,"Point Forecast"] <- pfcst

    }

    usePackage("stringr")

    for (i in 1:length(point_forecast_results)){
       mytable[i,1] <- str_replace(mytable[i,1], "_", " ")
    }
    mytable

}


SIMPLESIBREG$total.index <- SIMPLESIBREG$best.rmse.youngest.age$index.min.rmse.total.age # index of best model used
                                                               # to forecast abundance for
                                                               # youngest age, where
                                                               # index is 1 for naive (avg. of 5 years),
                                                               #          2 for arima
                                                               #          3 for exponential smoothing



## pred.int.individual.ages.avgfive.youngest
## pred.int.individual.ages.arima.youngest
## pred.int.individual.ages.expsmooth.youngest

## usePackage("Hmisc")

## insert definition of capitalize() function 

capitalize <- function (string) {

    capped <- grep("^[A-Z]", string, invert = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
        
    return(string)
}


SIMPLESIBREG$youngest.age.capitalized <- capitalize(SIMPLESIBREG$youngest.age)


if (SIMPLESIBREG$total.index==1) {   # naive model for youngest age

    usePackage("scales")

    SIMPLESIBREG$ttrow <- c(SIMPLESIBREG$youngest.age.capitalized,
           "Naive Model (Average of Previous 5 Years)", paste(SIMPLESIBREG$forecastingyear),
           comma(round(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$PI.ctr)))
    # c("Total","-",paste(forecastingyear),
    #                              comma(pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]]))

}

if (SIMPLESIBREG$total.index==2) {  # arima model for youngest age

    SIMPLESIBREG$arimafit <- SIMPLESIBREG$arima.model.fit.youngest
    SIMPLESIBREG$arimamodel <- SIMPLESIBREG$arimafit$model

    sink("arimafit.txt")
    print(SIMPLESIBREG$arimamodel)
    sink()

    SIMPLESIBREG$out <- readLines("arimafit.txt")
    usePackage("stringr")
    SIMPLESIBREG$out.pattern <- str_detect(string=SIMPLESIBREG$out, pattern="ARIMA")

    SIMPLESIBREG$modelarima <- SIMPLESIBREG$out[SIMPLESIBREG$out.pattern==TRUE]
    SIMPLESIBREG$modelarima <- str_trim(SIMPLESIBREG$modelarima)

    
    if (SIMPLESIBREG$boxcoxtransform==TRUE) {

          SIMPLESIBREG$out.lambda <- str_detect(string=SIMPLESIBREG$out, pattern="lambda")

          SIMPLESIBREG$lambda <- SIMPLESIBREG$out[SIMPLESIBREG$out.lambda==TRUE]
          SIMPLESIBREG$modellambda <- str_trim(SIMPLESIBREG$lambda, side="right")

          SIMPLESIBREG$modelarima <- paste0(SIMPLESIBREG$modelarima, "; ", SIMPLESIBREG$modellambda)

    }
    
    usePackage("scales")

    SIMPLESIBREG$ttrow <- c(SIMPLESIBREG$youngest.age.capitalized,
               SIMPLESIBREG$modelarima, paste(SIMPLESIBREG$forecastingyear),
               comma(round(SIMPLESIBREG$pred.int.individual.ages.arima.youngest$PI.ctr)))
}

if (SIMPLESIBREG$total.index==3) { # exponential smoothing model for youngest age

    SIMPLESIBREG$expsmoothfit <- SIMPLESIBREG$expsmooth.model.fit.youngest
    SIMPLESIBREG$expsmoothmodel <- SIMPLESIBREG$expsmoothfit$model

    sink("expsmoothfit.txt")
    print(SIMPLESIBREG$expsmoothfit)
    sink()

    SIMPLESIBREG$out <- readLines("expsmoothfit.txt")
    usePackage("stringr")
    SIMPLESIBREG$out.pattern <- str_detect(string=SIMPLESIBREG$out, pattern="ETS")

    SIMPLESIBREG$modelexpsmooth <- SIMPLESIBREG$out[SIMPLESIBREG$out.pattern==TRUE]
    SIMPLESIBREG$modelexpsmooth <- str_trim(SIMPLESIBREG$modelexpsmooth)

    

    if (SIMPLESIBREG$boxcoxtransform==TRUE) {

          usePackage("stringr")

          SIMPLESIBREG$out.lambda <- str_detect(string=SIMPLESIBREG$out, pattern="lambda")

          SIMPLESIBREG$lambda <- SIMPLESIBREG$out[SIMPLESIBREG$out.lambda==TRUE][2]
          SIMPLESIBREG$modellambda <- str_trim(SIMPLESIBREG$lambda, side="right")
          SIMPLESIBREG$modellambda <- str_trim(SIMPLESIBREG$lambda, side="left")

          SIMPLESIBREG$modelexpsmooth <- paste0(SIMPLESIBREG$modelexpsmooth, "; ", SIMPLESIBREG$modellambda)

    }

    usePackage("scales")

    SIMPLESIBREG$ttrow <- c(SIMPLESIBREG$youngest.age.capitalized,
               SIMPLESIBREG$modelexpsmooth, paste(SIMPLESIBREG$forecastingyear),
               comma(round(SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest$PI.ctr)))
}




SIMPLESIBREG$tt <- rbind.data.frame(SIMPLESIBREG$ttrow,
                       SIMPLESIBREG$table_point_forecasts(SIMPLESIBREG$results_best_fitting_model_for_each_age_class,
                            SIMPLESIBREG$forecastingyear, SIMPLESIBREG$stockabundance)
                       )


## pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]]

usePackage("scales")

SIMPLESIBREG$tt[nrow(SIMPLESIBREG$tt)+1,] <- c("Total","-",paste(SIMPLESIBREG$forecastingyear),
                              comma(SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models$p[[SIMPLESIBREG$total.index]]))


SIMPLESIBREG$tablecaption <- paste0("Point forecasts for the ",
                       SIMPLESIBREG$forecastingyear,
                          " ",
                          tolower(SIMPLESIBREG$stockabundance),
                          " for the ",
                          SIMPLESIBREG$stockname,
                          " ",
                          SIMPLESIBREG$stockspecies,
                          " stock."
                          )

doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

## doc = addTable(doc, data=tt)

baseCellProp = cellProperties( padding = 4)

SIMPLESIBREG$my_ft <- FlexTable( data = SIMPLESIBREG$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLESIBREG$my_ft[, 1:ncol(SIMPLESIBREG$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=SIMPLESIBREG$my_ft)

SIMPLESIBREG$my_ft <- NULL 
SIMPLESIBREG$tt <- NULL 
SIMPLESIBREG$tablecaption <- NULL 

## Jeffie


##
## Barplot of historical abundance values and associated point forecast: Youngest Age
##

## total.index

## pred.int.individual.ages.avgfive.youngest
## pred.int.individual.ages.arima.youngest
## pred.int.individual.ages.expsmooth.youngest


doc = addPageBreak(doc)


SIMPLESIBREG$myplot <- SIMPLESIBREG$barplot.forecasted.values.youngest.age.simplesib(
                                    SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                    SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                    SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                    SIMPLESIBREG$result.avgfive.youngest, 
                                    SIMPLESIBREG$result.arima.youngest, 
                                    SIMPLESIBREG$result.expsmooth.youngest, 
                                    SIMPLESIBREG$forecastingyear, 
                                    SIMPLESIBREG$total.index, 
                                    SIMPLESIBREG$stockabundance)

doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLESIBREG$myplot,
        width=plotwidth, height=plotheight)

SIMPLESIBREG$plotlegend <- paste0("Historical ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " values and ",
                     SIMPLESIBREG$forecastingyear,
                     " point forecast corresponding to the youngest age",
                     " component of the ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " for the ",
                     SIMPLESIBREG$stockname, " ",
                     SIMPLESIBREG$stockspecies,
                     " stock.",
                     # " The 2013 point forecast was derived via exponential smoothing."
                      " The ",
                     SIMPLESIBREG$forecastingyear,
                     " point forecast was derived via ",
                     SIMPLESIBREG$best.rmse.youngest.age$method,
                     "."
                     )

doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")
             
SIMPLESIBREG$plotlegend <- NULL 
SIMPLESIBREG$myplot <- NULL 


##
## Barplot of historical abundance values and associated point forecast: Older Ages
##



for (i in 1:length(SIMPLESIBREG$point_forecast_best_model_for_each_age_class)){

  print(i)

  doc = addPageBreak(doc)

  SIMPLESIBREG$myplot <- SIMPLESIBREG$barplot.forecasted.values.individual.ages.simplesib(
                           SIMPLESIBREG$point_forecast_best_model_for_each_age_class, 
                           SIMPLESIBREG$forecastingyear, i)

  doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLESIBREG$myplot,
        width=plotwidth, height=plotheight)

  ## rm(myplot)

  SIMPLESIBREG$agetmp <- names(SIMPLESIBREG$point_forecast_best_model_for_each_age_class)[i]
  SIMPLESIBREG$agetmp <- tolower(SIMPLESIBREG$agetmp)
  usePackage("stringr")
  SIMPLESIBREG$agetmp <- str_replace(SIMPLESIBREG$agetmp,"_"," ")

  SIMPLESIBREG$plotlegend <- paste0("Historical ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " values and ",
                     SIMPLESIBREG$forecastingyear,
                     " point forecast corresponding to the ",
                     SIMPLESIBREG$agetmp,
                     " component of the ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " for the ",
                     SIMPLESIBREG$stockname, " ",
                     SIMPLESIBREG$stockspecies,
                     " stock.",
                     " The ",
                     SIMPLESIBREG$forecastingyear,
                     " point forecast was derived from the associated best sibling regression model."
                     )

  doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")
  
  SIMPLESIBREG$plotlegend <- NULL 
  SIMPLESIBREG$myplot <- NULL 

}



##
## Barplot of historical abundance values and associated point forecast: Total Age
##


doc = addPageBreak(doc)

SIMPLESIBREG$myplot <- SIMPLESIBREG$barplot.forecasted.values.total.age.simplesib(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                                        SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                                        SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                                        SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,
                                                        SIMPLESIBREG$best.fits,
                                                        SIMPLESIBREG$result.avgfive.youngest, 
                                                        SIMPLESIBREG$result.arima.youngest, 
                                                        SIMPLESIBREG$result.expsmooth.youngest, 
                                                        SIMPLESIBREG$forecastingyear, 
                                                        SIMPLESIBREG$total.index, 
                                                        SIMPLESIBREG$stockabundance)

doc = addPlot(doc,
        fun = plot, # print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, height=plotheight)
        
## rm(myplot)

SIMPLESIBREG$myplot <- NULL

SIMPLESIBREG$plotlegend <- paste0("Historical ",
                     " values and ",
                     SIMPLESIBREG$forecastingyear,
                     " point forecast corresponding to the ",
                     " total ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " for the ",
                     SIMPLESIBREG$stockname, " ",
                     SIMPLESIBREG$stockspecies,
                     " stock.")

doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotlegend <- NULL 



###
###  Interval Forecasts
###

doc = addPageBreak(doc)

doc = addTitle( doc, "Interval Forecasts", level = 2)



SIMPLESIBREG$tt <- SIMPLESIBREG$point.and.interval.forecasts.simple.sibling.regression  # recycle point and interval forecasts from
                                                               # the executive summary

usePackage("stringr")

SIMPLESIBREG$tt[,1] <- str_replace_all(SIMPLESIBREG$tt[,1], "_", " ")

SIMPLESIBREG$tablecaption <- paste0("Point and interval forecasts for the ",
                       SIMPLESIBREG$forecastingyear,
                          " ",
                          tolower(SIMPLESIBREG$stockabundance),
                          " for the ",
                          SIMPLESIBREG$stockname,
                          " ",
                          SIMPLESIBREG$stockspecies,
                          " stock."
                          )

doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

## doc = addTable(doc, data=tt)


baseCellProp = cellProperties( padding = 4)

usePackage("scales")

SIMPLESIBREG$tt[,-1] <- comma(SIMPLESIBREG$tt[,-1])

SIMPLESIBREG$my_ft <- FlexTable( data = SIMPLESIBREG$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLESIBREG$my_ft[, 1:ncol(SIMPLESIBREG$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=SIMPLESIBREG$my_ft)


SIMPLESIBREG$my_ft <- NULL 
SIMPLESIBREG$tt <- NULL 
SIMPLESIBREG$tablecaption <- NULL 


###
### Scatterplot of youngest age with forecasting interval: Youngest Age
###


doc = addPageBreak(doc)

SIMPLESIBREG$myplot <- SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.youngest.age.simple.sibling.regression(
                                   SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                   SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                   SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                   SIMPLESIBREG$result.avgfive.youngest, 
                                   SIMPLESIBREG$result.arima.youngest, 
                                   SIMPLESIBREG$result.expsmooth.youngest, 
                                   SIMPLESIBREG$forecastingyear, 
                                   SIMPLESIBREG$total.index, 
                                   SIMPLESIBREG$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight - 2
        )

## rm(myplot)

SIMPLESIBREG$plotlegend <- paste0("Historical ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " values along with the ",
                     SIMPLESIBREG$forecastingyear,
                     " point forecast and 80% interval forecast of the ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " corresponding to the youngest age component of the ",
                     SIMPLESIBREG$stockname, " ",
                     SIMPLESIBREG$stockspecies,
                     " stock. ",
                     "The point forecast was obtained via ",
                     SIMPLESIBREG$best.rmse.youngest.age$method, ".",
                     " The interval forecast was derived on the basis of time series bootstrapping, based on ", 
                     "B = ", comma(SIMPLESIBREG$B), " bootstrap samples.")

doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")

SIMPLESIBREG$plotlegend <- NULL 
SIMPLESIBREG$myplot <- NULL 



###
### Scatterplots of individual ages with forecasting intervals: Older Ages
###


for (i in 1:length(SIMPLESIBREG$results_best_fitting_model_for_each_age_class)){

  SIMPLESIBREG$bestfits <- SIMPLESIBREG$results_best_fitting_model_for_each_age_class
  SIMPLESIBREG$pointforecasts <- SIMPLESIBREG$point_forecast_best_model_for_each_age_class
  SIMPLESIBREG$intervalforecasts <-   SIMPLESIBREG$PI.individual.ages.simple.sibling.regression.no.comma

  SIMPLESIBREG$tmpage <- names(SIMPLESIBREG$bestfits)[i]
  SIMPLESIBREG$tmpage <- tolower(SIMPLESIBREG$tmpage)

  usePackage("scales")

  SIMPLESIBREG$plotlegend <- paste0("Historical ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " values along with the ",
                     SIMPLESIBREG$forecastingyear,
                     " point forecast and 80% interval forecast of the ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " corresponding to the ", SIMPLESIBREG$tmpage,
                     " component of the ",
                     SIMPLESIBREG$stockname, " ",
                     SIMPLESIBREG$stockspecies,
                     " stock. ",
                     "The point forecast was produced from the best sibling regression model identified for this age component. ",
                     "The interval forecast was derived from this same model by using case-based bootstrapping, based on ", 
                     "B = ", comma(SIMPLESIBREG$B), " bootstrap samples.")

  SIMPLESIBREG$myplot <- SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.simple.sibling.regression(
                               SIMPLESIBREG$bestfits, 
                               SIMPLESIBREG$pointforecasts, 
                               SIMPLESIBREG$intervalforecasts, 
                               SIMPLESIBREG$forecastingyear,i)

  doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        ## height=plotheight
        height=plotheight - 2
        )

  doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")

  ## rm(myplot)

  SIMPLESIBREG$myplot <- NULL 
  SIMPLESIBREG$plotlegend <- NULL 

}



###
###   Scatterplots of total age with forecasting interval
###


SIMPLESIBREG$myplot <- SIMPLESIBREG$scatterplot.forecasted.values.and.forecast.intervals.total.age.simple.sibling.regression(
                                              SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                              SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                              SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                              SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,
                                              SIMPLESIBREG$best.fits,
                                              SIMPLESIBREG$result.avgfive.youngest, 
                                              SIMPLESIBREG$result.arima.youngest, 
                                              SIMPLESIBREG$result.expsmooth.youngest,
                                              SIMPLESIBREG$forecastingyear, 
                                              SIMPLESIBREG$total.index, 
                                              SIMPLESIBREG$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        ## height=plotheight
        height = plotheight - 2
        )

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 

SIMPLESIBREG$plotlegend <- paste0("Historical values along with the ",
                     SIMPLESIBREG$forecastingyear,
                     " point forecast and 80% interval forecast of the ",
                     "total ",
                     tolower(SIMPLESIBREG$stockabundance),
                     " corresponding to the ",
                     SIMPLESIBREG$stockname, " ",
                     SIMPLESIBREG$stockspecies,
                     " stock.")

doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")


##----- Bootstrap: Visual Display for Youngest Age

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.yboot.simple.sibling.regression.youngest.age(
                                  SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,
                                  SIMPLESIBREG$pred.int.individual.ages.arima.youngest,
                                  SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest,
                                  SIMPLESIBREG$total.index, 
                                  SIMPLESIBREG$stockabundance)
                                                               
doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        ## height=plotheight
        height=plotheight - 2
        )



if (SIMPLESIBREG$total.index==1) {
    SIMPLESIBREG$model.youngest <- "naive model (i.e., average of previous 5 years)"
}
if (SIMPLESIBREG$total.index==2) {
    SIMPLESIBREG$model.youngest <- "ARIMA model"
}
if (SIMPLESIBREG$total.index==3) {
    SIMPLESIBREG$model.youngest <- "exponential smoothing model"
}


usePackage("scales")

SIMPLESIBREG$plotlegend <- paste0("Histogram of ", 
                      "B = ", comma(SIMPLESIBREG$B), 
                      " bootstrapped point forecasts for the ",
                      SIMPLESIBREG$forecastingyear, " ",
                      tolower(SIMPLESIBREG$stockabundance), " ",
                      "corresponding to the ",
                      tolower(SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest$age),
                      " component of the ",
                      SIMPLESIBREG$stockname, " ",
                      SIMPLESIBREG$stockspecies,
                      " stock.",
                      " The dashed red line indicates the position on the horizontal axis of the point forecast derived from the ",
                       SIMPLESIBREG$model.youngest, ".",
                      " The blue segment indicates the 80% forecast interval."
                      )

doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")

doc = addPageBreak(doc)

SIMPLESIBREG$plotlegend <- NULL 


##----- Bootstrap Cases: Visual Displays for Older Ages

addTitle(doc, "Bootstrapping for Older Ages", level=2)

## B <- 1000
SIMPLESIBREG$best.fits <- SIMPLESIBREG$results_best_fitting_model_for_each_age_class
## pred.int.individual.ages.simple.sibling.regression <- best.sibling.regression.model.forecast(datafile_variables,
##                                                       results_best_fitting_model_for_each_age_class,
##                                                       forecastingyear,
##                                                       B)

SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression   ## this should already be computed in Review Code script
## doc = addPlot(doc,
##        fun=print,
##        x=plot.deltaboot.simple.sibling.regression.best.fitting.models(best.fits, pred.int.individual.ages.simple.sibling.regression),
##        width=plotwidth, height=plotheight,
##        vector.graphic = F)

## doc = addParagraph(doc, value=paste("Histograms of Bootstrap Forecast Errors"), stylename="rPlotLegend")


SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.yboot.simple.sibling.regression.best.fitting.models(
                             SIMPLESIBREG$best.fits, 
                             SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression)

doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, height=plotheight)

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 

usePackage("scales")

SIMPLESIBREG$plotlegend <- paste0("Histograms of ", 
                      "B = ", comma(SIMPLESIBREG$B), 
                      " bootstrapped point forecasts for the ",
                      SIMPLESIBREG$forecastingyear, " ",
                      tolower(SIMPLESIBREG$stockabundance), " ",
                      "corresponding to the ",
                      "older age",
                      " components of the ",
                      SIMPLESIBREG$stockname, " ",
                      SIMPLESIBREG$stockspecies,
                      " stock.",
                      " For each age component, the dashed red line indicates the position on the horizontal axis of the point forecast derived ",
                      " from the best sibling regression model identified for that component. ",
                      " The blue segment indicates the 80% forecast interval.")


doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$plotlegend <- NULL 

###
### Histogram of Bootstrap Predictions: Best Model for Total Age
###

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.yboot.simple.sibling.regression.total.age(
                             SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,
                             SIMPLESIBREG$total.index, 
                             SIMPLESIBREG$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        ## height=plotheight
        height=plotheight - 2
        )

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 

usePackage("scales")

SIMPLESIBREG$plotlegend <- paste0("Histogram of ",
                      "B = ", comma(SIMPLESIBREG$B),  
                      " bootstrapped point forecasts for the ",
                      SIMPLESIBREG$forecastingyear, " total ",
                      tolower(SIMPLESIBREG$stockabundance), " ",
                      "corresponding to the ",
                      SIMPLESIBREG$stockname, " ",
                      SIMPLESIBREG$stockspecies,
                      " stock.",
                      " The dashed red line indicates the position on the horizontal axis of the point forecast of ",
                      "total ",
                      tolower(SIMPLESIBREG$stockabundance), ".",
                      " The blue segment indicates the 80% forecast interval.")


doc = addParagraph(doc, value=SIMPLESIBREG$plotlegend, stylename="rPlotLegend")

SIMPLESIBREG$plotlegend <- NULL 

#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)


###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

SIMPLESIBREG$empirical.probability.yboot.simple.sibling.regression.total.age <- function(pred.int.total.age.simple.sibling.regression.all.models,
                                                               total.index, stockabundance){


    if (total.index==1) {   # naive model for youngest age (average of previous 5 years)

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]] ## point forecast of total abundance

    }

    if (total.index==2) {   # arima model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]] ## point forecast of total abundance

    }

    if (total.index==3) {   # exponential smoothing model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simple.sibling.regression.all.models$sim[[total.index]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simple.sibling.regression.all.models$p[[total.index]] ## point forecast of total abundance

    }

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
    
    prob.interval.percentage <- c(NA, diff(prob.less.percentage))
    
    prob.interval.percentage <- round(prob.interval.percentage,2)

    prob.threshold <- x.hist.breaks

    prob.thresholds <- data.frame(prob.threshold = prob.threshold,
                       prob.less.percentage = prob.less.percentage,
                       prob.greater.percentage = prob.greater.percentage, 
                       prob.interval.percentage = prob.interval.percentage)

    prob.thresholds

    ## cumulative probabilities evaluated for the point forecast of total abundance

    prob.less.pfct <- round(my.ecdf(pfct),4)
    prob.greater.pfct <- round(1 - my.ecdf(pfct),4)

    prob.less.percentage.pfct <- round(prob.less.pfct*100,2)

    prob.greater.percentage.pfct <- round(prob.greater.pfct*100,2)

    prob.threshold.pfct <- pfct
    
    prob.interval.percentage.pfct <- NA

    prob.pfct <-  data.frame(prob.threshold = prob.threshold.pfct,
                             prob.less.percentage = prob.less.percentage.pfct,
                             prob.greater.percentage = prob.greater.percentage.pfct, 
                             prob.interval.percentage = prob.interval.percentage.pfct)


    prob.pfct

    probs = list(prob.thresholds=prob.thresholds,
                 prob.point.forecast=prob.pfct)

    probs

}


SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age <- SIMPLESIBREG$empirical.probability.yboot.simple.sibling.regression.total.age(
                                                               SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,
                                                               SIMPLESIBREG$total.index, SIMPLESIBREG$stockabundance)




SIMPLESIBREG$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(SIMPLESIBREG$stockabundance)," ",
                       "value yet to be observed in ",
                       SIMPLESIBREG$forecastingyear, " for the ",
                       SIMPLESIBREG$stockname," ",
                       SIMPLESIBREG$stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(SIMPLESIBREG$stockabundance), ".")

doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

SIMPLESIBREG$tt_1 <- SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.thresholds

SIMPLESIBREG$tt_2 <- SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.point.forecast

SIMPLESIBREG$tt_1_and_2 <- rbind.data.frame(SIMPLESIBREG$tt_1, SIMPLESIBREG$tt_2)

usePackage("plyr")

## SIMPLESIBREG$tt_arrange <- arrange(SIMPLESIBREG$tt_1_and_2, SIMPLESIBREG$prob.threshold)

## SIMPLESIBREG$tt_arrange <- arrange(SIMPLESIBREG$tt_1_and_2, SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.point.forecast$prob.threshold)

SIMPLESIBREG$tt_arrange <- SIMPLESIBREG$tt_1_and_2[order(SIMPLESIBREG$tt_1_and_2$prob.threshold),]

### tt_arrange$prob.interval.percentage.updated <- c(NA, diff(tt_arrange$prob.less.percentage))

### tt_arrange$prob.interval.percentage <- NULL



## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2)

SIMPLESIBREG$from_tmp = which(SIMPLESIBREG$tt_arrange[,1] == SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.point.forecast$prob.threshold)

SIMPLESIBREG$tt_arrange[SIMPLESIBREG$from_tmp, 4] <- SIMPLESIBREG$tt_arrange[SIMPLESIBREG$from_tmp + 1, 4]

SIMPLESIBREG$tt_arrange[,1] <- comma(SIMPLESIBREG$tt_arrange[,1])
SIMPLESIBREG$tt_arrange[,2] <- paste0(sprintf("%.1f", SIMPLESIBREG$tt_arrange[,2]),"%")
SIMPLESIBREG$tt_arrange[,3] <- paste0(sprintf("%.1f", SIMPLESIBREG$tt_arrange[,3]),"%")
SIMPLESIBREG$tt_arrange[,4] <- paste0(sprintf("%.1f", SIMPLESIBREG$tt_arrange[,4]),"%")

SIMPLESIBREG$tt_arrange

names(SIMPLESIBREG$tt_arrange)[1] <- "Threshold"

names(SIMPLESIBREG$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(SIMPLESIBREG$tt_arrange)[3] <- "Prob(Actual > Threshold)"

## names(tt_arrange)[4] <- "Prob(Previous Threshold < Actual <= Current Threshold)"

names(SIMPLESIBREG$tt_arrange)[4] <- "Interval Probability"
SIMPLESIBREG$tt_arrange[1,4] <- "-"

SIMPLESIBREG$my_ft <- FlexTable( data = SIMPLESIBREG$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right"),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=2))

# overwrites some paragraph formatting properties
SIMPLESIBREG$my_ft[, 1:ncol(SIMPLESIBREG$tt_arrange)] = parProperties(text.align = "right")



SIMPLESIBREG$my_ft = spanFlexTableRows(SIMPLESIBREG$my_ft, j=4, from = SIMPLESIBREG$from_tmp, to = SIMPLESIBREG$from_tmp + 1)

SIMPLESIBREG$my_ft[SIMPLESIBREG$tt_arrange$Threshold %in% comma(SIMPLESIBREG$emp.prob.simple.sibling.regression.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

doc = addFlexTable(doc, flextable=SIMPLESIBREG$my_ft)


SIMPLESIBREG$my_ft_emp_prob_total <- SIMPLESIBREG$my_ft
SIMPLESIBREG$tablecaption_emp_prob_total <- SIMPLESIBREG$tablecaption

doc = addPageBreak(doc)

SIMPLESIBREG$my_ft <- NULL 
SIMPLESIBREG$tt_arrange <- NULL 

#================================================================================================
# 
# Retrospective Evaluation of Performance of Point Forecasts
#================================================================================================

doc = addTitle(doc, paste("Retrospective Evaluation of Performance of Point Forecasts"), level=1)


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("This section reports the results associated with the retrospective evaluation of the performance of the point forecasts of ",
                        ## stockspecies," ", stockname, " stock ",
                        ## "corresponding to the forecasting year ",forecastingyear,". ",
                        "age-specific and total ",
                        paste0(tolower(SIMPLESIBREG$stockabundance),"s"),
                        " corresponding to the ", SIMPLESIBREG$stockspecies, " ", SIMPLESIBREG$stockname, " stock.",
                        " For the youngest age group, the point forecasts were produced retrospectively by using ",
                        SIMPLESIBREG$best.rmse.youngest.age$method, ".",
                        " For each of the older age groups, the point forecasts were produced retrospectively by using ",
                        "the best sibling regression model for that group among all sibling regression models considered.",
                      sep="")

doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$pot1 <-  pot("Retrospective forecast errors were defined as the actual ") +
         pot(paste(tolower(SIMPLESIBREG$stockabundance))) +
         pot(" values ") +
         pot("minus the retrospectively forecasted ") +
         pot(paste(tolower(SIMPLESIBREG$stockabundance))) +
         pot(" values.") +
         pot(" In view of this definition, ") +
         pot(" positive", textProperties( font.style = "italic", font.size=11, font.family="Calibri")) +
         pot(" values for the retrospective forecast errors") +
         pot(" represent forecasts that were ") +
         pot("low", textProperties( font.style = "italic", font.size=11, font.family="Calibri")) +
         pot(" relative to the historical ") +
         pot(paste(tolower(SIMPLESIBREG$stockabundance))) +
         pot(" values, ") +
         pot(" whereas") +
         pot(" negative ", textProperties(font.style = "italic", font.size=11, font.family="Calibri")) +
         pot("values represent forecasts that were ") +
         pot("high ", textProperties( font.style = "italic", font.size=11, font.family="Calibri")) +
         pot("relative to the historical ") +
         pot(paste(tolower(SIMPLESIBREG$stockabundance))) +
         pot(" values.")

SIMPLESIBREG$paragraph <- set_of_paragraphs(SIMPLESIBREG$pot1)

doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

paragraph <- paste0("The following retrospective measures were used to characterize different aspects of the distribution",
                    " of the retrospective forecasting errors:")
doc = addParagraph(doc, paragraph, stylename = "Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$texts = c( "Mean Raw Error (MRE);",
           "Mean Absolute Error (MAE);",
           "Mean Percent Error (MPE);",
           "Mean Absolute Percent Error (MAPE);",
           "Mean Scaled Error (MASE);",
           "Root Mean Square Error.")
# add texts with stylename BulletList
doc = addParagraph( doc, value = SIMPLESIBREG$texts, stylename="BulletList" )


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("MAE and MAPE reflect overall forecast accuracy accounting for ",
                    "systematic bias and year-to-year variation.")
doc = addParagraph( doc, value = SIMPLESIBREG$paragraph, stylename="Normal" )

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("MRE and MPE reflect directional bias in raw and relative forecast errors, respectively, ",
                    "with negative values indicating a tendency to underforecast and positive values reflecting a tendency to overforecast.")
doc = addParagraph( doc, value = SIMPLESIBREG$paragraph, stylename="Normal" )


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("Just like MAE, RMSE is a measure of the absolute magnitude of the raw retrospective forecast errors, ",
                    "but is more sensitive to large values then MAE.")
doc = addParagraph( doc, value = SIMPLESIBREG$paragraph, stylename="Normal" )


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste("MASE was proposed by Hyndman and Koehler (2006) as a generally applicable, scale-free measure of forecast accuracy.",
                       "This measure never gives infinite or undefined values.",
                       "In this report, MASE is computed as the average of the absolute values of the scaled retrospective forecast errors produced by",
                       "the sibling regression methodology.",
                       "The scaling of the errors involves dividing the errors by the MAE computed from the retrospective forecast errors associated with",
                       "the naive model based on the",
                       # terminal run
                       tolower(SIMPLESIBREG$stockabundance),
                       "for the previous year.",
                       # "from the one-step, naive forecasting method.",
                       # "A scaled error is less than 1 if it arises from a better forecast than the one produced by the naive model based on the terminal run for the previous year.",
                       # "Conversely, it is greater than 1 if the forecast is worse than the average one-step, naive forecast computed in-sample.",
                       "A value of MASE less than 1 suggests that the retrospective forecasting accuracy of the sibling regression model",
                       # terminal run
                       tolower(SIMPLESIBREG$stockabundance),
                       "is better than the retrospective forecasting accuracy of",
                       "the benchmark naive model based on the",
                       # terminal run
                       tolower(SIMPLESIBREG$stockabundance),
                       "for the previous year.",
                       "A value of MASE greater than 1 suggests that the retrospective forecasting accuracy of the sibling regression model",
                       "is worse than the retrospective forecasting accuracy of",
                       "the benchmark naive model based on the",
                       # terminal run
                       tolower(SIMPLESIBREG$stockabundance),
                       "for the previous year.",
                      sep=" ")
doc = addParagraph( doc, value = SIMPLESIBREG$paragraph, stylename="Normal" )


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLESIBREG$paragraph <- paste("To facilitate the interpretation of the retrospective forecast errors, this section reports several types of plots:")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 

doc = addParagraph( doc, value = paste0('Plots illustrating the performance of the retrospective forecasting evaluation;'),
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Density plots of the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Bias coefficient plots obtained from the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Barplots of the retrospective forecast errors annotated with the interval forecast corresponding to the forecasting year of interest.',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
## doc = addParagraph( doc, value = paste0('Time series plots displaying the retrospective point forecasts against the actual historical values;'),
##       par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
## doc = addParagraph( doc, value = paste0('Scatter plots displaying the retrospective point forecasts against the actual historical values.'),
##       par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")

SIMPLESIBREG$paragraph <- paste0("The plots illustrating the performance of the retrospective forecast evaluations are reported only for the individual age components of ", 
                   tolower(SIMPLESIBREG$stockabundance), ". ", 
                   "All other plots are reported both for the individual age components of ", 
                   tolower(SIMPLESIBREG$stockabundance), 
                   " and for the total ", 
                    tolower(SIMPLESIBREG$stockabundance), ".")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste("Bias coefficients representing a new metric for forecast bias are also reported in numerical and visual form in this section. ",
                    "These coefficients are computed from the retrospective forecast errors for the age-specific and total ",
                    paste0(tolower(SIMPLESIBREG$stockabundance),"s"),
                    " using the formula developed by Kourentzes, Trapero and Svetunkov",
                    " in their 2014 working paper \"Measuring the behaviour of experts on demand forecasting: a simple task\".",
                    " In the context of this report, the bias coefficients describe the direction and magnitude of the retrospective forecast bias associated with the",
                    "exponential smoothing forecasting method.",
                    sep=" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste("Generally speaking, the bias coefficients are unit-free and bounded between -1 (maximum negative retrospective forecast bias)",
                    "and 1 (maximum positive retrospective forecast bias).",
                    "A forecasting method that is always producing retrospective point forecasts which are over the observed historical values will have",
                    "a bias coefficient equal to -1, always over-forecasting.",
                    "A forecasting method that is always producing retrospective point forecasts which are under the observed historical values",
                    "will have a bias coefficient equal to 1, always under-forecasting.",
                    "Given the bounded nature of the bias coefficient, we can describe a forecasting method as strongly biased if |bias coefficient| > 0.5",
                    "and weakly biased if 0 < |bias coefficient| <= 0.5, providing a simple and intuitive description of the forecast bias behaviour.",
                    "If the bias coefficient is equal to 0, the forecasting method is unbiased.",
                    sep=" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
doc = addTitle(doc, paste("Measures of Retrospective Point Forecast Performance"), level=2)



SIMPLESIBREG$tablecaption <- paste0("Retrospective measures of forecast performance associated with the point forecasts of the ",
                      SIMPLESIBREG$forecastingyear, " ",
                      "age-specific and total ",
                      paste0(tolower(SIMPLESIBREG$stockabundance),"s"),
                      " for the ",
                      SIMPLESIBREG$stockname, " ",
                      SIMPLESIBREG$stockspecies,
                      " stock.")

doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")
## tt <- retro.measures.all.ages.simple.sibling(best.rmse.youngest.age)
##

baseCellProp = cellProperties( padding = 4)

SIMPLESIBREG$tt <- SIMPLESIBREG$retro.measures.all.ages.simple.sibling(SIMPLESIBREG$best.rmse.youngest.age)

usePackage("stringr")
names(SIMPLESIBREG$tt) <- str_replace_all(names(SIMPLESIBREG$tt),"_"," ")



usePackage("scales")
SIMPLESIBREG$tt[,-1] <- comma(SIMPLESIBREG$tt[,-1])

SIMPLESIBREG$my_ft <- FlexTable( data = SIMPLESIBREG$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLESIBREG$my_ft[, 1:ncol(SIMPLESIBREG$tt)] = parProperties(text.align = "right")



doc = addFlexTable(doc, flextable=SIMPLESIBREG$my_ft)


SIMPLESIBREG$my_ft <- NULL 

#================================================================================================
# Retrospective Point Forecasts and Forecast Errors - May need to move this section somewhere else in the report!!!
#================================================================================================


doc = addPageBreak(doc)

doc = addTitle(doc, paste("Retrospective Point Forecasts and Forecast Errors"), level=2)


##
## Youngest Age
##

SIMPLESIBREG$tabledata <- SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin[[1]]

if (sum(names(SIMPLESIBREG$tabledata) %in% c("cy","a","p","e")) > 1) {

    SIMPLESIBREG$tabledata <- subset(SIMPLESIBREG$tabledata, select=c("cy","a","p","e"))

} 

if (sum(names(SIMPLESIBREG$tabledata) %in% c("cy","Actual","Forecast","Error")) > 1 ) {

    SIMPLESIBREG$tabledata <- subset(SIMPLESIBREG$tabledata, select=c("cy","Actual","Forecast","Error"))

} 

SIMPLESIBREG$tabledata <- as.data.frame(SIMPLESIBREG$tabledata)
names(SIMPLESIBREG$tabledata) <- c("Return Year","Actual","Forecast","Error")
print(SIMPLESIBREG$tabledata)

usePackage("scales")
SIMPLESIBREG$tabledata[,"Actual"] <- comma(SIMPLESIBREG$tabledata[,"Actual"])
SIMPLESIBREG$tabledata[,"Forecast"] <- comma(SIMPLESIBREG$tabledata[,"Forecast"])
SIMPLESIBREG$tabledata[,"Error"] <- comma(SIMPLESIBREG$tabledata[,"Error"])


SIMPLESIBREG$youngest.age <- names(SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin)[1]
usePackage("stringr")
SIMPLESIBREG$youngest.age <- str_replace_all(SIMPLESIBREG$youngest.age,"_", " ")
SIMPLESIBREG$youngest.age <- tolower(SIMPLESIBREG$youngest.age)

SIMPLESIBREG$tablecaption <- paste0("Retrospective point forecasts and associated forecast errors for the ",
                      SIMPLESIBREG$forecastingyear, " ",
                      tolower(SIMPLESIBREG$stockabundance),
                      " corresponding to the youngest age component",
                      " (i.e., ", SIMPLESIBREG$youngest.age, ")",
                      " of the ",
                      SIMPLESIBREG$stockname, " ",
                      SIMPLESIBREG$stockspecies,
                      " stock",
                      ".",
                      " Accompanying return years and actual ",
                      tolower(SIMPLESIBREG$stockabundance),
                      " values are also reported.",
                      " The retrospective point forecasts were obtained by using ",
                      SIMPLESIBREG$best.rmse.youngest.age$method,
                      ", ",
                      "since sibling regression could not be used for the youngest component of the stock. ",
                      "A positive forecast error indicates an under-forecast and a negative forecast error ",
                      "indicates an over-forecast.")


## tablecaption <- set_of_paragraphs(tablecaption)

baseCellProp = cellProperties( padding = 4)

doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

SIMPLESIBREG$ft_tabledata <- FlexTable(data=SIMPLESIBREG$tabledata, 
                          header.columns=TRUE, 
                          body.cell.props = baseCellProp,
                          header.par.props = parProperties(text.align = "right" ),
                          header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))

# overwrites some paragraph formatting properties
SIMPLESIBREG$ft_tabledata[, 1:ncol(SIMPLESIBREG$tabledata)] = parProperties(text.align = "right")

# set columns widths (inch)
SIMPLESIBREG$ft_tabledata = setFlexTableWidths(SIMPLESIBREG$ft_tabledata,  
	                                widths = c(1.2,1.2,1.2,1.2) )


doc = addFlexTable(doc, flextable = SIMPLESIBREG$ft_tabledata)

## rm(tabledata)
## rm(ft_tabledata)

SIMPLESIBREG$tabledata <- NULL 
SIMPLESIBREG$ft_tabledata <- NULL 
SIMPLESIBREG$tablecaption <- NULL 


## doc = addFlexTable(doc, flextable=ft_tabledata,
##                 layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
##                                                    data.cell=cellProperties(border.color="black")))

#### doc = addFlexTable(doc, flextable = ft_tabledata,
####       par.properties = parProperties(text.align = "right", padding=2))



doc = addParagraph(doc, value=" ", stylename="Normal")


##
## Older Ages
##

for (i in 1:(length(SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin)-1)) {

    ## doc = addPageBreak(doc)

    SIMPLESIBREG$tabledata <- SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin[[i+1]]

    ## tabledata <- subset(tabledata, select=c("cy","Actual","Forecast","Error"))
    ## tabledata <- subset(tabledata, select=c("cy","a","p","e"))
    
    if (sum(names(SIMPLESIBREG$tabledata) %in% c("cy","a","p","e")) > 1) {

    SIMPLESIBREG$tabledata <- subset(SIMPLESIBREG$tabledata, select=c("cy","a","p","e"))

    }

    if (sum(names(SIMPLESIBREG$tabledata) %in% c("cy","Actual","Forecast","Error")) > 1 ) {

    SIMPLESIBREG$tabledata <- subset(SIMPLESIBREG$tabledata, select=c("cy","Actual","Forecast","Error"))

    }

    SIMPLESIBREG$tabledata <- as.data.frame(SIMPLESIBREG$tabledata)
    names(SIMPLESIBREG$tabledata) <- c("Return Year","Actual","Forecast","Error")
    print(SIMPLESIBREG$tabledata)

    usePackage("scales")
    SIMPLESIBREG$tabledata[,"Actual"] <- comma(SIMPLESIBREG$tabledata[,"Actual"])
    SIMPLESIBREG$tabledata[,"Forecast"] <- comma(SIMPLESIBREG$tabledata[,"Forecast"])
    SIMPLESIBREG$tabledata[,"Error"] <- comma(SIMPLESIBREG$tabledata[,"Error"])

    SIMPLESIBREG$older.age <- names(SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin)[i+1]
    usePackage("stringr")
    SIMPLESIBREG$older.age <- str_replace_all(SIMPLESIBREG$older.age,"_", " ")
    SIMPLESIBREG$older.age <- tolower(SIMPLESIBREG$older.age)


    SIMPLESIBREG$tablecaption <- paste0("Retrospective point forecasts and associated forecast errors for the ",
                          SIMPLESIBREG$forecastingyear, " ",
                          tolower(SIMPLESIBREG$stockabundance),
                          " corresponding to the ", SIMPLESIBREG$older.age ,
                          " component",
                          " of the ",
                          SIMPLESIBREG$stockname, " ",
                          SIMPLESIBREG$stockspecies,
                          " stock",
                          ".",
                          " Accompanying return years and actual ",
                          tolower(SIMPLESIBREG$stockabundance),
                          " values are also reported.",
                          " The retrospective point forecasts were obtained from",
                          " the best sibling regression model identified for the ",
                          SIMPLESIBREG$older.age,
                          " component among all candidate sibling regression models",
                          " considered for that component. ",
                          "A positive forecast error indicates an under-forecast and a negative forecast error ",
                          "indicates an over-forecast."
                          )

    doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")
    
    baseCellProp = cellProperties( padding = 4)
    
    SIMPLESIBREG$ft_tabledata <- FlexTable(data=SIMPLESIBREG$tabledata, 
                              header.columns=TRUE, 
                              body.cell.props = baseCellProp,
                              header.par.props = parProperties(text.align = "right" ),
                              header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))
    
    ## doc = addFlexTable(doc, flextable=ft_tabledata,
    ##             layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
    ##                                               data.cell=cellProperties(border.color="black")))
      
    
    # overwrites some paragraph formatting properties
    SIMPLESIBREG$ft_tabledata[, 1:ncol(SIMPLESIBREG$tabledata)] = parProperties(text.align = "right")
                                               
    
    # set columns widths (inch)
    SIMPLESIBREG$ft_tabledata = setFlexTableWidths(SIMPLESIBREG$ft_tabledata,
	                                widths = c(1.2,1.2,1.2,1.2) )

    
    doc = addFlexTable(doc, flextable = SIMPLESIBREG$ft_tabledata)
    
    doc = addParagraph(doc, value=" ", stylename="Normal")

    ## rm(tabledata)
    ## rm(ft_tabledata)
    
    SIMPLESIBREG$tabledata <- NULL
    SIMPLESIBREG$ft_tabledata <- NULL  
    SIMPLESIBREG$tablecaption <- NULL 
}


##
## Total Age
##



SIMPLESIBREG$tabledata <- data.frame(cy = SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin[[1]]$cy,
                        a = SIMPLESIBREG$best.rmse.youngest.age$retro$a.total,
                        p = SIMPLESIBREG$best.rmse.youngest.age$retro$p.total,
                        e = SIMPLESIBREG$best.rmse.youngest.age$retro$e.total)


SIMPLESIBREG$tabledata <- subset(SIMPLESIBREG$tabledata, select=c("cy","a","p","e"))
SIMPLESIBREG$tabledata <- as.data.frame(SIMPLESIBREG$tabledata)
names(SIMPLESIBREG$tabledata) <- c("Return Year","Actual","Forecast","Error")
print(SIMPLESIBREG$tabledata)

usePackage("scales")
SIMPLESIBREG$tabledata[,"Actual"] <- comma(SIMPLESIBREG$tabledata[,"Actual"])
SIMPLESIBREG$tabledata[,"Forecast"] <- comma(SIMPLESIBREG$tabledata[,"Forecast"])
SIMPLESIBREG$tabledata[,"Error"] <- comma(SIMPLESIBREG$tabledata[,"Error"])

SIMPLESIBREG$tablecaption <- paste0("Retrospective point forecasts and associated forecast errors for the ",
                      SIMPLESIBREG$forecastingyear, " ",
                      "total ",
                      tolower(SIMPLESIBREG$stockabundance),
                      " corresponding to the ",
                      SIMPLESIBREG$stockname, " ",
                      SIMPLESIBREG$stockspecies,
                      " stock",
                      ".",
                      " Accompanying return years and actual ",
                      tolower(SIMPLESIBREG$stockabundance),
                      " values are also reported. ",
                      "A positive forecast error indicates an under-forecast and a negative forecast error ",
                      "indicates an over-forecast.")

doc = addParagraph(doc, value=SIMPLESIBREG$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

SIMPLESIBREG$ft_tabledata <- FlexTable(data=SIMPLESIBREG$tabledata, 
                          header.columns=TRUE, 
                          body.cell.props = baseCellProp,
                          header.par.props = parProperties(text.align = "right" ),
                          header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))

## doc = addFlexTable(doc, flextable=ft_tabledata,
##                  layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
##                                                   data.cell=cellProperties(border.color="black")))


# overwrites some paragraph formatting properties
SIMPLESIBREG$ft_tabledata[, 1:ncol(SIMPLESIBREG$tabledata)] = parProperties(text.align = "right")


# set columns widths (inch)
SIMPLESIBREG$ft_tabledata = setFlexTableWidths(SIMPLESIBREG$ft_tabledata,
	                                widths = c(1.2,1.2,1.2,1.2) )


doc = addFlexTable(doc, flextable = SIMPLESIBREG$ft_tabledata)

doc = addParagraph(doc, value=" ", stylename="Normal")

## rm(tabledata)
## rm(ft_tabledata)

SIMPLESIBREG$tabledata <- NULL 
SIMPLESIBREG$ft_tabledata <- NULL 
SIMPLESIBREG$tablecaption <- NULL 

##======================================================================================
##  "Cool" plots for the youngest age 
##======================================================================================

## doc = addPageBreak(doc)

addTitle(doc, "Plots Illustrating the Performance of the Retrospective Forecasting Evaluation", level=2)


usePackage("stringr")

if (SIMPLESIBREG$best.rmse.youngest.age$method == "naive forecasting (i.e., average of previous five years)") {

    SIMPLESIBREG$myplot <- SIMPLESIBREG$youngest.age.retro.plot.avgfive(SIMPLESIBREG$youngest.age.retro.plot.info.avgfive, SIMPLESIBREG$stockabundance)
    
    SIMPLESIBREG$age_youngest <- SIMPLESIBREG$youngest.age.retro.plot.info.avgfive$age0
    
    SIMPLESIBREG$model_age_youngest <- str_replace(SIMPLESIBREG$best.rmse.youngest.age$method, "forecasting", "modeling")
    
}

if (SIMPLESIBREG$best.rmse.youngest.age$method == "ARIMA forecasting") {

    SIMPLESIBREG$myplot <- SIMPLESIBREG$youngest.age.retro.plot.arima(SIMPLESIBREG$youngest.age.retro.plot.info.arima, SIMPLESIBREG$stockabundance)

    SIMPLESIBREG$age_youngest <- SIMPLESIBREG$youngest.age.retro.plot.info.arima$age0
    
    SIMPLESIBREG$model_age_youngest <- str_replace(SIMPLESIBREG$best.rmse.youngest.age$method, "forecasting", "modeling")
    
}

if (SIMPLESIBREG$best.rmse.youngest.age$method == "exponential smoothing forecasting") {

    SIMPLESIBREG$myplot <- SIMPLESIBREG$youngest.age.retro.plot.expsmooth(SIMPLESIBREG$youngest.age.retro.plot.info.expsmooth, SIMPLESIBREG$stockabundance)
    
    SIMPLESIBREG$age_youngest <- SIMPLESIBREG$youngest.age.retro.plot.info.expsmooth$age0
    
    SIMPLESIBREG$model_age_youngest <- str_replace(SIMPLESIBREG$best.rmse.youngest.age$method, "forecasting", "modeling")

}


SIMPLESIBREG$age_youngest <- tolower(SIMPLESIBREG$age_youngest)
SIMPLESIBREG$age_youngest <- str_replace(SIMPLESIBREG$age_youngest, "_", " ")

SIMPLESIBREG$figurecaption <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of the youngest age component ",
                        " (i.e., ", SIMPLESIBREG$age_youngest, ")",  
                        " of the ",
                        " ",
                        tolower(SIMPLESIBREG$stockabundance),
                        " for the ",
                        SIMPLESIBREG$stockname,
                        " ",
                        SIMPLESIBREG$stockspecies,
                        ".",
                        " Retrospective forecast errors were obtained via ",
                        SIMPLESIBREG$best.rmse.youngest.age$method,
                        ". ", 
                        "Historical values of ", SIMPLESIBREG$age_youngest, " ", tolower(SIMPLESIBREG$stockabundance), 
                        " (grey lines) ", 
                        "and fitted values produced by the", SIMPLESIBREG$model_age_youngest, 
                        " are also shown. ", 
                        "Each panel corresponds to a particular retrospective forecasting year.")

## Figure 22 : 	Actual values (grey dots) and retrospectively forecasted values (red dots) of the age 2 component of terminal run 
## for the SPR chinook salmon stock, derived via naive modeling based on the average of terminal run from the previous 3 years. 
## Historical values of age 2 terminal run (grey lines) 
## and fitted values produced by the naive modeling (red lines) are also shown. 
## Each panel corresponds to a particular retrospective forecasting year.


doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        height=plotheight)

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)
SIMPLESIBREG$myplot <- NULL 

## rm(age_youngest)
SIMPLESIBREG$age_youngest <- NULL 

## rm(model_age_youngest)
SIMPLESIBREG$model_age_youngest <- NULL 

##======================================================================================
##  "Cool" plots for the older ages 
##======================================================================================


for (j in 1:length(SIMPLESIBREG$individual.ages.retro.plot.info.simplesib)) {


    usePackage("stringr")
    
    SIMPLESIBREG$tmp_age <- names(SIMPLESIBREG$individual.ages.retro.plot.info.simplesib)[j]
    SIMPLESIBREG$tmp_age <- tolower(SIMPLESIBREG$tmp_age)
    SIMPLESIBREG$tmp_age <- str_replace_all(SIMPLESIBREG$tmp_age, "_", " ")
    SIMPLESIBREG$tmp_age

    SIMPLESIBREG$figurecaption <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                        SIMPLESIBREG$tmp_age, 
                        " component", 
                        " of ",
                        tolower(SIMPLESIBREG$stockabundance),
                        " for the ",
                        SIMPLESIBREG$stockname,
                        " ",
                        SIMPLESIBREG$stockspecies,
                        ".",
                        " Historical values of ", tolower(SIMPLESIBREG$stockabundance), 
                        " (grey lines) ", 
                        "and fitted values produced by sibling regression", 
                        " are also shown. ", 
                        "Each panel corresponds to a particular retrospective forecasting year.")

    ## Figure 22 : 	Actual values (grey dots) and retrospectively forecasted values (red dots) of the age 2 component of terminal run 
    ## for the SPR chinook salmon stock, derived via naive modeling based on the average of terminal run from the previous 3 years. 
    ## Historical values of age 2 terminal run (grey lines) 
    ## and fitted values produced by the naive modeling (red lines) are also shown. 
    ## Each panel corresponds to a particular retrospective forecasting year.

    SIMPLESIBREG$myplot <- SIMPLESIBREG$individual.ages.retro.plot.simplesib(SIMPLESIBREG$individual.ages.retro.plot.info.simplesib, SIMPLESIBREG$stockabundance, j)

    doc = addPlot(doc,
                  fun=print,
                  x=SIMPLESIBREG$myplot,
                  width=plotwidth, 
                  height=plotheight)

    doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

    ## rm(myplot)
    SIMPLESIBREG$myplot <- NULL 
    SIMPLESIBREG$figurecaption <- NULL 
    ## rm(tmp_age)
    SIMPLESIBREG$tmp_age <- NULL 

}




doc = addPageBreak(doc)


addTitle(doc, "Density Plots of the Retrospective Forecast Errors", level=2)



##
## Density plots of retrospective forecast errors: Individual Ages
##


## doc = addPageBreak(doc)

SIMPLESIBREG$figurecaption <- paste0("Density plots of the retrospective forecast errors corresponding to the ",
                        "individual age components of the ",
                        SIMPLESIBREG$forecastingyear,
                        " ",
                        tolower(SIMPLESIBREG$stockabundance),
                        " for the ",
                        SIMPLESIBREG$stockname,
                        " ",
                        SIMPLESIBREG$stockspecies,
                        ".",
                        "For the youngest age, retrospective forecast errors were obtained via ",
                        SIMPLESIBREG$best.rmse.youngest.age$method,
                        ".",
                        "For the other ages, retrospective forecast errors were obtained on the basis of the ",
                        " corresponding simple sibling regression models."
                        )

## myplot <- plot.hist.retrospective.forecast.errors.individual.ages.simple.sibling.regression(best.rmse.youngest.age, stockabundance)

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.dens.retrospective.forecast.errors.individual.ages.simple.sibling.regression(
                             SIMPLESIBREG$best.rmse.youngest.age, SIMPLESIBREG$stockabundance)


doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        height=plotheight)

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 


##
## Density plot of retrospective forecast errors: Total Age
##


doc = addPageBreak(doc)

SIMPLESIBREG$figurecaption <- paste0("Density plots of retrospective forecast errors corresponding to the ",
                        SIMPLESIBREG$forecastingyear,
                        " total ",
                        tolower(SIMPLESIBREG$stockabundance),
                        " for the ",
                        SIMPLESIBREG$stockname,
                        " ",
                        SIMPLESIBREG$stockspecies,
                        "."
                        )

## myplot <- plot.hist.retrospective.forecast.errors.total.age.simple.sibling.regression(best.rmse.youngest.age, stockabundance)

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.dens.retrospective.forecast.errors.total.age.simple.sibling.regression(SIMPLESIBREG$best.rmse.youngest.age, SIMPLESIBREG$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight - 2
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 


##
## Bias Coefficient Plot for the Youngest Age 
## 

doc = addPageBreak(doc)


addTitle(doc, "Bias Coefficient Plots Derived from the Retrospective Forecast Errors", level=2)


SIMPLESIBREG$figurecaption <- paste0("Bias coefficient plot obtained from the retrospective forecast errors corresponding to the ",
                        "youngest age component of ",
                        tolower(SIMPLESIBREG$stockabundance),
                        " for the ",
                        SIMPLESIBREG$stockname,
                        " ",
                        SIMPLESIBREG$stockspecies,
                        ".")

SIMPLESIBREG$results <- SIMPLESIBREG$best.rmse.youngest.age


SIMPLESIBREG$myplot <- SIMPLESIBREG$bias.coefficient.afe.youngest.age.retro.simplesib(SIMPLESIBREG$results, SIMPLESIBREG$stockabundance, SIMPLESIBREG$index.year)

## 

SIMPLESIBREG$results <- NULL 

doc = addPlot(doc,
        fun=grid.draw,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight 
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

##
## Bias Coefficient Plots for the Older Ages 
## 

doc = addPageBreak(doc)

SIMPLESIBREG$figurecaption <- paste0("Bias coefficient plot obtained from the retrospective forecast errors corresponding to the ",
                        "older age components of ",
                        tolower(SIMPLESIBREG$stockabundance),
                        " for the ",
                        SIMPLESIBREG$stockname,
                        " ",
                        SIMPLESIBREG$stockspecies,
                        ".")


SIMPLESIBREG$fits <- SIMPLESIBREG$results_best_fitting_model_for_each_age_class  ## fits
SIMPLESIBREG$results <- SIMPLESIBREG$individual.ages.retro.predictive.performance.simple.sibling.regression.youngest(
                                    SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression, 
                                    SIMPLESIBREG$index.year)  # results


SIMPLESIBREG$myplot <- SIMPLESIBREG$bias.coefficients.afe.older.ages.retro.simplesib(SIMPLESIBREG$fits, SIMPLESIBREG$results, SIMPLESIBREG$stockabundance)

## rm(fits)
SIMPLESIBREG$fits <- NULL 
## 
SIMPLESIBREG$results <- NULL 

doc = addPlot(doc,
        fun=grid.draw,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight 
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

##
## Bias Coefficient Plot for the Total Age 
## 

doc = addPageBreak(doc)

SIMPLESIBREG$figurecaption <- paste0("Bias coefficient plot obtained from the retrospective forecast errors corresponding to the ",
                        " total ",
                        tolower(SIMPLESIBREG$stockabundance),
                        " for the ",
                        SIMPLESIBREG$stockname,
                        " ",
                        SIMPLESIBREG$stockspecies,
                        ".")

## myplot <- plot.hist.retrospective.forecast.errors.total.age.simple.sibling.regression(best.rmse.youngest.age, stockabundance)

SIMPLESIBREG$results <- SIMPLESIBREG$best.rmse.youngest.age
SIMPLESIBREG$myplot <-  SIMPLESIBREG$bias.coefficient.afe.total.age.retro.simplesib(SIMPLESIBREG$results, SIMPLESIBREG$stockabundance)


## myplot <- ggplot(NULL, aes(rnorm(100),rnorm(100))) + geom_point(col="lightblue", size=3)


## 

SIMPLESIBREG$results <- NULL 

doc = addPlot(doc,
        fun=grid.draw,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight 
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

addTitle(doc, "Bar Plots of the Retrospective Forecast Errors Annotated with Forecast Intervals", level=2)

##
## Gary's Plot: Individual Ages
##


for (j in 1:length(SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin)){

    if (j > 1) { doc = addPageBreak(doc) }

    SIMPLESIBREG$age <- names(SIMPLESIBREG$best.rmse.youngest.age$retro$resjoin)[j]

    usePackage("stringr")

    SIMPLESIBREG$age <- str_replace(SIMPLESIBREG$age, "_"," ")

    SIMPLESIBREG$age <- tolower(SIMPLESIBREG$age)

    if (j==1){

    SIMPLESIBREG$figurecaption <- paste0("Retrospective forecast errors and 80% interval forecast associated with the ",
                            SIMPLESIBREG$age,
                            " component of the ",
                            tolower(SIMPLESIBREG$stockabundance), " ",
                            "for the ",
                            SIMPLESIBREG$stockname, " ",
                            SIMPLESIBREG$stockspecies, " stock,",
                            " derived via ",
                            SIMPLESIBREG$best.rmse.youngest.age$method,
                            ". ",
                            "A positive forecast error indicates an under-forecast and a negative forecast error indicates an over-forecast.")
    } else if (j > 1) {


     SIMPLESIBREG$figurecaption <- paste0("Retrospective forecast errors and 80% interval forecast associated with the ",
                            SIMPLESIBREG$age,
                            " component of the ",
                            tolower(SIMPLESIBREG$stockabundance), " ",
                            "for the ",
                            SIMPLESIBREG$stockname, " ",
                            SIMPLESIBREG$stockspecies, " stock,",
                            " derived via ",
                            "simple sibling regression (best model)",
                            ". ",
                            "A positive forecast error indicates an under-forecast and a negative forecast error indicates an over-forecast.")

    }


    SIMPLESIBREG$myplot <- SIMPLESIBREG$gary.plot.individual.ages.simple.sibling.regression(SIMPLESIBREG$best.rmse.youngest.age,
                                                                 SIMPLESIBREG$pred.int.individual.ages.avgfive.youngest,   
                                                                 # youngest age prediction: avgfive
                                                                 SIMPLESIBREG$pred.int.individual.ages.arima.youngest,     
                                                                 # youngest age prediction: arima
                                                                 SIMPLESIBREG$pred.int.individual.ages.expsmooth.youngest, 
                                                                 # youngest age prediction: expsmooth
                                                                 SIMPLESIBREG$pred.int.individual.ages.simple.sibling.regression, 
                                                                 # older ages prediction: sibling regression (best model)
                                                                 SIMPLESIBREG$forecastingyear, j)

    doc = addPlot(doc,
                  fun=print,
                  x=SIMPLESIBREG$myplot,
                  width=plotwidth, 
                  ## height=plotheight, 
                  height=plotheight - 2)

    doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")
    
    ## rm(myplot)

    SIMPLESIBREG$myplot <- NULL 
    SIMPLESIBREG$figurecaption <- NULL 

}


##
## Gary's Plot: Total Age
##

doc = addPageBreak(doc)

SIMPLESIBREG$figurecaption <- paste0("Retrospective forecast errors and 80% interval forecast associated with the ",
                            "total ",
                            tolower(SIMPLESIBREG$stockabundance), " ",
                            "for the ",
                            SIMPLESIBREG$stockname, " ",
                            SIMPLESIBREG$stockspecies, " stock,",
                            " derived via ",
                            "simple sibling regression",
                            ".",
                            " A positive forecast error indicates an under-forecast and a negative forecast error indicates an over-forecast.")

SIMPLESIBREG$myplot <- SIMPLESIBREG$gary.plot.total.age.simple.sibling.regression(SIMPLESIBREG$best.rmse.youngest.age,
                                               SIMPLESIBREG$pred.int.total.age.simple.sibling.regression.all.models,  # total age prediction
                                               SIMPLESIBREG$forecastingyear)

doc = addPlot(doc,
              fun=print,
              x=SIMPLESIBREG$myplot,
              width=plotwidth, 
              # height=plotheight
              height=plotheight - 2
              )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

#############################################################################################
##
## Retrospective Forecasts vs. Observed Values: Individual Ages
##
#############################################################################################

doc = addPageBreak(doc)

doc = addTitle(doc, "Forecast Diagnostics", level=1)

SIMPLESIBREG$paragraph <- paste("The forecast diagnostics included in this report are visual in nature and include:")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 

doc = addParagraph( doc, value = paste0('Superimposed time series plots displaying the retrospective point forecasts against the actual historical values;'),
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = paste0('Scatter plots displaying the retrospective point forecasts against the actual historical values.'),
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")


SIMPLESIBREG$paragraph <- paste("The purpose of the forecast diagnostics is to enable a visual assessment of how well the retrospective point forecasts ", 
                   "track the actual historical values of age-specific and total ", 
                   tolower(SIMPLESIBREG$stockabundance), ". ", 
                   "These diagnostics will reveal issues with the particular forecasting models used, such as a ", 
                   "tendency to under-forecast or over-forecast over time.")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 

doc = addSection(doc, landscape = TRUE, ncol = 1) 

##
## Superimposed Time Series Plots of Retrospective Forecasts and Actual Values: Individual Ages 
##

## doc = addPageBreak(doc)

SIMPLESIBREG$figurecaption <- paste0("Superimposed time series plots of  ",
                       " retrospectively forecasted and actual age-specific ",
                       tolower(SIMPLESIBREG$stockabundance), " values",
                       " for the ",
                       SIMPLESIBREG$stockname, " ",
                       SIMPLESIBREG$stockspecies, " stock.")


SIMPLESIBREG$results <- SIMPLESIBREG$best.rmse.youngest.age

SIMPLESIBREG$myplot <- SIMPLESIBREG$timeseries.plot.results.afe.individual.ages.retro.simplesib(SIMPLESIBREG$results,
                                                             SIMPLESIBREG$stockabundance)

## 

SIMPLESIBREG$results <- NULL 

doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotheight+2, 
        # height=plotheight
        height=plotheight-2
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

doc = addSection(doc, landscape = FALSE, ncol = 1) 

##
## Scatter Plots of Retrospective Forecasts and Actual Values: Individual Ages 
##


SIMPLESIBREG$figurecaption <- paste0("Scatter plots of retrospectively forecasted ",
                       " versus actual ",
                       tolower(SIMPLESIBREG$stockabundance), " values",
                       " for specific age components of the ",
                       SIMPLESIBREG$stockname, " ",
                       SIMPLESIBREG$stockspecies, " stock.",
                       " Observations are labeled according to the associated historical return years.",
                       " For the youngest age, the retrospectively forecasted ",
                       tolower(SIMPLESIBREG$stockabundance),
                       " values were obtained via ",
                        SIMPLESIBREG$best.rmse.youngest.age$method,
                        ".",
                        " For the other ages, the retrospectively forecasted ",
                        tolower(SIMPLESIBREG$stockabundance),
                       " values ",
                        "were obtained on the basis of the ",
                        " simple sibling regression models corresponding to those ages." )
                        
SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.results.afe.individual.ages.retro.simple.sibling.regression(SIMPLESIBREG$best.rmse.youngest.age)

doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        ## height=plotheight
        height = plotheight 
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)
SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 



##
## Superimposed Time Series Plots of Retrospective Forecasts and Actual Values: Total Age
##


doc = addPageBreak(doc)

SIMPLESIBREG$figurecaption <- paste0("Superimposed time series plots of  ",
                       " retrospectively forecasted and actual total ",
                       tolower(SIMPLESIBREG$stockabundance), " values",
                       " for the ",
                       SIMPLESIBREG$stockname, " ",
                       SIMPLESIBREG$stockspecies, " stock.")

SIMPLESIBREG$results <- SIMPLESIBREG$best.rmse.youngest.age

SIMPLESIBREG$myplot <- SIMPLESIBREG$timeseries.plot.results.afe.total.age.retro.simplesib(SIMPLESIBREG$results, SIMPLESIBREG$stockabundance)



doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight - 2
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

##
## Scatterplots of Retrospective Forecasts vs. Observed Values: Total Age
##


doc = addPageBreak(doc)

SIMPLESIBREG$figurecaption <- paste0("Scatter plots of retrospectively forecasted ",
                       " versus actual total ",
                       tolower(SIMPLESIBREG$stockabundance), " values",
                       " for the ",
                       SIMPLESIBREG$stockname, " ",
                       SIMPLESIBREG$stockspecies, " stock.",
                       " Observations are labeled according to the associated historical return years.")

SIMPLESIBREG$myplot <- SIMPLESIBREG$plot.results.afe.total.age.retro.simple.sibling.regression(SIMPLESIBREG$best.rmse.youngest.age, SIMPLESIBREG$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLESIBREG$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight - 2
        )

doc = addParagraph(doc, value=SIMPLESIBREG$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLESIBREG$myplot <- NULL 
SIMPLESIBREG$figurecaption <- NULL 

#=========================================================================================================
# Updated data exploration
#=========================================================================================================

SIMPLESIBREG$upgrade_scatmat_data <- function (data){
    dataIsCharacter <- sapply(data, is.character)
    if (any(dataIsCharacter)) {
        dataCharacterColumns <- names(dataIsCharacter[dataIsCharacter])
        for (dataCol in dataCharacterColumns) {
            data[dataCol] <- as.factor(data[, dataCol])
        }
    }
    data
}



SIMPLESIBREG$scatmatold <- function (data, columns = 1:ncol(data), color = NULL, alpha = 1)
{

    usePackage("ggplot2")

    data <- SIMPLESIBREG$upgrade_scatmat_data(data)
    
    data.choose <- data[, columns]
    dn <- data.choose[sapply(data.choose, is.numeric)]
    if (ncol(dn) == 0) {
        stop("All of your variables are factors. Need numeric variables to make scatterplot matrix.")
    }
    else {

        ltdata.new <- lowertriangle(data, columns = columns, color = color)

        r <- ggplot(ltdata.new, mapping = aes_string(x = "xvalue",y = "yvalue")) +
                    facet_grid(ylab ~ xlab, scales = "free", switch="both", drop=FALSE) +
                     theme(axis.title.x = element_blank(),
                          axis.title.y = element_blank()) +
                     theme(aspect.ratio = 1)



        if (is.null(color)) {
            densities <- do.call("rbind", lapply(1:ncol(dn),
                function(i) {
                  data.frame(xlab = names(dn)[i], ylab = names(dn)[i],
                    x = dn[, i])
                }))
            for (m in 1:ncol(dn)) {
                j <- subset(densities, xlab == names(dn)[m])
                r <- r + stat_density(aes(x = x, y = ..scaled.. *
                  diff(range(x)) + min(x)), data = j, position = "identity",
                  geom = "line", color = "black")
            }
            r <- r + geom_point(alpha = alpha, na.rm = TRUE)
            return(r)
        }
        else {
            densities <- do.call("rbind", lapply(1:ncol(dn),
                function(i) {
                  data.frame(xlab = names(dn)[i], ylab = names(dn)[i],
                    x = dn[, i], colorcolumn = data[, which(colnames(data) ==
                      color)])
                }))
            for (m in 1:ncol(dn)) {
                j <- subset(densities, xlab == names(dn)[m])
                r <- r + stat_density(aes_string(x = "x", y = "..scaled.. * diff(range(x)) + min(x)",
                  colour = "colorcolumn"), data = j, position = "identity",
                  geom = "line")
            }
            r <- r + geom_point(data = ltdata.new, aes_string(colour = "colorcolumn"),
                alpha = alpha, na.rm = TRUE)
            return(r)
        }
    }
}


SIMPLESIBREG$ggscatmatrix <- function (data, columns = 1:ncol(data), color = NULL, alpha = 1,
    corMethod = "pearson")
{
    usePackage("GGally")
    usePackage("ggplot2")

    data <- SIMPLESIBREG$upgrade_scatmat_data(data)
    data.choose <- data[, columns]
    dn <- data.choose[sapply(data.choose, is.numeric)]

    if (ncol(dn) == 0) {
        stop("All of your variables are factors. Need numeric variables to make scatterplot matrix.")
    }

    if (ncol(dn) < 2) {
        stop("Not enough numeric variables to make a scatter plot matrix")
    }

    a <- uppertriangle(data, columns = columns, color = color,
        corMethod = corMethod)

    if (is.null(color)) {
        plot <- scatmat(data, columns = columns, alpha = alpha) +
            geom_text(data = a, aes_string(label = "r"), colour = "black")
    }

    else {
        plot <- scatmatold(data, columns = columns, color = color,
            alpha = alpha) + geom_text(data = a, aes_string(label = "r",
            color = "colorcolumn")) + labs(color = color)
    }

    factor <- data.choose[sapply(data.choose, is.factor)]
    if (ncol(factor) == 0) {
        return(plot)
    }
    else {
        warning("Factor variables are omitted in plot")
        return(plot)
    }
}





#=========================================================================================================
# Data exploration for all considered models
#=========================================================================================================

addTitle(doc, "Data Exploration for Simple Sibling Regression Models", level=1)


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("This section includes exploratory data plots which can be used in conjunction with the ",
                   "simple sibling regression models considered for forecasting ", 
                   SIMPLESIBREG$forecastingyear, " ", 
                   tolower(SIMPLESIBREG$stockabundance), " ", 
                   "for the ", 
                   SIMPLESIBREG$stockspecies, " ",
                   toupper(SIMPLESIBREG$stockname),
                   " stock. It is recommended that these plots be examined before consulting the summary output produced by these models.",    
                    sep=" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The exploratory data plots considered for each simple sibling regression model consist of ",
                   "density plots of Age k and Age k - 1 ",
                   tolower(SIMPLESIBREG$stockabundance), ", ", 
                   "along with the scatterplot of Age k versus Age k - 1 ", 
                   tolower(SIMPLESIBREG$stockabundance), 
                   " and ",
                   "the accompanying Pearson correlation coefficient describing the ", 
                   "direction and strength of the linear association between these two variables.",     
                    sep=" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("While the simple sibling linear regression does not require the normality of the Age k and Age k - 1 ",
                   tolower(SIMPLESIBREG$stockabundance), " data ", 
                   "included in the simple sibling regression model relating Age k to Age k - 1",  " ", 
                    tolower(SIMPLESIBREG$stockabundance), ", ", 
                   "examining the distribution of these variables via density plots can reveal problems with the data that may ",
                   "conspire to affect the model fit (e.g., multi-modality, skewness, outliers).  These problems should be noted and their ", 
                   "potential impact on the model fit should be monitored when examining model diagnostic plots.",       
                    sep=" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The simple sibling regression model which relates Age k to Age k - 1", " ", 
                    tolower(SIMPLESIBREG$stockabundance), 
                    " relies on the assumption that Age k ", tolower(SIMPLESIBREG$stockabundance), " ", 
                    "is linearly related to ", 
                    "Age k-1 ", tolower(SIMPLESIBREG$paragraphstockabundance), ". ", 
                    "The reasonableness of this assumption for the underlying data used to fit this model ", 
                    "can be verified by examining the scatterplot of Age k versus Age k - 1", 
                    tolower(SIMPLESIBREG$stockabundance), ". ", 
                    "If the scatterplot reveals a cloud of points which seem to follow either an upward or a downward trend, ", 
                    "then the linearity assumption is supported by the data represented in the scatterplot. ", 
                    "Otherwise, the linearity assumption is violated by the data.",     
                    sep=" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("Another assumption required by the simple sibling regression model which relates Age k to Age k - 1", " ", 
                    tolower(SIMPLESIBREG$stockabundance), 
                    " is that of constant variability of the model errors. ",  
                    "This assumption can be verified by examining the scatterplot of Age k versus Age k - 1", 
                    tolower(SIMPLESIBREG$stockabundance), " ", 
                    "and making sure that it doesn't display a cloud of points which follow a funnel-like pattern, ",
                    "which would be indicative of a violation of the constant error variability assumption. ", 
                    "If the variability of the points about the underlying upward or downward trend is constant as we move along the x-axis, ",
                    "its magnitude will indicate how precise the estimation of the true underlying trend will be. ",
                    "The smaller the variability, the more precise the estimation of the true trend. ",
                    "Conversely, the larger the variability, the more imprecise that estimation. ",
                    "The precision of point forecasts produced by the model will also be affected by the amount of ",
                    "variability present in the data - the smaller (larger) the variability, the more (less) precise the point forecasts.",           
                    sep=" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The scatterplot of Age k versus Age k - 1 ", 
                     tolower(SIMPLESIBREG$stockabundance), " ", 
                    "may reveal other problems with the data it depicts, including ", 
                    "gaps in the data", " or unusually large or small ",  
                     tolower(SIMPLESIBREG$stockabundance), 
                    " values along its x-axis and/or or y-axis (known as x-outliers and/or y-outliers). ", 
                    "Such problems should be noted and their potential effect on the fit of the corresponding ",
                    "simple sibling regression model should be tracked. ",
                    "As an example of such tracking, will any unusual observations identified in the scatterplot ", 
                    "translate into influential observations, which could significantly alter the model fit?",            
                    sep=" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLESIBREG$paragraph <- paste0("The Pearson correlation coefficient which quantifies the direction and strength of ",
                    "the linear association between Age k and Age k - 1 ", 
                    tolower(SIMPLESIBREG$stockabundance), 
                    " is a number which can take on any value between -1 and 1. ",
                    "The sign of this coefficient indicates the direction of the association: ", 
                    "a positive sign corresponds to a positive linear association, while a negative one corresponds to a negative linear one. ", 
                    "The magnitude of this coefficient conveys the strength of the association. ", 
                    "The closer the coefficient is to 1 in absolute value, the stronger the association.", 
                    "Conversely, the closer the coefficient is to 0, the weaker the association. ", 
                    "It is well-known that Pearson's correlation coefficient is sensitive to outliers present in the data ", 
                    "(e.g., its value can be artifically inflated in the presence of x-outliers and/or y-outliers).", 
                    "The value of the coefficient should be interpreted with caution in such situations.",           
                    sep=" ")

doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal")

SIMPLESIBREG$paragraph <- NULL 


SIMPLESIBREG$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLESIBREG$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


usePackage("ez")


SIMPLESIBREG$myezCor <- function(data, r_size_lims = c(10, 30), point_alpha = 0.5, density_height = 1,
    density_adjust = 1, density_colour = "white", label_size = 10,
    label_colour = "black", label_alpha = 0.5, lm_colour = "red",
    ci_colour = "green", ci_alpha = 0.5, test_alpha = 0.05, test_correction = "none")
{
    ntests = ((((ncol(data) - 1)^2) - (ncol(data) - 1))/2)
    if (test_correction[1] == "bonferroni") {
        test_alpha = test_alpha/ntests
    } else {
        if (test_correction[1] == "sidak") {
            test_alpha = 1 - (1 - test_alpha)^(1/ntests)
        }
    }
    for (i in 1:length(data)) {
        data[, i] = (data[, i] - mean(data[, i], na.rm = T))/sd(data[,
            i], na.rm = T)
    }
    z = data.frame()
    z_cor = data.frame()
    i = 1
    j = i
    while (i <= length(data)) {
        if (j > length(data)) {
            i = i + 1
            j = i
        }
        else {
            x = data[, i]
            y = data[, j]
            toss = is.na(x) | is.na(y)
            x = x[!toss]
            y = y[!toss]
            temp = as.data.frame(cbind(x, y))
            temp = cbind(temp, names(data)[i], names(data)[j])
            z = rbind(z, temp)
            this_cor = round(cor(x, y), 2)
            this_cor.test = cor.test(x, y)
            this_col = ifelse(this_cor.test$p.value < test_alpha,
                "a", "b")
            this_size = (this_cor)^2
            cor_text = ifelse(this_cor == 0, "0", ifelse(this_cor ==
                1, "1", ifelse(this_cor == -1, "-1", ifelse(this_cor >
                0, substr(format(c(this_cor, 0.123456789), digits = 2)[1],
                2, 4), paste("-", substr(format(c(this_cor, 0.123456789),
                digits = 2)[1], 3, 5), sep = "")))))
            b = as.data.frame(cor_text)
            b = cbind(b, this_col, this_size, names(data)[j],
                names(data)[i])
            z_cor = rbind(z_cor, b)
            j = j + 1
        }
    }
    names(z) = c("x", "y", "x_lab", "y_lab")
    z = z[z$x_lab != z$y_lab, ]
    names(z_cor) = c("cor", "p", "rsq", "x_lab", "y_lab")
    z_cor = z_cor[z_cor$x_lab != z_cor$y_lab, ]
    diag = melt(data, measure.vars = names(data))
    names(diag)[1] = "x_lab"
    diag$y_lab = diag$x_lab
    dens = ddply(diag, .(x_lab, y_lab), function(x) {
        d = density(x$value[!is.na(x$value)], adjust = density_adjust)
        d = data.frame(x = d$x, y = d$y)
        d$ymax = d$y * (max(abs(c(z$x, z$y))) * 2 * density_height)/max(d$y) -
            max(abs(c(z$x, z$y))) * density_height
        d$ymin = -max(abs(c(z$x, z$y))) * density_height
        return(d)
    })
    labels = ddply(diag, .(x_lab, y_lab), function(x) {
        to_return = data.frame(x = 0, y = 0, label = x$x_lab[1])
        return(to_return)
    })
    
    points_layer = layer(geom = "point",  stat = "identity", position = "identity", params = list(alpha = point_alpha),
                         data = z, mapping = aes_string(x = "x", y = "y"))
    
    lm_line_layer = layer(geom = "line",  stat = "smooth", position = "identity", params = list(colour = lm_colour, method = "lm"),
                          data = z, mapping = aes_string(x = "x", y = "y"))
        
        
    lm_ribbon_layer = layer(geom = "ribbon",  stat = "smooth", position = "identity",
                            params = list(fill = ci_colour, alpha = ci_alpha, method = "lm"), 
                            data = z, mapping = aes_string(x = "x", y = "y"))
        
    cor_text_layer = layer(geom = "text", stat = "identity", position = "identity", 
                           data = z_cor, 
                           mapping = aes_string(label = "cor", size = "rsq", colour = "p", x = 0, y = 0))
        
    dens_layer = layer(geom = "ribbon", stat = "identity", position = "identity",
                       params = list(colour = "transparent", fill = "white"),      
                       data = dens, 
                       mapping = aes_string(x = "x", ymax = "ymax", ymin = "ymin"))
        
    label_layer = layer(geom = "text", stat = "identity", position = "identity",
                        params = list(colour = label_colour,size = label_size, alpha = label_alpha), 
                        data = labels, 
                        mapping = aes_string(x = "x", y = "y", label = "label"))
        
        
    y_lab = NULL
    x_lab = NULL
    
    f = facet_grid(x_lab ~ y_lab)
    
    packs = installed.packages()
    
    ggplot2_version_char = packs[dimnames(packs)[[1]] == "ggplot2",
        dimnames(packs)[[2]] == "Version"]
    
    ggplot2_version_char = strsplit(ggplot2_version_char, ".",
        fixed = T)[[1]]
    
   
    o = theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            axis.ticks = element_blank(), axis.text.y = element_blank(),
            axis.text.x = element_blank(), axis.title.y = element_blank(),
            axis.title.x = element_blank(), legend.position = "none"
            # ,
            # strip.background = element_blank() # strip.text.x = element_blank(),
            # strip.text.y = element_blank()
            )
            
    
    x_scale = scale_x_continuous(limits = c(-1 * max(abs(dens$x)),
        max(abs(dens$x))))
        
    size_scale = scale_size(limits = c(0, 1), range = r_size_lims)
    
    
    
    return(ggplot(z_cor) + points_layer + # lm_ribbon_layer + lm_line_layer +
        dens_layer + # label_layer +
        cor_text_layer + f + o +
        x_scale +
        size_scale
        )
}


## myezCor(mydata)

SIMPLESIBREG$fits <-  SIMPLESIBREG$sibling_regression_model_fits(SIMPLESIBREG$data_and_model_formulas)

SIMPLESIBREG$fits$model_data
SIMPLESIBREG$fits$model_formulas
SIMPLESIBREG$fits$model_fits


for (i in 1:length(SIMPLESIBREG$fits$model_fits)) {

     SIMPLESIBREG$forms <- SIMPLESIBREG$fits$model_formulas[[i]]$model_formulas
     SIMPLESIBREG$dats <- SIMPLESIBREG$fits$model_data[[i]]

     ## extract predictor variables
     usePackage("formula.tools")
     ## var <- attr(model$terms, "term.labels")
     SIMPLESIBREG$vars <- rhs.vars(SIMPLESIBREG$forms)
     ## extract response variable
     SIMPLESIBREG$resp <- lhs.vars(SIMPLESIBREG$forms)

     SIMPLESIBREG$mydata <- subset(SIMPLESIBREG$dats, select=c(SIMPLESIBREG$resp,SIMPLESIBREG$vars))
     
     SIMPLESIBREG$mydata <- SIMPLESIBREG$mydata[,ncol(SIMPLESIBREG$mydata):1]

     SIMPLESIBREG$pp <- SIMPLESIBREG$ggscatmatrix(SIMPLESIBREG$mydata) + 
               scale_y_continuous(labels=scales::comma) +  
                 scale_x_continuous(labels=scales::comma) + 
                 theme_bw() + 
                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                 xlab(paste0(SIMPLESIBREG$stockabundance, " for the ", SIMPLESIBREG$stockname, " Stock")) + 
                 ylab(paste0(SIMPLESIBREG$stockabundance, " for the ", SIMPLESIBREG$stockname, " Stock"))


        doc = addPlot(doc,
        fun = print,
        x=SIMPLESIBREG$pp,
        width=plotwidth, height=plotheight)

        SIMPLESIBREG$mylegend <- paste0("Data exploration for the simple sibling regression model whose formula is given by ",
                                            as.character(SIMPLESIBREG$forms), ". ", 
                           "The top left and bottom right panels display the density plots of ", SIMPLESIBREG$resp, " and ", SIMPLESIBREG$vars, " " , tolower(SIMPLESIBREG$stockabundance), ", ", 
                           "respectively. ",  
                           "The bottom left panel shows the scatterplot of ", SIMPLESIBREG$resp, " versus ", SIMPLESIBREG$vars, " ",  tolower(SIMPLESIBREG$stockabundance), ", while ", 
                           "the top right panel lists the accompanying Pearson correlation coefficient describing the direction and strength of", 
                           " linear association between ", SIMPLESIBREG$resp, " and ", SIMPLESIBREG$vars, " ",  tolower(SIMPLESIBREG$stockabundance), ".") 

        doc = addParagraph(doc, value=SIMPLESIBREG$mylegend, stylename="rPlotLegend")


    
}

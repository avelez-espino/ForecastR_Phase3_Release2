#========================================================================================================
# CoverPage
#========================================================================================================

## SIMPLELOGPOWER$best.fits.simplelogpower <- SIMPLELOGPOWER$fits.simplelogpower

if (SIMPLELOGPOWER$total.index.simplelogpower==1) {   # naive model for youngest age
    SIMPLELOGPOWER$model.used.for.youngest.age <- "Naive Model (Average of Previous 5 Years)"
}

if (SIMPLELOGPOWER$total.index.simplelogpower==2) {  # arima model for youngest age
    SIMPLELOGPOWER$model.used.for.youngest.age <- "ARIMA Model"
}

if (SIMPLELOGPOWER$total.index.simplelogpower==3) { # exponential smoothing model for youngest age
   SIMPLELOGPOWER$model.used.for.youngest.age <- "Exponential Smoothing Model"
}


pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 40) )
my.pars = set_of_paragraphs(pot1)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

doc = addParagraph( doc, value =" ", stylename="Normal" )

pot1 = pot(" ", textProperties(font.weight="bold", font.size = 20) )
my.pars = set_of_paragraphs(pot1)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

doc = addParagraph( doc, value =" ", stylename="Normal" )

pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(SIMPLELOGPOWER$stockname), textProperties(font.size = 20) )
my.pars = set_of_paragraphs(pot2)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

doc = addParagraph( doc, value =" ", stylename="Normal" )

pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(SIMPLELOGPOWER$stockspecies), textProperties(font.size = 20) )
my.pars = set_of_paragraphs(pot3)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


doc = addParagraph( doc, value =" ", stylename="Normal" )

pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(SIMPLELOGPOWER$stockabundance), textProperties(font.size = 20) )
my.pars = set_of_paragraphs(pot4)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


doc = addParagraph( doc, value =" ", stylename="Normal" )

pot5 =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(SIMPLELOGPOWER$forecastingyear), textProperties(font.size = 20) )
my.pars = set_of_paragraphs(pot5)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


doc = addParagraph( doc, value =" ", stylename="Normal" )

pot6 =  pot("Forecasting Model Used for Youngest Age: ", textProperties(font.weight="bold", font.size = 20)) 
my.pars = set_of_paragraphs(pot6)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


doc = addParagraph( doc, value =" ", stylename="Normal" )

pot9 = pot(paste(SIMPLELOGPOWER$model.used.for.youngest.age), textProperties(font.size = 20) ) 
my.pars = set_of_paragraphs(pot9)
doc = addParagraph( doc, value = my.pars, stylename="BulletList" )
               
## pot10 <- pot("ARIMA Model", textProperties(font.size = 20) ) 
## my.pars = set_of_paragraphs(pot10)
## doc = addParagraph( doc, value = my.pars, stylename="BulletList" )
     
## pot11 <- pot("Exponential Smoothing Model", textProperties(font.size = 20) )
## my.pars = set_of_paragraphs(pot11)
## doc = addParagraph( doc, value = my.pars, stylename="BulletList" )


doc = addParagraph( doc, value =" ", stylename="Normal" )

pot6 =  pot("Forecasting Models Used for Older Ages: ", textProperties(font.weight="bold", font.size = 20)) 
my.pars = set_of_paragraphs(pot6)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


doc = addParagraph( doc, value =" ", stylename="Normal" )

pot7 =  pot("Simple Log Power Regression Models", textProperties(font.size = 20) ) 
my.pars = set_of_paragraphs(pot7)
doc = addParagraph( doc, value = my.pars, stylename="BulletList" )


doc = addParagraph( doc, value =" ", stylename="Normal" )

## pot12 =  pot("Data File: ", textProperties(font.weight="bold", font.size = 20) ) + 
##        pot(paste(Sys.Date()), textProperties(font.size = 20) )
## my.pars = set_of_paragraphs(pot12)
## doc = addParagraph( doc, value = my.pars, stylename="Normal" )
     
pot13 =  pot("Date: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(Sys.Date()), textProperties(font.size = 20) )
my.pars = set_of_paragraphs(pot13)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )
     
doc = addPageBreak(doc)


#========================================================================================================
# Compute Summary of Forecasting Results for Log Power Regression Without Environmental Covariates
#========================================================================================================

## Measures of Retrospective Forecast Performance

SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression <-
   SIMPLELOGPOWER$retro.measures.all.ages.simplelogpower(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower)

## data.frame(lapply(summary.forecast.results.simplelogpower.regression, as.character), stringsAsFactors=FALSE)

usePackage("scales")  ## DEBUG - FEB 13, 2017

SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression[,-1]  <-
   comma(SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression[,-1])



SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression <-
    data.frame(lapply(SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression, as.character), stringsAsFactors=FALSE)

#### r.squared.retro.simplelogpower.regression(best.rmse.youngest.age.simplelogpower)


## SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression <- rbind.data.frame(
##       SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression,
##       c("R-squared",
##         as.character(SIMPLELOGPOWER$r.squared.retro.simplelogpower.regression(best.rmse.youngest.age.simplelogpower)$r.squared))
##       )


names(SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression)[1] <- "Item"

## Point and Interval Forecasts

# this function deals only with forecasts produced by best log power regression models for older ages
SIMPLELOGPOWER$table_interval_forecasts <- function(results_best_fitting_model_for_each_age_class_simplelogpower,
                                     PI.individual.ages.simplelogpower.regression.no.comma,
                                                           forecastingyear, stockabundance){

    point_forecast_results <-
      SIMPLELOGPOWER$point.forecast.best.fitting.model.for.each.age.class.simplelogpower(results_best_fitting_model_for_each_age_class_simplelogpower,
                                                           forecastingyear, SIMPLELOGPOWER$datafile_variables)

    interval_forecast_results <- PI.individual.ages.simplelogpower.regression.no.comma

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
         usePackage("scales")
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

## total.index.simplelogpower <- best.rmse.youngest.age.simplelogpower$index.min.rmse.total.age # index of best model used
##                                                               # to forecast abundance for
##                                                               # youngest age, where
##                                                               # index is 1 for naive (avg. of 5 years),
##                                                               #          2 for arima
##                                                               #          3 for exponential smoothing

SIMPLELOGPOWER$total.index.simplelogpower


## usePackage("Hmisc")

capitalize <- function (string) {
     capped <- grep("^[^A-Z]*$", string, perl = TRUE)
     substr(string[capped], 1, 1) <- toupper(substr(string[capped], 
         1, 1))
     return(string)
}


SIMPLELOGPOWER$youngest.age.simplelogpower <- names(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin)[1]
SIMPLELOGPOWER$youngest.age.simplelogpower.capitalized <- capitalize(SIMPLELOGPOWER$youngest.age.simplelogpower)


if (SIMPLELOGPOWER$total.index.simplelogpower==1) {   # naive model for youngest age

SIMPLELOGPOWER$ttrow <- c(SIMPLELOGPOWER$youngest.age.simplelogpower.capitalized,
           "Naive Model (Average of Previous 5 Years)", paste(SIMPLELOGPOWER$forecastingyear),
           comma(round(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.ctr)),
           paste0(comma(round(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.lwr)),
                  " - ",
                  comma(round(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.upr)))
           )
} 

if (SIMPLELOGPOWER$total.index.simplelogpower==2) {  # arima model for youngest age

    SIMPLELOGPOWER$arimafit <- SIMPLELOGPOWER$arima.model.fit.youngest
    SIMPLELOGPOWER$arimamodel <- SIMPLELOGPOWER$arimafit$model

    sink("arimafit.txt")
    print(SIMPLELOGPOWER$arimamodel)
    sink()

    SIMPLELOGPOWER$out <- readLines("arimafit.txt")
    usePackage("stringr")
    SIMPLELOGPOWER$out.pattern <- str_detect(string=SIMPLELOGPOWER$out, pattern="ARIMA")

    SIMPLELOGPOWER$modelarima <- SIMPLELOGPOWER$out[SIMPLELOGPOWER$out.pattern==TRUE]
    SIMPLELOGPOWER$modelarima <- str_trim(SIMPLELOGPOWER$modelarima)

    SIMPLELOGPOWER$ttrow <- c(SIMPLELOGPOWER$youngest.age.simplelogpower.capitalized,
               SIMPLELOGPOWER$modelarima, paste(SIMPLELOGPOWER$forecastingyear),
               comma(round(SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.ctr)),
                paste0(comma(round(SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.lwr)),
                  " - ",
                  comma(round(SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.upr))))
}

if (SIMPLELOGPOWER$total.index.simplelogpower==3) { # exponential smoothing model for youngest age

    SIMPLELOGPOWER$expsmoothfit <- SIMPLELOGPOWER$expsmooth.model.fit.youngest
    SIMPLELOGPOWER$expsmoothmodel <- SIMPLELOGPOWER$expsmoothfit$model

    sink("expsmoothfit.txt")
    print(SIMPLELOGPOWER$expsmoothfit)
    sink()

    SIMPLELOGPOWER$out <- readLines("expsmoothfit.txt")
    usePackage("stringr")
    SIMPLELOGPOWER$out.pattern <- str_detect(string=SIMPLELOGPOWER$out, pattern="ETS")

    SIMPLELOGPOWER$modelexpsmooth <- SIMPLELOGPOWER$out[SIMPLELOGPOWER$out.pattern==TRUE]
    SIMPLELOGPOWER$modelexpsmooth <- str_trim(SIMPLELOGPOWER$modelexpsmooth)

    SIMPLELOGPOWER$ttrow <- c(SIMPLELOGPOWER$youngest.age.simplelogpower.capitalized,
               SIMPLELOGPOWER$modelexpsmooth, paste(SIMPLELOGPOWER$forecastingyear),
               comma(round(SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.ctr)),
                paste0(comma(round(SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.lwr)),
                  " - ",
                  comma(round(SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.upr))))
}
       

SIMPLELOGPOWER$tt


SIMPLELOGPOWER$tt <- rbind.data.frame(SIMPLELOGPOWER$ttrow,
                       SIMPLELOGPOWER$table_interval_forecasts(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower,
                                     SIMPLELOGPOWER$PI.individual.ages.simplelogpower.regression.no.comma,
                                                           SIMPLELOGPOWER$forecastingyear, SIMPLELOGPOWER$stockabundance)
                       )


## pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]]

SIMPLELOGPOWER$tt[nrow(SIMPLELOGPOWER$tt)+1,] <- c("Total","-",paste(SIMPLELOGPOWER$forecastingyear),
                              comma(SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$p[[SIMPLELOGPOWER$total.index.simplelogpower]]),
                              paste0(comma(SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$p.lwr[[SIMPLELOGPOWER$total.index.simplelogpower]]),
                                    " - ",
                                    comma(SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$p.upr[[SIMPLELOGPOWER$total.index.simplelogpower]]))
                              )

SIMPLELOGPOWER$point.and.interval.forecasts.simplelogpower.regression <- SIMPLELOGPOWER$tt


SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression <- list()

SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression$Return_Year <-
c("Return Year",
  SIMPLELOGPOWER$point.and.interval.forecasts.simplelogpower.regression[,"Forecasting Year"])


SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression$Best_Model  <-
c("Best Model",
   SIMPLELOGPOWER$point.and.interval.forecasts.simplelogpower.regression[,"Best Model"])

SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression$Point_Forecast <-
c("Point Forecast",
  SIMPLELOGPOWER$point.and.interval.forecasts.simplelogpower.regression[,"Point Forecast"])

SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression$Interval_Forecast <-
c("Interval Forecast",
  SIMPLELOGPOWER$point.and.interval.forecasts.simplelogpower.regression[,"Interval Forecast"])


SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression <- do.call(rbind.data.frame, SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression)

SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression <- as.data.frame(SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression)


## Combine retrospective measures of forecast performance with
## point and interval forecast results

colnames(SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression)   <-
     colnames(SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression)


SIMPLELOGPOWER$summary.results.simplelogpower.regression <-
rbind.data.frame(SIMPLELOGPOWER$summary.point.and.interval.forecasts.simplelogpower.regression,
                 SIMPLELOGPOWER$summary.forecast.results.simplelogpower.regression)

SIMPLELOGPOWER$summary.results.simplelogpower.regression

SIMPLELOGPOWER$summary.results.simplelogpower.regression  <-
   data.frame(lapply(SIMPLELOGPOWER$summary.results.simplelogpower.regression, as.character), stringsAsFactors=FALSE)

#========================================================================================================
# Include Summary of Forecasting Results in Word Report
#========================================================================================================

doc = addTitle(doc, "Summary of Forecasting Results", level=1)


SIMPLELOGPOWER$tablecaption <- paste0("Summary of forecasting results for the ",
                      SIMPLELOGPOWER$forecastingyear, " ",
                      "age-specific and total ",
                      paste0(tolower(SIMPLELOGPOWER$stockabundance),"s"),
                      " associated with the ",
                      SIMPLELOGPOWER$stockname, " ",
                      SIMPLELOGPOWER$stockspecies,
                      " stock.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$summary.results.simplelogpower.regression

usePackage("stringr")
names(SIMPLELOGPOWER$tt) <- str_replace_all(names(SIMPLELOGPOWER$tt),"_"," ")


## tt[,-1] <- comma(tt[,-1])

SIMPLELOGPOWER$my_ft <- FlexTable( data = SIMPLELOGPOWER$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLELOGPOWER$my_ft[, 1:ncol(SIMPLELOGPOWER$tt)] = parProperties(text.align = "right")



doc = addFlexTable(doc, flextable=SIMPLELOGPOWER$my_ft)


#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

SIMPLELOGPOWER$empirical.probability.yboot.simplelogpower.regression.total.age <- function(pred.int.total.age.simplelogpower.regression.all.models,
                                                               total.index.simplelogpower, stockabundance){


    if (total.index.simplelogpower==1) {   # naive model for youngest age (average of previous 5 years)

        y.star.boot.stacked <- pred.int.total.age.simplelogpower.regression.all.models$sim[[total.index.simplelogpower]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]] ## point forecast of total abundance

    }

    if (total.index.simplelogpower==2) {   # arima model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simplelogpower.regression.all.models$sim[[total.index.simplelogpower]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]] ## point forecast of total abundance

    }

    if (total.index.simplelogpower==3) {   # exponential smoothing model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simplelogpower.regression.all.models$sim[[total.index.simplelogpower]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]] ## point forecast of total abundance

    }

    ## cumulative probabilities evaluated for the endpoints of equal-sized bins covering the
    ## range of bootstrapped point forecasts

    x <- as.numeric(y.star.boot.stacked)
    ## usePackage("Hmisc")

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


SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age <- SIMPLELOGPOWER$empirical.probability.yboot.simplelogpower.regression.total.age(
                                                               SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models,
                                                               SIMPLELOGPOWER$total.index.simplelogpower, 
                                                               SIMPLELOGPOWER$stockabundance)


SIMPLELOGPOWER$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(SIMPLELOGPOWER$stockabundance)," ",
                       "value yet to be observed in ",
                       SIMPLELOGPOWER$forecastingyear, " for the ",
                       SIMPLELOGPOWER$stockname," ",
                       SIMPLELOGPOWER$stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(SIMPLELOGPOWER$stockabundance), ".")



SIMPLELOGPOWER$tablecaption_emp_prob_total <- SIMPLELOGPOWER$tablecaption 

doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

SIMPLELOGPOWER$tt_1 <- SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.thresholds

SIMPLELOGPOWER$tt_2 <- SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast

SIMPLELOGPOWER$tt_1_and_2 <- rbind.data.frame(SIMPLELOGPOWER$tt_1, SIMPLELOGPOWER$tt_2)

## usePackage("plyr")

## SIMPLELOGPOWER$tt_arrange <- arrange(SIMPLELOGPOWER$tt_1_and_2, prob.threshold)

#### SIMPLELOGPOWER$prob.threshold <-  SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast$prob.threshold

SIMPLELOGPOWER$tt_arrange <- SIMPLELOGPOWER$tt_1_and_2[order(SIMPLELOGPOWER$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

SIMPLELOGPOWER$from_tmp = which(SIMPLELOGPOWER$tt_arrange[,1] == SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast$prob.threshold)

SIMPLELOGPOWER$tt_arrange[SIMPLELOGPOWER$from_tmp, 4] <- SIMPLELOGPOWER$tt_arrange[SIMPLELOGPOWER$from_tmp + 1, 4]

SIMPLELOGPOWER$tt_arrange[,1] <- comma(SIMPLELOGPOWER$tt_arrange[,1])
SIMPLELOGPOWER$tt_arrange[,2] <- paste0(SIMPLELOGPOWER$tt_arrange[,2],"%")
SIMPLELOGPOWER$tt_arrange[,3] <- paste0(SIMPLELOGPOWER$tt_arrange[,3],"%")
SIMPLELOGPOWER$tt_arrange[,4] <- paste0(SIMPLELOGPOWER$tt_arrange[,4],"%")

names(SIMPLELOGPOWER$tt_arrange)[1] <- "Threshold"
names(SIMPLELOGPOWER$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(SIMPLELOGPOWER$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(SIMPLELOGPOWER$tt_arrange)[4] <- "Interval Probability"
SIMPLELOGPOWER$tt_arrange[1,4] <- "-"

SIMPLELOGPOWER$my_ft <- FlexTable( data = SIMPLELOGPOWER$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLELOGPOWER$my_ft[, 1:ncol(SIMPLELOGPOWER$tt_arrange)] = parProperties(text.align = "right")

SIMPLELOGPOWER$my_ft = spanFlexTableRows(SIMPLELOGPOWER$my_ft, j=4, from = SIMPLELOGPOWER$from_tmp, to = SIMPLELOGPOWER$from_tmp + 1)

## SIMPLELOGPOWER$my_ft[SIMPLELOGPOWER$tt_arrange$Threshold %in% comma(SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast[1]), ] 
# = cellProperties( background.color = "orange" )

SIMPLELOGPOWER$my_ft[SIMPLELOGPOWER$tt_arrange$Threshold %in% comma(SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

SIMPLELOGPOWER$my_ft_emp_prob_total <- SIMPLELOGPOWER$my_ft

doc = addFlexTable(doc, flextable=SIMPLELOGPOWER$my_ft_emp_prob_total)

doc = addPageBreak(doc)

SIMPLELOGPOWER$my_ft <- NULL  # delete my_ft as it is no longer needed

#========================================================================================================
# Introduction
#========================================================================================================


doc = addTitle(doc, "Introduction", level=1)

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste("In this report, we forecast the",
                  SIMPLELOGPOWER$forecastingyear,
                  "age-specific and total",
                  paste(tolower(SIMPLELOGPOWER$stockabundance),collapse="s",sep=""),
                  "for the",
                  SIMPLELOGPOWER$stockname,
                  SIMPLELOGPOWER$stockspecies,
                  "stock",
                  "using log power regression models without (environmental) covariates.")

doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

#=========================================================================================================
# Data used to model stock abundance for each age class
#=========================================================================================================

addTitle(doc, "Data Used for Log Power Regression Modeling", level=1)


## doc = addPageBreak(doc)

SIMPLELOGPOWER$is.even <- function(x) {x %% 2 == 0}
SIMPLELOGPOWER$is.odd <- function(x) {x %% 2 != 0}

for (i in 1:length(SIMPLELOGPOWER$data_and_model_formulas_simplelogpower$model_data)){

    SIMPLELOGPOWER$age <- names(SIMPLELOGPOWER$best.fits.simplelogpower)[i]
    SIMPLELOGPOWER$age  <- tolower(SIMPLELOGPOWER$age)

    SIMPLELOGPOWER$tablecaption <- paste0("Data used for log power regression modeling in connection with forecasting the ",
                          SIMPLELOGPOWER$age,
                          " ",
                          tolower(SIMPLELOGPOWER$stockabundance),
                          " for the ",
                          SIMPLELOGPOWER$stockname,
                          " ",
                          SIMPLELOGPOWER$stockspecies,
                          " stock."
                          )

    doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

    SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$data_and_model_formulas_simplelogpower$model_data[[i]]
    SIMPLELOGPOWER$ll <- 1:length(names(SIMPLELOGPOWER$tt))

    usePackage("scales")

    # add commas to the numbers in the age-specific abundance columns
    SIMPLELOGPOWER$tt[,names(SIMPLELOGPOWER$tt) %in%  names(SIMPLELOGPOWER$tt)[SIMPLELOGPOWER$is.odd(SIMPLELOGPOWER$ll)][-1]] <- 
        comma(SIMPLELOGPOWER$tt[,names(SIMPLELOGPOWER$tt) %in%  names(SIMPLELOGPOWER$tt)[SIMPLELOGPOWER$is.odd(SIMPLELOGPOWER$ll)][-1]])

    # define flex table and set default formatting properties
    SIMPLELOGPOWER$MyFTable = FlexTable(data=SIMPLELOGPOWER$tt, header.columns = FALSE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))

    # names(tt)[is.even(ll)]
    # names(tt)[is.odd(ll)][-1]

    names(SIMPLELOGPOWER$tt)[SIMPLELOGPOWER$is.even(SIMPLELOGPOWER$ll)] <- substr(names(SIMPLELOGPOWER$tt)[SIMPLELOGPOWER$is.even(SIMPLELOGPOWER$ll)],1,8)

    names(SIMPLELOGPOWER$tt)[SIMPLELOGPOWER$is.odd(SIMPLELOGPOWER$ll)][-1] <- SIMPLELOGPOWER$stockabundance


    usePackage("stringr")
    names(SIMPLELOGPOWER$tt) <- str_replace_all(names(SIMPLELOGPOWER$tt),"_"," ")

    SIMPLELOGPOWER$ttt <- SIMPLELOGPOWER$data_and_model_formulas_simplelogpower$model_data[[i]]
    SIMPLELOGPOWER$lll <- 1:length(names(SIMPLELOGPOWER$ttt))
    usePackage("stringr")
    names(SIMPLELOGPOWER$ttt) <- str_replace_all(names(SIMPLELOGPOWER$ttt),"_"," ")

    # add a first header row
    SIMPLELOGPOWER$MyFTable = addHeaderRow(SIMPLELOGPOWER$MyFTable,
                            value = c("",names(SIMPLELOGPOWER$ttt)[SIMPLELOGPOWER$is.odd(SIMPLELOGPOWER$lll)][-1]),
                            colspan=c(1,rep(2,length(names(SIMPLELOGPOWER$tt)[SIMPLELOGPOWER$is.odd(SIMPLELOGPOWER$ll)][-1]))),
                            par.properties=parProperties(text.align="center"))

    # add a second header row
    SIMPLELOGPOWER$MyFTable = addHeaderRow(SIMPLELOGPOWER$MyFTable, value = names(SIMPLELOGPOWER$tt))

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

    doc = addFlexTable(doc, SIMPLELOGPOWER$MyFTable)

    doc = addPageBreak(doc)
}


SIMPLELOGPOWER$tt <- NULL 
SIMPLELOGPOWER$ll <- NULL 
SIMPLELOGPOWER$ttt <- NULL 
SIMPLELOGPOWER$lll <- NULL 
SIMPLELOGPOWER$MyFTable <- NULL 


#=========================================================================================================
# Modeling: Older Ages (Log Power Regression without Environmental Covariates)
#=========================================================================================================

## doc = addPageBreak(doc)

addTitle(doc, "Simple Log Power Regression Modeling Results for the Older Age Components", level=1)

## youngest.age <- names(best.rmse.youngest.age.simplelogpower$retro$resjoin)[1]
usePackage("stringr")
SIMPLELOGPOWER$youngest.age.simplelogpower <- tolower(str_replace(SIMPLELOGPOWER$youngest.age.simplelogpower,"_"," "))


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("The simple log power regression methodology is applicable only to the older age components of ",
                    # forecastingyear, " ",
                    tolower(SIMPLELOGPOWER$stockabundance), " for the ",
                    SIMPLELOGPOWER$stockname, " ",
                    SIMPLELOGPOWER$stockspecies, " stock. ",
                    "This methodology is not applicable to the youngest age represented in the available historical ",
                    tolower(SIMPLELOGPOWER$stockabundance), " ", 
                    "data",
                    " (i.e., ",
                    SIMPLELOGPOWER$youngest.age.simplelogpower,
                    "). ", 
                    "For the youngest age, naive modeling (i.e., average of previous five years) and ", 
                    "time series modeling (i.e., ARIMA and exponential smoothing) are utilized instead to identify the best forecasting model of ", 
                    tolower(SIMPLELOGPOWER$stockabundance), ".")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("For each of the older age components of ",
                     SIMPLELOGPOWER$stockname, " ",
                    SIMPLELOGPOWER$stockspecies, " stock ",
                    tolower(SIMPLELOGPOWER$stockabundance), ", ", 
                   "the simple log power regression methodology involves fitting a simple linear regression model without intercept ",
                   "to the corresponding historical log-transformed ", 
                    tolower(SIMPLELOGPOWER$stockabundance), " data ", 
                    "and using that model to forecast ", 
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " for that age component in the year ", SIMPLELOGPOWER$forecastingyear, ". ",  
                    "The fitted model relates the older age component (Age k) to the younger age component (Age k - 1) of ",
                    "the stock. ", 
                    "The table below displays the simple log power regression models considered for the older age components of the stock.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

## insert candidate models here

SIMPLELOGPOWER$tablecaption <- paste0("Simple log power regression models considered for the older age components of ",
                       tolower(SIMPLELOGPOWER$stockabundance), ". ",               
                      "The 1 notation appearing in each model formula denotes the fact that the model includes an intercept term. ",
                       "For each model, both the response and the predictor variables are log-transformed prior to being included in the model. ",
                       "If any of the response or predictor variables include zero values, the log1p() transformation is applied to these variables ",
                       "instead of the log() transformation. ",
                       "The transformation log(x) computes the natural logarithm of x for x>0. The transformation log1p(x) computes log(1+x) for x>0.")
                       
doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$model_weights_simplelogpower

SIMPLELOGPOWER$tt <- subset(SIMPLELOGPOWER$tt, select=c("Age_Class", "Model"))

SIMPLELOGPOWER$tt

usePackage("stringr")

names(SIMPLELOGPOWER$tt) <- str_replace_all(names(SIMPLELOGPOWER$tt),"_"," ")

SIMPLELOGPOWER$MyFTable = FlexTable(data=SIMPLELOGPOWER$tt, header.columns=TRUE) # create flexTable

## SIMPLELOGPOWER$MyFTable = spanFlexTableRows(SIMPLELOGPOWER$MyFTable, j="Age_Class", runs=as.character(SIMPLELOGPOWER$tt[,"Age_Class"]))

doc = addFlexTable(doc, SIMPLELOGPOWER$MyFTable,
                    layout.properties=tableProperties(header.text=textProperties(font.size=10, font.weight="bold"),
                                                 data.text=textProperties(font.size=10)))

## doc = addTable(doc, data=tt,
##               layout.properties=tableProperties(header.text=textProperties(font.size=10, font.weight="bold"),
##                                                 data.text=textProperties(font.size=10)))
## rm(tablecaption)

SIMPLELOGPOWER$tablecaption <- NULL 

doc = addParagraph(doc, value=" ", stylename="Normal")

SIMPLELOGPOWER$paragraph <- paste0("The output associated with the simple log power regression models considered for the older age components of the ", 
                            SIMPLELOGPOWER$stockname, " ",
                            SIMPLELOGPOWER$stockspecies, " ",
                            tolower(SIMPLELOGPOWER$stockabundance), 
                            " stock ",             
                            "is reported in the tables below.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 

for (i in 1:length(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower)) {
  SIMPLELOGPOWER$tablecaption <- paste0("Output associated with the best log power regression model identified on the basis of the (corrected) AIC criterion for the ",
                        tolower( names(SIMPLELOGPOWER$best.fits.simplelogpower))[i], " ",
                        "component of the ",
                        tolower(SIMPLELOGPOWER$stockabundance), " for the ",
                        SIMPLELOGPOWER$stockname, " ",
                        SIMPLELOGPOWER$stockspecies, " stock. ",
                        "The model formula is given by ", as.character(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower[[i]]$formula),".")
  doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")
  ## tt <- format_results_best_fitting_model_for_each_age_class_simplelogpower(results_best_fitting_model_for_each_age_class_simplelogpower,i)
  ## doc = addTable(doc, data=tt,
  ##               layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
  ##                                                 data.cell=cellProperties(border.color="black")))
  
  SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$format_results_best_fitting_model_for_each_age_class_simplelogpower(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower,i)
  SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$tt[,-ncol(SIMPLELOGPOWER$tt)]
  SIMPLELOGPOWER$tt_flex <- FlexTable(SIMPLELOGPOWER$tt, header.cell.props = cellProperties(padding = 5),   
                       body.cell.props = cellProperties( padding = 5 ))
  
  SIMPLELOGPOWER$simplelogpowerfit <- SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower[[i]]$summary
   
  sink("simplelogpowerfit.txt")
    print(SIMPLELOGPOWER$simplelogpowerfit)
  sink()

  SIMPLELOGPOWER$out <- readLines("simplelogpowerfit.txt")
  
  usePackage("stringr")
 
  SIMPLELOGPOWER$tempout1 <- SIMPLELOGPOWER$out
  SIMPLELOGPOWER$out1 <- SIMPLELOGPOWER$tempout1[grepl("Residual standard error", SIMPLELOGPOWER$tempout1)]

  SIMPLELOGPOWER$tempout2 <- SIMPLELOGPOWER$out
  SIMPLELOGPOWER$out2 <- SIMPLELOGPOWER$tempout2[grepl("Multiple R-squared", SIMPLELOGPOWER$tempout2)]
  SIMPLELOGPOWER$out2 <- str_replace(SIMPLELOGPOWER$out2,"\t", " ")

  SIMPLELOGPOWER$tempout3 <- SIMPLELOGPOWER$out
  SIMPLELOGPOWER$out3 <- SIMPLELOGPOWER$tempout3[grepl("F-statistic", SIMPLELOGPOWER$tempout3)]

  SIMPLELOGPOWER$tt_flex = addFooterRow(SIMPLELOGPOWER$tt_flex, value = SIMPLELOGPOWER$out1,
                          colspan = ncol(SIMPLELOGPOWER$tt), text.properties = textItalic())

  SIMPLELOGPOWER$tt_flex = addFooterRow(SIMPLELOGPOWER$tt_flex, value = SIMPLELOGPOWER$out2,
                          colspan = ncol(SIMPLELOGPOWER$tt), text.properties = textItalic())

  SIMPLELOGPOWER$tt_flex = addFooterRow(SIMPLELOGPOWER$tt_flex, value = SIMPLELOGPOWER$out3,
                          colspan = ncol(SIMPLELOGPOWER$tt), text.properties = textItalic())
  
  doc = addFlexTable(doc, SIMPLELOGPOWER$tt_flex)
  
  doc = addParagraph(doc, value=" ", stylename="Normal")
  
  ## doc = addParagraph(doc, value=" ", stylename="Normal")
  
  SIMPLELOGPOWER$out <- NULL 
  SIMPLELOGPOWER$tempout1 <- NULL 
  SIMPLELOGPOWER$tempout2 <- NULL 
  SIMPLELOGPOWER$tempout3 <- NULL
  SIMPLELOGPOWER$out1 <- NULL 
  SIMPLELOGPOWER$out2 <- NULL 
  SIMPLELOGPOWER$out3 <- NULL
  SIMPLELOGPOWER$tt_flex <- NULL  
  
}





##
## Model Fits:  Effect Visualization
##

## doc = addPageBreak(doc)

SIMPLELOGPOWER$pots <- pot("To help visualize the effects estimated by the simple log power regression models for the older ages, we resort to ") +
        pot("effect displays ", textProperties(font.style="italic")) +
        pot("(Fox, 1987, 2003).")
SIMPLELOGPOWER$pots.text <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.text, stylename="Normal", 
                   par.properties=parProperties(text.align="justify", padding=10))

SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.text <- NULL 
## doc = addParagraph(doc, value=" ", stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- pot("Effect displays are graphs of fitted values computed from an estimated log power regression model that allow us to see how the expected value of the response variable changes with the values of each of the predictors in the model. ") +
        pot("As such, effect displays are an alternative to interpreting simple log power regression models directly from the estimated coefficients.")
SIMPLELOGPOWER$pots.text <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.text, stylename="Normal", 
                   par.properties=parProperties(text.align="justify", padding=10))

SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.text <- NULL 
## doc = addParagraph(doc, value=" ", stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

## SIMPLELOGPOWER$pots <- pot("In this report, all effect displays are constructed under the assumption that the predictor variable included in the log power regression model of interest is assumed to have a linear effect on the (log-transformed) response variable. ")
## SIMPLELOGPOWER$pots.text <- set_of_paragraphs(SIMPLELOGPOWER$pots)
## doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.text, stylename="Normal")
##
## SIMPLELOGPOWER$pots <- NULL 
## SIMPLELOGPOWER$pots.text <- NULL 


SIMPLELOGPOWER$pots <- pot("Because the simple log power regression models reported here include a single predictor variable, a single effect display will be generated for each model. ") + 
        pot("Given a model, the corresponding effect display will reveal the linear effect of the predictor variable used in the model on the response variable. ") +
        pot("The effect display will consist of a straight line, whose slope will capture the direction and strength of the relationship between the predictor variable and the response variable. ") +
        pot("A positive slope will indicate a positive relationship, whereas a negative slope will indicate a negative relationship. ") +
        pot("On the other hand, a small/gentle slope will correspond to a weak relationship while a large/steep slope will corresponds to a strong relationship.")
SIMPLELOGPOWER$pots.text <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.text, stylename="Normal", 
                   par.properties=parProperties(text.align="justify", padding=10))

SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.text <- NULL 
## doc = addParagraph(doc, value=" ", stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- pot("Recall that all response and predictor variables utilized in this report represent log-transformed values of abundance. ") + 
                       pot("Specifically, the response variable represents log-transformed abundance at Age k, ") +
                       pot("while the predictor variable represents log-transformed abundance at Age k-1.")  
SIMPLELOGPOWER$pots.text <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.text, stylename="Normal", 
                   par.properties=parProperties(text.align="justify",padding=10))


SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.text <- NULL 
## doc = addParagraph(doc, value=" ", stylename="Normal")

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
SIMPLELOGPOWER$pots <- pot("All effect displays included in this report are accompanied by 95% (pointwise) confidence bands, which are derived by exploiting the fact that ") +
        pot(" effect displays are collections of fitted values.  This makes it straightforward to estimate the standard errors of the effects. ") +
        pot("The width of the 95% confidence band associated with an effect plot reflects the precision of estimation of the effect displayed in the plot. ") +
        pot("The narrower this width, the more precise the estimation. ")
SIMPLELOGPOWER$pots.text <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.text, stylename="Normal", 
                   par.properties=parProperties(text.align="justify",padding=10))

SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.text <- NULL 
## doc = addParagraph(doc, value=" ", stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <-  pot("The effect displays in this report also include residuals (for models with a single predictor) ") +
        pot("or partial residuals (for models with multiple predictors).")
SIMPLELOGPOWER$pots.text <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.text, stylename="Normal", 
                    par.properties=parProperties(text.align="justify", padding=10))


SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.text <- NULL 
## doc = addParagraph(doc, value=" ", stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

## doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.text, stylename="Normal")


## if (plots==1){

for (i in 1:length(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower)){

    doc = addPageBreak(doc)

    SIMPLELOGPOWER$current_age <- all.vars(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower[[i]]$formula)[1]
    
    usePackage("stringr")
    SIMPLELOGPOWER$current_age <- str_replace(SIMPLELOGPOWER$current_age,"_"," ")
    SIMPLELOGPOWER$current_age <- tolower(SIMPLELOGPOWER$current_age)

    SIMPLELOGPOWER$plotlegend <- paste0("Effect display for the simple log power regression model corresponding to the ",
                         SIMPLELOGPOWER$current_age, " component of the ",
                         tolower(SIMPLELOGPOWER$stockabundance), " ",
                         "for the ",
                         SIMPLELOGPOWER$stockname, " ",
                         SIMPLELOGPOWER$stockspecies, " ",
                         "stock.")


    SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$my.effect.plot.multiple.predictor.simplelogpower(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower, 
                                                                                             SIMPLELOGPOWER$stockabundance, i)

    usePackage("formula.tools")
    SIMPLELOGPOWER$vars.myplot <- rhs.vars(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower[[i]]$formula)


    if (length(SIMPLELOGPOWER$vars.myplot) > 1) { 
    
        doc = addPlot(doc,
                      fun=plot, # print,
                      x=SIMPLELOGPOWER$myplot,
                      width=plotwidth, 
                      height=plotheight)
    
    } else if (length(SIMPLELOGPOWER$vars.myplot) == 1) {
    
         doc = addPlot(doc,
                      fun=plot, # print,
                      x=SIMPLELOGPOWER$myplot,
                      width=plotwidth, 
                      height=plotheight-2)
    
    }

    doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

    ## rm(myplot)
    
    SIMPLELOGPOWER$myplot <- NULL 
    SIMPLELOGPOWER$vars.myplot <- NULL
    SIMPLELOGPOWER$plotlegend <- NULL  

}

## }

#=========================================================================================================
# Model Diagnostics
#=========================================================================================================

## Residuals vs. Fitted Values: Best Fitting Models

# Mamic

## best.fits.simplelogpower

doc = addPageBreak(doc)

doc = addTitle(doc, paste("Model Diagnostics for the Simple Log Power Regression Models"), level=1)


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("When fitting a simple log power regression model to a particular data set, many problems may occur. ",
                    "Most common among these are the following:")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- c("   1. Non-linearity of the relationship(s) between the (log-transformed) response and (log-transformed) predictors;",
              "   2. Correlation of the model errors;",
              "   3. Non-constant variance of the error terms;",
              "   4. Non-normality of error terms;",
              "   5. Outliers;",
              "   6. High-leverage points.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$sometext, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Identifying and overcoming these problems is as much an art as it is a science.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("For each of the older age components, ",
                    "the corresponding simple log power regression model fits the underlying data well ",
                    "if the observations in the plot of the residuals versus the fitted values are randomly scattered ",
                    "about the horizontal line going through zero. ",
                    "Any systematic pattern seen in this plot (e.g., funnel shape, non-linear shape) ",
                    "and/or any unusual features (e.g., outliers, gaps) ",
                    "are indicative of an inadequate model fit and warrant further investigation.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

##
## Correlation of Model Errors
##

SIMPLELOGPOWER$pots <- pot("Correlation of Model Errors", textProperties(font.weight="bold", font.style="italic"))
SIMPLELOGPOWER$pots.par <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.par, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- NULL
SIMPLELOGPOWER$pots.par <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("An important assumption for log power regression models is that their error terms are uncorrelated. What does this mean? For example, if",
                    " the errors are uncorrelated, then the fact that the error term corresponding to one of the model observations is positive provides little or no information about the sign",
                    " of the error term corresponding to the next model observation.",
                    " If the error terms are correlated and we ignore this at the modeling and forecasting stage, we may have an unwarranted sense of confidence in the results produced by the model.",
                    " In particular, prediction intervals may be narrower than they should be.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Correlations among the model errors tend to occur when the data utilized in the model were collected over time (e.g., on an annual basis).")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("In order to detect the presence of correlation among the errors associated with a log power regression model, we need to plot the residuals from the model as a function of time, ",
                    "obtaining a so-called time series plot of residuals. If the model errors are uncorrelated, then there should be no discernable pattern in this plot. ",
                    "On the other hand, if the residuals are positively correlated, then we may see tracking in the residuals - that is, adjacent residuals may have similar values.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Other graphical tools for detecting correlation among the errors associated with a log power regression model include the autocorrelation and partial autocorrelation plots. ",
                    "If the errors are uncorrelated, all of the autocorrelations and partial autocorrelations displayed in these plots would be expected to lie within the ",
                    " 95% confidence bands. ",
                    "The following table provides further guidance on how to interpret the autocorrelation and partial autocorrelation plots corresponding",
                    " to various underlying processes that may have generated the errors.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

doc = addPageBreak(doc)

SIMPLELOGPOWER$cortable <- matrix(NA, nrow=5, ncol=3)
SIMPLELOGPOWER$cortable[1,] <- c("Nonstationary",
                  "Autocorrelations do not die out; they remain large or diminish approximately linearly","")
SIMPLELOGPOWER$cortable[2,] <- c("Stationary",
                  "After the first few lags, autocorrelations die out (i.e., they collapse toward 0 in some combination of exponential decay or damped oscillation)",
                  "")

SIMPLELOGPOWER$cortable[3,] <- c("AR(p)",
                  "Autocorrelations die out",
                  "Partial autocorrelations cut off after the first p lags")

SIMPLELOGPOWER$cortable[4,] <- c("MA(q)",
                  "Autocorrelations cut off after the first q lags",
                  "Partial autocorrelations die out")

SIMPLELOGPOWER$cortable[5,] <- c("ARMA(p,q)",
                  "Autocorrelations die out after first q-p lags",
                  "Partial autocorrelations die out after first p-q lags")

SIMPLELOGPOWER$cortable <- as.data.frame(SIMPLELOGPOWER$cortable)

names(SIMPLELOGPOWER$cortable) <- c("Process","Autocorrelation Function","Partial Autocorrelation Function")

doc = addParagraph(doc, value=paste("Guidelines for interpreting the autocorrelation and partial autocorrelation functions."), stylename="rTableLegend")

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

# Create a FlexTable with data.frame dataset
SIMPLELOGPOWER$MyFTable = FlexTable( data = SIMPLELOGPOWER$cortable,
                      body.cell.props = baseCellProp,
                      header.cell.props = baseCellProp,
                      header.par.props = parProperties(text.align = "left" )
)


doc = addFlexTable(doc, SIMPLELOGPOWER$MyFTable)


paragraph <- paste0(" ")
doc = addParagraph(doc, value=paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 
SIMPLELOGPOWER$MyFTable <- NULL 
SIMPLELOGPOWER$cortable <- NULL 

##                                                                     
SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
## Non-Constant Variance of Error Terms
##

SIMPLELOGPOWER$paragraph <- paste0(" ")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL

SIMPLELOGPOWER$pots <- pot("Non-Constant Variance of Error Terms", textProperties(font.weight="bold", font.style="italic"))
SIMPLELOGPOWER$pots.par <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.par, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.par <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$paragraph <- paste0("Another important assumption for log power regression models is that their error terms have a constant variance. ",
                    "However, in practice it is often the case that the variances of the error terms are non-constant.  For instance, ",
                    "the variances of the error terms may increase with the value of the response variable included in the model.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("We can identify non-constant variances in the errors, or heteroscedasticity, from the presence of a funnel shape in the plot of residuals versus fitted values.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

## paragraph <- paste0("When faced with the problem of heteroscedasticity, one possible solution is to log-transform the response variable.  Such a transformation ",
##                    "tends to result in a greater amount of shrinkage of the larger responses, leading to a reduction in heteroscedasticity.")
##doc = addParagraph(doc, value=paragraph, stylename="Normal")


SIMPLELOGPOWER$pots <- pot("Non-Normality of Error Terms", textProperties(font.weight="bold", font.style="italic"))
SIMPLELOGPOWER$pots.par <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.par, stylename="Normal", 
                  par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.par <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("When the error terms in a log power regression model are not normally distributed, least squares estimates produced by the model ",
                    "may not be optimal.  They will still be best linear unbiased estimates, but other robust estimators may be more effective. ",
                    "Also, the p-values of the tests of statistical significance of the effects represented in the model are no longer exact. ")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Non-normality of the model errors can be diagnosed by examining histogram and density plots of the model residuals along with",
                    " normal probability plots of the model residuals.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("When non-normality of the model errors is found, the resolution depends on the type of problem found:")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- c("For short-tailed error distributions, the consequences of non-normality are not serious and can reasonably be ignored.",
           "For skewed error distributions, a transformation of the response may solve the problem.",
           "For long-tailed error distributions, we might just accept the non-normality and base the inference and/or prediction on resampling methods such as the bootstrap."
           )
doc = addParagraph( doc, value = SIMPLELOGPOWER$sometext, stylename="BulletList" )


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- pot("Outliers", textProperties(font.weight="bold", font.style="italic"))
SIMPLELOGPOWER$pots.par <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.par, stylename="Normal", 
                  par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("In a log power regression model, an outlier is a regression observation whose corresponding response value is far from the ",
                    "value predicted by the model.  Observations deemed to be outliers need to be investigated further as they have the potential to influence the ",
                    "model fit. ",
                    "(An observation is said to influence the model fit if omitting that observation from the model significantly changes the model fit.)")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("To identify outliers, we can use the studentized residuals.  Studentized residuals are computed by dividing each raw residual by its estimated standard error.",
                     " Observations whose studentized residuals are ",
                    "greater than 3 in absolute value are possible outliers.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- NULL


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- pot("High-Leverage Observations", textProperties(font.weight="bold", font.style="italic"))
SIMPLELOGPOWER$pots.par <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.par, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- NULL 
SIMPLELOGPOWER$pots.par <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("In a log power regression model, high-leverage observations are regression observations that are relatively far from the center of the predictor space,",
                    " taking into account the correlational pattern among the predictors. ",
                    " Such observations could have a large impact on the regression model fit so they need to be identified and investigated further.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("High leverage observations can be identified on the basis of the index plot of leverage (or hat) values. Any observation whose reported leverage value ",
                    "exceeds twice the average leverage of all observations is deemed to have high-leverage. ",
                    "(The leverage value quantifies how extreme an observation is in the predictor space when compared to all other observations.)")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$pots <- pot("Influence", textProperties(font.weight="bold", font.style="italic"))
SIMPLELOGPOWER$pots.par <- set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, value=SIMPLELOGPOWER$pots.par, stylename="Normal", 
                  par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("A log power regression model observation that is both outlying and has high leverage exerts influence on the regression coefficients, in the ",
                    "sense that if the observation is removed, the coefficients change considerably.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$paragraph <- paste0("The most common measure of influence of an observation is Cook's distance, expressed as a product of two factors. ",
                    "The first factor represents a measure of the outlyingness of the observation and the second factor represents a measure of leverage of the observation. ",
                    "Observations with a large Cook's distance are potentially influential cases.  In this report, we deem a Cook's distance value to be large if ",
                    "it exceeds the threshold 4/n, where n is the number of observations included in the model.")
doc = addParagraph(doc, value=SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

doc = addPageBreak(doc)

## if (plots==2){


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}


SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.residuals.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)

doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)
            
doc = addParagraph(doc, value=paste("Plots of residuals versus fitted values corresponding to the simple linear log power regression models."), stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 


## }

## Histogram of Residuals: Best Fitting Models

## if (plots==3){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.histresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)

doc = addParagraph(doc, value=paste("Histograms of residuals corresponding to the simple log power regression models."), stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 

## rm(myplot)

## }

## Density Plot of Residuals: Best Fitting Models

## if (plots==4){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.densresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)

doc = addParagraph(doc, value=paste("Density plots of residuals corresponding to the simple log power regression models."), stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 

## rm(myplot)

## }

## Time Series Plots of Residuals: Best Fitting Models

## if (plots==5){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.timeresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}


doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)

doc = addParagraph(doc, value=paste("Time series plots of residuals corresponding to the simple log power regression models."), stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 

## rm(myplot)

## }

## ACF Plots of Residuals: Best Fitting Models


## if (plots==6){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.acfresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}


doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)

doc = addParagraph(doc, value=paste("ACF plots of residuals corresponding to the simple log power regression models."), stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 

## rm(myplot)

## }

## PACF Plots of Residuals: Best Fitting Models

## if (plots==7){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.pacfresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)

doc = addParagraph(doc, value=paste("PACF plots of residuals corresponding to the simple log power regression models."), stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 

## rm(myplot)

## }

## Index Plots of Studentized Residuals: Best Fitting Models

## if (plots==8){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.studentresid.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)

doc = addParagraph(doc, value=paste("Index plots of studentized residuals corresponding to the simple log power regression models."), stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL

## rm(myplot)

## }

## Index Plots of Leverage Values: Best Fitting Models

## if (plots==9){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.hatvalues.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)

doc = addParagraph(doc, value=paste("Index plots of leverage values corresponding to the simple log power regression models."), stylename="rPlotLegend")

## rm(SIMPLELOGPOWER$myplot)

## }

## Index Plots of Cook's Distances: Best Fitting Models

## if (plots==10){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.cooks.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth+1, 
            height=plotheight.tmp)

doc = addParagraph(doc, value=paste("Index plots of Cook's distances corresponding to the simple log power regression models."), stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 

## rm(myplot)

## }


## Influence Plots:  Best Fitting Models

## if (plots==11){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.cooks.bubble.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower)


if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==4) {
   plotheight.tmp <- plotheight + 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==3) {
   plotheight.tmp <- plotheight 
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==2) {
   plotheight.tmp <- plotheight - 1
} else if (length(SIMPLELOGPOWER$best.fits.simplelogpower)==1) {
   plotheight.tmp <- plotheight - 2
}

doc = addPlot(doc,
            fun=print,
            x=SIMPLELOGPOWER$myplot,
            width=plotwidth, 
            height=plotheight.tmp)
            
doc = addParagraph(doc, value=paste("Influence plots corresponding to the simple log power regression models."), stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 

## rm(myplot)

## }

#=========================================================================================================
# Modeling: Youngest Age  (RETURN HERE)
#=========================================================================================================

doc = addPageBreak(doc)

addTitle(doc, "Modeling Results for the Youngest Age Component", level=1)


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("The log power regression methodology cannot be used to produce the ",
                     SIMPLELOGPOWER$forecastingyear, " ",
                     "forecast of ",
                     tolower(SIMPLELOGPOWER$stockabundance), " for the ",
                     SIMPLELOGPOWER$youngest.age.simplelogpower,
                     " component of the ",
                     SIMPLELOGPOWER$stockname, " ",
                     SIMPLELOGPOWER$stockspecies, " stock. ",
                    "To obtain this forecast,",
                    " we need to use a different methodology, which is outlined below.")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("First, we fit three different models to the ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " time series available for the ",
                    SIMPLELOGPOWER$youngest.age.simplelogpower, " component of the stock: ",
                    "1) a naive time series model (i.e., average of previous 5 years), ",
                    "2) an ARIMA time series model and ",
                    "3) an exponential smoothing model.")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("We use automatic model selection based on the Akaike's Information Criterion (AIC), corrected for small sample bias, to determine the optimal ARIMA model for the ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " series corresponding to ", SIMPLELOGPOWER$youngest.age.simplelogpower, ".")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
SIMPLELOGPOWER$paragraph <- paste0("In a similar fashion, we use Akaike's Information Criterion (AIC), corrected for small sample bias,",
                    " to select the optimal exponential smoothing model for the ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " series corresponding to ", SIMPLELOGPOWER$youngest.age.simplelogpower, ".")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Second, we use each of the three models to compute retrospective point forecasts of ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " for all of the historical return years preceding the forecasting year ",
                    SIMPLELOGPOWER$forecastingyear,
                    " except for the first ", 
                    SIMPLELOGPOWER$index.year, 
                    " historical return years.", sep="")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Third, for each model, we add the",
                    " retrospective point forecasts of ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " for ", SIMPLELOGPOWER$youngest.age.simplelogpower, " ",
                    " to the retrospective point forecasts of ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " for the older ages which were produced by log power regression. ",
                    "This enables us to compute retrospective point forecasts of total ",
                    tolower(SIMPLELOGPOWER$stockabundance)," for all of the historical return years preceding the forecasting year ",
                    SIMPLELOGPOWER$forecastingyear,
                    " except for the first ten historical return years.",
                    " Comparing these retrospective point forecasts against the actual total ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " values yields the retrospective forecast errors.")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Fourth, we use the retrospective point forecasts of total ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " along with the accompanying ",
                    "retrospective forecast errors ",
                    "to compute the RMSE measure corresponding to each model.")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Finally, we compare the three models based on the values of the RMSE measure and retain the model with the lowest value for this measure.")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- NULL 

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
                   
##
## Fitted Values for Candidate Models for Youngest Age:
##

## if (plots==12){

SIMPLELOGPOWER$figurecaption <- paste0("Fitted values produced by the three candidate model used for forecasting ",
                         "the youngest age component of the ", SIMPLELOGPOWER$forecastingyear, " ",
                         tolower(SIMPLELOGPOWER$stockabundance),
                         " for the ",
                         SIMPLELOGPOWER$stockname, " ",
                         SIMPLELOGPOWER$stockspecies,
                         " stock.")

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.fitted.value.youngest(SIMPLELOGPOWER$avgfive.model.fit.youngest,
                           SIMPLELOGPOWER$arima.model.fit.youngest,
                           SIMPLELOGPOWER$expsmooth.model.fit.youngest, 
                           SIMPLELOGPOWER$boxcoxtransform)

doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLELOGPOWER$myplot,
        width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 

## }

##
## Model Fit for Youngest Age: ARIMA
##


isEmpty <- function(x) {return(identical(x, numeric(0)))}

SIMPLELOGPOWER$extract.arima <- function(model){

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


SIMPLELOGPOWER$extract.arima.model.fit.youngest <- SIMPLELOGPOWER$extract.arima(SIMPLELOGPOWER$arima.model.fit.youngest$model)


## Marley


doc = addPageBreak(doc)


doc = addParagraph(doc, value=paste("ARIMA modeling results for the youngest age."), stylename="rTableLegend")

## SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.table
## SIMPLELOGPOWER$tt.1 <-  apply(SIMPLELOGPOWER$tt,2,as.character)
## SIMPLELOGPOWER$tt.2 <- rbind.data.frame(colnames(SIMPLELOGPOWER$tt), SIMPLELOGPOWER$tt.1)
## names(SIMPLELOGPOWER$tt.2) <- names(SIMPLELOGPOWER$tt)
## SIMPLELOGPOWER$tt.2
## SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$tt.2
## options(stringsAsFactors=TRUE)

## SIMPLELOGPOWER$

options(stringsAsFactors=FALSE)

if (!is.null(SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.table)) {
   SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.table 
   SIMPLELOGPOWER$tt.1 <-  apply(SIMPLELOGPOWER$tt,2,as.character)
   
   SIMPLELOGPOWER$tt.2 <- rbind.data.frame(colnames(SIMPLELOGPOWER$tt), SIMPLELOGPOWER$tt.1)

   names(SIMPLELOGPOWER$tt.2) <- names(SIMPLELOGPOWER$tt)
   SIMPLELOGPOWER$tt.2
   
   SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$tt.2 
   
} else {

   SIMPLELOGPOWER$tt <- do.call(rbind, lapply(SIMPLELOGPOWER$extract.arima.model.fit.youngest, data.frame, stringsAsFactors=FALSE))
   
   SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$tt[c("extract.arima.fit.model", 
              "extract.arima.fit.sigma2", 
              "extract.arima.fit.loglik", 
              "extract.arima.fit.aic",
              "extract.arima.fit.aicc", 
              "extract.arima.fit.bic"), ] 

   
   SIMPLELOGPOWER$tt[2] <- round(sqrt(as.numeric(SIMPLELOGPOWER$tt[2])),4)  # compute square root of "extract.arima.fit.sigma2"
   
   SIMPLELOGPOWER$tt <- cbind.data.frame(c("Model", 
                            "Estimated sigma", 
                            "Log-likelihood", 
                            "AIC", 
                            "AICc", 
                            "BIC"),SIMPLELOGPOWER$tt)


    SIMPLELOGPOWER$tt <- data.frame(lapply(SIMPLELOGPOWER$tt, as.character), stringsAsFactors=FALSE)
    

   colnames(SIMPLELOGPOWER$tt) <- NULL 
   
   for (k in 2:nrow(SIMPLELOGPOWER$tt)) {
   
        SIMPLELOGPOWER$tt[k,2] <- round(as.numeric(SIMPLELOGPOWER$tt[k,2]),4)
   }
   
}



options(stringsAsFactors=TRUE)


# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )
# Create a FlexTable with tt data frame
SIMPLELOGPOWER$my_ft = FlexTable(data = SIMPLELOGPOWER$tt,
                  body.cell.props = baseCellProp,
                  header.cell.props = baseCellProp,
                  header.par.props = parProperties(text.align = "center"),
                  add.rownames = FALSE,
                  header.columns= FALSE)
                  
if (!is.null(SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.table)) { 

    
    SIMPLELOGPOWER$my_ft = addHeaderRow(SIMPLELOGPOWER$my_ft, 
                     value=paste0(SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.model),
                     colspan=ncol(SIMPLELOGPOWER$tt),
                     text.properties=textBold())

    SIMPLELOGPOWER$my_ft = addFooterRow(SIMPLELOGPOWER$my_ft, 
                     value=paste0("sigma estimated as ",round(sqrt(SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.sigma2),4)),
                     colspan=ncol(SIMPLELOGPOWER$tt),
                     text.properties=textItalic())


    my_ft = addFooterRow(SIMPLELOGPOWER$my_ft, 
                     value=paste0("log-likelihood = ",round(SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.loglik,4)),
                     colspan=ncol(SIMPLELOGPOWER$tt),
                     text.properties=textItalic())

    my_ft = addFooterRow(SIMPLELOGPOWER$my_ft, 
                     value=paste0("AIC = ",round(SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.aic,4)),
                     colspan=ncol(SIMPLELOGPOWER$tt),
                     text.properties=textItalic())

    my_ft = addFooterRow(my_ft, value=paste0("AICc = ",round(SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.aicc,4)),
                     colspan=ncol(SIMPLELOGPOWER$tt),
                     text.properties=textItalic())

    my_ft = addFooterRow(my_ft, value=paste0("BIC = ",round(SIMPLELOGPOWER$extract.arima.model.fit.youngest$extract.arima.fit.bic,4)),
                     colspan=ncol(SIMPLELOGPOWER$tt),
                     text.properties=textItalic())

}


# overwrites some text formatting properties
## my_ft[, 1] = parProperties(font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, background.color = "lightgrey")

SIMPLELOGPOWER$my_ft[1, ] = textProperties(color="black", font.weight = "bold" )
# my_ft[, 2] = textProperties(color="black", font.weight = "bold" )


doc = addFlexTable(doc, SIMPLELOGPOWER$my_ft)


doc = addParagraph(doc, value=paste(" "), stylename="Normal")

#===============================================================================



##
## Model Fit for Youngest Age: Exponential Smoothing
##

SIMPLELOGPOWER$expsmoothfit <- SIMPLELOGPOWER$expsmooth.model.fit.youngest
sink("expsmoothfit.txt")
print(SIMPLELOGPOWER$expsmoothfit)
sink()
SIMPLELOGPOWER$out <- readLines("expsmoothfit.txt")
usePackage("stringr")
SIMPLELOGPOWER$out.pattern <- str_detect(string=SIMPLELOGPOWER$out, pattern="ETS")
SIMPLELOGPOWER$modelexpsmooth <- SIMPLELOGPOWER$out[SIMPLELOGPOWER$out.pattern==TRUE]
require(stringr)
SIMPLELOGPOWER$modelexpsmooth <- str_trim(SIMPLELOGPOWER$modelexpsmooth)
SIMPLELOGPOWER$model_desc <- SIMPLELOGPOWER$modelexpsmooth
SIMPLELOGPOWER$model_fit <- SIMPLELOGPOWER$out

SIMPLELOGPOWER$hadley <- NULL
for (k in 1:length(SIMPLELOGPOWER$model_fit)){
  SIMPLELOGPOWER$hadley <- c(SIMPLELOGPOWER$hadley,
              sum(unlist(str_locate( SIMPLELOGPOWER$model_fit[k], "original.data")))
              )
}
SIMPLELOGPOWER$hadley <- ifelse(!is.na(SIMPLELOGPOWER$hadley), 1, 0)
SIMPLELOGPOWER$hadley.index <- which(SIMPLELOGPOWER$hadley==1) - 2
SIMPLELOGPOWER$model_fit_expsmooth_youngest <- SIMPLELOGPOWER$model_fit[6:SIMPLELOGPOWER$hadley.index]

doc = addParagraph(doc, value=paste("Exponential smoothing modeling results for the youngest age."), stylename="rTableLegend")

SIMPLELOGPOWER$tt <- data.frame(SIMPLELOGPOWER$model_fit_expsmooth_youngest)

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )
# Create a FlexTable with tt data frame
SIMPLELOGPOWER$my_ft = FlexTable(data = SIMPLELOGPOWER$tt,
                  body.cell.props = baseCellProp,
                  header.cell.props = baseCellProp,
                  header.par.props = parProperties(text.align = "center"),
                  add.rownames = FALSE)

# overwrites some text formatting properties
## my_ft[, 1] = parProperties(font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, background.color = "lightgrey")

SIMPLELOGPOWER$my_ft[, 1] = textProperties(color="black", font.weight = "bold" )


doc = addFlexTable(doc, SIMPLELOGPOWER$my_ft)


## Sanda


##
## Results Produced by Candidate Models for Youngest Age
##


doc = addPageBreak(doc)

## doc = addParagraph(doc, value=paste(" "), stylename="Normal")

## COME BACK HERE!!!!!

doc = addTitle( doc, "Results Produced by the Candidate Models for the Youngest Age", level = 2)


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("The results produced by the candidate models considered for forecasting ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    " for the youngest age component of the ",
                    SIMPLELOGPOWER$stockname,
                    " stock ",
                    "(i.e., ",
                    SIMPLELOGPOWER$youngest.age.simplelogpower,
                    ") ",
                    "are provided below. ",
                    "They include the ",
                    SIMPLELOGPOWER$forecastingyear, " ",
                    "point forecast and interval forecast for the ",
                    SIMPLELOGPOWER$youngest.age.simplelogpower, " ",
                    tolower(SIMPLELOGPOWER$stockabundance)," ",
                    "produced by each model, as well as the ",
                    "RMSE value obtained by retrospectively forecasting the total ",
                    tolower(SIMPLELOGPOWER$stockabundance),".")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <-  paste0("The retrospective forecasts of total ",
                    tolower(SIMPLELOGPOWER$stockabundance), " utilized in the computation of the reported RMSE values",
                    " were obtained by combining the retrospective forecasts of ",
                    SIMPLELOGPOWER$youngest.age.simplelogpower, " ", tolower(SIMPLELOGPOWER$stockabundance), " ",
                    "produced by the candidate models",
                    " with the retrospective forecasts produced by the simple log power regression models for the older age components.")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$best.model.youngest.age.simplelogpower <- NULL
SIMPLELOGPOWER$best.model.youngest.age.simplelogpower[SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$index.min.rmse.total.age==1] <- "naive model based on the average of the previous five years"
SIMPLELOGPOWER$best.model.youngest.age.simplelogpower[SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$index.min.rmse.total.age==2] <- "ARIMA model"
SIMPLELOGPOWER$best.model.youngest.age.simplelogpower[SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$index.min.rmse.total.age==3] <- "exponential smoothing model"


doc = addParagraph(doc, value=paste("Results produced by the candidate models considered for the youngest age component."), stylename="rTableLegend")

SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$table.results.individual.ages.all.models.youngest

usePackage("scales")
SIMPLELOGPOWER$tt$RMSE <- comma(SIMPLELOGPOWER$tt$RMSE)

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )
# Create a FlexTable with tt data frame
SIMPLELOGPOWER$my_ft = FlexTable(data = SIMPLELOGPOWER$tt,
                  body.cell.props = baseCellProp,
                  header.cell.props = baseCellProp,
                  header.par.props = parProperties(text.align = "center"),
                  add.rownames = FALSE
)

# overwrites some text formatting properties
## my_ft[, 1] = parProperties(font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, font.weight = "bold")
## my_ft[, 1] = chprop(baseCellProp, background.color = "lightgrey")

SIMPLELOGPOWER$my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

## colour the row corresponding to the best model for youngest age
SIMPLELOGPOWER$my_ft[SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$index.min.rmse.total.age, ] = cellProperties(background.color = "yellow")



doc = addFlexTable(doc, SIMPLELOGPOWER$my_ft)

doc = addParagraph(doc, value=" ", stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- paste0("When comparing the candidate models for the youngest age in terms of their associated RMSE values, ",
                    "it emerges that the best model identified for this age ",
                    "is the ",
                    SIMPLELOGPOWER$best.model.youngest.age.simplelogpower,". ",
                    "This particular model leads to the lowest RMSE value when used in combination with the simple log power regression models for the older age components ",
                    "in order to forecast total ",
                    tolower(SIMPLELOGPOWER$stockabundance),
                    ".")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

## if (plots==13){

SIMPLELOGPOWER$figurecaption <- paste0("Model diagnostics for the naive model (i.e., average of previous five years) considered for forecasting ",
                         "the youngest age component of the ", SIMPLELOGPOWER$forecastingyear, " ",
                         tolower(SIMPLELOGPOWER$stockabundance),
                         " for the ",
                         SIMPLELOGPOWER$stockname, " ",
                         SIMPLELOGPOWER$stockspecies,
                         " stock.")

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$diagnostics.avgfive.model.fit.youngest.age(SIMPLELOGPOWER$avgfive.model.fit.youngest)

doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLELOGPOWER$myplot,
        width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)


SIMPLELOGPOWER$figurecaption <- NULL 
SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$my_ft <- NULL 

## }


## if (plots==14){

SIMPLELOGPOWER$figurecaption <- paste0("Model diagnostics for the ARIMA model considered for forecasting ",
                         "the youngest age component of the ", SIMPLELOGPOWER$forecastingyear, " ",
                         tolower(SIMPLELOGPOWER$stockabundance),
                         " for the ",
                         SIMPLELOGPOWER$stockname, " ",
                         SIMPLELOGPOWER$stockspecies,
                         " stock.")

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$diagnostics.arima.model.fit.youngest.age(SIMPLELOGPOWER$arima.model.fit.youngest)

doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLELOGPOWER$myplot,
        width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 

## }


## if (plots==15){

SIMPLELOGPOWER$figurecaption <- paste0("Model diagnostics for the exponential smoothing model considered for forecasting ",
                         "the youngest age component of the ", SIMPLELOGPOWER$forecastingyear, " ",
                         tolower(SIMPLELOGPOWER$stockabundance),
                         " for the ",
                         SIMPLELOGPOWER$stockname, " ",
                         SIMPLELOGPOWER$stockspecies,
                         " stock.")

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$diagnostics.expsmooth.model.fit.youngest.age(SIMPLELOGPOWER$expsmooth.model.fit.youngest)

doc = addPlot(doc,
        fun = plot, # print,
        x = SIMPLELOGPOWER$myplot,
        width=plotwidth, 
        height=plotheight)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 

## }

## Stats Tutorial

doc = addPageBreak(doc)

SIMPLELOGPOWER$pots = pot("Stats Tutorial:", textProperties(font.weight = "bold"))
SIMPLELOGPOWER$pars = set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, SIMPLELOGPOWER$pars, stylename = "Normal")

SIMPLELOGPOWER$pots <- NULL
SIMPLELOGPOWER$pars <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots = pot("After fitting the naive, ARIMA and exponential smoothing models considered for forecasting ") +
       pot(paste("the youngest age component of the", SIMPLELOGPOWER$forecastingyear, tolower(SIMPLELOGPOWER$stockabundance))) +
       pot(paste("for the", SIMPLELOGPOWER$stockname,SIMPLELOGPOWER$stockspecies,"stock, ")) +
       pot("we need to validate the model which emerged as best on the basis of the RMSE. ") +
       pot("We can do this by using diagnostic tests based on the residuals associated with the best model.")
SIMPLELOGPOWER$pars = set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, SIMPLELOGPOWER$pars, stylename = "Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$pots <- NULL
SIMPLELOGPOWER$pars <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots = pot("If the best model provides a good fit to the underlying data, ") +
       pot("then its residuals should exhibit no systematic patterns and no temporal dependence.")
SIMPLELOGPOWER$pars = set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, SIMPLELOGPOWER$pars, stylename = "Normal", 
                  par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$pots <- NULL
SIMPLELOGPOWER$pars <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$pots = pot("Useful diagnostic plots for verifying that the best model residuals exhibit no systematic patterns and no temporal dependence include:")
SIMPLELOGPOWER$pars = set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, SIMPLELOGPOWER$pars, stylename = "Normal", 
                   par.properties=parProperties(text.align="justify"))
                   

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$texts = c("Time series plot of the model residuals;",
          "Autocorrelation plot of the model residuals;",
          "Partial autocorrelation plot of the model residuals;",
          "Plot of p-values associated with the Ljung-Box test applied to the model residuals.")
# add texts with stylename BulletList
doc = addParagraph( doc, value = SIMPLELOGPOWER$texts, stylename="BulletList")

SIMPLELOGPOWER$texts <- NULL


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots = pot("The Ljung-Box test is a diagnostic tool used to test the lack of fit of the best model. ") +
       pot("The test is applied to the model residuals and examines the first ") +
       pot("m ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("autocorrelations of the residuals. ") +
       pot("If all of these autocorrelations are very small, we conclude that the model does not exhibit significant lack of fit.")
SIMPLELOGPOWER$pars = set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, SIMPLELOGPOWER$pars, stylename = "Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$pots <- NULL
SIMPLELOGPOWER$pars <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots = pot("The Ljung-Box test tests the following hypotheses ") +
       pot("Ho: The model does not exhibit lack of fit ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("versus ") +
       pot("Ha: The model exhibits lack of fit. ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("Small p-values for the Ljung-Box test lead to the rejection of the alternative hypothesis, suggesting that the model exhibits significant lack of fit. ") +
       pot("Conversely, large p-values suggest that the model does not exhibit significant lack of fit.")
SIMPLELOGPOWER$pars = set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, SIMPLELOGPOWER$pars, stylename = "Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- NULL
SIMPLELOGPOWER$pars <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$pots = pot("Since the choice of ") +
       pot("m ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("is important but somewhat arbitrary, ") +
       pot("in practice we perform the Ljung-Box test for several consecutive values of ") +
       pot("m ", textProperties(font.style = "italic", font.family="Calibri", font.size=11)) +
       pot("to see if the p-values it produces are large for all of these values. ") +
       pot("If they are, then we conclude that the model does not exhibit lack of fit.")
SIMPLELOGPOWER$pars = set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, SIMPLELOGPOWER$pars, stylename = "Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots <- NULL
SIMPLELOGPOWER$pars <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$pots = pot("If a naive model provides a good fit to a univariate time series, then:")
SIMPLELOGPOWER$pars = set_of_paragraphs(SIMPLELOGPOWER$pots)
doc = addParagraph(doc, SIMPLELOGPOWER$pars, stylename = "Normal", 
                  par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$pots <- NULL
SIMPLELOGPOWER$pars <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


SIMPLELOGPOWER$texts = c("The time series plot of the model residuals should exhibit no systematic patterns;",
          "The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals;",
          "The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;",
          "The p-values associated with the Ljung-Box test should be large for all values of m considered.")
doc = addParagraph( doc, value = SIMPLELOGPOWER$texts, stylename="BulletList", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$texts <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

#=========================================================================================================
# Forecasting Results
#=========================================================================================================

doc = addPageBreak(doc)

addTitle(doc, "Forecasting Results", level=1)


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste("This section reports the forecasting results for the",
                   SIMPLELOGPOWER$stockname, SIMPLELOGPOWER$stockspecies,
                  "stock corresponding to the forecasting year",
                   paste0(SIMPLELOGPOWER$forecastingyear,"."),
                  "The results were produced via log power regression models.")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste("In what follows, forecasting results are reported numerically and visually for two types of forecasts:",
                  "1) point forecasts and 2) interval forecasts.")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")



SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste("A point forecast is simply a number which represents our best guess of the future value of the age-specific or total",
                   tolower(SIMPLELOGPOWER$stockabundance),
                  "for the stock of interest based on available historical data.")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste("An interval forecast is not a single number, rather it is a range of values in which we expect the future value of an age-specific or total",
                  SIMPLELOGPOWER$stockabundance,
                  "series to fall with some (prespecified) probability.")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste("A couple of remarks are in order in connection with an interval forecast:")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste("• The width of the interval forecast conveys information regarding forecast uncertainty",
                  " (the wider the interval forecast, the more uncertain the forecast);")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")


SIMPLELOGPOWER$sometext <- paste("• The interval forecast conveys more information than the associated point forecast.")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste0("Interval forecasts were obtained separately for the youngest age component and for the older age components. ", 
                   "All reported interval forecasts are 80% interval forecasts.")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")

SIMPLELOGPOWER$sometext <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste0("The computation of the interval forecast for the youngest age component of the stock depended on the nature of the time series model ", 
                   "identified as best for that component. ", 
                   "If the naive model (average of previous five years) emerged as best for the youngest age component, maximum entropy bootstrapping was used for computing ", 
                   "the interval forecast. ", 
                   "If either the ARIMA model or the exponential smoothing model emerged as best, loess bootstrapping was used instead to compute the ", 
                   "interval forecast.  The maximum entropy and loess bootstrapping methods are described briefly below.")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")

SIMPLELOGPOWER$sometext <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste0("The maximum entropy bootstrap refers to a method for bootstrapping dependent time series, which was introduced by ",
                   "Vinod and Lopez-de-Lacalle in 2009. In this method, the original time series of annual abundance values corresponding to the youngest age component of the stock ", 
                   "is used as a basis for creating B bootstrap replicates using an algorithm designed ", 
                   "to satisfy the ergodic theorem (i.e., the grand mean of all replicates is close to ", 
                   "the sample mean of the original time series). ", 
                   "The algorithm can accommodate both stationary and non-stationary time series. ", 
                   "The B bootstrap replicates retain the basic shape (i.e., local peaks and troughs) of the original time series. ",  
                   "They also retain the time dependence structure of the autocorrelation function (ACF) and the partial autocorrelation function (PACF) of the original time series.") 
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <- paste0("The loess bootstrap is a time series bootstrapping method introduced by Bergmeir, Hyndman and Benitez in 2014 ", 
                   "in their working paper on bagging exponential smoothing methods using the STL decomposition and the Box-Cox transformation. ", 
                   "In this method, the original time series of annual abundance values corresponding to the youngest age component of the stock is first transformed ", 
                   "via a Box-Cox transformation. The transformed time series is then decomposed into its trend and remainder components using the loess method ", 
                   "(i.e., a smoothing method based on local linear regression). ", 
                   "Finally, the remainder component is bootstrapped using the moving block bootstrap (MBB), the trend and seasonal components are added back, ", 
                   "and the Box-Cox transformation is inverted. In this way, a random pool of B similar bootstrapped time series is generated from the original time series.")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")

SIMPLELOGPOWER$sometext <- NULL

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$sometext <-  paste0("The computation of the interval forecast for the older age components of the stock relied on case resampling ",  
                    "(but will be extended in the future to allow for normal-theory driven interval forecasts).")
doc = addParagraph(doc, SIMPLELOGPOWER$sometext, stylename="Normal")

SIMPLELOGPOWER$sometext <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

doc = addPageBreak(doc)

addTitle(doc, "Point Forecasts", level=2)


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$table_point_forecasts_simplelogpower <- function(results_best_fitting_model_for_each_age_class_simplelogpower,  
                                                           forecastingyear, stockabundance){

    point_forecast_results <-
      SIMPLELOGPOWER$point.forecast.best.fitting.model.for.each.age.class.simplelogpower(results_best_fitting_model_for_each_age_class_simplelogpower,
                                                           forecastingyear, SIMPLELOGPOWER$datafile_variables)

    mytable <- matrix(NA, nrow=length(point_forecast_results), ncol=4)
    mytable <- as.data.frame(mytable)
    names(mytable)[-1] <- c("Best Model","Forecasting Year","Point Forecast")
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

    }

    usePackage("stringr")

    for (i in 1:length(point_forecast_results)){
       mytable[i,1] <- str_replace(mytable[i,1], "_", " ")
    }
    mytable

}

## Jeffie

SIMPLELOGPOWER$total.index.simplelogpower <- SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$index.min.rmse.total.age # index of best model used
                                                               # to forecast abundance for
                                                               # youngest age, where
                                                               # index is 1 for naive (avg. of 5 years),
                                                               #          2 for arima
                                                               #          3 for exponential smoothing



## pred.int.individual.ages.avgfive.youngest
## pred.int.individual.ages.arima.youngest
## pred.int.individual.ages.expsmooth.youngest

## usePackage("Hmisc")
SIMPLELOGPOWER$youngest.age.simplelogpower.capitalized <- capitalize(SIMPLELOGPOWER$youngest.age.simplelogpower)


if (SIMPLELOGPOWER$total.index.simplelogpower==1) {   # naive model for youngest age
SIMPLELOGPOWER$ttrow <- c(SIMPLELOGPOWER$youngest.age.simplelogpower.capitalized,
           "Naive Model (Average of Previous 5 Years)", paste(SIMPLELOGPOWER$forecastingyear),
           comma(round(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$PI.ctr)))
# c("Total","-",paste(forecastingyear),
#                              comma(pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]]))
}

if (SIMPLELOGPOWER$total.index.simplelogpower==2) {  # arima model for youngest age

    SIMPLELOGPOWER$arimafit <- SIMPLELOGPOWER$arima.model.fit.youngest
    SIMPLELOGPOWER$arimamodel <- SIMPLELOGPOWER$arimafit$model

    sink("arimafit.txt")
    print(SIMPLELOGPOWER$arimamodel)
    sink()

    SIMPLELOGPOWER$out <- readLines("arimafit.txt")
    usePackage("stringr")
    SIMPLELOGPOWER$out.pattern <- str_detect(string=SIMPLELOGPOWER$out, pattern="ARIMA")

    SIMPLELOGPOWER$modelarima <- SIMPLELOGPOWER$out[SIMPLELOGPOWER$out.pattern==TRUE]
    SIMPLELOGPOWER$modelarima <- str_trim(SIMPLELOGPOWER$modelarima)

    SIMPLELOGPOWER$ttrow <- c(SIMPLELOGPOWER$youngest.age.simplelogpower.capitalized,
               SIMPLELOGPOWER$modelarima, paste(SIMPLELOGPOWER$forecastingyear),
               comma(round(SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest$PI.ctr)))
}

if (SIMPLELOGPOWER$total.index.simplelogpower==3) { # exponential smoothing model for youngest age

    SIMPLELOGPOWER$expsmoothfit <- SIMPLELOGPOWER$expsmooth.model.fit.youngest
    SIMPLELOGPOWER$expsmoothmodel <- SIMPLELOGPOWER$expsmoothfit$model

    sink("expsmoothfit.txt")
    print(SIMPLELOGPOWER$expsmoothfit)
    sink()

    SIMPLELOGPOWER$out <- readLines("expsmoothfit.txt")
    usePackage("stringr")
    SIMPLELOGPOWER$out.pattern <- str_detect(string=SIMPLELOGPOWER$out, pattern="ETS")

    SIMPLELOGPOWER$modelexpsmooth <- SIMPLELOGPOWER$out[SIMPLELOGPOWER$out.pattern==TRUE]
    SIMPLELOGPOWER$modelexpsmooth <- str_trim(SIMPLELOGPOWER$modelexpsmooth)

    SIMPLELOGPOWER$ttrow <- c(SIMPLELOGPOWER$youngest.age.simplelogpower.capitalized,
               SIMPLELOGPOWER$modelexpsmooth, paste(SIMPLELOGPOWER$forecastingyear),
               comma(round(SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest$PI.ctr)))
}




SIMPLELOGPOWER$tt <- rbind.data.frame(SIMPLELOGPOWER$ttrow,
                       SIMPLELOGPOWER$table_point_forecasts_simplelogpower(
                            SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower,
                            SIMPLELOGPOWER$forecastingyear, 
                            SIMPLELOGPOWER$stockabundance)
                       )

## remove log (or log1p) transformations when reporting age class 
SIMPLELOGPOWER$tt[,1] <- gsub("\\(|\\)", "", x=SIMPLELOGPOWER$tt[,1])
SIMPLELOGPOWER$tt[,1] <- gsub(pattern="log|log1p", replacement="", x=SIMPLELOGPOWER$tt[,1])


## pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]]

SIMPLELOGPOWER$tt[nrow(SIMPLELOGPOWER$tt)+1,] <- c("Total","-",paste(SIMPLELOGPOWER$forecastingyear),
                              comma(SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models$p[[SIMPLELOGPOWER$total.index.simplelogpower]]))


SIMPLELOGPOWER$tablecaption <- paste0("Point forecasts of the ",
                       SIMPLELOGPOWER$forecastingyear,
                          " ",
                          tolower(SIMPLELOGPOWER$stockabundance),
                          " for the ",
                          SIMPLELOGPOWER$stockname,
                          " ",
                          SIMPLELOGPOWER$stockspecies,
                          " stock."
                          )

doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

## doc = addTable(doc, data=tt)

baseCellProp = cellProperties( padding = 4)

SIMPLELOGPOWER$my_ft <- FlexTable( data = SIMPLELOGPOWER$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLELOGPOWER$my_ft[, 1:ncol(SIMPLELOGPOWER$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=SIMPLELOGPOWER$my_ft)



SIMPLELOGPOWER$tablecaption <- NULL
SIMPLELOGPOWER$my_ft <- NULL 


##
## Barplot of historical abundance values and associated point forecast: Youngest Age
##



## total.index.simplelogpower

## pred.int.individual.ages.avgfive.youngest
## pred.int.individual.ages.arima.youngest
## pred.int.individual.ages.expsmooth.youngest


doc = addPageBreak(doc)

## if (plots==16){


SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$barplot.forecasted.values.youngest.age.simplelogpower(
                     pred.int.individual.ages.avgfive.youngest = SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest,
                     pred.int.individual.ages.arima.youngest =  SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest,
                     pred.int.individual.ages.expsmooth.youngest = SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest,
                     result.avgfive.youngest = SIMPLELOGPOWER$result.avgfive.youngest,
                     result.arima.youngest = SIMPLELOGPOWER$result.arima.youngest,
                     result.expsmooth.youngest = SIMPLELOGPOWER$result.expsmooth.youngest, 
                     forecastingyear = SIMPLELOGPOWER$forecastingyear, 
                     total.index = SIMPLELOGPOWER$total.index.simplelogpower, 
                     stockabundance = SIMPLELOGPOWER$stockabundance)

doc = addPlot(doc,
        fun = plot, # print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight)

SIMPLELOGPOWER$plotlegend <- paste0("Historical ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " values and ",
                     SIMPLELOGPOWER$forecastingyear,
                     " point forecast corresponding to the youngest age",
                     " component of the ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " for the ",
                     SIMPLELOGPOWER$stockname, " ",
                     SIMPLELOGPOWER$stockspecies,
                     " stock.",
                     # " The 2013 point forecast was derived via exponential smoothing."
                      " The ",
                     SIMPLELOGPOWER$forecastingyear,
                     " point forecast was derived via ",
                     SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$method,
                     "."
                     )

doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

## }

SIMPLELOGPOWER$plotlegend <- NULL 
SIMPLELOGPOWER$myplot <- NULL 

##
## Barplot of historical abundance values and associated point forecast: Older Ages
##


## if (plots==17){

for (i in 1:length(SIMPLELOGPOWER$point_forecast_best_model_for_each_age_class_simplelogpower)){

  print(i)

  doc = addPageBreak(doc)

  SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$barplot.forecasted.values.individual.ages.simplelogpower(SIMPLELOGPOWER$point_forecast_best_model_for_each_age_class_simplelogpower, 
                                                                                                   SIMPLELOGPOWER$forecastingyear, i)

  doc = addPlot(doc,
        fun = plot, # print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight)

  ## rm(myplot)

  SIMPLELOGPOWER$agetmp <- names(SIMPLELOGPOWER$point_forecast_best_model_for_each_age_class_simplelogpower)[i]
  SIMPLELOGPOWER$agetmp <- tolower(SIMPLELOGPOWER$agetmp)
  usePackage("stringr")
  SIMPLELOGPOWER$agetmp <- str_replace(SIMPLELOGPOWER$agetmp,"_"," ")

  SIMPLELOGPOWER$plotlegend <- paste0("Historical ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " values and ",
                     SIMPLELOGPOWER$forecastingyear,
                     " point forecast corresponding to the ",
                     SIMPLELOGPOWER$agetmp,
                     " component of the ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " for the ",
                     SIMPLELOGPOWER$stockname, " ",
                     SIMPLELOGPOWER$stockspecies,
                     " stock.",
                     " The ",
                     SIMPLELOGPOWER$forecastingyear,
                     " point forecast was derived from the associated simple log power regression model."
                     )

  doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

}

SIMPLELOGPOWER$plotlegend <- NULL
SIMPLELOGPOWER$myplot <- NULL  

## }

##
## Barplot of historical abundance values and associated point forecast: Total Age
##

## if (plots==18){

doc = addPageBreak(doc)


SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$barplot.forecasted.values.total.age.simplelogpower(
                              pred.int.individual.ages.avgfive.youngest = SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest,
                              pred.int.individual.ages.arima.youngest = SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest,
                              pred.int.individual.ages.expsmooth.youngest = SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest,
                              pred.int.total.age.simplelogpower.regression.all.models = SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models,
                              best.fits = SIMPLELOGPOWER$best.fits.simplelogpower,
                              result.avgfive.youngest = SIMPLELOGPOWER$result.avgfive.youngest,
                              result.arima.youngest = SIMPLELOGPOWER$result.arima.youngest,  
                              result.expsmooth.youngest = SIMPLELOGPOWER$result.expsmooth.youngest, 
                              forecastingyear = SIMPLELOGPOWER$forecastingyear, 
                              total.index = SIMPLELOGPOWER$total.index.simplelogpower, 
                              stockabundance = SIMPLELOGPOWER$stockabundance)

doc = addPlot(doc,
        fun = plot, # print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight)
        
## rm(myplot)

SIMPLELOGPOWER$plotlegend <- paste0("Historical ",
                     " values and ",
                     SIMPLELOGPOWER$forecastingyear,
                     " point forecast corresponding to the ",
                     " total ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " for the ",
                     SIMPLELOGPOWER$stockname, " ",
                     SIMPLELOGPOWER$stockspecies,
                     " stock.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

## }


###
###  Interval Forecasts
###


doc = addPageBreak(doc)

doc = addTitle( doc, "Interval Forecasts", level = 2)


SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$point.and.interval.forecasts.simplelogpower.regression  # recycle point and interval forecasts from
                                                               # the executive summary



## remove log (or log1p) transformations when reporting age class 
SIMPLELOGPOWER$tt[,1] <- gsub("\\(|\\)", "", x=SIMPLELOGPOWER$tt[,1])
SIMPLELOGPOWER$tt[,1] <- gsub(pattern="log|log1p", replacement="", x=SIMPLELOGPOWER$tt[,1])

usePackage("stringr")
SIMPLELOGPOWER$tt[,1] <- str_replace_all(SIMPLELOGPOWER$tt[,1],"_"," ")


SIMPLELOGPOWER$tablecaption <- paste0("Point and interval forecasts for the ",
                       SIMPLELOGPOWER$forecastingyear,
                          " ", "age-specific and total ", 
                          paste(tolower(SIMPLELOGPOWER$stockabundance),"s",sep=""),
                          " for the ",
                          SIMPLELOGPOWER$stockname,
                          " ",
                          SIMPLELOGPOWER$stockspecies,
                          " stock."
                          )

doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

## doc = addTable(doc, data=tt)


baseCellProp = cellProperties( padding = 4)


SIMPLELOGPOWER$tt[,-1] <- comma(SIMPLELOGPOWER$tt[,-1])

SIMPLELOGPOWER$my_ft <- FlexTable( data = SIMPLELOGPOWER$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLELOGPOWER$my_ft[, 1:ncol(SIMPLELOGPOWER$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=SIMPLELOGPOWER$my_ft)

SIMPLELOGPOWER$my_ft <- NULL 
SIMPLELOGPOWER$tt <- NULL 
SIMPLELOGPOWER$tablecaption <- NULL 




###
### Scatterplot of youngest age with forecasting interval: Youngest Age
###

## if (plots==19){

doc = addPageBreak(doc)


 

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$scatterplot.forecasted.values.and.forecast.intervals.youngest.age.simplelogpower.regression(
                                         pred.int.individual.ages.avgfive.youngest = SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest,
                                         pred.int.individual.ages.arima.youngest = SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest,
                                         pred.int.individual.ages.expsmooth.youngest = SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest,
                                         result.avgfive.youngest = SIMPLELOGPOWER$result.avgfive.youngest,  
                                         result.arima.youngest = SIMPLELOGPOWER$result.arima.youngest, 
                                         result.expsmooth.youngest =  SIMPLELOGPOWER$result.expsmooth.youngest, 
                                         forecastingyear = SIMPLELOGPOWER$forecastingyear, 
                                         total.index = SIMPLELOGPOWER$total.index.simplelogpower, 
                                         stockabundance = SIMPLELOGPOWER$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight-2)

## rm(myplot)

SIMPLELOGPOWER$plotlegend <- paste0("Historical ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " values along with the ",
                     SIMPLELOGPOWER$forecastingyear,
                     " point forecast and 80% interval forecast of the ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " corresponding to the youngest age component of the ",
                     SIMPLELOGPOWER$stockname, " ",
                     SIMPLELOGPOWER$stockspecies,
                     " stock. ",
                     "The point forecast was obtained via ",
                     SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$method, ".",
                     " The interval forecast was derived on the basis of time series bootstrapping.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

## }

###
### Scatterplots of individual ages with forecasting intervals: Older Ages
###

## if (plots==20){

for (i in 1:length(SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower)){

SIMPLELOGPOWER$bestfitssimplelogpower <- SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower
SIMPLELOGPOWER$pointforecastssimplelogpower <- SIMPLELOGPOWER$point_forecast_best_model_for_each_age_class_simplelogpower
SIMPLELOGPOWER$intervalforecastssimplelogpower <-   SIMPLELOGPOWER$PI.individual.ages.simplelogpower.regression.no.comma

SIMPLELOGPOWER$tmpage <- names(SIMPLELOGPOWER$bestfitssimplelogpower)[i]
SIMPLELOGPOWER$tmpage <- tolower(SIMPLELOGPOWER$tmpage)

SIMPLELOGPOWER$plotlegend <- paste0("Historical ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " values along with the ",
                     SIMPLELOGPOWER$forecastingyear,
                     " point forecast and 80% interval forecast of the ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " corresponding to the ", SIMPLELOGPOWER$tmpage,
                     " component of the ",
                     SIMPLELOGPOWER$stockname, " ",
                     SIMPLELOGPOWER$stockspecies,
                     " stock. ",
                     "The point forecast was produced from the simple log power regression model used for this age component. ",
                     "The interval forecast was derived from this same model by using case-based bootstrapping.")

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.simplelogpower.regression(
                                   SIMPLELOGPOWER$bestfitssimplelogpower, 
                                   SIMPLELOGPOWER$pointforecastssimplelogpower, 
                                   SIMPLELOGPOWER$intervalforecastssimplelogpower, 
                                   SIMPLELOGPOWER$forecastingyear,i)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight-2)

doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

## rm(myplot)

}

## }

###
###   Scatterplots of total age with forecasting interval
###

## if (plots==21){


SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$scatterplot.forecasted.values.and.forecast.intervals.total.age.simplelogpower.regression(
                            pred.int.individual.ages.avgfive.youngest = SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest,
                            pred.int.individual.ages.arima.youngest = SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest,
                            pred.int.individual.ages.expsmooth.youngest = SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest,
                            pred.int.total.age.simplelogpower.regression.all.models = SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models,
                            best.fits = SIMPLELOGPOWER$best.fits.simplelogpower,
                            result.avgfive.youngest = SIMPLELOGPOWER$result.avgfive.youngest,
                            result.arima.youngest = SIMPLELOGPOWER$result.arima.youngest,
                            result.expsmooth.youngest = SIMPLELOGPOWER$result.expsmooth.youngest,
                            forecastingyear = SIMPLELOGPOWER$forecastingyear, 
                            total.index = SIMPLELOGPOWER$total.index.simplelogpower, 
                            stockabundance = SIMPLELOGPOWER$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight-2)

## rm(myplot)

SIMPLELOGPOWER$plotlegend <- paste0("Historical values along with the ",
                     SIMPLELOGPOWER$forecastingyear,
                     " point forecast and 80% interval forecast of the ",
                     "total ",
                     tolower(SIMPLELOGPOWER$stockabundance),
                     " corresponding to the ",
                     SIMPLELOGPOWER$stockname, " ",
                     SIMPLELOGPOWER$stockspecies,
                     " stock.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

## }


##----- Bootstrap: Visual Display for Youngest Age

## if (plots==22){


                                                               
                                                               
                                                                

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.yboot.simplelogpower.regression.youngest.age(
                                   pred.int.individual.ages.avgfive.youngest = SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest,
                                   pred.int.individual.ages.arima.youngest = SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest,
                                   pred.int.individual.ages.expsmooth.youngest = SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest,
                                   total.index = SIMPLELOGPOWER$total.index.simplelogpower, 
                                   stockabundance = SIMPLELOGPOWER$stockabundance)
                                                               
doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight-2)



if (SIMPLELOGPOWER$total.index.simplelogpower==1) {
    SIMPLELOGPOWER$model.youngest <- "naive model (i.e., average of previous 5 years)"
}
if (SIMPLELOGPOWER$total.index.simplelogpower==2) {
    SIMPLELOGPOWER$model.youngest <- "ARIMA model"
}
if (SIMPLELOGPOWER$total.index.simplelogpower==3) {
    SIMPLELOGPOWER$model.youngest <- "exponential smoothing model"
}


SIMPLELOGPOWER$plotlegend <- paste0("Histogram of bootstrapped point forecasts for the ",
                      SIMPLELOGPOWER$forecastingyear, " ",
                      tolower(SIMPLELOGPOWER$stockabundance), " ",
                      "corresponding to the ",
                      tolower(SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest$age),
                      " component of the ",
                      SIMPLELOGPOWER$stockname, " ",
                      SIMPLELOGPOWER$stockspecies,
                      " stock.",
                      " The dashed red line indicates the position on the horizontal axis of the point forecast derived from the ",
                       SIMPLELOGPOWER$model.youngest, ".",
                      " The blue segment indicates the 80% forecast interval."
                      )

doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

doc = addPageBreak(doc)


##----- Bootstrap Cases: Visual Displays for Older Ages


addTitle(doc, "Bootstrapping Results for Older Ages", level=2)

## B <- 1000
SIMPLELOGPOWER$best.fits.simplelogpower <- SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower
## pred.int.individual.ages.simplelogpower.regression <- best.simplelogpower.regression.model.forecast(datafile_variables,
##                                                       results_best_fitting_model_for_each_age_class_simplelogpower,
##                                                       forecastingyear,
##                                                       B)

SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression   ## this should already be computed in Review Code script
## doc = addPlot(doc,
##        fun=print,
##        x=plot.deltaboot.simplelogpower.regression.best.fitting.models(best.fits.simplelogpower, pred.int.individual.ages.simplelogpower.regression),
##        width=plotwidth, height=plotheight,
##        vector.graphic = F)

## doc = addParagraph(doc, value=paste("Histograms of Bootstrap Forecast Errors"), stylename="rPlotLegend")


## if (plots==23){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.yboot.simplelogpower.regression.best.fitting.models(SIMPLELOGPOWER$best.fits.simplelogpower, 
                                SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth, height=plotheight)

## rm(myplot)

SIMPLELOGPOWER$plotlegend <- paste0("Histograms of bootstrapped point forecasts for the ",
                      SIMPLELOGPOWER$forecastingyear, " ",
                      tolower(SIMPLELOGPOWER$stockabundance), " ",
                      "corresponding to the ",
                      "older age",
                      " components of the ",
                      SIMPLELOGPOWER$stockname, " ",
                      SIMPLELOGPOWER$stockspecies,
                      " stock.",
                      " For each age component, the dashed red line indicates the position on the horizontal axis of the point forecast derived",
                      " from the simple log power regression model used for that component. ",
                      " The blue segment indicates the 80% forecast interval.", sep="")


doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$plotlegend <- NULL 



## }

###
### Histogram of Bootstrap Predictions: Best Model for Total Age
###

## if (plots==24){

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.yboot.simplelogpower.regression.total.age(SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models,
                                                SIMPLELOGPOWER$total.index.simplelogpower, SIMPLELOGPOWER$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight-2)

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 

SIMPLELOGPOWER$plotlegend <- paste0("Histograms of bootstrapped point forecasts for the ",
                      SIMPLELOGPOWER$forecastingyear, " total ",
                      tolower(SIMPLELOGPOWER$stockabundance), " ",
                      "corresponding to the ",
                      SIMPLELOGPOWER$stockname, " ",
                      SIMPLELOGPOWER$stockspecies,
                      " stock.",
                      " The dashed red line indicates the position on the horizontal axis of the point forecast of ",
                      "total ",
                      tolower(SIMPLELOGPOWER$stockabundance), ".",
                      " The blue segment indicates the 80% forecast interval.")


doc = addParagraph(doc, value=SIMPLELOGPOWER$plotlegend, stylename="rPlotLegend")

SIMPLELOGPOWER$plotlegend <- NULL 
SIMPLELOGPOWER$myplot <- NULL 

## }


#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================


doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

SIMPLELOGPOWER$empirical.probability.yboot.simplelogpower.regression.total.age <- function(pred.int.total.age.simplelogpower.regression.all.models,
                                                               total.index.simplelogpower, stockabundance){


    if (total.index.simplelogpower==1) {   # naive model for youngest age (average of previous 5 years)

        y.star.boot.stacked <- pred.int.total.age.simplelogpower.regression.all.models$sim[[total.index.simplelogpower]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]] ## point forecast of total abundance

    }

    if (total.index.simplelogpower==2) {   # arima model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simplelogpower.regression.all.models$sim[[total.index.simplelogpower]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]] ## point forecast of total abundance

    }

    if (total.index.simplelogpower==3) {   # exponential smoothing model for youngest age

        y.star.boot.stacked <- pred.int.total.age.simplelogpower.regression.all.models$sim[[total.index.simplelogpower]]
        mylabel <- paste("Total", stockabundance)
        labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

        data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

        pfct <- pred.int.total.age.simplelogpower.regression.all.models$p[[total.index.simplelogpower]] ## point forecast of total abundance

    }

    ## cumulative probabilities evaluated for the endpoints of equal-sized bins covering the
    ## range of bootstrapped point forecasts

    x <- as.numeric(y.star.boot.stacked)
    ## usePackage("Hmisc")

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


SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age <- SIMPLELOGPOWER$empirical.probability.yboot.simplelogpower.regression.total.age(
                                                               SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models,
                                                               SIMPLELOGPOWER$total.index.simplelogpower, 
                                                               SIMPLELOGPOWER$stockabundance)


SIMPLELOGPOWER$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(SIMPLELOGPOWER$stockabundance)," ",
                       "value yet to be observed in ",
                       SIMPLELOGPOWER$forecastingyear, " for the ",
                       SIMPLELOGPOWER$stockname," ",
                       SIMPLELOGPOWER$stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ",  
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(SIMPLELOGPOWER$stockabundance), ".")



SIMPLELOGPOWER$tablecaption_emp_prob_total <- SIMPLELOGPOWER$tablecaption 

doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

SIMPLELOGPOWER$tt_1 <- SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.thresholds

SIMPLELOGPOWER$tt_2 <- SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast

SIMPLELOGPOWER$tt_1_and_2 <- rbind.data.frame(SIMPLELOGPOWER$tt_1, SIMPLELOGPOWER$tt_2)

## usePackage("plyr")

## SIMPLELOGPOWER$tt_arrange <- arrange(SIMPLELOGPOWER$tt_1_and_2, prob.threshold)

#### SIMPLELOGPOWER$prob.threshold <-  SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast$prob.threshold

SIMPLELOGPOWER$tt_arrange <- SIMPLELOGPOWER$tt_1_and_2[order(SIMPLELOGPOWER$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

SIMPLELOGPOWER$from_tmp = which(SIMPLELOGPOWER$tt_arrange[,1] == SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast$prob.threshold)

SIMPLELOGPOWER$tt_arrange[SIMPLELOGPOWER$from_tmp, 4] <- SIMPLELOGPOWER$tt_arrange[SIMPLELOGPOWER$from_tmp + 1, 4]

SIMPLELOGPOWER$tt_arrange[,1] <- comma(SIMPLELOGPOWER$tt_arrange[,1])
SIMPLELOGPOWER$tt_arrange[,2] <- paste0(SIMPLELOGPOWER$tt_arrange[,2],"%")
SIMPLELOGPOWER$tt_arrange[,3] <- paste0(SIMPLELOGPOWER$tt_arrange[,3],"%")
SIMPLELOGPOWER$tt_arrange[,4] <- paste0(SIMPLELOGPOWER$tt_arrange[,4],"%")


names(SIMPLELOGPOWER$tt_arrange)[1] <- "Threshold"
names(SIMPLELOGPOWER$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(SIMPLELOGPOWER$tt_arrange)[3] <- "Prob(Actual >= Threshold)"


names(SIMPLELOGPOWER$tt_arrange)[4] <- "Interval Probability"
SIMPLELOGPOWER$tt_arrange[1,4] <- "-"


SIMPLELOGPOWER$my_ft <- FlexTable( data = SIMPLELOGPOWER$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLELOGPOWER$my_ft[, 1:ncol(SIMPLELOGPOWER$tt_arrange)] = parProperties(text.align = "right")

SIMPLELOGPOWER$my_ft = spanFlexTableRows(SIMPLELOGPOWER$my_ft, j=4, from = SIMPLELOGPOWER$from_tmp, to = SIMPLELOGPOWER$from_tmp + 1)

## SIMPLELOGPOWER$my_ft[SIMPLELOGPOWER$tt_arrange$Threshold %in% comma(SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast[1]), ] 
# = cellProperties( background.color = "orange" )

SIMPLELOGPOWER$my_ft[SIMPLELOGPOWER$tt_arrange$Threshold %in% comma(SIMPLELOGPOWER$emp.prob.simplelogpower.regression.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

SIMPLELOGPOWER$my_ft_emp_prob_total <- SIMPLELOGPOWER$my_ft

doc = addFlexTable(doc, flextable=SIMPLELOGPOWER$my_ft_emp_prob_total)

doc = addPageBreak(doc)

SIMPLELOGPOWER$my_ft <- NULL  # delete my_ft as it is no longer needed


#================================================================================================
#
# Retrospective Evaluation of Performance of Point Forecasts
#
#================================================================================================

# SIMPLELOGPOWER$

## doc = addPageBreak(doc)


doc = addTitle(doc, paste("Retrospective Evaluation of Performance of Point Forecasts"), level=1)


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("This section reports the results associated with the retrospective evaluation of the performance of the point forecasts of ",
                        ## stockspecies," ", stockname, " stock ",
                        ## "corresponding to the forecasting year ",forecastingyear,". ",
                        "age-specific and total ",
                        paste0(tolower(SIMPLELOGPOWER$stockabundance),"s"),
                        " corresponding to the ", SIMPLELOGPOWER$stockspecies, " ", SIMPLELOGPOWER$stockname, " stock.",
                        " The retrospective evaluation was performed separately for the youngest age class and for the older age classes of the stock, as explained below.", 
                        sep="")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )
                        

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                        
SIMPLELOGPOWER$paragraph <- paste0("For the youngest age class, the retrospective evaluation relied on the computation of retrospective point forecasts produced via the ",
                        SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$method, ".", 
                        sep="")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )                        


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("For each of the older age groups, retrospective evaluation relied on the computation of retrospective point forecasts produced via ",
                        "the applicable simple log power regression model for that group. ",
                        "As an example, if Age 5 represents one of the older age groups, the simple ", 
                        "log power regression model corresponding to this age related ", 
                        "log-transformed Age 5 abundance to log-transformed Age 4 abundance.",
                      sep="")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

## SIMPLELOGPOWER$paragraph <- paste0("Note that the re",
##                      sep="")
## doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )


SIMPLELOGPOWER$paragraph <- NULL

SIMPLELOGPOWER$pot1 <-  pot("Given an age class for the stock, retrospective forecast errors were defined as the actual ") +
         pot(paste(tolower(SIMPLELOGPOWER$stockabundance))) +
         pot(" values ") +
         pot("minus the retrospectively forecasted ") +
         pot(paste(tolower(SIMPLELOGPOWER$stockabundance))) +
         pot(" values.") +
         pot(" In view of this definition, ") +
         pot(" positive", textProperties( font.style = "italic", font.size=11, font.family="Calibri")) +
         pot(" values for the retrospective forecast errors") +
         pot(" represent forecasts that were ") +
         pot("low", textProperties( font.style = "italic", font.size=11, font.family="Calibri")) +
         pot(" relative to the historical ") +
         pot(paste(tolower(SIMPLELOGPOWER$stockabundance))) +
         pot(" values, ") +
         pot(" whereas") +
         pot(" negative ", textProperties(font.style = "italic", font.size=11, font.family="Calibri")) +
         pot("values represent forecasts that were ") +
         pot("high ", textProperties( font.style = "italic", font.size=11, font.family="Calibri")) +
         pot("relative to the historical ") +
         pot(paste(tolower(SIMPLELOGPOWER$stockabundance))) +
         pot(" values.")

SIMPLELOGPOWER$paragraph <- set_of_paragraphs(SIMPLELOGPOWER$pot1)

doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- NULL 
SIMPLELOGPOWER$pot1 <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("The following retrospective measures were used to characterize different aspects of the distribution",
                    " of the retrospective forecasting errors:")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename = "Normal" )

SIMPLELOGPOWER$paragraph <- NULL 

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$texts = c( "Mean Raw Error (MRE);",
           "Mean Absolute Error (MAE);",
           "Mean Percent Error (MPE);",
           "Mean Absolute Percent Error (MAPE);",
           "Mean Scaled Error (MASE);",
           "Root Mean Square Error.")
# add texts with stylename BulletList
doc = addParagraph( doc, value = SIMPLELOGPOWER$texts, stylename="BulletList" )

SIMPLELOGPOWER$texts <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("MAE and MAPE reflect overall forecast accuracy accounting for ",
                    "systematic bias and year-to-year variation.")
doc = addParagraph( doc, value = SIMPLELOGPOWER$paragraph, stylename="Normal" )

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("MRE and MPE reflect directional bias in raw and relative forecast errors, respectively, ",
                    "with negative values indicating a tendency to underforecast and positive values reflecting a tendency to overforecast.")
doc = addParagraph( doc, value = SIMPLELOGPOWER$paragraph, stylename="Normal" )


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Just like MAE, RMSE is a measure of the absolute magnitude of the raw retrospective forecast errors, ",
                    "but is more sensitive to large values then MAE.")
doc = addParagraph( doc, value = SIMPLELOGPOWER$paragraph, stylename="Normal" )


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste("MASE was proposed by Hyndman and Koehler (2006) as a generally applicable, scale-free measure of forecast accuracy.",
                       "This measure never gives infinite or undefined values.",
                       "In this report, MASE is computed as the average of the absolute values of the scaled retrospective forecast errors produced by",
                       "the log power regression methodology.",
                       "The scaling of the errors involves dividing the errors by the MAE computed from the retrospective forecast errors associated with",
                       "the naive model based on the",
                       # terminal run
                       tolower(SIMPLELOGPOWER$stockabundance),
                       "for the previous year.",
                       # "from the one-step, naive forecasting method.",
                       # "A scaled error is less than 1 if it arises from a better forecast than the one produced by the naive model based on the terminal run for the previous year.",
                       # "Conversely, it is greater than 1 if the forecast is worse than the average one-step, naive forecast computed in-sample.",
                       "A value of MASE less than 1 suggests that the retrospective forecasting accuracy of the log power regression model",
                       # terminal run
                       tolower(SIMPLELOGPOWER$stockabundance),
                       "is better than the retrospective forecasting accuracy of",
                       "the benchmark naive model based on the",
                       # terminal run
                       tolower(SIMPLELOGPOWER$stockabundance),
                       "for the previous year.",
                       "A value of MASE greater than 1 suggests that the retrospective forecasting accuracy of the log power regression model",
                       "is worse than the retrospective forecasting accuracy of",
                       "the benchmark naive model based on the",
                       # terminal run
                       tolower(SIMPLELOGPOWER$stockabundance),
                       "for the previous year.",
                      sep=" ")
doc = addParagraph( doc, value = SIMPLELOGPOWER$paragraph, stylename="Normal" )

SIMPLELOGPOWER$paragraph <- NULL


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("To facilitate the interpretation of the retrospective forecast errors, ",
                    "this section reports several types of plots:")
doc = addParagraph( doc, value = SIMPLELOGPOWER$paragraph, stylename="Normal" )
SIMPLELOGPOWER$paragraph <- NULL 

SIMPLELOGPOWER$texts = c("Plots illustrating the performance of the retrospective forecasting evaluation;",
           "Density plots of the retrospective forecast errors;",   
           "Bias coefficient plots obtained from the retrospective forecast errors;",
           "Barplots of the retrospective forecast errors annotated with the interval forecasts corresponding to the forecasting year of interest.")
# add texts with stylename BulletList
doc = addParagraph( doc, value = SIMPLELOGPOWER$texts, stylename="BulletList" )
SIMPLELOGPOWER$texts <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
                   
SIMPLELOGPOWER$paragraph <- paste0("The plots illustrating the performance of the retrospective forecast evaluations ",
                    "are reported only for the individual age classes of ",
                    tolower(SIMPLELOGPOWER$stockabundance), ". ", 
                    "All other plots are reported both for the individual age classes of ", 
                     tolower(SIMPLELOGPOWER$stockabundance), " ", 
                    "and for the total ",
                    tolower(SIMPLELOGPOWER$stockabundance), ".", 
                    sep="")
doc = addParagraph( doc, value = SIMPLELOGPOWER$paragraph, stylename="Normal" )

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste("Bias coefficients representing a new metric for forecast bias are also reported in numerical and visual form in this section. ",
                    "These coefficients are computed from the retrospective forecast errors for the age-specific and total ",
                    paste0(tolower(SIMPLELOGPOWER$stockabundance),"s"),
                    " using the formula developed by Kourentzes, Trapero and Svetunkov",
                    " in their 2014 working paper \"Measuring the behaviour of experts on demand forecasting: a simple task\".",
                    " In the context of this report, the bias coefficients describe the direction and magnitude of the retrospective forecast bias associated with the",
                    "exponential smoothing forecasting method.",
                    sep=" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragragraph <- NULL  


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste("Generally speaking, the bias coefficients are unit-free and bounded between -1 (maximum negative retrospective forecast bias)",
                    "and 1 (maximum positive retrospective forecast bias).",
                    "A forecasting method that is always producing retrospective point forecasts which are over the observed historical values will have",
                    "a bias coefficient equal to -1, always over-forecasting.",
                    "A forecasting method that is always producing retrospective point forecasts which are under the observed historical values",
                    "will have a bias coefficient equal to 1, always under-forecasting.",
                    "Given the bounded nature of the bias coefficient, we can describe a forecasting method as strongly biased if |bias coefficient| > 0.5",
                    "and weakly biased if 0 < |bias coefficient| <= 0.5, providing a simple and intuitive description of the forecast bias behaviour.",
                    "If the bias coefficient is equal to 0, the forecasting method is unbiased.",
                    sep=" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

## SIA

### Measures of Retrospective Performance

doc = addPageBreak(doc)

doc = addTitle(doc, paste("Measures of Retrospective Point Forecast Performance"), level=2)


SIMPLELOGPOWER$tablecaption <- paste0("Retrospective measures of forecast performance associated with the point forecasts of the ",
                      SIMPLELOGPOWER$forecastingyear, " ",
                      "age-specific and total ",
                      paste0(tolower(SIMPLELOGPOWER$stockabundance),"s"),
                      " for the ",
                      SIMPLELOGPOWER$stockname, " ",
                      SIMPLELOGPOWER$stockspecies,
                      " stock.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")
## tt <- retro.measures.all.ages.simplelogpower(best.rmse.youngest.age.simplelogpower)
##

baseCellProp = cellProperties( padding = 4)

SIMPLELOGPOWER$tt <- SIMPLELOGPOWER$retro.measures.all.ages.simplelogpower(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower)

usePackage("stringr")
names(SIMPLELOGPOWER$tt) <- str_replace_all(names(SIMPLELOGPOWER$tt),"_"," ")


SIMPLELOGPOWER$tt[,-1] <- comma(SIMPLELOGPOWER$tt[,-1])

SIMPLELOGPOWER$my_ft <- FlexTable( data = SIMPLELOGPOWER$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
SIMPLELOGPOWER$my_ft[, 1:ncol(SIMPLELOGPOWER$tt)] = parProperties(text.align = "right")


doc = addFlexTable(doc, flextable=SIMPLELOGPOWER$my_ft)

SIMPLELOGPOWER$tt <- NULL 
SIMPLELOGPOWER$my_ft <- NULL 
SIMPLELOGPOWER$tablecaption <- NULL 

#================================================================================================
# Retrospective Point Forecasts and Forecast Errors - May need to move this section somewhere else in the report!!!
#================================================================================================

## ROBERT

doc = addPageBreak(doc)

doc = addTitle(doc, paste("Retrospective Point Forecasts and Forecast Errors"), level=2)


##
## Youngest Age
##

SIMPLELOGPOWER$tabledata <- SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin[[1]]



## if ( sum(grepl("^a^|^p^|^e^",names(SIMPLELOGPOWER$tabledata))) > 0 ) {
if (identical(c("a","p","e"), names(SIMPLELOGPOWER$tabledata)[3:5])) {
    SIMPLELOGPOWER$tabledata <- subset(SIMPLELOGPOWER$tabledata, select=c("cy","a","p","e"))
    SIMPLELOGPOWER$tabledata <- as.data.frame(SIMPLELOGPOWER$tabledata)
    names(SIMPLELOGPOWER$tabledata) <- c("Return Year","Actual","Forecast","Error")
    print(SIMPLELOGPOWER$tabledata)
}

## if (sum(grepl("Actual|Forecast|Error",names(SIMPLELOGPOWER$tabledata))) > 0 ) {
## if (identical(c("Actual","Forecast","Error"), names(SIMPLELOGPOWER$tabledata)[3:5])) {
##    SIMPLELOGPOWER$tabledata <- subset(SIMPLELOGPOWER$tabledata, select=c("cy","Actual","Forecast","Error"))
##    SIMPLELOGPOWER$tabledata <- as.data.frame(SIMPLELOGPOWER$tabledata)
##    names(SIMPLELOGPOWER$tabledata) <- c("Return Year","Actual","Forecast","Error")
##    print(SIMPLELOGPOWER$tabledata)
## }


SIMPLELOGPOWER$tabledata <- as.data.frame(SIMPLELOGPOWER$tabledata)

usePackage("scales")
SIMPLELOGPOWER$tabledata[,"Actual"] <- comma(SIMPLELOGPOWER$tabledata[,"Actual"])
SIMPLELOGPOWER$tabledata[,"Forecast"] <- comma(SIMPLELOGPOWER$tabledata[,"Forecast"])
SIMPLELOGPOWER$tabledata[,"Error"] <- comma(SIMPLELOGPOWER$tabledata[,"Error"])


SIMPLELOGPOWER$youngest.age.simplelogpower <- names(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin)[1]
usePackage("stringr")
SIMPLELOGPOWER$youngest.age.simplelogpower <- str_replace_all(SIMPLELOGPOWER$youngest.age.simplelogpower,"_", " ")
SIMPLELOGPOWER$youngest.age.simplelogpower <- tolower(SIMPLELOGPOWER$youngest.age.simplelogpower)

SIMPLELOGPOWER$tablecaption <- paste0("Retrospective point forecasts and associated forecast errors for the ",
                      SIMPLELOGPOWER$forecastingyear, " ",
                      tolower(SIMPLELOGPOWER$stockabundance),
                      " corresponding to the youngest age component",
                      " (i.e., ", SIMPLELOGPOWER$youngest.age.simplelogpower, ")",
                      " of the ",
                      SIMPLELOGPOWER$stockname, " ",
                      SIMPLELOGPOWER$stockspecies,
                      " stock",
                      ".",
                      " Accompanying return years and actual ",
                      tolower(SIMPLELOGPOWER$stockabundance),
                      " values are also reported.",
                      " The retrospective point forecasts were obtained by using ",
                      SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$method,
                      ", ",
                      "since log power regression could not be used for the youngest component of the stock. ",
                      "A positive forecast error indicates an under-forecast and a negative forecast error ",
                      "indicates an over-forecast.")


## tablecaption <- set_of_paragraphs(tablecaption)

baseCellProp = cellProperties( padding = 4)

doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

if (names(SIMPLELOGPOWER$tabledata)[1]=="cy") {
   SIMPLELOGPOWER$tabledata <- SIMPLELOGPOWER$tabledata[,c("cy", "Actual","Forecast","Error")]
   names(SIMPLELOGPOWER$tabledata)[names(SIMPLELOGPOWER$tabledata)=="cy"] <- "Return Year"
} else {
   SIMPLELOGPOWER$tabledata <- SIMPLELOGPOWER$tabledata[,1:4]
   names(SIMPLELOGPOWER$tabledata)[1] <- "Return Year"
}

head(SIMPLELOGPOWER$tabledata)


SIMPLELOGPOWER$ft_tabledata <- FlexTable(data=SIMPLELOGPOWER$tabledata, 
                          header.columns=TRUE, 
                          body.cell.props = baseCellProp,
                          header.par.props = parProperties(text.align = "right" ),
                          header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))


# overwrites some paragraph formatting properties
SIMPLELOGPOWER$ft_tabledata[, 1:ncol(SIMPLELOGPOWER$tabledata)] = parProperties(text.align = "right")


# set columns widths (inch)
SIMPLELOGPOWER$ft_tabledata = setFlexTableWidths(SIMPLELOGPOWER$ft_tabledata,  
	                                ## widths = c(1.2,1.2,1.2,1.2) 
                                  widths = rep(1.2, ncol(SIMPLELOGPOWER$tabledata)))


## doc = addFlexTable(doc, flextable=ft_tabledata,
##                 layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
##                                                   data.cell=cellProperties(border.color="black")))


doc = addFlexTable(doc, flextable = SIMPLELOGPOWER$ft_tabledata)
  
doc = addParagraph(doc, value=" ", stylename="Normal")

## rm(tabledata)

SIMPLELOGPOWER$tabledata <- NULL 
SIMPLELOGPOWER$ft_tabledata <- NULL
SIMPLELOGPOWER$tablecaption <- NULL  

##
## Older Ages
##

for (i in 1:(length(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin)-1)) {

    doc = addPageBreak(doc)

    SIMPLELOGPOWER$tabledata <- SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin[[i+1]]

    SIMPLELOGPOWER$tabledata <- as.data.frame(SIMPLELOGPOWER$tabledata)

    if (identical(c("a","p","e"), names(SIMPLELOGPOWER$tabledata)[2:4])) {
    ## if ( sum(grepl("^a^|^p^|^e^",names(SIMPLELOGPOWER$tabledata))) > 0 ) {
        SIMPLELOGPOWER$tabledata <- subset(SIMPLELOGPOWER$tabledata, select=c("cy","a","p","e"))
        SIMPLELOGPOWER$tabledata <- as.data.frame(SIMPLELOGPOWER$tabledata)
        names(SIMPLELOGPOWER$tabledata) <- c("Return Year","Actual","Forecast","Error")
        print(SIMPLELOGPOWER$tabledata)
    }

    ## if (identical(c("Actual","Forecast","Error"), names(SIMPLELOGPOWER$tabledata)[2:4])) {
    #### if (sum(grepl("Actual|Forecast|Error",names(SIMPLELOGPOWER$tabledata))) > 0 ) {
    ##   SIMPLELOGPOWER$tabledata <- subset(SIMPLELOGPOWER$tabledata, select=c("cy","Actual","Forecast","Error"))
    ##   SIMPLELOGPOWER$tabledata <- as.data.frame(SIMPLELOGPOWER$tabledata)
    ##   names(SIMPLELOGPOWER$tabledata) <- c("Return Year","Actual","Forecast","Error")
    ##   print(SIMPLELOGPOWER$tabledata)
    ## }


    usePackage("scales")
    SIMPLELOGPOWER$tabledata[,"Actual"] <- comma(SIMPLELOGPOWER$tabledata[,"Actual"])
    SIMPLELOGPOWER$tabledata[,"Forecast"] <- comma(SIMPLELOGPOWER$tabledata[,"Forecast"])
    SIMPLELOGPOWER$tabledata[,"Error"] <- comma(SIMPLELOGPOWER$tabledata[,"Error"])

    SIMPLELOGPOWER$older.age <- names(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin)[i+1]
    usePackage("stringr")
    SIMPLELOGPOWER$older.age <- str_replace_all(SIMPLELOGPOWER$older.age,"_", " ")
    SIMPLELOGPOWER$older.age <- tolower(SIMPLELOGPOWER$older.age)


    SIMPLELOGPOWER$tablecaption <- paste0("Retrospective point forecasts and associated forecast errors for the ",
                          SIMPLELOGPOWER$forecastingyear, " ",
                          tolower(SIMPLELOGPOWER$stockabundance),
                          " corresponding to the ", SIMPLELOGPOWER$older.age ,
                          " component",
                          " of the ",
                          SIMPLELOGPOWER$stockname, " ",
                          SIMPLELOGPOWER$stockspecies,
                          " stock",
                          ".",
                          " Accompanying return years and actual ",
                          tolower(SIMPLELOGPOWER$stockabundance),
                          " values are also reported.",
                          " The retrospective point forecasts were obtained from",
                          " the simple log power regression model used for the ",
                          SIMPLELOGPOWER$older.age,
                          ## " component among all candidate log power regression models",
                          " component. ",
                          "A positive forecast error indicates an under-forecast and a negative forecast error ",
                          "indicates an over-forecast."
                          )

    doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")
    
    baseCellProp = cellProperties( padding = 4)
    
    if (names(SIMPLELOGPOWER$tabledata)[1]=="cy") {
       SIMPLELOGPOWER$tabledata <- SIMPLELOGPOWER$tabledata[,c("cy", "Actual","Forecast","Error")]
       names(SIMPLELOGPOWER$tabledata)[names(SIMPLELOGPOWER$tabledata)=="cy"] <- "Return Year" 
    } else {
       SIMPLELOGPOWER$tabledata <- SIMPLELOGPOWER$tabledata[,1:4]
       names(SIMPLELOGPOWER$tabledata)[1] <- "Return Year" 
    }
    
    SIMPLELOGPOWER$ft_tabledata <- FlexTable(data=SIMPLELOGPOWER$tabledata, 
                              header.columns=TRUE, 
                              body.cell.props = baseCellProp,
                              header.par.props = parProperties(text.align = "right" ),
                              header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))
    
    
    ## doc = addFlexTable(doc, flextable=ft_tabledata,
    ##             layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
    ##                                               data.cell=cellProperties(border.color="black")))
    
    # overwrites some paragraph formatting properties
    SIMPLELOGPOWER$ft_tabledata[, 1:ncol(SIMPLELOGPOWER$tabledata)] = parProperties(text.align = "right")
    
    # set columns widths (inch)
    SIMPLELOGPOWER$ft_tabledata = setFlexTableWidths(SIMPLELOGPOWER$ft_tabledata,
	                                ## widths = c(1.2,1.2,1.2,1.2) 
                                  widths = rep(1.2, ncol(SIMPLELOGPOWER$tabledata))
                                  )

    
    doc = addFlexTable(doc, flextable = SIMPLELOGPOWER$ft_tabledata)
    
    doc = addParagraph(doc, value=" ", stylename="Normal")

    ## rm(tabledata)
    ## rm(ft_tabledata)

    SIMPLELOGPOWER$tabledata <- NULL 
    SIMPLELOGPOWER$ft_tabledata <- NULL 
    SIMPLELOGPOWER$tablecaption <- NULL 

}


##
## Total Age
##



SIMPLELOGPOWER$tabledata <- data.frame(cy =  SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin[[1]]$cy,
                        a = SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$a.total,
                        p = SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$p.total,
                        e = SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$e.total)


if (sum(grepl("Actual|Forecast|Error",names(SIMPLELOGPOWER$tabledata))) > 0 ) {
       SIMPLELOGPOWER$tabledata <- subset(SIMPLELOGPOWER$tabledata, select=c("cy","Actual","Forecast","Error"))
       SIMPLELOGPOWER$tabledata <- as.data.frame(SIMPLELOGPOWER$tabledata)
       names(SIMPLELOGPOWER$tabledata) <- c("Return Year","Actual","Forecast","Error")
       print(SIMPLELOGPOWER$tabledata)
}


if ( sum(grepl("a|p|e",names(SIMPLELOGPOWER$tabledata))) > 0 ) {
        SIMPLELOGPOWER$tabledata <- subset(SIMPLELOGPOWER$tabledata, select=c("cy","a","p","e"))
        SIMPLELOGPOWER$tabledata <- as.data.frame(SIMPLELOGPOWER$tabledata)
        names(SIMPLELOGPOWER$tabledata) <- c("Return Year","Actual","Forecast","Error")
        print(SIMPLELOGPOWER$tabledata)
}

usePackage("scales")
SIMPLELOGPOWER$tabledata[,"Actual"] <- comma(SIMPLELOGPOWER$tabledata[,"Actual"])
SIMPLELOGPOWER$tabledata[,"Forecast"] <- comma(SIMPLELOGPOWER$tabledata[,"Forecast"])
SIMPLELOGPOWER$tabledata[,"Error"] <- comma(SIMPLELOGPOWER$tabledata[,"Error"])

SIMPLELOGPOWER$tablecaption <- paste0("Retrospective point forecasts and associated forecast errors for the ",
                      SIMPLELOGPOWER$forecastingyear, " ",
                      "total ",
                      tolower(SIMPLELOGPOWER$stockabundance),
                      " corresponding to the ",
                      SIMPLELOGPOWER$stockname, " ",
                      SIMPLELOGPOWER$stockspecies,
                      " stock",
                      ".",
                      " Accompanying return years and actual ",
                      tolower(SIMPLELOGPOWER$stockabundance),
                      " values are also reported. ",
                      "A positive forecast error indicates an under-forecast and a negative forecast error ",
                      "indicates an over-forecast.")

doc = addParagraph(doc, value=SIMPLELOGPOWER$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

SIMPLELOGPOWER$ft_tabledata <- FlexTable(data=SIMPLELOGPOWER$tabledata, 
                          header.columns=TRUE, 
                          body.cell.props = baseCellProp,
                          header.par.props = parProperties(text.align = "right" ),
                          header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))

## doc = addFlexTable(doc, flextable=ft_tabledata,
##                 layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
##                                                    data.cell=cellProperties(border.color="black")))


# overwrites some paragraph formatting properties
SIMPLELOGPOWER$ft_tabledata[, 1:ncol(SIMPLELOGPOWER$tabledata)] = parProperties(text.align = "right")


# set columns widths (inch)
SIMPLELOGPOWER$ft_tabledata = setFlexTableWidths(SIMPLELOGPOWER$ft_tabledata,
	                                widths = c(1.2,1.2,1.2,1.2) )


doc = addFlexTable(doc, flextable = SIMPLELOGPOWER$ft_tabledata)

doc = addParagraph(doc, value=" ", stylename="Normal")

## rm(tabledata)
## rm(ft_tabledata)

SIMPLELOGPOWER$tabledata <- NULL
SIMPLELOGPOWER$ft_tabledata <- NULL
SIMPLELOGPOWER$tablecaption <- NULL 

#===============================================================================


##======================================================================================
##  "Cool" plots for the youngest age 
##======================================================================================

doc = addPageBreak(doc)

addTitle(doc, "Plots Illustrating the Performance of the Retrospective Forecasting Evaluation", level=2)


usePackage("stringr")

if (SIMPLELOGPOWER$best.rmse.youngest.age$method == "naive forecasting (i.e., average of previous five years)") {

    SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$youngest.age.retro.plot.avgfive(SIMPLELOGPOWER$youngest.age.retro.plot.info.avgfive, SIMPLELOGPOWER$stockabundance)
    
    SIMPLELOGPOWER$age_youngest <- SIMPLELOGPOWER$youngest.age.retro.plot.info.avgfive$age0
    
    SIMPLELOGPOWER$model_age_youngest <- str_replace(SIMPLELOGPOWER$best.rmse.youngest.age$method, "forecasting", "modeling")
    
}

if (SIMPLELOGPOWER$best.rmse.youngest.age$method == "ARIMA forecasting") {

    SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$youngest.age.retro.plot.arima(SIMPLELOGPOWER$youngest.age.retro.plot.info.arima, SIMPLELOGPOWER$stockabundance)

    SIMPLELOGPOWER$age_youngest <- SIMPLELOGPOWER$youngest.age.retro.plot.info.arima$age0
    
    SIMPLELOGPOWER$model_age_youngest <- str_replace(SIMPLELOGPOWER$best.rmse.youngest.age$method, "forecasting", "modeling")
    
}

if (SIMPLELOGPOWER$best.rmse.youngest.age$method == "exponential smoothing forecasting") {

    SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$youngest.age.retro.plot.expsmooth(SIMPLELOGPOWER$youngest.age.retro.plot.info.expsmooth, SIMPLELOGPOWER$stockabundance)
    
    SIMPLELOGPOWER$age_youngest <- SIMPLELOGPOWER$youngest.age.retro.plot.info.expsmooth$age0
    
    SIMPLELOGPOWER$model_age_youngest <- str_replace(SIMPLELOGPOWER$best.rmse.youngest.age$method, "forecasting", "modeling")

}


SIMPLELOGPOWER$age_youngest <- tolower(SIMPLELOGPOWER$age_youngest)
SIMPLELOGPOWER$age_youngest <- str_replace(SIMPLELOGPOWER$age_youngest, "_", " ")

SIMPLELOGPOWER$figurecaption <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of the youngest age component ",
                        " (i.e., ", SIMPLELOGPOWER$age_youngest, ")",  
                        " of the ",
                        " ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                        " for the ",
                        SIMPLELOGPOWER$stockname,
                        " ",
                        SIMPLELOGPOWER$stockspecies,
                        ".",
                        " Retrospective forecast errors were obtained via ",
                        SIMPLELOGPOWER$best.rmse.youngest.age$method,
                        ". ", 
                        "Historical values of ", SIMPLELOGPOWER$age_youngest, " ", tolower(SIMPLELOGPOWER$stockabundance), 
                        " (grey lines) ", 
                        "and fitted values produced by the", SIMPLELOGPOWER$model_age_youngest, 
                        " are also shown. ", 
                        "Each panel corresponds to a particular retrospective forecasting year.")

## Figure 22 : 	Actual values (grey dots) and retrospectively forecasted values (red dots) of the age 2 component of terminal run 
## for the SPR chinook salmon stock, derived via naive modeling based on the average of terminal run from the previous 3 years. 
## Historical values of age 2 terminal run (grey lines) 
## and fitted values produced by the naive modeling (red lines) are also shown. 
## Each panel corresponds to a particular retrospective forecasting year.


doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth, 
        height=plotheight)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 
SIMPLELOGPOWER$age_youngest <- NULL 
SIMPLELOGPOWER$model_age_youngest <- NULL 

## rm(age_youngest)
## rm(model_age_youngest)

##======================================================================================
##  "Cool" plots for the older ages 
##======================================================================================


for (j in 1:length(SIMPLELOGPOWER$individual.ages.retro.plot.info.simplelogpower)) {


    usePackage("stringr")
    
    SIMPLELOGPOWER$tmp_age <- names(SIMPLELOGPOWER$individual.ages.retro.plot.info.simplelogpower)[j]
    SIMPLELOGPOWER$tmp_age <- tolower(SIMPLELOGPOWER$tmp_age)
    SIMPLELOGPOWER$tmp_age <- str_replace_all(SIMPLELOGPOWER$tmp_age, "_", " ")
    SIMPLELOGPOWER$tmp_age

    SIMPLELOGPOWER$figurecaption <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                        SIMPLELOGPOWER$tmp_age, 
                        " component", 
                        " of ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                        " for the ",
                        SIMPLELOGPOWER$stockname,
                        " ",
                        SIMPLELOGPOWER$stockspecies,
                        ".",
                        " Historical values of ", tolower(SIMPLELOGPOWER$stockabundance), 
                        " (grey lines) ", 
                        "and fitted values produced by sibling regression", 
                        " are also shown. ", 
                        "Each panel corresponds to a particular retrospective forecasting year.")

    ## Figure 22 : 	Actual values (grey dots) and retrospectively forecasted values (red dots) of the age 2 component of terminal run 
    ## for the SPR chinook salmon stock, derived via naive modeling based on the average of terminal run from the previous 3 years. 
    ## Historical values of age 2 terminal run (grey lines) 
    ## and fitted values produced by the naive modeling (red lines) are also shown. 
    ## Each panel corresponds to a particular retrospective forecasting year.

    SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$individual.ages.retro.plot.simplelogpower(SIMPLELOGPOWER$individual.ages.retro.plot.info.simplelogpower, SIMPLELOGPOWER$stockabundance, j)

    doc = addPlot(doc,
                  fun=print,
                  x=SIMPLELOGPOWER$myplot,
                  width=plotwidth, 
                  height=plotheight)

    doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

    ## rm(myplot)
    SIMPLELOGPOWER$myplot <- NULL 
    SIMPLELOGPOWER$figurecaption <- NULL 

    ## rm(tmp_age)
    SIMPLELOGPOWER$tmp_age <- NULL 

}




doc = addPageBreak(doc)


addTitle(doc, "Density Plots of the Retrospective Forecast Errors", level=2)

#===============================================================================

##
## Density plots of retrospective forecast errors: Individual Ages
##

## if (plots==25){

## doc = addPageBreak(doc)

SIMPLELOGPOWER$figurecaption <- paste0("Density plots of the retrospective forecast errors corresponding to the ",
                        "individual age components of the ",
                        SIMPLELOGPOWER$forecastingyear,
                        " ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                        " for the ",
                        SIMPLELOGPOWER$stockname,
                        " ",
                        SIMPLELOGPOWER$stockspecies,
                        ".",
                        "For the youngest age, retrospective forecast errors were obtained via ",
                        SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$method,
                        ".",
                        "For the other ages, retrospective forecast errors were obtained on the basis of the ",
                        " corresponding simple log power regression models."
                        )

## myplot <- plot.hist.retrospective.forecast.errors.individual.ages.simplelogpower.regression(best.rmse.youngest.age.simplelogpower, stockabundance)

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.density.retrospective.forecast.errors.individual.ages.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower, 
                          SIMPLELOGPOWER$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 

## rm(myplot)

## }

##
## Density plot of retrospective forecast errors: Total Age
##

## if (plots==26){

doc = addPageBreak(doc)

SIMPLELOGPOWER$figurecaption <- paste0("Density plot of retrospective forecast errors corresponding to the ",
                        SIMPLELOGPOWER$forecastingyear,
                        " total ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                        " for the ",
                        SIMPLELOGPOWER$stockname,
                        " ",
                        SIMPLELOGPOWER$stockspecies,
                        "."
                        )

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.density.retrospective.forecast.errors.total.age.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower, 
                           SIMPLELOGPOWER$stockabundance)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight-2)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL

## }

#=================================================================================


##
## Bias Coefficient Plot for the Youngest Age 
## 

doc = addPageBreak(doc)

addTitle(doc, "Bias Coefficient Plots Derived from the Retrospective Forecast Errors", level=2)


SIMPLELOGPOWER$figurecaption <- paste0("Bias coefficient plot obtained from the retrospective forecast errors corresponding to the ",
                        "youngest age component of ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                        " for the ",
                        SIMPLELOGPOWER$stockname,
                        " ",
                        SIMPLELOGPOWER$stockspecies,
                        ".", sep="")

SIMPLELOGPOWER$results <- SIMPLELOGPOWER$best.rmse.youngest.age


SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$bias.coefficient.afe.youngest.age.retro.simplelogpower(SIMPLELOGPOWER$results, SIMPLELOGPOWER$stockabundance, SIMPLELOGPOWER$index.year)

## rm(results)

SIMPLELOGPOWER$results <- NULL 

doc = addPlot(doc,
        fun=grid.draw,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight 
        )

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 

##
## Bias Coefficient Plots for the Older Ages 
## 

doc = addPageBreak(doc)

SIMPLELOGPOWER$figurecaption <- paste0("Bias coefficient plot obtained from the retrospective forecast errors corresponding to the ",
                        "older age components of ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                        " for the ",
                        SIMPLELOGPOWER$stockname,
                        " ",
                        SIMPLELOGPOWER$stockspecies,
                        ".", sep="")


SIMPLELOGPOWER$fits <- SIMPLELOGPOWER$results_best_fitting_model_for_each_age_class_simplelogpower  ## fits
SIMPLELOGPOWER$results <- SIMPLELOGPOWER$individual.ages.retro.predictive.performance.simplelogpower.regression.youngest(
                             SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression, 
                             SIMPLELOGPOWER$index.year)  # results


SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$bias.coefficients.afe.older.ages.retro.simplelogpower(SIMPLELOGPOWER$fits, SIMPLELOGPOWER$results, SIMPLELOGPOWER$stockabundance)

## rm(fits)
## rm(results)

SIMPLELOGPOWER$fits <- NULL
SIMPLELOGPOWER$results <- NULL 

doc = addPlot(doc,
        fun=grid.draw,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight 
        )

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 

##
## Bias Coefficient Plot for the Total Age 
## 

doc = addPageBreak(doc)

SIMPLELOGPOWER$figurecaption <- paste0("Bias coefficient plot obtained from the retrospective forecast errors corresponding to the ",
                        "total ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                        " for the ",
                        SIMPLELOGPOWER$stockname,
                        " ",
                        SIMPLELOGPOWER$stockspecies,
                        ".", sep="")

## myplot <- plot.hist.retrospective.forecast.errors.total.age.simple.sibling.regression(best.rmse.youngest.age, stockabundance)

SIMPLELOGPOWER$results <- SIMPLELOGPOWER$best.rmse.youngest.age
SIMPLELOGPOWER$myplot <-  SIMPLELOGPOWER$bias.coefficient.afe.total.age.retro.simplelogpower(SIMPLELOGPOWER$results, SIMPLELOGPOWER$stockabundance)


## myplot <- ggplot(NULL, aes(rnorm(100),rnorm(100))) + geom_point(col="lightblue", size=3)


## rm(results)



doc = addPlot(doc,
        fun=grid.draw,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight 
        )

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$results <- NULL

##
## Gary's Plot: Individual Ages
##

## if (plots==27){

for (j in 1:length(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin)){

    doc = addPageBreak(doc)

    SIMPLELOGPOWER$age <- names(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$retro$resjoin)[j]

    usePackage("stringr")

    SIMPLELOGPOWER$age <- str_replace(SIMPLELOGPOWER$age, "_"," ")

    SIMPLELOGPOWER$age <- tolower(SIMPLELOGPOWER$age)

    if (j==1){

    SIMPLELOGPOWER$figurecaption <- paste0("Retrospective forecast errors and 80% interval forecast associated with the ",
                            SIMPLELOGPOWER$age,
                            " component of the ",
                            tolower(SIMPLELOGPOWER$stockabundance), " ",
                            "for the ",
                            SIMPLELOGPOWER$stockname, " ",
                            SIMPLELOGPOWER$stockspecies, " stock,",
                            " derived via ",
                            SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$method,
                            ". ",
                            "A positive forecast error indicates an under-forecast and a negative forecast error indicates an over-forecast.")
    } else if (j > 1) {

     SIMPLELOGPOWER$figurecaption <- paste0("Retrospective forecast errors and 80% interval forecast associated with the ",
                            SIMPLELOGPOWER$age,
                            " component of the ",
                            tolower(SIMPLELOGPOWER$stockabundance), " ",
                            "for the ",
                            SIMPLELOGPOWER$stockname, " ",
                            SIMPLELOGPOWER$stockspecies, " stock,",
                            " derived via ",
                            "simple log power regression",
                            ". ",
                            "A positive forecast error indicates an under-forecast and a negative forecast error indicates an over-forecast.")

    }


    SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$gary.plot.individual.ages.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower,
                                                                 SIMPLELOGPOWER$pred.int.individual.ages.avgfive.youngest,   # youngest age prediction: avgfive
                                                                 SIMPLELOGPOWER$pred.int.individual.ages.arima.youngest,     # youngest age prediction: arima
                                                                 SIMPLELOGPOWER$pred.int.individual.ages.expsmooth.youngest, # youngest age prediction: expsmooth
                                                                 SIMPLELOGPOWER$pred.int.individual.ages.simplelogpower.regression, # older ages prediction: log power regression (best model)
                                                                 SIMPLELOGPOWER$forecastingyear, 
                                                                 j)

    doc = addPlot(doc,
                  fun=plot, # print,
                  x=SIMPLELOGPOWER$myplot,
                  width=plotwidth+1, 
                  height=plotheight-2)

    doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")
    
    ## rm(myplot)

    SIMPLELOGPOWER$myplot <- NULL 
    SIMPLELOGPOWER$figurecaption <- NULL 



}

## }

##
## Gary's Plot: Total Age
##

## if (plots==28){

doc = addPageBreak(doc)

SIMPLELOGPOWER$figurecaption <- paste0("Retrospective forecast errors and 80% interval forecast associated with the ",
                            "total ",
                            tolower(SIMPLELOGPOWER$stockabundance), " ",
                            "for the ",
                            SIMPLELOGPOWER$stockname, " ",
                            SIMPLELOGPOWER$stockspecies, " stock,",
                            " derived via ",
                            "log power regression",
                            ".",
                            " A positive forecast error indicates an under-forecast and a negative forecast error indicates an over-forecast.")

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$gary.plot.total.age.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower,
                                               SIMPLELOGPOWER$pred.int.total.age.simplelogpower.regression.all.models,  # total age prediction
                                               SIMPLELOGPOWER$forecastingyear)

doc = addPlot(doc,
              fun= plot, # print,
              x=SIMPLELOGPOWER$myplot,
              width=plotwidth+1, 
              height=plotheight-2)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$figurecaption <- NULL 
SIMPLELOGPOWER$myplot <- NULL 

## }


#===============================================================================

##
## Retrospective Forecasts vs. Observed Values: Individual Ages
##

#===============================================================================

## if (plots==29){

## doc = addPageBreak(doc)


doc = addPageBreak(doc)

doc = addTitle(doc, "Forecast Diagnostics", level=1)

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- paste("The forecast diagnostics included in this report are visual in nature and include:")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")


doc = addParagraph( doc, value = paste0('Superimposed time series plots displaying the retrospective point forecasts against the actual historical values;'),
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = paste0('Scatter plots displaying the retrospective point forecasts against the actual historical values.'),
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")

SIMPLELOGPOWER$paragraph <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste("The purpose of the forecast diagnostics is to enable a visual assessment of how well the retrospective point forecasts ", 
                   "track the actual historical values of age-specific and total ", 
                   tolower(SIMPLELOGPOWER$stockabundance), ". ", 
                   "These diagnostics will reveal issues with the particular forecasting models used (in this case, simple log power regression models), such as a ", 
                   "tendency for the models to under-forecast or over-forecast over time.", sep="")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 

doc = addSection(doc, landscape = TRUE, ncol = 1) 

##
## Superimposed Time Series Plots of Retrospective Forecasts and Actual Values: Individual Ages 
##

## doc = addPageBreak(doc)

SIMPLELOGPOWER$figurecaption <- paste0("Superimposed time series plots of  ",
                       "retrospectively forecasted and actual age-specific ",
                       tolower(SIMPLELOGPOWER$stockabundance), " values",
                       " for the ",
                       SIMPLELOGPOWER$stockname, " ",
                       SIMPLELOGPOWER$stockspecies, " stock.")


SIMPLELOGPOWER$results <- SIMPLELOGPOWER$best.rmse.youngest.age

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$timeseries.plot.results.afe.individual.ages.retro.simplelogpower(SIMPLELOGPOWER$results,
                                                             SIMPLELOGPOWER$stockabundance)

## rm(results)

SIMPLELOGPOWER$results <- NULL 


doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth + 4, 
        # height=plotheight
        height=plotheight-1 
        )

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 

doc = addSection(doc, landscape = FALSE, ncol = 1) 

##
## Scatter Plots of Retrospective Forecasts and Actual Values: Individual Ages 
##


SIMPLELOGPOWER$figurecaption <- paste0("Scatter plots of retrospectively forecasted ",
                       " versus actual ",
                       tolower(SIMPLELOGPOWER$stockabundance), " values",
                       " for specific age components of the ",
                       SIMPLELOGPOWER$stockname, " ",
                       SIMPLELOGPOWER$stockspecies, " stock.",
                       " Observations are labeled according to the associated historical return years.",
                       " For the youngest age, the retrospectively forecasted ",
                       tolower(SIMPLELOGPOWER$stockabundance),
                       " values were obtained via ",
                        SIMPLELOGPOWER$best.rmse.youngest.age$method,
                        ".",
                        " For the other ages, the retrospectively forecasted ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                       " values ",
                        "were obtained on the basis of the ",
                        " simple log power regression models corresponding to those ages." )
                        
SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.results.afe.individual.ages.retro.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth, 
        ## height=plotheight
        height = plotheight 
        )

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 

##
## Superimposed Time Series Plots of Retrospective Forecasts and Actual Values: Total Age
##


doc = addPageBreak(doc)

SIMPLELOGPOWER$figurecaption <- paste0("Superimposed time series plots of  ",
                       " retrospectively forecasted and actual total ",
                       tolower(SIMPLELOGPOWER$stockabundance), " values",
                       " for the ",
                       SIMPLELOGPOWER$stockname, " ",
                       SIMPLELOGPOWER$stockspecies, " stock.")

SIMPLELOGPOWER$results <- SIMPLELOGPOWER$best.rmse.youngest.age

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$timeseries.plot.results.afe.total.age.retro.simplelogpower(SIMPLELOGPOWER$results, SIMPLELOGPOWER$stockabundance)

## rm(results)

doc = addPlot(doc,
        fun=print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth, 
        # height=plotheight
        height=plotheight - 2
        )

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 


#===============================================================================

## best.rmse.youngest.age.simplelogpower <-  best.rmse.results.youngest.age(rmse.results.youngest.age.avgfive.plus.simplelogpower.regression,
##                                                          rmse.results.youngest.age.arima.plus.simplelogpower.regression,
##                                                          rmse.results.youngest.age.expsmooth.plus.simplelogpower.regression)


SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower


SIMPLELOGPOWER$figurecaption <- paste0("Retrospectively forecasted ",
                       "versus actual ",
                       tolower(SIMPLELOGPOWER$stockabundance), " values",
                       " for specific age components of the ",
                       SIMPLELOGPOWER$stockname, " ",
                       SIMPLELOGPOWER$stockspecies, " stock.",
                       " Observations are labeled according to the associated historical return years.",
                       " For the youngest age, the retrospectively forecasted ",
                       tolower(SIMPLELOGPOWER$stockabundance),
                       " values were obtained via ",
                        SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower$method,
                        ".",
                        " For the other ages, the retrospectively forecasted ",
                        tolower(SIMPLELOGPOWER$stockabundance),
                       " values ",
                        "were obtained on the basis of the ",
                        " simple log power regression models corresponding to these ages.", sep="")
                        
SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.results.afe.individual.ages.retro.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower)

doc = addPlot(doc,
        fun=plot, # print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL 

## }

##
## Retrospective Forecasts vs. Observed Values: Total Age
##

## if (plots==30){

doc = addPageBreak(doc)

SIMPLELOGPOWER$figurecaption <- paste0("Retrospectively forecasted ",
                       " versus actual total ",
                       tolower(SIMPLELOGPOWER$stockabundance), " values",
                       " for the ",
                       SIMPLELOGPOWER$stockname, " ",
                       SIMPLELOGPOWER$stockspecies, " stock.",
                       " Observations are labeled according to the associated historical return years.")

SIMPLELOGPOWER$myplot <- SIMPLELOGPOWER$plot.results.afe.total.age.retro.simplelogpower.regression(SIMPLELOGPOWER$best.rmse.youngest.age.simplelogpower, SIMPLELOGPOWER$stockabundance)

doc = addPlot(doc,
        fun=plot, # print,
        x=SIMPLELOGPOWER$myplot,
        width=plotwidth+1, height=plotheight-2)

doc = addParagraph(doc, value=SIMPLELOGPOWER$figurecaption, stylename="rPlotLegend")

## rm(myplot)

SIMPLELOGPOWER$myplot <- NULL 
SIMPLELOGPOWER$figurecaption <- NULL  


## }

##
## Retrospective R Squared Values:  All Stock Components
##

## doc = addPageBreak(doc)

## tabledata <- r.squared.retro.simplelogpower.regression(best.rmse.youngest.age.simplelogpower)

## names(tabledata) <- c("Stock Component", "Model Class", "R-Squared")

## tablecaption <- paste0("R-squared values obtained by regressing retrospectively forecasted ",
##                       tolower(stockabundance), " ",
##                       "values on historical ",
##                       tolower(stockabundance), " ",
##                       "for the ",
##                       stockname, " ",
##                       stockspecies,
##                       " stock."
##                       )
## 

## doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

## ft_tabledata <- FlexTable(data=tabledata, header.columns=TRUE)

## doc = addFlexTable(doc, flextable=ft_tabledata,
##                 layout.properties=tableProperties(data.par=parProperties(text.align="right",padding=2),
##                                                   data.cell=cellProperties(border.color="black")))
## doc = addParagraph(doc, value=" ", stylename="Normal")

                                    

#=========================================================================================================
# Data exploration for the simple log power regression models
#=========================================================================================================

## addTitle(doc, "Data Exploration for All Log Power Regression Models", level=1)

addTitle(doc, "Data Exploration for the Simple Log Power Regression Models", level=1)


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("This section includes exploratory data plots which can be used in conjunction with the ",
                   "simple log power regression models considered for forecasting ", 
                   SIMPLELOGPOWER$forecastingyear, " ", 
                   tolower(SIMPLELOGPOWER$stockabundance), " ", 
                   "for the ", 
                   SIMPLELOGPOWER$stockspecies, " ",
                   toupper(SIMPLELOGPOWER$stockname),
                   " stock. It is recommended that these plots be examined before consulting the summary output produced by these models.",    
                    sep=" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragrah <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("The exploratory data plots considered for each simple sibling regression model include ",
                   "scatter plots of log-transformed Age k and log-transformed Age k - 1 ",
                   tolower(SIMPLELOGPOWER$stockabundance), ", ", 
                   "along with ", 
                   "the accompanying Pearson correlation coefficient describing the ", 
                   "direction and strength of the linear association between these two log-transformed variables. ",
                   " Additionally, density plots of the log-transformed Age k and log-transformed Age k - 1 ",
                    tolower(SIMPLELOGPOWER$stockabundance), " are also shown. ", 
                   " For situations where the historical data available on Age k and/or Age k - 1 ",
                    tolower(SIMPLELOGPOWER$stockabundance), 
                   " includes zero counts, the (natural) log transformation is replaced with a log1p transformation. ",
                   " The log1p transformation is given by log1p(x) = log(1 + x), where log is the natural log transformation and x is greater than or equal to 0.",       
                    sep=" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragrah <- NULL 


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("The simple log power regression model which relates the log-transformed Age k to the log-transformed Age k - 1", " ", 
                    tolower(SIMPLELOGPOWER$stockabundance), 
                    " relies on the assumption that the log-transformed Age k ", tolower(SIMPLELOGPOWER$stockabundance), " ", 
                    "is linearly related to the log-transformed ", 
                    "Age k-1 ", tolower(SIMPLELOGPOWER$stockabundance), ". ", 
                    "The reasonableness of this assumption for the underlying data used to fit this model ", 
                    "can be verified by examining the scatterplot of the log-transformed Age k versus the log-transformed Age k - 1 ", 
                    tolower(SIMPLELOGPOWER$stockabundance), ". ", 
                    "If the scatterplot reveals a cloud of points which seem to follow either an upward or a downward trend, ", 
                    "then the linearity assumption is supported by the data represented in the scatterplot. ", 
                    "Otherwise, the linearity assumption is violated by the data.",     
                    sep=" ")
doc = addParagraph(doc, paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("Another assumption required by the simple log power regression model which relates the log-transformed Age k to the log-transformed Age k - 1", " ", 
                    tolower(SIMPLELOGPOWER$stockabundance), 
                    " is that of constant variability of the model errors. ",  
                    "This assumption can be verified by examining the scatterplot of the log-transformed Age k versus the log-transformed Age k - 1", 
                    tolower(SIMPLELOGPOWER$stockabundance), " ", 
                    "and making sure that it doesn't display a cloud of points which follow a funnel-like pattern, ",
                    "which would be indicative of a violation of the constant error variability assumption. ", 
                    "If the variability of the points about the underlying upward or downward trend is constant as we move along the x-axis, ",
                    "its magnitude will indicate how precise the estimation of the true underlying trend will be. ",
                    "The smaller the variability, the more precise the estimation of the true trend. ",
                    "Conversely, the larger the variability, the more imprecise that estimation. ",
                    "The precision of point forecasts produced by the model will also be affected by the amount of ",
                    "variability present in the data - the smaller (larger) the variability, the more (less) precise the point forecasts.",           
                    sep=" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")

SIMPLELOGPOWER$paragraph <- NULL 

SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("The scatterplot of the log-transformed Age k versus the log-transformed Age k - 1 ", 
                     tolower(SIMPLELOGPOWER$stockabundance), " ", 
                    "may reveal other problems with the data it depicts, including ", 
                    "gaps in the data", " or unusually large or small ",  
                     tolower(SIMPLELOGPOWER$stockabundance), 
                    " values along its x-axis and/or or y-axis (known as x-outliers and/or y-outliers). ", 
                    "Such problems should be noted and their potential effect on the fit of the corresponding ",
                    "simple sibling regression model should be tracked. ",
                    "As an example of such tracking, will any unusual observations identified in the scatterplot ", 
                    "translate into influential observations, which could significantly alter the model fit?",            
                    sep=" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("The Pearson correlation coefficient which quantifies the direction and strength of ",
                    "the linear association between the log-transformed Age k and the log-transformed Age k - 1 ", 
                    tolower(SIMPLELOGPOWER$stockabundance), 
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


SIMPLELOGPOWER$paragraph <- NULL 

# SIMPLELOGPOWER$paragraph <- paste(" ")
# doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
#                   par.properties=parProperties(text.align="justify"))

SIMPLELOGPOWER$paragraph <- paste0("While the simple log power regression model does not require the normality of the log-transformed Age k and log-transformed Age k - 1 ",
                   tolower(SIMPLELOGPOWER$stockabundance), " data ", 
                   "included in the model, ",  
                   "examining the distribution of these variables via density plots can reveal problems with the data that may ",
                   "conspire to affect the model fit (e.g., multi-modality, skewness, outliers).  These problems should be noted and their ", 
                   "potential impact on the model fit should be monitored when examining model diagnostic plots.",       
                    sep=" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")


SIMPLELOGPOWER$paragraph <- paste(" ")
doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

# doc = addParagraph(doc, SIMPLELOGPOWER$paragraph, stylename="Normal")



SIMPLELOGPOWER$panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.6/strwidth(txt)   # cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

## if (plots==31){

## myezCor(mydata)

SIMPLELOGPOWER$fitssimplelogpower <-  SIMPLELOGPOWER$simplelogpower_regression_model_fits(SIMPLELOGPOWER$data_and_model_formulas_simplelogpower)



SIMPLELOGPOWER$fitssimplelogpower$model_data
SIMPLELOGPOWER$fitssimplelogpower$model_formulas
SIMPLELOGPOWER$fitssimplelogpower$model_fits


for (i in 1:length(SIMPLELOGPOWER$fitssimplelogpower$model_fits)) {

    ## for (j in 1:length(SIMPLELOGPOWER$fitssimplelogpower$model_formulas[[i]]$model_formulas)) {

        SIMPLELOGPOWER$forms <- SIMPLELOGPOWER$fitssimplelogpower$model_formulas[[i]]$model_formulas
        SIMPLELOGPOWER$dats <- SIMPLELOGPOWER$fitssimplelogpower$model_data[[i]]

        ## extract predictor variables
        usePackage("formula.tools")
        ## var <- attr(model$terms, "term.labels")
        SIMPLELOGPOWER$vars <- rhs.vars(SIMPLELOGPOWER$forms)
        ## extract response variable
        SIMPLELOGPOWER$resp <- lhs.vars(SIMPLELOGPOWER$forms)
        SIMPLELOGPOWER$resp <- attr(SIMPLELOGPOWER$resp,"term.labels")


        ## if predictor variable includes notation I()
        gr <- grep("\\bI\\b",SIMPLELOGPOWER$vars, value=T)

        #### if (length(gr) == 0) {
        ####    mydata <- subset(dats, select=c(resp,vars))
        #### } else {

        #### vars
        #### vars <- gsub("I", "", vars)
        #### vars

        #### vars <- gsub("\\(|\\)", "", vars)

        #### vars

        #### dats <- plyr::mutate(dats, I=eval(parse(text=vars)))
        #### names(dats)[names(dats)=="I"] <- paste(vars)
        #### mydata <- subset(dats, select=c(resp,vars))
        #### }


        ## pp <- myezCor(mydata)
        ## 
        ## doc = addPlot(doc,
        ## fun = print,
        ## x=pp,
        ## width=plotwidth+1, height=plotwidth+1)

        SIMPLELOGPOWER$plot_function <- function(){
                   formula.pairs <- paste(" ~ ", SIMPLELOGPOWER$resp, "+ ", paste(SIMPLELOGPOWER$vars, collapse=" + "))
                   formula.pairs <- formula(formula.pairs)
                   ## pairs(mydata, lower.panel=panel.cor)
                   pairs(formula.pairs, lower.panel=SIMPLELOGPOWER$panel.cor, data=SIMPLELOGPOWER$dats)
                  }
                                                                      
        
        doc = addPlot( doc = doc,
              fun = print(SIMPLELOGPOWER$plot_function),
              width = plotwidth+1, height = plotwidth+1,
              vector.graphic=F
         )



        doc = addParagraph(doc, value=paste0("Scatter plot of the response variable against the predictor variable included the simple log power regression model whose formula is given by ",
                                            as.character(SIMPLELOGPOWER$forms), "."), stylename="rPlotLegend")


    ## }

}

## }



for (i in 1:length(SIMPLELOGPOWER$fitssimplelogpower$model_fits)) {


        SIMPLELOGPOWER$forms <- SIMPLELOGPOWER$fitssimplelogpower$model_formulas[[i]]$model_formulas
        SIMPLELOGPOWER$dats <- SIMPLELOGPOWER$fitssimplelogpower$model_data[[i]]

        ## extract predictor variables
        usePackage("formula.tools")
        ## var <- attr(model$terms, "term.labels")
        SIMPLELOGPOWER$vars <- rhs.vars(SIMPLELOGPOWER$forms)
        ## extract response variable
        SIMPLELOGPOWER$resp <- lhs.vars(SIMPLELOGPOWER$forms)
        SIMPLELOGPOWER$resp <- attr(SIMPLELOGPOWER$resp,"term.labels")


        ## if predictor variable includes notation I()
        gr <- grep("\\bI\\b",SIMPLELOGPOWER$vars, value=T)

      

        SIMPLELOGPOWER$plot_function <- function(){
        
                   formula.1 <- paste(SIMPLELOGPOWER$resp) ## response transformation
                   formula.2 <- paste(SIMPLELOGPOWER$vars) ## predictor transformation
                   
                   transf.1 <- strsplit(formula.1, "\\(")[[1]][1]
                   transf.2 <- strsplit(formula.2, "\\(")[[1]][1]
                   
                   if (transf.1=="log") { resp.var <- log(SIMPLELOGPOWER$dats[,3]) }
                   if (transf.1=="log1p") { resp.var <- log1p(SIMPLELOGPOWER$dats[,3])}
                   
                   if (transf.2=="log") {pred.var <- log(SIMPLELOGPOWER$dats[,5])}
                   if (transf.2=="log1p") {pred.var <- log1p(SIMPLELOGPOWER$dats[,5])}
                   
                   par(mfrow=c(1,2))
                   
                   d2 <- density(pred.var)
                   plot(d2, las=1, main=formula.2 )
                   polygon(d2, col="lightgrey", border="black") 
                   
                   d1 <- density(resp.var)
                   plot(d1, las=1, main=formula.1)
                   polygon(d1, col="grey", border="black") 
                   
                  }
                                                                      
        
        doc = addPlot( doc = doc,
              fun = print(SIMPLELOGPOWER$plot_function),
              width = plotwidth + 1, height = plotheight - 2,
              vector.graphic=F
         )



        doc = addParagraph(doc, value=paste0("Density plots for the outcome variable and predictor variable in the simple log power regression model whose formula is given by ",
                                            as.character(SIMPLELOGPOWER$forms), "."), stylename="rPlotLegend")


    ## }

}

## }

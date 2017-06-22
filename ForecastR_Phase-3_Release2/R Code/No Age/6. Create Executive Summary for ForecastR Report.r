plotwidth <- 6
plotheight <- 7

## usePackage("ReporteRs")

## options("ReporteRs-fontsize" = 10,
##        "ReporteRs-default-font" = "Calibri")

## assign the template
## doc <- docx(template = "Template_ReporteRs.docx")


#========================================================================================================
# CoverPage
#========================================================================================================

## doc = addParagraph(doc, "ForecastR Output", stylename = "Normal" )

pot1 = pot("Executive Summary", textProperties(font.weight="bold", font.size = 20) )
my.pars = set_of_paragraphs(pot1)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot1 = pot(" ", textProperties(font.weight="bold", font.size = 10) )
my.pars = set_of_paragraphs(pot1)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 10) ) +
        pot(paste(stockname), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot2)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 10) ) +
        pot(paste(stockspecies), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot3)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 10) ) +
        pot(paste(stockabundance), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot4)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


pot5a =  pot("Historical Run Years: ", textProperties(font.weight="bold", font.size = 10) ) + 
        pot(paste(range(datalist$Run_Year)[1],"-",range(datalist$Run_Year)[2]), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot5a)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

pot5b =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 10) ) + 
        pot(paste(forecastingyear), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot5b)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


if (length(pred.args)>1) {
  pot6 =  pot("Forecasting Models: ", textProperties(font.weight="bold", font.size = 10))
  my.pars = set_of_paragraphs(pot6)
  doc = addParagraph( doc, value = my.pars, stylename="Normal" )
} else if (length(pred.args)==1) {
  pot6 =  pot("Forecasting Model: ", textProperties(font.weight="bold", font.size = 10))
  my.pars = set_of_paragraphs(pot6)
  doc = addParagraph( doc, value = my.pars, stylename="Normal" )
}



for (k in 1:length(pred.args)) {

    tmplabel <- class(pred.args[[k]])
    if (tmplabel=="naiveone") {
              mypot <- "Naive Model (Previous Year)"
    } else if (tmplabel=="avgthree") {
              mypot <- "Naive Model (Average of Previous 3 Years)"
    } else if (tmplabel=="avgfive") {
              mypot <- "Naive Model (Average of Previous 5 Years)"
    } else if (tmplabel=="arima") {
              mypot <- "ARIMA Model"
    } else {
              mypot <- "Exponential Smoothing Model"
    }

    pot.model =  pot(mypot, textProperties(font.size = 10) )
    my.pars = set_of_paragraphs(pot.model)
    doc = addParagraph( doc, value = my.pars, stylename="BulletList" )

}


if (length(names(retromeasure.args))>1) {
   pot6 =  pot("Forecast Performance Measures: ", textProperties(font.weight="bold", font.size = 10))
   my.pars = set_of_paragraphs(pot6)
   doc = addParagraph( doc, value = my.pars, stylename="Normal" )
} else if (length(names(retromeasure.args))==1) {
   pot6 =  pot("Forecast Performance Measure: ", textProperties(font.weight="bold", font.size = 10))
   my.pars = set_of_paragraphs(pot6)
   doc = addParagraph( doc, value = my.pars, stylename="Normal" )
}


usePackage("stringr")
measures <- str_replace_all(string=names(retromeasure.args), pattern="retromeasure", replacement="")


for (k in 1:length(retromeasure.args)) {

    pot.measures =  pot(measures[k], textProperties(font.size = 10) )
    my.pars = set_of_paragraphs(pot.measures)
    doc = addParagraph( doc, value = my.pars, stylename="BulletList" )

}

if (length(pred.args)>1) {

    pottmp =  pot("Best Forecasting Model: ", textProperties(font.weight="bold", font.size = 10))
    my.pars = set_of_paragraphs(pottmp)
    doc = addParagraph( doc, value = my.pars, stylename="Normal" )

    BestForecastingModel <- table.rank.rmse.results.no.age$best.model
    if (BestForecastingModel=="naiveone") {
        BestForecastingModel <- "Naive Model (Previous Year)"
    } else if (BestForecastingModel=="avgthree") {
        BestForecastingModel <- "Naive Model (Average of Previous 3 Years)"
    } else if (BestForecastingModel=="avgfive") {
        BestForecastingModel <- "Naive Model (Average of Previous 5 Years)"
    } else if (BestForecastingModel=="arima") {
        BestForecastingModel <- "ARIMA Model"
    } else if (BestForecastingModel=="expsmooth") {
        BestForecastingModel <- "Exponential Smoothing Model"
    }


    pot.best =  pot(BestForecastingModel, textProperties(font.size = 10) )
    my.pars = set_of_paragraphs(pot.best)
    doc = addParagraph( doc, value = my.pars, stylename="BulletList" )

}


pot7 =  pot("Date: ", textProperties(font.weight="bold", font.size = 10) ) +
        pot(paste(Sys.Date()), textProperties(font.size = 10) )
my.pars = set_of_paragraphs(pot7)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )


doc = addPageBreak(doc)

#========================================================================================================
# Table of Contents
#========================================================================================================

## pottoc =  pot("Table of Contents", textProperties(font.weight="bold", font.size = 13) )
## my.pars = set_of_paragraphs(pottoc)
## doc = addParagraph( doc, value = my.pars, stylename="Normal" )

## Add a table of contents
## doc <- addTOC(doc)

## doc <- addPageBreak(doc) # go to the next page


#========================================================================================================
# Summary of Results
#========================================================================================================

doc = addTitle(doc, "Summary of Forecasting Results", level=1)

usePackage("stringr")
usePackage("scales")
       
Summary_Row_0 <- "Item"
for (k in 1:length(pred.args)){

    tmplabel <- class(pred.args[[k]])  
    
    if (tmplabel=="naiveone") {
              mytitle <- "Naive Model"
    } else if (tmplabel=="avgthree") {
              mytitle <- "Naive Model"
    } else if (tmplabel=="avgfive") {
              mytitle <- "Naive Model"
    } else if (tmplabel=="arima") {
              mytitle <- "ARIMA Model"
    } else {
              mytitle <- "Exponential Smoothing Model"
    }

    Summary_Row_0 <- c(Summary_Row_0, mytitle)

}
    
                           
Summary_Row_1 <- "Return Year"
for (k in 1:length(pred.args)){
     Summary_Row_1 <- c(Summary_Row_1, forecastingyear)
}


Summary_Row_2 <- "Model Description"
for (k in 1:length(pred.args)){ 

    tmplabel <- class(pred.args[[k]])  
    
    if (tmplabel=="naiveone") {
              mytitle <- "Previous Year"
    } else if (tmplabel=="avgthree") {
              mytitle <- "Average of Previous 3 Years"
    } else if (tmplabel=="avgfive") {
              mytitle <- "Average of Previous 5 Years"
    } else if (tmplabel=="arima") {
              ## fit.arima.model.no.age
              sink("arimafit.txt")
              print(fit.arima.model.no.age)
              sink()
              out <- readLines("arimafit.txt")
              out.pattern <- str_detect(string=out, pattern="ARIMA")
              modelarima <- out[out.pattern==TRUE]
              modelarima <- str_trim(modelarima)
              mytitle <- modelarima
    } else {
              ## fit.expsmooth.model.no.age
              sink("expsmoothfit.txt")
              print(fit.expsmooth.model.no.age)
              sink()
              out <- readLines("expsmoothfit.txt")
              out.pattern <- str_detect(string=out, pattern="ETS")
              modelexpsmooth <- out[out.pattern==TRUE]
              modelexpsmooth <- str_trim(modelexpsmooth)
              mytitle <- modelexpsmooth
    }
 
    Summary_Row_2 <- c(Summary_Row_2, mytitle)

}


# Point Forecast
Summary_Row_3 <- "Point Forecast"
for (k in 1:length(pred.args)){

      Summary_Row_3 <- c(Summary_Row_3, comma(round(pred.args[[k]]$PI.ctr)))
}

       
# Interval Forecast
Summary_Row_4 <- "Interval Forecast" 
for (k in 1:length(pred.args)){

      Summary_Row_4 <- c(Summary_Row_4, 
                          paste0(comma(round(pred.args[[k]]$PI.lwr)), 
                          " - ",
                          comma(round(pred.args[[k]]$PI.upr))))

}


Summary_Row <- rbind.data.frame(Summary_Row_0, Summary_Row_1, Summary_Row_2, Summary_Row_3, Summary_Row_4)
Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)

# MRE
Summary_Row_5 <- "MRE"
if (retromeasureMRE==TRUE) {
for (k in 1:length(pred.args)){

     Summary_Row_5 <- c(Summary_Row_5, comma(round(rmse.results.no.age.args[[k]]$mre,2)) )

}                 
}
                 
 
# MAE              
Summary_Row_6 <- "MAE"
if (retromeasureMAE==TRUE) {
for (k in 1:length(pred.args)){

     Summary_Row_6 <- c(Summary_Row_6, comma(round(rmse.results.no.age.args[[k]]$mae,2)) )

}      
}

# MPE
Summary_Row_7 <- "MPE"
if (retromeasureMPE==TRUE){
for (k in 1:length(pred.args)){

     Summary_Row_7 <- c(Summary_Row_7, comma(round(rmse.results.no.age.args[[k]]$mpe,2)) )

}      
}

# MAPE
Summary_Row_8 <- "MAPE"
if (retromeasureMAPE==TRUE){
for (k in 1:length(pred.args)){
     Summary_Row_8 <- c(Summary_Row_8, comma(round(rmse.results.no.age.args[[k]]$mape,2)) )
}      
}

# MASE
Summary_Row_9 <- "MASE"
if (retromeasureMASE==TRUE) {
for (k in 1:length(pred.args)){
     Summary_Row_9 <- c(Summary_Row_9, comma(round(rmse.results.no.age.args[[k]]$mase,2)) )

}                       
} 

# RMSE
Summary_Row_10 <- "RMSE"  
if (retromeasureRMSE==TRUE){               
for (k in 1:length(pred.args)){

     Summary_Row_10 <- c(Summary_Row_10, comma(round(rmse.results.no.age.args[[k]]$rmse,2)) )

}           
}



what <- Summary_Row_5
what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)

what <- Summary_Row_6
what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)

what <- Summary_Row_7
what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)

what <- Summary_Row_8
what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)

what <- Summary_Row_9
what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)

what <- Summary_Row_10
what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)

         
# Average Rank
Summary_Row_11 <- c("Model-Specific Average Rank", table.rank.rmse.results.no.age$avg.rank)

what <- Summary_Row_11
what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)

         
# R-Squared
## r.sq.values <- r.squared.retro.individual.stock.all.models.no.age$r.squared
## r.sq.values <- data.frame(lapply(r.sq.values, as.character), stringsAsFactors=FALSE)
## r.sq.values <- as.character(r.sq.values[1, ])

## Summary_Row_12 <- c("R-squared", r.sq.values)

## what <- Summary_Row_12
## what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
## Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
## Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)


row.has.na <- apply(Summary_Row, 1, function(x){any(is.na(x))})
Summary_Row <- Summary_Row[!row.has.na,]




tablecaption <- paste0("Summary of forecasting results for the ",
                       forecastingyear, " ",
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ",
                       stockspecies, " stock. ", 
                       ifelse(length(pred.args)>1,"Results corresponding to the best forecasting model are highlighted in yellow colour.",
                                                  "Results corresponding to the only forecasting model considered for this forecasting exercise are highlighted in yellow colour."))

doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

usePackage("ReporteRs")

# set cell padding defaut to 2

tt <- Summary_Row

my_ft <- FlexTable( data = tt,
                    header.columns = FALSE,
                    body.cell.props = cellProperties( padding = 2 ),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties

my_ft[1, 1] = textProperties(font.weight = "bold")

my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

## my_ft[,"Best Model"] = parProperties(text.align = "left")

## my_ft[1,"Best Model"] = textProperties(font.weight = "bold")


best.index.no.age <- table.rank.rmse.results.no.age$index.min.avg.rank

my_ft[ , 1+best.index.no.age] = cellProperties( background.color = "yellow" )

print(1+best.index.no.age)

doc = addFlexTable(doc, flextable=my_ft)


doc = addParagraph(doc,paste(" "), stylename="Normal")

if (bootmethod=="meboot") {
  
   paragraph <- paste0("For the forecasting model(s) considered in this report, the interval forecast derivation ",
                   "was performed by applying maximum entropy bootstrapping to the historical time series of abundance. ",
                   "B = ", B, " bootstrap replicates were used per forecasting model.")
  
   doc = addParagraph(doc,paragraph, stylename="Normal")
   
}

if (bootmethod=="stlboot") {
  
   paragraph <- paste0("For the forecasting model(s) considered in this report, the interval forecast derivation ",
                   "was performed by applying loess bootstrapping to the historical time series of abundance. ",
                   "B = ", B, " bootstrap replicates were used per forecasting model.")
  
   doc = addParagraph(doc,paragraph, stylename="Normal")
   
}


doc = addPageBreak(doc)

#================================================================================================
# Point and Interval Forecasts for the Best Forecasting Model
#================================================================================================

tablelegend <- paste0("Point and interval forecasts of ", tolower(stockabundance), " ",
                      "corresponding to the ",
                      stockname, " ",
                      stockspecies, " stock ",
                      "and the ", forecastingyear, " forecasting year.")
doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend")


tt <- table.results.individual.stock.all.models.no.age
tt$Model <- as.character(tt$Model)

names_tt <- character(length(tt$Model))
for (k in 1:length(tt$Model)){
     if (tt$Model[k]=="Naive Model (Previous Year)") {
         names_tt[k] <- "naiveone"
     } else if (tt$Model[k]=="Naive Model (Average of Previous Three Years)") {
         names_tt[k] <- "avgthree"
     } else if (tt$Model[k]=="Naive Model (Average of Previous Five Years)") {
         names_tt[k] <- "avgfive"
     } else if (tt$Model[k]=="ARIMA Model") {
         names_tt[k] <- "arima"
     } else if (tt$Model[k]=="Exponential Smoothing Model") {
         names_tt[k] <- "expsmooth"
     }

}

tt$Label <- names_tt

tt_sub <- subset(tt, Label == table.rank.rmse.results.no.age$best.model)
tt_sub <- tt_sub[,-ncol(tt_sub)]

if (length(pred.args)>1) {
    names(tt_sub)[names(tt_sub)=="Model"] <- "Best Forecasting Model"
}

# set cell padding defaut to 2
# baseCellProp = cellProperties(padding = 5)
# Create a FlexTable with tt data frame
my_ft = FlexTable(data = tt_sub,
                  body.cell.props = cellProperties(padding = 5),
                  ## header.par.props = parProperties(text.align = "right"),
                  header.cell.props=cellProperties(background.color="#DDDDDD", padding=5),
                  add.rownames = FALSE
)


## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

my_ft[, 1, ] = parProperties(text.align = "left")
my_ft[, 2:ncol(tt_sub)] = parProperties(text.align = "center")

# my_ft[, 1, to="header"] = parProperties(text.align = "left")
# to="header"
# my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

doc = addFlexTable(doc, my_ft)

doc = addParagraph(doc, value=" ", style="Normal")


##
## Empirical Probabilities: Best Model (Distribution of Bootstrapped Point Forecasts)
##

## doc = addPageBreak(doc)

#====================================================================================================================
#
# Empirical Probabilities for Stock With No Age Information: Best Model
#
#====================================================================================================================

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts
###

## MANGO

## list_of_models_no_age <- c("Naive Model (Previous Year)",
##                           "Naive Model (Average of Previous 3 Years)",
##                           "Naive Model (Average of Previous 5 Years)",
##                           "ARIMA Model",
##                           "Exponential Smoothing Model")
## best.index.no.age <- table.rank.rmse.results.no.age$index.min.avg.rank
##
## best.model.no.age <- table.rank.rmse.results.no.age$best.model
##
## best.model.no.age <- tolower(best.model.no.age)

best.model.no.age <- table.rank.rmse.results.no.age$best.model

if (best.model.no.age=="naiveone"){ 
    best.model.no.age <- "Naive Model (Previous Year)"
} else if (best.model.no.age=="avgthree"){
    best.model.no.age <- "Naive Model (Average of Previous 3 Years)"
} else if (best.model.no.age=="avgfive") {
    best.model.no.age <- "Naive Model (Average of Previous 5 Years)"
} else if (best.model.no.age=="arima") {
    best.model.no.age <- "ARIMA Model"
} else if (best.model.no.age=="expsmooth") {
    best.model.no.age <- "Exponential Smoothing Model"
} 

best.model.no.age <- tolower(best.model.no.age)


# pred.int.individual.stock.naiveone.no.age
# pred.int.individual.stock.avgthree.no.age
# pred.int.individual.stock.avgfive.no.age
# pred.int.individual.stock.arima.no.age
# pred.int.individual.stock.expsmooth.no.age

empirical.probability.yboot.best.model.no.age <- function(pred.args,
                                                             best.index.no.age,
                                                             stockabundance){



     y.star.boot.stacked <- pred.args[[best.index.no.age]]$sim

     tmplabel <- class(pred.args[[best.index.no.age]])

     if (tmplabel=="naiveone") {
              mylabel <- "Naive Model (Previous Year)"
     } else if (tmplabel=="avgthree") {
              mylabel <- "Naive Model (Average of Previous 3 Years)"
     } else if (tmplabel=="avgfive") {
              mylabel <- "Naive Model (Average of Previous 5 Years)"
     } else if (tmplabel=="arima") {
              mylabel <- "ARIMA Model"
     } else {
              mylabel <- "Exponential Smoothing Model"
     }

     ## mylabel <- paste0(stockabundance)

     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked, stringsAsFactors=FALSE)
     data.stacked$labels <- factor(data.stacked$labels, levels=unique(data.stacked$labels))

     pfct <- pred.args[[best.index.no.age]]$PI.ctr ## point forecast of abundance

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

    prob.less.pfct <- round(my.ecdf(round(pfct)),4)
    prob.greater.pfct <- round(1 - my.ecdf(round(pfct)),4)

    prob.less.percentage.pfct <- round(prob.less.pfct*100,2)

    prob.greater.percentage.pfct <- round(prob.greater.pfct*100,2)

    prob.threshold.pfct <- round(pfct)
    
    prob.interval.percentage.pfct <- NA

    prob.pfct <-  data.frame(prob.threshold = prob.threshold.pfct,
                             prob.less.percentage = prob.less.percentage.pfct,
                             prob.greater.percentage = prob.greater.percentage.pfct, 
                              prob.interval.percentage = prob.interval.percentage.pfct)


    prob.pfct <- subset(prob.pfct, prob.threshold >= 0)     # report only positive thresholds

    prob.pfct

    probs = list(prob.thresholds=prob.thresholds,
                 prob.point.forecast=prob.pfct)

    probs

}


emp.prob.best.model.no.age <- empirical.probability.yboot.best.model.no.age(pred.args,
                                                             best.index.no.age,
                                                             stockabundance)




tablecaption <- paste0("Estimated probabilities that the ",
                       tolower(stockabundance)," ",
                       "value yet to be observed in ",
                       forecastingyear, " for the ",
                       stockname," ",
                       stockspecies, " stock",
                        " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       forecastingyear, " " ,
                       tolower(stockabundance), ", with the bootstrapped point forecasts being obtained from the ",
                       best.model.no.age, ".",
                       " Thresholds correspond to ", tolower(stockabundance), " values.",
                       " The threshold highlighted in orange represents the point forecast of ",
                       forecastingyear,  " ", tolower(stockabundance),
                       " produced by the ",
                       best.model.no.age, ".")

doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")

tt_1 <- emp.prob.best.model.no.age$prob.thresholds

tt_2 <- emp.prob.best.model.no.age$prob.point.forecast

tt_1_and_2 <- rbind.data.frame(tt_1, tt_2)

## usePackage("plyr")
## tt_arrange <- arrange(tt_1_and_2, prob.threshold)

tt_arrange <- tt_1_and_2[order(tt_1_and_2$prob.threshold),]

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )


from_tmp = which(tt_arrange[,1] == emp.prob.best.model.no.age$prob.point.forecast$prob.threshold)
tt_arrange[from_tmp, 4] <- tt_arrange[from_tmp + 1, 4]


tt_arrange[,1] <- comma(tt_arrange[,1])
## tt_arrange[,2] <- paste0(tt_arrange[,2],"%")
## tt_arrange[,3] <- paste0(tt_arrange[,3],"%")
tt_arrange[,2] <- paste0(sprintf("%.2f", tt_arrange[,2]),"%")
tt_arrange[,3] <- paste0(sprintf("%.2f",tt_arrange[,3]),"%")
tt_arrange[,4] <- paste0(sprintf("%.2f",tt_arrange[,4]),"%")


names(tt_arrange)[1] <- "Threshold"
names(tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(tt_arrange)[4] <- "Interval Probability"
tt_arrange[1,4] <- "-"

my_ft <- FlexTable( data = tt_arrange,
                    body.cell.props = cellProperties( padding = 2 ),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
my_ft[, 1:ncol(tt_arrange)] = parProperties(text.align = "right")

## my_ft[tt_arrange$Threshold %in% comma(emp.prob.best.model.no.age$prob.point.forecast[1]), ] = cellProperties( background.color = "lightblue" )


my_ft = spanFlexTableRows(my_ft, j=4, from = from_tmp, to = from_tmp + 1)

my_ft[tt_arrange$Threshold %in% comma(emp.prob.best.model.no.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

doc = addFlexTable(doc, flextable=my_ft)

rm(tt_arrange)
rm(from_tmp)
rm(tt_1_and_2)
rm(tt_1)
rm(tt_2) 

#================================================================================================
# Barplot of historical and forecasted values
#================================================================================================


#--- Point Forecast Visualization ------------------------------------------------------------------------

if (table.rank.rmse.results.no.age$best.model=="naiveone") {  # Naive Model (Previous Year)

    #--- Barplot of historical and forecasted values: naiveone ------------------------------------------------------

    figurecaption <- paste0("Historical ",
                         tolower(stockabundance), " values and ",
                         forecastingyear, " ",
                         "point forecast corresponding to the ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies,
                         " stock.",
                         " The ", forecastingyear, " point forecast was derived via ",
                         "naive forecasting (previous year).")

    myplot <- barplot.forecasted.values.individual.stock.naiveone.no.age(pred.int.individual.stock.naiveone.no.age,
                                                                 forecastingyear)


    doc = addPlot(doc,
        fun = plot, # print,
        x = myplot,
        width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

}


if (table.rank.rmse.results.no.age$best.model=="avgthree") {  # Naive Model (Average of Previous Three Years)

    #--- Barplot of historical and forecasted values: avgthree ------------------------------------------------------

    figurecaption <- paste0("Historical ",
                         tolower(stockabundance), " values and ",
                         forecastingyear, " ",
                         "point forecast corresponding to the ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies,
                         " stock.",
                         " The ", forecastingyear, " point forecast was derived via ",
                         "naive forecasting (average of previous 3 years).")

    myplot <- barplot.forecasted.values.individual.stock.avgthree.no.age(pred.int.individual.stock.avgthree.no.age,
                                                                 forecastingyear)


    doc = addPlot(doc,
                  fun = plot, # print,
                  x = myplot,
                  width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

}


if (table.rank.rmse.results.no.age$best.model=="avgfive") {  # Naive Model (Average of Previous 5 Years)

    #--- Barplot of historical and forecasted values: avgfive ------------------------------------------------------

    figurecaption <- paste0("Historical ",
                         tolower(stockabundance), " values and ",
                         forecastingyear, " ",
                         "point forecast corresponding to the ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies,
                         " stock.",
                         " The ", forecastingyear, " point forecast was derived via ",
                         "naive forecasting (average of previous 5 years).")

    myplot <- barplot.forecasted.values.individual.stock.avgfive.no.age(pred.int.individual.stock.avgfive.no.age,
                                                                 forecastingyear)


    doc = addPlot(doc,
                  fun = plot, # print,
                  x = myplot,
                  width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

}


if (table.rank.rmse.results.no.age$best.model=="arima") {  # ARIMA Model (Previous Year)

    #--- Barplot of historical and forecasted values: arima  -------------------------------------------------------

    figurecaption <- paste0("Historical ",
                         tolower(stockabundance), " values and ",
                         forecastingyear, " ",
                         "point forecast corresponding to the ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies,
                         " stock.",
                         " The ", forecastingyear, " point forecast was derived via ",
                         "ARIMA forecasting.")


    myplot <- barplot.forecasted.values.individual.stock.arima.no.age(pred.int.individual.stock.arima.no.age,
                                                                 forecastingyear)

    doc = addPlot(doc,
                  fun = plot, # print,
                  x = myplot,
                  width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

}


if (table.rank.rmse.results.no.age$best.model=="expsmooth") {  # Exponential Smoothing Model

    #--- Barplot of historical and forecasted values: expsmooth  -------------------------------------------------------

    figurecaption <- paste0("Historical ",
                         tolower(stockabundance), " values and ",
                         forecastingyear, " ",
                         "point forecast corresponding to the ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies,
                         " stock.",
                         " The ", forecastingyear, " point forecast was derived via ",
                         "exponential smoothing forecasting.")


    myplot <- barplot.forecasted.values.individual.stock.expsmooth.no.age(pred.int.individual.stock.expsmooth.no.age,
                                                                 forecastingyear)

    doc = addPlot(doc,
        fun = plot, # print,
        x = myplot,
        width=plotwidth, height=plotheight)

    doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

    rm(myplot)

}



#================================================================================================
# Gary's Plot for the Best Forecasting Model
#================================================================================================

# BestForecastingModel <- table.rank.rmse.results.no.age$best.model

if (table.rank.rmse.results.no.age$best.model=="naiveone"){

  ##--- Gary's Plot:  naiveone

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced bythe naive model (previous year) used for forecasting the value of ",
                       tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                      "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  myplot <- gary.plot.individual.stock.naiveone.no.age(pred.int.individual.stock.naiveone.no.age,
                                            results.retro.naiveone.no.age,
                                            stockabundance,
                                            forecastingyear)


  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-2)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

  rm(myplot)

} else if (table.rank.rmse.results.no.age$best.model=="avgthree") {

  ##--- Gary's Plot:  avgthree

  doc=addPageBreak(doc)

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the naive model (average of previous 3 years) used for forecasting the value of ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                     "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  myplot <- gary.plot.individual.stock.avgthree.no.age(pred.int.individual.stock.avgthree.no.age,
                                            results.retro.avgthree.no.age,
                                            stockabundance,
                                            forecastingyear)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-2)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

  rm(myplot)

} else if (table.rank.rmse.results.no.age$best.model=="avgfive") {

 ##--- Gary's Plot:  avgfive

  doc=addPageBreak(doc)

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the naive model (average of previous 5 years) used for forecasting the value of ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                     "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  myplot <- gary.plot.individual.stock.avgfive.no.age(pred.int.individual.stock.avgfive.no.age,
                                            results.retro.avgfive.no.age,
                                            stockabundance,
                                            forecastingyear)


  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-2)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

  rm(myplot)

} else if (table.rank.rmse.results.no.age$best.model=="arima") {

   ##--- Gary's Plot:  arima

  doc=addPageBreak(doc)

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the ARIMA model used for forecasting the value of ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                     "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  myplot <- gary.plot.individual.stock.arima.no.age(pred.int.individual.stock.arima.no.age,
                                            results.retro.arima.no.age,
                                            stockabundance,
                                            forecastingyear)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-2)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

  rm(myplot)

} else if (table.rank.rmse.results.no.age$best.model=="expsmooth") {

   ##--- Gary's Plot:  expsmooth

  doc=addPageBreak(doc)

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the exponential smoothing model used for forecasting the value of ",
                       tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                       "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  myplot <- gary.plot.individual.stock.expsmooth.no.age(pred.int.individual.stock.expsmooth.no.age,
                                            results.retro.expsmooth.no.age,
                                            stockabundance,
                                            forecastingyear)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-2)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

  rm(myplot)

}



#====================================================================================================================
#
# Table of Rankings
#
#====================================================================================================================

if (length(pred.args)>1) {


  doc = addPageBreak(doc)
  
  doc = addSection(doc, landscape=TRUE, ncol=1)

  tablelegend <- paste0("Ranking of models used for forecasting the ",
                      forecastingyear, " ",
                      paste0(tolower(stockabundance)),
                      " associated with the ",
                      stockname, " ",
                      stockspecies,
                      " stock. ",
                      "Models are first ranked with respect to each of the reported retrospective point forecast performance measures, ",
                      "such that the model which achieves the lowest absolute value for that measure receives the smallest rank. ",
                      "Model-specific ranks are then averaged across all performance measures to obtain a model-specific average rank."
                      #, "The smallest average rank corresponds to the best-performing model."
                      )

  doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend")


  f_tt_0 <- c("", "", rep("",ncol(table.rank.rmse.results.no.age$measure)))

  f_tt_1 <- c("", colnames(table.rank.rmse.results.no.age$measure),"")

  f_tt_2  <- table.rank.rmse.results.no.age$measure

  list_of_models_no_age <- NULL
  for (k in 1:length(pred.args)){

    tmplabel <- names(pred.args)[k]

    tmplabel <- str_replace_all(tmplabel, "pred.int.individual.stock.","")
    tmplabel <- str_replace_all(tmplabel, ".no.age","")
    tmplabel

    if (tmplabel=="naiveone") {
              list_of_models_no_age <- c(list_of_models_no_age, "Naive Model (Previous Year)")
    } else if (tmplabel=="avgthree") {
              list_of_models_no_age <-  c(list_of_models_no_age, "Naive Model (Average of Previous Three Years)")
    } else if (tmplabel=="avgfive") {
              list_of_models_no_age <-  c(list_of_models_no_age, "Naive Model (Average of Previous Five Years)")
    } else if (tmplabel=="arima") {
              list_of_models_no_age <-  c(list_of_models_no_age, "ARIMA Model")
    } else {
              list_of_models_no_age <-  c(list_of_models_no_age, "Exponential Smoothing Model")
    }
  }


  f_tt_2 <- cbind.data.frame(list_of_models_no_age,f_tt_2, rep("",nrow(table.rank.rmse.results.no.age$measure)))

  f_tt_2[,2:(ncol(f_tt_2)-1)] <- comma(f_tt_2[,2:(ncol(f_tt_2)-1)])


  f_tt_2 <- data.frame(lapply(f_tt_2, as.character), stringsAsFactors=FALSE)


  names(f_tt_2)[1] <- "Model"
  names(f_tt_2)[length(names(f_tt_2))] <- ""

  ## str(f_tt_2)

  f_tt_3 <- c("", "", rep("",ncol(table.rank.rmse.results.no.age$measure)))

  f_tt_4 <- table.rank.rmse.results.no.age$ranks

  f_tt_4 <- cbind.data.frame(list_of_models_no_age,f_tt_4, table.rank.rmse.results.no.age$avg.rank)

  f_tt_4 <- data.frame(lapply(f_tt_4, as.character), stringsAsFactors=FALSE)

  names(f_tt_4)[1] <- "Model"
  names(f_tt_4)[length(names(f_tt_4))] <- ""


  ## str(f_tt_4)

  f_tt <- rbind.data.frame(f_tt_0, f_tt_1, f_tt_2, f_tt_3,
                         c("",colnames(f_tt_4)[-1]),
                         f_tt_4)

  rownames(f_tt) <- 1:nrow(f_tt)

  names(f_tt)[1] <- "Model"

  f_tt

  ## baseCellProp = cellProperties( padding = 4)

  MyFTable = FlexTable(data = f_tt,
                       body.cell.props = cellProperties( padding = 4),  ## MANGO1
                        header.cell.props= cellProperties( padding = 4),
                       header.text.props = textBold(), header.columns = FALSE)

  Nmeasures <- length(table.rank.rmse.results.no.age$ranking.measures)


  Nmodels <- length(table.rank.rmse.results.no.age$all.models)

  MyFTable = spanFlexTableColumns( MyFTable, i = 1, from = 2, to = Nmeasures+1)
  MyFTable[1, 1, text.properties = textBold(color = "black"), newpar = FALSE] = "Model"
  MyFTable[1, 2, text.properties = textBold(color = "black"), newpar = FALSE] = "Measure"
  MyFTable[1, Nmeasures+1, text.properties = textBold(color = "black"), newpar = FALSE] = ""


  MyFTable = spanFlexTableColumns( MyFTable, i = 2 + Nmodels + 1, from = 2, to = Nmeasures+1)

  MyFTable = spanFlexTableRows( MyFTable, j = 1, from = 1, to = 2)

  MyFTable = spanFlexTableRows( MyFTable, j = 1, from = 2 + Nmodels + 1, to = 2 + Nmodels + 1 + 1)

  #### MyFTable = spanFlexTableRows( MyFTable, j = 1, from = 2 + Nmodels + 1, to = 2 + Nmodels + 1 + 1)
  # MyFTable = spanFlexTableRows( MyFTable, j = 8, from = 8, to = 9)
  # MyFTable = spanFlexTableRows( MyFTable, j = 2 + Nmodels + 1, from = 1, to = Nmeasures + 1)

  MyFTable[2 + Nmodels + 1, 1, text.properties = textBold(color = "black"), newpar = FALSE] = "Model"
  MyFTable[2 + Nmodels + 1, 2, text.properties = textBold(color = "black"), newpar = FALSE] = "Rank"

  MyFTable[2 + Nmodels + 1, Nmeasures + 2, text.properties = textBold(color = "black"), newpar = FALSE] = "Average Rank"
  MyFTable[2 + Nmodels + 2, Nmeasures + 2, text.properties = textNormal(color = "black"), newpar = FALSE] =  paste(colnames(table.rank.rmse.results.no.age$measures), collapse=", ")


  MyFTable[,1,] = parLeft()

  for (k in 2:(1 + Nmeasures + 1)){
    MyFTable[2:(2 + Nmodels),k,] = parRight()
    MyFTable[(2 + Nmodels + 1 + 1):(2 + Nmodels + 1 + 1 + Nmodels),k,] = parRight()
  }


  MyFTable = addFooterRow( MyFTable, value = c("Note: Ranking across models is based on the absolute values of the considered performance measures."),
                           colspan = ncol(f_tt),
                          text.properties=textProperties(color = "red", font.size=9, font.weight="bold"))  ## DAVID


  ## MyFTable = setFlexTableWidths( MyFTable, widths = c(2,rep(0.8, Nmeasures), 1) )

  doc = addFlexTable(doc, flextable=MyFTable)
  
  doc = addSection(doc, ncol=1, columns.only=TRUE)

}


## More than one model -- show average rank in relation to width of prediction intervals
if (length(pred.args)>1){


    ## doc = addSection(doc, ncol=1, columns.only=TRUE)

    ## doc= addPageBreak(doc)
    
    tablelegend <- paste0("Identifying the best model for forecasting the ",
                      forecastingyear, " ",
                      paste0(tolower(stockabundance)),
                      " associated with the ",
                      stockname, " ",
                      stockspecies,
                      " stock. ",
                      "The best model (highlighted in yellow) is the one which achieves the smallest average rank across all considered retrospective point forecast performance measures ",
                      " and produces the shortest forecast interval among models with the same smallest average rank."
                      )

    doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend")


    usePackage("scales")

    tt_best <- table.rank.rmse.results.no.age$pred.int.and.avg.ranks

    tt_best <- transform(tt_best, model = as.character(model))
    ## str(tt_best)


    for (k in 1:length(tt_best$model)){

         if (tt_best$model[k]=="naiveone"){
             tt_best$model[k] <- "Naive Model (Previous Year)"
         } else if (tt_best$model[k]=="avgthree"){
             tt_best$model[k] <- "Naive Model (Average of Previous 3 Years)"
         } else if (tt_best$model[k]=="avgfive"){
             tt_best$model[k] <- "Naive Model (Average of Previous 5 Years)"
         } else if (tt_best$model[k]=="arima"){
             tt_best$model[k] <- "ARIMA Model"
         } else if (tt_best$model[k]=="expsmooth") {
            tt_best$model[k] <- "Exponential Smoothing Model"
         }

    }


    tt_best$range <- paste0(comma(tt_best$lower), " - ", comma(tt_best$upper))
    tt_best$lower <- tt_best$range
    tt_best$upper <- NULL
    tt_best$range <- NULL
    tt_best$center <- comma(tt_best$center)
    tt_best$width <- comma(tt_best$width)


    names(tt_best)[names(tt_best)=="model"] <- "Model"
    names(tt_best)[names(tt_best)=="center"] <- "Point Forecast"
    names(tt_best)[names(tt_best)=="lower"] <- "Interval Forecast"
    names(tt_best)[names(tt_best)=="width"] <- "Length of Interval Forecast"
    names(tt_best)[names(tt_best)=="avg.ranks"] <- "Average Rank"


    ## tt_best

    MyFTable = FlexTable(data = tt_best,
                       body.cell.props = cellProperties( padding = 4),  ## MANGO1
                        header.cell.props= cellProperties( padding = 4),
                       header.text.props = textBold(), header.columns = TRUE)

    MyFTable_Index_1 <- table.rank.rmse.results.no.age$index.min.avg.rank

    MyFTable = setRowsColors( MyFTable, i=MyFTable_Index_1, colors = "yellow")


    doc = addFlexTable(doc, flextable=MyFTable)


}


if (length(pred.args)==1){

  doc=addParagraph(doc, value=" ", style="Normal")
    
  
  mylabel <- NULL 
  for (k in 1:nrow(tt)){

     tmplabel <- rownames(tt)[k]
  
     if (tmplabel=="naiveone") {
              mylabel <- c(mylabel, "naive model (previous year)")
     } else if (tmplabel=="avgthree") {
              mylabel <- c(mylabel, "naive model (average of previous 3 years)")
     } else if (tmplabel=="avgfive") {
              mylabel <- c(mylabel, "naive model (average of previous 5 years)")
     } else if (tmplabel=="arima") {
              mylabel <- c(mylabel, "ARIMA model")
     } else {
              mylabel <- c(mylabel, "exponential smoothing model")
     }

  }  
  
  doc = addParagraph(doc, value=paste0("Measures of retrospective point forecast performance for the ",mylabel," model."), stylename="rTableLegend") 
  
  tt <- table.rmse.results.no.age

  mylabel <- NULL 
  for (k in 1:nrow(tt)){

     tmplabel <- rownames(tt)[k]
  
     if (tmplabel=="naiveone") {
              mylabel <- c(mylabel, "Naive Model (Previous Year)")
     } else if (tmplabel=="avgthree") {
              mylabel <- c(mylabel, "Naive Model (Average of Previous Three Years)")
     } else if (tmplabel=="avgfive") {
              mylabel <- c(mylabel, "Naive Model (Average of Previous Five Years)")
     } else if (tmplabel=="arima") {
              mylabel <- c(mylabel, "ARIMA Model")
     } else {
              mylabel <- c(mylabel, "Exponential Smoothing Model")
     }

  }

  col_tt <- mylabel

  tt <- cbind.data.frame(col_tt, tt)
  ## tt 
  names(tt)[names(tt)=="col_tt"] <- "Model"
  # tt 
  usePackage("scales")
  tt[,-1] <- apply(tt[,-1],2,comma)
  # tt

  # set cell padding defaut to 2
  ## baseCellProp = cellProperties( padding = 5 )
  # Create a FlexTable with tt data frame
  my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 5 ), 
                  header.cell.props = cellProperties( padding = 5 ), 
                  header.par.props = parProperties(background.color="#DDDDDD", text.align = "center"),
                  add.rownames = FALSE
  )


  ## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

  my_ft[, 1, ] = parProperties(text.align = "center")
  my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

  # my_ft[, 1, to="header"] = parProperties(text.align = "left")
  # to="header"
  # my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

  doc = addFlexTable(doc, my_ft)

}



## clean up redundant files 

fn <- "arimafit.txt"
    if (file.exists(fn)) file.remove(fn)
    
fn <- "expsmoothfit.txt"
    if (file.exists(fn)) file.remove(fn)


#================================================================================================
# Produce report
#================================================================================================

## ## setwd(file.path(homeDir))    ## COMMENT THIS OUT WHEN TESTING R CODE ON JUNE 14, 2016

## folder <- function(folder.name){
##    #note: to do-> put in a check for 
##    #folder existing & return a warning
##
##    ## FN <- substitute(folder.name)
##    FN <- folder.name
##    FN <- as.character(FN)
##    x <- paste(getwd(),"/", FN, sep="")
##    dir.create(x)
## }

## ## folder("Output")  ## COMMENT THIS OUT WHEN TESTING ON JUNE 14, 2016

## ## setwd(file.path(homeDir,"Output"))     ## COMMENT THIS OUT WHEN TESTING ON JUNE 14, 2016

## ## folder(stockname) ## COMMENT THIS OUT WHEN TESTING ON JUNE 14, 2016

## ## setwd(file.path(homeDir,"Output",stockname))    ## COMMENT THIS OUT WHEN TESTING ON JUNE 14, 2016

## reportname <- paste0("Executive Report ",stockname," ",stockabundance," ",forecastingyear,".docx")

## writeDoc(doc, reportname)   

## ## usePackage("PBSmodelling")    

## ## local(envir=.PBSmodEnv,expr={
## ##          # use openFile directly:
## ##          openFile("Report.docx")})

## browseURL(reportname)    



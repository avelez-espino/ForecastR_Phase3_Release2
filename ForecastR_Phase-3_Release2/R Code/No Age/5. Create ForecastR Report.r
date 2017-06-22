plotwidth <- 6
plotheight <- 7


#========================================================================================================
# Initiate Word Report
#========================================================================================================

## usePackage("ReporteRs")

## options("ReporteRs-fontsize" = 10,
##        "ReporteRs-default-font" = "Calibri")

## assign the template
## doc <- docx(template = "Template_ReporteRs.docx")

#========================================================================================================
# CoverPage
#========================================================================================================

pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 20) )
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
        pot(paste(forecastingyear), textProperties(font.size = 11) )
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
        pot(paste(Sys.Date()), textProperties(font.size = 11) )
my.pars = set_of_paragraphs(pot7)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )
     

#========================================================================================================
# Table of Contents
#========================================================================================================

doc = addPageBreak(doc)

pottoc =  pot("Table of Contents", textProperties(font.weight="bold", font.size = 13) )
my.pars = set_of_paragraphs(pottoc)
doc = addParagraph( doc, value = my.pars, stylename="Normal" )

# Add a table of contents
doc <- addTOC(doc)

doc <- addPageBreak(doc) # go to the next page

#========================================================================================================
# Summary of Results
#========================================================================================================

doc = addTitle(doc, "Summary of Forecasting Results", level=1)

doc = addParagraph(doc, paste(" "), stylename="Normal")

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
# r.sq.values <- r.squared.retro.individual.stock.all.models.no.age$r.squared
# r.sq.values <- data.frame(lapply(r.sq.values, as.character), stringsAsFactors=FALSE)
# r.sq.values <- as.character(r.sq.values[1, ])

# Summary_Row_12 <- c("R-squared", r.sq.values)
  
# what <- Summary_Row_12
# what_ext <- c(what, rep_len(NA, ncol(Summary_Row) - length(what)))
# Summary_Row <- rbind.data.frame(Summary_Row, what_ext)
# Summary_Row <- data.frame(lapply(Summary_Row, as.character), stringsAsFactors=FALSE)


row.has.na <- apply(Summary_Row, 1, function(x){any(is.na(x))})
Summary_Row <- Summary_Row[!row.has.na,]


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

doc = addParagraph(doc, paste(" "), stylename="Normal")

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
##                            "Naive Model (Average of Previous 5 Years)",
##                            "ARIMA Model",
##                            "Exponential Smoothing Model")

## best.index.no.age <- table.rank.rmse.results.no.age$index.min.avg.rank
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

rm(from_tmp)
rm(tt_arrange)
rm(tt_1_and_2)
rm(tt_1)
rm(tt_2)

#========================================================================================================
# Introduction
#========================================================================================================

## doc = addPageBreak(doc)

doc = addTitle(doc, "Introduction", level=1)

doc = addParagraph(doc, paste(" "), stylename="Normal")

sometext <- paste("In this report, we forecast the",
                  forecastingyear,
                  paste(tolower(stockabundance)),
                  "for the",
                  stockname,
                  stockspecies,
                  "stock",
                  ifelse(length(pred.args)>1,"by using the best of the following models:","by using the following model:"))
doc = addParagraph(doc, sometext, stylename="Normal")

doc = addParagraph(doc, paste(" "), stylename="Normal")

sometext <- names(pred.args)
usePackage("stringr")
sometext <- str_replace_all(string=sometext,pattern="pred.int.individual.stock.",replacement="")  
sometext <- str_replace_all(string=sometext,pattern=".no.age",replacement="")  

models.stacked <- NULL 
for (k in 1:length(sometext)){

     tmplabel <- sometext[k]
     if (tmplabel=="naiveone") {
              models.stacked <- c(models.stacked, "Naive Model (Previous Year)")
          } else if (tmplabel=="avgthree") {
              models.stacked <- c(models.stacked, "Naive Model (Average of Previous Three Years)")
          } else if (tmplabel=="avgfive") {
              models.stacked <- c(models.stacked, "Naive Model (Average of Previous Five Years)")
          } else if (tmplabel=="arima") {
              models.stacked <- c(models.stacked, "ARIMA Model")
          } else {
              models.stacked <- c(models.stacked, "Exponential Smoothing Model")
          }

}

         
doc = addParagraph( doc, value = models.stacked, stylename="BulletList" )

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- ifelse(length(pred.args)>1,"Some general comments about each of these types of models are provided below.",
                                        "Some general comments about this type of model are provided below.")

doc = addParagraph( doc, value = paragraph, stylename="Normal")


if (noagemodelnaiveone | noagemodelavgthree | noagemodelavgfive) {

doc = addTitle( doc, "Naive Modeling", level=2)

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0(ifelse(sum(noagemodelnaiveone + noagemodelavgthree + noagemodelavgfive)>1, "The naive models", "The naive model"),  
                    " considered in this report",  
                    ifelse(sum(noagemodelnaiveone + noagemodelavgthree + noagemodelavgfive)>1, " do not require"," does not require"),
                    " statistical parameter estimation but rather", 
                    ifelse(sum(noagemodelnaiveone + noagemodelavgthree + noagemodelavgfive)>1, " summarize past "," summarizes past "),
                    tolower(stockabundance), 
                    " observations to make forecasts of future ", tolower(stockabundance)," values.") 
doc = addParagraph( doc, value = paragraph, stylename="Normal")

doc = addParagraph(doc, paste(" "), stylename="Normal")

}                    

if (noagemodelnaiveone){

    paragraph <- paste0("The \"naive model (previous year)\" uses the ", 
                    tolower(stockabundance), " from the previous year to forecast the ", 
                    tolower(stockabundance), " for the current year.")
    doc = addParagraph( doc, value = paragraph, stylename="Normal")                  

    doc = addParagraph(doc, paste(" "), stylename="Normal")

}

if (noagemodelavgthree){

    paragraph <- paste0("The \"naive model (average of previous 3 years)\" uses the average ", 
                    tolower(stockabundance), " over the previous 3 years to forecast the ", 
                    tolower(stockabundance), " for the current year.")
    doc = addParagraph( doc, value = paragraph, stylename="Normal")       

    doc = addParagraph(doc, paste(" "), stylename="Normal") 

}              

if (noagemodelavgfive){

    paragraph <- paste0("The \"naive model (average of previous 5 years)\" uses the average ", 
                    tolower(stockabundance), " over the previous 5 years to forecast the ", 
                    tolower(stockabundance), " for the current year.")
    doc = addParagraph( doc, value = paragraph, stylename="Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")       
}
                    

                    
if (noagemodelarima){
                    
   doc = addTitle(doc, "ARIMA Modeling", level=2)

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("ARIMA models are a general class of models for forecasting a univariate time series which can be ", 
                    "stationarized by transformations such as differencing and logging. ", 
                    "The acronym ARIMA stands for Auto-Regressive Integrated Moving Average.") 
   doc = addParagraph( doc, value = paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("ARIMA models are represented using the notation ARIMA(p,d,q), ", 
                    "where p is the number of autoregressive terms, ",
                    "d is the number of (nonseasonal) differences, ", 
                    "and q is the number of lagged forecast errors in the forecasting equation.") 
   doc = addParagraph( doc, value = paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("Forecasting a univariate time series on the basis of ARIMA modeling involves three steps:")
   doc = addParagraph( doc, value = paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   sometext <- c("   1. Identification: Differencing of order d is applied to make the time series stationary;",
              "   2. Estimation: An ARMA(p,q) model is fitted to the differenced time series;", 
              "   3. Forecasting: The ARIMA(p,d,q) model is used to forecast future values of the time series.")
   doc = addParagraph(doc, value=sometext, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")
   
   paragraph <- paste0("In this report, we rely on the auto.arima() function from the R package forecast to implement the steps described above. ",
                    "This function uses automation to identify the orders p,d and q of the ARIMA models fitted to historical ",  
                    tolower(stockabundance), " ",  
                    "data. ", 
                    "Specifically, the auto.arima() function finds the order of differencing d first and then identifies the orders p and q via AIC maximization. ",  
                    "The function then proceeds to estimate the unknown parameters describing the ARIMA(p,d,q) models and to produce point and interval forecasts. ",
                    "The function relies on time series bootstrapping to produce the interval forecasts.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")

   ## doc = addParagraph(doc, paste(" "), stylename="Normal")

}



if (noagemodelexpsmooth){

   doc = addTitle(doc, "Exponential Smoothing Modeling", level=2)

   doc = addParagraph(doc, paste(" "), stylename="Normal") 

   paragraph <- paste0("Exponential smoothing (ETS) models are a general class of innovations state space models for forecasting a univariate time series. ", 
                    "The acronym ETS can be thought of as ExponenTial Smoothing, but in effect denotes the error (E), trend (T) and seasonal components (S) ", 
                    "which can be used to describe the time series to be forecasted. ", 
                    "The trend component represents the growth or the decline of the time series over an extended period of time. ",
                    "For time series defined at time intervals which are fractions of a year (e.g., months), the seasonal component is a pattern of change that ", 
                    "repeats itself from year to year. The error component captures irregular, short-term fluctuations present in the series, ",
                    "which cannot be attributed to the trend and seasonal components.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("Exponential smoothing models can be classified according to the nature of the error, trend and seasonal components ",
                    "of the underlying time series. The error (E) component can be either additive (A) or multiplicative (M). ",
                    "The trend (T) component can be additive (A), multiplicative (M) or inexistent (N). ", 
                    "The trend (T) component can also be dampened additively (Ad) or multiplicatively (Md). ",
                    "The seasonal (S) component can be either additive (A), multiplicative (M) or inexistent (N).") 
   doc = addParagraph(doc, value=paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")
   
   paragraph <- paste0("Each particular combination of options for the error, trend and seasonal components of a time series ",
                    "gives rise to a specific exponential smoothing model. Since the possibilities for each component are ",
                    "Error = {A,M}, Trend = {N,A,Ad,M,Md} and Seasonal = {N,A,M}, in total there exist 2 x 5 x 3 = 30 such ",
                    "exponential smoothing models. Components designated by the letter N are not present in the time series of interest. ",
                    "Components designated by the letter A are present and are combined with the other components via addition. ",
                    "Components designated by the letter M are present and are combined with the other components via multiplication.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("For example, the exponential smoothing method ETS(AAN) has E(A), T(A) and S(N) structures, where E(A) stands for additive error, ",
                    "T(A) stands for additive trend and S(N) stands for inexistent seasonality. One can show that ETS(AAN) is Holt's linear method with ",
                    "additive errors.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("A complete list of the 30 exponential smoothing models is available at ",
                    "https://www.otexts.org/fpp/7/7 (Table 7.10). ",
                    "This list distinguishes between models with additive errors and models with multiplicative errors. ",
                    "Each model consists of a measurement equation which describes the observed time series data and some ",
                    "transition equations which describe how the unobserved states of the time series (i.e., level, trend, seasonal) change over time.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")
          
   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("The ets() function from the R package forecast is used in this report to implement exponential smoothing. ",
                    "Given an input time series, the ets() function uses Akaike’s Information Criterion (AIC), corrected for small sample bias, ",
                    "to select the optimal exponential smoothing model for that series. ",
                    "For ETS models, the AIC is defined as AIC = -2log(L) + 2k, where where L is the likelihood of the model and ",
                    "k is the total number of model parameters and initial states that have been estimated. ",
                    "The small sample bias corrected AIC, AICc, is defined as AICc = AIC + 2(k+1)(k+2)/(T-k), ",
                    "where T is the number of observations in the time series.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")
   
   doc = addParagraph(doc, paste(" "), stylename="Normal")       

   paragraph <- paste0("The input time series for the ets() function considered in this report consists of the ", 
                    tolower(stockabundance), " for the ",
                    stockname, " ", 
                    stockspecies, " stock.",  
                    "Based on this series, the ets() function will produce an optimal exponential smoothing model ",
                    "after performing automated model selection on the basis of the AICc criterion. ",
                    "The optimal model will be such that it will have the smallest AICc value among all candidate models ",
                    "entertained during the model selection process. ", 
                    "The ets() function is used with all of the default options, except for the damped option, which is set to FALSE.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("The estimated level, trend or seasonal states of the optimal exponential smoothing model are accompanied by smoothing coefficients, ",
                    "which depend on the rate of change of these states and reflect how much weight recent time series observations receive over older ones. ",
                    "In the output produced by the ets() function, the smoothing coefficients are denoted by alpha, beta and gamma, where alpha is the ",
                    "level smoothing coefficient, beta is the trend smoothing coefficient and gamma is the seasonal smoothing coefficient. ", 
                    "The closer a smoothing coefficient is to 1, the less smoothing is performed, allowing for rapid changes in the corresponding state ",
                    "and heavy reliance on recent time series observations. ",
                    "Conversely, the closer a smoothing coefficient is to 0, the more smoothing is performed, allowing for gradual changes in the corresponding state ",
                    "and less reliance on recent time series observations.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")

   doc = addParagraph(doc, paste(" "), stylename="Normal")

   paragraph <- paste0("The output produced by the ets() function also includes a parameter named sigma, ",
                    "which represents the standard deviation of the model errors.")
   doc = addParagraph(doc, value=paragraph, stylename="Normal")
   
   doc = addParagraph(doc, paste(" "), stylename="Normal")
                    
}

#========================================================================================================
# Data Used for Forecasting
#========================================================================================================


##
## Table of time series data used for forecasting
##

doc = addTitle(doc, "Data Used for Forecasting", level=1)

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("The following table displays the historical values of ", 
                    tolower(stockabundance), " for the ", 
                    stockname, " ", 
                    stockspecies, " stock, ", 
                    "together with the corresponding historical run (or return) years. ",
                    " The historical values of ", tolower(stockabundance), " will be used to forecast future ", 
                    tolower(stockabundance), " values.")
doc = addParagraph(doc, value=paragraph, stylename="Normal")
     

tablelegend <- paste0("Data used for forecasting the ",
                      forecastingyear, " ",
                      paste0(tolower(stockabundance)),
                      " associated with the ",
                      stockname, " ",
                      stockspecies,
                      " stock.")

doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend")

## baseCellProp = cellProperties( padding = 4)

tt = datalist
usePackage("stringr")
names(tt)[length(names(tt))] <- str_replace_all(names(tt)[length(names(tt))], "Average_","")


usePackage("scales")
tt[,-1] <- comma(tt[,-1])

usePackage("stringr")
names(tt) <- str_replace_all(names(tt),"_"," ")


my_ft <- FlexTable( data = tt,
                    body.cell.props = cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
my_ft[, 1:ncol(tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=my_ft)

## doc = addPageBreak(doc)

##
## Time series plot of data used for forecasting
##

plotlegend <- paste0("Plot of historical ",
                         tolower(stockabundance), " versus run year ",
                         "for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.")

doc = addPlot(doc=doc,
            fun=print,
            x=plot.data.no.age(datalist),
            width=plotwidth, height=plotheight-3,
            vector.graphic = F)

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


#========================================================================================================
# Modeling Results
#========================================================================================================


doc = addTitle(doc, "Modeling Results", level=1)


doc = addTitle( doc, "Fitted Values", level=2)

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("The next",
                    ifelse(length(pred.args)>1, " plots show the fitted values produced by", 
                                                " plot shows the fitted values produced by"),
                    ifelse(length(pred.args)>1, " all considered", 
                                                " the considered"),
                    ifelse(length(pred.args)>1, " forecasting models ", 
                                                " forecasting model "),
                   "versus the historical return years.",
                   " To facilitate comparison, the plots also show the historical ", tolower(stockabundance),
                   " values versus the historical return years. ", 
                   ifelse(length(pred.args)>1, "For each model, we can compare the fitted values produced by that model against the historical values of ", 
                                               "For the considered model, we can compare the fitted values produced by the model against the historical values of"),
                   tolower(stockabundance), 
                   " to see how well the model fits the historical data. ")

doc = addParagraph(doc, paragraph, stylename = "Normal" )



if (noagemodelnaiveone){
  ##
  ## Plot of Fitted Values: Naive Model (Previous Year)
  ##
  plotlegend <- paste0("Plot of fitted values produced by the naive model (previous year) used to forecast the ",
                         forecastingyear, " ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.",
                         " For comparison, actual values of ",
                         tolower(stockabundance),
                         " are also shown.")
                         
  doc = addPlot(doc=doc,
            fun=print,
            x=plot.fitted.naiveone.no.age(fit.naiveone.model.no.age),
            width=plotwidth, height=plotheight-4,
            vector.graphic = F)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")
}


if (noagemodelavgthree){
  ##
  ##  Plot of Fitted Values: Naive Model (Average of Previous Three Years)
  ##

  plotlegend <- paste0("Plot of fitted values produced by the naive model (average of previous three years) used to forecast the ",
                         forecastingyear, " ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.",
                         " For comparison, actual values of ",
                         tolower(stockabundance),
                         " are also shown.")


  doc = addPlot(doc=doc,
            fun=print,
            x=plot.fitted.avgthree.no.age(fit.avgthree.model.no.age),
            width=plotwidth, height=plotheight-4,
            vector.graphic = F)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")
}


if (noagemodelavgfive){
  ##
  ##  Plot of Fitted Values: Naive Model (Average of Previous Five Years)
  ##

  plotlegend <- paste0("Plot of fitted values produced by the naive model (average of previous five years) used to forecast the ",
                         forecastingyear, " ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.",
                         " For comparison, actual values of ",
                         tolower(stockabundance),
                         " are also shown.")


  doc = addPlot(doc=doc,
            fun=print,
            x= plot.fitted.avgfive.no.age(fit.avgfive.model.no.age),
            width=plotwidth, height=plotheight-4,
            vector.graphic = F)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")
}


if (noagemodelarima){
  ##
  ## Plot of Fitted Values: ARIMA Model
  ##

  plotlegend <- paste0("Plot of fitted values produced by the ARIMA model used to forecast the ",
                         forecastingyear, " ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.",
                         " For comparison, actual values of ",
                         tolower(stockabundance),
                         " are also shown.")


  doc = addPlot(doc=doc,
            fun=print,
            x=plot.fitted.arima.no.age(arima.model.fit.no.age, boxcoxtransform),
            width=plotwidth, height=plotheight-4,
            vector.graphic = F)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")
}

if (noagemodelexpsmooth){
  ##
  ## Plot of Fitted Values: Exponential Smoothing Model
  ##

  plotlegend <- paste0("Plot of fitted values produced by the exponential smoothing model used to forecast the ",
                         forecastingyear, " ",
                         tolower(stockabundance),
                         " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.",
                         " For comparison, actual values of ",
                         tolower(stockabundance),
                         " are also shown.")


  doc = addPlot(doc=doc,
            fun=print,
            x= plot.fitted.expsmooth.no.age(expsmooth.model.fit.no.age, boxcoxtransform),
            width=plotwidth, height=plotheight-4,
            vector.graphic = F)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

}


doc = addPageBreak(doc)


#=================================================================================================

doc = addTitle( doc, "Model Summary Tables", level=2)

doc = addParagraph(doc, paste(" "), stylename="Normal")

if (noagemodelnaiveone | noagemodelavgthree | noagemodelavgfive){
  if (sum(noagemodelnaiveone + noagemodelavgthree + noagemodelavgfive)>1){
  
    paragraph <- paste0("We do not report model summary tables for the naive models, ", 
                       "because these models do not require statistical parameter estimation.") 
    doc = addParagraph(doc, paragraph, stylename = "Normal" )
    rm(paragraph)
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")
  
  } else {
  
    paragraph <- paste0("We do not report a model summary table for the naive model, ", 
                       "because this model does not require statistical parameter estimation.") 
    doc = addParagraph(doc, paragraph, stylename = "Normal" )
    rm(paragraph)
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")
   
  }
}

##
## ARIMA Modeling Results
##

if (noagemodelarima | noagemodelexpsmooth){
  if (sum(noagemodelarima + noagemodelexpsmooth)==2){ 
    paragraph <- paste0("The next two tables represent model summary tables for the ARIMA and exponential smoothing models used to forecast the ",
                    forecastingyear, " ",
                    tolower(stockabundance),
                    " for the ", 
                    stockname, " ", 
                    stockspecies, 
                    " stock. ")
    doc = addParagraph(doc, paragraph, stylename = "Normal" )
    rm(paragraph)
  } else {
   paragraph <- paste0("The next table represents the model summary table for the ", 
                       ifelse(noagemodelarima,"ARIMA","exponential smoothing"),
                      " model used to forecast the ",
                      forecastingyear, " ",
                      tolower(stockabundance),
                      " for the ", 
                      stockname, " ", 
                      stockspecies, 
                      " stock. ")
   doc = addParagraph(doc, paragraph, stylename = "Normal" )
   rm(paragraph)
  
  }
}


if (noagemodelarima) {
  ##
  ## ARIMA Modeling Results
  ##
  extract.arima.model.fit.no.age <- extract.arima(arima.model.fit.no.age$model)
  doc = addParagraph(doc, value=paste("ARIMA modeling results."), stylename="rTableLegend") 
  tt <- extract.arima.model.fit.no.age$extract.arima.fit.table
  ## http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
  tt.1 <- data.frame(lapply(tt, as.character), stringsAsFactors=FALSE)
  tt.2 <- rbind.data.frame(colnames(tt), tt.1)
  names(tt.2) <- names(tt)
  tt <- tt.2
  # set cell padding default to 2
  # Create a FlexTable with tt data frame
  my_ft = FlexTable(data = tt,
                  body.cell.props =  cellProperties( padding = 2 ), 
                  header.cell.props =  cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "center"),
                  add.rownames = FALSE, 
                  header.columns= FALSE
  )
  my_ft = addHeaderRow(my_ft, value=paste0(extract.arima.model.fit.no.age$extract.arima.fit.model), 
                     colspan=ncol(tt), 
                     text.properties=textBold())
  my_ft = addFooterRow(my_ft, value=paste0("sigma estimated as ",round(sqrt(extract.arima.model.fit.no.age$extract.arima.fit.sigma2),4)), 
                     colspan=ncol(tt), 
                     text.properties=textItalic())
  my_ft = addFooterRow(my_ft, value=paste0("log-likelihood = ",round(extract.arima.model.fit.no.age$extract.arima.fit.loglik,4)), 
                     colspan=ncol(tt), 
                     text.properties=textItalic())
  my_ft = addFooterRow(my_ft, value=paste0("AIC = ",round(extract.arima.model.fit.no.age$extract.arima.fit.aic,4)), 
                     colspan=ncol(tt), 
                     text.properties=textItalic())
  my_ft = addFooterRow(my_ft, value=paste0("AICc = ",round(extract.arima.model.fit.no.age$extract.arima.fit.aicc,4)), 
                     colspan=ncol(tt), 
                     text.properties=textItalic())
  my_ft = addFooterRow(my_ft, value=paste0("BIC = ",round(extract.arima.model.fit.no.age$extract.arima.fit.bic,4)), 
                     colspan=ncol(tt), 
                     text.properties=textItalic())
  my_ft[1, ] = textProperties(color="black", font.weight = "bold" )
  doc = addFlexTable(doc, my_ft)
  doc = addParagraph(doc, value=paste(" "), stylename="Normal") 
  
  doc = addPageBreak(doc)
  
}


if (noagemodelexpsmooth) {
  ##
  ## Exponential Smoothing Results
  ##

  expsmoothfit <- expsmooth.model.fit.no.age 
  sink("expsmoothfit.txt")
  print(expsmoothfit)
  sink()
  out <- readLines("expsmoothfit.txt")
  usePackage("stringr")
  out.pattern <- str_detect(string=out, pattern="ETS")
  modelexpsmooth <- out[out.pattern==TRUE]
  modelexpsmooth <- str_trim(modelexpsmooth)
  model_desc <- modelexpsmooth
  model_fit <- out

  hadley <- NULL
  for (k in 1:length(model_fit)){
    hadley <- c(hadley, 
              sum(unlist(str_locate( model_fit[k], "original.data")))
              )
  }
  hadley <- ifelse(!is.na(hadley), 1, 0)
  hadley.index <- which(hadley==1) - 2
  model_fit_expsmooth_no_age <- model_fit[6:hadley.index]

  doc = addParagraph(doc, value=paste("Exponential smoothing modeling results."), stylename="rTableLegend") 

  tt <- data.frame(model_fit_expsmooth_no_age)

  # set cell padding defaut to 2
  ## baseCellProp = cellProperties( padding = 2 )
  # Create a FlexTable with tt data frame
  my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 2 ), 
                  header.cell.props = cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "center"),
                  add.rownames = FALSE
  )

  # overwrites some text formatting properties
  ## my_ft[, 1] = parProperties(font.weight = "bold")
  ## my_ft[, 1] = chprop(baseCellProp, font.weight = "bold")
  ## my_ft[, 1] = chprop(baseCellProp, background.color = "lightgrey")

  my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

  doc = addFlexTable(doc, my_ft)

}


#======================================================================================================
# Forecasting Results for All Models
#======================================================================================================

## doc = addPageBreak(doc)

doc = addTitle( doc, "Forecasting Results", level=1)

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("This section reports the forecasting results for the ", 
                    stockspecies, " ", 
                    stockname, " stock corresponding to the forecasting year ", 
                    forecastingyear, "."
                    # " The results were produced by naive modeling, ARIMA modeling and exponential smoothing modeling."
                    )
doc = addParagraph(doc, paragraph, stylename = "Normal")
rm(paragraph)

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("Forecasting results are reported numerically and visually for two types of forecasts: 1) point forecasts and 2) interval forecasts.")
doc = addParagraph(doc, paragraph, stylename = "Normal")

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("A point forecast is simply a number which represents our best guess of the ", forecastingyear," value of ", 
                     tolower(stockabundance), " for the ",
                     stockname, " ", 
                     stockspecies, " stock ", 
                     "based on the available historical ", tolower(stockabundance),  " data.")
doc = addParagraph(doc, paragraph, stylename = "Normal")
                     
doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("An interval forecast is a range in which we expect the ",
                    forecastingyear, " ",  
                    tolower(stockabundance), 
                    " value ",  
                    "to fall with some (prespecified) probability, such as 80% or 90%. ", 
                    "The interval",
                    ifelse(length(pred.args)>1, " forecasts ", " forecast "),
                    "of ", tolower(stockabundance),  
                    " included in this report ", 
                    ifelse(length(pred.args)>1,"are all ","is an "), 
                    "80% interval", 
                    ifelse(length(pred.args)>1, " forecasts.", " forecast.")
                    )
doc = addParagraph(doc, paragraph, stylename = "Normal")
   
doc = addParagraph(doc, paste(" "), stylename="Normal")
   
paragraph <- "A couple of remarks are in order in connection with an interval forecast:"
doc = addParagraph(doc, paragraph, stylename = "Normal")
   
doc = addParagraph(doc, paste(" "), stylename="Normal")
   
sometext <- c("The width of the interval forecast conveys information regarding forecast uncertainty (the wider the interval forecast, the more uncertain the forecast);", 
              "The interval forecast conveys more information than the associated point forecast.")
doc = addParagraph( doc, value = sometext, stylename="BulletList" )

doc = addParagraph(doc, paste(" "), stylename="Normal")

if (bootmethod=="meboot") {

   paragraph <- paste0("For the forecasting model(s) considered in this report, the interval forecast derivation ",
                   "was performed by applying maximum entropy bootstrapping to the historical time series of abundance. ",
                    "B = ", B, " bootstrap replicates were used per forecasting model.")
   doc = addParagraph(doc, paragraph, stylename="Normal")
   
   doc = addParagraph(doc, paste(" "), stylename="Normal")
   
}


if (bootmethod=="stlboot") {

   paragraph <- paste0("For the forecasting model(s) considered in this report, the interval forecast derivation ",
                   "was performed by applying loess bootstrapping to the historical time series of abundance. ",
                    "B = ", B, " bootstrap replicates were used per forecasting model.")
   doc = addParagraph(doc, paragraph, stylename="Normal")
   
   doc = addParagraph(doc, paste(" "), stylename="Normal")
   
}


if (bootmethod=="meboot") {

paragraph <- paste("The maximum entropy bootstrapping is a time series bootstrapping method introduced by Vinod in 2004 and 2006.",
                   "The method constructs an ensemble of bootstrapped time series using a seven-step algorithm",
                   "designed to satisfy the ergodic theorem (i.e., the grand mean of all ensembles is close to the sample mean).",
                   "The algorithm's practical appeal is that it avoids all structural change and unit root type testing involving complicated asymptotics",
                   "and all shape-destroying transformations like detrending or differencing to achieve stationarity.",
                   "The constructed ensemble elements retain the basic shape and time dependence structure of the",
                   "autocorrelation function (ACF) and the partial autocorrelation function (PACF) of the original time series.", 
                   sep=" ")
doc = addParagraph(doc, paragraph, stylename="Normal")
   
}


if (bootmethod=="stlboot") {

    paragraph <- paste("The loess bootstrapping is a time series bootstrapping method introduced by Bergmeir, Hyndman and Benitez in 2014",
                   "in their working paper on bagging exponential smoothing methods using the STL decomposition and the Box-Cox transformation.",
                   "In this method, the time series of annual abundance values which needs to be bootstrapped is first transformed via a Box-Cox transformation.",
                   "The transformed time series is then",
                   "decomposed into its trend and remainder components using the loess method (i.e., a smoothing method based on local linear regression).",
                   "Finally, the remainder component is bootstrapped using the moving block bootstrap (MBB),",
                   "the trend and seasonal components are added back, and the Box-Cox transformation is inverted.",
                   "In this way, a random pool of similar bootstrapped time series is generated from the original time series.",
                   sep=" ")
    doc = addParagraph(doc, paragraph, stylename="Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

}






##
## Point Forecasts
##

doc = addTitle( doc, "Point Forecasts", level=2)   

doc = addParagraph(doc, paste(" "), stylename="Normal")

tablelegend <- paste0("Point forecasts of ", tolower(stockabundance), " ",
                      "corresponding to the ", 
                      stockname, " ", 
                      stockspecies, " stock ", 
                      "and the ", forecastingyear, " forecasting year.")
doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend") 

tt <- table.results.individual.stock.all.models.no.age
tt <- tt[,-ncol(tt)]

# set cell padding defaut to 2
## baseCellProp = cellProperties( padding = 2 )
# Create a FlexTable with tt data frame
my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 2 ), 
                  header.cell.props = cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "right"),
                  add.rownames = FALSE
)

my_ft[, 1, ] = parProperties(text.align = "left")
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, my_ft)


if (noagemodelnaiveone){

  #--- Barplot of historical and forecasted values: naiveone -----------------------------------------------------
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

  ## doc=addPageBreak(doc)

}

if (noagemodelavgthree){
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
        width=plotwidth, height=plotheight-1)

  doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

  rm(myplot)
  
  ## doc=addPageBreak(doc)

}


if (noagemodelavgfive){
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
  
  ## doc=addPageBreak(doc)

}


if (noagemodelarima){
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
  
  ## doc=addPageBreak(doc)
  
}


if (noagemodelexpsmooth){
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
        width=plotwidth, height=plotheight-1)

  doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

  rm(myplot)
  
  ## doc=addPageBreak(doc)
}


##
## Interval Forecasts 
##

##---- Table of Interval Forecasts for All Models ----------------------------------

doc=addPageBreak(doc)

doc = addTitle(doc, "Interval Forecasts", level=2)

doc = addParagraph(doc, paste(" "), stylename="Normal")

if (bootmethod == "meboot") {
   
   if (length(all.args) == 1) {
      text0 <- "The interval forecast was derived via maximum entropy bootstrapping."
   }
    
   if (length(all.args) > 1) {
      text0 <- "The interval forecasts were derived via maximum entropy bootstrapping."
   }
   
} 

if (bootmethod == "stlboot") {

   if (length(all.args) == 1) {
      text0 <- "The interval forecast was derived via loess bootstrapping."
   } 
   
   if (length(all.args) > 1) {
      text0 <- "The interval forecasts were derived via loess bootstrapping."
   } 

} 

tablelegend <- paste0("Point and interval forecasts of ", tolower(stockabundance), " ",
                      "corresponding to the ", 
                      stockname, " ", 
                      stockspecies, " stock ", 
                      "and the ", forecastingyear, " forecasting year. ", 
                      text0)

rm(text0)
                      
doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend") 

tt <- table.results.individual.stock.all.models.no.age
## tt <- tt[,-ncol(tt)]

# set cell padding defaut to 2
# baseCellProp = cellProperties(padding = 5)
# Create a FlexTable with tt data frame
my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties(padding = 5), 
                  header.cell.props = cellProperties(padding = 5), 
                  header.par.props = parProperties(text.align = "right"),
                  add.rownames = FALSE
)


## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

my_ft[, 1, ] = parProperties(text.align = "left")
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

# my_ft[, 1, to="header"] = parProperties(text.align = "left")
# to="header"
# my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

doc = addFlexTable(doc, my_ft)

if (length(pred.args)>1) { doc = addPageBreak(doc) }

if (noagemodelnaiveone){
  
  ##--- plot forecasted values & forecast intervals:  scatterplot (naiveone)
  
  if (bootmethod == "meboot") {    
      text0 <- "The interval forecast was derived via maximum entropy bootstrapping."
  } 

  if (bootmethod == "stlboot") {
      text0 <- "The interval forecast was derived via loess bootstrapping."
  } 
  
  plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ", 
                     "corresponding to the ", 
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point forecast was obtained via naive forecasting (previous year). ", 
                         text0)
  rm(text0)

  myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.naiveone.no.age(pred.int.individual.stock.naiveone.no.age, 
                                                                                                 forecastingyear)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


  rm(myplot)

  doc = addPageBreak(doc)
  
}



if (noagemodelavgthree){

  ##--- plot forecasted values & forecast intervals:  scatterplot (avgthree)

    
  if (bootmethod == "meboot") {    
      text0 <- "The interval forecast was derived via maximum entropy bootstrapping."
  } 

  if (bootmethod == "stlboot") {
      text0 <- "The interval forecast was derived via loess bootstrapping."
  } 
  
  plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ", 
                     "corresponding to the ", 
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point forecast was obtained via naive forecasting (average of previous 3 years). ", 
                         text0)

   rm(text0)

   myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.avgthree.no.age(pred.int.individual.stock.avgthree.no.age, 
                                                                                                 forecastingyear)

   doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


  rm(myplot)
  
  doc = addPageBreak(doc)

}



if (noagemodelavgfive){

  ##--- plot forecasted values & forecast intervals:  scatterplot (avgfive)

  if (bootmethod == "meboot") {    
      text0 <- "The interval forecast was derived via maximum entropy bootstrapping."
  } 

  if (bootmethod == "stlboot") {
      text0 <- "The interval forecast was derived via loess bootstrapping."
  } 

  plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ", 
                     "corresponding to the ", 
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point forecast was obtained via naive forecasting (average of previous 5 years). ", 
                         text0)
  rm(text0)

  myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.avgfive.no.age(pred.int.individual.stock.avgfive.no.age, 
                                                                                                 forecastingyear)


  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

  rm(myplot)

  doc = addPageBreak(doc)

}


if (noagemodelarima){

  ##--- plot forecasted values & forecast intervals:  scatterplot (arima)

  
  if (bootmethod == "meboot") {    
      text0 <- "The interval forecast was derived via maximum entropy bootstrapping."
  } 

  if (bootmethod == "stlboot") {
      text0 <- "The interval forecast was derived via loess bootstrapping."
  } 

  plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ", 
                     "corresponding to the ", 
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point forecast was obtained via ARIMA forecasting. ", 
                         text0)
                         
  rm(text0)

  myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.arima.no.age(pred.int.individual.stock.arima.no.age, 
                                                                                                 forecastingyear)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


  rm(myplot)
  
  doc = addPageBreak(doc)

}


if (noagemodelexpsmooth){

  ##--- Plot forecasted values & forecast intervals:  scatterplot (expsmooth)

  if (bootmethod == "meboot") {    
      text0 <- "The interval forecast was derived via maximum entropy bootstrapping."
  } 

  if (bootmethod == "stlboot") {
      text0 <- "The interval forecast was derived via loess bootstrapping."
  } 

  plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ", 
                     "corresponding to the ", 
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point and interval forecasts were obtained via exponential smoothing forecasting. ", 
                         text0)

  myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.expsmooth.no.age(pred.int.individual.stock.expsmooth.no.age, 
                                                                                                 forecastingyear)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

  rm(myplot)
  
  doc = addPageBreak(doc)

}




##--- Histogram of Bootstrap Predictions: All Models for Stock with No Age Information


plotlegend <- paste0(ifelse(length(pred.args)>1, "Histograms ", "Histogram "),  
                     "of the B", " = ", scales::comma(B), " bootstrapped point forecasts for the ", 
                     forecastingyear, " ", 
                     tolower(stockabundance), 
                     " corresponding to the ", 
                     stockname, " ", 
                     stockspecies, ".", 
                     # "The bootstrapped point forecasts were produced via naive, ARIMA and exponential smoothing modeling. ", 
                     ifelse(length(pred.args)>1," For each histogram, the dashed red line indicates the position on the horizontal axis of the point forecast of ",
                                                " The dashed red line indicates the position on the horizontal axis of the point forecast of "), 
                      tolower(stockabundance), ", ", 
                      "while the blue segment indicates the 80% forecast interval of ",
                      tolower(stockabundance), 
                      ".")

myplot <- plot.yboot.all.models.no.age(pred.args, stockabundance)
                             
doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=ifelse(length(pred.args)>1,plotheight,plotheight-3))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


rm(myplot)


#================================================================================================
# Retrospective Performance for All Models 
#================================================================================================

doc = addPageBreak(doc)

doc = addTitle( doc, "Retrospective Forecast Performance", level = 1)

doc = addParagraph(doc, paste(" "), stylename="Normal")

##
## Stats Tutorial
##

paragraph <- paste0("This section reports the results associated with the retrospective analysis conducted to evaluate the performance of the ", 
                    ifelse(length(pred.args), "models ", "model "), 
                    "used to forecast the ",  
                    tolower(stockabundance),  
                    " corresponding to the ", 
                    stockspecies, " ", 
                    stockname, " stock.")
doc = addParagraph(doc, paragraph, stylename = "Normal")     
     
doc = addParagraph(doc, paste(" "), stylename="Normal")
     
paragraph <- paste0("For the retrospective analysis, we initialized ", 
                    ifelse(length(pred.args)>1, "each forecasting model", paste0("the ", best.model.no.age)), 
                    " with data from the first K historical run years for the ", stockname, " stock ", 
                    "and used these data to produce a retrospective point forecast of ", tolower(stockabundance), " for the year K+1. ", 
                    "We repeated this process for values of K ranging from ", index, " to N-1 in increments of 1, with N being the number of run years ", 
                    "for which historical ", tolower(stockabundance), " values were recorded. ",
                    "Comparing the values of the retrospective point forecasts of ", tolower(stockabundance), " against the historical values of ",
                    tolower(stockabundance), " obtained in this fashion enabled us to compute a series of ", 
                    paste0("N-",index), " retrospective forecast errors."  
                    )
doc = addParagraph(doc, paragraph, stylename = "Normal")   
     
doc = addParagraph(doc, paste(" "), stylename="Normal")
     
paragraph <- paste0(ifelse(length(pred.args)>1, "For each model,", "For the considered model, "), 
                    " retrospective forecast errors were defined as the actual ", 
                    tolower(stockabundance)," ", 
                    "values minus the retrospectively forecasted ", 
                    tolower(stockabundance), 
                    " values. ",
                    " In view of this definition,  positive values for the retrospective forecast errors ", 
                    "represent forecasts that were smaller than the historical ", 
                    tolower(stockabundance),
                    " values (i.e., underforecasts), ",  
                    "whereas negative values represent forecasts that were larger than the historical ", 
                    tolower(stockabundance), 
                    " values (i.e., overforecasts).")
doc = addParagraph(doc, paragraph, stylename = "Normal")  

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("The following performance ", 
                    ifelse(length(table.rank.rmse.results.no.age$ranking.measures)>1,"measures ","measure "),
                    ifelse(length(table.rank.rmse.results.no.age$ranking.measures)>1,"were used ","was used "), 
                    "to characterize ",
                    ifelse(length(table.rank.rmse.results.no.age$ranking.measures)>1,"different aspects of ", ""), 
                    "the distribution of the retrospective forecasting errors:")
doc = addParagraph(doc, paragraph, stylename = "Normal")  
                    
doc = addParagraph(doc, paste(" "), stylename="Normal")

texts <- table.rank.rmse.results.no.age$ranking.measures 
for (i in 1:length(texts)){
   if (texts[i]=="MRE") {texts[i] <- "Mean Raw Error (MRE);"} 
   if (texts[i]=="MAE") {texts[i] <- "Mean Absolute Error (MAE);"}  
   if (texts[i]=="MPE") {texts[i] <- "Mean Percent Error (MPE);"}  
   if (texts[i]=="MAPE") {texts[i] <- "Mean Absolute Percent Error (MAPE);"}  
   if (texts[i]=="MASE") {texts[i] <- "Mean Scaled Error (MASE);"}  
   if (texts[i]=="RMSE") {texts[i] <- "Root Mean Square Error (RMSE);"}  
}

usePackage("stringr")
texts[length(texts)] <- str_replace_all(texts[length(texts)], ";", ".")
texts

doc = addParagraph( doc, value = texts, stylename="BulletList")

doc = addParagraph(doc, paste(" "), stylename="Normal")

## rm(texts)
      
      
texts <- paste0("Details about ", 
                ifelse(length(table.rank.rmse.results.no.age$ranking.measures)>1,"each performance measure "," this performance measure"), 
                " are provided below.")    
doc = addParagraph( doc, value = texts, stylename="Normal")
rm(texts)
                       
                       
doc = addParagraph(doc, paste(" "), stylename="Normal")
                       
#================ 
                    
## See: http://people.duke.edu/~rnau/compare.htm
## See: http://onlinehelp.tableausoftware.com/current/pro/online/mac/en-us/forecast_describe.html

## IMPORTANT: https://github.com/robjhyndman/forecast/blob/master/R/errors.R

if (retromeasureMRE){

    mypot <- pot(paste0("Mean Relative Error (MRE)"), textProperties( font.weight = "bold" ))
    mypar <- set_of_paragraphs(mypot)
    doc = addParagraph(doc, mypot, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("The mean relative error (MRE) is computed by summing the retrospective forecast errors and then dividing the result by the ", 
                    "number ",
                    paste0("N-",index), 
                    ## N-10 ,
                    " of retrospective forecast errors included in the sum.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")  

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("MRE reflects directional bias in the raw forecast errors, ", 
                    "and a value of zero is most desirable.  Negative values indicate a tendency to overforecast ", 
                    "and positive values reflecting a tendency to underforecast.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")  

} 

if (retromeasureMAE){

    mypot <- pot(paste0("Mean Absolute Error (MAE)"), textProperties( font.weight = "bold" ))
    mypar <- set_of_paragraphs(mypot)
    doc = addParagraph(doc, mypot, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("The mean absolute error (MAE) is a quantity used to measure how close forecasts are to the eventual outcomes. ", 
                    "Calculation of MAE is simple. It involves summing the magnitudes (absolute values) of the ", 
                    "retrospective forecast errors to obtain the ‘total error’ and then dividing the total error by the number ", 
                     paste0("N-",index), 
                     ## N-10, 
                     " of retrospective forecast errors ", 
                    "included in the sum.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("MAE is measured in the same units as the data, and is usually similar in magnitude to, ",
             "but slightly smaller than, the root mean squared error (RMSE). ",
             "MAE is less sensitive to the occasional very large forecast error because it does not square the errors in the calculation of the 'total error'.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")

}

if (retromeasureMPE){

    mypot <- pot(paste0("Mean Percentage Error (MPE)"), textProperties( font.weight = "bold" ))
    mypar <- set_of_paragraphs(mypot)
    doc = addParagraph(doc, mypot, stylename = "Normal")
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("MPE reflects directional bias in the relative forecast errors, ", 
                    "with negative values indicating a tendency to overforecast ", 
                    "and positive values reflecting a tendency to underforecast.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")  

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("In this report, MPE values are expressed as proportions rather than percentages.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")

}


if (retromeasureMPE){

    mypot <- pot(paste0("Mean Absolute Percentage Error (MPE)"), textProperties( font.weight = "bold" ))
    mypar <- set_of_paragraphs(mypot)
    doc = addParagraph(doc, mypot, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("The mean absolute percentage error (MAPE) is the average of all of the percentage errors, with the average being ",
                    "taken without regard to sign so as to avoid the problem of positive and negative values cancelling one another.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("MAPE measures the magnitude of the forecasting errors compared to the magnitude of the actual data values, as a percentage. ",
                    "So, a MAPE of 20% is better than a MAPE of 60%. ",
                    "Also, perfect forecasts would have no error and would return a MAPE value of 0%. ",
                    "MAPE can only be computed with respect to data that are guaranteed to be strictly positive.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("Note that there is no effective upper bound on the MAPE metric.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("Lewis (1982) suggested interpreting typical MAPE values ",
                    "in the context of analyzing industrial and business data along the lines illustrated in the table shown below.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    mytable <- data.frame(MAPE=c(" < 10%","10% - 20%","20% - 50%",">50%"),
                      Interpretation=c("Highly accurate forecasting", "Good forecasting", "Reasonable forecasting", "Inaccurate forecasting"))

    my_ft <- FlexTable( data = mytable,
                    header.columns = FALSE,
                    body.cell.props = cellProperties( padding = 2 ),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

    doc = addFlexTable(doc, flextable=my_ft)

    rm(mytable)
    
    paragraph <- " "
    doc = addParagraph(doc, paragraph, stylename = "Normal")
    
    paragraph <- paste0("In this report, MAPE values are expressed as proportions rather than percentages.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")
    
}


if (retromeasureMASE){

    mypot <- pot(paste0("Mean Absolute Scaled Error (MASE)"), textProperties( font.weight = "bold" ))
    mypar <- set_of_paragraphs(mypot)
    doc = addParagraph(doc, mypot, stylename = "Normal")
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("MASE measures the magnitude of the error compared to the magnitude of the error of a naive one-step ahead forecast as a ratio. ",
        "A naive forecast assumes that whatever the stock abundance value was last year it will be the same value this current year. ",
        "Ideally, the value of MASE will be significantly less than 1. ",
        "For example, a MASE of 0.5 means that this year's forecast is likely to have half as much error as a naive forecast. ",
        # "which is better than a MASE of 1.0, which is no better than a naive forecast. ",
        "Since MASE is a normalized statistic that is defined for all data values and weighs errors evenly, ",
        "it is an excellent metric for comparing the quality of different forecasting methods.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("The advantage of MASE over the more common MAPE metric is that MASE is defined for time series that contain zero, whereas MAPE is not. ", 
                    "Also, MASE weights errors equally, whereas MAPE weights positive and/or extreme errors more heavily. ", 
                    "(MAPE stands for Mean Absolute Percentage Error.)")
    doc = addParagraph(doc, paragraph, stylename = "Normal")
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")

} 

if (retromeasureRMSE){

    mypot <- pot(paste0("Root Mean Squared Error (RMSE)"), textProperties( font.weight = "bold" ))
    mypar <- set_of_paragraphs(mypot)
    doc = addParagraph(doc, mypot, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("RMSE provides a measure of the variability of the retrospective forecast errors.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("Calculation of the RMSE involves a sequence of 3 simple steps. ", 
                    "‘Total square error’ is obtained first as the sum of the individual squared errors; ", 
                    " that is, each error influences the total in proportion to its square, rather than its magnitude. ", 
                    " Large errors, as a result, have a relatively greater influence on the total square error than do the smaller errors. ", 
                    " This means that the total square error will grow as the total error is concentrated within a decreasing number of ", 
                    " increasingly large individual errors. Total square error then is divided by n, which yields the mean-square error (MSE). ", 
                    " The third and final step is to take RMSE as the square root of the MSE.")
    doc = addParagraph(doc, paragraph, stylename = "Normal")

    doc = addParagraph(doc, paste(" "), stylename="Normal")

    paragraph <- paste0("The magnitude of RMSE is influenced by the following three quantities: (i) variability of the error magnitudes (or squared errors), ",
                    "(ii) the average-error magnitude (MAE) and (iii) the square root of ", 
                    paste0("N-",index), ", where ", 
                    paste0("N-",index), " is ", 
                    ## N-10, where N-10 is ", 
                    "the number of retrospective forecast errors used to calculate RMSE. ", 
                    "Without benefit of other information (e.g., MAE), it is impossible to discern to what extent RMSE ", 
                    "reflects central tendency (average error) and to what extent it represents the variability within ",
                    "the distribution of squared errors or the square root of ", 
                    paste0("N-",index), 
                    ## N-10, 
                     ".")
                    
    doc = addParagraph(doc, paste(" "), stylename="Normal")

}
                    
                    

## paragraph <- paste0("This section also reports the coefficient of determination obtained by regressing the retrospectively forecasted ", 
##                    tolower(stockabundance), 
##                    " values on the historical ", 
##                    tolower(stockabundance), 
##                    " values ",
##                    ifelse(length(pred.args),"for each forecasting model. ",paste0("for the ",best.model.no.age,".")),  
##                    "This is simply the squared correlation coefficient of the retrospectively forecasted and historical ", 
##                     tolower(stockabundance),
##                    " values.")
## doc = addParagraph( doc, value = paragraph, stylename="Normal")
                    

paragraph <- paste0("The section concludes by reporting the retrospective forecast errors in table form along with two types of plots constructed from these errors:")
doc = addParagraph(doc, paragraph, stylename = "Normal")    
       
doc = addParagraph(doc, paste(" "), stylename="Normal")
       
texts <- c(ifelse(length(pred.args)>1, "	i) Histograms of the retrospective forecast errors for all forecasting models;", 
                                       paste0("	i) Histogram of the retrospective forecast errors produced by the ",best.model.no.age,";")), 
           ifelse(length(pred.args)>1, 
                  "	ii) Barplots of the retrospective forecast errors for all forecasting models, enhanced with the corresponding forecast interval.",
                   paste0("	ii) Barplot of the retrospective forecast errors produced by the ", best.model.no.age,", enhanced with the corresponding forecast interval.")
                  )
)
doc = addParagraph( doc, value = texts, stylename="Normal")

          
##
## Measures of Retrospective Forecast Performance for All Models 
##

if (ncol(table.rank.rmse.results.no.age$measures)>1) {
doc = addTitle( doc, "Measures of Retrospective Point Forecast Performance", level = 2)
doc = addParagraph(doc, value=paste("Measures of retrospective point forecast performance."), stylename="rTableLegend") 
}



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
## tt 
usePackage("scales")
tt[,-1] <- apply(tt[,-1],2,comma)


# set cell padding defaut to 2
## baseCellProp = cellProperties( padding = 5 )
# Create a FlexTable with tt data frame
my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 5 ), 
                  header.cell.props = cellProperties( padding = 5 ), 
                  header.par.props = parProperties(text.align = "left"),
                  add.rownames = FALSE
)


## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

my_ft[, 1, ] = parProperties(text.align = "left")
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

# my_ft[, 1, to="header"] = parProperties(text.align = "left")
# to="header"
# my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

doc = addFlexTable(doc, my_ft)



##
## Scatter Plot of Forecasted vs. Observed Values for All Models 
##


doc = addPageBreak(doc)

plotlegend <- paste0(ifelse(length(pred.args)>1,"Scatter plots ","Scatter plot "),  
                     "of retrospectively forecasted versus actual ",
                     tolower(stockabundance)," values for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.",
                         # " corresponding to all models considered for forecasting.",
                         " Observations are labeled according to the associated historical return years.")


myplot <- scatter.plot.results.afe.individual.stock.retro.all.models.no.age(retro.args, stockabundance)



doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth+0.5, height=ifelse(length(pred.args)>1,plotheight+1, plotheight-2))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


rm(myplot)



##
## Time Series Plot of Forecasted and Observed Values for All Models 
##


doc = addPageBreak(doc)

plotlegend <- paste0(ifelse(length(pred.args)>1,"Time series plots ","Time series plot "), 
                     "of retrospectively forecasted versus actual ",
                     tolower(stockabundance)," values for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.")
                         # " corresponding to all models considered for forecasting.")


myplot <- timeseries.plot.results.afe.individual.stock.retro.all.models.no.age(retro.args, stockabundance)



doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth+0.5, height=ifelse(length(pred.args)>1,plotheight+1, plotheight-2))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


rm(myplot)



## 
## R-Squared Values for All Models
##

## rvalue <- ifelse(length(pred.args)>1, 
##                   paste("R-squared values for all considered forecasting models."), 
##                   paste("R-squared values for the considered forecasting model."))

## doc = addParagraph(doc, 
##                   value=rvalue, 
##                   stylename="rTableLegend") 

## tt <- r.squared.retro.individual.stock.all.models.no.age 
## colnames (tt) <- c("Model","R-Squared")



#### set cell padding defaut to 2
#### baseCellProp = cellProperties( padding = 2 )

## Create a FlexTable with tt data frame
## my_ft = FlexTable(data = tt,
##                  body.cell.props = cellProperties( padding = 2 ), 
##                  header.cell.props = cellProperties( padding = 2 ), 
##                  header.par.props = parProperties(text.align = "right"),
##                  add.rownames = FALSE
## )


#### my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

## my_ft[, 1, ] = parProperties(text.align = "left")
## my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

#### my_ft[, 1, to="header"] = parProperties(text.align = "left")
#### to="header"
#### my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

## doc = addFlexTable(doc, my_ft)


##
## RETROSPECTIVE POINT FORECASTS AND FORECAST ERRORS 
##

doc = addTitle( doc, "Retrospective Point Forecasts and Forecast Errors", level = 2)

doc = addParagraph(doc, paste(" "), stylename="Normal")

if (noagemodelnaiveone){
  
  ##--- naiveone ----------------------------------------------------------------------------------------------------------------------------

  doc = addParagraph(doc, value=" ", stylename="Normal") 

  tablelegend <- paste0("Retrospective point forecasts and associated forecast errors for the ", 
                      forecastingyear, " ", 
                      tolower(stockabundance), " ", 
                      "corresponding to the ", 
                      stockname, " ", 
                      stockspecies, " stock. ", 
                      "Accompanying return years and actual ", 
                      tolower(stockabundance), " ", 
                      "values are also reported. ", 
                      "The retrospective point forecasts were obtained via naive forecasting (previous year). ", 
                      "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend") 


  tt <- rmse.results.no.age.naiveone$resjoin
  tt <- subset(tt, select=-c(p.bench, e.bench))
  
  names(tt) <- c("Return Year", "Actual", "Forecasted", "Error")
  ## head(tt)
  usePackage("scales")
  tt[,-1] <- apply(tt[,-1],2, comma)

  # set cell padding defaut to 2
  # baseCellProp = cellProperties( padding = 2 )
  # Create a FlexTable with tt data frame
  my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 2 ), 
                  header.cell.props = cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "left"),
                  add.rownames = FALSE)


  ## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )
  my_ft[, 1, ] = parProperties(text.align = "left")
  my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

  # my_ft[, 1, to="header"] = parProperties(text.align = "left")
  # to="header"
  # my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

  doc = addFlexTable(doc, my_ft)
  
}


if (noagemodelavgthree){

  ##--- avgthree ----------------------------------------------------------------------------------------------------------------------------

  doc = addParagraph(doc, value=" ", stylename="Normal") 

  tablelegend <- paste0("Retrospective point forecasts and associated forecast errors for the ", 
                      forecastingyear, " ", 
                      tolower(stockabundance), " ", 
                      "corresponding to the ", 
                      stockname, " ", 
                      stockspecies, " stock. ", 
                      "Accompanying return years and actual ", 
                      tolower(stockabundance), " ", 
                      "values are also reported. ", 
                      "The retrospective point forecasts were obtained via naive forecasting (average of previous 3 years). ", 
                      "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend") 

  tt <- rmse.results.no.age.avgthree$resjoin
  tt <- subset(tt, select=-c(p.bench, e.bench))
  names(tt) <- c("Return Year", "Actual", "Forecasted", "Error")
  ## head(tt)

  usePackage("scales")
  tt[,-1] <- apply(tt[,-1],2, round)
  tt[,-1] <- apply(tt[,-1],2, comma)

  # set cell padding defaut to 2
  # baseCellProp = cellProperties( padding = 2 )
  # Create a FlexTable with tt data frame
  my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 2 ), 
                  header.cell.props = cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "left"),
                  add.rownames = FALSE)

  ## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

  my_ft[, 1, ] = parProperties(text.align = "left")
  my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

  # my_ft[, 1, to="header"] = parProperties(text.align = "left")
  # to="header"
  # my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

  doc = addFlexTable(doc, my_ft)
}



if (noagemodelavgfive){

  ##--- avgfive ----------------------------------------------------------------------------------------------------------------------------
  
  doc = addParagraph(doc, value=" ", stylename="Normal") 

  tablelegend <- paste0("Retrospective point forecasts and associated forecast errors for the ", 
                      forecastingyear, " ", 
                      tolower(stockabundance), " ", 
                      "corresponding to the ", 
                      stockname, " ", 
                      stockspecies, " stock. ", 
                      "Accompanying return years and actual ", 
                      tolower(stockabundance), " ", 
                      "values are also reported. ", 
                      "The retrospective point forecasts were obtained via naive forecasting (average of previous 5 years). ", 
                      "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend") 

  tt <- rmse.results.no.age.avgfive$resjoin
  tt <- subset(tt, select=-c(p.bench, e.bench))

  names(tt) <- c("Return Year", "Actual", "Forecasted", "Error")
  ## head(tt)
  usePackage("scales")
  tt[,-1] <- apply(tt[,-1],2, comma)

  # set cell padding defaut to 2
  # baseCellProp = cellProperties( padding = 2 )
  # Create a FlexTable with tt data frame
  my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 2 ), 
                  header.cell.props = cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "left"),
                  add.rownames = FALSE)

  ## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

  my_ft[, 1, ] = parProperties(text.align = "left")
  my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

  # my_ft[, 1, to="header"] = parProperties(text.align = "left")
  # to="header"
  # my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

  doc = addFlexTable(doc, my_ft)
  
}

if (noagemodelarima){

  ##--- arima ----------------------------------------------------------------------------------------------------------------------------

  doc = addParagraph(doc, value=" ", stylename="Normal") 

  tablelegend <- paste0("Retrospective point forecasts and associated forecast errors for the ", 
                      forecastingyear, " ", 
                      tolower(stockabundance), " ", 
                      "corresponding to the ", 
                      stockname, " ", 
                      stockspecies, " stock. ", 
                      "Accompanying return years and actual ", 
                      tolower(stockabundance), " ", 
                      "values are also reported. ", 
                      "The retrospective point forecasts were obtained via ARIMA forecasting. ", 
                      "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend") 

  tt <- rmse.results.no.age.arima$resjoin
  tt <- subset(tt, select=-c(p.bench, e.bench))
  
  names(tt) <- c("Return Year", "Actual", "Forecasted", "Error")
  ## head(tt)
  usePackage("scales")
  tt[,-1] <- apply(tt[,-1],2, comma)

  # set cell padding defaut to 2
  ## baseCellProp = cellProperties( padding = 2 )
  # Create a FlexTable with tt data frame
  my_ft = FlexTable(data = tt,
                  body.cell.props =  cellProperties( padding = 2 ), 
                  header.cell.props =  cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "left"),
                  add.rownames = FALSE)

  ## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

  my_ft[, 1, ] = parProperties(text.align = "left")
  my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

  # my_ft[, 1, to="header"] = parProperties(text.align = "left")
  # to="header"
  # my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

  doc = addFlexTable(doc, my_ft)

}


if (noagemodelexpsmooth){

  ##--- expsmooth ----------------------------------------------------------------------------------------------------------------------------

  doc = addParagraph(doc, value=" ", stylename="Normal") 

  tablelegend <- paste0("Retrospective point forecasts and associated forecast errors for the ", 
                      forecastingyear, " ", 
                      tolower(stockabundance), " ", 
                      "corresponding to the ", 
                      stockname, " ", 
                      stockspecies, " stock. ", 
                      "Accompanying return years and actual ", 
                      tolower(stockabundance), " ", 
                      "values are also reported. ", 
                      "The retrospective point forecasts were obtained via exponential smoothing forecasting. ", 
                      "A positive forecast error indicates an under-forecast while a negative forecast error indicates an over-forecast.")

  doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend") 

  tt <- rmse.results.no.age.expsmooth$resjoin
  tt <- subset(tt, select=-c(p.bench, e.bench))
  
  names(tt) <- c("Return Year", "Actual", "Forecasted", "Error")
  ## head(tt)
  usePackage("scales")
  tt[,-1] <- apply(tt[,-1],2, comma)

  # set cell padding defaut to 2
  # baseCellProp = cellProperties( padding = 2 )
  # Create a FlexTable with tt data frame
  my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 2 ), 
                  header.cell.props = cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "left"),
                  add.rownames = FALSE)

  ## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

  my_ft[, 1, ] = parProperties(text.align = "left")
  my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

  # my_ft[, 1, to="header"] = parProperties(text.align = "left")
  # to="header"
  # my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

  doc = addFlexTable(doc, my_ft)

}


## doc = addPageBreak(doc)

##---- Density Plots of Retrospective Forecast Errors for All Models ------------
##---- (Separate Plots) ---------------------------------------------------------

plotlegend <- paste0("Density plots of forecast errors produced by the models considered for forecasting the ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ")

myplot <- plot.dens.retrospective.forecast.errors.individual.stock.no.age(rmse.results.no.age.args, retro.args)

doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth-2, height=ifelse(length(pred.args)>1,plotheight,plotheight-3))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


rm(myplot)


##---- Density Plots of Retrospective Forecast Errors for All Models ------------
##---- (Superimposed Plots) ---------------------------------------------------------


plotlegend <- paste0("Superimposed density plots of forecast errors produced by the models considered for forecasting the ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ")

myplot <- plot.superimposed.dens.retrospective.forecast.errors.individual.stock.no.age(rmse.results.no.age.args, retro.args)

doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-2)

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


rm(myplot)


##---- Plots illustrating how well the retrospective point forecasts work --------------------

if (noagemodelnaiveone) {

  plotlegend <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of ",
                     tolower(stockabundance), 
                     " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock, ", 
                     "with the forecasted values computed on the basis of naive forecasting (previous year).", 
                     " Historical values of ", tolower(stockabundance), 
                     " (grey lines) and fitted values produced by the naive modeling (previous year) are also shown. ", 
                     "Each panel corresponds to a particular retrospective forecasting year.")

  myplot <- individual.stock.retro.plot.no.age.naiveone(no.age.retro.plot.info.naiveone, stockabundance)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth+1, height=plotheight)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


  rm(myplot)

}

if (noagemodelavgthree) {

  plotlegend <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of ",
                     tolower(stockabundance), 
                     " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock, ", 
                     "with the forecasted values computed on the basis of naive forecasting (average of previous 3 years).", 
                     " Historical values of ", tolower(stockabundance), 
                     " (grey lines) and fitted values produced by the naive modeling (average of previous 3 years) are also shown. ", 
                     "Each panel corresponds to a particular retrospective forecasting year.")

  myplot <-  individual.stock.retro.plot.no.age.avgthree(no.age.retro.plot.info.avgthree, stockabundance)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth+1, height=plotheight)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


  rm(myplot)

}


if (noagemodelavgfive) {

  plotlegend <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of ",
                     tolower(stockabundance), 
                     " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock, ", 
                     "with the forecasted values computed on the basis of naive forecasting (average of previous 5 years).", 
                     " Historical values of ", tolower(stockabundance), 
                     " (grey lines) and fitted values produced by the naive modeling (average of previous 5 years) are also shown. ", 
                     "Each panel corresponds to a particular retrospective forecasting year.")

  myplot <-  individual.stock.retro.plot.no.age.avgfive(no.age.retro.plot.info.avgfive, stockabundance)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth+1, height=plotheight)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


  rm(myplot)

}

if (noagemodelarima) {

  plotlegend <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of ",
                     tolower(stockabundance), 
                     " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock, ", 
                     "with the forecasted values computed on the basis of ARIMA forecasting.", 
                     " Historical values of ", tolower(stockabundance), 
                     " (grey lines) and fitted values produced by the ARIMA modeling are also shown. ", 
                     "Each panel corresponds to a particular retrospective forecasting year.")

  myplot <- individual.stock.retro.plot.no.age.arima(no.age.retro.plot.info.arima, stockabundance)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth+1, height=plotheight)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


  rm(myplot)

}


if (noagemodelexpsmooth) {

  plotlegend <- paste0("Actual values (grey dots) and retrospectively forecasted values (red dots) of ",
                     tolower(stockabundance), 
                     " for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock, ", 
                     "with the forecasted values computed on the basis of exponential smoothing forecasting.", 
                     " Historical values of ", tolower(stockabundance), 
                     " (grey lines) and fitted values produced by the exponential smoothing modeling are also shown. ", 
                     "Each panel corresponds to a particular retrospective forecasting year.")

  myplot <- individual.stock.retro.plot.no.age.expsmooth(no.age.retro.plot.info.expsmooth, stockabundance)

  doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth+1, height=plotheight)

  doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

  rm(myplot)

}




##---- Bias Coefficient Plots of Retrospective Forecast Errors for All Models ------------

doc = addPageBreak( doc )

plotlegend <- paste0("Bias coefficient", 
                     ifelse(length(pred.args)>1," plots "," plot "),  
                     "obtained from the retrospective forecast errors produced by the", 
                     ifelse(length(pred.args)>1," models "," model "),  
                     "considered for forecasting the ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.")

myplot <- plot.bias.coefficients.retrospective.forecast.errors.individual.stock.no.age(rmse.results.no.age.args, retro.args)

doc = addPlot(doc=doc,
            fun=grid.draw,
            x= myplot,
            width=plotwidth, height=plotheight+1)

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


rm(myplot)



##---- Table of Bias Coefficients Derived from Retrospective Forecast Errors for All Models ------------

doc = addPageBreak( doc )

doc = addParagraph(doc, value=" ", stylename="Normal") 

tablelegend <- paste0("Bias", 
                      ifelse(length(pred.args)>1," coefficients "," coefficient "),  
                      "obtained from the retrospective forecast errors produced by the", 
                      ifelse(length(pred.args)>1," models "," model "), 
                      "considered for forecasting the ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock.")

doc = addParagraph(doc, value=tablelegend, stylename="rTableLegend") 

tt <- data.frame(Model = names(bias.coeff.afe.individual.stock.retro.no.age), Coefficient = bias.coeff.afe.individual.stock.retro.no.age)
  
rownames(tt) <- NULL
  
names(tt) <- c("Model", "Bias Coefficient")
 
# set cell padding defaut to 2
# baseCellProp = cellProperties( padding = 2 )
# Create a FlexTable with tt data frame
 
my_ft = FlexTable(data = tt,
                  body.cell.props = cellProperties( padding = 2 ), 
                  header.cell.props = cellProperties( padding = 2 ), 
                  header.par.props = parProperties(text.align = "left"),
                  add.rownames = FALSE)

## my_ft[, 1] = textProperties(color="black", font.weight = "bold" )

my_ft[, 1, ] = parProperties(text.align = "left")
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

# my_ft[, 1, to="header"] = parProperties(text.align = "left")
# to="header"
# my_ft[, 2:ncol(tt), to="header"] = parProperties(text.align = "right")

  
doc = addFlexTable(doc, my_ft)

rm(tt)


doc = addPageBreak( doc )

##---- Gary's Plots ----------------------------------------------------------------

if (noagemodelnaiveone){

  ##--- Gary's Plot:  naiveone

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the naive model (previous year) used for forecasting the value of ",
                       tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ", 
                      "A positive forecast error indicates an underforecast while a negative forecast error indicates an overforecast.")

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

}


if (noagemodelavgthree){ 

  ##--- Gary's Plot:  avgthree

  doc=addPageBreak(doc)

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the naive model (average of previous 3 years) used for forecasting the value of ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ", 
                     "A positive forecast error indicates an underforecast while a negative forecast error indicates an overforecast.")

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

}


if (noagemodelavgfive){ 

  ##--- Gary's Plot:  avgfive

  doc=addPageBreak(doc)

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the naive model (average of previous 5 years) used for forecasting the value of ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ", 
                     "A positive forecast error indicates an underforecast while a negative forecast error indicates an overforecast.")

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

}



if (noagemodelarima){ 

  ##--- Gary's Plot:  arima

  doc=addPageBreak(doc)

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the ARIMA model used for forecasting the value of ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ", 
                     "A positive forecast error indicates an underforecast while a negative forecast error indicates an overforecast.")

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

}



if (noagemodelexpsmooth){ 

  ##--- Gary's Plot:  expsmooth

  doc=addPageBreak(doc)

  plotlegend <- paste0("Retrospective forecast errors and 80% interval forecast produced by the exponential smoothing model used for forecasting the value of ",
                       tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ", 
                       "A positive forecast error indicates an underforecast while a negative forecast error indicates an overforecast.")

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


##=============================================================================================
## Model Diagnostics - All Models
##=============================================================================================

doc = addPageBreak(doc)

doc = addTitle( doc, ifelse(length(pred.args)>1,"Model Diagnostics for All Models","Model Diagnostics"), level = 1 )

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("After fitting each of the forecasting models to the historical time series of ", 
                    paste0(tolower(stockabundance),"s, "), 
                    "we need to run diagnostics to validate the models.") 
doc = addParagraph(doc, paragraph, stylename = "Normal")

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("If the ", ifelse(length(pred.args)>1 ,"models provide", "model provides"),  
                    " a good fit to the historical time series of ", 
                    paste0(tolower(stockabundance),"s, "), 
                    "then ", 
                    ifelse(length(pred.args)>1 ,"their residuals", "its residuals"),  
                    " should exhibit no systematic patterns and no temporal dependence.")
doc = addParagraph(doc, paragraph, stylename = "Normal")

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("Useful diagnostic plots for verifying that the residuals exhibit no systematic patterns and no temporal dependence include:")
doc = addParagraph(doc, paragraph, stylename = "Normal")

doc = addParagraph(doc, paste(" "), stylename="Normal")

texts <- c("Time series plot of the model residuals;",
           "Autocorrelation plot of the model residuals;", 
           "Partial autocorrelation plot of the model residuals;",
           "Plot of p-values associated with the Ljung-Box test applied to the model residuals.")
doc = addParagraph(doc, texts, stylename = "BulletList")

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("The Ljung-Box test is a diagnostic tool used to test the lack of fit of an ARIMA model. ", 
                    "The test is applied to the model residuals and examines the first m autocorrelations of the residuals. ", 
                    "If all of these autocorrelations are very small, we conclude that the model does not exhibit significant lack of fit. ",
                    "The Ljung-Box test tests the following hypotheses: Ho: The model does not exhibit lack of fit versus Ha: The model exhibits lack of fit. ",
                    "Small p-values for the Ljung-Box test lead to the rejection of the alternative hypothesis, ", 
                    "suggesting that the model exhibits significant lack of fit. ", 
                    "Conversely, large p-values suggest that the model does not exhibit significant lack of fit. ", 
                    "Since the choice of m is important but somewhat arbitrary, in practice we perform the Ljung-Box test for several consecutive values of m ", 
                    "to see if the p-values it produces are large for all of these values. ", 
                    "If they are, then we conclude that the model does not exhibit lack of fit.")
doc = addParagraph(doc, paragraph, stylename = "Normal")

doc = addParagraph(doc, paste(" "), stylename="Normal")

paragraph <- paste0("If a model provides a good fit to a univariate time series, then: ")
doc = addParagraph(doc, paragraph, stylename = "Normal")

texts <- c("The time series plot of the model residuals should exhibit no systematic patterns; ",
           "The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals; ", 
           "The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;", 
           "The p-values associated with the Ljung-Box test should be large for all values of m considered.")
doc = addParagraph(doc, texts, stylename = "BulletList")

doc = addParagraph(doc, paste(" "), stylename="Normal")

##--- Index Plot of Residuals: All Models -----------------------------------------------------------

plotlegend <- paste0("Index plot of the residuals associated with the models considered for forecasting ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ")

myplot <- index.plot.residuals.individual.stock.all.models.no.age(user.fits, retro.args)

                              
doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=ifelse(length(pred.args)>1, plotheight,plotheight-4))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

rm(myplot)


##--- Time Series Plot of Residuals: All Models -----------------------------------------------------------

## doc = addPageBreak(doc)

plotlegend <- paste0("Time series plot of the residuals associated with the models considered for forecasting ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ")

myplot <- timeseries.plot.residuals.individual.stock.all.models.no.age(user.fits, retro.args)

doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=ifelse(length(pred.args)>1, plotheight,plotheight-4))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

rm(myplot)


##--- ACF Plot of Residuals: All Models -----------------------------------------------------------

## doc = addPageBreak(doc)

plotlegend <- paste0("ACF plot of the residuals associated with the models considered for forecasting ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ")

myplot <- acf.plot.residuals.individual.stock.all.models.no.age(user.fits, retro.args)
                    
doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=ifelse(length(pred.args)>1, plotheight,plotheight-4))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

rm(myplot)



##--- PACF Plot of Residuals: All Models -----------------------------------------------------------

## doc = addPageBreak(doc)

plotlegend <- paste0("PACF plot of the residuals associated with the models considered for forecasting ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ")

myplot <- pacf.plot.residuals.individual.stock.all.models.no.age(user.fits, retro.args)
                    
doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=ifelse(length(pred.args)>1, plotheight,plotheight-4))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

rm(myplot)

##--- P-Values of Ljung-Box Test Applied to Residuals: All Models --------------------------------------


## doc = addPageBreak(doc)


plotlegend <- paste0("P-values of the Ljung-Box test applied to the residuals associated with the models considered for forecasting ",
                     tolower(stockabundance)," for the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ")

myplot <- ljung.box.plot.residuals.individual.stock.all.models.no.age(datalist, user.fits, retro.args, boxcoxtransform)


doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=ifelse(length(pred.args)>1, plotheight,plotheight-4))

doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

rm(myplot)

##============================================================================================= 
## Choosing the Best of All Models 
##=============================================================================================

# tt <- table.results.individual.stock.all.models.no.age


if (length(pred.args)>1) {

    doc = addPageBreak(doc)
    doc = addTitle( doc, "Choosing the Best of All Forecasting Models", level = 1)
    
    doc = addParagraph(doc, paste(" "), stylename="Normal")
    
}


# table.rank.rmse.results.no.age$ranking.method

## More than one model 
if (length(pred.args)>1){

  # paragraph <- "MRE"
  paragraph <- paste0("Several different models were considered in this report for forecasting the ",
                     forecastingyear, " ",
                     tolower(stockabundance), " ",
                     "for the ",
                     stockname, " ",
                     stockspecies, " stock: "
                     # "The optimal point forecast of ",
                     # tolower(stockabundance), " ",
                     # "is the one produced whose retrospective point forecast performance ",
                     # "is best according to the procedure outlined below."
                     )
  doc = addParagraph(doc, paragraph, stylename = "Normal")
    
  doc = addParagraph(doc, paste(" "), stylename="Normal")  
                                           
  texts <- table.rank.rmse.results.no.age$all.models 
  
  for (k in 1:length(texts)){
  
       if (texts[k]=="naiveone") {
           texts[k] <- "Naive model (previous year);"
       } else if (texts[k]=="avgthree") {
           texts[k] <- "Naive model (average of previous 3 years);"
       } else if (texts[k]=="avgfive") {
           texts[k] <- "Naive model (average of previous 5 years);"
       } else if (texts[k]=="arima") {
           texts[k] <- "ARIMA model;"
       } else if (texts[k]=="expsmooth") {
           texts[k] <-  "Exponential smoothing model;"
       }
  }
  
  texts[length(texts)] <- str_replace_all(texts[length(texts)], ";", ".")
  
  doc = addParagraph(doc, texts, stylename = "BulletList")                                         
  
  
  rm(texts)
                                           
  paragraph <- paste0("The model with the best forecast performance ",
                    "is determined as follows:")
  doc = addParagraph(doc, paragraph, stylename = "Normal")
  
  doc = addParagraph(doc, paste(" "), stylename="Normal")
  
  texts <- c("The values of the chosen retrospective point forecast performance measures (i.e., MRE, MAE, MPE, MAPE, MASE and RMSE) are computed for each model;",
             paste0("For every performance measure, the values of that measure are ordered from smallest to largest in absolute value across all models and the models are then ranked based on the magnitude of these absolute values ",
            "(i.e., the model with the smallest absolute value for that performance measure receives the smallest rank);"),
            "For each model, the ranks computed in the previous step are averaged across all retrospective point forecast performance measures to obtain a model-specific average rank; ",
           "The model with the smallest average rank among all models is deemed to be the best-performing model.", 
           paste0("If two or more models produce the same smallest average rank, they are further ranked with respect to the length of the interval forecasts they produce", 
                   " (i.e., the model which produces the shortest interval forecast is deemed to be best).")
           )
  doc = addParagraph(doc, texts, stylename = "BulletList")


  paragraph <- "  "
  doc =  addParagraph(doc, paragraph, stylename = "Normal")

  paragraph <- paste0("The first of the next two tables displays the individual and average ranks corresponding to the models considered for forecasting ",
                    tolower(stockabundance)," along with the values of the retrospective point forecast performance measures. "
                    ## , "The rows of the table corresponding to the best-performing model ",
                    ## "are highlighted in yellow colour."
                    )
  doc =  addParagraph(doc, paragraph, stylename = "Normal")

  doc = addParagraph(doc, paste(" "), stylename="Normal")

  paragraph <- paste0("The second of the next two tables displays the average ranks as well as the point forecasts, the interval forecasts and the lengths of the interval forecasts. ",
                      "The row of the table corresponding to the best model ",
                      "is highlighted in yellow colour."
                    ## , "The rows of the table corresponding to the best-performing model ",
                    ## "are highlighted in yellow colour."
                    )
  doc =  addParagraph(doc, paragraph, stylename = "Normal")

  doc = addParagraph(doc, paste(" "), stylename="Normal")

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

  ## f_tt

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


}


## doc= addPageBreak(doc)

doc = addSection(doc, ncol=1, columns.only=TRUE)

## More than one model -- show average rank in relation to width of prediction intervals  
if (length(pred.args)>1){

  
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
    str(tt_best)
    
    
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


####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
###
###
### Come back here (i.e., to code above)! 
###
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################


## pred.args[[k]]$PI.lwr
## pred.args[[k]]$PI.upr
## pred.args[[k]]$PI.ctr

PI <- NULL
PI.desc <- NULL
for (k in 1:length(pred.args)) {
    PI$PI.ctr <- c(PI$PI.ctr, round(pred.args[[k]]$PI.ctr))
    PI$PI.lwr <- c(PI$PI.lwr, round(pred.args[[k]]$PI.lwr))
    PI$PI.upr <- c(PI$PI.upr, round(pred.args[[k]]$PI.upr))

    PI.desc <- c(PI.desc,  class(pred.args[[k]]))
}

usePackage("stringr")
PI.desc <- str_replace(PI.desc, "naiveone", "Naive (1 yr)") 
PI.desc <- str_replace(PI.desc, "avgthree", "Naive (3 yrs)")
PI.desc <- str_replace(PI.desc, "avgfive", "Naive (5 yrs)")  
PI.desc <- str_replace(PI.desc, "arima", "ARIMA")  
PI.desc <- str_replace(PI.desc, "expsmooth", "Exponential Smoothing")


PI <- as.data.frame(PI)

PI <- cbind.data.frame(PI.desc, PI, AvgRank = table.rank.rmse.results.no.age$avg.rank)

usePackage("dplyr")

PI <- arrange(PI, AvgRank)

PI$PI.desc <- factor(PI$PI.desc, levels=PI$PI.desc)


if (length(pred.args) > 1) {

  doc = addSection(doc, landscape=TRUE)

	figurecaption <- paste0("Visual display of point forecasts and 80% forecast intervals of ", tolower(stockabundance), " ",
                      "corresponding to the ", 
                      stockname, " ", 
                      stockspecies, " stock ", 
                      "and the ", forecastingyear, " forecasting year. ", 
                      "The numbers listed in red at the top of the display represent the model-specific average ranks. ", 
                      "Models were ordered based on their average rank, from the model with the lowest average rank (\"best model\") ", 
                      "to the one with the highest average rank (\"worst model\").")


	myplot <- ggplot(PI, aes(x=PI.desc, y=PI.ctr)) + 
 		     geom_errorbar(aes(ymin=PI.lwr, 
                   ymax=PI.upr), width=0.1, 
                 colour="black") + 
  		     geom_point() + 
                 annotate("text", x = PI$PI.desc, y = PI$PI.ctr, label = comma(round(PI$PI.ctr)), hjust = -0.3, size=3) + 
                 annotate("text", x = PI$PI.desc, y = PI$PI.lwr, label = comma(round(PI$PI.lwr)), hjust = -0.3, size=3) + 
                 annotate("text", x = PI$PI.desc, y = PI$PI.upr, label = comma(round(PI$PI.upr)), hjust = -0.3, size=3) + 
                 annotate("text", x= PI$PI.desc, y = rep(1.05*max(PI$PI.upr), length(PI$PI.upr)), 
                          label=paste0(PI$AvgRank), col="red", size=3) + 
                 scale_y_continuous(paste0("Point Forecast and 80% Forecast Interval of ", stockabundance), labels = scales::comma) + 
                 xlab("Model") + 
                 theme_bw() +
                 theme(axis.title.x=element_text(size=10,vjust=-0.5),
                axis.title.y=element_text(size=10,vjust=1.5),
                axis.text.x=element_text(size=8),
                axis.text.y=element_text(size=8),
                strip.text.x = element_text(size = 8, colour = "black", angle = 0))
                
         
                 
                 
    doc = addPlot(doc, 
        fun = plot, # print,
        x = myplot,
        width=plotheight+2, height=plotwidth-2)

  doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

  rm(myplot)

}




#================================================================================================
# Best Model Results -- Only Report These Results if Working with More than One Forecasting Model
#================================================================================================


if (length(pred.args) > 1) {  

doc = addSection(doc, landscape=FALSE)

## doc = addPageBreak(doc)

doc = addTitle(doc, "Forecasting Results Produced by the Best Model", level=1)

doc = addParagraph(doc, paste(" "), stylename="Normal")

best.forecasting.model <- table.rank.rmse.results.no.age$best.model

if (best.forecasting.model=="naiveone") {
   best.forecasting.model <- "naive model (previous year)" 
} else if (best.forecasting.model=="avgthree") {
   best.forecasting.model <- "naive model (average of previous 3 years)" 
} else if (best.forecasting.model=="avgfive") {
   best.forecasting.model <- "naive model (average of previous 5 years)" 
} else if (best.forecasting.model=="arima") {
   best.forecasting.model <- "ARIMA model" 
} else if (best.forecasting.model=="expsmooth") {
   best.forecasting.model <- "exponential smoothing model" 
}

paragraph <- paste0("The best model for forecasting the ", forecastingyear, " ", 
                    tolower(stockabundance), " is the ", 
                    best.forecasting.model, ".")

doc = addParagraph(doc, value=paragraph, stylename = "Normal")

doc = addParagraph(doc, paste(" "), stylename="Normal")

##
## Point Forecast Results
##


doc = addTitle(doc, "Point Forecast Results", level=2)

doc = addParagraph(doc, paste(" "), stylename="Normal")

#--- Point Forecast Table ------------------------------------------------------------------------

if (table.rank.rmse.results.no.age$best.model=="naiveone") { #  Naive Model (Previous Year)

tablecaption <- paste0("Point forecast of the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the naive model (previous year).")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("Naive Model (Previous Year)", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.naiveone.no.age$PI.ctr))) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                     body.cell.props=cellProperties( padding = 4),
                     ## header.cell.props= cellProperties( padding = 4),
                    ## body.cell.props = baseCellProp, 
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}




if (table.rank.rmse.results.no.age$best.model=="avgthree") { #  Naive Model (Average of Previous 3 Years)

tablecaption <- paste0("Point forecast of the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the naive model (average of previous 3 years).")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("Naive Model (Average of Previous 3 Years)", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.avgthree.no.age$PI.ctr))) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    ## body.cell.props = baseCellProp, 
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}



if (table.rank.rmse.results.no.age$best.model=="avgfive") { #  Naive Model (Average of Previous 5 Years)

tablecaption <- paste0("Point forecast of the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the naive model (average of previous 5 years).")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("Naive Model (Acerage of Previous 5 Years)", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.avgfive.no.age$PI.ctr))) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    ## body.cell.props = baseCellProp,
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4), 
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}




if (table.rank.rmse.results.no.age$best.model=="arima") { #  ARIMA Model 

tablecaption <- paste0("Point forecast of the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the ARIMA model.")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("ARIMA Model", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.arima.no.age$PI.ctr))) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    ## body.cell.props = baseCellProp, 
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}





if (table.rank.rmse.results.no.age$best.model=="expsmooth") { #  Exponential Smoothing Model 

tablecaption <- paste0("Point forecast of the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the exponential smoothing model.")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("Exponential Smoothing Model", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.expsmooth.no.age$PI.ctr))) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    ## body.cell.props = baseCellProp, 
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}






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


##
## Interval Forecast Results 
##

doc = addPageBreak(doc)

doc = addTitle(doc, "Interval Forecast Results", level=2)

doc = addParagraph(doc, paste(" "), stylename="Normal")

#--- Interval Forecast Table ------------------------------------------------------------------------


if (table.rank.rmse.results.no.age$best.model=="naiveone") { #  Naive Model (Previous Year)

tablecaption <- paste0("Point and interval forecasts for the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the naive model (previous year).")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("Naive Model (Previous Year)", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.naiveone.no.age$PI.ctr)),
                          paste0(comma(round(pred.int.individual.stock.naiveone.no.age$PI.lwr)), 
                                " - ", 
                                comma(round(pred.int.individual.stock.naiveone.no.age$PI.upr)))
                           ) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    # body.cell.props = baseCellProp, 
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}




if (table.rank.rmse.results.no.age$best.model=="avgthree") { #  Naive Model (Average of Previous 3 Years)


tablecaption <- paste0("Point and interval forecasts for the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the naive model (average of previous 3 years).")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("Naive Model (Average of Previous 3 Years)", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.avgthree.no.age$PI.ctr)),
                          paste0(comma(round(pred.int.individual.stock.avgthree.no.age$PI.lwr)), 
                                " - ", 
                                comma(round(pred.int.individual.stock.avgthree.no.age$PI.upr)))
                           ) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    # body.cell.props = baseCellProp, 
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}




if (table.rank.rmse.results.no.age$best.model=="avgfive") { #  Naive Model (Average of Previous 5 Years)


tablecaption <- paste0("Point and interval forecasts for the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the naive model (average of previous 5 years).")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("Naive Model (Average of Previous 5 Years)", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.avgfive.no.age$PI.ctr)),
                          paste0(comma(round(pred.int.individual.stock.avgfive.no.age$PI.lwr)), 
                                " - ", 
                                comma(round(pred.int.individual.stock.avgfive.no.age$PI.upr)))
                           ) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    # body.cell.props = baseCellProp, 
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}




if (table.rank.rmse.results.no.age$best.model=="arima") { # ARIMA Model


tablecaption <- paste0("Point and interval forecasts for the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the ARIMA model.")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("ARIMA Model", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.arima.no.age$PI.ctr)),
                          paste0(comma(round(pred.int.individual.stock.arima.no.age$PI.lwr)), 
                                " - ", 
                                comma(round(pred.int.individual.stock.arima.no.age$PI.upr)))
                           ) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    # body.cell.props = baseCellProp, 
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}




if (table.rank.rmse.results.no.age$best.model=="expsmooth") { # Exponential Smoothing Model


tablecaption <- paste0("Point and interval forecasts for the ",
                       forecastingyear, " ", 
                       tolower(stockabundance)," ",
                       "corresponding to the ",
                       stockname," ", 
                       stockspecies, " stock, ",   
                       "obtained on the basis of the exponential smoothing model.")
 
doc = addParagraph(doc, value=tablecaption, stylename="rTableLegend")
 
usePackage("scales")

tt_1 <- data.frame("Exponential Smoothing Model", 
                          forecastingyear,
                          comma(round(pred.int.individual.stock.expsmooth.no.age$PI.ctr)),
                          paste0(comma(round(pred.int.individual.stock.expsmooth.no.age$PI.lwr)), 
                                " - ", 
                                comma(round(pred.int.individual.stock.expsmooth.no.age$PI.upr)))
                           ) 
         
         
colnames(tt_1) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                       
tt_2 <- data.frame("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
       
colnames(tt_2) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
                           
tt_1 <- data.frame(lapply(tt_1, as.character), stringsAsFactors=FALSE)
             
tt_2 <- data.frame(lapply(tt_2, as.character), stringsAsFactors=FALSE)
                  
                           
tt <- rbind.data.frame(tt_2, 
                       tt_1)
                        
## tt


colnames(tt) <- c("Best Model",
                  "Forecasting Year", 
                  "Point Forecast", 
                  "Interval Forecast")
   
my_ft <- FlexTable( data = tt, 
                    header.columns = FALSE, 
                    # body.cell.props = baseCellProp, 
                     body.cell.props=cellProperties( padding = 4),
                       ## header.cell.props= cellProperties( padding = 4),
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
my_ft[, 2:ncol(tt)] = parProperties(text.align = "right")

my_ft[1, 2:ncol(tt)] = textProperties(font.weight = "bold")

my_ft[,"Best Model"] = parProperties(text.align = "left")


my_ft[1,"Best Model"] = textProperties(font.weight = "bold")

doc = addFlexTable(doc, flextable=my_ft)

}


doc = addParagraph(doc, " ", style="Normal")



#--- Interval Forecast Visualization ------------------------------------------------------------------------

if (table.rank.rmse.results.no.age$best.model=="naiveone") {   # Naive Model (Previous Year)

    ##--- plot forecasted values & forecast intervals:  scatterplot (naiveone)


    plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ",
                     "corresponding to the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point and interval forecasts were obtained via naive forecasting (previous year).")

    myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.naiveone.no.age(pred.int.individual.stock.naiveone.no.age,
                                                                                                 forecastingyear)


    doc = addPlot(doc=doc,
                  fun=print,
                  x= myplot,
                  width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

    rm(myplot)

}


if (table.rank.rmse.results.no.age$best.model=="avgthree") {   # Naive Model (Average of Previous 3 Years)

    ##--- plot forecasted values & forecast intervals:  scatterplot (avgthree)

    plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ",
                     "corresponding to the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point and interval forecasts were obtained via naive forecasting (average of previous 3 years).")

    myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.avgthree.no.age(pred.int.individual.stock.avgthree.no.age,
                                                                                                 forecastingyear)


    doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


    rm(myplot)

}



if (table.rank.rmse.results.no.age$best.model=="avgfive") {   # Naive Model (Average of Previous 5 Years)

    ##--- plot forecasted values & forecast intervals:  scatterplot (avgfive)

    plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ",
                     "corresponding to the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point and interval forecasts were obtained via naive forecasting (average of previous 5 years).")

    myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.avgfive.no.age(pred.int.individual.stock.avgfive.no.age,
                                                                                                 forecastingyear)

    doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

    rm(myplot)

}


if (table.rank.rmse.results.no.age$best.model=="arima") {   # ARIMA Model

    ##--- plot forecasted values & forecast intervals:  scatterplot (arima)

    plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ",
                     "corresponding to the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point and interval forecasts were obtained via ARIMA forecasting.")

    myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.arima.no.age(pred.int.individual.stock.arima.no.age,
                                                                                                 forecastingyear)


    doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")


    rm(myplot)

}

if (table.rank.rmse.results.no.age$best.model=="expsmooth") {   # Exponential Smoothing Model

    ##--- plot forecasted values & forecast intervals:  scatterplot (expsmooth)


    plotlegend <- paste0("Historical ",
                     tolower(stockabundance)," values along with the ",
                     forecastingyear, " ",
                     "point forecast and 80% interval forecast of the ",
                     tolower(stockabundance), " ",
                     "corresponding to the ",
                         stockname, " ",
                         stockspecies, " ",
                         "stock. ",
                         " The point and interval forecasts were obtained via exponential smoothing forecasting.")

    myplot <- scatterplot.forecasted.values.and.forecast.intervals.individual.stock.expsmooth.no.age(pred.int.individual.stock.expsmooth.no.age,
                                                                                                 forecastingyear)
    doc = addPlot(doc=doc,
            fun=print,
            x= myplot,
            width=plotwidth, height=plotheight-3)

    doc = addParagraph(doc, value=plotlegend, stylename="rPlotLegend")

    rm(myplot)

}






#==================================================================================================

## doc = addPageBreak(doc)


##
## Histogram of Bootstrap Predictions: Best Model for Stock with No Age Information
##

best.index.no.age <- table.rank.rmse.results.no.age$index.min.avg.rank
best.model.no.age <- table.rank.rmse.results.no.age$best.model

if (best.model.no.age=="naiveone") {
    best.model.no.age <- "Naive Model (Previous Year)"
} else if (best.model.no.age=="avgthree") {
    best.model.no.age <- "Naive Model (Average of Previous 3 Years)"
} else if (best.model.no.age=="avgfive") {
    best.model.no.age <- "Naive Model (Average of Previous 5 Years)"
} else if (best.model.no.age=="arima") {
    best.model.no.age <- "ARIMA Model" 
} else if (best.model.no.age=="expsmooth") {
    best.model.no.age <- "Exponential Smoothing Model" 
}

best.model.no.age <- tolower(best.model.no.age)

usePackage("scales")
figurecaption <- paste0("Histogram of the B = ", 
                        scales::comma(B), 
                        " bootstrapped point forecasts for the ", 
                        forecastingyear, " ", 
                        tolower(stockabundance), " ", 
                        "corresponding to the ", 
                        stockname, " ", 
                        stockspecies, " stock. ",
                        "The bootstrapped point forecasts were produced by the ",
                        best.model.no.age, ". ",  
                        "The dashed red line indicates the position on the horizontal axis of the ", 
                        "point forecast of ",
                          tolower(stockabundance), ". ",
                        "The blue segment indicates the 80% forecast interval of ", 
                         tolower(stockabundance), ".")


myplot <- plot.yboot.best.model.no.age(pred.args, best.index.no.age, stockabundance)

doc = addPlot(doc, 
        fun = print,
        x = myplot,
        width=plotwidth, height=plotheight-3)

doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

rm(myplot)

##
## Empirical Probabilities: Best Model (Distribution of Bootstrapped Point Forecasts)
##

## doc = addPageBreak(doc)

#====================================================================================================================
#
# Empirical Probabilities for Stock With No Age Information: Best Model
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts
###


emp.prob.best.model.no.age <- empirical.probability.yboot.best.model.no.age(pred.args, best.index.no.age, stockabundance)


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

from_tmp = which(tt_arrange[,1] == emp.prob.best.model.no.age$prob.point.forecast$prob.threshold)
tt_arrange[from_tmp, 4] <- tt_arrange[from_tmp + 1, 4]

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

tt_arrange[,1] <- comma(tt_arrange[,1])
tt_arrange[,2] <- paste0(sprintf("%.2f", tt_arrange[,2]),"%")
tt_arrange[,3] <- paste0(sprintf("%.2f",tt_arrange[,3]),"%")
tt_arrange[,4] <- paste0(sprintf("%.2f",tt_arrange[,4]),"%")


names(tt_arrange)[1] <- "Threshold"
names(tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(tt_arrange)[4] <- "Interval Probability"
tt_arrange[1,4] <- "-"


my_ft <- FlexTable( data = tt_arrange,
                     body.cell.props=cellProperties( padding = 2),
                    # body.cell.props = baseCellProp,
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
tm(tt_2)

doc = addPageBreak(doc)



##-- Time series plot of actual and retrospectively forecasted values versus return year: Best Model (Stock with No Age Information)

figurecaption <- paste0("Time series plot of retrospectively forecasted and actual ", 
                        tolower(stockabundance), " values for the ", 
                        stockname, " ", 
                        stockspecies, " stock, ",  
                        "corresponding to the ", 
                        tolower(list_of_models_no_age[best.index.no.age]), ".")
                      
myplot <- timeseries.plot.results.afe.individual.stock.retro.best.model.no.age(retro.args,
                                                                               best.index.no.age, 
                                                                               stockabundance)

doc = addPlot(doc, 
        fun = print,
        x = myplot,
        width=plotwidth, height=plotheight-1)

doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

rm(myplot)



##-- Plot actual versus retrospectively forecasted values: Best Model (Stock with No Age Information)

figurecaption <- paste0("Scatter plot of retrospectively forecasted versus actual ", 
                        tolower(stockabundance), " values for the ", 
                        stockname, " ", 
                        stockspecies, " stock, ",  
                        "corresponding to the ", 
                        tolower(list_of_models_no_age[best.index.no.age]), ".", 
                        " Observations are labeled according to the associated historical return years.")

myplot <- plot.results.afe.individual.stock.retro.best.model.no.age(retro.args, best.index.no.age)

doc = addPlot(doc, 
        fun = print,
        x = myplot,
        width=plotwidth, height=plotheight-3)

doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

rm(myplot)


##-- Bias Coefficient Plot: Best Model (Stock with No Age Information)

doc = addPageBreak(doc)

figurecaption <- paste0("Bias coefficient plot ", 
                        "obtained from the retrospective forecast errors ",   
                        "corresponding to the ", 
                        tolower(list_of_models_no_age[best.index.no.age]), ".")

myplot <- plot.bias.coefficients.retrospective.forecast.errors.individual.stock.best.model.no.age (
            rmse.results.no.age.args,  
            retro.args, 
            best.index.no.age)

doc = addPlot(doc, 
        fun = grid.draw,
        x = myplot,
        width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")

rm(myplot)



} ## end reporting of result for best forecasting model (when working with more than just one forecasting model)


## clean up redundant files 

fn <- "arimafit.txt"
    if (file.exists(fn)) file.remove(fn)
    
fn <- "expsmoothfit.txt"
    if (file.exists(fn)) file.remove(fn)


##================================================================================================
## Produce report
##================================================================================================

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

## reportname <- paste0("Full Report ",stockname," ",stockabundance," ",forecastingyear,".docx")

## writeDoc(doc, reportname)

#### usePackage("PBSmodelling")

#### local(envir=.PBSmodEnv,expr={
####          # use openFile directly:
####          openFile("Report.docx")})

## browseURL(reportname)


#========================================================================================================
# CoverPage
#========================================================================================================

ARIMA$fits <- ARIMA$arima.model.fits

usePackage("stringr")
ARIMA$stockabundance <- str_replace(ARIMA$stockabundance,"_"," ") 


ARIMA$pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 40) )
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot1)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )

ARIMA$pot1 <- NULL 
ARIMA$my.pars <- NULL 

ARIMA$pot1 = pot(" ", textProperties(font.weight="bold", font.size = 20) )
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot1)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )

ARIMA$pot1 <- NULL 
ARIMA$my.pars <- NULL 

ARIMA$pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(ARIMA$stockname), textProperties(font.size = 20) )
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot2)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )

ARIMA$pot2 <- NULL 
ARIMA$my.pars <- NULL 

ARIMA$pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(ARIMA$stockspecies), textProperties(font.size = 20) )
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot3)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )

ARIMA$pot3 <- NULL 
ARIMA$my.pars <- NULL 

ARIMA$pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(ARIMA$stockabundance), textProperties(font.size = 20) )
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot4)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )

ARIMA$pot4 <- NULL 
ARIMA$my.pars <- NULL 

ARIMA$pot5 =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(ARIMA$forecastingyear), textProperties(font.size = 20) )
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot5)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )

ARIMA$pot5 <- NULL 
ARIMA$my.pars <- NULL 

ARIMA$pot6 =  pot("Forecasting Model: ", textProperties(font.weight="bold", font.size = 20)) 
ARIMA$pot9 = pot(paste("ARIMA Model"), textProperties(font.size = 20) ) 
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot6, ARIMA$pot9)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )
          
ARIMA$pot6 <- NULL 
ARIMA$pot9 <- NULL 
ARIMA$my.pars <- NULL 
               
ARIMA$pot6a =  pot("Time Series Bootstrapping Method: ", textProperties(font.weight="bold", font.size = 20)) 
ARIMA$pot9a = pot(paste("Maximum Entropy Bootstrap"), textProperties(font.size = 20) ) 
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot6a, ARIMA$pot9a)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )
               
ARIMA$pot6a <- NULL
ARIMA$pot9a <- NULL  
ARIMA$my.pars <- NULL 
               
ARIMA$pot13 =  pot("Date: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(Sys.Date()), textProperties(font.size = 20) )
ARIMA$my.pars = set_of_paragraphs(ARIMA$pot13)
doc = addParagraph( doc, value = ARIMA$my.pars, stylename="Normal" )
     
ARIMA$pot13 <- NULL 
ARIMA$my.pars <- NULL 

doc = addPageBreak(doc)


#=================================================================================================
# Summary of Results
#=================================================================================================

doc = addTitle(doc, "Summary of Results", level=1)

## point forecasts + individual ages
ARIMA$results.point.forecast.arima


ARIMA$tabledata <- matrix(NA, nrow=11, ncol=length(ARIMA$fits)+2)

## tabledata <- matrix(NA, nrow=10, ncol=length(fits)+2)

colnames(ARIMA$tabledata) <- rep("Name", length(ARIMA$fits)+2)
colnames(ARIMA$tabledata)[1] <- "Item"
colnames(ARIMA$tabledata)[2:(1+length(ARIMA$fits))] <- as.character(ARIMA$results.point.forecast.arima$Age)
colnames(ARIMA$tabledata)[2+length(ARIMA$fits)] <- "Total"

ARIMA$tabledata[1,] <- c("Return Year", rep(unique(ARIMA$results.point.forecast.arima$RY),
                   length(ARIMA$fits)+1))

ARIMA$model_desc <- NULL
for (i in 1:length(ARIMA$fits)){
    ARIMA$arimafit <- ARIMA$fits[[i]]$model

    sink("arimafit.txt")
    print(ARIMA$arimafit)
    sink()

    ARIMA$out <- readLines("arimafit.txt")

    require(stringr)

    ARIMA$out.pattern <- str_detect(string=ARIMA$out, pattern="ARIMA")

    ARIMA$modelarima <- ARIMA$out[ARIMA$out.pattern==TRUE]
    usePackage("stringr")
    ARIMA$modelarima <- str_trim(ARIMA$modelarima)

    if (ARIMA$boxcoxtransform==TRUE){
       ARIMA$out.lambda <- str_detect(string=ARIMA$out, pattern="lambda")

       ARIMA$modellambda <- ARIMA$out[ARIMA$out.lambda==TRUE]
       usePackage("stringr")
       ARIMA$modellambda <- str_trim(ARIMA$modellambda, side="right")
       ARIMA$model_desc <- c(ARIMA$model_desc, paste0(ARIMA$modelarima, "; ", ARIMA$modellambda))
    } else {
       ARIMA$model_desc <- c(ARIMA$model_desc, paste0(ARIMA$modelarima)) 
    }

}

ARIMA$tabledata[2,] <- c("Model", ARIMA$model_desc, "-")

ARIMA$tabledata[3,] <- c("Point Forecast",
                   c(as.character(comma(round(ARIMA$results.point.forecast.arima$p))),
                     as.character(comma(sum(round(ARIMA$results.point.forecast.arima$p))))))     ## Why does this report point forecasts again?


ARIMA$PI.individual.ages.arima
ARIMA$PI.total.age.arima

ARIMA$PI.combined <- rbind(ARIMA$PI.individual.ages.arima, ARIMA$PI.total.age.arima)

ARIMA$PI.combined.vec <- NULL
for (i in 1:nrow(ARIMA$PI.combined)){

   ARIMA$tmp.vec <- paste0(ARIMA$PI.combined[i,"PI.lwr"]," - ",ARIMA$PI.combined[i,"PI.upr"])

   ARIMA$PI.combined.vec <- c(ARIMA$PI.combined.vec, ARIMA$tmp.vec)

}


ARIMA$tabledata[4,] <- c("Interval Forecast", ARIMA$PI.combined.vec)

## tabledata <- M.arima
## tabledata <- subset(M.arima, select=-Model)

ARIMA$M.sub.arima <- subset(ARIMA$M.arima, select=-Model)

ARIMA$tabledata[5:(5 + nrow(ARIMA$M.sub.arima) - 1),"Item"] <- as.character(ARIMA$M.sub.arima$Measure)

for (k in 2:ncol(ARIMA$M.sub.arima)){
    ARIMA$tabledata[5:(5 + nrow(ARIMA$M.sub.arima) - 1),k] <- ARIMA$M.sub.arima[,k]
}



ARIMA$fits <-  ARIMA$arima.model.fits
ARIMA$results <- ARIMA$results.individual.ages.retro.predictive.performance.arima
ARIMA$results.total.age <- ARIMA$results.total.age.retro.predictive.performance.arima

# r.squared.table <- r.squared.values.arima(fits, results, results.total.age)

# tabledata[5 + nrow(M.sub.arima),"Item"] <- "R-squared"

# tabledata[5 + nrow(M.sub.arima),-1] <- as.character(r.squared.table$R.squared)

ARIMA$bias.coeff.table <- c(round(ARIMA$bias.coeff.afe.individual.ages.retro.arima,4), 
                      round(ARIMA$bias.coeff.afe.total.age.retro.arima,4))

ARIMA$tabledata[5 + nrow(ARIMA$M.sub.arima),"Item"] <- "Bias Coefficient"

ARIMA$tabledata[5 + nrow(ARIMA$M.sub.arima),-1] <-  ARIMA$bias.coeff.table

ARIMA$tabledata <- as.data.frame(ARIMA$tabledata)

ARIMA$tablecaption <- paste("Summary of forecasting results for the",
                      ARIMA$forecastingyear,
                      "age-specific and total",
                      # terminal run
                      paste(tolower(ARIMA$stockabundance),"s",collapse="", sep=""),
                      " associated with the ",
                      ARIMA$stockname, " ",
                      tolower(ARIMA$stockspecies), " stock.")

ARIMA$tabledata <- ARIMA$tabledata

doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")
    
ARIMA$MyFTable = FlexTable(data=ARIMA$tabledata, 
                     header.columns = TRUE,
                     add.rownames = FALSE,
                     header.cell.props = cellProperties(padding=7),
                     body.cell.props = cellProperties(padding=7),
                     body.text.props = textProperties(font.weight = "normal",
                                                      font.size = 10,
                                                      font.family = "Calibri"),
                     header.text.props = textProperties(font.weight = "normal",
                                                        font.size = 10,
                                                        font.family = "Calibri"),
                     body.par.props = parProperties(text.align = "right"), 
                     header.par.props = parProperties(text.align = "right")
)

# applies a border grid on table
ARIMA$MyFTable <- setFlexTableBorders(ARIMA$MyFTable, 
                                inner.vertical = borderProperties(style = "none"), 
                                inner.horizontal = borderProperties(style = "none"), 
                                outer.vertical = borderProperties(style = "none"), 
                                outer.horizontal = borderProperties(color = "gray5", style = "solid")
)


doc = addFlexTable(doc, ARIMA$MyFTable)

## rm(tablecaption)

ARIMA$tablecaption <- NULL 
ARIMA$MyFTable <- NULL 

#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

ARIMA$empirical.probability.yboot.arima.total.age <- function(PI.total.age.arima.sim, PI.total.age.arima.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.arima.sim
     mylabel <- paste("Total", stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.arima.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


ARIMA$emp.prob.arima.total.age <- ARIMA$empirical.probability.yboot.arima.total.age(ARIMA$PI.total.age.arima.sim, 
                                                                              ARIMA$PI.total.age.arima.no.comma, 
                                                                              ARIMA$stockabundance)



ARIMA$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(ARIMA$stockabundance)," ",
                       "value yet to be observed in ",
                       ARIMA$forecastingyear, " for the ",
                       ARIMA$stockname," ",
                       ARIMA$stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(ARIMA$stockabundance), ".", 
                       " Maximum entropy bootstrapping was used as a basis for obtaining this distribution.")

doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")

ARIMA$tt_1 <- ARIMA$emp.prob.arima.total.age$prob.thresholds

ARIMA$tt_2 <- ARIMA$emp.prob.arima.total.age$prob.point.forecast

ARIMA$tt_1_and_2 <- rbind.data.frame(ARIMA$tt_1, ARIMA$tt_2)

usePackage("plyr")

## ARIMA$tt_arrange <- arrange(ARIMA$tt_1_and_2, ARIMA$prob.threshold)

ARIMA$tt_arrange <- ARIMA$tt_1_and_2[order(ARIMA$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties(padding = 2)

ARIMA$from_tmp = which(ARIMA$tt_arrange[,1] == ARIMA$emp.prob.arima.total.age$prob.point.forecast$prob.threshold)
ARIMA$tt_arrange[ARIMA$from_tmp, 4] <- ARIMA$tt_arrange[ARIMA$from_tmp + 1, 4]



usePackage("scales")
ARIMA$tt_arrange[,1] <- comma(ARIMA$tt_arrange[,1])
ARIMA$tt_arrange[,2] <- paste0(ARIMA$tt_arrange[,2],"%")
ARIMA$tt_arrange[,3] <- paste0(ARIMA$tt_arrange[,3],"%")
ARIMA$tt_arrange[,4] <- paste0(ARIMA$tt_arrange[,4],"%")

names(ARIMA$tt_arrange)[1] <- "Threshold"
names(ARIMA$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(ARIMA$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(ARIMA$tt_arrange)[4] <- "Interval Probability"
ARIMA$tt_arrange[1,4] <- "-"

ARIMA$my_ft <- FlexTable( data = ARIMA$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
ARIMA$my_ft[, 1:ncol(ARIMA$tt_arrange)] = parProperties(text.align = "right")


ARIMA$my_ft = spanFlexTableRows(ARIMA$my_ft, j=4, from = ARIMA$from_tmp, to = ARIMA$from_tmp + 1)

ARIMA$my_ft[ARIMA$tt_arrange$Threshold %in% comma(ARIMA$emp.prob.arima.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)


## ARIMA$my_ft[ARIMA$tt_arrange$Threshold %in% comma(ARIMA$emp.prob.arima.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )


doc = addFlexTable(doc, flextable=ARIMA$my_ft)

ARIMA$my_ft_emp_prob_total <- ARIMA$my_ft
ARIMA$tablecaption_emp_prob_total <- ARIMA$tablecaption

ARIMA$my_ft <- NULL 
ARIMA$tablecaption <- NULL 
ARIMA$tt_arrange <- NULL 
ARIMA$tt_1_and_2 <- NULL 
ARIMA$tt_1 <- NULL 
ARIMA$tt_2 <- NULL 

doc = addPageBreak(doc)



#---- Introduction -------------------------------------------------------------------------


doc = addTitle(doc, "Introduction", level=1)


ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 


ARIMA$paragraph <- paste("In this report, ARIMA modeling is used to forecast the age-specific and total", 
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=""),
                      "for the",
                      ARIMA$stockname, ARIMA$stockspecies, "stock.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste0("ARIMA models are a general class of models for forecasting a univariate time series ",  
                    "which can be stationarized by transformations such as differencing and logging. ", 
                    "The acronym ARIMA stands for \"Auto-Regressive Integrated Moving Average.\"")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("ARIMA models are represented using the notation ARIMA(p,d,q), where p is the number of autoregressive terms,", 
                       "d is the number of (nonseasonal) differences, and q is the number of lagged forecast errors in the forecasting equation.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL

ARIMA$paragraph <- paste("Forecasting a univariate time series on the basis of ARIMA modeling generally involves four steps:")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMAparagraph <- c("   1) Transformation: A Box-Cox transformation is applied to the time series to suppress large fluctuations in its values;",
               "   2) Identification: Differencing of order d is applied to make the transformed time series stationary;", 
               "   3) Estimation: An ARMA(p,q) model is fitted to the differenced, transformed time series;",
               "   4) Forecasting:  The ARIMA(p,d,q) model is used to forecast future values of the differenced, transformed time series and the forecasts are back transformed so they are expressed on the same scale as that of the original time series.")
doc = addParagraph(doc, ARIMA$paragraph, parent.type="ol", stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("We rely on the function BoxCox.lambda() from the R package \"forecast\" to implement Step 1).",
"By default, this function uses Guerrero's (1993) method to find the optimal value of lambda to be used when applying",
"the Box-Cox transformation to the univariate time series.",
"Furthermore, we rely on the auto.arima() function from the same package to implement Steps 2) - 4) described above.",
                       "The auto.arima() function uses automation to identify the orders p,d and q of the",
                       "ARIMA models fitted to historical", 
                       tolower(ARIMA$stockabundance),  
                       "(Box-Cox transformed) data series specific to each age class.",
                       "Specifically, the auto.arima() function finds the order of differencing d first and then identifies the orders p and q via AIC maximization.",
                       "The function then proceeds to estimate the unknown parameters describing the ARIMA(p,d,q) models and to produce point forecasts.", sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

#-----------------------------------------------------------------------------------------------------
######################################################################################################
#
#  Section:  Data
#
######################################################################################################
#-----------------------------------------------------------------------------------------------------


doc = addTitle(doc, "Data", level=1)

ARIMA$tablecaption <- paste("Historical ",
                      # terminal run
                      tolower(ARIMA$stockabundance),
                      " data for the ",
                      ARIMA$stockname, " ",
                      tolower(ARIMA$stockspecies), " stock,",
                      "reported as a function of calendar year for specific ages of the stock.")

ARIMA$tabledata <- ARIMA$datafile
ARIMA$tabledata[ARIMA$tabledata<0] <- NA

ARIMA$datalist

ARIMA$datalist1 <- ARIMA$datalist[[1]]
ARIMA$datalist1 <- subset(ARIMA$datalist1,select=-BY)
for (i in 2:length(ARIMA$datalist)){
      ARIMA$datalist.tmp <- ARIMA$datalist[[i]]
      ARIMA$datalist.tmp <- subset(ARIMA$datalist.tmp,select=-BY)

      ARIMA$datalist1 <-  merge(ARIMA$datalist1, ARIMA$datalist.tmp, by=c("CY"))

}

ARIMA$datalist1

## tabledata$Sum <- apply(tabledata[,-1],1, sum, na.rm=TRUE)

ARIMA$tabledata <- ARIMA$datalist1

usePackage("stringr")
names(ARIMA$tabledata) <- str_replace_all(names(ARIMA$tabledata),"T","Age ")
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="CY"] <- "Calendar Year"


usePackage("scales")
ARIMA$tabledata[,-1] <- comma(ARIMA$tabledata[,-1] )


doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")
    
ARIMA$MyFTable = FlexTable(data=ARIMA$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
                                  
doc = addFlexTable(doc, ARIMA$MyFTable)

ARIMA$MyFTable <- NULL 

#---- Plot original data by return year (for specific ages) -----------------------------------------------

ARIMA$figurecaption <- paste("Plots of historical ",
                       # terminal runs,
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " versus return years for the ",
                       ARIMA$stockname," ",
                       tolower(ARIMA$stockspecies), " stock, ",
                       "with each plot corresponding to a specific age component of the stock.",sep="")

doc = addPlot(doc, 
        fun = print,
        x = ARIMA$plot.data.arima(ARIMA$datalist),
        width=6.5, height=6)

doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")


#-----------------------------------------------------------------------------------------------------
######################################################################################################
#
#  Section:  Modeling Results
#
######################################################################################################
#-----------------------------------------------------------------------------------------------------

#----- Plot Fitted Values Produced by ARIMA Modeling for Each Age Class ------------------------------------------------------------------------------

doc = addTitle(doc, "ARIMA Time Series Modeling Results", level=1)

ARIMA$figurecaption <- paste("Fitted values produced by the ARIMA models used to forecast ",
                       "specific age components of the ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       # " terminal run for the",
                       " ",
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       ARIMA$stockname, " ", tolower(ARIMA$stockspecies), " stock.",
                       sep="")


doc = addPlot(doc, 
        fun = print,
        x = ARIMA$plot.fitted.arima(ARIMA$arima.model.fits, ARIMA$boxcoxtransform),
        width=6.5, height=6)

doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$figurecaption <- NULL 

ARIMA$paragraph <- paste("Stats Tutorial:")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")


ARIMA$paragraph <- paste("For each age-specific",
                      # terminal run",
                      tolower(ARIMA$stockabundance),
                      "time series, compare the fitted values produced by the ARIMA model",
                      "against the observed (or historical) values of the",
                      #terminal run
                      tolower(ARIMA$stockabundance),
                      "time series.  Does the ARIMA model appear to provide",
                      "a good fit to the time series?",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

  
#----- ARIMA Time Series Modeling Diagnostics: Checking Randomness of Residuals ------------------------------------------------------------------------------

doc = addTitle(doc, "ARIMA Modeling Diagnostics", level=1)

for (i in 1:length(ARIMA$arima.model.fits)) {

     ARIMA$age <- ARIMA$arima.model.fits[[i]]$age
     ARIMA$age <- tolower(ARIMA$age)

     ARIMA$figurecaption <- paste("Diagnostic plots for the ARIMA model used to forecast the ",
                       ARIMA$age,
                       " component of the ",
                       # max(results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, " ",
                       # " terminal run ",
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       sep="")

    
     ARIMA$myplot <- ARIMA$diagnostics.arima.model.fit(ARIMA$arima.model.fits, ARIMA$boxcoxtransform, i)
     
     doc = addPlot(doc,
            fun=plot,  # print,
            x=ARIMA$myplot,
            width=plotwidth+1, height=plotheight)
            
     doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

     ## rm(myplot)
    
     ARIMA$myplot <- NULL 
     ARIMA$figurecaption <- NULL 

}



doc = addPageBreak(doc)

ARIMA$paragraph <- paste("Stats Tutorial:",sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
  
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("After fitting an ARIMA model to a univariate time series,",
                       "we need to run diagnostic tests to validate the model.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 
  
ARIMA$paragraph <- paste("If the ARIMA model provides a good fit to a univariate time series,", 
                       "the residuals associated with the model should exhibit",
                       "no systematic patterns and ",
                       "no temporal dependence.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
    
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("Useful diagnostic plots for verifying that the ARIMA model residuals exhibit no systematic patterns and no temporal dependence include:")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 


ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- c("   - Time series plot of the model residuals; ", 
               "   - Autocorrelation plot of the model residuals;",
               "   - Partial autocorrelation plot of the model residuals;",
               "   - Plot of p-values associated with the Ljung-Box test applied to the model residuals.")
doc = addParagraph(doc, ARIMA$paragraph, parent.type="ul", stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$pots <- pot("The Ljung-Box test is a diagnostic tool used to test the lack of fit of an ARIMA model. ") + 
        pot("The test is applied to the model residuals and examines the first") + 
        pot(" m ", textProperties(font.style = "italic", font.size=10, font.family="Calibri")) + 
        pot("autocorrelations of the residuals. ") +
        pot("If all of these autocorrelations are very small, we conclude that the model does not exhibit significant lack of fit. ") +
        pot("The Ljung-Box test tests the following hypotheses: ") +   
        pot("Ho: The model does not exhibit lack of fit", textProperties(font.style = "italic", font.size=10, font.family="Calibri")) +     
        pot(" versus ") +  
        pot("Ha: The model exhibits lack of fit. ", textProperties(font.style = "italic", font.size=10, font.family="Calibri"))+
        pot("Small p-values for the Ljung-Box test lead to the rejection of the alternative hypothesis, suggesting that the model ") +
        pot("exhibits significant lack of fit. ")+
        pot("Conversely, large p-values suggest that the model does not exhibit significant lack of fit. ") +
        pot("Since the choice of") + 
        pot(" m ", textProperties(font.style = "italic", font.size=10, font.family="Calibri")) +  
        pot("is important but somewhat arbitrary, in practice we perform the Ljung-Box test for several consecutive values of") + 
        pot(" m ", textProperties(font.style = "italic", font.size=10, font.family="Calibri")) + 
        pot("to see if the p-values it produces are large for all of these values. ") + 
        pot("If they are, then we conclude that the model does not exhibit lack of fit.")
             
ARIMA$paragraph <- set_of_paragraphs(ARIMA$pots)

doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 


ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("If an ARIMA model provides a good fit to a univariate time series, then:")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- pot("The time series plot of the model residuals should exhibit no systematic patterns;")
ARIMA$paragraph <- set_of_paragraphs(ARIMA$paragraph)
doc = addParagraph(doc, ARIMA$paragraph, stylename="BulletList")

ARIMA$paragraph <- NULL

ARIMA$paragraph <- pot("The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals;")
ARIMA$paragraph <- set_of_paragraphs(ARIMA$paragraph)
doc = addParagraph(doc, ARIMA$paragraph, stylename="BulletList")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- pot("The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;")
ARIMA$paragraph <- set_of_paragraphs(ARIMA$paragraph)
doc = addParagraph(doc, ARIMA$paragraph, stylename="BulletList")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <-  set_of_paragraphs(pot("The p-values associated with the Ljung-Box test should be large for all values of") +  
                                pot(" m ",  textProperties(font.style = "italic", font.size=10, font.family="Calibri")) + 
                                pot("considered."))             
doc = addParagraph(doc, ARIMA$paragraph, stylename="BulletList")

ARIMA$paragraph <- NULL 

#----- Point Forecast Results ------------------------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, "Forecasting Results", level=1)


ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 


ARIMA$paragraph <- paste("This section reports the ",
                        "forecasting results for the ",
                        ARIMA$stockspecies," ", ARIMA$stockname, " stock ",
                        "corresponding to the forecasting year ",ARIMA$forecastingyear,". ",
                        # "corresponding to the", ARIMA$stockspecies, ARIMA$stockname, "model stock.",
                        "The results were produced by ARIMA modeling.",
                      sep="")
   
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- pot("Forecasting results are reported numerically and visually for two types of forecasts:")
ARIMA$paragraph <- set_of_paragraphs(ARIMA$paragraph)
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL

ARIMA$paragraph <- c("   1) point forecasts;", "   2) interval forecasts.")
doc = addParagraph(doc, ARIMA$paragraph, parent.type="ol", stylename="Normal")

ARIMA$paragraph <- NULL 
                      
ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 
                      
ARIMA$paragraph <- pot("A point forecast is simply a number which represents our best guess") +
             pot("of the future value of the age-specific or total ") + 
             pot(paste(tolower(ARIMA$stockabundance))) + 
             pot(" for the stock of interest based on available historical data.")
ARIMA$paragraph <- set_of_paragraphs(ARIMA$paragraph)             
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- pot("An interval forecast is not a single number, rather it is a range of values ") + 
             pot("in which we expect the future value of an age-specific or total ") + 
             pot(paste(tolower(ARIMA$stockabundance))) + 
             pot("series to fall with some (prespecified) probability.")
ARIMA$paragraph <- set_of_paragraphs(ARIMA$paragraph)             
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- pot("A couple of remarks are in order in connection with an interval forecast:") 
ARIMA$paragraph <- set_of_paragraphs(ARIMA$paragraph)
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- c(paste("The width of the interval forecast conveys information regarding forecast uncertainty", 
                     "(the wider the interval forecast, the more uncertain the forecast);"),
               "The interval forecast conveys more information than the associated point forecast.")
doc = addParagraph(doc, ARIMA$paragraph, stylename="BulletList")
 
ARIMA$paragraph <- NULL 
 
ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 
 
 
ARIMA$paragraph <- paste("The interval forecast provided in this report for each abundance time series (age-specific or total)",
                   "was obtained by applying maximum entropy bootstrapping to that series.", 
                   sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("The maximum entropy bootstrapping is a time series bootstrapping method introduced by Vinod in 2004 and 2006.",
                   "The method constructs an ensemble of bootstrapped time series using a seven-step algorithm ",
                   "designed to satisfy the ergodic theorem (i.e., the grand mean of all ensembles is close to the sample mean).",
                   "The algorithm's practical appeal is that it avoids all structural change and unit root type testing involving complicated asymptotics ",
                   "and all shape-destroying transformations like detrending or differencing to achieve stationarity. ",
                   "The constructed ensemble elements retain the basic shape and time dependence structure of the ",
                   "autocorrelation function (ACF) and the partial autocorrelation function (PACF) of the original time series.", 
                   sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
   
ARIMA$paragraph <- NULL 


#--- Stats tutorial on bootstrap for ARIMA model --------------------------------------------------


#--- Point forecast results ------------------------------------------------------------------------


doc = addTitle(doc, "Point Forecasts", level=2)


ARIMA$tabledata <- ARIMA$results.point.forecast.arima    ## this object is created in the review code file

ARIMA$tabledata[nrow(ARIMA$tabledata)+1, ] <- ARIMA$tabledata[nrow(ARIMA$tabledata), ]

ARIMA$tabledata <- transform(ARIMA$tabledata, Age = as.character(Age))

str(ARIMA$tabledata)

ARIMA$tabledata[nrow(ARIMA$tabledata), "Age"] <-  "Total"
ARIMA$tabledata[nrow(ARIMA$tabledata), "Model"] <-  ""
ARIMA$tabledata <- transform(ARIMA$tabledata, p = round(p))
ARIMA$tabledata[nrow(ARIMA$tabledata), "p"] <-  sum(ARIMA$tabledata[1:(nrow(ARIMA$tabledata)-1), "p"])

usePackage("scales")
ARIMA$tabledata <- transform(ARIMA$tabledata, p = comma(p))


names(ARIMA$tabledata)[names(ARIMA$tabledata)=="Age"] <- "Terminal Run"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="Model"] <- "Model"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="RY"] <- "Forecasting Year"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="p"] <- "Point Forecast"

ARIMA$tabledata$Model


ARIMA$tablecaption <- paste("Point forecasts of the ",
                      ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                      ARIMA$forecastingyear, 
                      " age-specific and total ",
                      " ",
                      # terminal runs
                      paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                      " for the ",
                      ARIMA$stockname,
                       " ",
                       tolower(ARIMA$stockspecies),
                       " stock. ",
                       "The point forecasts for the age-specific ",
                       # terminal runs,
                       tolower(ARIMA$stockabundance),
                       " were produced by ARIMA models. ",
                       "The point forecast for the total ",
                       # terminal run ",
                       tolower(ARIMA$stockabundance),
                       " was obtained by totaling the age-specific point forecasts produced by these ARIMA models. ",
                       "All point forecasts were rounded to the nearest integer for reporting purposes.",
                       sep=""
                       )


doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")
    
ARIMA$MyFTable = FlexTable(data=ARIMA$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
    
ARIMA$MyFTable[,2,to = "body"] = parLeft()
ARIMA$MyFTable[,2,to = "header"] = parLeft()
    
doc = addFlexTable(doc, ARIMA$MyFTable)
          
ARIMA$MyFTable <- NULL           

#####################################################################################################################
### Visualize point forecasts - barplots (specific age components)
#####################################################################################################################

ARIMA$fits <- ARIMA$arima.model.fits
ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)

for (i in 1:length(ARIMA$arima.model.fits)) {

   
     ARIMA$age <- ARIMA$arima.model.fits[[i]]$age
     ARIMA$age <- tolower(ARIMA$age)

     doc = addPageBreak(doc)

     
     ARIMA$fits <- ARIMA$arima.model.fits
     ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)


     doc = addPlot(doc, 
        fun = plot,  # print,
        x = ARIMA$barplot.forecasted.values.individual.ages.arima(ARIMA$fits, ARIMA$boxcoxtransform, ARIMA$pointforecasts,i),
        width=6.5, height=6)

     ARIMA$age <- ARIMA$arima.model.fits[[i]]$age
     ARIMA$age <- tolower(ARIMA$age)

     ARIMA$figurecaption <- paste("Historical ",
                       #terminal run
                       tolower(ARIMA$stockabundance),
                       " values and ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       " point forecast ",
                       "corresponding to the ", ARIMA$age, " component of the ",
                       # "terminal run ,
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       " The ", ARIMA$forecastingyear ,  # max(results.afe.total.age.retro.arima$CY)+1,
                       " point forecast was derived from the ARIMA model.",
                       sep="")

    
     
     doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")
    
     ARIMA$figurecaption <- NULL 


}


#################################################################################################
### Visualize point forecasts - barplot (total age)
#################################################################################################


doc = addPageBreak(doc)

ARIMA$results <- ARIMA$results.total.age.retro.predictive.performance.arima

ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)



doc = addPlot(doc, 
              fun = plot,  # print,
              x = ARIMA$barplot.forecasted.values.total.age.arima(ARIMA$results, ARIMA$pointforecasts),
              width=6.5, height=6)

ARIMA$figurecaption <- paste("Historical total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " values",
                       # "(","ages ",min(ages)," - ",max(ages), ")",
                       " and corresponding ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       " point forecast ",
                       "for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       " The",
                       " point forecast of total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " was obtained by totaling the point forecasts of the age-specific ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " produced by the",
                       " ARIMA models.",
                       sep="")


doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$figurecaption <- NULL 

#---- Forecast Intervals ------------------------------------------------------------------------


doc = addPageBreak(doc)

doc = addTitle(doc, "Interval Forecasts", level=2)


ARIMA$tabledata <- rbind(ARIMA$PI.individual.ages.arima, ARIMA$PI.total.age.arima)

ARIMA$tabledata

## tabledata <- subset(tabledata, select=-PI.med)

ARIMA$tabledata$PI <- paste( ARIMA$tabledata[,"PI.lwr"]," - ", ARIMA$tabledata[,"PI.upr"], sep="")

ARIMA$tabledata <- subset(ARIMA$tabledata, select=-PI.lwr)
ARIMA$tabledata <- subset(ARIMA$tabledata, select=-PI.upr)

names(ARIMA$tabledata)[names(ARIMA$tabledata)=="PI.ctr"] <- "Point Forecast"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="PI"] <- "Interval Forecast"

ARIMA$nms <- c(as.character(ARIMA$results.point.forecast.arima$Age), "Total")
ARIMA$mds <- c(as.character(ARIMA$results.point.forecast.arima$Model), "")
ARIMA$yr <-  c(ARIMA$results.point.forecast.arima$RY, unique(ARIMA$results.point.forecast.arima$RY))

ARIMA$tabledata <- data.frame(nms=ARIMA$nms, mds=ARIMA$mds, yr=ARIMA$yr, ARIMA$tabledata)

names(ARIMA$tabledata)[names(ARIMA$tabledata)=="nms"] <- "Terminal Run"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="mds"] <- "Model"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="yr"] <- "Return Year"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="Point.Forecast"] <- "Point Forecast"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="Interval.Forecast"] <- "Interval Forecast"


ARIMA$tablecaption <- paste("Point forecasts and associated 80% interval forecasts of the ",
                      ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                      ARIMA$forecastingyear, 
                                              # " terminal run ",
                      " ",
                      tolower(ARIMA$stockabundance),
                      " for the ",
 				              ARIMA$stockname,
                		  " ",
                		  tolower(ARIMA$stockspecies),
                		  " stock.",
                      # min(ages)," - ",max(ages),
                      #     " and the total age. ",
                          ## " Negative lower limits for the interval forecasts were truncated to zero to ",
                          ## "ensure the interval forecasts only include positive values. ",
                          " The point forecasts for age-specific components of ",
                          # terminal run
                          tolower(ARIMA$stockabundance),
                          " were obtained from ARIMA models. ",
                          " The point forecast for total ",
                          # terminal run
                          tolower(ARIMA$stockabundance),
                          " was obtained by adding up the point forecasts for the age-specific components of ",
                          # terminal run
                          tolower(ARIMA$stockabundance),
                          " produced by the ARIMA models.",
                          " Interval forecasts were obtained by maximum entropy bootstrap.",
                          sep="")

doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")
   
ARIMA$MyFTable = FlexTable(data=ARIMA$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
    
ARIMA$MyFTable[,2,to = "body"] = parLeft()
ARIMA$MyFTable[,2,to = "header"] = parLeft()

doc = addFlexTable(doc, ARIMA$MyFTable)



### Visualization of forecast intervals - scatterplots (individual ages)

doc = addPageBreak(doc)

for (i in 1:length(ARIMA$arima.model.fits)){

    ARIMA$figurecaption <- paste("Historical ",
                           # terminal run
                           tolower(ARIMA$stockabundance),
                           " values along with the ",
                           ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                           ARIMA$forecastingyear, 
                          " point forecast and 80% interval forecast of the ",
                       # " terminal run",
                        tolower(ARIMA$stockabundance),
                       " corresponding to the ",
                       tolower(ARIMA$arima.model.fits[[i]]$age),
                       " component of the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       " The point forecast was obtained from the ARIMA model. ",
                       " The interval forecast was obtained by maximum entropy bootstrap.",
                       sep="")

   
    ARIMA$fits <- ARIMA$arima.model.fits
    ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits, ARIMA$boxcoxtransform)
    ARIMA$intervalforecasts <-   ARIMA$PI.individual.ages.arima.no.comma

    ARIMA$myplot <- ARIMA$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.arima(ARIMA$fits, 
                          ARIMA$boxcoxtransform, ARIMA$pointforecasts, ARIMA$intervalforecasts,i)

    doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth+1, height=plotheight-2)
            
    doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

    ## rm(myplot)
  
    ARIMA$myplot <- NULL
    ARIMA$figurecaption <- NULL 
}


### Visualization of forecast intervals - scatterplot(total age)


ARIMA$figurecaption <- paste("Historical total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " values ",
                       # "(", "ages ", min(ARIMA$ages), " - ", max(ARIMA$ages), ")",
                       "and corresponding ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       " point forecast and 80% forecast interval for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       " The point forecast for total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       # terminal run",
                        tolower(ARIMA$stockabundance),
                       " produced by the ARIMA models.",
                       " The interval forecast was obtained by maximum entropy bootstrap.",
                       sep="")


ARIMA$results <- ARIMA$results.total.age.retro.predictive.performance.arima

ARIMA$pointforecasts <- ARIMA$point.forecast.arima(ARIMA$datalist, ARIMA$arima.model.fits,  ARIMA$boxcoxtransform)

ARIMA$intervalforecasts <-  ARIMA$PI.total.age.arima.no.comma


ARIMA$myplot <- ARIMA$scatterplot.forecasted.values.and.forecast.intervals.total.age.arima(ARIMA$results, ARIMA$pointforecasts, ARIMA$intervalforecasts)  


doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL
ARIMA$figurecaption <- NULL 


#--- Bootstrap Distribution of Point Forecasts - Age-Specific Components

ARIMA$figurecaption <- paste("Distributions of bootstrapped forecasted ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " values ",
                       "for specific age components of the ",
                       paste(ARIMA$stockname," ", tolower(ARIMA$stockspecies),
                       " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       ".",
                       " The dashed red line indicates the position of the point forecast on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecasts were obtained on the basis of ARIMA modeling.",
                       # " The interval forecasts were obtained by bootstrap.",
                       sep="")



ARIMA$myplot <- ARIMA$plot.distribution.bootstrapped.point.forecasts.individual.ages.arima(ARIMA$PI.individual.ages.arima.sim,
                                                                               ARIMA$PI.individual.ages.arima.no.comma, 
                                                                               ARIMA$arima.model.fits,  
                                                                               ARIMA$boxcoxtransform, 
                                                                               ARIMA$extract_ages, 
                                                                               ARIMA$stockabundance)



doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL 



#--- Bootstrap Distribution of Point Forecasts - Total Age

ARIMA$figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " corresponding to the ",
                       paste(ARIMA$stockname," ", tolower(ARIMA$stockspecies),
                       " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       ".",
                       " The dashed red line indicates the position of the point forecast of total ", 
                       tolower(ARIMA$stockabundance),
                       " on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ", 
                       tolower(ARIMA$stockabundance),
                       "was obtained by adding up the point forecasts",
                       " for the age-specific components of ", tolower(ARIMA$stockabundance),
                       " produced by the ARIMA models.",
                       sep="")


## myplot <-  plot.distribution.bootstrapped.point.forecasts.total.age.arima(PI.total.age.arima.sim, PI.total.age.arima.no.comma)


ARIMA$myplot <- ARIMA$plot.distribution.bootstrapped.point.forecasts.total.age.arima(ARIMA$PI.total.age.arima.sim, 
                                                               ARIMA$PI.total.age.arima.no.comma, 
                                                               ARIMA$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth, height=plotheight-3)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL 
ARIMA$figurecaption <- NULL 


#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================


doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

ARIMA$empirical.probability.yboot.arima.total.age <- function(PI.total.age.arima.sim, PI.total.age.arima.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.arima.sim
     mylabel <- paste("Total", stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.arima.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


ARIMA$emp.prob.arima.total.age <- ARIMA$empirical.probability.yboot.arima.total.age(ARIMA$PI.total.age.arima.sim, 
                                                                              ARIMA$PI.total.age.arima.no.comma, 
                                                                              ARIMA$stockabundance)



ARIMA$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(ARIMA$stockabundance)," ",
                       "value yet to be observed in ",
                       ARIMA$forecastingyear, " for the ",
                       ARIMA$stockname," ",
                       ARIMA$stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(ARIMA$stockabundance), ".", 
                       " Maximum entropy bootstrapping was used as a basis for obtaining this distribution.")

doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")

ARIMA$tt_1 <- ARIMA$emp.prob.arima.total.age$prob.thresholds

ARIMA$tt_2 <- ARIMA$emp.prob.arima.total.age$prob.point.forecast

ARIMA$tt_1_and_2 <- rbind.data.frame(ARIMA$tt_1, ARIMA$tt_2)

usePackage("plyr")

## ARIMA$tt_arrange <- arrange(ARIMA$tt_1_and_2, ARIMA$prob.threshold)

ARIMA$tt_arrange <- ARIMA$tt_1_and_2[order(ARIMA$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties(padding = 2)

ARIMA$from_tmp = which(ARIMA$tt_arrange[,1] == ARIMA$emp.prob.arima.total.age$prob.point.forecast$prob.threshold)
ARIMA$tt_arrange[ARIMA$from_tmp, 4] <- ARIMA$tt_arrange[ARIMA$from_tmp + 1, 4]



usePackage("scales")
ARIMA$tt_arrange[,1] <- comma(ARIMA$tt_arrange[,1])
ARIMA$tt_arrange[,2] <- paste0(ARIMA$tt_arrange[,2],"%")
ARIMA$tt_arrange[,3] <- paste0(ARIMA$tt_arrange[,3],"%")
ARIMA$tt_arrange[,4] <- paste0(ARIMA$tt_arrange[,4],"%")

names(ARIMA$tt_arrange)[1] <- "Threshold"
names(ARIMA$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(ARIMA$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(ARIMA$tt_arrange)[4] <- "Interval Probability"
ARIMA$tt_arrange[1,4] <- "-"

ARIMA$my_ft <- FlexTable( data = ARIMA$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
ARIMA$my_ft[, 1:ncol(ARIMA$tt_arrange)] = parProperties(text.align = "right")


ARIMA$my_ft = spanFlexTableRows(ARIMA$my_ft, j=4, from = ARIMA$from_tmp, to = ARIMA$from_tmp + 1)

ARIMA$my_ft[ARIMA$tt_arrange$Threshold %in% comma(ARIMA$emp.prob.arima.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)


## ARIMA$my_ft[ARIMA$tt_arrange$Threshold %in% comma(ARIMA$emp.prob.arima.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )


doc = addFlexTable(doc, flextable=ARIMA$my_ft)

ARIMA$my_ft_emp_prob_total <- ARIMA$my_ft
ARIMA$tablecaption_emp_prob_total <- ARIMA$tablecaption

ARIMA$my_ft <- NULL 
ARIMA$tablecaption <- NULL 
ARIMA$tt_arrange <- NULL 
ARIMA$tt_1_and_2 <- NULL 
ARIMA$tt_1 <- NULL 
ARIMA$tt_2 <- NULL 

doc = addPageBreak(doc)
 

#---- Forecast Performance Measures ---------------------------------------------------------------

## doc = addPageBreak(doc)


doc = addTitle(doc, value="Retrospective Evaluation of Performance of Point Forecasts", level=1)
  
ARIMA$paragraph <- paste("This section reports the results corresponding to the retrospective evaluation of the performance of the",
                       "point forecasts produced",
                       "by the ARIMA models for the", ARIMA$forecastingyear, "age-specific and total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       "corresponding to the",
                       ARIMA$stockname, ARIMA$stockspecies, "stock.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 
   
ARIMA$paragraph <- paste("The retrospective evaluation of the performance of the point forecasts assessed how well",
                       "the ARIMA model",
                       "performed when used for retrospective forecasting of the historical",
                       tolower(ARIMA$stockabundance), "values.",
                       "For this evaluation, the ARIMA model was fit to all of the historical",
                       tolower(ARIMA$stockabundance), "values",
                       "available prior to a given historical return year, then the fitted model was used",
                       "to forecast the",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       "for that year.",   
                       "This evaluation captures how well the model would have performed in practice year over year",
                       "and was performed separately for each age-specific",
                        tolower(ARIMA$stockabundance),
                        "and for the total",
                        tolower(ARIMA$stockabundance),
                        ".", 
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("The Transformation, Identification, Estimation and Forecasting steps involved in fitting an ARIMA model",
                   "to historical values of", "age-specific or total", tolower(ARIMA$stockabundance), 
                   "were conducted independently from one retrospective forecasting year to another.",    
                   "In other words, the parameter lambda involved in the Box-Cox transformation - if selected by the user - and the order (p, d, q) of the ARIMA model",
                   "were estimated from scratch for each retrospective forecasting year using the historical",
                   tolower(ARIMA$stockabundance), "data available up to but not including that year, as explained in the Introduction. ",   
                   "As a consequence, different retrospective forecasting years can be expected to yield different values for lambda, p, d and q",
                   " for a given age component of ", tolower(ARIMA$stockabundance), " or for the total", tolower(ARIMA$stockabundance),      
                   sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL
 
ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("We initialized the retrospective evaluation with data from the first", ARIMA$index.year, 
                   "return years.",        
                   sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("Retrospective forecast errors were defined as the actual",
                       # terminal run                                            
                       tolower(ARIMA$stockabundance),
                       "values minus the retrospectively forecasted",
                       # "terminal run values.",
                        tolower(ARIMA$stockabundance),
                       "values.",
                       "In view of this definition, positive values for the retrospective forecast errors represent forecasts that were too low,",
                       "whereas negative values represent forecasts that were too high.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("The following retrospective measures were used to characterize different aspects of the retrospective forecasting errors:")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
                      
ARIMA$paragraph <- NULL 

doc = addParagraph( doc, value = 'Mean Raw Error (MRE);',
      par.properties = parProperties(list.style = 'unordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Mean Absolute Error (MAE);',
      par.properties = parProperties(list.style = 'unordered', level = 1), stylename="BulletList" )
doc = addParagraph( doc, value = 'Mean Percent Error (MPE);',
      par.properties = parProperties(list.style = 'unordered', level = 1), stylename="BulletList" )
doc = addParagraph( doc, value = 'Mean Absolute Percent Error (MAPE);',
      par.properties = parProperties(list.style = 'unordered', level = 1), stylename="BulletList" )
doc = addParagraph( doc, value = 'Mean Scaled Error (MASE);',
      par.properties = parProperties(list.style = 'unordered', level = 1), stylename="BulletList" )
doc = addParagraph( doc, value = 'Root Mean Square Error (RMSE).',
      par.properties = parProperties(list.style = 'unordered', level = 1), stylename="BulletList" )

ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("MAE and MAPE reflect the overall forecast accuracy accounting for systematic bias and year-to-year variation.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
   
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("MRE and MPE reflect directional bias in raw and relative forecast errors, respectively, with negative values indicating a tendency to",
                       "underforecast and positive values reflecting a tendency to overforecast.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
      
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("Just like MAE, RMSE  is a measure of the absolute magnitude of the raw retrospective forecast errors, but is more sensitive to large values",
                       "then MAE.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
    
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("MASE was proposed by Hyndman and Koehler (2006) as a generally applicable, scale-free measure of forecast accuracy.",
                       "This measure never gives infinite or undefined values.",
                       "In this report, MASE is computed as the average of the absolute values of the scaled retrospective forecast errors produced by",
                       "the ARIMA model.",
                       "The scaling of the errors involves dividing the errors by the MAE computed from the retrospective forecast errors associated with",
                       "the naive model based on the",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       "for the previous year.",
                       # "from the one-step, naive forecasting method.",
                       # "A scaled error is less than 1 if it arises from a better forecast than the one produced by the naive model based on the terminal run for the previous year.",
                       # "Conversely, it is greater than 1 if the forecast is worse than the average one-step, naive forecast computed in-sample.",
                       "A value of MASE less than 1 suggests that the retrospective forecasting accuracy of the ARIMA model",
                       "is better than the retrospective forecasting accuracy of",
                       "the benchmark naive model based on the",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       "for the previous year.",
                       "A value of MASE greater than 1 suggests that the retrospective forecasting accuracy of the ARIMA model",
                       "is worse than the retrospective forecasting accuracy of",
                       "the benchmark naive model based on the",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       "for the previous year.",
                      sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
    
ARIMA$paragraph <- NULL 
                      
ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 
                      
ARIMA$paragraph <- paste("To facilitate the interpretation of the retrospective forecast errors, this section reports several types of plots:")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 

doc = addParagraph( doc, value = paste0('Plots illustrating the performance of the retrospective forecasting evaluation;'),
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Density plots of the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Barplots of the retrospective forecast errors together with the forecast interval corresponding to the forecasting year of interest.',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = paste0('Time series plots displaying the retrospective point forecasts against the actual historical values;'),
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = paste0('Scatter plots displaying the retrospective point forecasts against the actual historical values.'),
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")


ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("Bias coefficients representing a new metric for forecast bias are also reported in numerical and visual form in this section. ", 
                    "These coefficients are computed from the retrospective forecast errors for the age-specific and total ", 
                    paste0(tolower(ARIMA$stockabundance),"s"), 
                    " using the formula developed by Kourentzes, Trapero and Svetunkov", 
                    " in their 2014 working paper \"Measuring the behaviour of experts on demand forecasting: a complex task\".", 
                    " In the context of this report, the bias coefficients describe the direction and magnitude of the retrospective forecast bias associated with the", 
                    "exponential smoothing forecasting method.", 
                    sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL

ARIMA$paragraph <- paste(" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")
ARIMA$paragraph <- NULL 

ARIMA$paragraph <- paste("Generally speaking, the bias coefficients are unit-free and bounded between -1 (maximum negative retrospective forecast bias)",  
                    "and 1 (maximum positive retrospective forecast bias).", 
                    "A forecasting method that is always producing retrospective point forecasts which are over the observed historical values will have",
                    "a bias coefficient equal to -1, always over-forecasting.", 
                    "A forecasting method that is always producing retrospective point forecasts which are under the observed historical values", 
                    "will have a bias coefficient equal to 1, always under-forecasting.", 
                    "Given the bounded nature of the bias coefficient, we can describe a forecasting method as strongly biased if |bias coefficient| > 0.5", 
                    "and weakly biased if 0 < |bias coefficient| <= 0.5, providing a simple and intuitive description of the forecast bias behaviour.", 
                    "If the bias coefficient is equal to 0, the forecasting method is unbiased.", 
                    sep=" ")
doc = addParagraph(doc, ARIMA$paragraph, stylename="Normal")

ARIMA$paragraph <- NULL 


## paragraph <- paste("We also calculated the coefficient of determination obtained by regressing the retrospectively forecasted",
##                       tolower(stockabundance),
##                       "values",
##                       "on the historical",
##                       # terminal run
##                       tolower(stockabundance),
##                       "values.",
##                       "This is simply the squared correlation coefficient of the retrospectively forecasted and historical",
##                       # terminal run
##                        tolower(stockabundance),
##                       "values.",
##                       sep=" ")
## doc = addParagraph(doc, paragraph, stylename="Normal")
  

  
#---- Retrospective Measures of Forecast Performance -------------------------------------

doc = addTitle(doc, value="Retrospective Measures of Forecast Performance", level=2)

ARIMA$tabledata <- ARIMA$M.arima

ARIMA$tabledata <- subset(ARIMA$M.arima, select=-Model)

ARIMA$tabledata <- cbind(Model=rep("ARIMA",nrow(ARIMA$M.arima)),ARIMA$tabledata)

ARIMA$tablecaption <- paste("Retrospective measures of forecast performance ",
                "associated with the point forecasts of the ",
                ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                ARIMA$forecastingyear, 
                " age-specific and total ",
                # terminal runs
                paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                " for the ",
                ARIMA$stockname,
                " ",
                tolower(ARIMA$stockspecies),
                " stock.",
                " The point forecasts of age-specific ",
                # terminal runs
                paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                " were obtained on the basis of ARIMA modeling.",
                " The point forecast for the total ",
                # terminal run
                 tolower(ARIMA$stockabundance),
                " was obtained by adding up the point forecasts for the age-specific components of ",
                # terminal run",
                 tolower(ARIMA$stockabundance),
                " produced by the ARIMA models.",
                sep=""
                )

names(ARIMA$tabledata)[names(ARIMA$tabledata)=="Model"] <- "Model Class"

ARIMA$tt <- ARIMA$tabledata 


usePackage("stringr")
names(ARIMA$tt) <- str_replace_all(names(ARIMA$tt),"_"," ")

## tt[,-1] <- comma(tt[,-1])

doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")

ARIMA$my_ft <- FlexTable( data = ARIMA$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
ARIMA$my_ft[, 1:ncol(ARIMA$tt)] = parProperties(text.align = "right")

ARIMA$my_ft = addFooterRow( ARIMA$my_ft, value = paste("Legend:  MRE = Mean Relative Error; ", 
                                           "MAE = Mean Absolute Error; ", 
                                           "MPE = Mean Percentage Error; ", 
                                           "MAPE = Mean Absolute Percentage Error; ",
                                           "MASE = Mean Scaled Error; ",
                                           "MSE = Root Mean Square Error."), 
        colspan = ncol(ARIMA$tt), 
        text.properties = textProperties(font.size = 9),
        par.properties = parProperties(text.align = "left"))
        
## my_ft = addFooterRow( my_ft, value = c("         MAE = Mean Absolute Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MPE = Mean Percentage Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MAPE = Mean Absolute Percentage Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MASE = Mean Scaled Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         RMSE = Root Mean Square Error."), colspan = ncol(tt))


doc = addFlexTable(doc, flextable=ARIMA$my_ft)
          
ARIMA$my_ft <- NULL 
ARIMA$tablecaption <- NULL 

#---- Retrospective Forecast Errors:  Specific Ages ------------------------------------------------------


doc = addPageBreak(doc)

doc = addTitle(doc, "Retrospective Point Forecasts and Forecast Errors", level=2)

for (i in 1:length(ARIMA$results.individual.ages.retro.predictive.performance.arima)) {

    ARIMA$results <- ARIMA$results.individual.ages.retro.predictive.performance.arima

    ARIMA$tabledata <- ARIMA$results[[i]]$data.retro

    ARIMA$age <- names(ARIMA$results)[i]
    usePackage("stringr")
    ARIMA$age <- str_extract(ARIMA$age,"[[:digit:]]+")
    
    
    
    ARIMA$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the age ",ARIMA$age,
                          " component of the ",
 				              ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                		  ARIMA$forecastingyear, 
                      # " terminal run for the ",
                		  " ",
                      tolower(ARIMA$stockabundance),                                              
                      " for the ",
                      ARIMA$stockname,
                		  " ",
                		  tolower(ARIMA$stockspecies),
                		  " stock. ",
                          "Accompanying return years and actual ",
                     # terminal run
                     tolower(ARIMA$stockabundance),
                     " values are also reported.",
                      " The retrospective point forecasts were obtained on the basis of ARIMA modeling.",
                          sep="")

    ARIMA$tabledata$p <- round(ARIMA$tabledata$p,2)
    ARIMA$tabledata$e <- round(ARIMA$tabledata$e,2)

    usePackage("scales")
    ARIMA$tabledata$a <- comma(round(ARIMA$tabledata$a))
    ARIMA$tabledata$p <- comma(round(ARIMA$tabledata$p))
    ARIMA$tabledata$e <- comma(round(ARIMA$tabledata$e))

    names(ARIMA$tabledata)[names(ARIMA$tabledata)=="cy"] <- "Return Year"
    names(ARIMA$tabledata)[names(ARIMA$tabledata)=="a"] <-  "Actual"
    names(ARIMA$tabledata)[names(ARIMA$tabledata)=="p"] <-  "Forecast"
    names(ARIMA$tabledata)[names(ARIMA$tabledata)=="e"] <-  "Error"


    ARIMA$tabledata
    
    ARIMA$tabledata <- subset(ARIMA$tabledata, select=c(-p.bench, -e.bench))

    doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")

    baseCellProp = cellProperties( padding = 4)

    ARIMA$tt <- ARIMA$tabledata


    usePackage("stringr")
    names(ARIMA$tt) <- str_replace_all(names(ARIMA$tt),"_"," ")

    ## tt[,-1] <- comma(tt[,-1])

    ARIMA$my_ft <- FlexTable( data = ARIMA$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
    )

    # overwrites some paragraph formatting properties
    ARIMA$my_ft[, 1:ncol(ARIMA$tt)] = parProperties(text.align = "right")

    doc = addFlexTable(doc, flextable=ARIMA$my_ft)
    
    ARIMA$my_ft <- NULL
    ARIMA$tablecaption <- NULL
    
    doc = addPageBreak(doc)

}



#---- Retrospective Forecast Errors:  Total Age ------------------------------------------------------------

ARIMA$results <- ARIMA$results.total.age.retro.predictive.performance.arima

ARIMA$tabledata <- data.frame(cy = ARIMA$results$data.retro[[1]]$cy,
                        a = ARIMA$results$a.total.retro,
                        p = ARIMA$results$p.total.retro,
                        e = ARIMA$results$e.total.retro)


ARIMA$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the ",
 				              ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                		  ARIMA$forecastingyear, 
                      " total ",
                      # terminal run
                      tolower(ARIMA$stockabundance),
                      " for the ",
                		  ARIMA$stockname,
                		  " ",
                		  tolower(ARIMA$stockspecies),
                		  " stock. ",
                          "Accompanying return years and actual total ",
                      # terminal run
                      tolower(ARIMA$stockabundance),
                      " values are also reported.",
                      " The retrospective point forecasts for the total ",
                      # terminal run
                       tolower(ARIMA$stockabundance),
                      " were obtained by adding up the retrospective point forecasts for the age-specific components of ",
                      # terminal run
                      tolower(ARIMA$stockabundance),
                      " produced by the ARIMA models.",
                      sep="")

usePackage("scales")
ARIMA$tabledata$a <- comma(round(ARIMA$tabledata$a))
ARIMA$tabledata$p <- comma(round(ARIMA$tabledata$p))
ARIMA$tabledata$e <- comma(round(ARIMA$tabledata$e))

names(ARIMA$tabledata)[names(ARIMA$tabledata)=="cy"] <- "Return Year"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="a"] <-  "Actual"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="p"] <-  "Forecast"
names(ARIMA$tabledata)[names(ARIMA$tabledata)=="e"] <-  "Error"

ARIMA$tabledata


doc = addParagraph(doc, value=ARIMA$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

ARIMA$tt <- ARIMA$tabledata


usePackage("stringr")
names(ARIMA$tt) <- str_replace_all(names(ARIMA$tt),"_"," ")


## tt[,-1] <- comma(tt[,-1])

ARIMA$my_ft <- FlexTable( data = ARIMA$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
ARIMA$my_ft[, 1:ncol(ARIMA$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=ARIMA$my_ft)


#----- Retrospective Point Forecasts vs. Actual Over Time: Individual Ages ---------------------------------------------------------------------

for (j in 1:length(ARIMA$individual.ages.retro.plot.info)) {

     doc = addPageBreak(doc)
     
     doc = addPlot(doc, 
        fun = print,
        x = ARIMA$individual.ages.retro.plot(ARIMA$individual.ages.retro.plot.info, ARIMA$stockabundance, j),
        width=6.5, height=7)
        
     ARIMA$figurecaption <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ", 
                       tolower(ARIMA$arima.model.fits[[j]]$age),
                       " component of ",
                       # max(results.afe.total.age.retro.arima$CY)+1,
                       # " terminal run
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock,"),
                       " derived on the basis of ARIMA modeling. ",
                       "Historical values of age-specific ", ARIMA$stockabundance, 
                       " (grey lines) and fitted values produced by the ARIMA modeling (red lines) are also shown. ", 
                       "Each panel corresponds to a particular retrospective forecasting year.",   
                       sep="")
     
     doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

}

#----- Retrospective Point Forecasts vs. Actual Over Time: Total Age ---------------------------------------------------------------------

doc = addPageBreak(doc)

doc = addPlot(doc, 
        fun = print,
        x = ARIMA$total.age.retro.plot(ARIMA$total.age.retro.plot.info, ARIMA$stockabundance),
        width=6.5, height=7)
        
ARIMA$figurecaption <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ", 
                      "total ", 
                       # max(results.afe.total.age.retro.arima$CY)+1,
                       # " terminal run
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock,"),
                       " derived on the basis of ARIMA modeling. ",
                       "Historical values of total ", ARIMA$stockabundance, 
                       " (grey lines) and fitted values produced by the ARIMA modeling (red lines) are also shown. ", 
                       "Each panel corresponds to a particular retrospective forecasting year.",   
                       sep="")
     
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")



#---- Retrospective Forecast Errors: Age-Specific Density Plots ---------------------------------------------------------------


doc = addPageBreak(doc)

ARIMA$figurecaption <- paste("Density plots of the retrospective forecast errors derived from the ARIMA models used to forecast ",
                       "specific age components of the ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       " ",
                       # " terminal run for the ",
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       sep="")




   
ARIMA$myplot <- ARIMA$dens.results.afe.individual.ages.retro.arima(
    ARIMA$arima.model.fits,
    ARIMA$boxcoxtransform, 
    ARIMA$results.individual.ages.retro.predictive.performance.arima
   )

doc = addPlot(doc,
            fun= plot, # print,  ## DEBUG
            x=ARIMA$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL

#---- Retrospective Forecast Errors: Individual Ages Bias Coefficients Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

ARIMA$figurecaption <- paste("Bias coefficient plots constructed from the retrospective forecast errors produced by the ARIMA models used to forecast ",
                       "specific age components of the ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       " ",
                       # " terminal run for the ",
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       sep="")


ARIMA$myplot <- ARIMA$bias.coefficients.afe.individual.ages.retro.arima(
              ARIMA$arima.model.fits,
              ARIMA$results.individual.ages.retro.predictive.performance.arima, 
              ARIMA$boxcoxtransform, 
              ARIMA$stockabundance)

usePackage("gridGraphics")

doc = addPlot(doc,                                                                                                         
            fun=grid.draw,
            x=ARIMA$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL
ARIMA$figurecaption <- NULL 

#---- Retrospective Forecast Errors: Total Age Density Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

ARIMA$figurecaption <- paste("Density plots of the retrospective forecast errors ",
                       "involved in forecasting the total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       sep="")

ARIMA$myplot <- ARIMA$dens.results.afe.total.age.retro.arima(ARIMA$results.afe.total.age.retro.arima)


doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth, height=plotheight-3)


doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL
ARIMA$figurecaption <- NULL 


#---- Retrospective Forecast Errors: Total Age Bias Coefficients Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

ARIMA$figurecaption <- paste("Bias coefficient plot constructed from the retrospective forecast errors produced by the ARIMA models used to forecast ",
                       "the ",
                       ## max(ARIMA$results.afe.total.age.retro.arima$CY)+1,
                       ARIMA$forecastingyear, 
                       " total ",
                       # " terminal run for the ",
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock."),
                       sep="")


ARIMA$myplot <- ARIMA$bias.coefficient.afe.total.age.retro.arima(
              ## ARIMA$results.individual.ages.retro.predictive.performance.arima, 
              ARIMA$results.total.age.retro.predictive.performance.arima, 
              ARIMA$stockabundance)

usePackage("gridGraphics")

doc = addPlot(doc,
            fun=grid.draw,
            x=ARIMA$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL 
ARIMA$figurecaption <- NULL 


#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Specific Ages ---------------------------------------------------------------


for (i in 1:length(ARIMA$arima.model.fits)) {

    doc = addPageBreak(doc)

    ARIMA$results.retro <- ARIMA$results.individual.ages.retro.predictive.performance.arima
    ARIMA$results.pred <- ARIMA$pred.int.individual.ages.arima


    doc = addPlot(doc, 
        fun = print,
        x = ARIMA$gary.plot.individual.ages(ARIMA$results.retro, ARIMA$results.pred, i),
        width=6.5, height=6)

    ARIMA$figurecaption <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       tolower(ARIMA$arima.model.fits[[i]]$age),
                       " component of the ",
                       # max(results.afe.total.age.retro.arima$CY)+1,
                       # " terminal run
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock,"),
                       " derived on the basis of ARIMA modeling.",
                       sep="")
     
     doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

     ARIMA$figurecaption <- NULL 

}





#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Total Age ---------------------------------------------------------------


doc = addPageBreak(doc)

ARIMA$results.retro <- ARIMA$results.total.age.retro.predictive.performance.arima
names(ARIMA$results.retro)
ARIMA$results.pred <- ARIMA$PI.total.age.arima.no.comma

doc = addPlot(doc, 
        fun = print,
        x = ARIMA$gary.plot.total.age(ARIMA$results.retro, ARIMA$results.pred),
        width=6.5, height=6) 


ARIMA$figurecaption <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       # "the ",
                       # max(results.afe.total.age.retro.arima$CY)+1,
                       " total ",
                       # terminal run ",
                       tolower(ARIMA$stockabundance),
                       " for the ",
                       paste(ARIMA$stockname,tolower(ARIMA$stockspecies), "stock,"),
                       " derived on the basis of ARIMA modeling.",
                       sep="")
                       

doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$figurecaption <- NULL 


#---- Forecast Diagnostics - Individual Ages - Plot No. 2 ------------------------------------------------------------------------

doc = addTitle( doc, "Forecast Diagnostics", level = 1)

ARIMA$figurecaption <- paste("Time series plots of retrospectively forecasted ",
                       # terminal runs
                       " and actual ",  
                        paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " for specific age ",
                       "components of the ",
                       ARIMA$stockname, " ",
                       tolower(ARIMA$stockspecies),
                       " stock. ", 
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " for a given return year was derived by applying ARIMA modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       sep=""
                       )


ARIMA$myplot <- ARIMA$timeseries.plot.results.afe.individual.ages.retro.arima(
       ## arima.model.fits,
       ARIMA$results.individual.ages.retro.predictive.performance.arima, 
       ARIMA$stockabundance
       )
 

doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL 


#---- Forecast Diagnostics - Individual Ages - Plot No. 1 ------------------------------------------------------------------------

ARIMA$figurecaption <- paste("Scatterplots of retrospectively forecasted ",
                       # terminal runs
                       " versus actual ", 
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " for specific age ",
                       "components of the ",
                       ARIMA$stockname, " ",
                       tolower(ARIMA$stockspecies),
                       " stock. ",
                       "Observations in each panel are labeled according to the last two digits of the associated historical return years. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " for a given return year was derived by applying ARIMA modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       sep=""
                       )


ARIMA$myplot <- ARIMA$scatter.plot.results.afe.individual.ages.retro.arima(
       ## arima.model.fits,
       ARIMA$results.individual.ages.retro.predictive.performance.arima, 
       ARIMA$stockname, ARIMA$stockabundance
       )
 

doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL 
ARIMA$figurecaption <- NULL 
                                                                                                                           

#---- Forecast Diagnostics - Plot No. 2 - Total Age ------------------------------------------------------------------------

doc = addPageBreak(doc)

ARIMA$figurecaption <- paste("Time series plot of retrospectively forecasted and ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " actual total ",
                       # terminal runs ",
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(ARIMA$results.afe.total.age.retro.arima$CY),
                       " - ",
                       max(ARIMA$results.afe.total.age.retro.arima$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " ARIMA modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")

ARIMA$myplot <- ARIMA$timeseries.plot.results.afe.total.age.retro.arima(ARIMA$results.total.age.retro.predictive.performance.arima, ARIMA$stockabundance)

doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL 



#---- Forecast Diagnostics - Plot No. 1 - Total Age ------------------------------------------------------------------------

doc = addPageBreak(doc)

ARIMA$figurecaption <- paste("Scatterplot of retrospectively forecasted total ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " versus actual total ",
                       # terminal runs ",
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(ARIMA$results.afe.total.age.retro.arima$CY),
                       " - ",
                       max(ARIMA$results.afe.total.age.retro.arima$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the last two digits of the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(ARIMA$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " ARIMA modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(ARIMA$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")

ARIMA$myplot <- ARIMA$scatter.plot.results.afe.total.age.retro.arima(ARIMA$results.total.age.retro.predictive.performance.arima, ARIMA$stockabundance)

doc = addPlot(doc,
            fun=print,
            x=ARIMA$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=ARIMA$figurecaption, stylename="rPlotLegend")

ARIMA$myplot <- NULL 

ARIMA$fits <- NULL 

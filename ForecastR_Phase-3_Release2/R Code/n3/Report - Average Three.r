#========================================================================================================
# CoverPage
#========================================================================================================

n3$fits <- n3$avgthree.model.fits

usePackage("stringr")
n3$stockabundance <- str_replace(n3$stockabundance,"_"," ") 

n3$pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 40) )
n3$my.pars = set_of_paragraphs(n3$pot1)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )

n3$pot1 <- NULL 
n3$my.pars <- NULL 

n3$pot1 = pot(" ", textProperties(font.weight="bold", font.size = 20) )
n3$my.pars = set_of_paragraphs(n3$pot1)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )

n3$pot1 <- NULL 
n3$my.pars <- NULL 

pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n3$stockname), textProperties(font.size = 20) )
n3$my.pars = set_of_paragraphs(pot2)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )

n3$pot2 <- NULL 
n3$my.pars <- NULL 

pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n3$stockspecies), textProperties(font.size = 20) )
n3$my.pars = set_of_paragraphs(pot3)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )

n3$pot3 <- NULL 
n3$my.pars <- NULL 

pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n3$stockabundance), textProperties(font.size = 20) )
n3$my.pars = set_of_paragraphs(pot4)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )

n3$pot4 <- NULL 
n3$my.pars <- NULL 

pot5 =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n3$forecastingyear), textProperties(font.size = 20) )
n3$my.pars = set_of_paragraphs(pot5)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )

n3$pot5 <- NULL 
n3$my.pars <- NULL 

n3$pot6 =  pot("Forecasting Model: ", textProperties(font.weight="bold", font.size = 20)) 
n3$pot9 = pot(paste("Naive Model (Average of Previous 3 Years)"), textProperties(font.size = 20) ) 
n3$my.pars = set_of_paragraphs(n3$pot6, n3$pot9)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )
       
n3$pot6 <- NULL
n3$pot9 <- NULL  
n3$my.pars <- NULL 
               
if (n3$bootmethod=="meboot") {      
    n3$texta <- "Maximum Entropy Bootstrap"
}

if (n3$bootmethod=="stlboot") {      
    n3$texta <- "Loess Bootstrap"
}
               
n3$pot6a =  pot("Time Series Bootstrap Method: ", textProperties(font.weight="bold", font.size = 20)) 
n3$pot9a = pot(paste0(n3$texta), 
            textProperties(font.size = 20) ) 
n3$my.pars = set_of_paragraphs(n3$pot6a, n3$pot9a)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )
               
n3$pot6a <- NULL 
n3$pot9a <- NULL 
n3$my.pars <- NULL 
     
n3$pot13 =  pot("Date: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(Sys.Date()), textProperties(font.size = 20) )
n3$my.pars = set_of_paragraphs(n3$pot13)
doc = addParagraph( doc, value = n3$my.pars, stylename="Normal" )
     
n3$pot13 <- NULL 
n3$my.pars <- NULL 

doc = addPageBreak(doc)


#=================================================================================================
# Summary of Results
#=================================================================================================


doc = addTitle(doc, "Summary of Results", level=1)

## point forecasts + individual ages
n3$results.point.forecast.avgthree


n3$tabledata <- matrix(NA, nrow=11, ncol=length(n3$fits)+2)

colnames(n3$tabledata) <- rep("Name", length(n3$fits)+2)
colnames(n3$tabledata)[1] <- "Item"
colnames(n3$tabledata)[2:(1+length(n3$fits))] <- as.character(n3$results.point.forecast.avgthree$Age)
colnames(n3$tabledata)[2+length(n3$fits)] <- "Total"


n3$tabledata[1,] <- c("Return Year", rep(unique(n3$results.point.forecast.avgthree$RY),
                   length(n3$fits)+1))

n3$tabledata[2,] <- c("Model", rep("Previous Year", length(n3$fits)+1))

n3$tabledata[3,] <- c("Point Forecast",
                   c(as.character(comma(round(n3$results.point.forecast.avgthree$p))),
                     as.character(comma(sum(round(n3$results.point.forecast.avgthree$p))))))


n3$PI.individual.ages.avgthree
n3$PI.total.age.avgthree

n3$PI.combined <- rbind(n3$PI.individual.ages.avgthree, n3$PI.total.age.avgthree)

n3$PI.combined.vec <- NULL
for (i in 1:nrow(n3$PI.combined)){

   n3$tmp.vec <- paste0(n3$PI.combined[i,"PI.lwr"]," - ",n3$PI.combined[i,"PI.upr"])

   n3$PI.combined.vec <- c(n3$PI.combined.vec, n3$tmp.vec)

}


n3$tabledata[4,] <- c("Interval Forecast", n3$PI.combined.vec)

## tabledata <- M.avgthree
## tabledata <- subset(M.avgthree, select=-Model)

n3$M.sub.avgthree <- subset(n3$M.avgthree, select=-Model)

n3$tabledata[5:(5 + nrow(n3$M.sub.avgthree) - 1),"Item"] <- as.character(n3$M.sub.avgthree$Measure)

for (k in 2:ncol(n3$M.sub.avgthree)){
    n3$tabledata[5:(5 + nrow(n3$M.sub.avgthree) - 1),k] <- n3$M.sub.avgthree[,k]
}



n3$fits <-  n3$avgthree.model.fits
n3$results <- n3$results.individual.ages.retro.predictive.performance.avgthree
n3$results.total.age <- n3$results.total.age.retro.predictive.performance.avgthree

## r.squared.table <- r.squared.values.avgthree(fits, results, results.total.age)
## tabledata[5 + nrow(M.sub.avgthree),"Item"] <- "R-squared"
## tabledata[5 + nrow(M.sub.avgthree),-1] <- as.character(r.squared.table$R.squared)


##
## Ethan fix - March 16, 2017
##
n3$bias.coeff.afe.individual.ages.retro.avgthree <- n3$bias.coeff.afe.individual.ages.retro.avgthree[!is.na(names(n3$bias.coeff.afe.individual.ages.retro.avgthree))]

n3$tabledata[5 + nrow(n3$M.sub.avgthree),"Item"] <- "Bias Coefficient"
n3$tabledata[5 + nrow(n3$M.sub.avgthree),-1] <- round(c(n3$bias.coeff.afe.individual.ages.retro.avgthree, 
                                            n3$bias.coeff.afe.total.age.retro.avgthree),4)

n3$tabledata <- as.data.frame(n3$tabledata)


n3$tablecaption <- paste("Summary of forecasting results for the",
                      n3$forecastingyear,
                      "age-specific and total",
                      # terminal run
                      paste(tolower(n3$stockabundance),"s",collapse="", sep=""),
                      " associated with the ",
                      n3$stockname, " ",
                      tolower(n3$stockspecies), " stock.")

n3$tabledata <- n3$tabledata

doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")
    
n3$MyFTable = FlexTable(data=n3$tabledata, 
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
n3$MyFTable <- setFlexTableBorders(n3$MyFTable, 
                                inner.vertical = borderProperties(style = "none"), 
                                inner.horizontal = borderProperties(style = "none"), 
                                outer.vertical = borderProperties(style = "none"), 
                                outer.horizontal = borderProperties(color = "gray5", style = "solid")
)


doc = addFlexTable(doc, n3$MyFTable)


n3$MyFTable <- NULL 
n3$tablecaption <- NULL 
n3$tabledata <- NULL 

#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

n3$empirical.probability.yboot.avgthree.total.age <- function(PI.total.age.avgthree.sim, PI.total.age.avgthree.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.avgthree.sim
     mylabel <- paste("Total", n3$stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.avgthree.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


n3$emp.prob.avgthree.total.age <- n3$empirical.probability.yboot.avgthree.total.age(n3$PI.total.age.avgthree.sim, 
                                                                              n3$PI.total.age.avgthree.no.comma, 
                                                                              n3$stockabundance)



n3$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(n3$stockabundance)," ",
                       "value yet to be observed in ",
                       n3$forecastingyear, " for the ",
                       n3$stockname," ",
                       n3$stockspecies, " stock",
                        " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(n3$stockabundance), ".")

doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")

n3$tt_1 <- n3$emp.prob.avgthree.total.age$prob.thresholds

n3$tt_2 <- n3$emp.prob.avgthree.total.age$prob.point.forecast

n3$tt_1_and_2 <- rbind.data.frame(n3$tt_1, n3$tt_2)

usePackage("plyr")

## n3$tt_arrange <- arrange(n3$tt_1_and_2, prob.threshold)

n3$tt_arrange <- n3$tt_1_and_2[order(n3$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

n3$from_tmp = which(n3$tt_arrange[,1] == n3$emp.prob.avgthree.total.age$prob.point.forecast$prob.threshold)
n3$tt_arrange[n3$from_tmp, 4] <- n3$tt_arrange[n3$from_tmp + 1, 4]

n3$tt_arrange[,1] <- comma(n3$tt_arrange[,1])
n3$tt_arrange[,2] <- paste0(n3$tt_arrange[,2],"%")
n3$tt_arrange[,3] <- paste0(n3$tt_arrange[,3],"%")
n3$tt_arrange[,4] <- paste0(n3$tt_arrange[,4],"%")


names(n3$tt_arrange)[1] <- "Threshold"
names(n3$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(n3$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(n3$tt_arrange)[4] <- "Interval Probability"
n3$tt_arrange[1,4] <- "-"


n3$my_ft <- FlexTable( data = n3$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
n3$my_ft[, 1:ncol(n3$tt_arrange)] = parProperties(text.align = "right")

## n3$my_ft[n3$tt_arrange$Threshold %in% comma(n3$emp.prob.avgthree.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )
## Mistake?

n3$my_ft = spanFlexTableRows(n3$my_ft, j=4, from = n3$from_tmp, to = n3$from_tmp + 1)

n3$my_ft[n3$tt_arrange$Threshold %in% comma(n3$emp.prob.avgthree.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

doc = addFlexTable(doc, flextable=n3$my_ft)

n3$my_ft <- NULL 
n3$tt_arrange <- NULL 
n3$tt_1 <- NULL
n3$tt_2 <- NULL
n3$tt_1_and_2 <- NULL 
n3$tablecaption <- NULL 

doc = addPageBreak(doc)


#=================================================================================================
# Introduction
#=================================================================================================

## doc = addPageBreak(doc)

doc = addTitle(doc, "Introduction", level=1)

n3$paragraph <- paste("In this report, we forecast the",
                    n3$forecastingyear,
                    "age-specific and total",
                    paste0(tolower(n3$stockabundance),"s",collapse=" "),
                    "for the",
                    n3$stockname, n3$stockspecies, "stock using a naive forecasting model which does not require statistical parameter estimation",
                    "but rather summarizes past",  
                    tolower(n3$stockabundance),
                    "observations to make forecasts.",
                    "Specifically, given a yearly",
                    tolower(n3$stockabundance),
                    "series, the model uses the average",
                    tolower(n3$stockabundance),
                    "from the previous 3 years to forecast the",
                    tolower(n3$stockabundance),
                    "for the next year.",
                    sep=" ")
    
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

#================================================================================================
# Data
#================================================================================================

doc = addTitle(doc, "Data", level=1)


#---- Show original data (by calendar year) ------------------------------------------------------

tablecounter <- 1
n3$tablecaption <- paste("Historical ",
                      # terminal run
                      tolower(n3$stockabundance),
                      " data for the ",
                      n3$stockname, 
                      tolower(n3$stockspecies), " stock,",
                      "reported as a function of calendar year for specific ages of the stock.")

n3$tabledata <- n3$datafile
n3$tabledata[n3$tabledata<0] <- NA

# datalist

n3$datalist1 <- n3$datalist[[1]]
n3$datalist1 <- subset(n3$datalist1,select=-BY)
for (i in 2:length(n3$datalist)){
      n3$datalist.tmp <- n3$datalist[[i]]
      n3$datalist.tmp <- subset(n3$datalist.tmp,select=-BY)

      n3$datalist1 <-  merge(n3$datalist1, n3$datalist.tmp, by=c("CY"))

}

# datalist1

n3$tabledata <- n3$datalist1

usePackage("stringr")
names(n3$tabledata) <- str_replace_all(names(n3$tabledata),"T","Age ")
names(n3$tabledata)[names(n3$tabledata)=="CY"] <- "Calendar Year"

usePackage("scales")
n3$tabledata[,-1] <- comma(n3$tabledata[,-1] )


doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")
    
n3$MyFTable = FlexTable(data=n3$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
                                  
doc = addFlexTable(doc, n3$MyFTable)

n3$MyFTable <- NULL 
n3$tabledata <- NULL 
n3$tablecaption <- NULL 

#---- Plot original data by return year (for specific ages) -----------------------------------------------

doc = addPageBreak(doc)

n3$figurecaption <- paste("Plots of historical ",
                       # terminal runs,
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " versus return years for the ",
                       n3$stockname," ",
                       tolower(n3$stockspecies), " stock, ",
                       "with each plot corresponding to a specific age component of the stock.",sep="")

doc = addPlot(doc, 
        fun = print,
        x = n3$plot.data.avgthree(n3$datalist),
        width=6.5, height=6)

doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

#================================================================================================
# Modeling Results
#================================================================================================


doc = addTitle(doc, "Naive Time Series Modeling Results", level=1)

#----- Plot Fitted Values Produced by Naive Forecasting (Average of Previous 3 Years) for Each Age Class -------


n3$figurecaption <- paste("Fitted values produced by the naive models used to forecast ",
                       "specific age components of the ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " ",
                       tolower(n3$stockabundance),
                       " for the ",
                       n3$stockname, " ", tolower(n3$stockspecies), " stock. ",
                       "The naive models were based on the average ",
                       tolower(n3$stockabundance),
                       " over the previous 3 years.", sep="")

doc = addPlot(doc, 
        fun = print,
        x = n3$plot.fitted.avgthree(n3$avgthree.model.fits),
        width=6.5, height=6)

doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$figurecaption <- NULL 

#---- Stats Tutorial ----------------------------------------------------------------------------

# doc = addPageBreak(doc)

n3$paragraph <- paste("Stats Tutorial:")

doc = addParagraph(doc, n3$paragraph, stylename="Normal")


n3$paragraph <- paste("For each age-specific",
                   tolower(n3$stockabundance),
                   "time series, compare the fitted values produced by the naive model based on the average",   
                   tolower(n3$stockabundance),
                   "over the previous 3 years",
                   "against the observed (or historical) values of the",
                   tolower(n3$stockabundance),
                   "time series.  Does the naive model appear to provide",
                   "a good fit to the time series?",
                   sep=" ")

doc = addParagraph(doc, n3$paragraph, stylename="Normal")
                         
n3$paragraph <- NULL                  

#----- Naive Time Series Modeling Diagnostics (Average of Previous 3 Years): Checking Randomness of Residuals --

doc = addTitle(doc, "Modeling Diagnostics", level=1)

## Come back here!!
## plot.model.diagnostics.avgthree(avgthree.model.fits, i)


#---- Stats Tutorial ----------------------------------------------------------------------------

n3$paragraph <- paste("Stats Tutorial:")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$paragraph <- NULL 

n3$paragraph <- paste("After fitting a naive model to a univariate time series, ",
                   "where the model was based on the average ",
                   tolower(n3$stockabundance),
                   " over the previous 3 years,",
                   " we need to run diagnostic tests to validate the model.",
                   sep="")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n3$pots <- pot("If the naive model provides a good fit to a univariate time series, the residuals associated with the model should exhibit ") +
        pot("no systematic patterns and ") + 
        pot("no temporal dependence.")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
         
n3$paragraph <- NULL 
         
n3$pots <- pot("Useful diagnostic plots for verifying that the naive model residuals exhibit no systematic patterns and no temporal dependence include:")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$paragraph <- NULL 

n3$paragraph <- c("Time series plot of the model residuals;",
                "Autocorrelation plot of the model residuals;",
                "Partial autocorrelation plot of the model residuals;",
                "Plot of p-values associated with the Ljung-Box test applied to the model residuals.")
doc = addParagraph(doc, n3$paragraph, stylename="BulletList")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$pots <- pot("The Ljung-Box test is a diagnostic tool used to test the lack of fit of a naive model. ") + 
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
             
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal")
          
n3$pots <- NULL
n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
          
n3$pots <- NULL
n3$paragraph <- NULL 
          
n3$paragraph <- "If a naive model provides a good fit to a univariate time series, then:"
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$paragraph <- pot("The time series plot of the model residuals should exhibit no systematic patterns;")
n3$paragraph <- set_of_paragraphs(n3$paragraph)
doc = addParagraph(doc, n3$paragraph, stylename="BulletList")

n3$paragraph <- NULL 

n3$paragraph <- pot("The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals;")
n3$paragraph <- set_of_paragraphs(n3$paragraph)
doc = addParagraph(doc, n3$paragraph, stylename="BulletList")

n3$paragraph <- NULL 

n3$paragraph <- pot("The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;")
n3$paragraph <- set_of_paragraphs(n3$paragraph)
doc = addParagraph(doc, n3$paragraph, stylename="BulletList")

n3$paragraph <- NULL 

n3$paragraph <-  set_of_paragraphs(pot("The p-values associated with the Ljung-Box test should be large for all values of") +  
                                pot(" m ",  textProperties(font.style = "italic", font.size=10, font.family="Calibri")) + 
                                pot("considered."))
doc = addParagraph(doc, n3$paragraph, stylename="BulletList")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 
                   
#---- Model Diagnostic Plots

doc = addPageBreak(doc)

for (i in 1:length(n3$fits)) {

## fits[[i]]$age   gives you the age 
n3$figurecaption <- paste0("Model diagnostics for the naive model fit corresponding to the ",
                       tolower(n3$fits[[i]]$age), " component of the ", 
                       n3$stockname, " ", n3$stockspecies, " stock. ", 
                       "The naive model was based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years.")

n3$myplot <- n3$diagnostics.avgthree.model.fit(n3$fits,i)


doc = addPlot(doc,
            fun=plot,  # print,
            x=n3$myplot,
            width=plotwidth+1, height=plotheight)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 

}





#================================================================================================
# Forecasting Results
#================================================================================================

doc = addPageBreak(doc)

doc = addTitle(doc, "Forecasting Results", level=1)


n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$paragraph <- pot("This section reports the forecasting results for the ") + 
             pot(paste(n3$stockspecies)) + 
             pot(" ") + 
             pot(n3$stockname) + 
             pot(" stock ") + 
             pot("corresponding to the forecasting year ") + 
             pot(paste(n3$forecastingyear,". ", sep="")) + 
             pot("The results were produced via naive modeling based on the average ") + 
             pot(paste(tolower(n3$stockabundance)," over the previous 3 years.", sep="")) 
n3$paragraph <- set_of_paragraphs(n3$paragraph)
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$paragraph <- pot("Forecasting results are reported numerically and visually for two types of forecasts:")
n3$paragraph <- set_of_paragraphs(n3$paragraph)
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$paragraph <- c("   1) point forecasts;", "   2) interval forecasts.")
doc = addParagraph(doc, n3$paragraph, parent.type="ol", stylename="Normal")

n3$paragraph <- NULL                       

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n3$pots <- NULL 
n3$paragraph <- NULL 
                   
n3$paragraph <- pot("A point forecast is simply a number which represents our best guess ") +
             pot("of the future value of the age-specific or total ") + 
             pot(paste(tolower(n3$stockabundance))) + 
             pot(" for the stock of interest based on available historical data.")
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$paragraph <- pot("An interval forecast is not a single number, rather it is a range of values ") + 
             pot("in which we expect the future value of an age-specific or total ") + 
             pot(paste(tolower(n3$stockabundance))) + 
             pot(" series to fall with some (prespecified) probability.")
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n3$pots <- NULL 
n3$paragraph <- NULL 
                   
n3$paragraph <- pot("A couple of remarks are in order in connection with an interval forecast:") 
n3$paragraph <- set_of_paragraphs(n3$paragraph)
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$paragraph <- c(paste("The width of the interval forecast conveys information regarding forecast uncertainty", 
                     "(the wider the interval forecast, the more uncertain the forecast);"),
               "The interval forecast conveys more information than the associated point forecast.")
doc = addParagraph(doc, n3$paragraph, stylename="BulletList")
 
n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 
 
if (n3$bootmethod=="meboot") {
n3$paragraph <- pot("The interval forecast provided in this report for each ") + 
             pot(paste(tolower(n3$stockabundance))) + 
             pot(" time series was obtained by applying maximum entropy bootstrapping to that series. ") +
             pot("The maximum entropy bootstrap constructs a large number B (say, B=999) ") +
             pot("of replicates of a dependent time series using an algorithm designed ") + 
             pot("to satisfy the ergodic theorem (i.e., the grand mean of all replicates is close to the sample mean of the original time series). ") +
             pot("The algorithm can accommodate both stationary and non-stationary time series. ") +
             pot("The constructed replicates retain the basic shape (i.e., local peaks and troughs) of the original time series. ") + 
             pot("They also retain the time dependence structure of the autocorrelation function (ACF) ") +
             pot("and the partial autocorrelation function (PACF) of the original time series.")
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")
                
n3$paragraph <- NULL     

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n3$pots <- NULL 
n3$paragraph <- NULL 
                   
n3$paragraph <- pot("Given a ") + 
             pot(paste(tolower(n3$stockabundance))) + 
             pot(" time series and the B maximum entropy bootstrap replicates of the series, the 80% interval forecast ") + 
             pot("for the next (future) value of the series is obtained by following the steps below: ") 
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")
                
n3$paragraph <- NULL 
                        
n3$paragraph <- c("   a) Apply naive forecasting to each of the B maximum entropy bootstrap replicates in order to forecast the next (future) value of that replicate;",
               "   b) Compute the 10% percentile (P.10) and the 90% percentile (P.90) of the distribution of the B forecasted values derived in the previous step;",
               "   c) Set the lower and upper limits of the 80% interval forecast to be P.10 and P.90, respectively.")
doc = addParagraph(doc, n3$paragraph, parent.style="ol",stylename="Normal")
          
n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))     
              
n3$pots <- NULL        
n3$paragraph <- NULL 
                           
n3$paragraph <- pot("For interval forecast with a negative lower limit, ") +  
             pot("the lower limit is set to zero to ensure the interval forecast includes only positive values.")
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 
                   
                   
} 



if (n3$bootmethod=="stlboot"){

n3$paragraph <- pot("The interval forecast provided in this report for each ") + 
             pot(paste0(tolower(n3$stockabundance), " time series ")) + 
             pot("age-specific or total) ") + 
             pot(" was obtained by applying loess bootstrapping to that series.")
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$paragraph <- pot("The loess bootstrapping is a time series bootstrapping method introduced by Bergmeir, Hyndman and Benitez in 2014 in their working paper on ") +
             pot("bagging exponential smoothing methods using the STL decomposition and the Box-Cox transformation. ") + 
             pot("In this method, the time series of annual abundance values which needs to be bootstrapped is first transformed via a Box-Cox transformation. ") +
             pot("The transformed time series is then decomposed into its trend and remainder components using the loess method ") +
             pot("(i.e., a smoothing method based on local linear regression). ") +
             pot("Finally, the remainder component is bootstrapped using the moving block bootstrap (MBB), ") +
             pot("the trend and seasonal components are added back, and the Box-Cox transformation is inverted. ") +
             pot("In this way, a random pool of B similar bootstrapped time series is generated from the original time series.")
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")             

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$paragraph <- pot("Given a ") + 
             pot(paste(tolower(n3$stockabundance))) + 
             pot(" time series and the B loess bootstrap replicates of the series, the 80% interval forecast ") + 
             pot("for the next (future) value of the series is obtained by following the steps below: ") 
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")
          
n3$paragraph <- NULL 
          
n3$paragraph <- c("   a) Apply naive forecasting to each of the B loess bootstrap replicates in order to forecast the next (future) value of that replicate;",
               "   b) Compute the 10% percentile (P.10) and the 90% percentile (P.90) of the distribution of the B forecasted values derived in the previous step;",
               "   c) Set the lower and upper limits of the 80% interval forecast to be P.10 and P.90, respectively.")
doc = addParagraph(doc, n3$paragraph, parent.style="ol",stylename="Normal")
               
n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
               
n3$pots <- NULL 
n3$paragraph <- NULL 
               
n3$paragraph <- pot("For interval forecast with a negative lower limit, ") +  
             pot("the lower limit is set to zero to ensure the interval forecast includes only positive values.")
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$paragraph <- pot("Note that, if the historical abundance data for a specific age class includes too many 0 and/or 1 values, ") +  
             pot("the loess bootstrapping for that age will fail, in the sense of producing bootstrapping point forecasts whose values are huge and thereby implausible. ") + 
             pot("Currently, such situations are handled by reverting to the maximum entropy bootstrap for the problematic age(s).")   
n3$paragraph <- set_of_paragraphs(n3$paragraph)             
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n3$pots <- NULL 
n3$paragraph <- NULL 

}

#----- Point Forecasts --------------------------------------------------------------------------      

doc = addTitle(doc, "Point Forecasts", level=2)


n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$tabledata <- n3$results.point.forecast.avgthree    ## this object is created in the review code file

n3$tabledata[nrow(n3$tabledata)+1, ] <- n3$tabledata[nrow(n3$tabledata), ]

n3$tabledata <- transform(n3$tabledata, Age = as.character(Age))

str(n3$tabledata)

n3$tabledata[nrow(n3$tabledata), "Age"] <-  "Total"
n3$tabledata[nrow(n3$tabledata), "Model"] <-  ""
n3$tabledata <- transform(n3$tabledata, p = round(p))
n3$tabledata[nrow(n3$tabledata), "p"] <-  sum(n3$tabledata[1:(nrow(n3$tabledata)-1), "p"])

usePackage("scales")
n3$tabledata <- transform(n3$tabledata, p = comma(p))


names(n3$tabledata)[names(n3$tabledata)=="Age"] <- "Terminal Run"
names(n3$tabledata)[names(n3$tabledata)=="Model"] <- "Model"
names(n3$tabledata)[names(n3$tabledata)=="RY"] <- "Forecasting Year"
names(n3$tabledata)[names(n3$tabledata)=="p"] <- "Point Forecast"

n3$tabledata$Model

usePackage("stringr")
n3$tabledata$Model <- str_replace_all(n3$tabledata$Model, "Past", "Previous")

n3$tablecaption <- paste("Point forecasts of the ",
                      max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                      " age-specific and total",
                      " ",
                      paste0(tolower(n3$stockabundance),"s",collapse=" "),
                      " for the ",
                      n3$stockname,
                      " ",
                      tolower(n3$stockspecies),
                      " stock. ",
                      "The point forecasts for the age-specific ",
                      tolower(n3$stockabundance),
                      " were produced via naive models based on the average ",
                      paste0(tolower(n3$stockabundance)),
                      " over the previous 3 years. ",
                      "The point forecast for the total ",
                      tolower(n3$stockabundance),
                      " was obtained by totaling the age-specific point forecasts produced by these naive models. ",
                      "All point forecasts were rounded to the nearest integer for reporting purposes.",
                      sep="")


doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")
    
n3$MyFTable = FlexTable(data=n3$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
    
doc = addFlexTable(doc, n3$MyFTable)
   
n3$MyFTable <- NULL        
n3$tablecaption <- NULL 

#----- Barplot of historical abundance values and associated point forecast: Individual Ages ---------------------------------------

n3$fits <- n3$avgthree.model.fits

n3$pointforecasts <- n3$point.forecast.avgthree(n3$datalist, n3$avgthree.model.fits)

for (i in 1:length(n3$pointforecasts)){

  print(i)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = plot,   # print,
        x = n3$barplot.forecasted.values.individual.ages.avgthree(n3$fits, n3$pointforecasts,i),
        width=6.5, height=6)

   n3$age <- n3$avgthree.model.fits[[i]]$age
   n3$age <- tolower(n3$age)

   n3$figurecaption <- paste("Historical ",
                       #terminal run
                       tolower(n3$stockabundance),
                       " values and ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " point forecast ",
                       "corresponding to the ", n3$age, " component of the ",
                       # "terminal run ,
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The ",  max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " point forecast was derived via the naive model based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years.",
                       sep="")


   doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

   n3$figurecaption <- NULL 

}

 
#----- Barplot of historical abundance values and associated point forecast: Total Age ---------------------------------------

doc = addPageBreak(doc)

n3$results <- n3$results.total.age.retro.predictive.performance.avgthree
pointforecasts <- n3$point.forecast.avgthree(n3$datalist, n3$avgthree.model.fits)

doc = addPlot(doc, 
              fun = plot, # print,
              x = n3$barplot.forecasted.values.total.age.avgthree(n3$results, n3$pointforecasts),
              width=6.5, height=6)
              
n3$figurecaption <- paste("Historical total ",
                       tolower(n3$stockabundance),
                       " values",
                       " and corresponding ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The",
                       " point forecast of total ",
                       tolower(n3$stockabundance),
                       " was obtained by totaling the point forecasts of the age-specific ",
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " produced by",
                       " the naive models based on the average ",
                       tolower(n3$stockabundance),
                       " over the previous 3 years.",
                       sep="")

doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$figurecaption <- NULL 


#---- Forecast Intervals ------------------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, "Interval Forecasts", level=2)


n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n3$pots <- NULL 
n3$paragraph <- NULL 

n3$tabledata <- rbind(n3$PI.individual.ages.avgthree, n3$PI.total.age.avgthree)

n3$tabledata

## n3$tabledata <- subset(n3$tabledata, select=-PI.med)

n3$tabledata$PI <- paste( n3$tabledata[,"PI.lwr"]," - ", n3$tabledata[,"PI.upr"], sep="")

n3$tabledata <- subset(n3$tabledata, select=-PI.lwr)
n3$tabledata <- subset(n3$tabledata, select=-PI.upr)

names(n3$tabledata)[names(n3$tabledata)=="PI.ctr"] <- "Point Forecast"
names(n3$tabledata)[names(n3$tabledata)=="PI"] <- "Interval Forecast"

n3$nms <- c(as.character(n3$results.point.forecast.avgthree$Age), "Total")
n3$mds <- c(as.character(n3$results.point.forecast.avgthree$Model), "")
n3$yr <-  c(n3$results.point.forecast.avgthree$RY, unique(n3$results.point.forecast.avgthree$RY))

n3$tabledata <- data.frame(nms=n3$nms, mds=n3$mds, yr=n3$yr, n3$tabledata)

names(n3$tabledata)[names(n3$tabledata)=="nms"] <- "Terminal Run"
names(n3$tabledata)[names(n3$tabledata)=="mds"] <- "Model"
names(n3$tabledata)[names(n3$tabledata)=="yr"] <- "Return Year"
names(n3$tabledata)[names(n3$tabledata)=="Point.Forecast"] <- "Point Forecast"
names(n3$tabledata)[names(n3$tabledata)=="Interval Forecast"] <- "Interval Forecast"

usePackage("stringr")
n3$tabledata$Model <- str_replace_all(n3$tabledata$Model, "Past", "Previous")

if (n3$bootmethod=="stlboot") {
   n3$tabletext <- paste0(" Interval forecasts were obtained by loess bootstrap.")
}

if (n3$bootmethod=="meboot") {
   n3$tabletext <- paste0(" Interval forecasts were obtained by maximum entropy bootstrap.")
}

n3$tablecaption <- paste("Point forecasts and associated 80% interval forecasts of the ",
                      max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                                              # " terminal run ",
                      " ",
                      tolower(n3$stockabundance),
                      " for the ",
 				              n3$stockname,
                		  " ",
                		  tolower(n3$stockspecies),
                		  " stock.",
                      # min(ages)," - ",max(ages),
                      #     " and the total age. ",
                          ## " Negative lower limits for the interval forecasts were truncated to zero to ",
                          ## "ensure the interval forecasts only include positive values. ",
                          " The point forecasts for age-specific components of ",
                          # terminal run
                          tolower(n3$stockabundance),
                          " were obtained via naive models based on the average ",
                          # terminal run
                          tolower(n3$stockabundance),
                          " over the previous 3 years.",
                          " The point forecast for total ",
                          # terminal run
                          tolower(n3$stockabundance),
                          " was obtained by adding up the point forecasts for the age-specific components of ",
                          # terminal run
                          tolower(n3$stockabundance),
                          " produced by the naive models.",
                          n3$tabletext,
                          sep="")

doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")
    
n3$MyFTable = FlexTable(data=n3$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
    
doc = addFlexTable(doc, n3$MyFTable)

n3$MyFTable <- NULL 
n3$tablecaption <- NULL 

### Visualization of forecast intervals - scatterplots (individual ages)

doc = addPageBreak(doc)

for (i in 1:length(n3$avgthree.model.fits)){

    if (n3$bootmethod=="stlboot"){
       n3$figuretext <- " The interval forecast was obtained by loess bootstrap."
    }
    
    if (n3$bootmethod=="meboot"){
       n3$figuretext <- " The interval forecast was obtained by maximum entropy bootstrap."
    }
    
    
    n3$figurecaption <- paste("Historical ",
                           # terminal run
                           tolower(n3$stockabundance),
                           " values along with the ",
                            max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                          " point forecast and 80% interval forecast of the ",
                       # " terminal run",
                        tolower(n3$stockabundance),
                       " corresponding to the ",
                       tolower(n3$avgthree.model.fits[[i]]$age),
                       " component of the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The point forecast was obtained via the naive model based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years. ",
                       n3$figuretext,
                       sep="")

   
       n3$fits <- n3$avgthree.model.fits
       n3$pointforecasts <- n3$point.forecast.avgthree(n3$datalist, n3$avgthree.model.fits)
       n3$intervalforecasts <-   n3$PI.individual.ages.avgthree.no.comma

       n3$myplot <- n3$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.avgthree(n3$fits, n3$pointforecasts, n3$intervalforecasts,i)

    
       
       doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth+1, height=plotheight-2)
            
        doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

        n3$myplot <- NULL 

        n3$figuretext <- NULL

}



### Visualization of forecast intervals - scatterplot(total age)

if (n3$bootmethod=="stlboot") {
    n3$figuretext <- " The interval forecast was obtained by loess bootstrap."
}

if (n3$bootmethod=="meboot") {
    n3$figuretext <- " The interval forecast was obtained by maximum entropy bootstrap."
}

n3$figurecaption <- paste("Historical total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " values ",
                       # "(", "ages ", min(ages), " - ", max(ages), ")",
                       "and corresponding ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " point forecast and 80% forecast interval for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The point forecast for total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       # terminal run",
                        tolower(n3$stockabundance),
                       " produced by the naive models based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years. ",
                       n3$figuretext,
                       sep="")


n3$results <- n3$results.total.age.retro.predictive.performance.avgthree

n3$pointforecasts <- n3$point.forecast.avgthree(n3$datalist, n3$avgthree.model.fits)

n3$intervalforecasts <-  n3$PI.total.age.avgthree.no.comma

n3$myplot <- n3$scatterplot.forecasted.values.and.forecast.intervals.total.age.avgthree(n3$results, n3$pointforecasts, n3$intervalforecasts)



doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 

n3$figuretext <- NULL


#--- Bootstrap Distribution of Point Forecasts - Age-Specific Components

doc = addPageBreak(doc)

if (n3$bootmethod=="stlboot"){
     n3$figuretext <- " stock, derived on the basis of loess bootstrapping for the forecasting year "
}

if (n3$bootmethod=="meboot"){
    n3$figuretext <- " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year "
}

n3$figurecaption <- paste("Distributions of bootstrapped forecasted ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " values ",
                       "for specific age components of the ",
                       paste(n3$stockname," ", tolower(n3$stockspecies),
                       n3$figuretext,
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       ".",
                       " The dashed red line indicates the position of the point forecast on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecasts were obtained via naive modeling based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years.",
                       sep="")


n3$myplot <- n3$plot.distribution.bootstrapped.point.forecasts.individual.ages.avgthree(n3$PI.individual.ages.avgthree.sim,
                                                                        n3$PI.individual.ages.avgthree.no.comma, n3$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figuretext <- NULL
n3$figurecaption <- NULL 

#--- Bootstrap Distribution of Point Forecasts - Total Age

if (n3$bootmethod=="stlboot"){
    n3$figuretext <- " stock, derived on the basis of loess bootstrapping for the forecasting year "
}

if (n3$bootmethod=="meboot"){
    n3$figuretext <- " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year "
}

n3$figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " corresponding to the ",
                       paste(n3$stockname," ", tolower(n3$stockspecies),
                       n3$figuretext, 
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       ".",
                       " The dashed red line indicates the position of the point forecast of ",
                       tolower(n3$stockabundance),
                       " on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ",
                       tolower(n3$stockabundance),  
                       "was obtained by adding up ", 
                       "the point forecasts for the age-specific components of ",
                       tolower(n3$stockabundance), 
                       " produced by the naive models based on the average ", 
                       paste(tolower(n3$stockabundance)), 
                       " over the previous 3 years.",
                       sep="")

n3$myplot <- n3$plot.distribution.bootstrapped.point.forecasts.total.age.avgthree(n3$PI.total.age.avgthree.sim, 
                                                                            n3$PI.total.age.avgthree.no.comma, 
                                                                            n3$stockabundance)

doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth, height=plotheight-3)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL
n3$figuretext <- NULL 
n3$figurecaption <- NULL 

#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###


###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

n3$empirical.probability.yboot.avgthree.total.age <- function(PI.total.age.avgthree.sim, PI.total.age.avgthree.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.avgthree.sim
     mylabel <- paste("Total", n3$stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.avgthree.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


n3$emp.prob.avgthree.total.age <- n3$empirical.probability.yboot.avgthree.total.age(n3$PI.total.age.avgthree.sim, 
                                                                              n3$PI.total.age.avgthree.no.comma, 
                                                                              n3$stockabundance)



n3$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(n3$stockabundance)," ",
                       "value yet to be observed in ",
                       n3$forecastingyear, " for the ",
                       n3$stockname," ",
                       n3$stockspecies, " stock",
                        " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(n3$stockabundance), ".")

doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")

n3$tt_1 <- n3$emp.prob.avgthree.total.age$prob.thresholds

n3$tt_2 <- n3$emp.prob.avgthree.total.age$prob.point.forecast

n3$tt_1_and_2 <- rbind.data.frame(n3$tt_1, n3$tt_2)

usePackage("plyr")

## n3$tt_arrange <- arrange(n3$tt_1_and_2, prob.threshold)

n3$tt_arrange <- n3$tt_1_and_2[order(n3$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

n3$from_tmp = which(n3$tt_arrange[,1] == n3$emp.prob.avgthree.total.age$prob.point.forecast$prob.threshold)
n3$tt_arrange[n3$from_tmp, 4] <- n3$tt_arrange[n3$from_tmp + 1, 4]

n3$tt_arrange[,1] <- comma(n3$tt_arrange[,1])
n3$tt_arrange[,2] <- paste0(n3$tt_arrange[,2],"%")
n3$tt_arrange[,3] <- paste0(n3$tt_arrange[,3],"%")
n3$tt_arrange[,4] <- paste0(n3$tt_arrange[,4],"%")


names(n3$tt_arrange)[1] <- "Threshold"
names(n3$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(n3$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(n3$tt_arrange)[4] <- "Interval Probability"
n3$tt_arrange[1,4] <- "-"


n3$my_ft <- FlexTable( data = n3$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
n3$my_ft[, 1:ncol(n3$tt_arrange)] = parProperties(text.align = "right")

## n3$my_ft[n3$tt_arrange$Threshold %in% comma(n3$emp.prob.avgthree.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )
## Mistake?

n3$my_ft = spanFlexTableRows(n3$my_ft, j=4, from = n3$from_tmp, to = n3$from_tmp + 1)

n3$my_ft[n3$tt_arrange$Threshold %in% comma(n3$emp.prob.avgthree.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

doc = addFlexTable(doc, flextable=n3$my_ft)

n3$my_ft <- NULL 
n3$tt_arrange <- NULL 
n3$tt_1 <- NULL
n3$tt_2 <- NULL
n3$tt_1_and_2 <- NULL 
n3$tablecaption <- NULL 

#---- Forecast Performance Measures ---------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, value="Retrospective Evaluation of Performance of Point Forecasts", level=1)
  

n3$pots <- pot(" ")
n3$paragraph <- set_of_paragraphs(n3$pots)
doc = addParagraph(doc, n3$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
  
n3$pots <- NULL 
n3$paragraph <- NULL 
  
n3$paragraph <- paste("This section reports the results corresponding to the retrospective evaluation of the performance of the",
                       "point forecasts produced",
                       "by the naive models for the", n3$forecastingyear, "age-specific and total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       "corresponding to the",
                       n3$stockname, n3$stockspecies, "model stock.",
                       "The naive models were based on the average ",  
                       paste(tolower(n3$stockabundance)),
                       "over the previous 3 years.",
                      sep=" ")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$paragraph <- paste("The retrospective evaluation of the performance of the point forecasts assessed how well the naive model ",
                       "performed when used for retrospective forecasting of the historical ",
                       tolower(n3$stockabundance), " values. ",
                       "For this evaluation, the naive model was fit to all of the historical ",
                       tolower(n3$stockabundance), " values ",
                       "available prior to a given historical return year, then the fitted model was used ",
                       "to forecast the ",
                       # terminal run
                       tolower(n3$stockabundance),
                       "for that year. ",
                       "This evaluation captures how well the model would have performed in practice year over year ",
                       "and was performed separately for each age-specific ",
                        tolower(n3$stockabundance),
                        " and for the total ",
                        tolower(n3$stockabundance),
                        ".",
                      sep="")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$paragraph <- paste("Retrospective forecast errors were defined as the actual",
                       # terminal run
                       tolower(n3$stockabundance),
                       "values minus the retrospectively forecasted",
                       # "terminal run values.",
                        tolower(n3$stockabundance),
                       "values.",
                       "In view of this definition, positive values for the retrospective forecast errors represent forecasts that were too low,",
                       "whereas negative values represent forecasts that were too high.",
                      sep=" ")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$paragraph <- "The following retrospective measures were used to characterize different aspects of the retrospective forecasting errors:"
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 
    
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


n3$paragraph <- paste("MAE and MAPE reflect the overall forecast accuracy accounting for systematic bias and year-to-year variation.")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 
        
n3$paragraph <- paste("MRE and MPE reflect directional bias in raw and relative forecast errors, respectively, with negative values indicating a tendency to",
                       "underforecast and positive values reflecting a tendency to overforecast.",
                      sep=" ")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$paragraph <- paste("Just like MAE, RMSE  is a measure of the absolute magnitude of the raw retrospective forecast errors, but is more sensitive to large values",
                       "then MAE.", sep=" ")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 
    
n3$paragraph <- paste("MASE was proposed by Hyndman and Koehler (2006) as a generally applicable, scale-free measure of forecast accuracy.",
                       "This measure never gives infinite or undefined values.",
                       "In this report, MASE is computed as the average of the absolute values of the scaled retrospective forecast errors produced by",
                       "the naive model based on the",
                       # terminal run
                       tolower(n3$stockabundance),
                       "from the previous year.",
                       "The scaling of the errors involves dividing the errors by the MAE computed from the retrospective forecast errors associated with",
                       "this model.",  
                       # based on the",
                       # terminal run
                       # tolower(n3$stockabundance),
                       # "from the previous year.",
                       # "from the one-step, naive forecasting method.",
                       # "A scaled error is less than 1 if it arises from a better forecast than the one produced by the naive model based on the terminal run for the previous year.",
                       # "Conversely, it is greater than 1 if the forecast is worse than the average one-step, naive forecast computed in-sample.",
                       "The resulting value of MASE will be equal to 1, since the naive model based on the",
                       tolower(n3$stockabundance),
                       "from the previous year",  
                       "is used as a benchmark against which all other forecasting models will be compared in terms of their",
                       "retrospective forecasting performance (as captured by the MASE).",
                      sep=" ")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$paragraph <- paste0("To facilitate the interpretation of the retrospective forecast errors, this section reports several types of plots",
                    " for the age-specific and total ", tolower(n3$stockabundance), "s",  
                     ":")  
doc = addParagraph(doc, n3$paragraph, stylename="Normal")
  
n3$paragraph <- NULL 
  
doc = addParagraph( doc, value = 'Plots illustrating the performance of the retrospective forecasting evaluation;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")      
doc = addParagraph( doc, value = 'Density plots of the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Bias coefficient plots derived from the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")          
doc = addParagraph( doc, value = 'Barplots of the retrospective forecast errors together with the forecast interval corresponding to the forecasting year of interest.',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
  
  
## paragraph <- paste("We also calculated the coefficient of determination obtained by regressing the retrospectively forecasted",
##                       tolower(n3$stockabundance),
##                       "values",
##                       "on the historical",
##                       # terminal run
##                       tolower(n3$stockabundance),
##                       "values.",
##                       "This is simply the squared correlation coefficient of the retrospectively forecasted and historical",
##                       # terminal run
##                        tolower(n3$stockabundance),
##                       "values.",
##                      sep=" ")
## doc = addParagraph(doc, paragraph, stylename="Normal")


n3$paragraph <- paste0("Bias coefficients representing a new metric for forecast bias. ", 
                   "These coefficients are computed from the retrospective forecast errors for the age-specific and total ",
                   tolower(n3$stockabundance),"s", 
                   " using the formula developed by Kourentzes, Trapero and Svetunkov in their 2014 working paper ", 
                   "\"Measuring the behaviour of experts on demand forecasting: a complex task\". ", 
                   "In the context of this report, the bias coefficients describe the direction and magnitude",
                   " of the retrospective forecast bias ", 
                   "associated with the naive forecasting method (average of previous 3 years).")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")

n3$paragraph <- NULL 

n3$paragraph <- paste0("Generally speaking, the bias coefficients are unit-free and bounded between -1 (maximum negative retrospective forecast bias) ",  
                    "and 1 (maximum positive retrospective forecast bias). ", 
                    "A forecasting method that is always producing retrospective point forecasts which are over the observed historical values ", 
                    "will have a bias coefficient equal to -1, always over-forecasting. ", 
                    "A forecasting method that is always producing retrospective point forecasts which are under the observed historical values will ", 
                    "have a bias coefficient equal to 1, always under-forecasting. Given the bounded nature of the bias coefficient, ", 
                    "we can describe a forecasting method as strongly biased if |bias coefficient| > 0.5 and weakly biased if 0 < |bias coefficient| <= 0.5, ", 
                    "providing a simple and intuitive description of the forecast bias behaviour. ", 
                    "If the bias coefficient is equal to 0, the forecasting method is unbiased.")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")
                    
n3$paragraph <- NULL 

#---- Retrospective Measures of Forecast Performance -------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, value="Retrospective Measures of Forecast Performance", level=2)

n3$tabledata <- n3$M.avgthree

n3$tabledata <- subset(n3$M.avgthree, select=-Model)

n3$tabledata <- cbind(Model=rep("Naive (Average of Previous 3 Years)",nrow(n3$M.avgthree)),n3$tabledata)


n3$tablecaption <- paste("Retrospective measures of forecast performance ",
                "associated with the point forecasts of the ",
                max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                " age-specific and total ",
                # terminal runs
                paste0(tolower(n3$stockabundance),"s",collapse=" "),
                " for the ",
                n3$stockname,
                " ",
                tolower(n3$stockspecies),
                " stock.",
                " The point forecasts of age-specific ",
                # terminal runs
                paste0(tolower(n3$stockabundance),"s",collapse=" "),
                " were obtained via naive modeling based on the average ",
                # terminal run
                tolower(n3$stockabundance),
                " over the previous 3 years.",
                " The point forecast for the total ",
                # terminal run
                 tolower(n3$stockabundance),
                " was obtained by adding up the point forecasts for the age-specific components of ",
                # terminal run",
                 tolower(n3$stockabundance),
                " produced by the naive models based on the ",
                # terminal run
                 tolower(n3$stockabundance),
                " from the previous year.",
                sep=""
                )

names(n3$tabledata)[names(n3$tabledata)=="Model"] <- "Model Class"

doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

n3$tt <- n3$tabledata

usePackage("stringr")
names(n3$tt) <- str_replace_all(names(n3$tt),"_"," ")

## tt[,-1] <- comma(tt[,-1])

n3$my_ft <- FlexTable( data = n3$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
n3$my_ft[, 1:ncol(n3$tt)] = parProperties(text.align = "right")

n3$my_ft = addFooterRow( n3$my_ft, value = paste("Legend:  MRE = Mean Relative Error; ", 
                                           "MAE = Mean Absolute Error; ", 
                                           "MPE = Mean Percentage Error; ", 
                                           "MAPE = Mean Absolute Percentage Error; ",
                                           "MASE = Mean Scaled Error; ",
                                           "MSE = Root Mean Square Error."), 
        colspan = ncol(n3$tt), 
        text.properties = textProperties(font.size = 9),
        par.properties = parProperties(text.align = "left"))
        
## my_ft = addFooterRow( my_ft, value = c("         MAE = Mean Absolute Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MPE = Mean Percentage Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MAPE = Mean Absolute Percentage Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MASE = Mean Scaled Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         RMSE = Root Mean Square Error."), colspan = ncol(tt))


doc = addFlexTable(doc, flextable=n3$my_ft)
          
n3$my_ft <- NULL 
n3$tt <- NULL 
n3$tabledata <- NULL 
n3$tablecaption <- NULL 
          
#---- Retrospective Forecast Errors:  Individual Ages ------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, "Retrospective Point Forecasts and Forecast Errors", level=2)


for (i in 1:length(n3$results.individual.ages.retro.predictive.performance.avgthree)) {

    print(i)

    n3$results <- n3$results.individual.ages.retro.predictive.performance.avgthree

    n3$tabledata <- n3$results[[i]]$data.retro

   
    n3$age <- names(n3$results)[i]
    usePackage("stringr")
    n3$age <- str_extract(n3$age,"[[:digit:]]+")
    
    n3$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the age ", n3$age,
                          " component of the ",
 				              max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                		  # " terminal run for the ",
                		  " ",
                      tolower(n3$stockabundance),
                      " for the ",
                      n3$stockname,
                		  " ",
                		  tolower(n3$stockspecies),
                		  " stock. ",
                          "Accompanying return years and actual ",
                     # terminal run
                     tolower(n3$stockabundance),
                     " values are also reported.",
                      " The retrospective point forecasts were obtained via naive modeling based on the average ",
                     # terminal run
                     tolower(n3$stockabundance),
                     " over the previous 3 years.",
                          sep="")

    n3$tabledata$p <- round(n3$tabledata$p,2)
    n3$tabledata$e <- round(n3$tabledata$e,2)

    usePackage("scales")
    n3$tabledata$a <- comma(round(n3$tabledata$a))
    n3$tabledata$p <- comma(round(n3$tabledata$p))
    n3$tabledata$e <- comma(round(n3$tabledata$e))

    names(n3$tabledata)[names(n3$tabledata)=="cy"] <- "Return Year"
    names(n3$tabledata)[names(n3$tabledata)=="a"] <-  "Actual"
    names(n3$tabledata)[names(n3$tabledata)=="p"] <-  "Forecast"
    names(n3$tabledata)[names(n3$tabledata)=="e"] <-  "Error"
    ## names(tabledata)[names(tabledata)=="p.bench"] <-  "Benchmark Forecast"
    ## names(tabledata)[names(tabledata)=="e.bench"] <-  "Benchmark Error"


    n3$tabledata
    
    n3$tabledata <- subset(n3$tabledata, select=c(-p.bench, -e.bench))
    
    doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")

    baseCellProp = cellProperties( padding = 4)

    n3$tt <- n3$tabledata


    usePackage("stringr")
    names(n3$tt) <- str_replace_all(names(n3$tt),"_"," ")

    ## tt[,-1] <- comma(tt[,-1])

    n3$my_ft <- FlexTable( data = n3$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
    )

    # overwrites some paragraph formatting properties
    n3$my_ft[, 1:ncol(n3$tt)] = parProperties(text.align = "right")

    doc = addFlexTable(doc, flextable=n3$my_ft)
    
    n3$my_ft <- NULL 
    n3$tabledata <- NULL 
    n3$tablecaption <- NULL 
    
    doc = addPageBreak(doc)
    
}

    
#---- Retrospective Forecast Errors:  Total Age ------------------------------------------------------------


n3$results <- n3$results.total.age.retro.predictive.performance.avgthree

n3$tabledata <- data.frame(cy = n3$results$data.retro[[1]]$cy,
                        a = n3$results$a.total.retro,
                        p = n3$results$p.total.retro,
                        e = n3$results$e.total.retro)


n3$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the ",
 				              max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                		  " total ",
                      # terminal run
                      tolower(n3$stockabundance),
                      " for the ",
                		  n3$stockname,
                		  " ",
                		  tolower(n3$stockspecies),
                		  " stock. ",
                          "Accompanying return years and actual total ",
                      # terminal run
                      tolower(n3$stockabundance),
                      " values are also reported.",
                      " The retrospective point forecasts for the total ",
                      # terminal run
                       tolower(n3$stockabundance),
                      " were obtained by adding up the retrospective point forecasts for the age-specific components of ",
                      # terminal run
                      tolower(n3$stockabundance),
                      " produced by the naive models based on the average ",
                      # terminal run
                       tolower(n3$stockabundance),
                      " over the previous 3 years.",
                          sep="")

usePackage("scales")
n3$tabledata$a <- comma(round(n3$tabledata$a))
n3$tabledata$p <- comma(round(n3$tabledata$p))
n3$tabledata$e <- comma(round(n3$tabledata$e))

names(n3$tabledata)[names(n3$tabledata)=="cy"] <- "Return Year"
names(n3$tabledata)[names(n3$tabledata)=="a"] <-  "Actual"
names(n3$tabledata)[names(n3$tabledata)=="p"] <-  "Forecast"
names(n3$tabledata)[names(n3$tabledata)=="e"] <-  "Error"


doc = addParagraph(doc, value=n3$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

n3$tt <- n3$tabledata


usePackage("stringr")
names(n3$tt) <- str_replace_all(names(n3$tt),"_"," ")


## tt[,-1] <- comma(tt[,-1])

n3$my_ft <- FlexTable( data = n3$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
n3$my_ft[, 1:ncol(n3$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=n3$my_ft)

n3$my_ft <- NULL 
n3$tablecaption <- NULL 
n3$tabledata <- NULL 


#---- Illustration of how well the retrospective forecast evaluation works: Individual ages ----------

for (j in 1:length(n3$avgthree.model.fits)) {

  print(j)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = print,
        x = n3$individual.ages.retro.plot.avgthree(n3$individual.ages.retro.plot.info.avgthree, n3$stockabundance, j),
        width=plotwidth, height=plotheight)

  
  n3$figurecaption  <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                       tolower( n3$avgthree.model.fits[[j]]$age),
                       " component of ",
                       # max(n3$results.afe.total.age.retro.naiveone$CY)+1,
                       # " terminal run
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock,"),
                       " derived via naive modeling based on the average of ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " from the previous 3 years. ",
                       "Historical values of ",  
                       tolower( n3$avgthree.model.fits[[j]]$age), " ",  
                       tolower(n3$stockabundance),
                       " (grey lines) and fitted values produced by the naive modeling (red lines)",
                       " are also shown. ", 
                       "Each panel corresponds to a particular retrospective forecasting year.",   
                       sep="")


  doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

  n3$figurecaption <- NULL 

}


#---- Illustration of how well the retrospective forecast evaluation works: Total age ----------

doc = addPageBreak(doc)

doc = addPlot(doc, 
        fun = print,
        x = n3$total.age.retro.plot.avgthree(n3$total.age.retro.plot.info.avgthree, n3$stockabundance),
        width=plotwidth, height=plotheight)

n3$figurecaption  <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                        "total ", 
                       # max(n3$results.afe.total.age.retro.naiveone$CY)+1,
                       # " terminal run
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock,"),
                       " derived via naive modeling based on the average of total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " from the previous three years. ",
                       "Historical values of total ",    
                       tolower(n3$stockabundance),
                       " (grey lines) and fitted values produced by the naive modeling (red lines)",
                       " are also shown. ", 
                       "Each panel corresponds to a particular retrospective forecasting year.",   
                       sep="")

doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$figurecaption <- NULL 

                    
#---- Retrospective Forecast Errors: Age-Specific Density Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

n3$figurecaption <- paste("Density plots of the retrospective forecast errors derived from the naive models used to forecast ",
                       "specific age components of the ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " ",
                       # " terminal run for the ",
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The naive models were based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years.",
                       sep="")

n3$myplot <- n3$dens.results.afe.individual.ages.retro.avgthree(
               n3$avgthree.model.fits,
               n3$results.individual.ages.retro.predictive.performance.avgthree
               )

doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 
   
#---- Retrospective Forecast Errors: Total Age Density Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

n3$figurecaption <- paste("Density plot of the retrospective forecast errors ",
                       "involved in forecasting the total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       sep="")


n3$myplot <- n3$dens.results.afe.total.age.retro.avgthree(n3$results.afe.total.age.retro.avgthree)

doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth, height=plotheight-3)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 


#---- Retrospective Forecast Errors: Age-Specific Bias Coefficient Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

n3$figurecaption <- paste("Bias coefficient plots obtained from the retrospective forecast errors derived from the naive models used to forecast ",
                       "specific age components of the ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " ",
                       # " terminal run for the ",
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The naive models were based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " from the previous 3 years.",
                       sep="")

n3$myplot <- n3$bias.coefficients.afe.individual.ages.retro.avgthree(n3$avgthree.model.fits,
                                                     n3$results.individual.ages.retro.predictive.performance.avgthree,
                                                     n3$stockabundance)
                                                        
doc = addPlot(doc,
            fun=grid.draw,
            x=n3$myplot,
            width=plotwidth, height=plotheight+1)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 
            
#---- Retrospective Forecast Errors: Total Age Bias Coefficient Plot ---------------------------------------------------------------

doc = addPageBreak(doc)

n3$figurecaption <- paste("Bias coefficient plot obtained from the retrospective forecast errors derived from the naive models used to forecast the ",
                       max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock."),
                       " The naive models were based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " from the previous 3 years.",
                       sep="")

n3$myplot <- n3$bias.coefficient.afe.total.age.retro.avgthree(n3$results.total.age.retro.predictive.performance.avgthree,
                                               n3$stockabundance)
                                                        
doc = addPlot(doc,
            fun=grid.draw,
            x=n3$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 


#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Specific Ages ----------

n3$results.retro <- n3$results.individual.ages.retro.predictive.performance.avgthree 
n3$results.pred <- n3$pred.int.individual.ages.avgthree
 
for (i in 1:length(n3$avgthree.model.fits)) {

  print(i)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = print,
        x = n3$gary.plot.individual.ages(n3$results.retro, n3$results.pred, i),
        width=6.5, height=6)

  if (n3$bootmethod == "meboot") {
   n3$figuretext <- "The interval forecast was obtained by maximum entropy bootstrap."
  }

  if (n3$bootmethod == "stlboot") {
   n3$figuretext <- "The interval forecast was obtained by loess bootstrap."
  }

  n3$figurecaption  <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       tolower(n3$avgthree.model.fits[[i]]$age),
                       " component of the ",
                       # max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       # " terminal run
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock,"),
                       " derived via naive modeling based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years. ",
                       n3$figuretext, 
                       sep="")

   doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

   n3$figuretext <- NULL 
}



#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Total Age ---------------------------------------------------------------

doc = addPageBreak(doc)

n3$results.retro <- n3$results.total.age.retro.predictive.performance.avgthree
names(n3$results.retro)
n3$results.pred <- n3$PI.total.age.avgthree.no.comma 

doc = addPlot(doc, 
        fun = print,
        x = n3$gary.plot.total.age(n3$results.retro, n3$results.pred),
        width=6.5, height=6) 
 
if (n3$bootmethod == "meboot") {
   n3$figuretext <- "The interval forecast was obtained by maximum entropy bootstrap."
}

if (n3$bootmethod == "stlboot") {
   n3$figuretext <- "The interval forecast was obtained by loess bootstrap."
}
 
n3$figurecaption <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       # "the ",
                       # max(n3$results.afe.total.age.retro.avgthree$CY)+1,
                       " total ",
                       # terminal run ",
                       tolower(n3$stockabundance),
                       " for the ",
                       paste(n3$stockname,tolower(n3$stockspecies), "stock,"),
                       " derived via naive modeling based on the average ",
                       tolower(n3$stockabundance), 
                       " from the previous 3 years. ",
                       n3$figuretext, 
                       sep="")

doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$figuretext <- NULL 
n3$figurecaption <- NULL 

doc = addPageBreak(doc)

#---- Forecast Diagnostics - Individual Ages ------------------------------------------------------------------------

doc = addTitle( doc, "Forecast Diagnostics", level = 1)


n3$paragraph <- paste0("This section reports two types of forecast diagnostic plots for age-specific and total ", 
                    tolower(n3$stockabundance), ":")
doc = addParagraph(doc, n3$paragraph, stylename="Normal")
  
n3$paragraph <- NULL 
  
doc = addParagraph( doc, value = 'Superimposed time series plots of retrospectively forecasted and actual abundance values;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Scatter plots of retrospectively forecasted versus actual abundance values.',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")      


#--- Forecast Diagnostics - Time Series Plots of Retrospectively Forecasted Values - Individual Ages -----------

doc = addPageBreak( doc )

n3$figurecaption <- paste("Superimposed time series plots of retrospectively forecasted and actual ",
                       #### terminal runs
                       ## paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       ## " and actual terminal runs for specific age ",
                       tolower(n3$stockabundance), " values ",  
                       "for specific age ", 
                       "components of the ",
                       n3$stockname, " ",
                       tolower(n3$stockspecies),
                       " stock. ",
                       ## "Observations in each panel are labeled according to the associated historical return years. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " for a given return year was derived by applying naive modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       " The naive modeling was based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " from the previous 3 years.",
                       sep=""
                       )


n3$myplot <- n3$timeseries.plot.results.afe.individual.ages.retro.avgthree(n3$results.individual.ages.retro.predictive.performance.avgthree, 
                                                           n3$stockabundance)



doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth+1, height=plotheight+1)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 

#-----------------------------------------------------------------------------------------------------

n3$figurecaption <- paste("Scatter plots of retrospectively forecasted versus actual ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       "for specific age ",
                       "components of the ",
                       n3$stockname, " ",
                       tolower(n3$stockspecies),
                       " stock. ",
                       "Observations in each panel are labeled according to the associated historical return years. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " for a given return year was derived by applying naive modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       " The naive modeling was based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years.",
                       sep=""
                       )


n3$myplot <-  n3$scatter.plot.results.afe.individual.ages.retro.avgthree(n3$results.individual.ages.retro.predictive.performance.avgthree)


doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth+1, height=plotheight)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 


#---- Forecast Diagnostics - Total Age ------------------------------------------------------------------------

#--- Forecast Diagnostics - Scatter Plots of Retrospectively Forecasted Values - Total Age -----------


doc = addPageBreak(doc)

n3$figurecaption <- paste("Superimposed time series plots of retrospectively forecasted and actual total ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance)," values",collapse=" "),
                       ## " versus actual total ",
                       # terminal runs ",
                       ## paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n3$results.afe.total.age.retro.avgthree$CY),
                       " - ",
                       max(n3$results.afe.total.age.retro.avgthree$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       ## "Observations are labeled according to the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " value for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " Naive modeling based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " from the previous 3 years was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")


n3$myplot <- n3$timeseries.plot.results.afe.total.age.retro.avgthree(n3$results.total.age.retro.predictive.performance.avgthree, n3$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 

#------------------------------------------------------------------------------------------

doc = addPageBreak(doc)

n3$figurecaption <- paste("Scatter plot of retrospectively forecasted versus actual total ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n3$results.afe.total.age.retro.avgthree$CY),
                       " - ",
                       max(n3$results.afe.total.age.retro.avgthree$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " Naive modeling based on the average ",
                       # terminal run
                       tolower(n3$stockabundance),
                       " over the previous 3 years was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n3$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")


n3$myplot <- n3$scatter.plot.results.afe.total.age.retro.avgthree(n3$results.total.age.retro.predictive.performance.avgthree)

doc = addPlot(doc,
            fun=print,
            x=n3$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n3$figurecaption, stylename="rPlotLegend")

n3$myplot <- NULL 
n3$figurecaption <- NULL 


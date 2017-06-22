#========================================================================================================
# CoverPage
#========================================================================================================

n5$fits <- n5$avgfive.model.fits

usePackage("stringr")
n5$stockabundance <- str_replace(n5$stockabundance,"_"," ") 

n5$pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 40) )
n5$my.pars = set_of_paragraphs(n5$pot1)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )

n5$pot1 <- NULL 
n5$my.pars <- NULL 

n5$pot1 = pot(" ", textProperties(font.weight="bold", font.size = 20) )
n5$my.pars = set_of_paragraphs(n5$pot1)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )

n5$pot1 <- NULL 
n5$my.pars <- NULL 

n5$pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n5$stockname), textProperties(font.size = 20) )
n5$my.pars = set_of_paragraphs(n5$pot2)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )

n5$pot2 <- NULL 
n5$my.pars <- NULL 

n5$pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n5$stockspecies), textProperties(font.size = 20) )
n5$my.pars = set_of_paragraphs(n5$pot3)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )

n5$pot3 <- NULL 
n5$my.pars <- NULL 

n5$pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n5$stockabundance), textProperties(font.size = 20) )
n5$my.pars = set_of_paragraphs(n5$pot4)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )

n5$pot4 <- NULL 
n5$my.pars <- NULL 

n5$pot5 =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n5$forecastingyear), textProperties(font.size = 20) )
n5$my.pars = set_of_paragraphs(n5$pot5)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )

n5$pot5 <- NULL 
n5$my.pars <- NULL 

n5$pot6 =  pot("Forecasting Model: ", textProperties(font.weight="bold", font.size = 20)) 
n5$pot9 = pot(paste("Naive Model (Average of Previous 5 Years)"), textProperties(font.size = 20) ) 
n5$my.pars = set_of_paragraphs(n5$pot6, n5$pot9)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )
       
n5$pot6 <- NULL 
n5$pot9 <- NULL 
n5$my.pars <- NULL 
               
if (n5$bootmethod=="meboot") {      
    n5$texta <- "Maximum Entropy Bootstrap"
}

if (n5$bootmethod=="stlboot") {      
    n5$texta <- "Loess Bootstrap"
}
           

n5$pot6a =  pot("Time Series Bootstrap Method: ", textProperties(font.weight="bold", font.size = 20)) 
n5$pot9a = pot(paste0(n5$texta), 
            textProperties(font.size = 20) ) 
n5$my.pars = set_of_paragraphs(n5$pot6a, n5$pot9a)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )
       
n5$pot6a <- NULL
n5$pot9a <- NULL  
n5$my.pars <- NULL             

n5$pot13 =  pot("Date: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(Sys.Date()), textProperties(font.size = 20) )
n5$my.pars = set_of_paragraphs(n5$pot13)
doc = addParagraph( doc, value = n5$my.pars, stylename="Normal" )
     
n5$pot13 <- NULL 
n5$my.pars <- NULL 

doc = addPageBreak(doc)


#=================================================================================================
# Summary of Results
#=================================================================================================

doc = addTitle(doc, "Summary of Results", level=1)

## doc = addParagraph(doc, " ", stylename="Normal", par.properties=parProperties(text.align="justify", padding.top = 0, padding.bottom = 0))

## point forecasts + individual ages
n5$results.point.forecast.avgfive

n5$tabledata <- matrix(NA, nrow=11, ncol=length(n5$fits)+2)

colnames(n5$tabledata) <- rep("Name", length(n5$fits)+2)
colnames(n5$tabledata)[1] <- "Item"
colnames(n5$tabledata)[2:(1+length(n5$fits))] <- as.character(n5$results.point.forecast.avgfive$Age)
colnames(n5$tabledata)[2+length(n5$fits)] <- "Total"


n5$tabledata[1,] <- c("Return Year", rep(unique(n5$results.point.forecast.avgfive$RY),
                   length(n5$fits)+1))

n5$tabledata[2,] <- c("Model", rep("Average of Previous 5 Years", length(n5$fits)+1))

n5$tabledata[3,] <- c("Point Forecast",
                   c(as.character(comma(round(n5$results.point.forecast.avgfive$p))),
                     as.character(comma(sum(round(n5$results.point.forecast.avgfive$p))))))


n5$PI.individual.ages.avgfive
n5$PI.total.age.avgfive

n5$PI.combined <- rbind(n5$PI.individual.ages.avgfive, n5$PI.total.age.avgfive)

n5$PI.combined.vec <- NULL
for (i in 1:nrow(n5$PI.combined)){

   n5$tmp.vec <- paste0(n5$PI.combined[i,"PI.lwr"]," - ",n5$PI.combined[i,"PI.upr"])

   n5$PI.combined.vec <- c(n5$PI.combined.vec, n5$tmp.vec)

}


n5$tabledata[4,] <- c("Interval Forecast", n5$PI.combined.vec)

## tabledata <- M.avgfive
## tabledata <- subset(M.avgfive, select=-Model)

n5$M.sub.avgfive <- subset(n5$M.avgfive, select=-Model)

n5$tabledata[5:(5 + nrow(n5$M.sub.avgfive) - 1),"Item"] <- as.character(n5$M.sub.avgfive$Measure)

for (k in 2:ncol(n5$M.sub.avgfive)){
    n5$tabledata[5:(5 + nrow(n5$M.sub.avgfive) - 1),k] <- n5$M.sub.avgfive[,k]
}



n5$fits <-  n5$avgfive.model.fits
n5$results <- n5$results.individual.ages.retro.predictive.performance.avgfive
n5$results.total.age <- n5$results.total.age.retro.predictive.performance.avgfive

## r.squared.table <- r.squared.values.avgfive(fits, results, results.total.age)
## tabledata[5 + nrow(M.sub.avgfive),"Item"] <- "R-squared"
## tabledata[5 + nrow(M.sub.avgfive),-1] <- as.character(r.squared.table$R.squared)



##
## Ethan fix - March 16, 2017
##
n5$bias.coeff.afe.individual.ages.retro.avgfive <- n5$bias.coeff.afe.individual.ages.retro.avgfive[!is.na(names(n5$bias.coeff.afe.individual.ages.retro.avgfive))]


n5$tabledata[5 + nrow(n5$M.sub.avgfive),"Item"] <- "Bias Coefficient"
n5$tabledata[5 + nrow(n5$M.sub.avgfive),-1] <- round(c(n5$bias.coeff.afe.individual.ages.retro.avgfive, 
                                            n5$bias.coeff.afe.total.age.retro.avgfive),4)


n5$tabledata <- as.data.frame(n5$tabledata)



# tablecounter <- 1

n5$tablecaption <- paste("Summary of forecasting results for the",
                      n5$forecastingyear,
                      "age-specific and total",
                      # terminal run
                      paste(tolower(n5$stockabundance),"s",collapse="", sep=""),
                      " associated with the ",
                      n5$stockname, " ",
                      tolower(n5$stockspecies), " stock.")

n5$tabledata <- n5$tabledata

doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")
    
n5$MyFTable = FlexTable(data=n5$tabledata, 
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
n5$MyFTable <- setFlexTableBorders(n5$MyFTable, 
                                inner.vertical = borderProperties(style = "none"), 
                                inner.horizontal = borderProperties(style = "none"), 
                                outer.vertical = borderProperties(style = "none"), 
                                outer.horizontal = borderProperties(color = "gray5", style = "solid"))


doc = addFlexTable(doc, n5$MyFTable)

n5$MyFTable <- NULL 
n5$tablecaption <- NULL 
        
#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

n5$empirical.probability.yboot.avgfive.total.age <- function(PI.total.age.avgfive.sim, PI.total.age.avgfive.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.avgfive.sim
     mylabel <- paste("Total", n5$stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.avgfive.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


n5$emp.prob.avgfive.total.age <- n5$empirical.probability.yboot.avgfive.total.age(n5$PI.total.age.avgfive.sim, 
                                                                              n5$PI.total.age.avgfive.no.comma, 
                                                                              n5$stockabundance)



n5$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(n5$stockabundance)," ",
                       "value yet to be observed in ",
                       n5$forecastingyear, " for the ",
                       n5$stockname," ",
                       n5$stockspecies, " stock",
                        " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(n5$stockabundance), ".")

doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")

n5$tt_1 <- n5$emp.prob.avgfive.total.age$prob.thresholds

n5$tt_2 <- n5$emp.prob.avgfive.total.age$prob.point.forecast

n5$tt_1_and_2 <- rbind.data.frame(n5$tt_1, n5$tt_2)

usePackage("plyr")

## n5$tt_arrange <- arrange(n5$tt_1_and_2, prob.threshold)

n5$tt_arrange <- n5$tt_1_and_2[order(n5$tt_1_and_2$prob.threshold),]

## n5$tt_arrange <- n5$tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

n5$from_tmp = which(n5$tt_arrange[,1] == n5$emp.prob.avgfive.total.age$prob.point.forecast$prob.threshold)
n5$tt_arrange[n5$from_tmp, 4] <- n5$tt_arrange[n5$from_tmp + 1, 4]


n5$tt_arrange[,1] <- comma(n5$tt_arrange[,1])
n5$tt_arrange[,2] <- paste0(n5$tt_arrange[,2],"%")
n5$tt_arrange[,3] <- paste0(n5$tt_arrange[,3],"%")
n5$tt_arrange[,4] <- paste0(n5$tt_arrange[,4],"%")

names(n5$tt_arrange)[1] <- "Threshold"
names(n5$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(n5$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(n5$tt_arrange)[4] <- "Interval Probability"
n5$tt_arrange[1,4] <- "-"


n5$my_ft <- FlexTable( data = n5$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))

# overwrites some paragraph formatting properties
n5$my_ft[, 1:ncol(n5$tt_arrange)] = parProperties(text.align = "right")

## n5$my_ft[n5$tt_arrange$Threshold %in% comma(n5$emp.prob.avgfive.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )
## Mistake?

n5$my_ft = spanFlexTableRows(n5$my_ft, j=4, from = n5$from_tmp, to = n5$from_tmp + 1)

n5$my_ft[n5$tt_arrange$Threshold %in% comma(n5$emp.prob.avgfive.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

doc = addFlexTable(doc, flextable=n5$my_ft)

n5$my_ft <- NULL 
n5$tablecaption <- NULL 

doc = addPageBreak(doc)


#=================================================================================================
# Introduction
#=================================================================================================

## doc = addPageBreak(doc)

doc = addTitle(doc, "Introduction", level=1)

n5$paragraph <- paste("In this report, we forecast the",
                    n5$forecastingyear,
                    "age-specific and total",
                    paste0(tolower(n5$stockabundance),"s",collapse=" "),
                    "for the",
                    n5$stockname, n5$stockspecies, "stock using a naive forecasting model which does not require statistical parameter estimation",
                    "but rather summarizes past",  
                    tolower(n5$stockabundance),
                    "observations to make forecasts.",
                    "Specifically, given a yearly",
                    tolower(n5$stockabundance),
                    "series, the model uses the average",
                    tolower(n5$stockabundance),
                    "from the previous 5 years to forecast the",
                    tolower(n5$stockabundance),
                    "for the next year.",
                    sep=" ")
    
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

#================================================================================================
# Data
#================================================================================================

doc = addTitle(doc, "Data", level=1)


#---- Show original data (by calendar year) ------------------------------------------------------

# tablecounter <- tablecounter + 1

n5$tablecaption <- paste("Historical ",
                      # terminal run
                      tolower(n5$stockabundance),
                      " data for the ",
                      n5$stockname,
                      tolower(n5$stockspecies), " stock,",
                      "reported as a function of calendar year for specific ages of the stock.")

n5$tabledata <- n5$datafile
n5$tabledata[n5$tabledata<0] <- NA

# datalist

n5$datalist1 <- n5$datalist[[1]]
n5$datalist1 <- subset(n5$datalist1,select=-BY)
for (i in 2:length(n5$datalist)){
      n5$datalist.tmp <- n5$datalist[[i]]
      n5$datalist.tmp <- subset(n5$datalist.tmp,select=-BY)

      n5$datalist1 <-  merge(n5$datalist1, n5$datalist.tmp, by=c("CY"))

}

# datalist1

n5$tabledata <- n5$datalist1

usePackage("stringr")
names(n5$tabledata) <- str_replace_all(names(n5$tabledata),"T","Age ")
names(n5$tabledata)[names(n5$tabledata)=="CY"] <- "Calendar Year"

usePackage("scales")
n5$tabledata[,-1] <- comma(n5$tabledata[,-1] )


doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")
    
n5$MyFTable = FlexTable(data=n5$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
                                  
doc = addFlexTable(doc, n5$MyFTable)

n5$MyFTable <- NULL 

#---- Plot original data by return year (for specific ages) -----------------------------------------------

doc = addPageBreak(doc)

n5$figurecaption <- paste("Plots of historical ",
                       # terminal runs,
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " versus return years for the ",
                       n5$stockname," ",
                       tolower(n5$stockspecies), " stock, ",
                       "with each plot corresponding to a specific age component of the stock.",sep="")

doc = addPlot(doc, 
        fun = print,
        x = n5$plot.data.avgfive(n5$datalist),
        width=6.5, height=6)

doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$figurecaption <- NULL 

#================================================================================================
# Modeling Results
#================================================================================================


doc = addTitle(doc, "Naive Time Series Modeling Results", level=1)

#----- Plot Fitted Values Produced by Naive Forecasting (Average of Previous 5 Years) for Each Age Class -------


n5$figurecaption <- paste("Fitted values produced by the naive models used to forecast ",
                       "specific age components of the ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " ",
                       tolower(n5$stockabundance),
                       " for the ",
                       n5$stockname, " ", tolower(n5$stockspecies), " stock. ",
                       "The naive models were based on the average ",
                       tolower(n5$stockabundance),
                       " over the previous 5 years.", sep="")

doc = addPlot(doc, 
        fun = print,
        x = n5$plot.fitted.avgfive(n5$avgfive.model.fits),
        width=6.5, height=6)

doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$figurecaption <- NULL 

#---- Stats Tutorial ----------------------------------------------------------------------------

# doc = addPageBreak(doc)

n5$paragraph <- paste("Stats Tutorial:")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$paragraph <- paste("For each age-specific",
                   tolower(n5$stockabundance),
                   "time series, compare the fitted values produced by the naive model based on the average",   
                   tolower(n5$stockabundance),
                   "over the previous 5 years",
                   "against the observed (or historical) values of the",
                   tolower(n5$stockabundance),
                   "time series.  Does the naive model appear to provide",
                   "a good fit to the time series?",
                   sep=" ")

doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

doc = addPageBreak(doc)

#----- Naive Time Series Modeling Diagnostics (Average of Previous 5 Years): Checking Randomness of Residuals --

doc = addTitle(doc, "Modeling Diagnostics", level=1)

## Come back here!!
## plot.model.diagnostics.avgfive(avgfive.model.fits, i)


#---- Stats Tutorial ----------------------------------------------------------------------------

doc = addParagraph(doc, " ", stylename="Normal", par.properties=parProperties(text.align="justify"))

n5$paragraph <- paste("Stats Tutorial:")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$paragraph <- paste("After fitting a naive model to a univariate time series, ",
                   "where the model was based on the average ",
                   tolower(n5$stockabundance),
                   " over the previous 5 years,",
                   " we need to run diagnostic tests to validate the model.",
                   sep="")
doc = addParagraph(doc, n5$paragraph, stylename="Normal", par.properties=parProperties(text.align="justify"))

n5$paragraph <- NULL

## doc = addParagraph(doc, " ", stylename="Normal", par.properties=parProperties(text.align="justify"))

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$pots <- pot("If the naive model provides a good fit to a univariate time series, the residuals associated with the model should exhibit ") +
        pot("no systematic patterns and ") + 
        pot("no temporal dependence.")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
       
n5$pots <- NULL
n5$paragraph <- NULL
         
n5$pots <- pot("Useful diagnostic plots for verifying that the naive model residuals exhibit no systematic patterns and no temporal dependence include:")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$paragraph <- c("Time series plot of the model residuals;",
                "Autocorrelation plot of the model residuals;",
                "Partial autocorrelation plot of the model residuals;",
                "Plot of p-values associated with the Ljung-Box test applied to the model residuals.")
doc = addParagraph(doc, n5$paragraph, stylename="BulletList", par.properties=parProperties(text.align="justify"))

n5$paragraph <- NULL

## doc = addParagraph(doc, " ", stylename="Normal", par.properties=parProperties(text.align="justify", padding.top=0, padding.bottom = 0))


n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$pots <- pot("The Ljung-Box test is a diagnostic tool used to test the lack of fit of a naive model. ") + 
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
             
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", par.properties=parProperties(text.align="justify", padding.top=0, padding.bottom = 0))

n5$pots <- NULL 
n5$paragraph <- NULL

## doc = addParagraph(doc, " ", stylename="Normal", par.properties=parProperties(text.align="justify", padding.top=0, padding.bottom = 0))
          

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
         
n5$pots <- NULL
n5$paragraph <- NULL
          
n5$paragraph <- "If a naive model provides a good fit to a univariate time series, then:"
doc = addParagraph(doc, n5$paragraph, stylename="Normal", par.properties=parProperties(text.align="justify"))

n5$paragraph <- NULL

n5$paragraph <- pot("The time series plot of the model residuals should exhibit no systematic patterns;")
n5$paragraph <- set_of_paragraphs(n5$paragraph)
doc = addParagraph(doc, n5$paragraph, stylename="BulletList")

n5$paragraph <- NULL

n5$paragraph <- pot("The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals;")
n5$paragraph <- set_of_paragraphs(n5$paragraph)
doc = addParagraph(doc, n5$paragraph, stylename="BulletList")

n5$paragraph <- NULL

n5$paragraph <- pot("The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;")
n5$paragraph <- set_of_paragraphs(n5$paragraph)
doc = addParagraph(doc, n5$paragraph, stylename="BulletList")

n5$paragraph <- NULL

n5$paragraph <-  set_of_paragraphs(pot("The p-values associated with the Ljung-Box test should be large for all values of") +  
                                pot(" m ",  textProperties(font.style = "italic", font.size=10, font.family="Calibri")) + 
                                pot("considered."))
doc = addParagraph(doc, n5$paragraph, stylename="BulletList")

n5$paragraph <- NULL

#---- Model Diagnostic Plots

doc = addPageBreak(doc)

for (i in 1:length(n5$fits)) {

## fits[[i]]$age   gives you the age 
n5$figurecaption <- paste0("Model diagnostics for the naive model fit corresponding to the ",
                       tolower(n5$fits[[i]]$age), " component of the ", 
                       n5$stockname, " ", n5$stockspecies, " stock. ", 
                       "The naive model was based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years.")

n5$myplot <- n5$diagnostics.avgfive.model.fit(n5$fits,i)


doc = addPlot(doc,
            fun= plot, # print,
            x=n5$myplot,
            width=plotwidth+1, height=plotheight)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 

}






#================================================================================================
# Forecasting Results
#================================================================================================

doc = addPageBreak(doc)

doc = addTitle(doc, "Forecasting Results", level=1)

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$paragraph <- pot("This section reports the forecasting results for the ") + 
             pot(paste(n5$stockspecies)) + 
             pot(" ") + 
             pot(n5$stockname) + 
             pot(" stock ") + 
             pot("corresponding to the forecasting year ") + 
             pot(paste(n5$forecastingyear,". ", sep="")) + 
             pot("The results were produced via naive modeling based on the average ") + 
             pot(paste(tolower(n5$stockabundance)," over the previous 5 years.", sep="")) 
n5$paragraph <- set_of_paragraphs(n5$paragraph)
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$paragraph <- pot("Forecasting results are reported numerically and visually for two types of forecasts:")
n5$paragraph <- set_of_paragraphs(n5$paragraph)
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

n5$paragraph <- c("   1) point forecasts;", "   2) interval forecasts.")
doc = addParagraph(doc, n5$paragraph, parent.type="ol", stylename="Normal")

n5$paragraph <- NULL                      

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n5$pots <- NULL 
n5$paragraph <- NULL
                      
n5$paragraph <- pot("A point forecast is simply a number which represents our best guess ") +
             pot("of the future value of the age-specific or total ") + 
             pot(paste(tolower(n5$stockabundance))) + 
             pot(" for the stock of interest based on available historical data.")
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n5$pots <- NULL
n5$paragraph <- NULL
                   
n5$paragraph <- pot("An interval forecast is not a single number, rather it is a range of values ") + 
             pot("in which we expect the future value of an age-specific or total ") + 
             pot(paste(tolower(n5$stockabundance))) + 
             pot(" series to fall with some (prespecified) probability.")
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$paragraph <- pot("A couple of remarks are in order in connection with an interval forecast:") 
n5$paragraph <- set_of_paragraphs(n5$paragraph)
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

n5$paragraph <- c(paste("The width of the interval forecast conveys information regarding forecast uncertainty", 
                     "(the wider the interval forecast, the more uncertain the forecast);"),
               "The interval forecast conveys more information than the associated point forecast.")
doc = addParagraph(doc, n5$paragraph, stylename="BulletList")

n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                    
n5$pots <- NULL
                    
if (n5$bootmethod=="meboot") {
n5$paragraph <- pot("The interval forecast provided in this report for each ") + 
             pot(paste(tolower(n5$stockabundance))) + 
             pot(" time series was obtained by applying maximum entropy bootstrapping to that series. ") +
             pot("The maximum entropy bootstrap constructs a large number B (say, B=999) ") +
             pot("of replicates of a dependent time series using an algorithm designed ") + 
             pot("to satisfy the ergodic theorem (i.e., the grand mean of all replicates is close to the sample mean of the original time series). ") +
             pot("The algorithm can accommodate both stationary and non-stationary time series. ") +
             pot("The constructed replicates retain the basic shape (i.e., local peaks and troughs) of the original time series. ") + 
             pot("They also retain the time dependence structure of the autocorrelation function (ACF) ") +
             pot("and the partial autocorrelation function (PACF) of the original time series.")
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")
       
n5$paragraph <- NULL            

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                    
n5$pots <- NULL
n5$paragraph <- NULL
                    
n5$paragraph <- pot("Given a ") + 
             pot(paste(tolower(n5$stockabundance))) + 
             pot(" time series and the B maximum entropy bootstrap replicates of the series, the 80% interval forecast ") + 
             pot("for the next (future) value of the series is obtained by following the steps below: ") 
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")
       
n5$paragraph <- NULL   

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
              
n5$pots <- NULL
n5$paragraph <- NULL
              
n5$paragraph <- c("   a) Apply naive forecasting to each of the B maximum entropy bootstrap replicates in order to forecast the next (future) value of that replicate;",
               "   b) Compute the 10% percentile (P.10) and the 90% percentile (P.90) of the distribution of the B forecasted values derived in the previous step;",
               "   c) Set the lower and upper limits of the 80% interval forecast to be P.10 and P.90, respectively.")
doc = addParagraph(doc, n5$paragraph, parent.style="ol",stylename="Normal")
       
n5$paragraph <- NULL        

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n5$pots <- NULL
n5$paragraph <- NULL
                           
n5$paragraph <- pot("For interval forecast with a negative lower limit, ") +  
             pot("the lower limit is set to zero to ensure the interval forecast includes only positive values.")
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

}
             

if (n5$bootmethod=="stlboot"){
n5$paragraph <- pot("The interval forecast provided in this report for each ") + 
             pot(paste0(tolower(n5$stockabundance), " time series ")) + 
             pot("age-specific or total) ") + 
             pot(" was obtained by applying loess bootstrapping to that series.")
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n5$pots <- NULL
n5$paragraph <- NULL
                   
n5$paragraph <- pot("The loess bootstrapping is a time series bootstrapping method introduced by Bergmeir, Hyndman and Benitez in 2014 in their working paper on ") +
             pot("bagging exponential smoothing methods using the STL decomposition and the Box-Cox transformation. ") + 
             pot("In this method, the time series of annual abundance values which needs to be bootstrapped is first transformed via a Box-Cox transformation. ") +
             pot("The transformed time series is then decomposed into its trend and remainder components using the loess method ") +
             pot("(i.e., a smoothing method based on local linear regression). ") +
             pot("Finally, the remainder component is bootstrapped using the moving block bootstrap (MBB), ") +
             pot("the trend and seasonal components are added back, and the Box-Cox transformation is inverted. ") +
             pot("In this way, a random pool of B similar bootstrapped time series is generated from the original time series.")
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")             

n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                   
n5$pots <- NULL
n5$paragraph <- NULL
                   
n5$paragraph <- pot("Given a ") + 
             pot(paste(tolower(n5$stockabundance))) + 
             pot(" time series and the B loess bootstrap replicates of the series, the 80% interval forecast ") + 
             pot("for the next (future) value of the series is obtained by following the steps below: ") 
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")
       
n5$paragraph <- NULL
          
n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL                   
n5$paragraph <- NULL
            
n5$paragraph <- c("   a) Apply naive forecasting to each of the B loess bootstrap replicates in order to forecast the next (future) value of that replicate;",
               "   b) Compute the 10% percentile (P.10) and the 90% percentile (P.90) of the distribution of the B forecasted values derived in the previous step;",
               "   c) Set the lower and upper limits of the 80% interval forecast to be P.10 and P.90, respectively.")
doc = addParagraph(doc, n5$paragraph, parent.style="ol",stylename="Normal")

n5$pots <- NULL
n5$paragraph <- NULL          

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
       
n5$pots <- NULL
n5$paragraph <- NULL
               
n5$paragraph <- pot("For interval forecast with a negative lower limit, ") +  
             pot("the lower limit is set to zero to ensure the interval forecast includes only positive values.")
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$pots <- NULL
n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

n5$paragraph <- pot("Note that, if the historical abundance data for a specific age class includes too many 0 and/or 1 values, ") +  
             pot("the loess bootstrapping for that age will fail, in the sense of producing bootstrapping point forecasts whose values are huge and thereby implausible. ") + 
             pot("Currently, such situations are handled by reverting to the maximum entropy bootstrap for the problematic age(s).")   
n5$paragraph <- set_of_paragraphs(n5$paragraph)             
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$pots <- NULL
n5$paragraph <- NULL

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL
n5$paragraph <- NULL

}

             
             
#----- Point Forecasts --------------------------------------------------------------------------      

doc = addTitle(doc, "Point Forecasts", level=2)


n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
       
n5$pots <- NULL
n5$paragraph <- NULL
                           
n5$tabledata <- n5$results.point.forecast.avgfive    ## this object is created in the review code file

n5$tabledata[nrow(n5$tabledata)+1, ] <- n5$tabledata[nrow(n5$tabledata), ]

n5$tabledata <- transform(n5$tabledata, Age = as.character(Age))

str(n5$tabledata)

n5$tabledata[nrow(n5$tabledata), "Age"] <-  "Total"
n5$tabledata[nrow(n5$tabledata), "Model"] <-  ""
n5$tabledata <- transform(n5$tabledata, p = round(p))
n5$tabledata[nrow(n5$tabledata), "p"] <-  sum(n5$tabledata[1:(nrow(n5$tabledata)-1), "p"])

usePackage("scales")
n5$tabledata <- transform(n5$tabledata, p = comma(p))


names(n5$tabledata)[names(n5$tabledata)=="Age"] <- "Terminal Run"
names(n5$tabledata)[names(n5$tabledata)=="Model"] <- "Model"
names(n5$tabledata)[names(n5$tabledata)=="RY"] <- "Forecasting Year"
names(n5$tabledata)[names(n5$tabledata)=="p"] <- "Point Forecast"

n5$tabledata$Model

usePackage("stringr")
n5$tabledata$Model <- str_replace_all(n5$tabledata$Model, "Past", "Previous")

n5$tablecaption <- paste("Point forecasts of the ",
                      max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                      " age-specific and total",
                      " ",
                      paste0(tolower(n5$stockabundance),"s",collapse=" "),
                      " for the ",
                      n5$stockname,
                      " ",
                      tolower(n5$stockspecies),
                      " stock. ",
                      "The point forecasts for the age-specific ",
                      tolower(n5$stockabundance),
                      " were produced via naive models based on the average",
                      paste0(tolower(n5$stockabundance)),
                      " over the previous 5 years. ",
                      "The point forecast for the total ",
                      tolower(n5$stockabundance),
                      " was obtained by totaling the age-specific point forecasts produced by these naive models. ",
                      "All point forecasts were rounded to the nearest integer for reporting purposes.",
                      sep="")


doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")
    
n5$MyFTable = FlexTable(data=n5$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
    
doc = addFlexTable(doc, n5$MyFTable)
          

#----- Barplot of historical abundance values and associated point forecast: Individual Ages ---------------------------------------

n5$fits <- n5$avgfive.model.fits

n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)

for (i in 1:length(n5$pointforecasts)){

  print(i)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = plot, # print,
        x = n5$barplot.forecasted.values.individual.ages.avgfive(n5$fits, n5$pointforecasts,i),
        width=6.5, height=6)

   n5$age <- n5$avgfive.model.fits[[i]]$age
   n5$age <- tolower(n5$age)

   n5$figurecaption <- paste("Historical ",
                       #terminal run
                       tolower(n5$stockabundance),
                       " values and ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " point forecast ",
                       "corresponding to the ", n5$age, " component of the ",
                       # "terminal run ,
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The ",  max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " point forecast was derived via the naive model based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years.",
                       sep="")


   doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

}

 
#----- Barplot of historical abundance values and associated point forecast: Total Age ---------------------------------------

doc = addPageBreak(doc)

n5$results <- n5$results.total.age.retro.predictive.performance.avgfive
n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)

doc = addPlot(doc, 
              fun = plot, # print,
              x = n5$barplot.forecasted.values.total.age.avgfive(n5$results, n5$pointforecasts),
              width=6.5, height=6)
              
figurecaption <- paste("Historical total ",
                       tolower(n5$stockabundance),
                       " values",
                       " and corresponding ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The",
                       " point forecast of total ",
                       tolower(n5$stockabundance),
                       " was obtained by totaling the point forecasts of the age-specific ",
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " produced by",
                       " the naive models based on the average ",
                       tolower(n5$stockabundance),
                       " over the previous 5 years.",
                       sep="")

doc = addParagraph(doc, value=figurecaption, stylename="rPlotLegend")


#---- Forecast Intervals ------------------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, "Interval Forecasts", level=2)


n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                           
n5$pots <- NULL
n5$paragraph <- NULL

n5$tabledata <- rbind(n5$PI.individual.ages.avgfive, n5$PI.total.age.avgfive)

n5$tabledata

## tabledata <- subset(tabledata, select=-PI.med)

n5$tabledata$PI <- paste( n5$tabledata[,"PI.lwr"]," - ", n5$tabledata[,"PI.upr"], sep="")

n5$tabledata <- subset(n5$tabledata, select=-PI.lwr)
n5$tabledata <- subset(n5$tabledata, select=-PI.upr)

names(n5$tabledata)[names(n5$tabledata)=="PI.ctr"] <- "Point.Forecast"  ## ILLEGAL
names(n5$tabledata)[names(n5$tabledata)=="PI"] <- "Interval.Forecast"

n5$nms <- c(as.character(n5$results.point.forecast.avgfive$Age), "Total")
n5$mds <- c(as.character(n5$results.point.forecast.avgfive$Model), "")
n5$yr <-  c(n5$results.point.forecast.avgfive$RY, unique(n5$results.point.forecast.avgfive$RY))

n5$tabledata <- data.frame(nms=n5$nms, mds=n5$mds, yr=n5$yr, n5$tabledata)

names(n5$tabledata)[names(n5$tabledata)=="nms"] <- "Terminal Run"
names(n5$tabledata)[names(n5$tabledata)=="mds"] <- "Model"
names(n5$tabledata)[names(n5$tabledata)=="yr"] <- "Return Year"
names(n5$tabledata)[names(n5$tabledata)=="Point.Forecast"] <- "Point Forecast"
names(n5$tabledata)[names(n5$tabledata)=="Interval.Forecast"] <- "Interval Forecast"

usePackage("stringr")
n5$tabledata$Model <- str_replace_all(n5$tabledata$Model, "Past", "Previous")


if (n5$bootmethod=="stlboot") {
   n5$tabletext <- paste0(" Interval forecasts were obtained by loess bootstrap.")
}

if (n5$bootmethod=="meboot") {
   n5$tabletext <- paste0(" Interval forecasts were obtained by maximum entropy bootstrap.")
}

n5$tablecaption <- paste("Point forecasts and associated 80% interval forecasts of the ",
                      max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                                              # " terminal run ",
                      " ",
                      tolower(n5$stockabundance),
                      " for the ",
 				              n5$stockname,
                		  " ",
                		  tolower(n5$stockspecies),
                		  " stock.",
                      # min(ages)," - ",max(ages),
                      #     " and the total age. ",
                          ## " Negative lower limits for the interval forecasts were truncated to zero to ",
                          ## "ensure the interval forecasts only include positive values. ",
                          " The point forecasts for age-specific components of ",
                          # terminal run
                          tolower(n5$stockabundance),
                          " were obtained via naive models based on the average ",
                          # terminal run
                          tolower(n5$stockabundance),
                          " over the previous 5 years.",
                          " The point forecast for total ",
                          # terminal run
                          tolower(n5$stockabundance),
                          " was obtained by adding up the point forecasts for the age-specific components of ",
                          # terminal run
                          tolower(n5$stockabundance),
                          " produced by the naive models. ",
                          n5$tabletext,
                          sep="")

doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")
    
n5$MyFTable = FlexTable(data=n5$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
    
doc = addFlexTable(doc, n5$MyFTable)

n5$MyFTable <- NULL 
n5$tablecaption <- NULL 

### Visualization of forecast intervals - scatterplots (individual ages)

doc = addPageBreak(doc)

for (i in 1:length(n5$avgfive.model.fits)){

    if (n5$bootmethod=="stlboot"){
       n5$figuretext <- " The interval forecast was obtained by loess bootstrap."
    }
    
    if (n5$bootmethod=="meboot"){
       n5$figuretext <- " The interval forecast was obtained by maximum entropy bootstrap."
    }

    n5$figurecaption <- paste("Historical ",
                           # terminal run
                           tolower(n5$stockabundance),
                           " values along with the ",
                            max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                          " point forecast and 80% interval forecast of the ",
                       # " terminal run",
                        tolower(n5$stockabundance),
                       " corresponding to the ",
                       tolower(n5$avgfive.model.fits[[i]]$age),
                       " component of the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The point forecast was obtained via the naive model based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years.",
                       ## " The interval forecast was obtained by maximum entropy bootstrap. ",
                       n5$figuretext, 
                       sep="")

   
       n5$fits <- n5$avgfive.model.fits
       n5$pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)
       n5$intervalforecasts <-   n5$PI.individual.ages.avgfive.no.comma

       n5$myplot <- n5$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.avgfive(n5$fits, n5$pointforecasts, n5$intervalforecasts,i)

    
       
       doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth+1, height=plotheight-2)
            
        doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

        n5$myplot <- NULL
        n5$figurecaption <- NULL 
        n5$figuretext <- NULL 


}



### Visualization of forecast intervals - scatterplot(total age)

if (n5$bootmethod=="stlboot") {
    n5$figuretext <- " The interval forecast was obtained by loess bootstrap."
}

if (n5$bootmethod=="meboot") {
    n5$figuretext <- " The interval forecast was obtained by maximum entropy bootstrap."
}


n5$figurecaption <- paste("Historical total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " values ",
                       # "(", "ages ", min(ages), " - ", max(ages), ")",
                       "and corresponding ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " point forecast and 80% forecast interval for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The point forecast for total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       # terminal run",
                        tolower(n5$stockabundance),
                       " produced by the naive models based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years.",
                       ## " The interval forecast was obtained by maximum entropy bootstrap. ",
                       n5$figuretext,
                       sep="")


n5$results <- n5$results.total.age.retro.predictive.performance.avgfive

pointforecasts <- n5$point.forecast.avgfive(n5$datalist, n5$avgfive.model.fits)

n5$intervalforecasts <-  n5$PI.total.age.avgfive.no.comma

n5$myplot <- n5$scatterplot.forecasted.values.and.forecast.intervals.total.age.avgfive(n5$results, n5$pointforecasts, n5$intervalforecasts)



doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot  <- NULL 
n5$figurecaption <- NULL 
n5$figuretext <- NULL 



#--- Bootstrap Distribution of Point Forecasts - Age-Specific Components

doc = addPageBreak(doc)


if (n5$bootmethod=="stlboot"){
     n5$figuretext <- " stock, derived on the basis of loess bootstrapping for the forecasting year "
}

if (n5$bootmethod=="meboot"){
    n5$figuretext <- " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year "
}

n5$figurecaption <- paste("Distributions of bootstrapped forecasted ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " values ",
                       "for specific age components of the ",
                       paste(n5$stockname," ", tolower(n5$stockspecies),
                       n5$figuretext, 
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       ".",
                       " The dashed red line indicates the position of the point forecast on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecasts were obtained via naive modeling based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years.",
                       # " The interval forecasts were obtained by maximum entropy bootstrap.",
                       sep="")


n5$myplot <- n5$plot.distribution.bootstrapped.point.forecasts.individual.ages.avgfive(n5$PI.individual.ages.avgfive.sim,
                                                                        n5$PI.individual.ages.avgfive.no.comma, n5$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL
n5$figurecaption <- NULL 
n5$figuretext <- NULL 


#--- Bootstrap Distribution of Point Forecasts - Total Age


if (n5$bootmethod=="stlboot"){
    n5$figuretext <- " stock, derived on the basis of loess bootstrapping for the forecasting year "
}

if (n5$bootmethod=="meboot"){
    n5$figuretext <- " stock, derived on the basis of maximum entropy bootstrapping for the forecasting year "
}

n5$figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " corresponding to the ",
                       paste(n5$stockname," ", tolower(n5$stockspecies),
                       n5$figuretext, 
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       ".",
                       " The dashed red line indicates the position of the point forecast of total ",  
                       tolower(n5$stockabundance),  
                       " on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ",  
                       tolower(n5$stockabundance), 
                       " was obtained by adding up the point forecasts for the age-specific components of ", 
                       tolower(n5$stockabundance),
                       " produced by the naive models based on the average ", 
                       paste(tolower(n5$stockabundance)), 
                       " over the previous 5 years.",
                       sep="")

n5$myplot <- n5$plot.distribution.bootstrapped.point.forecasts.total.age.avgfive(n5$PI.total.age.avgfive.sim, n5$PI.total.age.avgfive.no.comma, n5$stockabundance)

doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth, height=plotheight-3)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 
n5$figuretext <- NULL 


#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

n5$empirical.probability.yboot.avgfive.total.age <- function(PI.total.age.avgfive.sim, PI.total.age.avgfive.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.avgfive.sim
     mylabel <- paste("Total", n5$stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.avgfive.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


n5$emp.prob.avgfive.total.age <- n5$empirical.probability.yboot.avgfive.total.age(n5$PI.total.age.avgfive.sim, 
                                                                              n5$PI.total.age.avgfive.no.comma, 
                                                                              n5$stockabundance)



n5$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(n5$stockabundance)," ",
                       "value yet to be observed in ",
                       n5$forecastingyear, " for the ",
                       n5$stockname," ",
                       n5$stockspecies, " stock",
                        " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(n5$stockabundance), ".")

doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")

n5$tt_1 <- n5$emp.prob.avgfive.total.age$prob.thresholds

n5$tt_2 <- n5$emp.prob.avgfive.total.age$prob.point.forecast

n5$tt_1_and_2 <- rbind.data.frame(n5$tt_1, n5$tt_2)

usePackage("plyr")

## n5$tt_arrange <- arrange(n5$tt_1_and_2, prob.threshold)

n5$tt_arrange <- n5$tt_1_and_2[order(n5$tt_1_and_2$prob.threshold),]

## n5$tt_arrange <- n5$tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

n5$from_tmp = which(n5$tt_arrange[,1] == n5$emp.prob.avgfive.total.age$prob.point.forecast$prob.threshold)
n5$tt_arrange[n5$from_tmp, 4] <- n5$tt_arrange[n5$from_tmp + 1, 4]


n5$tt_arrange[,1] <- comma(n5$tt_arrange[,1])
n5$tt_arrange[,2] <- paste0(n5$tt_arrange[,2],"%")
n5$tt_arrange[,3] <- paste0(n5$tt_arrange[,3],"%")
n5$tt_arrange[,4] <- paste0(n5$tt_arrange[,4],"%")


names(n5$tt_arrange)[1] <- "Threshold"
names(n5$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(n5$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(n5$tt_arrange)[4] <- "Interval Probability"
n5$tt_arrange[1,4] <- "-"


n5$my_ft <- FlexTable( data = n5$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))

# overwrites some paragraph formatting properties
n5$my_ft[, 1:ncol(n5$tt_arrange)] = parProperties(text.align = "right")

## n5$my_ft[n5$tt_arrange$Threshold %in% comma(n5$emp.prob.avgfive.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )
## Mistake?

n5$my_ft = spanFlexTableRows(n5$my_ft, j=4, from = n5$from_tmp, to = n5$from_tmp + 1)

n5$my_ft[n5$tt_arrange$Threshold %in% comma(n5$emp.prob.avgfive.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)

doc = addFlexTable(doc, flextable=n5$my_ft)

n5$my_ft <- NULL 
n5$tablecaption <- NULL 

#---- Forecast Performance Measures ---------------------------------------------------------------

doc = addPageBreak(doc)


doc = addTitle(doc, value="Retrospective Evaluation of Performance of Point Forecasts", level=1)
  

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL 
n5$paragraph <- NULL                 
  
n5$paragraph <- paste("This section reports the results corresponding to the retrospective evaluation of the performance of the",
                       "point forecasts produced",
                       "by the naive models for the", n5$forecastingyear, "age-specific and total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       "corresponding to the",
                       n5$stockname, n5$stockspecies, "model stock.",
                       "The naive models were based on the average ",  
                       paste(tolower(n5$stockabundance)),
                       "over the previous 5 years.",
                      sep=" ")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))

n5$pots <- NULL 
n5$paragraph <- NULL       

n5$paragraph <- paste("The retrospective evaluation of the performance of the point forecasts assessed how well the naive model",
                       "performed when used for retrospective forecasting of the historical",
                       tolower(n5$stockabundance), "values.",
                       "For this evaluation, the naive model was fit to all of the historical",
                       tolower(n5$stockabundance), "values",
                       "available prior to a given historical return year, then the fitted model was used",
                       "to forecast the",
                       # terminal run
                       tolower(n5$stockabundance),
                       "for that year.",
                       "This evaluation captures how well the model would have performed in practice year over year",
                       "and was performed separately for each age-specific",
                        tolower(n5$stockabundance),
                        "and for the total",
                        tolower(n5$stockabundance),
                        ".",
                      sep=" ")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


n5$pots <- NULL 
n5$paragraph <- NULL                            

n5$paragraph <- paste("Retrospective forecast errors were defined as the actual",
                       # terminal run
                       tolower(n5$stockabundance),
                       "values minus the retrospectively forecasted",
                       # "terminal run values.",
                        tolower(n5$stockabundance),
                       "values.",
                       "In view of this definition, positive values for the retrospective forecast errors represent forecasts that were too low,",
                       "whereas negative values represent forecasts that were too high.",
                      sep=" ")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")


n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                           

n5$pots <- NULL 
n5$paragraph <- NULL 

n5$paragraph <- "The following retrospective measures were used to characterize different aspects of the retrospective forecasting errors:"
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                           
n5$pots <- NULL 
n5$paragraph <- NULL 
    
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


n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


n5$pots <- NULL 
n5$paragraph <- NULL                            

n5$paragraph <- paste("MAE and MAPE reflect the overall forecast accuracy accounting for systematic bias and year-to-year variation.")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


n5$pots <- NULL
n5$paragraph <- NULL                            
        
n5$paragraph <- paste("MRE and MPE reflect directional bias in raw and relative forecast errors, respectively, with negative values indicating a tendency to",
                       "underforecast and positive values reflecting a tendency to overforecast.",
                      sep=" ")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


n5$pots <- NULL 
n5$paragraph <- NULL                            

n5$paragraph <- paste("Just like MAE, RMSE  is a measure of the absolute magnitude of the raw retrospective forecast errors, but is more sensitive to large values",
                       "then MAE.", sep=" ")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


n5$pots <- NULL 
n5$paragraph <- NULL                            
    
n5$paragraph <- paste("MASE was proposed by Hyndman and Koehler (2006) as a generally applicable, scale-free measure of forecast accuracy.",
                       "This measure never gives infinite or undefined values.",
                       "In this report, MASE is computed as the average of the absolute values of the scaled retrospective forecast errors produced by",
                       "the naive model based on the",
                       # terminal run
                       tolower(n5$stockabundance),
                       "from the previous year.",
                       "The scaling of the errors involves dividing the errors by the MAE computed from the retrospective forecast errors associated with",
                       "this model.",  
                       # based on the",
                       # terminal run
                       # tolower(n5$stockabundance),
                       # "from the previous year.",
                       # "from the one-step, naive forecasting method.",
                       # "A scaled error is less than 1 if it arises from a better forecast than the one produced by the naive model based on the terminal run for the previous year.",
                       # "Conversely, it is greater than 1 if the forecast is worse than the average one-step, naive forecast computed in-sample.",
                       "The resulting value of MASE will be equal to 1, since the naive model based on the",
                       tolower(n5$stockabundance),
                       "from the previous year",  
                       "is used as a benchmark against which all other forecasting models will be compared in terms of their",
                       "retrospective forecasting performance (as captured by the MASE).",
                      sep=" ")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")
 
n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


n5$pots <- NULL 
n5$paragraph <- NULL                            
    
n5$paragraph <- paste0("To facilitate the interpretation of the retrospective forecast errors, this section reports several types of plots",
                    " for the age-specific and total ", tolower(n5$stockabundance), "s",  
                     ":")  
doc = addParagraph(doc, n5$paragraph, stylename="Normal")
  
n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
  

n5$pots <- NULL 
n5$paragraph <- NULL                            
  
doc = addParagraph( doc, value = 'Plots illustrating the performance of the retrospective forecasting evaluation;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")      
doc = addParagraph( doc, value = 'Density plots of the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Bias coefficient plots derived from the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")          
doc = addParagraph( doc, value = 'Barplots of the retrospective forecast errors together with the forecast interval corresponding to the forecasting year of interest.',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
    

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                           

n5$pots <- NULL 
n5$paragraph <- NULL 

## paragraph <- paste("We also calculated the coefficient of determination obtained by regressing the retrospectively forecasted",
##                       tolower(n5$stockabundance),
##                       "values",
##                       "on the historical",
##                       # terminal run
##                       tolower(n5$stockabundance),
##                       "values.",
##                       "This is simply the squared correlation coefficient of the retrospectively forecasted and historical",
##                       # terminal run
##                        tolower(n5$stockabundance),
##                       "values.",
##                       sep=" ")
## doc = addParagraph(doc, paragraph, stylename="Normal")


n5$paragraph <- paste0("Bias coefficients representing a new metric for forecast bias. ", 
                   "These coefficients are computed from the retrospective forecast errors for the age-specific and total ",
                   tolower(n5$stockabundance),"s", 
                   " using the formula developed by Kourentzes, Trapero and Svetunkov in their 2014 working paper ", 
                   "\"Measuring the behaviour of experts on demand forecasting: a complex task\". ", 
                   "In the context of this report, the bias coefficients describe the direction and magnitude",
                   " of the retrospective forecast bias ", 
                   "associated with the naive forecasting method (average of previous 5 years).")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))


n5$pots <- NULL 
n5$paragraph <- NULL                            

n5$paragraph <- paste0("Generally speaking, the bias coefficients are unit-free and bounded between -1 (maximum negative retrospective forecast bias) ",  
                    "and 1 (maximum positive retrospective forecast bias). ", 
                    "A forecasting method that is always producing retrospective point forecasts which are over the observed historical values ", 
                    "will have a bias coefficient equal to -1, always over-forecasting. ", 
                    "A forecasting method that is always producing retrospective point forecasts which are under the observed historical values will ", 
                    "have a bias coefficient equal to 1, always under-forecasting. Given the bounded nature of the bias coefficient, ", 
                    "we can describe a forecasting method as strongly biased if |bias coefficient| > 0.5 and weakly biased if 0 < |bias coefficient| <= 0.5, ", 
                    "providing a simple and intuitive description of the forecast bias behaviour. ", 
                    "If the bias coefficient is equal to 0, the forecasting method is unbiased.")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")

n5$paragraph <- NULL 

n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                           

n5$pots <- NULL 
n5$paragraph <- NULL 

#---- Retrospective Measures of Forecast Performance -------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, value="Retrospective Measures of Forecast Performance", level=2)


n5$pots <- pot(" ")
n5$paragraph <- set_of_paragraphs(n5$pots)
doc = addParagraph(doc, n5$paragraph, stylename="Normal", 
                   par.properties=parProperties(text.align="justify"))
                     

n5$pots <- NULL 
n5$paragraph <- NULL       

n5$tabledata <- n5$M.avgfive

n5$tabledata <- subset(n5$M.avgfive, select=-Model)

n5$tabledata <- cbind(Model=rep("Naive (Average of Previous 5 Years)",nrow(n5$M.avgfive)),n5$tabledata)


n5$tablecaption <- paste("Retrospective measures of forecast performance ",
                "associated with the point forecasts of the ",
                max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                " age-specific and total ",
                # terminal runs
                paste0(tolower(n5$stockabundance),"s",collapse=" "),
                " for the ",
                n5$stockname,
                " ",
                tolower(n5$stockspecies),
                " stock.",
                " The point forecasts of age-specific ",
                # terminal runs
                paste0(tolower(n5$stockabundance),"s",collapse=" "),
                " were obtained via naive modeling based on the average ",
                # terminal run
                tolower(n5$stockabundance),
                " over the previous 5 years.",
                " The point forecast for the total ",
                # terminal run
                 tolower(n5$stockabundance),
                " was obtained by adding up the point forecasts for the age-specific components of ",
                # terminal run",
                 tolower(n5$stockabundance),
                " produced by the naive models based on the ",
                # terminal run
                 tolower(n5$stockabundance),
                " from the previous year.",
                sep=""
                )

names(n5$tabledata)[names(n5$tabledata)=="Model"] <- "Model Class"

doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

n5$tt <- n5$tabledata

usePackage("stringr")
names(n5$tt) <- str_replace_all(names(n5$tt),"_"," ")

## tt[,-1] <- comma(tt[,-1])

n5$my_ft <- FlexTable( data = n5$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
n5$my_ft[, 1:ncol(n5$tt)] = parProperties(text.align = "right")

n5$my_ft = addFooterRow( n5$my_ft, value = paste("Legend:  MRE = Mean Relative Error; ", 
                                           "MAE = Mean Absolute Error; ", 
                                           "MPE = Mean Percentage Error; ", 
                                           "MAPE = Mean Absolute Percentage Error; ",
                                           "MASE = Mean Scaled Error; ",
                                           "MSE = Root Mean Square Error."), 
        colspan = ncol(n5$tt), 
        text.properties = textProperties(font.size = 9),
        par.properties = parProperties(text.align = "left"))
        
## my_ft = addFooterRow( my_ft, value = c("         MAE = Mean Absolute Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MPE = Mean Percentage Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MAPE = Mean Absolute Percentage Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MASE = Mean Scaled Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         RMSE = Root Mean Square Error."), colspan = ncol(tt))


doc = addFlexTable(doc, flextable=n5$my_ft)
          
n5$my_ft <- NULL 

          
#---- Retrospective Forecast Errors:  Individual Ages ------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, "Retrospective Point Forecasts and Forecast Errors", level=2)


for (i in 1:length(n5$results.individual.ages.retro.predictive.performance.avgfive)) {

    print(i)

    n5$results <- n5$results.individual.ages.retro.predictive.performance.avgfive

    n5$tabledata <- n5$results[[i]]$data.retro

   
    n5$age <- names(n5$results)[i]
    usePackage("stringr")
    n5$age <- str_extract(n5$age,"[[:digit:]]+")
    
    n5$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the age ", n5$age,
                          " component of the ",
 				              max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                		  # " terminal run for the ",
                		  " ",
                      tolower(n5$stockabundance),
                      " for the ",
                      n5$stockname,
                		  " ",
                		  tolower(n5$stockspecies),
                		  " stock. ",
                          "Accompanying return years and actual ",
                     # terminal run
                     tolower(n5$stockabundance),
                     " values are also reported.",
                      " The retrospective point forecasts were obtained via naive modeling based on the average ",
                     # terminal run
                     tolower(n5$stockabundance),
                     " over the previous 5 years.",
                          sep="")

    n5$tabledata$p <- round(n5$tabledata$p,2)
    n5$tabledata$e <- round(n5$tabledata$e,2)

    usePackage("scales")
    n5$tabledata$a <- comma(round(n5$tabledata$a))
    n5$tabledata$p <- comma(round(n5$tabledata$p))
    n5$tabledata$e <- comma(round(n5$tabledata$e))

    names(n5$tabledata)[names(n5$tabledata)=="cy"] <- "Return Year"
    names(n5$tabledata)[names(n5$tabledata)=="a"] <-  "Actual"
    names(n5$tabledata)[names(n5$tabledata)=="p"] <-  "Forecast"
    names(n5$tabledata)[names(n5$tabledata)=="e"] <-  "Error"
    ## names(tabledata)[names(tabledata)=="p.bench"] <-  "Benchmark Forecast"
    ## names(tabledata)[names(tabledata)=="e.bench"] <-  "Benchmark Error"


    n5$tabledata
    
    n5$tabledata <- subset(n5$tabledata, select=c(-p.bench, -e.bench))
    
    doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")

    baseCellProp = cellProperties( padding = 4)

    n5$tt <- n5$tabledata


    usePackage("stringr")
    names(n5$tt) <- str_replace_all(names(n5$tt),"_"," ")

    ## n5$tt[,-1] <- comma(n5$tt[,-1])

    n5$my_ft <- FlexTable( data = n5$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
    )

    # overwrites some paragraph formatting properties
    n5$my_ft[, 1:ncol(n5$tt)] = parProperties(text.align = "right")

    doc = addFlexTable(doc, flextable=n5$my_ft)
    
    n5$my_ft <- NULL 
    n5$tablecaption <- NULL 
    n5$tabledata <- NULL 
    
    doc = addPageBreak(doc)
    
}

    
#---- Retrospective Forecast Errors:  Total Age ------------------------------------------------------------


n5$results <- n5$results.total.age.retro.predictive.performance.avgfive

n5$tabledata <- data.frame(cy = n5$results$data.retro[[1]]$cy,
                        a = n5$results$a.total.retro,
                        p = n5$results$p.total.retro,
                        e = n5$results$e.total.retro)


n5$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the ",
 				              max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                		  " total ",
                      # terminal run
                      tolower(n5$stockabundance),
                      " for the ",
                		  n5$stockname,
                		  " ",
                		  tolower(n5$stockspecies),
                		  " stock. ",
                          "Accompanying return years and actual total ",
                      # terminal run
                      tolower(n5$stockabundance),
                      " values are also reported.",
                      " The retrospective point forecasts for the total ",
                      # terminal run
                       tolower(n5$stockabundance),
                      " were obtained by adding up the retrospective point forecasts for the age-specific components of ",
                      # terminal run
                      tolower(n5$stockabundance),
                      " produced by the naive models based on the average ",
                      # terminal run
                       tolower(n5$stockabundance),
                      " over the previous 5 years.",
                          sep="")

usePackage("scales")
n5$tabledata$a <- comma(round(n5$tabledata$a))
n5$tabledata$p <- comma(round(n5$tabledata$p))
n5$tabledata$e <- comma(round(n5$tabledata$e))

names(n5$tabledata)[names(n5$tabledata)=="cy"] <- "Return Year"
names(n5$tabledata)[names(n5$tabledata)=="a"] <-  "Actual"
names(n5$tabledata)[names(n5$tabledata)=="p"] <-  "Forecast"
names(n5$tabledata)[names(n5$tabledata)=="e"] <-  "Error"


doc = addParagraph(doc, value=n5$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

n5$tt <- n5$tabledata


usePackage("stringr")
names(n5$tt) <- str_replace_all(names(n5$tt),"_"," ")


## n5$tt[,-1] <- comma(n5$tt[,-1])

n5$my_ft <- FlexTable( data = n5$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4))

# overwrites some paragraph formatting properties
n5$my_ft[, 1:ncol(n5$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=n5$my_ft)


n5$my_ft <- NULL 
n5$tablecaption <- NULL 
n5$tabledata <- NULL 

#---- Illustration of how well the retrospective forecast evaluation works: Individual ages ----------

for (j in 1:length(n5$avgfive.model.fits)) {

  print(j)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = print,
        x = n5$individual.ages.retro.plot.avgfive(n5$individual.ages.retro.plot.info.avgfive, n5$stockabundance, j),
        width=plotwidth, height=plotheight)

  
  n5$figurecaption  <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                       tolower( n5$avgfive.model.fits[[j]]$age),
                       " component of ",
                       # max(results.afe.total.age.retro.naiveone$CY)+1,
                       # " terminal run
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock,"),
                       " derived via naive modeling based on the average of ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " from the previous 3 years. ",
                       "Historical values of ",  
                       tolower( n5$avgfive.model.fits[[j]]$age), " ",  
                       tolower(n5$stockabundance),
                       " (grey lines) and fitted values produced by the naive modeling (red lines)",
                       " are also shown. ", 
                       "Each panel corresponds to a particular retrospective forecasting year.",   
                       sep="")


  doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

  n5$figurecaption  <- NULL 

}


#---- Illustration of how well the retrospective forecast evaluation works: Total age ----------

doc = addPageBreak(doc)

doc = addPlot(doc, 
        fun = print,
        x = n5$total.age.retro.plot.avgfive(n5$total.age.retro.plot.info.avgfive, n5$stockabundance),
        width=plotwidth, height=plotheight)

n5$figurecaption  <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                        "total ", 
                       # max(results.afe.total.age.retro.naiveone$CY)+1,
                       # " terminal run
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock,"),
                       " derived via naive modeling based on the average of total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " from the previous 5 years. ",
                       "Historical values of total ",    
                       tolower(n5$stockabundance),
                       " (grey lines) and fitted values produced by the naive modeling (red lines)",
                       " are also shown. ", 
                       "Each panel corresponds to a particular retrospective forecasting year.",   
                       sep="")

doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$figurecaption <- NULL 


#---- Retrospective Forecast Errors: Age-Specific Density Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

n5$figurecaption <- paste("Density plots of the retrospective forecast errors derived from the naive models used to forecast ",
                       "specific age components of the ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " ",
                       # " terminal run for the ",
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The naive models were based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years.",
                       sep="")

n5$myplot <- n5$dens.results.afe.individual.ages.retro.avgfive(
               n5$avgfive.model.fits,
               n5$results.individual.ages.retro.predictive.performance.avgfive
               )
               
doc = addPlot(doc,
            fun=print, 
            x=n5$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 
   
#---- Retrospective Forecast Errors: Total Age Density Plot ---------------------------------------------------------------

doc = addPageBreak(doc)

n5$figurecaption <- paste("Density plots of the retrospective forecast errors ",
                       "involved in forecasting the total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       sep="")


n5$myplot <- n5$dens.results.afe.total.age.retro.avgfive(n5$results.afe.total.age.retro.avgfive)

doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth, height=plotheight-3)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 



#---- Retrospective Forecast Errors: Age-Specific Bias Coefficient Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

n5$figurecaption <- paste("Bias coefficient plots obtained from the retrospective forecast errors derived from the naive models used to forecast ",
                       "specific age components of the ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " ",
                       # " terminal run for the ",
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The naive models were based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " from the previous 5 years.",
                       sep="")

n5$myplot <- n5$bias.coefficients.afe.individual.ages.retro.avgfive(n5$avgfive.model.fits,
                                                     n5$results.individual.ages.retro.predictive.performance.avgfive,
                                                     n5$stockabundance)
                                                        
doc = addPlot(doc,
            fun=grid.draw,
            x=n5$myplot,
            width=plotwidth, height=plotheight+1)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 


#---- Retrospective Forecast Errors: Total Age Bias Coefficient Plot ---------------------------------------------------------------

doc = addPageBreak(doc)

n5$figurecaption <- paste("Bias coefficient plot obtained from the retrospective forecast errors derived from the naive models used to forecast the ",
                       max(n5$results.afe.total.age.retro.avgfive$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock."),
                       " The naive models were based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " from the previous 5 years.",
                       sep="")

n5$myplot <- n5$bias.coefficient.afe.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive,
                                               n5$stockabundance)
                                                        
doc = addPlot(doc,
            fun=grid.draw,
            x=n5$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 


#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Specific Ages ----------

n5$results.retro <- n5$results.individual.ages.retro.predictive.performance.avgfive 
n5$results.pred <- n5$pred.int.individual.ages.avgfive
 
for (i in 1:length(n5$avgfive.model.fits)) {

  print(i)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = print,
        x = n5$gary.plot.individual.ages(n5$results.retro, n5$results.pred, i),
        width=6.5, height=6)

  if (n5$bootmethod == "meboot") {
   n5$figuretext <- "The interval forecast was obtained by maximum entropy bootstrap."
  }

  if (n5$bootmethod == "stlboot") {
   n5$figuretext <- "The interval forecast was obtained by loess bootstrap."
  }

  n5$figurecaption  <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       tolower( n5$avgfive.model.fits[[i]]$age),
                       " component of the ",
                       # max(results.afe.total.age.retro.avgfive$CY)+1,
                       # " terminal run
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock,"),
                       " derived via naive modeling based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years. ",
                       n5$figuretext, 
                       sep="")

   doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

   n5$figuretext <- NULL 
   n5$figurecaption <- NULL 
}



#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Total Age ---------------------------------------------------------------

doc = addPageBreak(doc)

n5$results.retro <- n5$results.total.age.retro.predictive.performance.avgfive
names(n5$results.retro)
n5$results.pred <- n5$PI.total.age.avgfive.no.comma 

doc = addPlot(doc, 
        fun = print,
        x = n5$gary.plot.total.age(n5$results.retro, n5$results.pred),
        width=6.5, height=6) 
 
if (n5$bootmethod == "meboot") {
   n5$figuretext <- "The interval forecast was obtained by maximum entropy bootstrap."
}

if (n5$bootmethod == "stlboot") {
   n5$figuretext <- "The interval forecast was obtained by loess bootstrap."
}
 
n5$figurecaption <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       # "the ",
                       # max(results.afe.total.age.retro.avgfive$CY)+1,
                       " total ",
                       # terminal run ",
                       tolower(n5$stockabundance),
                       " for the ",
                       paste(n5$stockname,tolower(n5$stockspecies), "stock,"),
                       " derived via naive modeling based on the ",  
                       tolower(n5$stockabundance), 
                       " from the previous year. ",
                       n5$figuretext, 
                       sep="")

doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$figuretext <- NULL 
n5$figurecaption <- NULL 

#---- Forecast Diagnostics - Individual Ages ------------------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle( doc, "Forecast Diagnostics", level = 1)

n5$paragraph <- paste0("This section reports two types of forecast diagnostic plots for age-specific and total ", 
                    tolower(n5$stockabundance), ":")
doc = addParagraph(doc, n5$paragraph, stylename="Normal")
  
n5$paragraph <- NULL 
  
doc = addParagraph( doc, value = 'Superimposed time series plots of retrospectively forecasted and actual abundance values;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Scatter plots of retrospectively forecasted versus actual abundance values.',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")      

#--- Forecast Diagnostics - Time Series Plots of Retrospectively Forecasted Values - Individual Ages -----------

doc = addPageBreak( doc )

n5$figurecaption <- paste("Superimposed time series plots of retrospectively forecasted and actual ",
                       #### terminal runs
                       ## paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       ## " and actual terminal runs for specific age ",
                       tolower(n5$stockabundance), " values ",  
                       "for specific age ", 
                       "components of the ",
                       n5$stockname, " ",
                       tolower(n5$stockspecies),
                       " stock. ",
                       ## "Observations in each panel are labeled according to the associated historical return years. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " for a given return year was derived by applying naive modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       " The naive modeling was based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " from the previous 5 years.",
                       sep=""
                       )

n5$myplot <- n5$timeseries.plot.results.afe.individual.ages.retro.avgfive(n5$results.individual.ages.retro.predictive.performance.avgfive, 
                                                           n5$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth+1, height=plotheight+1)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 


#-----------------------------------------------------------------------------------------------------

n5$figurecaption <- paste("Scatter plots of retrospectively forecasted versus actual ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       "for specific age ",
                       "components of the ",
                       n5$stockname, " ",
                       tolower(n5$stockspecies),
                       " stock. ",
                       "Observations in each panel are labeled according to the associated historical return years. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " for a given return year was derived by applying naive modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       " The naive modeling was based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years.",
                       sep=""
                       )


n5$myplot <-  n5$scatter.plot.results.afe.individual.ages.retro.avgfive(n5$results.individual.ages.retro.predictive.performance.avgfive)


doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth+1, height=plotheight)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 


#---- Forecast Diagnostics - Total Age ------------------------------------------------------------------------

#--- Forecast Diagnostics - Scatter Plots of Retrospectively Forecasted Values - Total Age -----------


doc = addPageBreak(doc)

n5$figurecaption <- paste("Superimposed time series plots of retrospectively forecasted and actual total ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance)," values",collapse=" "),
                       ## " versus actual total ",
                       # terminal runs ",
                       ## paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n5$results.afe.total.age.retro.avgfive$CY),
                       " - ",
                       max(n5$results.afe.total.age.retro.avgfive$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       ## "Observations are labeled according to the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " value for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " Naive modeling based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " from the previous 3 years was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")


n5$myplot <- n5$timeseries.plot.results.afe.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive, n5$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 


#------------------------------------------------------------------------------------------

doc = addPageBreak(doc)

n5$figurecaption <- paste("Scatter plot of retrospectively forecasted versus actual total ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n5$results.afe.total.age.retro.avgfive$CY),
                       " - ",
                       max(n5$results.afe.total.age.retro.avgfive$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " Naive modeling based on the average ",
                       # terminal run
                       tolower(n5$stockabundance),
                       " over the previous 5 years was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n5$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")


n5$myplot <- n5$scatter.plot.results.afe.total.age.retro.avgfive(n5$results.total.age.retro.predictive.performance.avgfive)

doc = addPlot(doc,
            fun=print,
            x=n5$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n5$figurecaption, stylename="rPlotLegend")

n5$myplot <- NULL 
n5$figurecaption <- NULL 

n5$fits <- NULL 

#========================================================================================================
# CoverPage
#========================================================================================================

n1$fits <- n1$naiveone.model.fits 

usePackage("stringr")
n1$stockabundance <- str_replace(n1$stockabundance,"_"," ") 

n1$pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 40) )
n1$my.pars = set_of_paragraphs(n1$pot1)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )

n1$pot1 = pot(" ", textProperties(font.weight="bold", font.size = 20) )
n1$my.pars = set_of_paragraphs(n1$pot1)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )

n1$pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n1$stockname), textProperties(font.size = 20) )
n1$my.pars = set_of_paragraphs(n1$pot2)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )

n1$pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n1$stockspecies), textProperties(font.size = 20) )
n1$my.pars = set_of_paragraphs(n1$pot3)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )

n1$pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n1$stockabundance), textProperties(font.size = 20) )
n1$my.pars = set_of_paragraphs(n1$pot4)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )

n1$pot5 =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(n1$forecastingyear), textProperties(font.size = 20) )
n1$my.pars = set_of_paragraphs(n1$pot5)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )

n1$pot6 =  pot("Forecasting Model: ", textProperties(font.weight="bold", font.size = 20)) 
n1$pot9 = pot(paste("Naive Model (Previous Year)"), textProperties(font.size = 20) ) 
n1$my.pars = set_of_paragraphs(n1$pot6, n1$pot9)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )
         
if (n1$bootmethod=="meboot") {      
    n1$texta <- "Maximum Entropy Bootstrap"
}

if (n1$bootmethod=="stlboot") {      
    n1$texta <- "Loess Bootstrap"
}
               
n1$pot6a =  pot("Time Series Bootstrap Method: ", textProperties(font.weight="bold", font.size = 20)) 
n1$pot9a = pot(paste0(n1$texta), 
            textProperties(font.size = 20) ) 
n1$my.pars = set_of_paragraphs(n1$pot6a, n1$pot9a)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )
               
     
n1$pot13 =  pot("Date: ", textProperties(font.weight="bold", font.size = 20) ) + 
        pot(paste(Sys.Date()), textProperties(font.size = 20) )
n1$my.pars = set_of_paragraphs(n1$pot13)
doc = addParagraph( doc, value = n1$my.pars, stylename="Normal" )
     

doc = addPageBreak(doc)



#=================================================================================================
# Summary of Results
#=================================================================================================


doc = addTitle(doc, "Summary of Results", level=1)

## point forecasts + individual ages
n1$results.point.forecast.naiveone


n1$tabledata <- matrix(NA, nrow=11, ncol=length(n1$fits)+2)

colnames(n1$tabledata) <- rep("Name", length(n1$fits)+2)
colnames(n1$tabledata)[1] <- "Item"
colnames(n1$tabledata)[2:(1+length(n1$fits))] <- as.character(n1$results.point.forecast.naiveone$Age)
colnames(n1$tabledata)[2+length(n1$fits)] <- "Total"


n1$tabledata[1,] <- c("Return Year", rep(unique(n1$results.point.forecast.naiveone$RY),
                   length(n1$fits)+1))

n1$tabledata[2,] <- c("Model", rep("Previous Year", length(n1$fits)+1))

n1$tabledata[3,] <- c("Point Forecast",
                   c(as.character(comma(round(n1$results.point.forecast.naiveone$p))),
                     as.character(comma(sum(round(n1$results.point.forecast.naiveone$p))))))


n1$PI.individual.ages.naiveone
n1$PI.total.age.naiveone

n1$PI.combined <- rbind(n1$PI.individual.ages.naiveone, n1$PI.total.age.naiveone)

n1$PI.combined.vec <- NULL
for (i in 1:nrow(n1$PI.combined)){

   n1$tmp.vec <- paste0(n1$PI.combined[i,"PI.lwr"]," - ",n1$PI.combined[i,"PI.upr"])

   n1$PI.combined.vec <- c(n1$PI.combined.vec, n1$tmp.vec)

}


n1$tabledata[4,] <- c("Interval Forecast", n1$PI.combined.vec)

## tabledata <- M.naiveone
## tabledata <- subset(M.naiveone, select=-Model)

n1$M.sub.naiveone <- subset(n1$M.naiveone, select=-Model)

n1$tabledata[5:(5 + nrow(n1$M.sub.naiveone) - 1),"Item"] <- as.character(n1$M.sub.naiveone$Measure)

for (k in 2:ncol(n1$M.sub.naiveone)){
    n1$tabledata[5:(5 + nrow(n1$M.sub.naiveone) - 1),k] <- n1$M.sub.naiveone[,k]
}



n1$fits <-  n1$naiveone.model.fits
n1$results <- n1$results.individual.ages.retro.predictive.performance.naiveone
n1$results.total.age <- n1$results.total.age.retro.predictive.performance.naiveone

## r.squared.table <- r.squared.values.naiveone(fits, results, results.total.age)
## tabledata[5 + nrow(M.sub.naiveone),"Item"] <- "R-squared"
## tabledata[5 + nrow(M.sub.naiveone),-1] <- as.character(r.squared.table$R.squared)

n1$tabledata[5 + nrow(n1$M.sub.naiveone),"Item"] <- "Bias Coefficient"

##
## Ethan fix - March 16, 2017
##
n1$bias.coeff.afe.individual.ages.retro.naiveone <- n1$bias.coeff.afe.individual.ages.retro.naiveone[!is.na(names(n1$bias.coeff.afe.individual.ages.retro.naiveone))]


n1$tabledata[5 + nrow(n1$M.sub.naiveone),-1] <- round(c(n1$bias.coeff.afe.individual.ages.retro.naiveone, 
                                            n1$bias.coeff.afe.total.age.retro.naiveone),4)

n1$tabledata <- as.data.frame(n1$tabledata)

n1$tablecaption <- paste("Summary of forecasting results for the",
                      n1$forecastingyear,
                      "age-specific and total",
                      # terminal run
                      paste(tolower(n1$stockabundance),"s",collapse="", sep=""),
                      " associated with the ",
                      n1$stockname, " ",
                      tolower(n1$stockspecies), " stock.")

n1$tabledata <- n1$tabledata

doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")
    
n1$MyFTable = FlexTable(data=n1$tabledata, 
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
n1$MyFTable <- setFlexTableBorders(n1$MyFTable, 
                                inner.vertical = borderProperties(style = "none"), 
                                inner.horizontal = borderProperties(style = "none"), 
                                outer.vertical = borderProperties(style = "none"), 
                                outer.horizontal = borderProperties(color = "gray5", style = "solid")
)


doc = addFlexTable(doc, n1$MyFTable)


#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

n1$empirical.probability.yboot.naiveone.total.age <- function(PI.total.age.naiveone.sim, PI.total.age.naiveone.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.naiveone.sim
     mylabel <- paste("Total", stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.naiveone.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


n1$emp.prob.naiveone.total.age <- n1$empirical.probability.yboot.naiveone.total.age(n1$PI.total.age.naiveone.sim, 
                                                                              n1$PI.total.age.naiveone.no.comma, 
                                                                              n1$stockabundance)



n1$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(n1$stockabundance)," ",
                       "value yet to be observed in ",
                       n1$forecastingyear, " for the ",
                       n1$stockname," ",
                       n1$stockspecies, " stock",
                        " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(n1$stockabundance), ".")

doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")

n1$tt_1 <- n1$emp.prob.naiveone.total.age$prob.thresholds

n1$tt_2 <- n1$emp.prob.naiveone.total.age$prob.point.forecast

n1$tt_1_and_2 <- rbind.data.frame(n1$tt_1, n1$tt_2)

usePackage("plyr")

## n1$tt_arrange <- arrange(n1$tt_1_and_2, n1$prob.threshold)

n1$tt_arrange <- n1$tt_1_and_2[order(n1$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )


n1$from_tmp = which(n1$tt_arrange[,1] == n1$emp.prob.naiveone.total.age$prob.point.forecast$prob.threshold)
n1$tt_arrange[n1$from_tmp, 4] <- n1$tt_arrange[n1$from_tmp + 1, 4]


n1$tt_arrange[,1] <- comma(n1$tt_arrange[,1])
n1$tt_arrange[,2] <- paste0(n1$tt_arrange[,2],"%")
n1$tt_arrange[,3] <- paste0(n1$tt_arrange[,3],"%")
n1$tt_arrange[,4] <- paste0(n1$tt_arrange[,4],"%")

names(n1$tt_arrange)[1] <- "Threshold"
names(n1$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(n1$tt_arrange)[3] <- "Prob(Actual >= Threshold)"


names(n1$tt_arrange)[4] <- "Interval Probability"
n1$tt_arrange[1,4] <- "-"


n1$my_ft <- FlexTable( data = n1$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
n1$my_ft[, 1:ncol(n1$tt_arrange)] = parProperties(text.align = "right")

## n1$my_ft[n1$tt_arrange$Threshold %in% comma(n1$emp.prob.naiveone.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )
## Mistake?

n1$my_ft = spanFlexTableRows(n1$my_ft, j=4, from = n1$from_tmp, to = n1$from_tmp + 1)

n1$my_ft[n1$tt_arrange$Threshold %in% comma(n1$emp.prob.naiveone.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)


doc = addFlexTable(doc, flextable=n1$my_ft)

doc = addPageBreak(doc)


#=================================================================================================
# Introduction
#=================================================================================================

## doc = addPageBreak(doc)

doc = addTitle(doc, "Introduction", level=1)

n1$paragraph <- paste("In this report, we forecast the",
                    n1$forecastingyear,
                    "age-specific and total",
                    paste0(tolower(n1$stockabundance),"s",collapse=" "),
                    "for the",
                    n1$stockname, n1$stockspecies, "stock using a naive forecasting model which does not require statistical parameter estimation",
                    "but rather summarizes past",  
                    tolower(n1$stockabundance),
                    "observations to make forecasts.",
                    "Specifically, given a yearly",
                    tolower(n1$stockabundance),
                    "series, the model uses the",
                    tolower(n1$stockabundance),
                    "from the previous year to forecast the",
                    tolower(n1$stockabundance),
                    "for the next year.",
                    sep=" ")
    
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

#================================================================================================
# Data
#================================================================================================

doc = addTitle(doc, "Data", level=1)


#---- Show original data (by calendar year) ------------------------------------------------------


n1$tablecaption <- paste("Historical ",
                      # terminal run
                      tolower(n1$stockabundance),
                      " data for the ",
                      n1$stockname, 
                      tolower(n1$stockspecies), " stock,",
                      "reported as a function of calendar year for specific ages of the stock.")

n1$tabledata <- n1$datafile
n1$tabledata[n1$tabledata<0] <- NA

# datalist

n1$datalist1 <- n1$datalist[[1]]
n1$datalist1 <- subset(n1$datalist1,select=-BY)
for (i in 2:length(n1$datalist)){
      n1$datalist.tmp <- n1$datalist[[i]]
      n1$datalist.tmp <- subset(n1$datalist.tmp,select=-BY)

      n1$datalist1 <-  merge(n1$datalist1, n1$datalist.tmp, by=c("CY"))

}

# datalist1

n1$tabledata <- n1$datalist1

usePackage("stringr")
names(n1$tabledata) <- str_replace_all(names(n1$tabledata),"T","Age ")
names(n1$tabledata)[names(n1$tabledata)=="CY"] <- "Calendar Year"

usePackage("scales")
n1$tabledata[,-1] <- comma(n1$tabledata[,-1] )


doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")
    
n1$MyFTable = FlexTable(data=n1$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
                                  
doc = addFlexTable(doc, n1$MyFTable)

#---- Plot original data by return year (for specific ages) -----------------------------------------------

doc = addPageBreak(doc)

n1$figurecaption <- paste("Plots of historical ",
                       # terminal runs,
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " versus return years for the ",
                       n1$stockname," ",
                       tolower(n1$stockspecies), " stock, ",
                       "with each plot corresponding to a specific age component of the stock.",sep="")

doc = addPlot(doc, 
        fun = print,
        x = n1$plot.data.naiveone(n1$datalist),
        width=6.5, height=6)

doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

#================================================================================================
# Modeling Results
#================================================================================================


doc = addTitle(doc, "Naive Time Series Modeling Results", level=1)

#----- Plot Fitted Values Produced by Naive Forecasting (Previous Year) for Each Age Class -------


n1$figurecaption <- paste("Fitted values produced by the naive models used to forecast ",
                       "specific age components of the ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " ",
                       tolower(n1$stockabundance),
                       " for the ",
                       n1$stockname, " ", tolower(n1$stockspecies), " stock. ",
                       "The naive models were based on the ",
                       tolower(n1$stockabundance),
                       " from the previous year.", sep="")

doc = addPlot(doc, 
        fun = print,
        x = n1$plot.fitted.naiveone(n1$naiveone.model.fits),
        width=6.5, height=6)

doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")


#---- Stats Tutorial ----------------------------------------------------------------------------

# doc = addPageBreak(doc)

n1$paragraph <- paste("Stats Tutorial:")

doc = addParagraph(doc, n1$paragraph, stylename="Normal")


n1$paragraph <- paste("For each age-specific",
                   tolower(n1$stockabundance),
                   "time series, compare the fitted values produced by the naive model based on the",   
                   tolower(n1$stockabundance),
                   "from the previous year",
                   "against the observed (or historical) values of the",
                   tolower(n1$stockabundance),
                   "time series.  Does the naive model appear to provide",
                   "a good fit to the time series?",
                   sep=" ")

doc = addParagraph(doc, n1$paragraph, stylename="Normal")


#----- Naive Time Series Modeling Diagnostics (Previous Year): Checking Randomness of Residuals --

doc = addTitle(doc, "Modeling Diagnostics", level=1)

## Come back here!!
## plot.model.diagnostics.naiveone(naiveone.model.fits, i)


#---- Stats Tutorial ----------------------------------------------------------------------------

n1$paragraph <- paste("Stats Tutorial:")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$paragraph <- paste("After fitting a naive model to a univariate time series, ",
                   "where the model was based on the previous year's ",
                   tolower(n1$stockabundance),
                   ",",
                   " we need to run diagnostic tests to validate the model.",
                   sep="")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$pots <- pot("If the naive model provides a good fit to a univariate time series, the residuals associated with the model should exhibit ") +
        pot("no systematic patterns and ") + 
        pot("no temporal dependence.")
n1$paragraph <- set_of_paragraphs(n1$pots)
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

         
n1$pots <- pot("Useful diagnostic plots for verifying that the naive model residuals exhibit no systematic patterns and no temporal dependence include:")
n1$paragraph <- set_of_paragraphs(n1$pots)
doc = addParagraph(doc, n1$paragraph, stylename="Normal")


n1$paragraph <- c("Time series plot of the model residuals;",
                "Autocorrelation plot of the model residuals;",
                "Partial autocorrelation plot of the model residuals;",
                "Plot of p-values associated with the Ljung-Box test applied to the model residuals.")
doc = addParagraph(doc, n1$paragraph, stylename="BulletList")

n1$pots <- pot("The Ljung-Box test is a diagnostic tool used to test the lack of fit of a naive model. ") + 
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
             
n1$paragraph <- set_of_paragraphs(n1$pots)

doc = addParagraph(doc, n1$paragraph, stylename="Normal")
          
n1$paragraph <- "If a naive model provides a good fit to a univariate time series, then:"
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

## n1$paragraph <- c("The time series plot of the model residuals should exhibit no systematic patterns;",
##               "The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals;",
##               "The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;",
##               )
               

n1$paragraph <- pot("The time series plot of the model residuals should exhibit no systematic patterns;")
n1$paragraph <- set_of_paragraphs(n1$paragraph)
doc = addParagraph(doc, n1$paragraph, stylename="BulletList")

n1$paragraph <- pot("The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals;")
n1$paragraph <- set_of_paragraphs(n1$paragraph)
doc = addParagraph(doc, n1$paragraph, stylename="BulletList")

n1$paragraph <- pot("The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;")
n1$paragraph <- set_of_paragraphs(n1$paragraph)
doc = addParagraph(doc, n1$paragraph, stylename="BulletList")

n1$paragraph <-  set_of_paragraphs(pot("The p-values associated with the Ljung-Box test should be large for all values of") +  
                                pot(" m ",  textProperties(font.style = "italic", font.size=10, font.family="Calibri")) + 
                                pot("considered."))
doc = addParagraph(doc, n1$paragraph, stylename="BulletList")



#---- Model Diagnostic Plots

doc = addPageBreak(doc)

for (i in 1:length(n1$fits)) {

## fits[[i]]$age   gives you the age 
n1$figurelegend <- paste0("Model diagnostics for the naive model fit corresponding to the ",
                       tolower(n1$fits[[i]]$age), " component of the ", 
                       n1$stockname, " ", n1$stockspecies, " stock. ", 
                       "The naive model was based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " value corresponding to the previous year.")

n1$myplot <- n1$diagnostics.naiveone.model.fit(n1$fits,i)


doc = addPlot(doc,
            fun=plot,   ## WRONG
            x=n1$myplot,
            width=plotwidth+1, height=plotheight)
            
doc = addParagraph(doc, value=n1$figurelegend, stylename="rPlotLegend")

n1$myplot <- NULL 

}


#================================================================================================
# Forecasting Results
#================================================================================================

doc = addPageBreak(doc)

doc = addTitle(doc, "Forecasting Results", level=1)

n1$paragraph <- pot("This section reports the forecasting results for the ") + 
             pot(paste(n1$stockspecies)) + 
             pot(" ") + 
             pot(n1$stockname) + 
             pot(" stock ") + 
             pot("corresponding to the forecasting year ") + 
             pot(paste(n1$forecastingyear,".", sep="")) + 
             pot("The results were produced via naive modeling based on the previous year's ") + 
             pot(paste(tolower(n1$stockabundance),".", sep="")) 
n1$paragraph <- set_of_paragraphs(n1$paragraph)
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$paragraph <- pot("Forecasting results are reported numerically and visually for two types of forecasts:")
n1$paragraph <- set_of_paragraphs(n1$paragraph)
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$paragraph <- c("   1) point forecasts;", "   2) interval forecasts.")
doc = addParagraph(doc, n1$paragraph, parent.type="ol", stylename="Normal")
                      
n1$paragraph <- pot("A point forecast is simply a number which represents our best guess") +
             pot("of the future value of the age-specific or total") + 
             pot(paste(tolower(n1$stockabundance))) + 
             pot("for the stock of interest based on available historical data.")
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$paragraph <- pot("An interval forecast is not a single number, rather it is a range of values ") + 
             pot("in which we expect the future value of an age-specific or total ") + 
             pot(paste(tolower(n1$stockabundance))) + 
             pot("series to fall with some (prespecified) probability.")
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$paragraph <- pot("A couple of remarks are in order in connection with an interval forecast:") 
n1$paragraph <- set_of_paragraphs(n1$paragraph)
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$paragraph <- c(paste("The width of the interval forecast conveys information regarding forecast uncertainty", 
                     "(the wider the interval forecast, the more uncertain the forecast);"),
               "The interval forecast conveys more information than the associated point forecast.")
doc = addParagraph(doc, n1$paragraph, stylename="BulletList")
 
if (bootmethod=="meboot") {
n1$paragraph <- pot("The interval forecast provided in this report for each ") + 
             pot(paste(tolower(n1$stockabundance))) + 
             pot(" time series was obtained by applying maximum entropy bootstrapping to that series. ") +
             pot("The maximum entropy bootstrap constructs a large number B (say, B=999) ") +
             pot("of replicates of a dependent time series using an algorithm designed ") + 
             pot("to satisfy the ergodic theorem (i.e., the grand mean of all replicates is close to the sample mean of the original time series). ") +
             pot("The algorithm can accommodate both stationary and non-stationary time series. ") +
             pot("The constructed replicates retain the basic shape (i.e., local peaks and throughs) of the original time series. ") + 
             pot("They also retain the time dependence structure of the autocorrelation function (ACF) ") +
             pot("and the partial autocorrelation function (PACF) of the original time series.")
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")
                    
n1$paragraph <- pot("Given a ") + 
             pot(paste(tolower(n1$stockabundance))) + 
             pot(" time series and the B maximum entropy bootstrap replicates of the series, the 80% interval forecast ") + 
             pot("for the next (future) value of the series is obtained by following the steps below: ") 
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")
          
              
n1$paragraph <- c("   a) Apply naive forecasting to each of the B maximum entropy bootstrap replicates in order to forecast the next (future) value of that replicate;",
               "   b) Compute the 10% percentile (P.10) and the 90% percentile (P.90) of the distribution of the B forecasted values derived in the previous step;",
               "   c) Set the lower and upper limits of the 80% interval forecast to be P.10 and P.90, respectively.")
doc = addParagraph(doc, n1$paragraph, parent.style="ol",stylename="Normal")
               
                           
n1$paragraph <- pot("For interval forecast with a negative lower limit, ") +  
             pot("the lower limit is set to zero to ensure the interval forecast includes only positive values.")
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")
} 



if (bootmethod=="stlboot"){
n1$paragraph <- pot("The interval forecast provided in this report for each abundance time series (age-specific or total) was obtained by applying loess bootstrapping to that series.")
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$paragraph <- pot("The loess bootstrapping is a time series bootstrapping method introduced by Bergmeir, Hyndman and Benitez in 2014 in their working paper on ") +
             pot("bagging exponential smoothing methods using the STL decomposition and the Box-Cox transformation. ") + 
             pot("In this method, the time series of annual abundance values which needs to be bootstrapped is first transformed via a Box-Cox transformation. ") +
             pot("The transformed time series is then decomposed into its trend and remainder components using the loess method ") +
             pot("(i.e., a smoothing method based on local linear regression). ") +
             pot("Finally, the remainder component is bootstrapped using the moving block bootstrap (MBB), ") +
             pot("the trend and seasonal components are added back, and the Box-Cox transformation is inverted. ") +
             pot("In this way, a random pool of J similar bootstrapped time series is generated from the original time series.")
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")             

n1$paragraph <- pot("Given a ") + 
             pot(paste(tolower(n1$stockabundance))) + 
             pot(" time series and the J loess bootstrap replicates of the series, the 80% interval forecast ") + 
             pot("for the next (future) value of the series is obtained by following the steps below: ") 
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")
          
n1$paragraph <- c("   a) Apply naive forecasting to each of the J loess bootstrap replicates in order to forecast the next (future) value of that replicate;",
               "   b) Compute the 10% percentile (P.10) and the 90% percentile (P.90) of the distribution of the J forecasted values derived in the previous step;",
               "   c) Set the lower and upper limits of the 80% interval forecast to be P.10 and P.90, respectively.")
doc = addParagraph(doc, n1$paragraph, parent.style="ol",stylename="Normal")
               
n1$paragraph <- pot("For interval forecast with a negative lower limit, ") +  
             pot("the lower limit is set to zero to ensure the interval forecast includes only positive values.")
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

n1$paragraph <- pot("Note that, if the historical abundance data for a specific age class includes too many 0 and/or 1 values, ") +  
             pot("the loess bootstrapping for that age will fail, in the sense of producing bootstrapping point forecasts whose values are huge and thereby implausible. ") + 
             pot("Currently, such situations are handled by reverting to the maximum entropy bootstrap for the problematic age(s).")   
n1$paragraph <- set_of_paragraphs(n1$paragraph)             
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

}

             
#----- Point Forecasts --------------------------------------------------------------------------      

doc = addTitle(doc, "Point Forecasts", level=2)

n1$tabledata <- n1$results.point.forecast.naiveone    ## this object is created in the review code file

n1$tabledata[nrow(n1$tabledata)+1, ] <- n1$tabledata[nrow(n1$tabledata), ]

n1$tabledata <- transform(n1$tabledata, Age = as.character(Age))

str(n1$tabledata)

n1$tabledata[nrow(n1$tabledata), "Age"] <-  "Total"
n1$tabledata[nrow(n1$tabledata), "Model"] <-  ""
n1$tabledata <- transform(n1$tabledata, p = round(p))
n1$tabledata[nrow(n1$tabledata), "p"] <-  sum(n1$tabledata[1:(nrow(n1$tabledata)-1), "p"])

usePackage("scales")
n1$tabledata <- transform(n1$tabledata, p = comma(p))


names(n1$tabledata)[names(n1$tabledata)=="Age"] <- "Terminal Run"
names(n1$tabledata)[names(n1$tabledata)=="Model"] <- "Model"
names(n1$tabledata)[names(n1$tabledata)=="RY"] <- "Forecasting Year"
names(n1$tabledata)[names(n1$tabledata)=="p"] <- "Point Forecast"

n1$tabledata$Model

usePackage("stringr")
n1$tabledata$Model <- str_replace_all(n1$tabledata$Model, "Past", "Previous")

n1$tablecaption <- paste("Point forecasts of the ",
                      max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                      " age-specific and total",
                      " ",
                      paste0(tolower(n1$stockabundance),"s",collapse=" "),
                      " for the ",
                      n1$stockname,
                      " ",
                      tolower(n1$stockspecies),
                      " stock. ",
                      "The point forecasts for the age-specific ",
                      tolower(n1$stockabundance),
                      " were produced via naive models based on the ",
                      paste0(tolower(n1$stockabundance),"s",collapse=" "),
                      " from the previous year. ",
                      "The point forecast for the total ",
                      tolower(n1$stockabundance),
                      " was obtained by totaling the age-specific point forecasts produced by these naive models. ",
                      "All point forecasts were rounded to the nearest integer for reporting purposes.",
                      sep="")


doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")
    
n1$MyFTable = FlexTable(data=n1$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
    
doc = addFlexTable(doc, n1$MyFTable)
          

#----- Barplot of historical abundance values and associated point forecast: Individual Ages ---------------------------------------

n1$fits <- n1$naiveone.model.fits

n1$pointforecasts <- n1$point.forecast.naiveone(n1$datalist, n1$naiveone.model.fits)

for (i in 1:length(n1$pointforecasts)){

  print(i)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = plot,   # print 
        x = n1$barplot.forecasted.values.individual.ages.naiveone(n1$fits, n1$pointforecasts,i),
        width=6.5, height=6)

   n1$age <- n1$naiveone.model.fits[[i]]$age
   n1$age <- tolower(n1$age)

   n1$figurecaption <- paste("Historical ",
                       #terminal run
                       tolower(n1$stockabundance),
                       " values and ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " point forecast ",
                       "corresponding to the ", n1$age, " component of the ",
                       # "terminal run ,
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The ",  max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " point forecast was derived via the naive model based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       sep="")


   doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

}

 
#----- Barplot of historical abundance values and associated point forecast: Total Age ---------------------------------------

doc = addPageBreak(doc)

n1$results <- n1$results.total.age.retro.predictive.performance.naiveone
n1$pointforecasts <- n1$point.forecast.naiveone(n1$datalist, n1$naiveone.model.fits)

doc = addPlot(doc, 
              fun = plot, # print,
              x = n1$barplot.forecasted.values.total.age.naiveone(n1$results, n1$pointforecasts),
              width=6.5, height=6)
              
n1$figurecaption <- paste("Historical total ",
                       tolower(n1$stockabundance),
                       " values",
                       " and corresponding ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The",
                       " point forecast of total ",
                       tolower(n1$stockabundance),
                       " was obtained by totaling the point forecasts of the age-specific ",
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " produced by",
                       " the naive models based on the ",
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       sep="")

doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")


#---- Forecast Intervals ------------------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, "Interval Forecasts", level=2)

n1$tabledata <- rbind(n1$PI.individual.ages.naiveone, n1$PI.total.age.naiveone)

n1$tabledata

## tabledata <- subset(tabledata, select=-PI.med)

n1$tabledata$PI <- paste( n1$tabledata[,"PI.lwr"]," - ", n1$tabledata[,"PI.upr"], sep="")

n1$tabledata <- subset(n1$tabledata, select=-PI.lwr)
n1$tabledata <- subset(n1$tabledata, select=-PI.upr)

names(n1$tabledata)[names(n1$tabledata)=="PI.ctr"] <- "Point Forecast"
names(n1$tabledata)[names(n1$tabledata)=="PI"] <- "Interval Forecast"

n1$nms <- c(as.character(n1$results.point.forecast.naiveone$Age), "Total")
n1$mds <- c(as.character(n1$results.point.forecast.naiveone$Model), "")
n1$yr <-  c(n1$results.point.forecast.naiveone$RY, unique(n1$results.point.forecast.naiveone$RY))

n1$tabledata <- data.frame(nms=n1$nms, mds=n1$mds, yr=n1$yr, n1$tabledata)

names(n1$tabledata)[names(n1$tabledata)=="nms"] <- "Terminal Run"
names(n1$tabledata)[names(n1$tabledata)=="mds"] <- "Model"
names(n1$tabledata)[names(n1$tabledata)=="yr"] <- "Return Year"
names(n1$tabledata)[names(n1$tabledata)=="Point.Forecast"] <- "Point Forecast"
names(n1$tabledata)[names(n1$tabledata)=="Interval.Forecast"] <- "Interval Forecast"

usePackage("stringr")
n1$tabledata$Model <- str_replace_all(n1$tabledata$Model, "Past", "Previous")


if (n1$bootmethod == "meboot") {
   n1$tabletext <- " Interval forecasts were obtained by maximum entropy bootstrap."
}

if (n1$bootmethod == "stlboot") {
   n1$tabletext <- " Interval forecasts were obtained by loess bootstrap."
}


n1$tablecaption <- paste("Point forecasts and associated 80% interval forecasts of the ",
                      max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                                              # " terminal run ",
                      " ",
                      tolower(n1$stockabundance),
                      " for the ",
 				              n1$stockname,
                		  " ",
                		  tolower(n1$stockspecies),
                		  " stock.",
                      # min(ages)," - ",max(ages),
                      #     " and the total age. ",
                          ## " Negative lower limits for the interval forecasts were truncated to zero to ",
                          ## "ensure the interval forecasts only include positive values. ",
                          " The point forecasts for age-specific components of ",
                          # terminal run
                          tolower(n1$stockabundance),
                          " were obtained via naive models based on the ",
                          # terminal run
                          tolower(n1$stockabundance),
                          " from the previous year.",
                          " The point forecast for total ",
                          # terminal run
                          tolower(n1$stockabundance),
                          " was obtained by adding up the point forecasts for the age-specific components of ",
                          # terminal run
                          tolower(n1$stockabundance),
                          " produced by the naive models.",
                          n1$tabletext, 
                          sep="")

doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")
    
## rm(tabletext)
   
n1$tabletext <- NULL
    
n1$MyFTable = FlexTable(data=n1$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))
    
doc = addFlexTable(doc, n1$MyFTable)


### Visualization of forecast intervals - scatterplots (individual ages)

doc = addPageBreak(doc)

for (i in 1:length(n1$naiveone.model.fits)){

    if (n1$bootmethod=="meboot") {
       n1$figuretext <- " The interval forecast was obtained by maximum entropy bootstrap."
    }
    
    if (n1$bootmethod=="stlboot") {
       n1$figuretext <- " The interval forecast was obtained by loess bootstrap."
    }

    n1$figurecaption <- paste("Historical ",
                           # terminal run
                           tolower(n1$stockabundance),
                           " values along with the ",
                            max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                          " point forecast and 80% interval forecast of the ",
                       # " terminal run",
                        tolower(n1$stockabundance),
                       " corresponding to the ",
                       tolower(n1$naiveone.model.fits[[i]]$age),
                       " component of the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The point forecast was obtained via the naive model based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       n1$figuretext,
                       sep="")

   
       n1$fits <- n1$naiveone.model.fits
       n1$pointforecasts <- n1$point.forecast.naiveone(n1$datalist, n1$naiveone.model.fits)
       n1$intervalforecasts <-   n1$PI.individual.ages.naiveone.no.comma

       n1$myplot <- n1$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.naiveone(n1$fits, n1$pointforecasts, n1$intervalforecasts,i)

    
       
       doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth+1, height=plotheight-2)
            
        doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

        ## rm(myplot)
        
        n1$myplot <- NULL 


}



### Visualization of forecast intervals - scatterplot(total age)

if (n1$bootmethod=="meboot") {
       n1$figuretext <- " The interval forecast was obtained by maximum entropy bootstrap."
}
    
if (n1$bootmethod=="stlboot") {
       n1$figuretext <- " The interval forecast was obtained by loess bootstrap."
}

n1$figurelegend <- paste("Historical total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " values ",
                       # "(", "ages ", min(ages), " - ", max(ages), ")",
                       "and corresponding ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " point forecast and 80% forecast interval for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The point forecast for total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       # terminal run",
                        tolower(n1$stockabundance),
                       " produced by the naive models based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       n1$figuretext,
                       sep="")


n1$results <- n1$results.total.age.retro.predictive.performance.naiveone

n1$pointforecasts <- n1$point.forecast.naiveone(n1$datalist, n1$naiveone.model.fits)

n1$intervalforecasts <-  n1$PI.total.age.naiveone.no.comma

n1$myplot <- n1$scatterplot.forecasted.values.and.forecast.intervals.total.age.naiveone(n1$results, n1$pointforecasts, n1$intervalforecasts)



doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n1$figurelegend, stylename="rPlotLegend")

## rm(myplot)

n1$myplot <- NULL 


#--- Bootstrap Distribution of Point Forecasts - Age-Specific Components

doc = addPageBreak(doc)

if (n1$bootmethod=="meboot") {
    n1$figuretext1 <- "derived on the basis of maximum entropy bootstrapping for the forecasting year "
}

if (n1$bootmethod=="stlboot") {
    n1$figuretext1 <- "derived on the basis of loess bootstrapping for the forecasting year "
}

## if (bootmethod=="meboot") {
##     figuretext2 <- "The interval forecasts were also derived on the basis of maximum entropy bootstrapping."
## }

## if (bootmethod=="stlboot") {
##     figuretext2 <- "The interval forecasts were also derived on the basis of loess bootstrapping."
## }

n1$figurelegend <- paste("Distributions of bootstrapped forecasted ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " values ",
                       "for specific age components of the ",
                       paste(n1$stockname," ", tolower(n1$stockspecies),
                       " stock, ", n1$figuretext1, 
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       ".",
                       " The dashed red line indicates the position of the point forecast on the horizontal axis,",
                       " while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecasts were obtained via naive modeling based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",  
                       # figuretext2,
                       # " The interval forecasts were obtained by maximum entropy bootstrap.",
                       sep="")

## rm(figuretext1)
## rm(figuretext2)

n1$figuretext1 <- NULL 


n1$myplot <- n1$plot.distribution.bootstrapped.point.forecasts.individual.ages.naiveone(n1$PI.individual.ages.naiveone.sim,
                                                                        n1$PI.individual.ages.naiveone.no.comma, 
                                                                        n1$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n1$figurelegend, stylename="rPlotLegend")

# rm(myplot)

n1$myplot <- NULL 

#--- Bootstrap Distribution of Point Forecasts - Total Age


if (n1$bootmethod=="meboot") {
    n1$figuretext1 <- "derived on the basis of maximum entropy bootstrapping for the forecasting year "
}

if (n1$bootmethod=="stlboot") {
    n1$figuretext1 <- "derived on the basis of loess bootstrapping for the forecasting year "
}

n1$figurelegend <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " corresponding to the ",
                       paste(n1$stockname," ", tolower(n1$stockspecies),
                       " stock, ",  n1$figuretext1, 
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       ".",
                       " The dashed red line indicates the position of the point forecast of total ", 
                       tolower(n1$stockabundance), 
                       " on the horizontal axis, ",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ", 
                       tolower(n1$stockabundance),  
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       tolower(n1$stockabundance), 
                       " produced by the naive models based on the ", 
                       paste(tolower(n1$stockabundance)), 
                       " from the previous year.",
                       sep="")

n1$myplot <- n1$plot.distribution.bootstrapped.point.forecasts.total.age.naiveone(n1$PI.total.age.naiveone.sim, n1$PI.total.age.naiveone.no.comma, n1$stockabundance)

# rm(figuretext1)

n1$figuretext1 <- NULL 

doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth, height=plotheight-3)
            
doc = addParagraph(doc, value=n1$figurelegend, stylename="rPlotLegend")

## rm(myplot)

n1$figuretext1 <- NULL 

#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

n1$empirical.probability.yboot.naiveone.total.age <- function(PI.total.age.naiveone.sim, PI.total.age.naiveone.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.naiveone.sim
     mylabel <- paste("Total", stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.naiveone.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


n1$emp.prob.naiveone.total.age <- n1$empirical.probability.yboot.naiveone.total.age(n1$PI.total.age.naiveone.sim, 
                                                                              n1$PI.total.age.naiveone.no.comma, 
                                                                              n1$stockabundance)



n1$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(n1$stockabundance)," ",
                       "value yet to be observed in ",
                       n1$forecastingyear, " for the ",
                       n1$stockname," ",
                       n1$stockspecies, " stock",
                        " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(n1$stockabundance), ".")

doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")

n1$tt_1 <- n1$emp.prob.naiveone.total.age$prob.thresholds

n1$tt_2 <- n1$emp.prob.naiveone.total.age$prob.point.forecast

n1$tt_1_and_2 <- rbind.data.frame(n1$tt_1, n1$tt_2)

usePackage("plyr")

## n1$tt_arrange <- arrange(n1$tt_1_and_2, n1$prob.threshold)

n1$tt_arrange <- n1$tt_1_and_2[order(n1$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )


n1$from_tmp = which(n1$tt_arrange[,1] == n1$emp.prob.naiveone.total.age$prob.point.forecast$prob.threshold)
n1$tt_arrange[n1$from_tmp, 4] <- n1$tt_arrange[n1$from_tmp + 1, 4]


n1$tt_arrange[,1] <- comma(n1$tt_arrange[,1])
n1$tt_arrange[,2] <- paste0(n1$tt_arrange[,2],"%")
n1$tt_arrange[,3] <- paste0(n1$tt_arrange[,3],"%")


names(n1$tt_arrange)[1] <- "Threshold"
names(n1$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(n1$tt_arrange)[3] <- "Prob(Actual >= Threshold)"


names(n1$tt_arrange)[4] <- "Interval Probability"
n1$tt_arrange[1,4] <- "-"


n1$my_ft <- FlexTable( data = n1$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
n1$my_ft[, 1:ncol(n1$tt_arrange)] = parProperties(text.align = "right")

## n1$my_ft[n1$tt_arrange$Threshold %in% comma(n1$emp.prob.naiveone.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )
## Mistake?

n1$my_ft = spanFlexTableRows(n1$my_ft, j=4, from = n1$from_tmp, to = n1$from_tmp + 1)

n1$my_ft[n1$tt_arrange$Threshold %in% comma(n1$emp.prob.naiveone.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)


doc = addFlexTable(doc, flextable=n1$my_ft)

doc = addPageBreak(doc)



#---- Forecast Performance Measures ---------------------------------------------------------------

doc = addTitle(doc, value="Retrospective Evaluation of Performance of Point Forecasts", level=1)
  
n1$paragraph <- paste("This section reports the results corresponding to the retrospective evaluation of the performance of the",
                       "point forecasts produced",
                       "by the naive models for the", n1$forecastingyear, "age-specific and total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       "corresponding to the",
                       n1$stockname, n1$stockspecies, "model stock.",
                       "The naive models were based on the",  
                       paste(tolower(n1$stockabundance)),
                       "from the previous year.",
                      sep=" ")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")


n1$paragraph <- paste("The retrospective evaluation of the performance of the point forecasts assessed how well the naive model",
                       "performed when used for retrospective forecasting of the historical",
                       tolower(n1$stockabundance), "values.",
                       "For this evaluation, the naive model was fit to all of the historical",
                       tolower(n1$stockabundance), "values",
                       "available prior to a given historical return year, then the fitted model was used",
                       "to forecast the",
                       # terminal run
                       tolower(n1$stockabundance),
                       "for that year.",
                       "This evaluation captures how well the model would have performed in practice year over year",
                       "and was performed separately for each age-specific",
                        tolower(n1$stockabundance),
                        "and for the total",
                        tolower(n1$stockabundance),
                        ".",
                      sep=" ")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")


n1$paragraph <- paste("Retrospective forecast errors were defined as the actual",
                       # terminal run
                       tolower(n1$stockabundance),
                       "values minus the retrospectively forecasted",
                       # "terminal run values.",
                        tolower(n1$stockabundance),
                       "values.",
                       "In view of this definition, positive values for the retrospective forecast errors represent forecasts that were too low,",
                       "whereas negative values represent forecasts that were too high.",
                      sep=" ")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")


n1$paragraph <- "The following retrospective measures were used to characterize different aspects of the retrospective forecasting errors:"
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

    
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


n1$paragraph <- paste("MAE and MAPE reflect the overall forecast accuracy accounting for systematic bias and year-to-year variation.")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

        
n1$paragraph <- paste("MRE and MPE reflect directional bias in raw and relative forecast errors, respectively, with negative values indicating a tendency to",
                       "underforecast and positive values reflecting a tendency to overforecast.",
                      sep=" ")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")


n1$paragraph <- paste("Just like MAE, RMSE  is a measure of the absolute magnitude of the raw retrospective forecast errors, but is more sensitive to large values",
                       "then MAE.", sep=" ")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

    
n1$paragraph <- paste("MASE was proposed by Hyndman and Koehler (2006) as a generally applicable, scale-free measure of forecast accuracy.",
                       "This measure never gives infinite or undefined values.",
                       "In this report, MASE is computed as the average of the absolute values of the scaled retrospective forecast errors produced by",
                       "the naive model based on the",
                       # terminal run
                       tolower(n1$stockabundance),
                       "from the previous year.",
                       "The scaling of the errors involves dividing the errors by the MAE computed from the retrospective forecast errors associated with",
                       "this model.",  
                       # based on the",
                       # terminal run
                       # tolower(n1$stockabundance),
                       # "from the previous year.",
                       # "from the one-step, naive forecasting method.",
                       # "A scaled error is less than 1 if it arises from a better forecast than the one produced by the naive model based on the terminal run for the previous year.",
                       # "Conversely, it is greater than 1 if the forecast is worse than the average one-step, naive forecast computed in-sample.",
                       "The resulting value of MASE will be equal to 1, since the naive model based on the",
                       tolower(n1$stockabundance),
                       "from the previous year",  
                       "is used as a benchmark against which all other forecasting models will be compared in terms of their",
                       "retrospective forecasting performance (as captured by the MASE).",
                      sep=" ")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")

    
n1$paragraph <- paste0("To facilitate the interpretation of the retrospective forecast errors, this section reports several types of plots",
                    "for the age-specific and total", tolower(n1$stockabundance), "s",  
                     ":")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")
  
doc = addParagraph( doc, value = 'Plots illustrating the performance of the retrospective forecasting evaluation;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")      
doc = addParagraph( doc, value = 'Density plots of the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Bias coefficient plots derived from the retrospective forecast errors;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")          
doc = addParagraph( doc, value = 'Barplots of the retrospective forecast errors together with the forecast interval corresponding to the forecasting year of interest.',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
  

## paragraph <- paste("We also calculated the coefficient of determination obtained by regressing the retrospectively forecasted",
##                       tolower(n1$stockabundance),
##                       "values",
##                       "on the historical",
##                       # terminal run
##                       tolower(n1$stockabundance),
##                       "values.",
##                       "This is simply the squared correlation coefficient of the retrospectively forecasted and historical",
##                       # terminal run
##                        tolower(n1$stockabundance),
##                       "values.",
##                       sep=" ")
## doc = addParagraph(doc, paragraph, stylename="Normal")

 
n1$paragraph <- paste0("Bias coefficients representing a new metric for forecast bias. ", 
                   "These coefficients are computed from the retrospective forecast errors for the age-specific and total ",
                   tolower(n1$stockabundance),"s", 
                   " using the formula developed by Kourentzes, Trapero and Svetunkov in their 2014 working paper ", 
                   "\"Measuring the behaviour of experts on demand forecasting: a complex task\". ", 
                   "In the context of this report, the bias coefficients describe the direction and magnitude of the retrospective forecast bias ", 
                   "associated with the naive forecasting method (previous year).")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")


n1$paragraph <- paste0("Generally speaking, the bias coefficients are unit-free and bounded between -1 (maximum negative retrospective forecast bias) ",  
                    "and 1 (maximum positive retrospective forecast bias). ", 
                    "A forecasting method that is always producing retrospective point forecasts which are over the observed historical values ", 
                    "will have a bias coefficient equal to -1, always over-forecasting. ", 
                    "A forecasting method that is always producing retrospective point forecasts which are under the observed historical values will ", 
                    "have a bias coefficient equal to 1, always under-forecasting. Given the bounded nature of the bias coefficient, ", 
                    "we can describe a forecasting method as strongly biased if |bias coefficient| > 0.5 and weakly biased if 0 < |bias coefficient| <= 0.5, ", 
                    "providing a simple and intuitive description of the forecast bias behaviour. ", 
                    "If the bias coefficient is equal to 0, the forecasting method is unbiased.")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")
                    

#---- Retrospective Measures of Forecast Performance -------------------------------------

doc = addTitle(doc, value="Retrospective Measures of Forecast Performance", level=2)

n1$tabledata <- n1$M.naiveone

n1$tabledata <- subset(n1$M.naiveone, select=-Model)

n1$tabledata <- cbind(Model=rep("Naive (Previous Year)",nrow(n1$M.naiveone)),n1$tabledata)


n1$tablecaption <- paste("Retrospective measures of forecast performance ",
                "associated with the point forecasts of the ",
                max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                " age-specific and total ",
                # terminal runs
                paste0(tolower(n1$stockabundance),"s",collapse=" "),
                " for the ",
                n1$stockname,
                " ",
                tolower(n1$stockspecies),
                " stock.",
                " The point forecasts of age-specific ",
                # terminal runs
                paste0(tolower(n1$stockabundance),"s",collapse=" "),
                " were obtained via naive modeling based on the ",
                # terminal run
                tolower(n1$stockabundance),
                " from the previous year.",
                " The point forecast for the total ",
                # terminal run
                 tolower(n1$stockabundance),
                " was obtained by adding up the point forecasts for the age-specific components of ",
                # terminal run",
                 tolower(n1$stockabundance),
                " produced by the naive models based on the ",
                # terminal run
                 tolower(n1$stockabundance),
                " from the previous year.",
                sep=""
                )

names(n1$tabledata)[names(n1$tabledata)=="Model"] <- "Model Class"

doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

n1$tt <- n1$tabledata

usePackage("stringr")
names(n1$tt) <- str_replace_all(names(n1$tt),"_"," ")

## tt[,-1] <- comma(tt[,-1])

n1$my_ft <- FlexTable( data = n1$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
n1$my_ft[, 1:ncol(n1$tt)] = parProperties(text.align = "right")

n1$my_ft = addFooterRow(n1$my_ft, value = paste("Legend:  MRE = Mean Relative Error; ", 
                                           "MAE = Mean Absolute Error; ", 
                                           "MPE = Mean Percentage Error; ", 
                                           "MAPE = Mean Absolute Percentage Error; ",
                                           "MASE = Mean Scaled Error; ",
                                           "MSE = Root Mean Square Error."), 
        colspan = ncol(n1$tt), 
        text.properties = textProperties(font.size = 9),
        par.properties = parProperties(text.align = "left"))
        
## my_ft = addFooterRow( my_ft, value = c("         MAE = Mean Absolute Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MPE = Mean Percentage Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MAPE = Mean Absolute Percentage Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         MASE = Mean Scaled Error;"), colspan = ncol(tt))
## my_ft = addFooterRow( my_ft, value = c("         RMSE = Root Mean Square Error."), colspan = ncol(tt))


doc = addFlexTable(doc, flextable=n1$my_ft)
          
#---- Retrospective Forecast Errors:  Individual Ages ------------------------------------------------------------

doc = addPageBreak(doc)

doc = addTitle(doc, "Retrospective Point Forecasts and Forecast Errors", level=2)


for (i in 1:length(n1$results.individual.ages.retro.predictive.performance.naiveone)) {

    print(i)

    n1$results <- n1$results.individual.ages.retro.predictive.performance.naiveone

    n1$tabledata <- n1$results[[i]]$data.retro

   
    n1$age <- names(n1$results)[i]
    usePackage("stringr")
    n1$age <- str_extract(n1$age,"[[:digit:]]+")
    
    n1$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the age ",n1$age,
                          " component of the ",
 				              max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                		  # " terminal run for the ",
                		  " ",
                      tolower(n1$stockabundance),
                      " for the ",
                      n1$stockname,
                		  " ",
                		  tolower(n1$stockspecies),
                		  " stock. ",
                          "Accompanying return years and actual ",
                     # terminal run
                     tolower(n1$stockabundance),
                     " values are also reported.",
                      " The retrospective point forecasts were obtained via naive modeling based on the ",
                     # terminal run
                     tolower(n1$stockabundance),
                     " from the previous year.",
                          sep="")

    n1$tabledata$p <- round(n1$tabledata$p,2)
    n1$tabledata$e <- round(n1$tabledata$e,2)

    usePackage("scales")
    n1$tabledata$a <- comma(round(n1$tabledata$a))
    n1$tabledata$p <- comma(round(n1$tabledata$p))
    n1$tabledata$e <- comma(round(n1$tabledata$e))

    names(n1$tabledata)[names(n1$tabledata)=="cy"] <- "Return Year"
    names(n1$tabledata)[names(n1$tabledata)=="a"] <-  "Actual"
    names(n1$tabledata)[names(n1$tabledata)=="p"] <-  "Forecast"
    names(n1$tabledata)[names(n1$tabledata)=="e"] <-  "Error"


    n1$tabledata
    
    ### tabledata <- subset(tabledata, select=c(-p.bench, -e.bench))
    
    doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")

    baseCellProp = cellProperties( padding = 4)

    n1$tt <- n1$tabledata


    usePackage("stringr")
    names(n1$tt) <- str_replace_all(names(n1$tt),"_"," ")

    ## tt[,-1] <- comma(tt[,-1])

    n1$my_ft <- FlexTable( data = n1$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
    )

    # overwrites some paragraph formatting properties
    n1$my_ft[, 1:ncol(n1$tt)] = parProperties(text.align = "right")

    doc = addFlexTable(doc, flextable=n1$my_ft)
    
    doc = addPageBreak(doc)
    
}

    
#---- Retrospective Forecast Errors:  Total Age ------------------------------------------------------------


n1$results <- n1$results.total.age.retro.predictive.performance.naiveone

n1$tabledata <- data.frame(cy = n1$results$data.retro[[1]]$cy,
                        a = n1$results$a.total.retro,
                        p = n1$results$p.total.retro,
                        e = n1$results$e.total.retro)


n1$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the ",
 				              max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                		  " total ",
                      # terminal run
                      tolower(n1$stockabundance),
                      " for the ",
                		  n1$stockname,
                		  " ",
                		  tolower(n1$stockspecies),
                		  " stock. ",
                          "Accompanying return years and actual total ",
                      # terminal run
                      tolower(n1$stockabundance),
                      " values are also reported.",
                      " The retrospective point forecasts for the total ",
                      # terminal run
                       tolower(n1$stockabundance),
                      " were obtained by adding up the retrospective point forecasts for the age-specific components of ",
                      # terminal run
                      tolower(n1$stockabundance),
                      " produced by the naive models based on the ",
                      # terminal run
                       tolower(n1$stockabundance),
                      " from the previous year.",
                          sep="")

usePackage("scales")
n1$tabledata$a <- comma(round(n1$tabledata$a))
n1$tabledata$p <- comma(round(n1$tabledata$p))
n1$tabledata$e <- comma(round(n1$tabledata$e))

names(n1$tabledata)[names(n1$tabledata)=="cy"] <- "Return Year"
names(n1$tabledata)[names(n1$tabledata)=="a"] <-  "Actual"
names(n1$tabledata)[names(n1$tabledata)=="p"] <-  "Forecast"
names(n1$tabledata)[names(n1$tabledata)=="e"] <-  "Error"


doc = addParagraph(doc, value=n1$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

n1$tt <- n1$tabledata


usePackage("stringr")
names(n1$tt) <- str_replace_all(names(n1$tt),"_"," ")


## tt[,-1] <- comma(tt[,-1])

n1$my_ft <- FlexTable( data = n1$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
n1$my_ft[, 1:ncol(n1$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=n1$my_ft)


#---- Illustration of how well the retrospective forecast evaluation works: Individual ages ----------

for (j in 1:length(n1$naiveone.model.fits)) {

  print(j)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = print,
        x = n1$individual.ages.retro.plot.naiveone(n1$individual.ages.retro.plot.info.naiveone, n1$stockabundance, j),
        width=plotwidth, height=plotheight)

       ## Actual values (grey dots) and retrospectively forecasted values (red dots) of the age 2 component of terminal run for the SPR chinook salmon stock, 
       ## derived on the basis of exponential smoothing modeling. Historical values of age-specific Terminal Run (grey lines) 
       ## and fitted values produced by the exponential smoothing modeling (red lines) are also shown. Each panel corresponds to a particular retrospective forecasting year.

  n1$figurecaption  <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                       tolower(n1$naiveone.model.fits[[j]]$age),
                       " component of the ",
                       # max(results.afe.total.age.retro.naiveone$CY)+1,
                       # " terminal run
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock,"),
                       " derived via naive modeling based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year. ",
                       "Historical values of ",  
                       tolower(n1$naiveone.model.fits[[j]]$age), " ",  
                       tolower(n1$stockabundance),
                       " (grey lines) and fitted values produced by the naive modeling (red lines)",
                       " are also shown. ", 
                       "Each panel corresponds to a particular retrospective forecasting year.",   
                       sep="")



   doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")
   
   n1$figurecaption <- NULL 

}


#---- Illustration of how well the retrospective forecast evaluation works: Total age ----------

doc = addPageBreak(doc)

doc = addPlot(doc, 
        fun = print,
        x = n1$total.age.retro.plot.naiveone(n1$total.age.retro.plot.info.naiveone, n1$stockabundance),
        width=plotwidth, height=plotheight)

       ## Actual values (grey dots) and retrospectively forecasted values (red dots) of the age 2 component of terminal run for the SPR chinook salmon stock, 
       ## derived on the basis of exponential smoothing modeling. Historical values of age-specific Terminal Run (grey lines) 
       ## and fitted values produced by the exponential smoothing modeling (red lines) are also shown. Each panel corresponds to a particular retrospective forecasting year.

n1$figurecaption  <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                        "total ", 
                       # max(results.afe.total.age.retro.naiveone$CY)+1,
                       # " terminal run
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock,"),
                       " derived via naive modeling based on the total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year. ",
                       "Historical values of total ",    
                       tolower(n1$stockabundance),
                       " (grey lines) and fitted values produced by the naive modeling (red lines)",
                       " are also shown. ", 
                       "Each panel corresponds to a particular retrospective forecasting year.",   
                       sep="")

doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

n1$figurecaption <- NULL 


#---- Retrospective Forecast Errors: Age-Specific Density Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

n1$figurecaption <- paste("Density plots of the retrospective forecast errors derived from the naive models used to forecast ",
                       "specific age components of the ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " ",
                       # " terminal run for the ",
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The naive models were based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       sep="")

n1$myplot <- n1$dens.results.afe.individual.ages.retro.naiveone(
               n1$naiveone.model.fits,
               n1$results.individual.ages.retro.predictive.performance.naiveone
               )

doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

## rm(myplot)

n1$myplot <- NULL 
n1$figurecaption <- NULL 
   
#---- Retrospective Forecast Errors: Total Age Density Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

n1$figurecaption <- paste("Density plot of the retrospective forecast errors ",
                       "involved in forecasting the total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                        " The naive models were based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       sep="")


n1$myplot <- n1$dens.results.afe.total.age.retro.naiveone(n1$results.afe.total.age.retro.naiveone)

doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth, height=plotheight-3)
            
doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

## rm(myplot)

n1$myplot <- NULL 


#---- Retrospective Forecast Errors: Age-Specific Bias Coefficient Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

n1$figurecaption <- paste("Bias coefficient plots obtained from the retrospective forecast errors derived from the naive models used to forecast ",
                       "specific age components of the ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " ",
                       # " terminal run for the ",
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The naive models were based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       sep="")

n1$myplot <- n1$bias.coefficients.afe.individual.ages.retro.naiveone(n1$naiveone.model.fits,
                                                     n1$results.individual.ages.retro.predictive.performance.naiveone,
                                                     n1$stockabundance)
                                                        
doc = addPlot(doc,
            fun=grid.draw,
            x=n1$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

n1$myplot <- NULL 
n1$figurecaption <- NULL 

#---- Retrospective Forecast Errors: Total Age Bias Coefficient Plot ---------------------------------------------------------------

doc = addPageBreak(doc)

n1$figurecaption <- paste("Bias coefficient plot obtained from the retrospective forecast errors derived from the naive models used to forecast the ",
                       max(n1$results.afe.total.age.retro.naiveone$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock."),
                       " The naive models were based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       sep="")

n1$myplot <- n1$bias.coefficient.afe.total.age.retro.naiveone(n1$results.total.age.retro.predictive.performance.naiveone,
                                               n1$stockabundance)
                                                        
doc = addPlot(doc,
            fun=grid.draw,
            x=n1$myplot,
            width=plotwidth, height=plotheight)
            
doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

## rm(myplot)

n1$myplot <- NULL 
n1$figurecaption <- NULL 

#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Specific Ages ----------

n1$results.retro <- n1$results.individual.ages.retro.predictive.performance.naiveone 
n1$results.pred <- n1$pred.int.individual.ages.naiveone
 
for (i in 1:length(n1$naiveone.model.fits)) {

  print(i)              
  
  doc = addPageBreak(doc)

  doc = addPlot(doc, 
        fun = print,
        x = n1$gary.plot.individual.ages(n1$results.retro, n1$results.pred, i),
        width=6.5, height=6)

  if (n1$bootmethod == "meboot") {
   n1$figuretext <- "The interval forecast was obtained by maximum entropy bootstrap."
  }

  if (n1$bootmethod == "stlboot") {
   n1$figuretext <- "The interval forecast was obtained by loess bootstrap."
  }

  n1$figurecaption  <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       tolower( n1$naiveone.model.fits[[i]]$age),
                       " component of the ",
                       # max(results.afe.total.age.retro.naiveone$CY)+1,
                       # " terminal run
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock,"),
                       " derived via naive modeling based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       n1$figuretext, 
                       sep="")

   doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

   n1$figurecaption <- NULL 

}



#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Total Age ---------------------------------------------------------------

doc = addPageBreak(doc)

n1$results.retro <- n1$results.total.age.retro.predictive.performance.naiveone
names(n1$results.retro)
n1$results.pred <- n1$PI.total.age.naiveone.no.comma 

doc = addPlot(doc, 
        fun = print,
        x = n1$gary.plot.total.age(n1$results.retro, n1$results.pred),
        width=6.5, height=6) 
 
if (n1$bootmethod == "meboot") {
   n1$figuretext <- "The interval forecast was obtained by maximum entropy bootstrap."
}

if (n1$bootmethod == "stlboot") {
   n1$figuretext <- "The interval forecast was obtained by loess bootstrap."
}

 
n1$figurecaption <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       # "the ",
                       # max(results.afe.total.age.retro.naiveone$CY)+1,
                       " total ",
                       # terminal run ",
                       tolower(n1$stockabundance),
                       " for the ",
                       paste(n1$stockname,tolower(n1$stockspecies), "stock,"),
                       " derived via naive modeling based on the ",
                       tolower(n1$stockabundance), 
                       " from the previous year. ",
                       n1$figuretext, 
                       sep="")

doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

## rm(figuretext)

n1$figurecaption <- NULL 

n1$figuretext <- NULL 

doc = addPageBreak(doc)

doc = addTitle( doc, "Forecast Diagnostics", level = 1)


n1$paragraph <- paste0("This section reports two types of forecast diagnostic plots for age-specific and total", 
                    tolower(n1$stockabundance), ":")
doc = addParagraph(doc, n1$paragraph, stylename="Normal")
  
doc = addParagraph( doc, value = 'Superimposed time series plots of retrospectively forecasted and actual abundance values;',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
doc = addParagraph( doc, value = 'Scatter plots of retrospectively forecasted versus actual abundance values.',
      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")      

#--- Forecast Diagnostics - Time Series Plots of Retrospectively Forecasted Values - Individual Ages -----------

doc = addPageBreak( doc )

n1$figurecaption <- paste("Superimposed time series plots of retrospectively forecasted and actual ",
                       #### terminal runs
                       ## paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       ## " and actual terminal runs for specific age ",
                       tolower(n1$stockabundance), " values ",  
                       "for specific age ", 
                       "components of the ",
                       n1$stockname, " ",
                       tolower(n1$stockspecies),
                       " stock. ",
                       ## "Observations in each panel are labeled according to the associated historical return years. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " for a given return year was derived by applying naive modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       " The naive modeling was based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       sep=""
                       )


n1$myplot <- n1$timeseries.plot.results.afe.individual.ages.retro.naiveone(n1$results.individual.ages.retro.predictive.performance.naiveone, 
                                                           n1$stockabundance)



doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth+1, height=plotheight)
            
doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

# rm(myplot)

n1$myplot <- NULL 
n1$figurecaption <- NULL 

#--- Forecast Diagnostics - Scatter Plots of Retrospectively Forecasted Values - Individual Ages -----------

n1$figurecaption <- paste("Scatter plots of retrospectively forecasted versus actual ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       "for specific age ",
                       "components of the ",
                       n1$stockname, " ",
                       tolower(n1$stockspecies),
                       " stock. ",
                       "Observations in each panel are labeled according to the associated historical return years. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " for a given return year was derived by applying naive modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       " The naive modeling was based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year.",
                       sep=""
                       )


n1$myplot <-  n1$scatter.plot.results.afe.individual.ages.retro.naiveone(n1$results.individual.ages.retro.predictive.performance.naiveone)


doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth+1, height=plotheight)
            
doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

## rm(myplot)

n1$myplot <- NULL 
n1$figurecaption <- NULL 

#---- Forecast Diagnostics - Total Age ------------------------------------------------------------------------

doc = addPageBreak(doc)

n1$figurecaption <- paste("Superimposed time series plot of retrospectively forecasted and actual total ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance)," values",collapse=" "),
                       ## " versus actual total ",
                       # terminal runs ",
                       ## paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n1$results.afe.total.age.retro.naiveone$CY),
                       " - ",
                       max(n1$results.afe.total.age.retro.naiveone$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       ## "Observations are labeled according to the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " value for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " Naive modeling based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")


n1$myplot <- n1$timeseries.plot.results.afe.total.age.retro.naiveone(n1$results.total.age.retro.predictive.performance.naiveone, n1$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

## rm(myplot)

n1$myplot <- NULL 
n1$figurecaption <- NULL 

#---- Forecast Diagnostics - Total Age ------------------------------------------------------------------------

doc = addPageBreak(doc)

n1$figurecaption <- paste("Scatter plot of retrospectively forecasted total ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance)," values ",collapse=" "),
                       " versus actual total ",
                       # terminal runs ",
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(n1$results.afe.total.age.retro.naiveone$CY),
                       " - ",
                       max(n1$results.afe.total.age.retro.naiveone$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " Naive modeling based on the ",
                       # terminal run
                       tolower(n1$stockabundance),
                       " from the previous year was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(n1$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")


n1$myplot <- n1$scatter.plot.results.afe.total.age.retro.naiveone(n1$results.total.age.retro.predictive.performance.naiveone)

doc = addPlot(doc,
            fun=print,
            x=n1$myplot,
            width=plotwidth+1, height=plotheight-2)
            
doc = addParagraph(doc, value=n1$figurecaption, stylename="rPlotLegend")

## rm(myplot)

n1$myplot <- NULL 
n1$figurecaption <- NULL 


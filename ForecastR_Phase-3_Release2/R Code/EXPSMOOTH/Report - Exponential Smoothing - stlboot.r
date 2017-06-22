#========================================================================================================
# CoverPage
#========================================================================================================

EXPSMOOTH$fits <- EXPSMOOTH$expsmooth.model.fits


usePackage("stringr")
EXPSMOOTH$stockabundance <- str_replace(EXPSMOOTH$stockabundance,"_"," ") 


EXPSMOOTH$pot1 = pot("ForecastR Output Report", textProperties(font.weight="bold", font.size = 40) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot1)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )

EXPSMOOTH$pot1 <- NULL 
EXPSMOOTH$my.pars <- NULL 

EXPSMOOTH$pot1 = pot(" ", textProperties(font.weight="bold", font.size = 20) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot1)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )

EXPSMOOTH$pot1 <- NULL 
EXPSMOOTH$my.pars <- NULL 

EXPSMOOTH$pot2 =  pot("Stock Name: ", textProperties(font.weight="bold", font.size = 20) ) +
        pot(paste(EXPSMOOTH$stockname), textProperties(font.size = 20) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot2)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )

EXPSMOOTH$pot2 <- NULL 
EXPSMOOTH$my.pars <- NULL 

EXPSMOOTH$pot3 =  pot("Stock Species: ", textProperties(font.weight="bold", font.size = 20) ) +
        pot(paste(EXPSMOOTH$stockspecies), textProperties(font.size = 20) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot3)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )

EXPSMOOTH$pot3 <- NULL 
EXPSMOOTH$my.pars <- NULL 

EXPSMOOTH$pot4 =  pot("Abundance Measure: ", textProperties(font.weight="bold", font.size = 20) ) +
        pot(paste(EXPSMOOTH$stockabundance), textProperties(font.size = 20) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot4)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )

EXPSMOOTH$pot4 <- NULL 
EXPSMOOTH$my.pars <- NULL 

EXPSMOOTH$pot5 =  pot("Forecasting Year: ", textProperties(font.weight="bold", font.size = 20) ) +
        pot(paste(EXPSMOOTH$forecastingyear), textProperties(font.size = 20) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot5)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )


EXPSMOOTH$pot5 <- NULL 
EXPSMOOTH$my.pars <- NULL 

EXPSMOOTH$pot6 =  pot("Forecasting Model: ", textProperties(font.weight="bold", font.size = 20))
EXPSMOOTH$pot9 = pot(paste("Exponential Smoothing Model"), textProperties(font.size = 20) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot6, EXPSMOOTH$pot9)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )


EXPSMOOTH$pot6 <- NULL 
EXPSMOOTH$pot9 <- NULL 
EXPSMOOTH$my.pars <- NULL 

EXPSMOOTH$pot6a =  pot("Time Series Bootstrapping Method: ", textProperties(font.weight="bold", font.size = 20))
EXPSMOOTH$pot9a = pot(paste("Loess Bootstrap"), textProperties(font.size = 20) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot6a, EXPSMOOTH$pot9a)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )

EXPSMOOTH$pot6a <- NULL
EXPSMOOTH$pot9a <- NULL  
EXPSMOOTH$my.pars <- NULL 

EXPSMOOTH$pot13 =  pot("Date: ", textProperties(font.weight="bold", font.size = 20) ) +
        pot(paste(Sys.Date()), textProperties(font.size = 20) )
EXPSMOOTH$my.pars = set_of_paragraphs(EXPSMOOTH$pot13)
doc = addParagraph( doc, value = EXPSMOOTH$my.pars, stylename="Normal" )

EXPSMOOTH$pot13 <- NULL 
EXPSMOOTH$my.pars <- NULL 

doc = addPageBreak(doc)


#=================================================================================================
# Summary of Results
#=================================================================================================

doc = addTitle(doc, "Summary of Results", level=1)

## point forecasts + individual ages
EXPSMOOTH$results.point.forecast.expsmooth

EXPSMOOTH$tabledata <- matrix(NA, nrow=11, ncol=length(EXPSMOOTH$fits)+2)
## EXPSMOOTH$tabledata <- matrix(NA, nrow=10, ncol=length(EXPSMOOTH$fits)+2)

colnames(EXPSMOOTH$tabledata) <- rep("Name", length(EXPSMOOTH$fits)+2)
colnames(EXPSMOOTH$tabledata)[1] <- "Item"
colnames(EXPSMOOTH$tabledata)[2:(1+length(EXPSMOOTH$fits))] <- as.character(EXPSMOOTH$results.point.forecast.expsmooth$Age)
colnames(EXPSMOOTH$tabledata)[2+length(EXPSMOOTH$fits)] <- "Total"


EXPSMOOTH$tabledata[1,] <- c("Return Year", rep(unique(EXPSMOOTH$results.point.forecast.expsmooth$RY),
                   length(EXPSMOOTH$fits)+1))

EXPSMOOTH$tabledata[2,] <- c("Model", c(EXPSMOOTH$results.point.forecast.expsmooth$Model," - "))

EXPSMOOTH$tabledata[3,] <- c("Point Forecast",
                   c(as.character(comma(round(EXPSMOOTH$results.point.forecast.expsmooth$p))),
                     as.character(comma(sum(round(EXPSMOOTH$results.point.forecast.expsmooth$p))))))


EXPSMOOTH$PI.individual.ages.expsmooth
EXPSMOOTH$PI.total.age.expsmooth

EXPSMOOTH$PI.combined <- rbind(EXPSMOOTH$PI.individual.ages.expsmooth, EXPSMOOTH$PI.total.age.expsmooth)

EXPSMOOTH$PI.combined.vec <- NULL
for (i in 1:nrow(EXPSMOOTH$PI.combined)){

   EXPSMOOTH$tmp.vec <- paste0(EXPSMOOTH$PI.combined[i,"PI.lwr"]," - ",EXPSMOOTH$PI.combined[i,"PI.upr"])

   EXPSMOOTH$PI.combined.vec <- c(
      EXPSMOOTH$PI.combined.vec, EXPSMOOTH$tmp.vec)

}


EXPSMOOTH$tabledata[4,] <- c("Interval Forecast", EXPSMOOTH$PI.combined.vec)

## EXPSMOOTH$tabledata <- M.expsmooth
## EXPSMOOTH$tabledata <- subset(M.expsmooth, select=-Model)

EXPSMOOTH$M.sub.expsmooth <- subset(EXPSMOOTH$M.expsmooth, select=-Model)

EXPSMOOTH$tabledata[5:(5 + nrow(EXPSMOOTH$M.sub.expsmooth) - 1),"Item"] <- as.character(EXPSMOOTH$M.sub.expsmooth$Measure)

for (k in 2:ncol(EXPSMOOTH$M.sub.expsmooth)){
    EXPSMOOTH$tabledata[5:(5 + nrow(EXPSMOOTH$M.sub.expsmooth) - 1),k] <- EXPSMOOTH$M.sub.expsmooth[,k]
}



EXPSMOOTH$fits <-  EXPSMOOTH$expsmooth.model.fits
EXPSMOOTH$results <- EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth
EXPSMOOTH$results.total.age <- EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth

## r.squared.table <- r.squared.values(fits, results, results.total.age)

## EXPSMOOTH$tabledata[5 + nrow(M.sub.expsmooth),"Item"] <- "R-squared"

## EXPSMOOTH$tabledata[5 + nrow(M.sub.expsmooth),-1] <- paste0(as.character(r.squared.table$R.squared),"%")

EXPSMOOTH$bias.coeff.table <- c(round(EXPSMOOTH$bias.coeff.afe.individual.ages.retro.expsmooth,4),
                      round(EXPSMOOTH$bias.coeff.afe.total.age.retro.expsmooth,4))

EXPSMOOTH$tabledata[5 + nrow(EXPSMOOTH$M.sub.expsmooth),"Item"] <- "Bias Coefficient"

EXPSMOOTH$tabledata[5 + nrow(EXPSMOOTH$M.sub.expsmooth),-1] <-  EXPSMOOTH$bias.coeff.table

EXPSMOOTH$tabledata <- as.data.frame(EXPSMOOTH$tabledata)


EXPSMOOTH$tabledata <- as.data.frame(EXPSMOOTH$tabledata)


EXPSMOOTH$tablecaption <- paste("Summary of forecasting results for the",
                      EXPSMOOTH$forecastingyear,
                      "age-specific and total",
                      # terminal run
                      paste(tolower(EXPSMOOTH$stockabundance),"s",collapse="", sep=""),
                      " associated with the ",
                      EXPSMOOTH$stockname, " ",
                      tolower(EXPSMOOTH$stockspecies), " stock.")

EXPSMOOTH$tabledata <- EXPSMOOTH$tabledata


doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

EXPSMOOTH$MyFTable = FlexTable(data=EXPSMOOTH$tabledata,
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
EXPSMOOTH$MyFTable <- setFlexTableBorders(EXPSMOOTH$MyFTable,
                                inner.vertical = borderProperties(style = "none"),
                                inner.horizontal = borderProperties(style = "none"),
                                outer.vertical = borderProperties(style = "none"),
                                outer.horizontal = borderProperties(color = "gray5", style = "solid")
)


doc = addFlexTable(doc, EXPSMOOTH$MyFTable)

EXPSMOOTH$MyFTable <- NULL 

#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

EXPSMOOTH$empirical.probability.yboot.expsmooth.total.age <- function(PI.total.age.expsmooth.sim, PI.total.age.expsmooth.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.expsmooth.sim
     mylabel <- paste("Total", EXPSMOOTH$stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.expsmooth.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


EXPSMOOTH$emp.prob.expsmooth.total.age <- EXPSMOOTH$empirical.probability.yboot.expsmooth.total.age(EXPSMOOTH$PI.total.age.expsmooth.sim,
                                                                              EXPSMOOTH$PI.total.age.expsmooth.no.comma,
                                                                              EXPSMOOTH$stockabundance)



EXPSMOOTH$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(EXPSMOOTH$stockabundance)," ",
                       "value yet to be observed in ",
                       EXPSMOOTH$forecastingyear, " for the ",
                       EXPSMOOTH$stockname," ",
                       EXPSMOOTH$stockspecies, " stock",
                       " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(EXPSMOOTH$stockabundance), ".",
                       " Loess bootstrapping was used as a basis for obtaining this distribution.")

doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

EXPSMOOTH$tt_1 <- EXPSMOOTH$emp.prob.expsmooth.total.age$prob.thresholds

EXPSMOOTH$tt_2 <- EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast

EXPSMOOTH$tt_1_and_2 <- rbind.data.frame(EXPSMOOTH$tt_1, EXPSMOOTH$tt_2)

usePackage("plyr")

## EXPSMOOTH$tt_arrange <- arrange(EXPSMOOTH$tt_1_and_2, EXPSMOOTH$prob.threshold)

EXPSMOOTH$tt_arrange <- EXPSMOOTH$tt_1_and_2[order(EXPSMOOTH$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

EXPSMOOTH$from_tmp = which(EXPSMOOTH$tt_arrange[,1] == EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast$prob.threshold)
EXPSMOOTH$tt_arrange[EXPSMOOTH$from_tmp, 4] <- EXPSMOOTH$tt_arrange[EXPSMOOTH$from_tmp + 1, 4]

EXPSMOOTH$tt_arrange[,1] <- comma(EXPSMOOTH$tt_arrange[,1])
EXPSMOOTH$tt_arrange[,2] <- paste0(EXPSMOOTH$tt_arrange[,2],"%")
EXPSMOOTH$tt_arrange[,3] <- paste0(EXPSMOOTH$tt_arrange[,3],"%")
EXPSMOOTH$tt_arrange[,4] <- paste0(EXPSMOOTH$tt_arrange[,4],"%")

names(EXPSMOOTH$tt_arrange)[1] <- "Threshold"
names(EXPSMOOTH$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(EXPSMOOTH$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(EXPSMOOTH$tt_arrange)[4] <- "Interval Probability"
EXPSMOOTH$tt_arrange[1,4] <- "-"

EXPSMOOTH$my_ft <- FlexTable( data = EXPSMOOTH$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
EXPSMOOTH$my_ft[, 1:ncol(EXPSMOOTH$tt_arrange)] = parProperties(text.align = "right")

## EXPSMOOTH$my_ft[EXPSMOOTH$tt_arrange$Threshold %in% comma(EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )


EXPSMOOTH$my_ft = spanFlexTableRows(EXPSMOOTH$my_ft, j=4, from = EXPSMOOTH$from_tmp, to = EXPSMOOTH$from_tmp + 1)

EXPSMOOTH$my_ft[EXPSMOOTH$tt_arrange$Threshold %in% comma(EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)


doc = addFlexTable(doc, flextable=EXPSMOOTH$my_ft)


EXPSMOOTH$my_ft_emp_prob_total   <- EXPSMOOTH$my_ft
EXPSMOOTH$tablecaption_emp_prob_total <- EXPSMOOTH$tablecaption

EXPSMOOTH$my_ft <- NULL 
EXPSMOOTH$tablecaption <- NULL 
EXPSMOOTH$tt_arrange <- NULL
EXPSMOOTH$tt_1 <- NULL 
EXPSMOOTH$tt_2 <- NULL  
EXPSMOOTH$tt_1_and_2 <- NULL

## doc = addPageBreak(doc)


#---- Introduction -------------------------------------------------------------------------


doc = addTitle(doc, "Introduction", level=1)


EXPSMOOTH$paragraph <- paste("In this report, exponential smoothing is used to forecast the age-specific",
                       "and total",  
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "), 
                       "for the",
                       EXPSMOOTH$stockname, EXPSMOOTH$stockspecies, "stock.",
                       sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("Exponential smoothing (ETS) models are a general class of innovations state space models for forecasting a univariate time series.",
                       "The acronym ETS can be thought of as ExponenTial Smoothing, but in effect denotes the error (E), trend (T) and seasonal components (S)",
                       "which can be used to describe the time series to be forecasted.",
                       "The trend component represents the growth or the decline of the time series over an extended period of time.",
                       "For time series defined at time intervals which are fractions of a year (e.g., months), the seasonal component",
                       "is a pattern of change that repeats itself from year to year.",
                       "The error component captures irregular, short-term fluctuations present in the series, which cannot be attributed to the trend and seasonal components.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("Exponential smoothing models can be classified according to the nature of the error, trend and seasonal components of the underlying time series.",
                       "The error (E) component can be either additive (A) or multiplicative (M).",
                       "The trend (T) component can be additive (A), multiplicative (M) or inexistent (N).",
                       "The trend (T) component can also be dampened additively (Ad) or multiplicatively (Md).",
                       "The seasonal (S) component can be either additive (A), multiplicative (M) or inexistent (N).",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("Each particular combination of options for the error, trend and seasonal components of a time series gives rise to a specific exponential smoothing model.",
                       "Since the possibilities for each component are Error = {A,M}, Trend = {N,A,Ad,M,Md} and Seasonal = {N,A,M}, in total there exist 2 x 5 x 3 = 30",
                       " such exponential smoothing models.",
                       "Components designated by the letter N are not present in the time series of interest.  Components designated by the letter A are present and are combined",
                       " with the other components via addition.",
                       "Components designated by the letter M are present and are combined with the other components via multiplication.",
                       sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("For example, the exponential smoothing method ETS(AAN) has E(A), T(A) and S(N) structures, where E(A) stands for",
                       "additive error, T(A) stands for additive trend and S(N) stands for inexistent seasonality.",
                       "One can show that",
                       "ETS(AAN) is Holt's linear method with additive errors.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("A complete list of the 30 exponential smoothing models is available at",
                       "https://www.otexts.org/fpp/7/7",
                       "(Table 7.10).",
                       "This list distinguishes between models with additive errors and models with multiplicative errors.",
                       "Each model consists of a measurement equation which describes the observed time series data",
                       "and some transition equations which describe how the unobserved states of the time series (i.e., level, trend, seasonal) change over time.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("The ets() function from the R package \"forecast\" is used in this report to implement exponential smoothing.",
                       "Given an input time series, the ets() function uses Akaike’s Information Criterion (AIC), corrected for small sample bias,",
                       "to select the optimal exponential smoothing model for that series.",
                       "For ETS models, the AIC is defined as AIC = -2log(L) + 2k,",
                       "where where L  is the likelihood of the model and k is the total number of model parameters and initial states that have been estimated.",
                       "The small sample bias corrected AIC, AICc, is defined as AICc = AIC + 2(k+1)(k+2)/(T-k), where T is the number of observations in the time series.",
                       sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("The input time series for the ets() function considered in this report include the age-specific and total", 
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "), 
                       "for the",
                       EXPSMOOTH$stockname, EXPSMOOTH$stockspecies, "stock.",
                       "For each of these series, the ets() function will produce an optimal exponential smoothing model after performing automated model selection on the",
                       "basis of the AICc criterion.  The optimal model will be such that it will have the smallest AICc value among all candidate models entertained during the",
                       "model selection process.  The ets() function is used with all of the default options, except for the damped option, which is set to FALSE.",
                       sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("The estimated level, trend or seasonal states of the optimal exponential smoothing model are accompanied by smoothing coefficients,",
                       "which depend on the rate of change of these states and reflect how much weight recent time series observations receive over older ones.",
                       "In the output produced by the ets() function, the smoothing coefficients are denoted by alpha, beta and gamma, where alpha is the level smoothing coefficient,",
                       "beta is the trend smoothing coefficient and gamma is the seasonal smoothing coefficient.",
                       "The closer a smoothing coefficient is to 1, the less smoothing is performed, allowing for rapid changes in the corresponding state and heavy reliance",
                       "on recent time series observations.",
                       "Conversely, the closer a smoothing coefficient is to 0, the more smoothing is performed, allowing for gradual changes in the corresponding state",
                       "and less reliance on recent time series observations.",
                       sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 


EXPSMOOTH$paragraph <- paste("The output produced by the ets() function also includes a parameter named sigma,",
                       "which represents the standard deviation of the model errors.",
                       sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL



#-----------------------------------------------------------------------------------------------------
######################################################################################################
#
#  Section:  Data
#
######################################################################################################
#-----------------------------------------------------------------------------------------------------


doc = addTitle(doc, "Data", level=1)

#---- Show original data (by calendar year) ------------------------------------------------------------

EXPSMOOTH$tablecaption <- paste("Historical ",
                      # terminal run
                      tolower(EXPSMOOTH$stockabundance),
                      " data for the ",
                      EXPSMOOTH$stockname, " ",
                      tolower(EXPSMOOTH$stockspecies), " stock,",
                      "reported as a function of calendar year for specific ages of the stock.")

EXPSMOOTH$tabledata <- EXPSMOOTH$datafile
EXPSMOOTH$tabledata[EXPSMOOTH$tabledata<0] <- NA

EXPSMOOTH$datalist

EXPSMOOTH$datalist1 <- EXPSMOOTH$datalist[[1]]
EXPSMOOTH$datalist1 <- subset(EXPSMOOTH$datalist1,select=-BY)
for (i in 2:length(EXPSMOOTH$datalist)){
      EXPSMOOTH$datalist.tmp <- EXPSMOOTH$datalist[[i]]
      EXPSMOOTH$datalist.tmp <- subset(EXPSMOOTH$datalist.tmp,select=-BY)

      EXPSMOOTH$datalist1 <-  merge(EXPSMOOTH$datalist1, EXPSMOOTH$datalist.tmp, by=c("CY"))

}

EXPSMOOTH$datalist1

## EXPSMOOTH$tabledata$Sum <- apply(EXPSMOOTH$tabledata[,-1],1, sum, na.rm=TRUE)

EXPSMOOTH$tabledata <- EXPSMOOTH$datalist1

usePackage("stringr")
names(EXPSMOOTH$tabledata) <- str_replace_all(names(EXPSMOOTH$tabledata),"T","Age ")
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="CY"] <- "Calendar Year"
## names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="Sum"] <- "Total"


usePackage("scales")
EXPSMOOTH$tabledata[,-1] <- comma(EXPSMOOTH$tabledata[,-1] )



doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

EXPSMOOTH$MyFTable = FlexTable(data=EXPSMOOTH$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))

doc = addFlexTable(doc, EXPSMOOTH$MyFTable)

EXPSMOOTH$MyFTable <- NULL 
EXPSMOOTH$tablecation <- NULL 

#---- Plot original data by return year (for specific ages) -----------------------------------------------


EXPSMOOTH$figurecaption <- paste("Plots of historic ",
                       # terminal runs,
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " versus return years for the ",
                       EXPSMOOTH$stockname," ",
                       tolower(EXPSMOOTH$stockspecies), " stock, ",
                       "with each plot corresponding to a specific age component of the stock.",sep="")
doc = addPlot(doc,
        fun = print,
        x = EXPSMOOTH$plot.data.expsmooth(EXPSMOOTH$datalist),
        width=6.5, height=6)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$figurecaption <- NULL 

#-----------------------------------------------------------------------------------------------------
######################################################################################################
#
#  Section:  Modeling Results
#
######################################################################################################
#-----------------------------------------------------------------------------------------------------

#----- Plot Fitted Values Produced by Exponential Smoothing for Each Age Class ------------------------------------------------------------------------------

doc = addTitle(doc, "Exponential Smoothing Results", level=1)

EXPSMOOTH$figurecaption <- paste("Fitted values produced by the exponential smoothing models used to forecast ",
                       "specific age components of the ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       # " terminal run for the",
                       " ",
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       EXPSMOOTH$stockname, " ", tolower(EXPSMOOTH$stockspecies), " stock.",
                       sep="")

doc = addPlot(doc,
        fun = print,
        x = EXPSMOOTH$plot.fitted.expsmooth(EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform),
        width=6.5, height=6)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")


EXPSMOOTH$paragraph <- paste("Stats Tutorial:")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- paste("For each age-specific",
                      # terminal run",
                      tolower(EXPSMOOTH$stockabundance),
                      "time series, compare the fitted values produced by the exponential smoothing model",
                      "against the observed (or historical) values of the",
                      #terminal run
                      tolower(EXPSMOOTH$stockabundance),
                      "time series.  Does the exponential smoothing model appear to provide",
                      "a good fit to the time series?",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

#----- Modeling Diagnostics  (Exponential Smoothing): Checking Randomness of Residuals ------------------------------------------------------------------------------


doc = addTitle(doc, "Exponential Smoothing Modeling Diagnostics", level=1)


for (i in 1:length(EXPSMOOTH$expsmooth.model.fits)) {


     EXPSMOOTH$age <- EXPSMOOTH$expsmooth.model.fits[[i]]$age
     EXPSMOOTH$age <- tolower(EXPSMOOTH$age)

     print(i)

     EXPSMOOTH$figurecaption <- paste("Diagnostic plots for the exponential smoothing model used to forecast the ",
                       EXPSMOOTH$age,
                       " component of the ",
                       # max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       EXPSMOOTH$forecastingyear, " ",
                       # " terminal run ",
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       sep="")


     EXPSMOOTH$myplot <- EXPSMOOTH$diagnostics.expsmooth.model.fit(EXPSMOOTH$fits, EXPSMOOTH$boxcoxtransform, i)

     doc = addPlot(doc,
            fun=plot,   # print,
            x=EXPSMOOTH$myplot,
            width=plotwidth+1, height=plotheight)

     doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

     EXPSMOOTH$myplot <- NULL 


}




doc = addPageBreak(doc)

EXPSMOOTH$paragraph <- paste("Stats Tutorial:",sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- paste("After fitting an exponential smoothing model to a univariate time series,",
                       "we need to run diagnostic tests to validate the model.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("If the exponential smoothing model provides a good fit to a univariate time series, the residuals associated with the model should exhibit",
                       "no systematic patterns and ",
                       "no temporal dependence.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("Useful diagnostic plots for verifying that the exponential smoothing model residuals exhibit no systematic patterns and no temporal dependence include:")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- c("   - Time series plot of the model residuals; ",
               "   - Autocorrelation plot of the model residuals;",
               "   - Partial autocorrelation plot of the model residuals;",
               "   - Plot of p-values associated with the Ljung-Box test applied to the model residuals.")
doc = addParagraph(doc, EXPSMOOTH$paragraph, parent.type="ul", stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$pots <- pot("The Ljung-Box test is a diagnostic tool used to test the lack of fit of an exponential smoothing model. ") +
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

EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$pots)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 


EXPSMOOTH$paragraph <- paste("If an exponential smoothing model provides a good fit to a univariate time series, then:")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- pot("The time series plot of the model residuals should exhibit no systematic patterns;")
EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$paragraph)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="BulletList")

EXPSMOOTH$paragraph <- pot("The autocorrelation plot of the model residuals should show no significant autocorrelations between the residuals;")
EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$paragraph)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="BulletList")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- pot("The partial autocorrelation plot of the model residuals should show no significant partial autocorrelations between the residuals;")
EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$paragraph)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="BulletList")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <-  set_of_paragraphs(pot("The p-values associated with the Ljung-Box test should be large for all values of") +
                                pot(" m ",  textProperties(font.style = "italic", font.size=10, font.family="Calibri")) +
                                pot("considered."))
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="BulletList")

EXPSMOOTH$paragraph <- NULL 

#----- Point Forecast Results ------------------------------------------------------------------------------


doc = addPageBreak(doc)

doc = addTitle(doc, "Forecasting Results", level=1)

EXPSMOOTH$paragraph <- paste("This section reports the ",
                        "forecasting results for the ",
                        EXPSMOOTH$stockspecies," ", EXPSMOOTH$stockname, " stock ",
                        "corresponding to the forecasting year ",EXPSMOOTH$forecastingyear,". ",
                        "The results were produced using exponential smoothing.",
                      sep="")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- pot("Forecasting results are reported numerically and visually for two types of forecasts:")
EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$paragraph)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- c("   1) point forecasts;", "   2) interval forecasts.")
doc = addParagraph(doc, EXPSMOOTH$paragraph, parent.type="ol", stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- pot("A point forecast is simply a number which represents our best guess") +
             pot("of the future value of the age-specific or total ") +
             pot(paste(tolower(EXPSMOOTH$stockabundance))) +
             pot(" for the stock of interest based on available historical data.")
EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$paragraph)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- pot("An interval forecast is not a single number, rather it is a range of values ") +
             pot("in which we expect the future value of an age-specific or total ") +
             pot(paste(tolower(EXPSMOOTH$stockabundance))) +
             pot("series to fall with some (prespecified) probability.")
EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$paragraph)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- pot("A couple of remarks are in order in connection with an interval forecast:")
EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$paragraph)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- c(paste("The width of the interval forecast conveys information regarding forecast uncertainty",
                     "(the wider the interval forecast, the more uncertain the forecast);"),
               "The interval forecast conveys more information than the associated point forecast.")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="BulletList")

EXPSMOOTH$paragraph <- NULL 

## paragraph <- paste("The interval forecast provided in this report for each",
##                       # terminal run
##                       tolower(EXPSMOOTH$stockabundance),
##                       "time series was obtained by applying bootstrapping to that series.",
##                      sep=" ")
## doc = addParagraph(doc, paragraph, stylename="Normal")


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("The interval forecast provided in this report for each", 
                    paste0(tolower(EXPSMOOTH$stockabundance)), 
                    "time series (age-specific or total)",
                   "was obtained by applying loess bootstrapping to that series.",
                   sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 


EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("The loess bootstrapping is a time series bootstrapping method introduced by Bergmeir, Hyndman and Benitez in 2014",
                   "in their working paper on bagging exponential smoothing methods using the STL decomposition and the Box-Cox transformation.",
                   "In this method, the time series of annual abundance values which needs to be bootstrapped is first transformed via a Box-Cox transformation.",
                   "The transformed time series is then",
                   "decomposed into its trend and remainder components using the loess method (i.e., a smoothing method based on local linear regression).",
                   "Finally, the remainder component is bootstrapped using the moving block bootstrap (MBB),",
                   "the trend and seasonal components are added back, and the Box-Cox transformation is inverted.",
                   "In this way, a random pool of similar bootstrapped time series is generated from the original time series.",
                   sep=" ")
EXPSMOOTH$paragraph <- set_of_paragraphs(EXPSMOOTH$paragraph)
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

#--- Stats tutorial on bootstrap for Exponential Smoothing Model --------------------------------------------------

#--- Point forecast results ------------------------------------------------------------------------


doc = addTitle(doc, "Point Forecasts", level=2)

EXPSMOOTH$tabledata <- EXPSMOOTH$results.point.forecast.expsmooth    ## this object is created in the review code file

EXPSMOOTH$tabledata[nrow(EXPSMOOTH$tabledata)+1, ] <- EXPSMOOTH$tabledata[nrow(EXPSMOOTH$tabledata), ]

EXPSMOOTH$tabledata <- transform(EXPSMOOTH$tabledata, Age = as.character(Age))

str(EXPSMOOTH$tabledata)

EXPSMOOTH$tabledata[nrow(EXPSMOOTH$tabledata), "Age"] <-  "Total"
EXPSMOOTH$tabledata[nrow(EXPSMOOTH$tabledata), "Model"] <-  ""
EXPSMOOTH$tabledata <- transform(EXPSMOOTH$tabledata, p = round(p))
EXPSMOOTH$tabledata[nrow(EXPSMOOTH$tabledata), "p"] <-  sum(EXPSMOOTH$tabledata[1:(nrow(EXPSMOOTH$tabledata)-1), "p"])

usePackage("scales")
EXPSMOOTH$tabledata <- transform(EXPSMOOTH$tabledata, p = comma(p))


names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="Age"] <- "Terminal Run"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="Model"] <- "Model"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="RY"] <- "Forecasting Year"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="p"] <- "Point Forecast"

EXPSMOOTH$tabledata$Model

usePackage("stringr")
EXPSMOOTH$tabledata$Model <- str_replace_all(EXPSMOOTH$tabledata$Model, "Past", "Previous")


EXPSMOOTH$tablecaption <- paste("Point forecasts of the ",
                      max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                      " age-specific and total ",
                      " ",
                      # terminal runs
                      paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                      " for the ",
                       EXPSMOOTH$stockname,
                       " ",
                       tolower(EXPSMOOTH$stockspecies),
                       " stock. ",
                       "The point forecasts for the age-specific ",
                       # terminal runs,
                       tolower(EXPSMOOTH$stockabundance),
                       " were produced via exponential smoothing. ",
                       "The point forecast for the total ",
                       # terminal run ",
                       tolower(EXPSMOOTH$stockabundance),
                       " was obtained by totaling the age-specific point forecasts. ",
                       "All point forecasts were rounded to the nearest integer for reporting purposes.",
                       sep=""
                       )


doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

EXPSMOOTH$MyFTable = FlexTable(data=EXPSMOOTH$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))

doc = addFlexTable(doc, EXPSMOOTH$MyFTable)


#####################################################################################################################
### Visualize point forecasts - barplots (specific age components)
#####################################################################################################################

EXPSMOOTH$fits <- EXPSMOOTH$expsmooth.model.fits
EXPSMOOTH$pointforecasts <- EXPSMOOTH$point.forecast.expsmooth(EXPSMOOTH$datalist, EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform)

for (i in 1:length(EXPSMOOTH$expsmooth.model.fits)) {


     EXPSMOOTH$age <- EXPSMOOTH$expsmooth.model.fits[[i]]$age
     EXPSMOOTH$age <- tolower(EXPSMOOTH$age)

     doc = addPlot(doc,
     fun = plot, # print,
     x = EXPSMOOTH$barplot.forecasted.values.individual.ages.expsmooth(EXPSMOOTH$fits,EXPSMOOTH$pointforecasts,i),
     width=6.5, height=6)

     EXPSMOOTH$figurecaption <- paste("Historical ",
                       tolower(EXPSMOOTH$stockabundance),
                       " values and ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " point forecast ",
                       "corresponding to the ", EXPSMOOTH$age, " component of the ",
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       " The ",  max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " point forecast was derived via exponential smoothing.",
                       sep="")

      doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

      EXPSMOOTH$figurecaption <- NULL 

}



#################################################################################################
### Visualize point forecasts - barplot (total age)
#################################################################################################


EXPSMOOTH$figurecaption <- paste("Historical total ",
                       tolower(EXPSMOOTH$stockabundance),
                       " values",
                       " and corresponding ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " point forecast ",
                       "for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       " The point forecast of total ",
                       tolower(EXPSMOOTH$stockabundance),
                       " was obtained by totaling the point forecasts of the age-specific ",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " produced via",
                       " exponential smoothing.",
                       sep="")


EXPSMOOTH$results <- EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth

EXPSMOOTH$pointforecasts <- EXPSMOOTH$point.forecast.expsmooth(EXPSMOOTH$datalist, EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform)


doc = addPlot(doc,
              fun = plot, # print,
              x = EXPSMOOTH$barplot.forecasted.values.total.age.expsmooth(EXPSMOOTH$results, EXPSMOOTH$pointforecasts),
              width=6.5, height=6)



doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")


#---- Forecast Intervals ------------------------------------------------------------------------


doc = addPageBreak(doc)

doc = addTitle(doc, "Interval Forecasts", level=2)

EXPSMOOTH$tabledata <- rbind(EXPSMOOTH$PI.individual.ages.expsmooth, EXPSMOOTH$PI.total.age.expsmooth)

EXPSMOOTH$tabledata

## EXPSMOOTH$tabledata <- subset(EXPSMOOTH$tabledata, select=-PI.med)

EXPSMOOTH$tabledata$PI <- paste( EXPSMOOTH$tabledata[,"PI.lwr"]," - ", EXPSMOOTH$tabledata[,"PI.upr"], sep="")

EXPSMOOTH$tabledata <- subset(EXPSMOOTH$tabledata, select=-PI.lwr)
EXPSMOOTH$tabledata <- subset(EXPSMOOTH$tabledata, select=-PI.upr)

names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="PI.ctr"] <- "Point Forecast"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="PI"] <- "Interval Forecast"

EXPSMOOTH$nms <- c(as.character(EXPSMOOTH$results.point.forecast.expsmooth$Age), "Total")
EXPSMOOTH$mds <- c(as.character(EXPSMOOTH$results.point.forecast.expsmooth$Model), "")
EXPSMOOTH$yr <-  c(EXPSMOOTH$results.point.forecast.expsmooth$RY, unique(EXPSMOOTH$results.point.forecast.expsmooth$RY))

EXPSMOOTH$tabledata <- data.frame(nms=EXPSMOOTH$nms, mds=EXPSMOOTH$mds, yr=EXPSMOOTH$yr, EXPSMOOTH$tabledata)

names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="nms"] <- "Terminal Run"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="mds"] <- "Model"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="yr"] <- "Return Year"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="Point.Forecast"] <- "Point Forecast"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="Interval Forecast"] <- "Interval Forecast"

usePackage("stringr")
EXPSMOOTH$tabledata$Model <- str_replace_all(EXPSMOOTH$tabledata$Model, "Past", "Previous")


EXPSMOOTH$tablecaption <- paste("Point forecasts and associated 80% interval forecasts of the ",
                      max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                                              # " terminal run ",
                      " ",
                      tolower(EXPSMOOTH$stockabundance),
                      " for the ",
 				              EXPSMOOTH$stockname,
                		  " ",
                		  tolower(EXPSMOOTH$stockspecies),
                		  " stock.",
                      # min(ages)," - ",max(ages),
                      #     " and the total age. ",
                          ## " Negative lower limits for the interval forecasts were truncated to zero to ",
                          ## "ensure the interval forecasts only include positive values. ",
                          " The point forecasts for age-specific components of ",
                          # terminal run
                          tolower(EXPSMOOTH$stockabundance),
                          " were obtained via exponential smoothing. ",
                          " The point forecast for total ",
                          # terminal run
                          tolower(EXPSMOOTH$stockabundance),
                          " was obtained by adding up the point forecasts for the age-specific components of ",
                          # terminal run
                          tolower(EXPSMOOTH$stockabundance),
                          ".",
                          " Interval forecasts were obtained by loess bootstrap.",
                          sep="")

doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

EXPSMOOTH$MyFTable = FlexTable(data=EXPSMOOTH$tabledata, header.columns = TRUE,
                         header.cell.props = cellProperties(padding=2),
                         header.par.props = parProperties(text.align="right"),
                         body.par.props = parProperties(text.align="right"),
                         body.cell.props = cellProperties(padding=2))

doc = addFlexTable(doc, EXPSMOOTH$MyFTable)



### Visualization of forecast intervals - scatterplots (individual ages)


for (i in 1:length(EXPSMOOTH$expsmooth.model.fits)){


    doc = addPageBreak(doc)

    EXPSMOOTH$figurecaption <- paste("Historical ",
                           # terminal run
                           tolower(EXPSMOOTH$stockabundance),
                           " values along with the ",
                            max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                          " point forecast and 80% interval forecast of the ",
                       # " terminal run",
                        tolower(EXPSMOOTH$stockabundance),
                       " corresponding to the ",
                       tolower(EXPSMOOTH$expsmooth.model.fits[[i]]$age),
                       " component of the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       " The point forecast was obtained by exponential smoothing.",
                       " The interval forecast was obtained by loess bootstrap.",
                       sep="")


     EXPSMOOTH$fits <- EXPSMOOTH$expsmooth.model.fits
     EXPSMOOTH$pointforecasts <- EXPSMOOTH$point.forecast.expsmooth(EXPSMOOTH$datalist, EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform)
     EXPSMOOTH$intervalforecasts <-   EXPSMOOTH$PI.individual.ages.expsmooth.no.comma

     EXPSMOOTH$myplot <- EXPSMOOTH$scatterplot.forecasted.values.and.forecast.intervals.individual.ages.expsmooth(EXPSMOOTH$fits, EXPSMOOTH$pointforecasts, EXPSMOOTH$intervalforecasts,i)

     doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth+1, height=plotheight-2)

    doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

    EXPSMOOTH$myplot <- NULL 

}


### Visualization of forecast intervals - scatterplot(total age)


EXPSMOOTH$figurecaption <- paste("Historical total ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " values ",
                       # "(", "ages ", min(ages), " - ", max(ages), ")",
                       "and corresponding ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " point forecast and 80% forecast interval for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       " The point forecast for total ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       # terminal run",
                        tolower(EXPSMOOTH$stockabundance),
                       " produced by exponential smoothing. ",
                       " The interval forecast was obtained by loess bootstrap.",
                       sep="")


EXPSMOOTH$results <- EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth

EXPSMOOTH$pointforecasts <- EXPSMOOTH$point.forecast.expsmooth(EXPSMOOTH$datalist, EXPSMOOTH$expsmooth.model.fits, EXPSMOOTH$boxcoxtransform)

EXPSMOOTH$intervalforecasts <-  EXPSMOOTH$PI.total.age.expsmooth.no.comma

EXPSMOOTH$myplot <- EXPSMOOTH$scatterplot.forecasted.values.and.forecast.intervals.total.age.expsmooth(EXPSMOOTH$results, EXPSMOOTH$pointforecasts, EXPSMOOTH$intervalforecasts)


doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth+1, height=plotheight-2)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 


#--- Bootstrap Distribution of Point Forecasts - Age-Specific Components



EXPSMOOTH$figurecaption <- paste("Distributions of bootstrapped forecasted ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " values ",
                       "for specific age components of the ",
                       paste(EXPSMOOTH$stockname," ", tolower(EXPSMOOTH$stockspecies),
                       " stock, derived on the basis of loess bootstrapping for the forecasting year ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       ".",
                       "For each age component, the dashed red line indicates the position of the point forecast on the horizontal axis,",
                       " while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast was obtained by exponential smoothing.",
                       sep="")


## myplot <- hist.results.afe.individual.ages.retro.expsmooth(
##     expsmooth.model.fits,
##      results.individual.ages.retro.predictive.performance.expsmooth)


## DEBUGGED: June 13, 2016

EXPSMOOTH$myplot <- EXPSMOOTH$plot.distribution.bootstrapped.point.forecasts.individual.ages.expsmooth(EXPSMOOTH$PI.individual.ages.expsmooth.sim,
                                                                       EXPSMOOTH$PI.individual.ages.expsmooth.no.comma,
                                                                       EXPSMOOTH$expsmooth.model.fits,  EXPSMOOTH$boxcoxtransform,
                                                                       EXPSMOOTH$extract_ages,
                                                                       EXPSMOOTH$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 



#--- Bootstrap Distribution of Point Forecasts - Total Age


EXPSMOOTH$figurecaption <- paste("Distribution of bootstrapped forecasted values ",
                       "for the total ",
                       tolower(EXPSMOOTH$stockabundance),
                       " corresponding to the ",
                       paste(EXPSMOOTH$stockname," ", tolower(EXPSMOOTH$stockspecies),
                       " stock, derived on the basis of loess bootstrapping for the forecasting year ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       ".",
                       " The dashed red line indicates the position of the point forecast of total ", 
                       tolower(EXPSMOOTH$stockabundance), 
                       " on the horizontal axis,",
                       "while the blue segment indicates the 80% forecast interval.", sep=""),
                       " The point forecast for the total ",
                       tolower(EXPSMOOTH$stockabundance),
                       " was obtained by adding up the point forecasts for the age-specific components of ",
                       tolower(EXPSMOOTH$stockabundance),
                       " produced by exponential smoothing.",
                       sep="")


EXPSMOOTH$results <- EXPSMOOTH$results.afe.total.age.retro.expsmooth

## myplot <- hist.results.afe.total.age.retro.expsmooth(results)

EXPSMOOTH$myplot <- EXPSMOOTH$plot.distribution.bootstrapped.point.forecasts.total.age.expsmooth(EXPSMOOTH$PI.total.age.expsmooth.sim,
                                                               EXPSMOOTH$PI.total.age.expsmooth.no.comma,
                                                               EXPSMOOTH$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth, height=plotheight-3)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 

#====================================================================================================================
#
# Empirical Probabilities for Total Age
#
#====================================================================================================================

doc = addPageBreak(doc)

###
### Empirical Probabilities:  Distribution of Bootstrapped Point Forecasts for Total Age
###

EXPSMOOTH$empirical.probability.yboot.expsmooth.total.age <- function(PI.total.age.expsmooth.sim, PI.total.age.expsmooth.no.comma, stockabundance){


     y.star.boot.stacked <- PI.total.age.expsmooth.sim
     mylabel <- paste("Total", EXPSMOOTH$stockabundance)
     labels.stacked <-  rep(mylabel, length(y.star.boot.stacked))

     data.stacked <- data.frame(y.star.boot=y.star.boot.stacked, labels=labels.stacked)

     pfct <- PI.total.age.expsmooth.no.comma[["PI.ctr"]] ## point forecast of total abundance


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


EXPSMOOTH$emp.prob.expsmooth.total.age <- EXPSMOOTH$empirical.probability.yboot.expsmooth.total.age(EXPSMOOTH$PI.total.age.expsmooth.sim,
                                                                              EXPSMOOTH$PI.total.age.expsmooth.no.comma,
                                                                              EXPSMOOTH$stockabundance)


EXPSMOOTH$tablecaption <- paste0("Estimated probabilities that the actual total ",
                       tolower(EXPSMOOTH$stockabundance)," ",
                       "value yet to be observed in ",
                       EXPSMOOTH$forecastingyear, " for the ",
                       EXPSMOOTH$stockname," ",
                       EXPSMOOTH$stockspecies, " stock",
                          " is less than or equal to a specific threshold, ",
                       " greater than a specific threshold ",
                       " or falls between two specific thresholds ",
                       " (i.e., the threshold located on the previous row and the one located on the current row of the table). ", 
                       "Probabilities are derived from the distribution of bootstrapped point forecasts of ",
                       "total ",
                       tolower(EXPSMOOTH$stockabundance), ".",
                       " Loess bootstrapping was used as a basis for obtaining this distribution.")

doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

EXPSMOOTH$tt_1 <- EXPSMOOTH$emp.prob.expsmooth.total.age$prob.thresholds

EXPSMOOTH$tt_2 <- EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast

EXPSMOOTH$tt_1_and_2 <- rbind.data.frame(EXPSMOOTH$tt_1, EXPSMOOTH$tt_2)

usePackage("plyr")

## EXPSMOOTH$tt_arrange <- arrange(EXPSMOOTH$tt_1_and_2, EXPSMOOTH$prob.threshold)

EXPSMOOTH$tt_arrange <- EXPSMOOTH$tt_1_and_2[order(EXPSMOOTH$tt_1_and_2$prob.threshold),]

## tt_arrange <- tt_1_and_2

# set cell padding defaut to 2
baseCellProp = cellProperties( padding = 2 )

EXPSMOOTH$from_tmp = which(EXPSMOOTH$tt_arrange[,1] == EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast$prob.threshold)
EXPSMOOTH$tt_arrange[EXPSMOOTH$from_tmp, 4] <- EXPSMOOTH$tt_arrange[EXPSMOOTH$from_tmp + 1, 4]

EXPSMOOTH$tt_arrange[,1] <- comma(EXPSMOOTH$tt_arrange[,1])
EXPSMOOTH$tt_arrange[,2] <- paste0(EXPSMOOTH$tt_arrange[,2],"%")
EXPSMOOTH$tt_arrange[,3] <- paste0(EXPSMOOTH$tt_arrange[,3],"%")
EXPSMOOTH$tt_arrange[,4] <- paste0(EXPSMOOTH$tt_arrange[,4],"%")

names(EXPSMOOTH$tt_arrange)[1] <- "Threshold"
names(EXPSMOOTH$tt_arrange)[2] <- "Prob(Actual <= Threshold)"
names(EXPSMOOTH$tt_arrange)[3] <- "Prob(Actual >= Threshold)"

names(EXPSMOOTH$tt_arrange)[4] <- "Interval Probability"
EXPSMOOTH$tt_arrange[1,4] <- "-"

EXPSMOOTH$my_ft <- FlexTable( data = EXPSMOOTH$tt_arrange,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
EXPSMOOTH$my_ft[, 1:ncol(EXPSMOOTH$tt_arrange)] = parProperties(text.align = "right")

## EXPSMOOTH$my_ft[EXPSMOOTH$tt_arrange$Threshold %in% comma(EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast[1]), ] = cellProperties( background.color = "orange" )


EXPSMOOTH$my_ft = spanFlexTableRows(EXPSMOOTH$my_ft, j=4, from = EXPSMOOTH$from_tmp, to = EXPSMOOTH$from_tmp + 1)

EXPSMOOTH$my_ft[EXPSMOOTH$tt_arrange$Threshold %in% comma(EXPSMOOTH$emp.prob.expsmooth.total.age$prob.point.forecast[1]), 1:3] = 
       cellProperties( background.color = "orange",  padding=2)


doc = addFlexTable(doc, flextable=EXPSMOOTH$my_ft)


EXPSMOOTH$my_ft_emp_prob_total   <- EXPSMOOTH$my_ft
EXPSMOOTH$tablecaption_emp_prob_total <- EXPSMOOTH$tablecaption

EXPSMOOTH$my_ft <- NULL 
EXPSMOOTH$tablecaption <- NULL 
EXPSMOOTH$tt_arrange <- NULL
EXPSMOOTH$tt_1 <- NULL 
EXPSMOOTH$tt_2 <- NULL  
EXPSMOOTH$tt_1_and_2 <- NULL

doc = addPageBreak(doc)

#---- Forecast Performance Measures ---------------------------------------------------------------


## doc = addPageBreak(doc)


doc = addTitle(doc, value="Retrospective Evaluation of Performance of Point Forecasts", level=1)


EXPSMOOTH$paragraph <- paste("This section reports the results corresponding to the retrospective evaluation",
                       "of the performance of the",
                       "point forecasts produced",
                       "by the exponential smoothing models for the", EXPSMOOTH$forecastingyear, "age-specific and total ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       "corresponding to the",
                       EXPSMOOTH$stockname, EXPSMOOTH$stockspecies, "stock.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("The retrospective evaluation of the performance of the point forecasts assessed how well the exponential smoothing model",
                       "performed when used for retrospective forecasting of the historical",
                       tolower(EXPSMOOTH$stockabundance), "values.",
                       "For this evaluation, the exponential smoothing model was fit to all of the historical",
                       tolower(EXPSMOOTH$stockabundance), "values",
                       "available prior to a given historical return year, then the fitted model was used",
                       "to forecast the",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       "for that year.",
                       "This evaluation captures how well the model would have performed in practice year over year",
                       "and was performed separately for each age-specific",
                        tolower(EXPSMOOTH$stockabundance),
                        "and for the total",
                        tolower(EXPSMOOTH$stockabundance),
                        ".",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("Retrospective forecast errors were defined as the actual",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       "values minus the retrospectively forecasted",
                       # "terminal run values.",
                        tolower(EXPSMOOTH$stockabundance),
                       "values.",
                       "In view of this definition, positive values for the retrospective forecast errors represent forecasts that were too low,",
                       "whereas negative values represent forecasts that were too high.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("The following retrospective measures were used to characterize different aspects of the retrospective forecasting errors:")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")


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

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("MAE and MAPE reflect the overall forecast accuracy accounting for systematic bias and year-to-year variation.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("MRE and MPE reflect directional bias in raw and relative forecast errors, respectively, with negative values indicating a tendency to",
                       "underforecast and positive values reflecting a tendency to overforecast.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("Just like MAE, RMSE  is a measure of the absolute magnitude of the raw retrospective forecast errors, but is more sensitive to large values",
                       "then MAE.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("MASE was proposed by Hyndman and Koehler (2006) as a generally applicable, scale-free measure of forecast accuracy.",
                       "This measure never gives infinite or undefined values.",
                       "In this report, MASE is computed as the average of the absolute values of the scaled",
                       "retrospective forecast errors produced by",
                       "the exponential smoothing model based.",
                       "The scaling of the errors involves dividing the errors by the MAE computed from the retrospective forecast errors associated with",
                       "the naive model based on the",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       "for the previous year.",
                       # "A scaled error is less than 1 if it arises from a better forecast than the one produced by the naive model based on the terminal run for the previous year.",
                       # "Conversely, it is greater than 1 if the forecast is worse than the average one-step, naive forecast computed in-sample.",
                       "A value of MASE less than 1 suggests that the retrospective forecasting accuracy of the exponential smoothing model",
                       "is better than the retrospective forecasting accuracy of",
                       "the benchmark naive model based on the",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       "for the previous year.",
                       "A value of MASE greater than 1 suggests that the retrospective forecasting accuracy of",
                       "the exponential smoothing model",
                       "is worse than the retrospective forecasting accuracy of",
                       "the benchmark naive model based on the",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       "for the previous year.",
                      sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("To facilitate the interpretation of the retrospective forecast errors, this section reports several types of plots:")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")


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

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("Bias coefficients representing a new metric for forecast bias are also reported in numerical and visual form in this section. ",
                    "These coefficients are computed from the retrospective forecast errors for the age-specific and total ",
                    paste0(tolower(EXPSMOOTH$stockabundance),"s"),
                    " using the formula developed by Kourentzes, Trapero and Svetunkov",
                    " in their 2014 working paper \"Measuring the behaviour of experts on demand forecasting: a complex task\".",
                    " In the context of this report, the bias coefficients describe the direction and magnitude of the retrospective forecast bias associated with the",
                    "exponential smoothing forecasting method.",
                    sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")

EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste(" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")
EXPSMOOTH$paragraph <- NULL 

EXPSMOOTH$paragraph <- paste("Generally speaking, the bias coefficients are unit-free and bounded between -1 (maximum negative retrospective forecast bias)",
                    "and 1 (maximum positive retrospective forecast bias).",
                    "A forecasting method that is always producing retrospective point forecasts which are over the observed historical values will have",
                    "a bias coefficient equal to -1, always over-forecasting.",
                    "A forecasting method that is always producing retrospective point forecasts which are under the observed historical values",
                    "will have a bias coefficient equal to 1, always under-forecasting.",
                    "Given the bounded nature of the bias coefficient, we can describe a forecasting method as strongly biased if |bias coefficient| > 0.5",
                    "and weakly biased if 0 < |bias coefficient| <= 0.5, providing a simple and intuitive description of the forecast bias behaviour.",
                    "If the bias coefficient is equal to 0, the forecasting method is unbiased.",
                    sep=" ")
doc = addParagraph(doc, EXPSMOOTH$paragraph, stylename="Normal")


## doc = addParagraph( doc, value = 'Histograms of the retrospective forecast errors;',
##      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")
## doc = addParagraph( doc, value = 'Barplots of the retrospective forecast errors together with the forecast interval corresponding to the forecasting year of interest.',
##      par.properties = parProperties(list.style = 'ordered', level = 1), stylename="BulletList")


## paragraph <- paste("We also calculated the coefficient of determination obtained by regressing the retrospectively forecasted",
##                       tolower(EXPSMOOTH$stockabundance),
##                       "values",
##                       "on the historical",
##                       # terminal run
##                       tolower(EXPSMOOTH$stockabundance),
##                       "values.",
##                       "This is simply the squared correlation coefficient of the retrospectively forecasted and historical",
##                       # terminal run
##                        tolower(EXPSMOOTH$stockabundance),
##                       "values.",
##                      sep=" ")
## doc = addParagraph(doc, paragraph, stylename="Normal")



#---- Retrospective Measures of Forecast Performance -------------------------------------

doc = addTitle(doc, value="Retrospective Measures of Forecast Performance", level=2)


EXPSMOOTH$tabledata <- EXPSMOOTH$M.expsmooth

EXPSMOOTH$tabledata <- subset(EXPSMOOTH$M.expsmooth, select=-Model)

EXPSMOOTH$tabledata <- cbind(Model=rep("Exponential Smoothing",nrow(EXPSMOOTH$M.expsmooth)),EXPSMOOTH$tabledata)

EXPSMOOTH$tablecaption <- paste("Retrospective measures of forecast performance ",
                "associated with the point forecasts of the ",
                max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                " age-specific and total ",
                # terminal runs
                paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                " for the ",
                EXPSMOOTH$stockname,
                " ",
                tolower(EXPSMOOTH$stockspecies),
                " stock.",
                " The point forecasts of age-specific ",
                # terminal runs
                paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                " were obtained via exponential smoothing. ",
                " The point forecast for the total ",
                tolower(EXPSMOOTH$stockabundance),
                " was obtained by adding up the point forecasts for the age-specific components of ",
                # terminal run",
                 tolower(EXPSMOOTH$stockabundance),
                ".",
                sep=""
                )

names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="Model"] <- "Model Class"


EXPSMOOTH$tt <- EXPSMOOTH$tabledata


usePackage("stringr")
names(EXPSMOOTH$tt) <- str_replace_all(names(EXPSMOOTH$tt),"_"," ")

## EXPSMOOTH$tt[,-1] <- comma(EXPSMOOTH$tt[,-1])


doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

EXPSMOOTH$my_ft <- FlexTable( data = EXPSMOOTH$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
                   )

# overwrites some paragraph formatting properties
EXPSMOOTH$my_ft[, 1:ncol(EXPSMOOTH$tt)] = parProperties(text.align = "right")

EXPSMOOTH$my_ft = addFooterRow( EXPSMOOTH$my_ft, value = paste("Legend:  MRE = Mean Relative Error; ",
                                           "MAE = Mean Absolute Error; ",
                                           "MPE = Mean Percentage Error; ",
                                           "MAPE = Mean Absolute Percentage Error; ",
                                           "MASE = Mean Scaled Error; ",
                                           "MSE = Root Mean Square Error."),
        colspan = ncol(EXPSMOOTH$tt),
        text.properties = textProperties(font.size = 9),
        par.properties = parProperties(text.align = "left"))

## EXPSMOOTH$my_ft = addFooterRow( EXPSMOOTH$my_ft, value = c("         MAE = Mean Absolute Error;"), colspan = ncol(EXPSMOOTH$tt))
## EXPSMOOTH$my_ft = addFooterRow( EXPSMOOTH$my_ft, value = c("         MPE = Mean Percentage Error;"), colspan = ncol(EXPSMOOTH$tt))
## EXPSMOOTH$my_ft = addFooterRow( EXPSMOOTH$my_ft, value = c("         MAPE = Mean Absolute Percentage Error;"), colspan = ncol(EXPSMOOTH$tt))
## EXPSMOOTH$my_ft = addFooterRow( EXPSMOOTH$my_ft, value = c("         MASE = Mean Scaled Error;"), colspan = ncol(EXPSMOOTH$tt))
## EXPSMOOTH$my_ft = addFooterRow( EXPSMOOTH$my_ft, value = c("         RMSE = Root Mean Square Error."), colspan = ncol(EXPSMOOTH$tt))


doc = addFlexTable(doc, flextable=EXPSMOOTH$my_ft)



#---- Retrospective Forecast Errors:  Specific Ages ------------------------------------------------------


doc = addPageBreak(doc)

doc = addTitle(doc, "Retrospective Point Forecasts and Forecast Errors", level=2)


for (i in 1:length(EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth)) {

    EXPSMOOTH$results <- EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth

    EXPSMOOTH$tabledata <- EXPSMOOTH$results[[i]]$data.retro

    EXPSMOOTH$age <- names(EXPSMOOTH$results)[i]
    usePackage("stringr")
    EXPSMOOTH$age <- str_extract( EXPSMOOTH$age,"[[:digit:]]+")

    EXPSMOOTH$tablecaption <- paste("Retrospective point forecasts",
                          " and associated forecast errors for the age ", EXPSMOOTH$age,
                          " component of the ",
 				              max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                		  # " terminal run for the ",
                		  " ",
                      tolower(EXPSMOOTH$stockabundance),
                      " for the ",
                      EXPSMOOTH$stockname,
                		  " ",
                		  tolower(EXPSMOOTH$stockspecies),
                		  " stock. ",
                     "Accompanying return years and actual ",
                     # terminal run
                     tolower(EXPSMOOTH$stockabundance),
                     " values are also reported.",
                     " The retrospective point forecasts were obtained via exponential smoothing.",
                     sep="")

    EXPSMOOTH$tabledata$p <- round(EXPSMOOTH$tabledata$p,2)
    EXPSMOOTH$tabledata$e <- round(EXPSMOOTH$tabledata$e,2)

    usePackage("scales")
    EXPSMOOTH$tabledata$a <- comma(round(EXPSMOOTH$tabledata$a))
    EXPSMOOTH$tabledata$p <- comma(round(EXPSMOOTH$tabledata$p))
    EXPSMOOTH$tabledata$e <- comma(round(EXPSMOOTH$tabledata$e))

    names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="cy"] <- "Return Year"
    names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="a"] <-  "Actual"
    names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="p"] <-  "Forecast"
    names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="e"] <-  "Error"


    EXPSMOOTH$tabledata

    EXPSMOOTH$tabledata <- subset(EXPSMOOTH$tabledata, select=c(-p.bench, -e.bench))

     doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

    baseCellProp = cellProperties( padding = 4)

    EXPSMOOTH$tt <- EXPSMOOTH$tabledata


    usePackage("stringr")
    names(EXPSMOOTH$tt) <- str_replace_all(names(EXPSMOOTH$tt),"_"," ")

    ## EXPSMOOTH$tt[,-1] <- comma(EXPSMOOTH$tt[,-1])

    EXPSMOOTH$my_ft <- FlexTable( data = EXPSMOOTH$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
    )

    # overwrites some paragraph formatting properties
    EXPSMOOTH$my_ft[, 1:ncol(EXPSMOOTH$tt)] = parProperties(text.align = "right")

    doc = addFlexTable(doc, flextable=EXPSMOOTH$my_ft)

    doc = addPageBreak(doc)


}

# cat("<br clear=all style=\'mso-special-character:line-break;page-break-before:always\'>",
#    file=filename, append=TRUE, sep="\n\n")

#---- Retrospective Forecast Errors:  Total Age ------------------------------------------------------------

EXPSMOOTH$results <- EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth

EXPSMOOTH$tabledata <- data.frame(cy = EXPSMOOTH$results$data.retro[[1]]$cy,
                        a = EXPSMOOTH$results$a.total.retro,
                        p = EXPSMOOTH$results$p.total.retro,
                        e = EXPSMOOTH$results$e.total.retro)

EXPSMOOTH$tablecaption <- paste("Retrospective point forecasts",
                      " and associated forecast errors for the ",
 				              max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                		  " total ",
                      # terminal run
                      tolower(EXPSMOOTH$stockabundance),
                      " for the ",
                		  EXPSMOOTH$stockname,
                		  " ",
                		  tolower(EXPSMOOTH$stockspecies),
                		  " stock. ",
                      "Accompanying return years and actual total ",
                      # terminal run
                      tolower(EXPSMOOTH$stockabundance),
                      " values are also reported.",
                      " The retrospective point forecasts for the total ",
                      # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                      " were obtained by adding up the retrospective point forecasts for the age-specific components of ",
                      # terminal run
                      tolower(EXPSMOOTH$stockabundance),
                      " produced by the exponential smoothing models.",
                          sep="")

usePackage("scales")
EXPSMOOTH$tabledata$a <- comma(round(EXPSMOOTH$tabledata$a))
EXPSMOOTH$tabledata$p <- comma(round(EXPSMOOTH$tabledata$p))
EXPSMOOTH$tabledata$e <- comma(round(EXPSMOOTH$tabledata$e))

names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="cy"] <- "Return Year"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="a"] <-  "Actual"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="p"] <-  "Forecast"
names(EXPSMOOTH$tabledata)[names(EXPSMOOTH$tabledata)=="e"] <-  "Error"

EXPSMOOTH$tabledata


doc = addParagraph(doc, value=EXPSMOOTH$tablecaption, stylename="rTableLegend")

baseCellProp = cellProperties( padding = 4)

EXPSMOOTH$tt <- EXPSMOOTH$tabledata


usePackage("stringr")
names(EXPSMOOTH$tt) <- str_replace_all(names(EXPSMOOTH$tt),"_"," ")


## EXPSMOOTH$tt[,-1] <- comma(EXPSMOOTH$tt[,-1])

EXPSMOOTH$my_ft <- FlexTable( data = EXPSMOOTH$tt,
                    body.cell.props = baseCellProp,
                    header.par.props = parProperties(text.align = "right" ),
                    header.cell.props=cellProperties(background.color="#DDDDDD", padding=4)
)

# overwrites some paragraph formatting properties
EXPSMOOTH$my_ft[, 1:ncol(EXPSMOOTH$tt)] = parProperties(text.align = "right")

doc = addFlexTable(doc, flextable=EXPSMOOTH$my_ft)




#----- Retrospective Point Forecasts vs. Actual Over Time: Individual Ages ---------------------------------------------------------------------

for (j in 1:length(EXPSMOOTH$expsmooth.individual.ages.retro.plot.info)) {

     doc = addPageBreak(doc)

     doc = addPlot(doc,
        fun = print,
        x = EXPSMOOTH$expsmooth.individual.ages.retro.plot(EXPSMOOTH$expsmooth.individual.ages.retro.plot.info, EXPSMOOTH$stockabundance, j),
        width=6.5, height=7)

     EXPSMOOTH$figurecaption <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                       tolower(EXPSMOOTH$expsmooth.model.fits[[j]]$age),
                       " component of ",
                       # max(results.afe.total.age.retro.arima$CY)+1,
                       # " terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock,"),
                       " derived on the basis of exponential smoothing modeling. ",
                       "Historical values of age-specific ", EXPSMOOTH$stockabundance,
                       " (grey lines) and fitted values produced by the exponential smoothing modeling (red lines) are also shown. ",
                       "Each panel corresponds to a particular retrospective forecasting year.",
                       sep="")

     doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

}

#----- Retrospective Point Forecasts vs. Actual Over Time: Total Age ---------------------------------------------------------------------

doc = addPageBreak(doc)

doc = addPlot(doc,
        fun = print,
        x = EXPSMOOTH$expsmooth.total.age.retro.plot(EXPSMOOTH$expsmooth.total.age.retro.plot.info, EXPSMOOTH$stockabundance),
        width=6.5, height=7)

EXPSMOOTH$figurecaption <- paste("Actual values (grey dots) and retrospectively forecasted values (red dots) of the ",
                      "total ",
                       # max(results.afe.total.age.retro.arima$CY)+1,
                       # " terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock,"),
                       " derived on the basis of exponential smoothing modeling. ",
                       "Historical values of total ", EXPSMOOTH$stockabundance,
                       " (grey lines) and fitted values produced by the exponential smoothing modeling (red lines) are also shown. ",
                       "Each panel corresponds to a particular retrospective forecasting year.",
                       sep="")

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")


#---- Retrospective Forecast Errors: Age-Specific Density Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

EXPSMOOTH$figurecaption <- paste("Density plots of the retrospective forecast errors derived from the exponential smoothing models used to forecast ",
                       "specific age components of the ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " ",
                       # " terminal run for the ",
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       sep="")



## myplot <- hist.results.afe.individual.ages.retro.expsmooth(
##     expsmooth.model.fits,
##    results.individual.ages.retro.predictive.performance.expsmooth
##   )


EXPSMOOTH$myplot <- EXPSMOOTH$dens.results.afe.individual.ages.retro.expsmooth(
              EXPSMOOTH$expsmooth.model.fits,
              EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth,
              EXPSMOOTH$boxcoxtransform)




doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 


#---- Retrospective Forecast Errors: Individual Ages Bias Coefficients Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

EXPSMOOTH$figurecaption <- paste("Bias coefficient plots constructed from the retrospective forecast errors produced by the exponential smoothing models used to forecast ",
                       "specific age components of the ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " ",
                       # " terminal run for the ",
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       sep="")


EXPSMOOTH$myplot <- EXPSMOOTH$bias.coefficients.afe.individual.ages.retro.expsmooth(
              EXPSMOOTH$expsmooth.model.fits,
              EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth,
              EXPSMOOTH$boxcoxtransform,
              EXPSMOOTH$stockabundance)

usePackage("gridGraphics")

doc = addPlot(doc,
            fun=grid.draw,
            x=EXPSMOOTH$myplot,
            width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 


#---- Retrospective Forecast Errors: Total Age Density Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

EXPSMOOTH$figurecaption <- paste("Density plots of the retrospective forecast errors ",
                       "involved in forecasting the ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " total ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       sep="")




## results <- EXPSMOOTH$results.afe.total.age.retro.expsmooth
## myplot <- hist.results.afe.total.age.retro.expsmooth(results)


EXPSMOOTH$myplot <- EXPSMOOTH$dens.results.afe.total.age.retro.expsmooth(EXPSMOOTH$results.afe.total.age.retro.expsmooth)

doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth+1, height=plotheight-3)


doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 



#---- Retrospective Forecast Errors: Total Age Bias Coefficients Plots ---------------------------------------------------------------

doc = addPageBreak(doc)

EXPSMOOTH$figurecaption <- paste("Bias coefficient plot constructed from the retrospective forecast errors produced by the exponential smoothing models used to forecast ",
                       "the ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " total ",
                       # " terminal run for the ",
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock."),
                       sep="")


EXPSMOOTH$myplot <- EXPSMOOTH$bias.coefficient.afe.total.age.retro.expsmooth(
              ##EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth,
              EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth,
              EXPSMOOTH$stockabundance)

usePackage("gridGraphics")

doc = addPlot(doc,
            fun=grid.draw,
            x=EXPSMOOTH$myplot,
            width=plotwidth, height=plotheight)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 


#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Specific Ages ---------------------------------------------------------------


for (i in 1:length(EXPSMOOTH$expsmooth.model.fits)) {

    doc = addPageBreak(doc)

    EXPSMOOTH$figurecaption <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       tolower( EXPSMOOTH$expsmooth.model.fits[[i]]$age),
                       " component of the ",
                       # max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       # " terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock,"),
                       " derived via exponential smoothing modeling.",
                       sep="")


      EXPSMOOTH$results.retro <- EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth
      EXPSMOOTH$results.pred <-  EXPSMOOTH$pred.int.individual.ages.expsmooth

      doc = addPlot(doc,
        fun = print,
        x =   EXPSMOOTH$gary.plot.individual.ages(EXPSMOOTH$results.retro,  EXPSMOOTH$results.pred, i),
        width=6.5, height=6)

      doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")


}


#---- Retrospective Forecast Errors and Forecast Interval: Gary's Plot for Total Age ---------------------------------------------------------------


doc = addPageBreak(doc)
EXPSMOOTH$results.retro <- EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth
names(EXPSMOOTH$results.retro)
EXPSMOOTH$results.pred <-  EXPSMOOTH$PI.total.age.expsmooth.no.comma


doc = addPlot(doc,
        fun = print,
        x =  EXPSMOOTH$gary.plot.total.age( EXPSMOOTH$results.retro,  EXPSMOOTH$results.pred),
        width=6.5, height=6)



EXPSMOOTH$figurecaption <- paste("Retrospective forecast errors and 80% interval forecast associated with the ",
                       # "the ",
                       # max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY)+1,
                       " total ",
                       # terminal run ",
                       tolower(EXPSMOOTH$stockabundance),
                       " for the ",
                       paste(EXPSMOOTH$stockname,tolower(EXPSMOOTH$stockspecies), "stock,"),
                       " derived via exponential smoothing modeling.",
                       sep="")


doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")




#---- Forecast Diagnostics - Individual Ages - Plot No. 2 ------------------------------------------------------------------------

doc = addTitle( doc, "Forecast Diagnostics", level = 1)

EXPSMOOTH$figurecaption <- paste("Time series plots of retrospectively forecasted ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " and actual", 
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " for specific age ",
                       "components of the ",
                       EXPSMOOTH$stockname, " ",
                       tolower(EXPSMOOTH$stockspecies),
                       " stock. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for a given return year was derived by applying ARIMA modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       sep=""
                       )


EXPSMOOTH$myplot <-  EXPSMOOTH$timeseries.plot.results.afe.individual.ages.retro.expsmooth(
       ## arima.model.fits,
        EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth,
       EXPSMOOTH$stockabundance
       )


doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth+1, height=plotheight)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 


#---- Forecast Diagnostics - Plot No. 1 ------------------------------------------------------------------------


EXPSMOOTH$figurecaption <- paste("Scatterplots of retrospectively forecasted ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " versus actual ",  
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),  
                       " for specific age ",
                       "components of the ",
                       EXPSMOOTH$stockname, " ",
                       tolower(EXPSMOOTH$stockspecies),
                       " stock. ",
                       "Observations in each panel are labeled according to the last two digits of the associated historical return years. ",
                       "For each age component, the retrospectively forecasted ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for a given return year was derived by applying exponential smoothing modeling to ",
                       "the time series of age-specific ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " up to (but not including) that return year.",
                       sep=""
                       )



EXPSMOOTH$myplot <- EXPSMOOTH$scatter.plot.results.afe.individual.ages.retro.expsmooth(results=EXPSMOOTH$results.individual.ages.retro.predictive.performance.expsmooth, EXPSMOOTH$stockabundance)


doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth+1, height=plotheight)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 

#---- Forecast Diagnostics - Plot No. 2 - Total Age ------------------------------------------------------------------------

doc = addPageBreak(doc)

EXPSMOOTH$figurecaption <- paste("Time series plot of retrospectively forecasted and ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " actual total ",
                       # terminal runs ",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY),
                       " - ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " Exponential smoothing modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")

EXPSMOOTH$myplot <- EXPSMOOTH$timeseries.plot.results.afe.total.age.retro.expsmooth(EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth, EXPSMOOTH$stockabundance)

doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth+1, height=plotheight-2)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 


#---- Forecast Diagnostics - Plot No. 1 - Total Age ------------------------------------------------------------------------


EXPSMOOTH$figurecaption <- paste("Scatterplot of retrospectively forecasted total ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " versus actual total ",
                       # terminal runs ",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " corresponding to the return years ",
                       min(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY),
                       " - ",
                       max(EXPSMOOTH$results.afe.total.age.retro.expsmooth$CY),
                       ". ", ## "Forecasted total terminal runs were derived via naive time series modeling.",
                       "Observations are labeled according to the last two digits of the associated historical return years.",
                       " The retrospectively forecasted total ",
                       # terminal run
                       tolower(EXPSMOOTH$stockabundance),
                       " for a given return year was derived by adding up the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       " up to (but not including) that year.",
                       " Exponential smoothing modeling ",
                       " was used to obtain the",
                       " retrospectively forecasted age-specific ",
                       # terminal runs.",
                       paste0(tolower(EXPSMOOTH$stockabundance),"s",collapse=" "),
                       ".",
                       sep="")


EXPSMOOTH$results <- EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth

EXPSMOOTH$myplot <- EXPSMOOTH$scatter.plot.results.afe.total.age.retro.expsmooth(EXPSMOOTH$results.total.age.retro.predictive.performance.expsmooth, EXPSMOOTH$stockabundance)

doc = addPlot(doc,
            fun=print,
            x=EXPSMOOTH$myplot,
            width=plotwidth+1, height=plotheight-2)

doc = addParagraph(doc, value=EXPSMOOTH$figurecaption, stylename="rPlotLegend")

EXPSMOOTH$myplot <- NULL 

EXPSMOOTH$fits <- NULL

## doc = addPageBreak(doc)
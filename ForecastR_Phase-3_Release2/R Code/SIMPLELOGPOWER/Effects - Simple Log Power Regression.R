usePackage("effects")

SIMPLELOGPOWER$Analyze.model.old <- function (focal.predictors, mod, xlevels, default.levels = NULL,
    formula.rhs, partial.residuals = FALSE, quantiles, x.var = NULL,
    data = NULL)
{
    if ((!is.null(mod$na.action)) && class(mod$na.action) ==
        "exclude")
        class(mod$na.action) <- "omit"
    all.predictors <- all.vars(formula.rhs)
    check.vars <- !(focal.predictors %in% all.predictors)
    excluded.predictors <- setdiff(all.predictors, focal.predictors)
    number.bad <- sum(check.vars)
    if (any(check.vars)) {
        message <- if (number.bad == 1)
            paste("the following predictor is not in the model:",
                focal.predictors[check.vars])
        else paste("the following predictors are not in the model:",
            paste(focal.predictors[check.vars], collapse = ", "))
        stop(message)
    }

    is.factor.predictor <- function (predictor, model) {
       !is.null(model$xlevels[[predictor]])
    }

    is.numeric.predictor <- function (predictor, model) {
       is.null(model$xlevels[[predictor]])
    }

    subscripts <- function (index, dims) {
    	subs <- function(dims, index) {
        dim <- length(dims)
        if (dim == 0)
            return(NULL)
        cum <- c(1, cumprod(dims))[dim]
        i <- index%/%cum
        if (index%%cum != 0)
            i <- i + 1
        c(i, subs(dims[-dim], index - (i - 1) * cum))
    	}
    	rev(subs(dims, index))
    }

    matrix.to.df <- function (matrix, colclasses) {
    opt <- options(warn = -1)
    on.exit(options(opt))
    ncol <- ncol(matrix)
    colnames <- colnames(matrix)
    colclasses[sapply(colclasses, function(x) "integer" %in%
        x)] <- "numeric"
    result <- vector(mode = "list", length = ncol)
    names(result) <- colnames
    for (j in 1:ncol) {
        result[[j]] <- matrix[, j]
        class <- colclasses[[colnames[j]]]
        result[[colnames[j]]] <- if ("numeric" %in% class) {
            decChar <- getOption("OutDec")
            if (decChar == ".")
                as.numeric(result[[colnames[j]]])
            else as.numeric(gsub(decChar, ".", matrix[, j]))
        }
        else if ("ordered" %in% class)
            ordered(result[[colnames[j]]])
        else if ("factor" %in% class)
            factor(result[[colnames[j]]])
        else result[[colnames[j]]]
    }
    as.data.frame(result)
    }





    X.mod <- model.matrix(mod)
    cnames <- colnames(X.mod)
    factor.cols <- rep(FALSE, length(cnames))
    names(factor.cols) <- cnames
    for (name in all.predictors) {
        if (is.factor.predictor(name, mod))
            factor.cols[grep(paste("^", name, sep = ""), cnames)] <- TRUE
    }
    factor.cols[grep(":", cnames)] <- FALSE
    X <- na.omit(expand.model.frame(mod, all.predictors))
    bad <- sapply(X[, all.predictors, drop = FALSE], function(x) !(is.factor(x) ||
        is.numeric(x)))
    if (any(bad)) {
        message <- if (sum(bad) == 1)
            paste("the following predictor isn't a factor or numeric:",
                all.predictors[bad])
        else paste("the following predictors aren't factors or numeric:",
            paste(all.predictors[bad], collapse = ", "))
        stop(message)
    }
    x <- list()
    factor.levels <- list()
    if (length(xlevels) == 0 & length(default.levels) == 1L)
        xlevels <- default.levels
    if (is.numeric(xlevels) & length(xlevels) == 1L) {
        levs <- xlevels
        for (name in focal.predictors) xlevels[[name]] <- levs
    }
    for (name in focal.predictors) {
        levels <- mod$xlevels[[name]]
        if (is.null(levels))
            levels <- mod$xlevels[[paste("factor(", name, ")",
                sep = "")]]
        fac <- !is.null(levels)
        if (!fac) {
            levels <- if (is.null(xlevels[[name]])) {
                if (partial.residuals) {
                  quantile(X[, name], quantiles)
                }
                else {
                  grid.pretty(range(X[, name]))
                }
            }
            else {
                if (length(xlevels[[name]]) == 1L) {
                  seq(min(X[, name]), max(X[, name]), length = xlevels[[name]])
                }
                else xlevels[[name]]
            }
        }
        else factor.levels[[name]] <- levels
        x[[name]] <- list(name = name, is.factor = fac, levels = levels)
    }
    if (partial.residuals) {
        numeric.predictors <- sapply(focal.predictors, function(predictor) is.numeric.predictor(predictor,
            mod))
        if (!any(numeric.predictors))
            warning("there are no numeric focal predictors",
                "\n  partial residuals suppressed")
        else {
            x.var <- which(numeric.predictors)[1]
            x.var.name <- focal.predictors[x.var]
            if (is.null(mod$xlevels[[x.var.name]])) {
                x.var.levels <- x[[x.var]][["levels"]]
                x.var.range <- range(X[, focal.predictors[x.var]])
                x[[x.var]][["levels"]] <- seq(from = x.var.range[1],
                  to = x.var.range[2], length = 100)
            }
        }
    }
    x.excluded <- list()
    for (name in excluded.predictors) {
        levels <- mod$xlevels[[name]]
        fac <- !is.null(levels)
        level <- if (fac)
            levels[1]
        else mean(X[, name])
        if (fac)
            factor.levels[[name]] <- levels
        x.excluded[[name]] <- list(name = name, is.factor = fac,
            level = level)
    }
    dims <- sapply(x, function(x) length(x$levels))
    len <- prod(dims)
    n.focal <- length(focal.predictors)
    n.excluded <- length(excluded.predictors)
    n.vars <- n.focal + n.excluded
    predict.data <- matrix("", len, n.vars)
    excluded <- sapply(x.excluded, function(x) x$level)
    for (i in 1:len) {
        subs <- subscripts(i, dims)
        for (j in 1:n.focal) {
            predict.data[i, j] <- x[[j]]$levels[subs[j]]
        }
        if (n.excluded > 0)
            predict.data[i, (n.focal + 1):n.vars] <- excluded
    }
    colnames(predict.data) <- c(sapply(x, function(x) x$name),
        sapply(x.excluded, function(x) x$name))
    colclasses <- lapply(X, class)
    colclasses[colclasses == "matrix"] <- "numeric"
    predict.data <- matrix.to.df(predict.data, colclasses = colclasses)
    list(predict.data = predict.data, factor.levels = factor.levels,
        factor.cols = factor.cols, focal.predictors = focal.predictors,
        n.focal = n.focal, excluded.predictors = excluded.predictors,
        n.excluded = n.excluded, x = x, X.mod = X.mod, cnames = cnames,
        X = X, x.var = x.var)
}




SIMPLELOGPOWER$Fixup.model.matrix.old <- function(mod, mod.matrix, mod.matrix.all, X.mod, factor.cols,
    cnames, focal.predictors, excluded.predictors, typical, given.values,
    partial.residuals = FALSE, mod.matrix.all.rounded)
{

   has.intercept <- function (model, ...) {
      any(names(coefficients(model)) == "(Intercept)")
   }

   matchVarName <- function(name, expressions) {
    a <- !grepl(paste("[.]+", name, sep = ""), expressions)
    b <- !grepl(paste(name, "[.]+", sep = ""), expressions)
    c <- grepl(paste("\\b", name, "\\b", sep = ""), expressions)
    a & b & c
   }

   Strangers <- function (mod, focal.predictors, excluded.predictors) {
    names <- term.names(mod)
    if (has.intercept(mod))
        names <- names[-1]
    sel <- apply(sapply(excluded.predictors, matchVarName, expressions = names),
        1, any)
    (1:length(sel))[sel]
   }

   term.names <- function (model, ...) {
    term.names <- gsub(" ", "", labels(terms(model)))
    if (has.intercept(model))
        c("(Intercept)", term.names)
    else term.names
   }

    vars <- as.character(attr(terms(mod), "variables"))[-(1:2)]
    attr(mod.matrix, "assign") <- attr(mod.matrix.all, "assign")
    if (length(excluded.predictors) > 0) {
        sel <- apply(sapply(excluded.predictors, matchVarName,
            expressions = vars), 1, any)
        strangers <- Strangers(mod, focal.predictors, excluded.predictors)
        stranger.cols <- apply(outer(strangers, attr(mod.matrix,
            "assign"), "=="), 2, any)
    }
    else stranger.cols <- rep(FALSE, ncol(mod.matrix))
    if (has.intercept(mod))
        stranger.cols[1] <- TRUE
    if (any(stranger.cols)) {
        facs <- factor.cols & stranger.cols
        covs <- (!factor.cols) & stranger.cols
        if (any(facs)) {
            mod.matrix[, facs] <- matrix(apply(as.matrix(X.mod[,
                facs]), 2, mean), nrow = nrow(mod.matrix), ncol = sum(facs),
                byrow = TRUE)
            if (partial.residuals) {
                mod.matrix.all.rounded[, facs] <- mod.matrix.all[,
                  facs] <- matrix(apply(as.matrix(X.mod[, facs]),
                  2, mean), nrow = nrow(mod.matrix.all), ncol = sum(facs),
                  byrow = TRUE)
            }
        }
        if (any(covs)) {
            mod.matrix[, covs] <- matrix(apply(as.matrix(X.mod[,
                covs]), 2, typical), nrow = nrow(mod.matrix),
                ncol = sum(covs), byrow = TRUE)
            if (partial.residuals) {
                mod.matrix.all.rounded[, covs] <- mod.matrix.all[,
                  covs] <- matrix(apply(as.matrix(X.mod[, covs]),
                  2, typical), nrow = nrow(mod.matrix.all), ncol = sum(covs),
                  byrow = TRUE)
            }
        }
        if (!is.null(given.values)) {
            stranger.names <- cnames[stranger.cols]
            given <- stranger.names %in% names(given.values)
            if (any(given)) {
                mod.matrix[, stranger.names[given]] <- matrix(given.values[stranger.names[given]],
                  nrow = nrow(mod.matrix), ncol = length(stranger.names[given]),
                  byrow = TRUE)
                if (partial.residuals) {
                  mod.matrix.all.rounded[, stranger.names[given]] <- mod.matrix.all[,
                    stranger.names[given]] <- matrix(given.values[stranger.names[given]],
                    nrow = nrow(mod.matrix.all), ncol = length(stranger.names[given]),
                    byrow = TRUE)
                }
            }
        }
        for (name in cnames) {
            components <- unlist(strsplit(name, ":"))
            components <- components[components %in% cnames]
            if (length(components) > 1) {
                mod.matrix[, name] <- apply(mod.matrix[, components],
                  1, prod)
                if (partial.residuals) {
                  mod.matrix.all[, name] <- apply(mod.matrix.all[,
                    components], 1, prod)
                  mod.matrix.all.rounded[, name] <- apply(mod.matrix.all.rounded[,
                    components], 1, prod)
                }
            }
        }
    }
    if (partial.residuals)
        list(mod.matrix = mod.matrix, mod.matrix.all = mod.matrix.all,
            mod.matrix.all.rounded = mod.matrix.all.rounded)
    else mod.matrix
}



SIMPLELOGPOWER$my.Effect.lm <- function(focal.predictors, mod, xlevels = list(), default.levels = NULL,
                                 given.values, se = TRUE, confidence.level = 0.95,
                                 transformation = list(link = family(mod)$linkfun, inverse = family(mod)$linkinv),
                                 typical = mean, offset = mean, partial.residuals = FALSE,
                                 quantiles = seq(0.2, 0.8, by = 0.2), x.var = NULL, ...){


    if (partial.residuals) {
        all.vars <- all.vars(formula(mod))
        data <- expand.model.frame(mod, all.vars)[, all.vars]
    } else { NULL }

    if (missing(given.values)){
        given.values <- NULL
    } else if (!all(which <- names(given.values) %in% names(coef(mod)))) {
        stop("given.values (", names(given.values[!which]), ") not in the model")
    }

    off <- if (is.numeric(offset) && length(offset) == 1) {
        offset
    } else if (is.function(offset)) {
        mod.off <- model.offset(model.frame(mod))
        if (is.null(mod.off)) {
            0
        } else { offset(mod.off) }
    } else stop("offset must be a function or a number")


    formula.rhs <- formula(mod)[[3]]

    ## predict.data

    model.components <- SIMPLELOGPOWER$Analyze.model.old(focal.predictors, mod,
        xlevels, default.levels, formula.rhs, partial.residuals = partial.residuals,
        quantiles = quantiles, x.var = x.var, data = data)

    excluded.predictors <- model.components$excluded.predictors

    predict.data <- model.components$predict.data

    predict.data.all.rounded <- predict.data.all <- if (partial.residuals) {
       na.omit(data[, all.vars(formula(mod))])
    } else { NULL }


    factor.levels <- model.components$factor.levels
    factor.cols <- model.components$factor.cols
    n.focal <- model.components$n.focal
    x <- model.components$x
    X.mod <- model.components$X.mod
    cnames <- model.components$cnames
    X <- model.components$X
    x.var <- model.components$x.var
    formula.rhs <- formula(mod)[c(1, 3)]
    Terms <- delete.response(terms(mod))
    mf <- model.frame(Terms, predict.data, xlev = factor.levels,
        na.action = NULL)
    mod.matrix <- model.matrix(formula.rhs, data = mf, contrasts.arg = mod$contrasts)
    factors <- sapply(predict.data, is.factor)
    if (partial.residuals) {
        for (predictor in focal.predictors[-x.var]) {
            if (!factors[predictor]) {
                values <- unique(predict.data[, predictor])
                predict.data.all.rounded[, predictor] <- values[apply(outer(predict.data.all[,
                  predictor], values, function(x, y) (x - y)^2),
                  1, which.min)]
            }
        }
        mf.all <- model.frame(Terms, predict.data.all, xlev = factor.levels,
            na.action = NULL)
        mod.matrix.all <- model.matrix(formula.rhs, data = mf.all,
            contrasts.arg = mod$contrasts)
        mf.all.rounded <- model.frame(Terms, predict.data.all.rounded,
            xlev = factor.levels, na.action = NULL)
        mod.matrix.all.rounded <- model.matrix(formula.rhs, data = mf.all.rounded,
            contrasts.arg = mod$contrasts)
    } else { mod.matrix.all <- model.matrix(mod) }
    wts <- weights(mod)
    if (is.null(wts))
        wts <- rep(1, length(residuals(mod)))

    has.intercept <- function (model, ...) {
        any(names(coefficients(model)) == "(Intercept)")
    }

    res <- SIMPLELOGPOWER$Fixup.model.matrix.old(mod, mod.matrix, mod.matrix.all,
        X.mod, factor.cols, cnames, focal.predictors, excluded.predictors,
        typical=mean, given.values, partial.residuals = partial.residuals,
        mod.matrix.all.rounded)

    mod.matrix <- if (partial.residuals){
     res$mod.matrix
    } else { res }
    mod.matrix.cases <- if (partial.residuals) {
        res$mod.matrix.all
    } else { NULL }
    mod.matrix.cases.rounded <- if (partial.residuals) {
        res$mod.matrix.all.rounded
    } else { NULL }
    mod.matrix <- mod.matrix[, !is.na(mod$coefficients), drop = FALSE]
    effect <- off + mod.matrix %*% mod$coefficients[!is.na(mod$coefficients)]


    if (partial.residuals) {
        mod.matrix.cases <- na.omit(mod.matrix.cases[, !is.na(mod$coefficients), drop = FALSE])
        fitted <- as.vector(off + mod.matrix.cases %*% mod$coefficients[!is.na(mod$coefficients)])
        ## mod.matrix.cases.rounded doesn't have the correct dimensions! it currently has a NULL dimension,
        ## when it fact it should have an n x 1 dimension (where n is the number of regression observations)
        dim(mod.matrix.cases.rounded)  <- dim(mod.matrix.cases) # force dimension of mod.matrix.cases.rounded
                                                                # to be the same as that of mod.matrix.cases, i.e., n x 1
        mod.matrix.cases.rounded <- na.omit(mod.matrix.cases.rounded[, !is.na(mod$coefficients), drop=FALSE])
        # quote(mod.matrix <- mod.matrix[, !is.na(mod$coefficients), drop=FALSE])
        fitted.rounded <- as.vector(off + mod.matrix.cases.rounded %*%
            mod$coefficients[!is.na(mod$coefficients)])
        res <- na.omit(residuals(mod, type = "working"))
        partial.residuals.raw <- fitted + res
        partial.residuals.adjusted <- fitted.rounded + res

    } else {
        partial.residuals.raw <- partial.residuals.adjusted <- fitted.rounded <- fitted <- NULL
    }

    result <- list(term = paste(focal.predictors, collapse = "*"),
        formula = formula(mod), response = effects:::response.name(mod),
        variables = x, fit = effect, x = predict.data[, 1:n.focal,
            drop = FALSE], x.all = predict.data.all.rounded,
        model.matrix = mod.matrix, mod.matrix.all = mod.matrix.cases,
        data = X, discrepancy = 0, offset = off, fitted.rounded = fitted.rounded,
        fitted = fitted, partial.residuals.raw = partial.residuals.raw,
        partial.residuals.adjusted = partial.residuals.adjusted,
        x.var = x.var)
    whichFact <- unlist(lapply(result$variables, function(x) x$is.factor))
    zeroes <- NULL
    if (sum(whichFact) > 1) {
        nameFact <- names(whichFact)[whichFact]
        counts <- xtabs(as.formula(paste("~", paste(nameFact,
            collapse = "+"))), model.frame(mod))
        zeroes <- which(counts == 0)
    }
    if (length(zeroes) > 0) {
        levs <- expand.grid(lapply(result$variables, function(x) x$levels))
        good <- rep(TRUE, dim(levs)[1])
        for (z in zeroes) {
            good <- good & apply(levs, 1, function(x) !all(x ==
                levs[z, whichFact]))
        }
        result$fit[!good] <- NA
    }
    if (se) {
        if (any(family(mod)$family == c("binomial", "poisson"))) {
            dispersion <- 1
            z <- qnorm(1 - (1 - confidence.level)/2)
        }
        else {
            dispersion <- sum(wts * (residuals(mod))^2, na.rm = TRUE)/mod$df.residual  # dispersion is squared residual standard error?
            z <- qt(1 - (1 - confidence.level)/2, df = mod$df.residual)
        }
        V2 <- dispersion * summary.lm(mod)$cov
        V1 <- vcov(mod)
        V <- if (inherits(mod, "fakeglm")) {
             V1
        } else { V2 }
        vcov <- mod.matrix %*% V %*% t(mod.matrix)
        rownames(vcov) <- colnames(vcov) <- NULL
        var <- diag(vcov)
        result$vcov <- vcov
        result$se <- sqrt(var)
        result$predictor <- mod.matrix[ ,focal.predictors]  ## report value of predictor used for computation of confidence bands
        result$lower <- effect - z * result$se
        result$upper <- effect + z * result$se
        result$confidence.level <- confidence.level
        if (length(zeroes) > 0) {
            result$se[!good] <- NA
            result$lower[!good] <- NA
            result$upper[!good] <- NA
        }
    }
    if (is.null(transformation$link) && is.null(transformation$inverse)) {
        transformation$link <- I
        transformation$inverse <- I
    }
    result$transformation <- transformation
    ## print(result)

    ## report the value of the predictor variable used to computed the raw partial residual
    result$predictor.partial.residual.raw <- predict.data.all.rounded[,focal.predictors]
    result$excluded.predictors <- excluded.predictors
    result$focal.predictors <- focal.predictors
    result$outcome <- all.vars[1]
    class(result) <- "eff"
    result
}



SIMPLELOGPOWER$my.effect.plot.multiple.predictor.simplelogpower <- function(results_best_fitting_model_for_each_age_class_simplelogpower, stockabundance, i){ # i is for ith age class

    .e <- environment()

    results_best_fitting_model_for_each_age_class <- results_best_fitting_model_for_each_age_class_simplelogpower

    usePackage("scales")

    model <- results_best_fitting_model_for_each_age_class[[i]]

    mod <- model$model

    mod.data <- model$data
    mod.formula <- model$formula
    usePackage("stringr")
    ## mod.formula <- str_replace(as.character(mod.formula),"-1","0")
    mod.formula <- as.formula(mod.formula)
    mod.formula

    mod.data <- mod.data[ , names(mod.data) %in% all.vars(mod.formula)]
    mod <- lm(mod.formula, mod.data)

    data <- mod.data

    ##########################################################################################

    all.vars <- all.vars(formula(mod))

    ## data <- expand.model.frame(mod, all.vars)[, all.vars]  ## this doesn't extract all of the data???

    resp <- lhs.vars(formula(mod))

    resp <- attr(resp,"term.labels")

    resp_bracket <-  str_locate(pattern="\\(",resp)

    resp_bracket <- as.numeric(resp_bracket[1,1])

    resp_transf <- substr(resp, start=1, stop=resp_bracket-1)

    teval <- function(...){
         eval(parse(text=paste(...,sep="")))
         }

    ## create log-transformed response
    teval("data$",paste0(resp_transf,".",all.vars[1]),
      " <<- ", paste0(resp_transf,"(","data$",all.vars[1],")"))

    log.resp <- paste0(resp_transf,".",all.vars[1])

    ## create log-transformed predictors

    pred <- rhs.vars(formula(mod))
    pred_no_log <- all.vars[-1]

    log.pred <- NULL
    for (k in 1:length(pred)){
      pred_bracket <-  str_locate(pattern="\\(",pred[k])
      pred_bracket <- as.numeric(pred_bracket[1,1])
      pred_transf <- substr(pred[k], start=1, stop=pred_bracket-1)

      teval("data$",paste0(pred_transf,".",pred_no_log[k]),
        " <<- ", paste0(pred_transf,"(","data$",pred_no_log[k],")"))

      log.pred <- c(log.pred, paste0(pred_transf,".",pred_no_log[k]))
    }

    log.formula <- paste(log.resp," ~ ", paste(log.pred,collapse=" + "))
    log.mod <- lm(log.formula, data=data)

    ##########################################################################################

    ## my.result <- my.Effect.lm(all.vars(mod.formula)[2], mod=mod, se=TRUE, partial.residuals=TRUE)

    my.result <- SIMPLELOGPOWER$my.Effect.lm(all.vars(formula(log.mod))[2], mod=log.mod, se=TRUE, partial.residuals=TRUE)

    if (length(my.result$excluded.predictors)==0) {

      # 51 x 2

    	## my.result <- my.Effect.lm(all.vars(formula(log.mod))[2], mod=log.mod, se=TRUE, partial.residuals=TRUE)
      my.result <- SIMPLELOGPOWER$my.Effect.lm(all.vars(formula(log.mod))[-1], mod=log.mod, se=TRUE, partial.residuals=TRUE)

    	my.df <- data.frame(predictor.partial.residual.raw = my.result$predictor.partial.residual.raw,
                        fitted=my.result$fitted, partial.residuals.raw=my.result$partial.residuals.raw)

    	# 100 x 2
    	my.df.bands <- data.frame(predictor=my.result$predictor,lower=my.result$lower, upper=my.result$upper)

      xlabel <- my.result$focal.predictors
      xlabel <- str_replace(xlabel,"\\.","\\(")
      xlabel <- paste0(xlabel,")")
      xlabel

      ylabel <- all.vars(formula(log.mod))[1]
      ylabel <- str_replace(ylabel,"\\.","\\(")
      ylabel <- paste0(ylabel,")")
      ylabel

      ## gg <- ggplot(data=my.df.bands, environment=.e) +
    	gg <- ggplot(data=my.df.bands, environment=.e) +
               geom_ribbon(data=my.df.bands, aes(x=predictor, ymin=lower, ymax=upper), fill="pink") +
                geom_point(data=my.df,aes(predictor.partial.residual.raw, partial.residuals.raw),
                           colour="darkblue", alpha=0.5) +
              geom_line(data=my.df, aes(predictor.partial.residual.raw, fitted),col="red", size=0.8) +
                ## labs(x=paste(my.result$focal.predictors)) +
                  ## labs(y=paste(my.result$outcome))  +
                   ggtitle(paste(xlabel,"Effect Plot")) +
                    scale_y_continuous(paste(ylabel),labels=comma) +
                     scale_x_continuous(paste(xlabel),labels=comma) +
                       theme_bw() +
                        theme(plot.title=element_text(size=12, hjust=0.5),
                        axis.title.x=element_text(size=10,vjust=-0.5),
                         axis.title.y=element_text(size=10,vjust=1.5),
                          axis.text.x=element_text(size=8),
                           axis.text.y=element_text(size=8),
                            strip.text.x = element_text(size = 8, colour = "black", angle = 0))




     }


    if (length(my.result$excluded.predictors)>0) {

    # 51 x 2


      for (k in 1:length(all.vars(formula(log.mod))[-1])) {

        allvars <-  all.vars(formula(log.mod))[-1]
    		my.result <- SIMPLELOGPOWER$my.Effect.lm(focal.predictors=allvars[k], mod=log.mod, se=TRUE, partial.residuals=TRUE)

        my.df <- data.frame(predictor.partial.residual.raw = my.result$predictor.partial.residual.raw,
                        fitted=my.result$fitted, partial.residuals.raw=my.result$partial.residuals.raw)

        my.result$focal.predictors
        my.result$excluded.predictors

        xlabel <- my.result$focal.predictors
        xlabel <- str_replace(xlabel,"\\.","\\(")
        xlabel <- paste0(xlabel,")")
        xlabel

        ylabel <- all.vars(formula(log.mod))[1]
        ylabel <- str_replace(ylabel,"\\.","\\(")
        ylabel <- paste0(ylabel,")")
        ylabel


    		# 100 x 2
    		my.df.bands <- data.frame(predictor=my.result$predictor,lower=my.result$lower, upper=my.result$upper)

        assign(paste("p", k, sep=""),

               # ggplot(data=my.df.bands, environment=.e)
               ggplot(data=my.df.bands, environment=.e) +
                        geom_ribbon(data=my.df.bands, aes(x=predictor, ymin=lower, ymax=upper), fill="pink") +
                         geom_point(data=my.df,aes(predictor.partial.residual.raw, partial.residuals.raw),
                                    colour="darkblue", alpha=0.5) +
                          geom_line(data=my.df, aes(predictor.partial.residual.raw, fitted),col="red", size=0.8) +
                           ggtitle(paste(xlabel,"Effect Plot")) +
                            scale_y_continuous(paste(ylabel),labels=comma) +
                             scale_x_continuous(paste(xlabel),labels=comma) +
                              theme_bw() +
                               theme(plot.title=element_text(size=12, hjust=0.5), 
                               axis.title.x=element_text(size=10,vjust=-0.5),
                                axis.title.y=element_text(size=10,vjust=1.5),
                                 axis.text.x=element_text(size=8),
                                  axis.text.y=element_text(size=8),
                                   strip.text.x = element_text(size = 8, colour = "black", angle = 0))
                )


      }  # end for

       ## gList(ggplotGrob(gp1),ggplotGrob(gp2))

      ## gg
      ## do.call("grid.arrange", c(gg,ncol=1))

      usePackage("gridExtra")

      tmptext <- paste0("gg <- arrangeGrob(")
      for (k in 1:length(all.vars(mod.formula)[-1])) {
           if (k < length(all.vars(mod.formula)[-1])){
               tmptext <- paste0(tmptext, "p", k, ",")
           } else {
               tmptext <- paste0(tmptext,"p",k,")")
           }
      }


      eval(parse(text=paste(tmptext,sep="")))




     } # end if


    return(gg)

}


## test
## my.effect.plot.multiple.predictor.simplelogpower(results_best_fitting_model_for_each_age_class_simplelogpower, stockabundance, i=1)


# test
## my.effect.plot.multiple.predictor.simplelogpower(results_best_fitting_model_for_each_age_class_simplelogpower, stockabundance, i=3)

## test
## bla <- my.effect.plot.multiple.predictor.simplelogpower(results_best_fitting_model_for_each_age_class_simplelogpower, stockabundance, i=3)
## plot(bla)

## test
## bla <- my.effect.plot.multiple.predictor.simplelogpower(results_best_fitting_model_for_each_age_class_simplelogpower, stockabundance, i=4)
## plot(bla)


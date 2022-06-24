
# This file is a generated template, your changes will not be overwritten

RCIClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "RCIClass",
    inherit = RCIBase,
    private = list(
        #.finalscores = NA,
        #.init = function() {
        #    private$.finalscores <- NULL  
        #},
        .run = function() {

            threshold <- switch(self$options$percentile,
                                "ten" = 0.1,
                                "five" = 0.05,
                                "two" = 0.025)
            
            regdata <- self$data[self$data[,self$options$group] == self$options$refgroup,]
            new <- self$data[self$data[,self$options$group] == self$options$newgroup,]
            
             if(self$options$regtype == "logistic") {
                 
                 my_fmla <- paste0("cbind(", self$options$dep, ", ", paste0(self$options$maximum), " - ", self$options$dep,") ~ ",
                                   paste0(self$options$predictors, collapse = " + "))
                 
                 model <- glm(formula = my_fmla, family = quasibinomial, data = regdata)
                 
                 ibeta <- Vectorize(function(x, a = 2/3, b = 2/3) {
                     pbeta(x, a, b) * beta(a, b)
                 }, "x")
                 
                 X <- model.matrix(model)
                 disp <- summary(model)$dispersion
                 beta <- coef(model)
                 m <- model$prior.weights
                 p <- model$y
                 p_hat <- plogis(as.numeric(X %*% beta))
                 numerator <- ibeta(p) - ibeta(p_hat - (1 - 2 * p_hat)/(6 * m))
                 denominator <- (p_hat * (1 - p_hat))^(-1/6)
                 score <- sqrt(m) * numerator/denominator
                 
                 anova_results <- anova(model, test = "F")
                 
                 disp <- summary(model)$dispersion
                 m <- model$prior.weights[1]
                 p <- as.numeric(new[, all.vars(formula(model))[1]])/m
                 p_hat <- predict(model, new, type = "response")
                 numerator <- ibeta(p) - ibeta(p_hat - (1 - 2 * p_hat)/(6 * m))
                 denominator <- (p_hat * (1 - p_hat))^(-1/6)
                 new_score <- sqrt(m) * numerator/denominator
                 
             } else {
                 
                 my_fmla <- paste0(self$options$dep, " ~ ",
                                   paste0(self$options$predictors, collapse = " + "))
                 
                 model <- lm(formula = my_fmla, data = regdata)
                 
                 m <- model.matrix(model)
                 numerator <- residuals(model, type = "response")
                 sigma <- summary(model)$sigma
                 h <- influence(model)$h
                 denominator <- sigma
                 score <- numerator/denominator
                 
                 anova_results <- anova(model)
                 
                 yhat <- predict(model, new)
                 y <- as.numeric(new[, all.vars(formula(model))[1]])
                 numerator <- y - yhat
                 sigma <- summary(model)$sigma
                 h <- influence(model)$h
                 denominator <- sigma
                 new_score <- numerator/denominator
                 
             }
            
            model_summary <- summary(model)
            ID_below <- which(score < qnorm(threshold))
            n_below <- length(ID_below)
            
            new_ID_below <- which(new_score < qnorm(threshold))
            new_n_below <- length(new_ID_below)
            
            if(!is.null(self$options$id)) {
                who_below <- as.character(self$data[,self$options$id][ID_below])
                new_who_below <- as.character(new[,self$options$id][new_ID_below])
            } else {
                who_below <- c(1:nrow(self$data))[ID_below]
                new_who_below <- c(1:nrow(self$data))[new_ID_below]
            }
            
            citation <- "If you use the Logistic RCI in your work, please cite\n\nDe Andrade Moral, R., Díaz-Orueta, U., & Oltra-Cucarella, J. (2022). Logistic versus\nlinear regression-based reliable change index: A simulation study with implications\nfor clinical studies with different sample sizes. Psychological Assessment."
            
            self$results$citation$setContent(citation)
            self$results$modelsummary$setContent(model_summary)
            self$results$anovaresults$setContent(anova_results)
            self$results$scores$setContent(score)
            self$results$nbelow$setContent(n_below)
            self$results$IDbelow$setContent(who_below)
            self$results$newscores$setContent(new_score)
            self$results$newnbelow$setContent(new_n_below)
            self$results$newIDbelow$setContent(new_who_below)
            
            all_scores <- rep(NA, nrow(self$data))
            all_scores[which(self$data[,self$options$group] == self$options$refgroup)] <- score
            all_scores[which(self$data[,self$options$group] == self$options$newgroup)] <- new_score
            
            self$results$rciOV$setValues(all_scores)
            
        })
)
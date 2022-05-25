
# This file is a generated template, your changes will not be overwritten

#' @importFrom R6 R6Class
#' @import jmvcore
#' @import difNLR
#' @import ShinyItemAnalysis
#' @importFrom difNLR difORD
#' @export


logitClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "logitClass",
    inherit = logitBase,
    private = list(
        .run = function() {

            if (length(self$options$vars)<1) return()
            
            # get variables-------
            
            data <- self$data
            
            groupVarName <- self$options$group
            
            vars <- self$options$vars
            
            varNames <- c(groupVarName, vars)
            
            
            if (is.null(groupVarName))
                
                return()
            
            data <- select(self$data, varNames)
            
            for (var in vars)
                
                data[[var]] <- jmvcore::toNumeric(data[[var]])
            
            # exclude rows with missings in the grouping variable
            
            data <- data[!is.na(data[[groupVarName]]), ]
            
            
            # analysis--------
            
            fit <- difNLR::difORD(
                Data = data, group = groupVarName, focal.name = 1,
                model = self$options$model,
                type = self$options$type, 
                match = self$options$match, 
                p.adjust.method = self$options$padjust, 
                purify = FALSE
            )
            
            
            self$results$text$setContent(fit)
            
            
         #   if(self$options$padjust=='none'){
                
                chi<- fit$Sval
                p<- fit$pval
                padj <- fit$adj.pval
                
                
             #   res<- data.frame(chi, p)
            #    names <-  dimnames(res)[[1]]
           #     dims <- dimnames(res)[[2]]
               
            
                    table <- self$results$method
                    
                    
                    for (i in seq_along(self$options$vars)) {
                        
                        row <- list()
                        
                        row[["chi"]] <- chi[i]
                        row[["p"]] <- p[i]
                        row[["padj"]] <- padj[i]
                        
                        table$setRow(rowKey = vars[i], values = row)
                
                }
                
            
            
            
        })
)

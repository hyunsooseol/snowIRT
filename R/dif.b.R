
# This file is a generated template, your changes will not be overwritten

# Dichotomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import difR
#' @import TAM 
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom TAM tam.mml
#' @importFrom difR difRaju
#' @export



difClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "difClass",
    inherit = difBase,
    private = list(
        
#=============================================================
        
        .init = function() {
            if (is.null(self$data) | is.null(self$options$vars)) {
                self$results$instructions$setVisible(visible = TRUE)
                
            }
            
            self$results$instructions$setContent(
                "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p>Welcome to DIF Rasch Model.</p>
            
            <p><b>To get started:</b></p>
            
            <p>- Each variable should be coded as 0 or 1 with the 'Grouping variable'in jamovi.</p>
            <P>- The focal group should be coded as 1.</P>
            
            <p>- The result tables are estimated by Marginal Maximum Likelihood Estimation(JMLE) using TAM package.</p>
            <p>- the Raju's Z statistics are obtained by using the unsigned areas between the ICCs.</p>
            
            <p>- Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/'  target = '_blank'>GitHub</a></p>
            <p>If you have any questions, please e-mail me: snow@cau.ac.kr</a></p>
            </div>
            </body>
            </html>"
            )
            
            if (self$options$raju)
              self$results$raju$setNote(
                "Note",
                "1. z = Raju's Z statistics. 2. Adj.p = Benjamini-Hochberg p-value adjustment. 3. Effect size(ETS Delta scale) for absolute values of 'deltaRaju' = A: negligible effect(<1.0), B: moderate effect(>1.0),C: large effect(>1.5)."
 
              )
            
           
            if (length(self$options$vars) <= 1)
                self$setStatus('complete')
            
        }, 
        
     #=================================================   
        
        
        .run = function() {

            
            for (varName in self$options$vars) {
                var <- self$data[[varName]]
                if (any(var < 0) | any(var >= 2))
                    stop('The dichotomous model requires dichotomos items(values 0 and 1)')
            }
            
            
            # get variables-------
            
            data <- self$data
            
            vars <- self$options$vars
            
            
            # Ready--------
            
            ready <- TRUE
            
            if (is.null(self$options$vars) ||
                length(self$options$vars) < 2)
                
                ready <- FALSE
            
            if (ready) {
                
               
                results <- private$.compute(data)
                
                # populate dif table-----------
                
                
                private$.populateRajuTable(results)
                
                
            }
            
            
            },
        

# compute results=====================================================

.compute = function(data) {
    
    
    groupVarName <- self$options$group
    
    vars <- self$options$vars
    
    varNames <- c(groupVarName, vars)
    
    if (is.null(groupVarName))
        
        return()
    
    data <- select(self$data, varNames)
    
    for (var in vars)
        
        data[[var]] <- jmvcore::toNumeric(data[[var]])
    
    # exclude rows with missings in the grouping variable
    
    data <- data[ ! is.na(data[[groupVarName]]),]
    
    groupLevels <- base::levels(data[[groupVarName]])
    
    if (length(groupLevels) != 2)
        jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels", code="grouping_var_must_have_2_levels", a=groupVarName)
    
  
   
    ref=dplyr::filter(data, data[[groupVarName]]==0)
    ref.data=dplyr::select(ref, -groupVarName)
    tam.ref <-  TAM::tam.mml(resp = ref.data)
    ref1=tam.ref$xsi
   
    
    
    focal=dplyr::filter(data, data[[groupVarName]]==1)
    focal.data=dplyr::select(focal, -groupVarName)
    tam.focal<- TAM::tam.mml(resp = focal.data)
    focal1=tam.focal$xsi
   
    
   
    # calculating item parameter for each group----------
   
    
    item.1PL<-rbind(ref1,focal1)
    
    res1<- difR::difRaju(irtParam = item.1PL,focal.name = 1,
                         p.adjust.method = "BH",
                         same.scale = FALSE)
    
   
    #dif result---------
    
    zstat<-as.vector(res1$RajuZ)
    
    pvalue <- as.vector(res1$adjusted.p)
    
    
    # get ETS delta scale--------
    
    itk <- 1:length(res1$RajuZ)
    pars <- res1$itemParInit
    J <- nrow(pars)/2
    mR <- pars[1:J, 1]
    mF <- itemRescale(pars[1:J, ], pars[(J + 1):(2 * J),])[, 1]
    
    
    rr1 <- mF - mR
    rr2<- -2.35 * rr1
    
    symb1 <- symnum(abs(rr2), c(0, 1, 1.5, Inf), 
                    symbols = c("A", "B", "C"))
    
    #get ETS delta result------                                                                                                                   
    
    delta <- as.vector(rr2)
    es <- as.vector(symb1)
    
    results <-
        list(
            'zstat'=zstat,
            'pvalue'=pvalue,
            'delta'=delta,
            'es'=es
        )

    # Prepare Data For Plot -------
    image <- self$results$plot
    image$setState(res1)
},


# populate dif table--------------

.populateRajuTable=function(results){
    
    if(is.null(self$options$group))
        
        return()
    
    table <- self$results$raju
    
    items <- self$options$vars
    
    
    # get result---
    
    zstat<- results$zstat
    pvalue<- results$pvalue
    delta<- results$delta
    es<- results$es
    
    
    for (i in seq_along(items)) {
        
        row <- list()
        
        row[["zstat"]] <- zstat[i]
        
        row[["pvalue"]] <- pvalue[i]
        
        row[["delta"]] <- delta[i]
        
        row[["es"]] <- es[i]
        
        
        table$setRow(rowKey = items[i], values = row)
    }
  
    
},

.plot = function(image, ...) {
  
 
  plotData <- image$state
  
  plot <- plot(plotData)
  
  print(plot)
  TRUE
}

  )
)


# This file is a generated template, your changes will not be overwritten

# Dichotomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import difR
#' @import TAM 
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
            <p>Welcome to Dichotomous Rasch Model.</p>

            <p><b>To get started:</b></p>

            <p>- Each variable should be <b>coded as 0 or 1 with the 'Grouping variable'</b> in jamovi.</p>
            <p>- Move items to be assessed for DIF into the 'Variable' box.</p>
            <p>- Move the grouping variable into the 'Grouping variable'box.</p>
            <p>- The focal group should be coded as 1 and Benjamini-Hochberg adjustments for multipl comparisons are provided.</p>
            <p>- The result tables are estimated by Marginal Maximum Likelihood Estimation(JMLE).</p>
            
            <p>- Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/'  target = '_blank'>GitHub</a></p>
            <p>If you have any questions, please e-mail me: snow@cau.ac.kr</a></p>
            </div>
            </body>
            </html>"
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
    
 
#     data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))
    
    # exclude rows with missings in the grouping variable
    
    data <- data[ ! is.na(data[[groupVarName]]),]
    
    groupLevels <- base::levels(data[[groupVarName]])
    
    if (length(groupLevels) != 2)
        jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels", code="grouping_var_must_have_2_levels", a=groupVarName)
    
  
   # managing input data for dif--------------
    
    nF<-sum(data$groupVarName)
    nR<-nrow(data)-nF
    
    data.ref<-data[,1:length(vars)][order(groupVarName),][1:nR,]
    data.focal<-data[,1:length(vars)][order(groupVarName),][(nR+1):(nR+nF),]
    
    # calculating item parameter for each group----------
    
    ref <-  TAM::tam.mml(resp = as.matrix(data.ref))
    focal <-  TAM::tam.mml(resp = as.matrix(data.focal))
    
    ref=ref$xsi
    focal=focal$xsi
    
    item.1PL<-rbind(ref,focal)
    
    res1<- difR::difRaju(irtParam = item.1PL,
                         p.adjust.method = "BH",
                         same.scale = FALSE)
    
    
    
    # ### get difRaju------------
    # 
    # 
    # res1 <- difR::difRaju(data, group = "groupName", focal.name = 1,
    #                       model = "1PL", engine = "ltm",
    #                       p.adjust.method = "BH")
    # 
    # 
    
    
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
    
    diff <- as.vector(rr1)
    delta <- as.vector(rr2)
    es <- as.vector(symb1)
    
    results <-
        list(
            'zstat'=zstat,
            'pvalue'=pvalue,
            'diff'=diff,
            'delta'=delta,
            'es'=es
        )
            
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
    diff<- results$diff
    delta<- results$delta
    es<- results$es
    
    
    for (i in seq_along(items)) {
        
        row <- list()
        
        row[["z"]] <- zstat[i]
        row[["p"]] <- pvalue[i]
        
        row[["Difference"]] <- diff[i]
        
        row[["deltaRaju"]] <- delta[i]
        
        row[["Effect size"]] <- es[i]
        
        
        table$setRow(rowKey = items[i], values = row)
    }
    
    
    
}


  )
)
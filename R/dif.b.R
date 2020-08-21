
# This file is a generated template, your changes will not be overwritten

# Dichotomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import difR
#' @importFrom TAM tam.jml
#' @importFrom TAM tam.jml.fit
#' @importFrom TAM tam.fit
#' @importFrom TAM tam.mml
#' @importFrom TAM tam.modelfit
#' @importFrom TAM tam
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

            <p>- Each variable must be <b>coded as 0 or 1 with the type of numeric-continuous</b> in jamovi.</p>
            <p>- Just highlight the variables and click the arrow to move it across into the 'Variables' box.</p>
            <p>- The result tables are estimated by Joint Maximum Likelihood(JML) estimation.</p>
            <p>- MADaQ3 statistic(an effect size of model fit) is estimated based on Marginal Maximum Likelihood(MML) estimation.</P>
            <p>- The rationale of snowIRT module is described in the <a href='https://bookdown.org/dkatz/Rasch_Biome/' target = '_blank'>documentation</a></p>
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
            
            # groupName<- self$options$group
            
            # groupLevels <- base::levels(data[[groupName]])
            # 
            # if (length(groupLevels) != 2)
            #   jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels", code="grouping_var_must_have_2_levels", a=groupName)
            
            
            # Ready--------
            
            ready <- TRUE
            
            if (is.null(self$options$vars) ||
                length(self$options$vars) < 2)
                
                ready <- FALSE
            
            if (ready) {
                
                data <- private$.cleanData()
                
                results <- private$.compute(data)
                
                # populate dif table-----------
                
                
                private$.populateRajuTable(results)
                
            }
            
            },
        

# compute results=====================================================

.compute = function(data) {
    
    # get variables------
    
    data <- self$data
    
    vars <- self$options$vars
    
    groupName <- self$options$group
    
   
    if(is.null(self$options$group))
        
        return()
    
    ## Splitting the data into reference and focal groups
    
    nF<-sum(groupName)
    
    nR<-nrow(data)-nF
    
    data.ref<-data[,1:length(vars)][order(groupName),][1:nR,]
   
    
    data.focal<-data[,1:length(vars)][order(groupName),][(nR+1):(nR+nF),]
    
    ref <-  TAM::tam.mml(resp = data.ref)
    focal <-  TAM::tam.mml(resp = data.focal)
    
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
    
    
    # get ETS result--------
    
    itk <- 1:length(res1$RajuZ)
    pars <- res1$itemParInit
    J <- nrow(pars)/2
    mR <- pars[1:J, 1]
    mF <- itemRescale(pars[1:J, ], pars[(J + 1):(2 * J),])[, 1]
    
    
    rr1 <- mF - mR
    rr2<- -2.35 * rr1
    
    symb1 <- symnum(abs(rr2), c(0, 1, 1.5, Inf), 
                    symbols = c("A", "B", "C"))
    
    #get result------                                                                                                                   
    
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
    
    
    
},

#### Helper functions =================================

.cleanData = function() {
    items <- self$options$vars
    
    data <- list()
    
    for (item in items)
        data[[item]] <- jmvcore::toNumeric(self$data[[item]])
    
    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'
    data <- jmvcore::naOmit(data)
    
    return(data)
     }

   )
)

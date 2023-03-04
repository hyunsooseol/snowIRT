
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
#' @importFrom ShinyItemAnalysis plotDIFirt
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
            <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p>1. Each variable should be coded as 0 or 1 with the 'Grouping variable'in jamovi.</p>
            <P>2. The focal group should be coded as 1.</P>
            <p>3. The Raju's Z statistics are estimated by using <b>difR::difRaju</b> function.</p></p>
            <p>4. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
            </div>
            </body>
            </html>"
  )
  
  if (self$options$raju)
    self$results$raju$setNote(
      "Note",
      "1. Effect size(ETS Delta scale) for absolute values of 'deltaRaju' = A: negligible effect(<1.0), B: moderate effect(>1.0),C: large effect(>1.5)."
      
    )
  
  
  if (length(self$options$vars) <= 1)
    self$setStatus('complete')
  
},

#=================================================


.run = function() {
  # get variables-------
  
  data <- self$data
  
  groupVarName <- self$options$group
  
  vars <- self$options$vars
  
  varNames <- c(groupVarName, vars)
  
  padjust<- self$options$padjust
  
  if (is.null(groupVarName))
    
    return()
  
  data <- select(self$data, varNames)
  
  for (var in vars)
    
    data[[var]] <- jmvcore::toNumeric(data[[var]])
  
  # exclude rows with missings in the grouping variable
  
  data <- data[!is.na(data[[groupVarName]]), ]
  
  groupLevels <- base::levels(data[[groupVarName]])
  
  if (length(groupLevels) != 2)
    jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels",
                    code = "grouping_var_must_have_2_levels",
                    a = groupVarName)
  
  
  ref = dplyr::filter(data, data[[groupVarName]] == 0)
  ref.data = dplyr::select(ref,-groupVarName)
  tam.ref <-  TAM::tam.mml(resp = ref.data)
  ref1 = tam.ref$xsi
  
  
  focal = dplyr::filter(data, data[[groupVarName]] == 1)
  focal.data = dplyr::select(focal,-groupVarName)
  tam.focal <- TAM::tam.mml(resp = focal.data)
  focal1 = tam.focal$xsi
  
  
  # calculating item parameter for each group----------
  
  
  item.1PL <- rbind(ref1, focal1)
  
  res1 <- difR::difRaju(
    irtParam = item.1PL,
    focal.name = 1,
    p.adjust.method = padjust,
    same.scale = FALSE
  )
  
  
  # ICC PLOT RESULT----------
  
  # Coefficients for all items
  itempar <- res1$itemParInit
  
  
  #dif result---------
  
  zstat <- as.vector(res1$RajuZ)
  
  p <- as.vector(res1$p.value)
  
  padjust <- as.vector(res1$adjusted.p)
  
  
  # get ETS delta scale--------
  
  itk <- 1:length(res1$RajuZ)
  pars <- res1$itemParInit
  J <- nrow(pars) / 2
  mR <- pars[1:J, 1]
  mF <- itemRescale(pars[1:J,], pars[(J + 1):(2 * J), ])[, 1]
  
  
  rr1 <- mF - mR
  rr2 <- -2.35 * rr1
  
  symb1 <- symnum(abs(rr2), c(0, 1, 1.5, Inf),
                  symbols = c("A", "B", "C"))
  
  #get ETS delta result------
  
  delta <- as.vector(rr2)
  es <- as.vector(symb1)
  
  results <-
    list(
      'zstat' = zstat,
      'p'= p,
      'padjust' = padjust,
      'delta' = delta,
      'es' = es,
      'itempar'=itempar
    )
  
  
  if (is.null(self$options$group))
    
    return()
  
  table <- self$results$raju
  
  items <- self$options$vars
  
  # get result---
  
  zstat <- results$zstat
  p <- results$p
  padjust <- results$padjust
  delta <- results$delta
  es <- results$es
  
  
  for (i in seq_along(items)) {
    row <- list()
    
    row[["zstat"]] <- zstat[i]
    
    row[["p"]] <- p[i]
    
    row[["padjust"]] <- padjust[i]
    
    row[["delta"]] <- delta[i]
    
    row[["es"]] <- es[i]
    
    
    table$setRow(rowKey = items[i], values = row)
  }
  
  
  # Prepare Data For Plot -------
  image <- self$results$zplot
  image$setState(res1)
 
  # prepare dif icc plot--------
  
  image<- self$results$iccplot
  image$setState(itempar)
  
  
},


.plot = function(image, ...) {
  
  if (is.null(image$state))
    return(FALSE)
  
  plotData <- image$state
  
  plot <- plot(plotData)
  
  print(plot)
  TRUE
},

.iccplot=function(image,...){

  itempar <- image$parent$state
  
  if (is.null(itempar))
    return()
  
  images <- self$results$iccplot
  
  index <- 1
  
  for (item in images$items) {
    if (identical(image, item))
      break()
    
    index <- index + 1
  }
  
  
  plot2 <- ShinyItemAnalysis::plotDIFirt(parameters = itempar, 
                                         item = index, 
                                         test = "Raju")
  
  print(plot2)
  TRUE
  
}


    )
)

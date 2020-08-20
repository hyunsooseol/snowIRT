
# Dichotomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ltm
#' @import difR
#' @import MASS
#' @import msm
#' @import polycor
#' @importFrom TAM tam.jml
#' @importFrom TAM tam.jml.fit
#' @importFrom TAM tam.fit
#' @importFrom TAM tam.mml
#' @importFrom TAM tam.modelfit
#' @importFrom TAM IRT.WrightMap
#' @importFrom TAM tam
#' @importFrom difR difRaju
#' @export


dichotomousClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "dichotomousClass",
    inherit = dichotomousBase,
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
        
        #  private$.initItemsTable()
        
        if (self$options$modelfitp)
          self$results$scale$setNote(
            "Note",
            "MADaQ3= Mean of absolute values of centered Q_3 statistic with p value obtained by Holm
adjustment; Ho= the data fit the Rasch model."
          )
        
        if (self$options$infit)
          self$results$items$setNote(
            "Note",
            "Infit= Information-weighted mean square statistic; Outfit= Outlier-sensitive means square statistic."
          )
        
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      
      
      #======================================++++++++++++++++++++++
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
          
          # populate scale table-----
          
          private$.populateScaleTable(results)
          
          # populate item table----
          
          private$.populateItemsTable(results)
          
          # Populate q3 matrix table-----
          
          private$.populateMatrixTable(results)
          
          # populate dif table-----------
          
          
          private$.populateRajuTable(results)
          
          
          # prepare plot-----
          
          private$.prepareWrightmapPlot(data)
          
          # prepare Expected score curve plot---------
          
          private$.prepareEscPlot(data)
        }
        
      },
      
      
      # compute results=====================================================
      
      .compute = function(data) {
        
        # get variables------
        
        data <- self$data
        
        vars <- self$options$vars
        
       
        if(is.null(self$options$group)){
        
          # estimate the Rasch model with JML using function 'tam.jml'-----
        
        tamobj = TAM::tam.jml(resp = as.matrix(data))
        
        
        # estimate Item Total Score(Sufficient Statistics)-------
        
        itotal <- tamobj$ItemScore
        itotal <- -itotal
        
        # computing proportions for each item---------
        
        n <- tamobj$nstud
        prop <- itotal / n
        
        # estimate item difficulty measure---------------
        
        imeasure <-  tamobj$xsi
        
        
        # estimate standard error of the item parameter-----
        
        ise <- tamobj$errorP
        
        
        # computing infit and outfit statistics---------------------
        
        fit <- TAM::tam.fit(tamobj)
        
        infit <- fit$fit.item$infitItem
        
        outfit <- fit$fit.item$outfitItem
        
        
        # computing person separation reliability-------
        
        reliability <- tamobj$WLEreliability
        
        #computing an effect size of model fit(MADaQ3) using MML-------
        
        tamobj1 = TAM::tam.mml(resp = as.matrix(data))
        
        # assess model fit----
        
        res <- TAM::tam.modelfit(tamobj = tamobj1)
        
        modelfit <- res$stat.MADaQ3$MADaQ3
        
        # pvalue--------
        
        modelfitp <- res$stat.MADaQ3$p
        
        # q3 matrix----------
        
        mat <- res$Q3.matr
        
        } else {
       
        
        ### get difRaju------------
        
        
        res1 <- difR::difRaju(data, group = "groupName", focal.name = 1,
                              model = "1PL", engine = "ltm",
                              p.adjust.method = "BH")
        
       
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
       
        }
        
        results <-
          list(
            'itotal' = itotal,
            'prop' = prop,
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            'reliability' = reliability,
            'modelfit' = modelfit,
            'modelfitp' = modelfitp,
            'mat' = mat
            # 'zstat'=zstat,
            # 'pvalue'=pvalue,
            # 'diff'=diff,
            # 'delta'=delta,
            # 'es'=es
            )
      
        
        if(!is.null(self$options$group)){

        results$zstat <- zstat
        results$pvaue <- pvalue
        results$diff <- diff
        results$delta <- delta
        results$es <- es

        }
          
      },
      
  
  
      #### Init. tables ====================================================
      
      .initItemsTable = function() {
        
       table <- self$results$items
        
        for (i in seq_along(items))
          table$addFootnote(rowKey = items[i], 'name')
        
      },
      
      # populate scale table-------------------
      
      .populateScaleTable = function(results) {
        
       
        table <- self$results$scale
        
        reliability <- results$reliability
        
        modelfit <- results$modelfit
        modelfitp <- results$modelfitp
        
        row <- list()
        
        row[['reliability']] <- reliability
        row[['modelfit']] <- modelfit
        row[['modelfitp']] <- modelfitp
        
        table$setRow(rowNo = 1, values = row)
        
        
      },
      
      
      # Populate q3 matrix table-----
      
      .populateMatrixTable = function(results) {
        
        
        # get variables---------------------------------
        
        matrix <- self$results$get('mat')
        vars <- self$options$get('vars')
        nVars <- length(vars)
        
        # add columns--------
        
        for (i in seq_along(vars)) {
          var <- vars[[i]]
          
          matrix$addColumn(
            name = paste0(var, '[r]'),
            title = var,
            type = 'number',
            format = 'zto'
          )
          
          # empty cells above and put "-" in the main diagonal
          
          for (i in seq_along(vars)) {
            var <- vars[[i]]
            
            values <- list()
            
            for (j in seq(i, nVars)) {
              v <- vars[[j]]
              
              values[[paste0(v, '[r]')]]  <- ''
              
            }
            values[[paste0(var, '[r]')]]  <- '\u2014'
            matrix$setRow(rowKey = var, values)
            
          }
          
          
          data <- self$data
          
          for (v in vars)
            data[[v]] <- jmvcore::toNumeric(data[[v]])
          
          #compute again------
          
          mat <- results$mat
          
          # populate result----------------------------------------
          
          for (i in 2:nVars) {
            for (j in seq_len(i - 1)) {
              values <- list()
              
              values[[paste0(vars[[j]], '[r]')]] <- mat[i, j]
              
              matrix$setRow(rowNo = i, values)
            }
          }
        }
        
      },
      
      # populate item tables==============================================
      
      .populateItemsTable = function(results) {
        
        table <- self$results$items
        
        items <- self$options$vars
        
        
        itotal <- results$itotal
        prop <- results$prop
        
        imeasure <- results$imeasure
        ise <- results$ise
        
        infit <- results$infit
        outfit <- results$outfit
        
        
        for (i in seq_along(items)) {
          row <- list()
          
          
          row[["total"]] <- itotal[i, 1]
          row[["prop"]] <- prop[i, 1]
          
          row[["measure"]] <- imeasure[i]
          
          row[["ise"]] <- ise[i]
          
          row[["infit"]] <- infit[i]
          
          row[["outfit"]] <- outfit[i]
          
          
          table$setRow(rowKey = items[i], values = row)
        }
        
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

      
  
  #### Prepare Plot functions ----
      
      .prepareWrightmapPlot = function(data) {
        wright = TAM::tam.mml(resp = as.matrix(data))
        
        
        # Prepare Data For Plot -------
        
        image <- self$results$plot
        image$setState(wright)
        
        
      },
      
 # Prepare Expected score curve functions------------
      
 .prepareEscPlot = function(data) {
   
   tamp = TAM::tam(resp =as.matrix(data))
   
  
   # Prepare Data For ESC Plot -------
   
   image <- self$results$get('esc')
   
   image$setState(tamp)
   
   
 },

 
 # Expected score curve plot----------
 
 
 .escPlot = function(image, ...) {
   
   tamp <- image$parent$state
   
   if (is.null(tamp))
     return()
   
   images <- self$results$esc
   
   index <- 1

   for (item in images$items) {
     if (identical(image, item))
       break()

     index <- index + 1
   }

    plot1 <- plot(tamp,
                  items = index,
                  #type="items" produce item response curve not expected curve
                  type = "expected",
                  export = FALSE)

  print(plot1)
   TRUE
   
   
 },
 
 
 #================================================================
      
      .plot = function(image, ...) {
   
        wrightmap <- self$options$wrightmap
        
        if (!wrightmap)
          return()
       
        
        wright <- image$state
        
        plot <- TAM::IRT.WrightMap(wright)
        
        print(plot)
        TRUE
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

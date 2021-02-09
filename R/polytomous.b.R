
# Polytomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom TAM tam.mml
#' @importFrom TAM tam.fit
#' @importFrom TAM tam.modelfit
#' @importFrom TAM tam.threshold
#' @importFrom TAM tam
#' @importFrom TAM tam.wle
#' @importFrom ShinyItemAnalysis ggWrightMap
#' @import ggplot2
#' @export


polytomousClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "polytomousClass",
    inherit = polytomousBase,
    private = list(
      #======================================================
      
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
           
            <p> Note that Polytomous model needs <b>the bottom category to be coded as 0.</b>
            
            <p> The result tables are estimated by Marginal Maximum likelihood Estimation(MMLE).</p>
            <p> The rationale of snowIRT module is described in the <a href='https://bookdown.org/dkatz/Rasch_Biome/' target = '_blank'>documentation.</a></p>
            <p> Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/'  target = '_blank'>GitHub.</a></p>

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
        
        if (self$options$thresh)
          self$results$thresh$setNote(
            "Note",
            "The Thurstonian threshold for a score category is defined as the ability at which the probability of achieving that score or higher reaches 0.50."
          )
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      
      
      #======================================++++++++++++++++++++++
      
      .run = function() {
        allDicho <- TRUE
        for (varName in self$options$vars) {
          var <- self$data[[varName]]
          if (any(var != 0 || var != 1))
            allDicho <- FALSE
        }
        if (allDicho)
          stop(
            'The polytomous model requires Likert-type items,
          for binary data use the dichotomous model instead.'
          )
        
        # get variables-------
        
        data <- self$data
        
        vars <- self$options$get('vars')
        
        
        # Ready--------
        
        ready <- TRUE
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          data <- private$.cleanData()
          
          results <- private$.compute(data)
          
          #populate scale table-----
          
          private$.populateScaleTable(results)
          
          
          # populate item table----
          
          private$.populateItemsTable(results)
          
          # Populate q3 matrix table-----
          
          private$.populateMatrixTable(results)
          
          # populate thurstonian thresholds
          
          private$.populateThresholdsTable(results)
          
          # populate person table------
          
          private$.populatePersonTable(results)
          
          #prepare plot-----
          
          private$.prepareIccPlot(data)
          
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
        
        vars <- self$options$get('vars')
        
        
        # estimate the Rasch model with MML using function 'tam.mml'-----
        
        tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        
        
        # estimate item difficulty measure---------------
        
        imeasure <- tamobj$item_irt[[3]]
        
        
        # estimate standard error of the item parameter-----
        
        ise <- tamobj$se.AXsi[,2]
        
        
        # computing infit and outfit statistics---------------------
        
        infit <- TAM::tam.fit(tamobj)$itemfit$Infit
        
        
        outfit <- TAM::tam.fit(tamobj)$itemfit$Infit
        
        
        # computing person separation reliability-------
        
       
        person<- TAM::tam.wle(tamobj)
        
        reliability<- person$WLE.rel
        
        
        # person statistics------------------
        
        total<- person$PersonScores
        personmeasure<- person$theta
        pse <- person$error
        
        #computing an effect size of model fit(MADaQ3)-------
        
       # assess model fit
        res <- TAM::tam.modelfit(tamobj)
        
        modelfit <- res$stat.MADaQ3$MADaQ3
        
        # pvalue--------
        modelfitp <- res$stat.MADaQ3$p
        
        # q3 matrix----------
        
        mat <- res$Q3.matr
        
        # Calculation of Thurstonian thresholds----
        
        thresh <- TAM::tam.threshold(tamobj)
        nc <- ncol(thresh)
        
        # Partial credit model using MML estimation---
        
        tampartial = TAM::tam.mml(resp = as.matrix(data))
         pmeasure <- tampartial$item_irt$beta
        
        
        results <-
          list(
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            'total' = total,
            'personmeasure'=personmeasure,
            'pse'=pse,
            'reliability' = reliability,
            'modelfit' = modelfit,
            'modelfitp' = modelfitp,
            'mat' = mat,
            'thresh' = thresh,
            'nc' = nc,
            'pmeasure' = pmeasure
          )
        
      },
      
      # Init. tables ------------------------------------
      
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
        
        row[['reliability']] <- reliability[1]
        row[['modelfit']] <- modelfit
        row[['modelfitp']] <- modelfitp
        
        table$setRow(rowNo = 1, values = row)
        
        
      },
      
      
      # populate item tables----------------------
      
      .populateItemsTable = function(results) {
        table <- self$results$items
        
        items <- self$options$vars
        
        
        imeasure <- results$imeasure
        ise <- results$ise
        
        infit <- results$infit
        outfit <- results$outfit
        
        
        for (i in seq_along(items)) {
          row <- list()
          
          
          row[["measure"]] <- imeasure[i]
          
          row[["ise"]] <- ise[i]
          
          row[["infit"]] <- infit[i]
          
          row[["outfit"]] <- outfit[i]
          
          
          table$setRow(rowKey = items[i], values = row)
        }
        
        
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
            name = paste0(var),
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
              
              values[[paste0(v)]]  <- ''
              
            }
            values[[paste0(var)]]  <- '\u2014'
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
              
              values[[paste0(vars[[j]])]] <- mat[i, j]
              
              matrix$setRow(rowNo = i, values)
            }
          }
        }
        
      },
      
      
      
      #  populate Thurstonian thresholds------------
      
      .populateThresholdsTable = function(results) {
        table <- self$results$thresh
        
        thr <- results$thresh # matrix
        pmeasure <- results$pmeasure
        
        nCategory <- results$nc # number of thresholds
        
        vars <- self$options$vars
        
        
        if (nCategory > 1) {
          for (i in 1:nCategory)
            
            table$addColumn(
              name = paste0("name", i),
              title = as.character(i),
              superTitle = 'Threshold',
              type = 'number'
            )
        }
        
        
        
        for (i in seq_along(vars)) {
          row <- list()
          
          
          for (j in 1:nCategory) {
            row[[paste0("name", j)]] <- thr[i, j]
            
            
          }
          
          row[["pmeasure"]] <- pmeasure[i]
          table$setRow(rowNo = i, values = row)
        }
      },
      
   
      # populate person tables----------------------
      
      .populatePersonTable = function(results) {
        
        table <- self$results$persons
        
         data<- self$data
      
         #result---
         
         total <- results$total
         
         personmeasure <- results$personmeasure
         
         pse <- results$pse
         
         
         for (i in 1:nrow(data)) {
           
           row <- list()
           
           
           row[["total"]] <- total[i]
           
           row[["personmeasure"]] <- personmeasure[i]
           
           row[["pse"]] <- pse[i]
           
           table$addRow(rowKey = i, values = row)
           
         }
         
         
      },
         
      
      #### Plot functions ------------------------
      
      .prepareIccPlot = function(data) {
        
        # item characteristic curves based on partial credit model--------
        
        tam <- TAM::tam.mml(resp = as.matrix(data))
        
        
        # Prepare Data For Plot -------
        
        image <- self$results$plot
        image$setState(tam)
        
      },
      
      ### wrightmap Plot functions ----------------------------
      
      
      .prepareWrightmapPlot = function(data) {
        
        
        tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        
        
        # person statistics-----------  
        
        person<- TAM:: tam.wle(tamobj)
       
        pmeasure<- person$theta
        
       
        # item difficulty for rating scale model---------------
        
       
        imeasure <- tamobj$item_irt[[3]]
        
        # plot-----------------
        
        image <- self$results$wrightmap
        
        nvars <- length(self$options$vars)
        
        # width <- 400 + nvars * 30
        # 
        # image$setSize(width, 400)
        
        state <- list(pmeasure, imeasure)
        
        image$setState(state)
        
      },
      
      
      
      #================================================================
      
      .plot = function(image, ...) {
        
        tam <- image$parent$state
        
        if (is.null(tam))
          return()
        
        images <- self$results$plot
        
        index <- 1
        
        for (item in images$items) {
          if (identical(image, item))
            break()
          
          index <- index + 1
        }
        
        plot <- plot(tam,
                     items = index,
                     type = 'items',
                     export = FALSE)
        
        
        print(plot)
        TRUE
        
      },
 
      # wright map plot--------------
      
      .wrightmapPlot = function(image,ggtheme, theme, ...) {
        
        wrightmap <- self$options$wrightmap
        
        if (!wrightmap)
          return()
        
        pmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        
        # plot1 <- WrightMap::wrightMap(pmeasure,imeasure,
        #                              show.thr.lab= FALSE,
        #                              thr.sym.cex = 2.0,
        #                              thr.sym.pch = 17,
        #                              axis.persons = "Person distribution",
        #                              thr.sym.col.fg = RColorBrewer::brewer.pal(10, "Paired"))
        # 
        
        plot1<- ShinyItemAnalysis::ggWrightMap(pmeasure, imeasure,
                                              color = "deepskyblue")
        
        
        
        plot1 <- plot1+ggtheme
        
        
        
        print(plot1)
        TRUE
        
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
        
        plot2 <- plot(tamp,
                      items = index,
                      #type="items" produce item response curve not expected curve
                      type = "expected",
                      export = FALSE)
        
        print(plot2)
        TRUE
        
        
      },
      
      
      ### Helper functions =================================
      
      .cleanData = function() {
        items <- self$options$vars
        
        data <- list()
        
        for (item in items)
          data[[item]] <-
          jmvcore::toNumeric(self$data[[item]])
        
        attr(data, 'row.names') <-
          seq_len(length(data[[1]]))
        attr(data, 'class') <- 'data.frame'
        data <- jmvcore::naOmit(data)
        
        return(data)
      }
      
      
    )
  )

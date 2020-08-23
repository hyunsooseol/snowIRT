
# Polytomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom TAM tam.mml
#' @importFrom TAM tam.fit
#' @importFrom TAM tam.modelfit
#' @importFrom TAM IRT.WrightMap
#' @importFrom TAM tam.threshold
#' @importFrom TAM tam
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
            <p>Welcome to Polytomous Rasch Model.</p>

            <p><b>To get started:</b></p>

            <p>- The input dataset require polytomous data with the type of <b>numeric-continuous</b> in jamovi.</p>
            <p>- Note that Polytomous model needs <b>the bottom category to be coded as 0</b>, so you may need to recode.
            <p>- Just highlight the variables and click the arrow to move it across into the 'Variables' box.</p>
            
            <p>- The result tables are estimated by Marginal Maximum likelihood Estimation(MMLE) using TAM package.</p>
            <p>- The item and model fit statistics are estimated by Andrich's rating scale model based on Marginal Maximum Likelihood(MML).</P>
            <P>- Item characteristic curves are visualized using Partial Credit Model.</p>
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
          if (any(var != 0 && var != 1))
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
        
        imeasure <- tamobj$xsi$xsi
        
        
        # estimate standard error of the item parameter-----
        
        ise <- tamobj$xsi$se.xsi
        
        
        # computing infit and outfit statistics---------------------
        
        infit <- TAM::tam.fit(tamobj)$itemfit$Infit
        
        
        outfit <- TAM::tam.fit(tamobj)$itemfit$Infit
        
        
        # computing person separation reliability-------
        
        # reliability <- tamobj$EAP.rel
        
        abil<- tam.wle(tamobj)
        
        reliability<- abil$WLE.rel
        
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
        
        pmeasure <- tamobj$item_irt$beta
        
        
        results <-
          list(
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
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
      
      
      
      #### Plot functions ------------------------
      
      .prepareIccPlot = function(data) {
        # item characteristic curves based on partial credit model--------
        
        tam <- TAM::tam.mml(resp = as.matrix(data))
        
        # Prepare Data For Plot -------
        
        image <- self$results$get('plot')
        image$setState(tam)
        
      },
      
      ### wrightmap Plot functions ----
      
      
      .prepareWrightmapPlot = function(data) {
        wright = TAM::tam.mml(resp = as.matrix(data))
        
        
        # Prepare Data For wrightmap Plot -------
        
        image <- self$results$wrightmap
        image$setState(wright)
        
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
      
      .wrightmapPlot = function(image, ...) {
        wrightmap <- self$options$wrightmap
        
        if (!wrightmap)
          return()
        
        
        wright <- image$state
        
        plot1 <- TAM::IRT.WrightMap(wright)
        
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

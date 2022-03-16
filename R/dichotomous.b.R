
# Dichotomous Rasch model
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom TAM tam.fit
#' @importFrom TAM tam.mml
#' @importFrom TAM tam.modelfit
#' @importFrom TAM tam
#' @importFrom difR difRaju
#' @importFrom TAM tam.wle
#' @importFrom TAM tam.personfit
#' @importFrom ShinyItemAnalysis ggWrightMap
#' @import ggplot2
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
            <p><b>Instructions</b></p>
            <p>____________________________________________________________________________________</p>
            <p>1. Each variable must be <b>coded as 0 or 1 with the type of numeric-continuous</b> in jamovi.</p>
            <p>2. The results of <b> Save </b> will be displayed in the datasheet.</p>
            <p>3. The result tables are estimated by Marginal Maximum Likelihood estimation(MMLE).</p>
            <p>4. The rationale of snowIRT module is described in the <a href='https://bookdown.org/dkatz/Rasch_Biome/' target = '_blank'>documentation.</a></p>
            <p>5. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub.</a></p>
            <p>____________________________________________________________________________________</p>
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

        # for (varName in self$options$vars) {
        #   var <- self$data[[varName]]
        #   if (any(var < 0) | any(var >= 2))
        #     stop('The dichotomous model requires dichotomos items(values 0 and 1)')
        # }

        
        # # get variables-------
        # 
        # data <- self$data
        # 
        # vars <- self$options$vars
        # 
        
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
          
          # Populate person table-----
          
         # private$.populatePersonTable(results)
          
          # populate output variables-----
          
          private$.populateOutputs(data)
          
          # prepare plot-----
          
          private$.prepareWrightmapPlot(data)
          
          # prepare Expected score curve plot---------
          
          private$.prepareEscPlot(data)
        }
        
      },
      
      
      # compute results=====================================================
      
      .compute = function(data) {
        
        # get variables------
        # 
        # data <- self$data
        # 
        # vars <- self$options$vars
        # 
      
        # estimate the Rasch model with MMLE-----
        
        tamobj = TAM::tam.mml(resp = as.matrix(data))
        
        # computing item mean
        
        prop <- tamobj$item$M
        
        
        # estimate item difficulty measure---------------
        
        imeasure <-tamobj$xsi$xsi
        
        
        # estimate standard error of the item parameter-----
        
        ise <- tamobj$xsi$se.xsi
        
        
        # computing infit and outfit statistics---------------------
        
        fit <- TAM::tam.fit(tamobj)
        
        infit <- fit$itemfit$Infit
        
        outfit <- fit$itemfit$Outfit
        
        
        # computing person separation reliability-------
        
        person<- TAM::tam.wle(tamobj)
        
        reliability<- person$WLE.rel
        
        # person statistics
        
        # total<- person$PersonScores
        # pmeasure<- person$theta
        # pse <- person$error
        # 
        
        #computing an effect size of model fit(MADaQ3) using MML-------
        
       # assess model fit----
        
        model <- TAM::tam.modelfit(tamobj)
        
        modelfit <- model$stat.MADaQ3$MADaQ3
        
        # pvalue--------
        
        modelfitp <- model$stat.MADaQ3$p
        
        # q3 matrix----------
        
        mat <- model$Q3.matr
        
      
        results <-
          list(
            'prop' = prop,
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            # 'total' = total,
            # 'pmeasure'=pmeasure,
            # 'pse'=pse,
            'reliability' = reliability,
            'modelfit' = modelfit,
            'modelfitp' = modelfitp,
            'mat' = mat
          
            )
      
       
          
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
        
        row[['reliability']] <- reliability[1]
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
      
      # populate item table==============================================
      
      .populateItemsTable = function(results) {
        
        table <- self$results$items
        
        items <- self$options$vars
        
        
        prop <- results$prop
        
        imeasure <- results$imeasure
        ise <- results$ise
        
        infit <- results$infit
        outfit <- results$outfit
        
        
        for (i in seq_along(items)) {
          row <- list()
          
          
          row[["prop"]] <- prop[i]
          
          row[["measure"]] <- imeasure[i]
          
          row[["ise"]] <- ise[i]
          
          row[["infit"]] <- infit[i]
          
          row[["outfit"]] <- outfit[i]
          
          
          table$setRow(rowKey = items[i], values = row)
        }
        
      },
      
 
######### person output variables========================

       .populateOutputs= function(data) {
         
         if (self$options$total && self$results$total$isNotFilled()) {
         
           tamobj = TAM::tam.mml(resp = as.matrix(data))
           person<- TAM::tam.wle(tamobj)
           
          
           # person statistics
           
           total<- person$PersonScores
           
           # total <- results$total
           
           self$results$total$setRowNums(rownames(data))
           self$results$total$setValues(total)
           
         }
         
         if (self$options$pmeasure && self$results$pmeasure$isNotFilled()) {
           
           tamobj = TAM::tam.mml(resp = as.matrix(data))
           person<- TAM::tam.wle(tamobj)
          
            pmeasure<- person$theta
           
           # pmeasure <- results$pmeasure
           
           self$results$pmeasure$setRowNums(rownames(data))
           self$results$pmeasure$setValues(pmeasure)
           
         }
         
         if (self$options$pse && self$results$pse$isNotFilled()) {
           
           tamobj = TAM::tam.mml(resp = as.matrix(data))
           person<- TAM::tam.wle(tamobj)
           
           
            pse <- person$error
           
          # pse <- results$pse
           
           self$results$pse$setRowNums(rownames(data))
           self$results$pse$setValues(pse)
           
         }
         
         if (self$options$pinfit && self$results$pinfit$isNotFilled()) {
           
           tamobj = TAM::tam.mml(resp = as.matrix(data))
           
           fit <- TAM::tam.personfit(tamobj)
           
           pinfit <- fit$infitPerson
           
           self$results$pinfit$setRowNums(rownames(data))
           self$results$pinfit$setValues(pinfit)
         
         }
         
         if (self$options$poutfit && self$results$poutfit$isNotFilled()) {
           
           tamobj = TAM::tam.mml(resp = as.matrix(data))
           
           fit <- TAM::tam.personfit(tamobj)
           
           poutfit <- fit$outfitPerson
           
           self$results$poutfit$setRowNums(rownames(data))
           self$results$poutfit$setValues(poutfit)
           
         }
         
         
       },
      
      
      # person statistics
 
      # .populatePersonTable = function(results) {  
      # 
      #   data <- self$data
      #   
      #   table <- self$results$persons
      #   
      #   
      #   #result---
      #   
      #   total <- results$total
      #   
      #   pmeasure <- results$pmeasure
      #   
      #   pse <- results$pse
      #   
      #   
      #   for (i in 1:nrow(data)) {
      #     
      #     row <- list()
      #     
      #     
      #     row[["total"]] <- total[i]
      #     
      #     row[["pmeasure"]] <- pmeasure[i]
      #     
      #     row[["pse"]] <- pse[i]
      #    
      #     table$addRow(rowKey = i, values = row)
      #     
      #   }
      #   
      
        
      # },
      
      
      
  
  #### Prepare Plot functions ----
      
      .prepareWrightmapPlot = function(data) {
       
        tamobj = TAM::tam.mml(resp = as.matrix(data))
        
        # item difficulty ---------------
        
        imeasure <- tamobj$xsi$xsi
        
        # person statistics--------
        
        person<- TAM:: tam.wle(tamobj)
        
        pmeasure<- person$theta
        
        # plot---------
        
        image <- self$results$plot
        
        nvars <- length(self$options$vars)
        
        # width <- 400 + nvars * 30
        # 
        # image$setSize(width, 400)
        
        state <- list(pmeasure, imeasure)
        
        image$setState(state)
        
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
      
      .plot = function(image,...){
        wrightmap <- self$options$wrightmap
        
        if (!wrightmap)
          return()
       
        pmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        
        # plot <- WrightMap::wrightMap(pmeasure,imeasure,
        #                              show.thr.lab= FALSE,
        #                              thr.sym.cex = 2.0,
        #                              thr.sym.pch = 17,
        #                              axis.persons = "Person distribution",
        #                              thr.sym.col.fg = RColorBrewer::brewer.pal(10, "Paired"))
        # 
       
        plot<- ShinyItemAnalysis::ggWrightMap(pmeasure, imeasure,
                                              color = "deepskyblue")
                                              
                                              
        
       # plot <- plot+ggtheme
        
        
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

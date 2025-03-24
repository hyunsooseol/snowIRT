#' @import ggplot2

polytomousClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "polytomousClass",
    inherit = polytomousBase,
    private = list(
      .allCache = NULL,      
      .htmlwidget = NULL, 
      #======================================================
      
      .init = function() {
        
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        # self$results$instructions$setContent(
        #   "<html>
        #     <head>
        #     </head>
        #     <body>
        #     <div class='instructions'>
        #     <p>____________________________________________________________________________________</p>
        #     <p>1. Note that Polytomous model needs <b>the bottom category to be coded as 0.</b>
        #     <p>2. <b>Person Analysis</b> will be displayed in the datasheet.</p>
        #     <p>3. The result tables are estimated by Marginal Maximum likelihood Estimation(MMLE).</p>
        #     <p>4. The <b>eRm</b> R package was used for the person-item map for PCM.</p>
        #     <p>5. The rationale of snowIRT module is described in the <a href='https://bookdown.org/dkatz/Rasch_Biome/' target = '_blank'>documentation</a>.</p>
        #     <p>6. Feature requests and bug reports can be made on my <a href='https://github.com/hyunsooseol/snowIRT/issues'  target = '_blank'>GitHub</a>.</p>
        #     <p>____________________________________________________________________________________</p>
        #     </div>
        #     </body>
        #     </html>"
        # )
        
        self$results$instructions$setContent(
          private$.htmlwidget$generate_accordion(
            title="Instructions",
            content = paste(
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<ul>',
              '<li>Note that Polytomous model needs the bottom category to be coded as <b>0</b>.</li>',
              '<li><b>Person Analysis</b> will be displayed in the datasheet.</li>',
              '<li>The result tables are estimated by Marginal Maximum likelihood Estimation(MMLE).</li>',
              '<li>The <b>eRm</b> R package was used for the person-item map for PCM.</li>',
              '<li>The rationale of snowIRT module is described in the <a href="https://bookdown.org/dkatz/Rasch_Biome/" target = "_blank">documentation</a>.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
              
            )
            
          )
        )          
        
        #  private$.initItemsTable()
        
        if (self$options$modelfitp)
          self$results$mf$scale$setNote(
            "Note",
            "MADaQ3= Mean of absolute values of centered Q_3 statistic with p value obtained by Holm
adjustment; Ho= the data fit the Rasch model."
          )
        
        if (self$options$infit)
          self$results$ia$items$setNote(
            "Note",
            "Infit= Information-weighted mean square statistic; Outfit= Outlier-sensitive means square statistic."
          )
        
        if (self$options$thurs)
          self$results$ia$thurs$setNote(
            "Note",
            "The Thurstonian threshold for a score category is defined as the ability at which the probability of achieving that score or higher reaches 0.50."
          )
        if(isTRUE(self$options$wplot)){
          width <- self$options$width
          height <- self$options$height
          self$results$wplot$setSize(width, height)
        }
        
        if(isTRUE(self$options$piplot)){
          width <- self$options$width5
          height <- self$options$height5
          self$results$piplot$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot4)){
          width <- self$options$width4
          height <- self$options$height4
          self$results$plot4$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot6)){
          width <- self$options$width6
          height <- self$options$height6
          self$results$plot6$setSize(width, height)
        }
        
        if(isTRUE(self$options$inplot)){
          width <- self$options$width7
          height <- self$options$height7
          self$results$inplot$setSize(width, height)
        }
        
        if(isTRUE(self$options$outplot)){
          width <- self$options$width7
          height <- self$options$height7
          self$results$outplot$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot3)){
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot3$setSize(width, height)
        }
        
        if(isTRUE(self$options$plot2)){
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot2$setSize(width, height)
        }
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      
      
      #======================================++++++++++++++++++++++
      
      .run = function() {
        
        
        # Ready--------
        
        ready <- TRUE
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 2)
          
          ready <- FALSE
        
        if (ready) {
          
          data <- private$.cleanData()
          #results <- private$.compute(data)
          
          if (is.null(private$.allCache)) {
            private$.allCache <- private$.compute(data)
          }
          results <- private$.allCache
          
          #populate scale table-----
          private$.populateScaleTable(results)
          
          # populate item table----
          private$.populateItemsTable(results)
          
          # Populate q3 matrix table-----
          private$.populateMatrixTable(results)
          
          # populate thurstonian thresholds
          private$.populateThurstoneTable(results)
          
          # delta-tau parameter--------
          private$.populateThresholdsTable(results)
          
          # model comparison----------
          private$.populateModelTable(results)
          private$.populateLrTable(results)
          
          #prepare plot-----
          #private$.prepareIccPlot(data)
          
          # prepare Expected score curve plot---------
          #private$.prepareEscPlot(data) 
          
          # prepare person-item map
          private$.preparepiPlot(data)
          
          # prepare item fit plot-------
          private$.prepareInfitPlot(data)
          private$.prepareOutfitPlot(data)
          
          # prepare rating scale category plot=========
          #  private$.prepareRatingPlot(data)
          # Summary of total score-----
          private$.populateToTable(results)  
          
          #Standard score---------
          private$.populateStTable(results)
          
        }
        
      },
      
      
      # compute results=====================================================
      
      .compute = function(data) {
        
        ##################################################################
        #set.seed(1234)
        
        # estimate the Rasch model with MML using function 'tam.mml'-----
        #tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        ###########################################################
        tamobj <- private$.computeTamobj()
        
        if(self$options$tau==TRUE){
          
          tau <- tamobj$item_irt
          
          # rsmod <- psychotools::rsmodel(as.matrix(data))
          # 
          # ## extract threshold parameters with sum zero restriction
          # thr <- psychotools::threshpar(rsmod)
          # 
          # # convering data frame-------
          # 
          # df <- purrr::map_df(thr, dplyr::bind_rows)
          # 
          # tau<- data.frame(df)
          # 
          # 
          self$results$text$setContent(tau)
          
        }
        
        
        # estimate item difficulty measure---------------
        
        imeasure<- tamobj$xsi[,1]
        
        #imeasure <- tamobj$item_irt[[3]]
        # estimate standard error of the item parameter-----
        #ise <- tamobj$se.AXsi[,2]
        
        ise<- tamobj$xsi[,2]
        
        # computing infit and outfit statistics---------------------
        
        infit <- TAM::tam.fit(tamobj)$itemfit$Infit
        outfit <- TAM::tam.fit(tamobj)$itemfit$Outfit
        
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
        
        # Partial credit model using MML estimation---
        mod_pcm <- TAM::tam(resp = as.matrix(data))
        
        #  Calculation of Thurstonian thresholds----
        thresh <- TAM::tam.threshold(mod_pcm)
        nc <- ncol(thresh)
        
        # tampartial = TAM::tam.mml(resp = as.matrix(data))
        # Delta parameter-------------------
        
        pmeasure <- mod_pcm$item_irt$beta
        
        # delta-tau parameterization--------
        delta <- mod_pcm$item_irt
        tau<- delta[,c(-1,-2,-3)]
        nc1 <- ncol(tau)
        
        ########## model comparison-----------
        RSM <- tamobj
        PCM <- mod_pcm
        
        comp <- CDM::IRT.compareModels(PCM, RSM)
        
        name <- comp$IC$Model
        log <- comp$IC$loglike
        dev <- comp$IC$Deviance
        aic <- comp$IC$AIC
        bic <- comp$IC$BIC
        caic <- comp$IC$CAIC
        npars <- comp$IC$Npars
        obs <- comp$IC$Nobs
        
        #####################
        lr<- comp$LRtest
        
        model1 <- lr$Model1
        model2<- lr$Model2
        chi<- lr$Chi2
        df<- lr$df
        p<- lr$p
        
        # total score calculation
        score <- apply(data, 1,sum)
        
        # summary of total score
        to <- psych::describe(score)
        to$kurtosis <- to$kurtosis + 3
        
        # Histogram of total score-------
        
        # colors by cut-score
        cut <- median(score) # cut-score
        color <- c(rep("red", cut - min(score)), "gray", rep("blue", max(score) - cut))
        df2 <- data.frame(score)
        
        state <- list(df2, score,color)
        
        
        image2 <- self$results$plot2
        
        image2$setState(state)
        
        
        # Standard score----------
        
        tosc <- sort(unique(score))          # Levels of total score
        perc <- stats::ecdf(score)(tosc)     # Percentiles
        zsco <- sort(unique(scale(score)))   # Z-score
        tsco <- 50 + 10 * zsco               # T-score
        
        st<- cbind(tosc, perc, zsco, tsco)
        st<- as.data.frame(st)
        
        # self$results$text1$setContent(st)
        
        # person infit---------
        pfit <- TAM::tam.personfit(tamobj)
        pinfit <- pfit$infitPerson
        
        # person outfit---------
        pfit <- TAM::tam.personfit(tamobj)
        poutfit <- pfit$outfitPerson
        
        # Residual----------
        
        res <- TAM::IRT.residuals(tamobj )
        resid <- res$stand_residuals
        
        #### Person Statistics###########################
        
        # Person tables------------
        
        if(self$options$total==TRUE){
          
          self$results$total$setRowNums(rownames(data))     
          self$results$total$setValues(total)
          
        }
        
        if(self$options$personmeasure==TRUE){
          
          self$results$personmeasure$setRowNums(rownames(data))
          self$results$personmeasure$setValues(personmeasure)
          
        }
        
        if(self$options$pse==TRUE){
          
          self$results$pse$setRowNums(rownames(data))
          self$results$pse$setValues(pse)
          
        }
        
        if(self$options$pinfit==TRUE){
          
          self$results$pinfit$setRowNums(rownames(data))
          self$results$pinfit$setValues(pinfit)
          
        }
        
        if(self$options$poutfit==TRUE){
          
          self$results$poutfit$setRowNums(rownames(data))
          self$results$poutfit$setValues(poutfit)
          
        }
        
        if(self$options$resid==TRUE){
          
          keys <- 1:length(self$options$vars)
          titles <- paste("Item", 1:length(self$options$vars))
          descriptions <- paste("Item", 1:length(self$options$vars))
          measureTypes <- rep("continuous", length(self$options$vars))
          
          self$results$resid$set(
            keys=keys,
            titles=titles,
            descriptions=descriptions,
            measureTypes=measureTypes
          )
          
          self$results$resid$setRowNums(rownames(data))
          
          
          resid <- as.data.frame(resid)
          
          for (i in 1:length(self$options$vars)) {
            scores <- as.numeric(resid[, i])
            self$results$resid$setValues(index=i, scores)
          }
          
        }
        
        # Wrightmap plot--------------
        if(self$options$wplot==TRUE){
          
          vars <- self$options$vars
          image <- self$results$wplot
          
          imeasure <- tamobj$item_irt[[3]]
          
          state <- list(personmeasure, imeasure, vars)
          image$setState(state)
        }
        
        # Person fit plot3----------------------
        
        Measure <- personmeasure
        Infit <- pinfit
        Outfit <- poutfit
        
        daf <- data.frame(Measure,Infit,Outfit)
        
        pf<- reshape2::melt(daf,
                            id.vars='Measure',
                            variable.name="Fit",
                            value.name='Value')
        
        image <- self$results$plot3
        
        image$setState(pf)
        
        # ICC Plot -------
        
        # image4 <- self$results$plot4
        # image4$setState(tamobj)
        
        # 'Item category for PCM' Plot -------
        
        # image6 <- self$results$plot6
        # image6$setState(tamobj)
        
        
        
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
            'pmeasure' = pmeasure,
            'tau'=tau,
            'nc1'=nc1,
            'name'=name,
            'log'=log,
            'dev'=dev,
            'aic'=aic,
            'bic'=bic,
            'caic'=caic,
            'npars'=npars,
            'obs'=obs,
            'model1'=model1,
            'model2'=model2,
            'chi'=chi,
            'df'=df,
            'p'=p,
            'to'=to,
            'st'=st,
            'total'=total,
            'personmeasure'=personmeasure,
            'pse'=pse,
            'pinfit'=pinfit,
            'poutfit'=poutfit,
            'resid'=resid
          )
        
        
      },
      
      
      # Standard score----------
      
      .populateStTable = function(results) {
        
        table <- self$results$ss$st
        
        st <- results$st
        
        names<- dimnames(st)[[1]]
        
        for (name in names) {
          
          row <- list()
          
          row[['Total']] <- st[name,1]
          row[['Percentile']] <- st[name,2]
          row[['Z']] <- st[name,3]
          row[['T']] <- st[name,4]
          
          
          table$addRow(rowKey=name, values=row)
          
        }
        
        
      },
      
      
      # Summary of total score---------
      
      .populateToTable = function(results) {
        
        table <- self$results$ss$to 
        
        to <- results$to
        
        n<- to$n
        min<- to$min
        max<- to$max
        mean<- to$mean
        median<- to$median
        sd<- to$sd
        se <- to$se
        skew<- to$skew
        kurtosis<- to$kurtosis
        
        
        row <- list()
        
        row[['N']] <- n
        row[['Minimum']] <- min
        row[['Maximum']] <- max
        row[['Mean']] <- mean
        row[['Median']] <- median
        row[['SD']] <- sd
        row[['SE']] <- se
        row[['Skewness']] <- skew
        row[['Kurtosis']] <- kurtosis
        
        
        table$setRow(rowNo = 1, values = row)
        
        
      },
      
      
      # Init. tables ------------------------------------
      
      .initItemsTable = function() {
        
        table <- self$results$ia$items
        
        for (i in seq_along(items))
          table$addFootnote(rowKey = items[i], 'name')
        
      },
      
      .populateModelTable = function(results) {
        
        table <- self$results$mcc$model
        
        name <- results$name
        log <- results$log
        dev <- results$dev
        aic <- results$aic
        bic <- results$bic
        caic <- results$caic
        npars <- results$npars
        obs <- results$obs
        
        
        for(i in seq_along(1:2)){
          
          row <- list()
          
          row[['name']] <- name[i]
          row[['log']] <- log[i]
          row[['dev']] <- dev[i]
          row[['aic']] <- aic[i]
          row[['bic']] <- bic[i]
          row[['caic']] <- caic[i]
          row[['npars']] <- npars[i]
          row[['obs']] <- obs[i]
          
          table$addRow(rowKey = i, values = row)
          
        }
        
        
      },
      
      .populateLrTable = function(results) {
        
        table <- self$results$mcc$lr
        
        model1 <- results$model1
        model2 <- results$model2
        chi <- results$chi
        df <- results$df
        p <- results$p
        
        
        row <- list()
        
        row[['model1']] <- model1
        row[['model2']] <- model2
        row[['chi']] <- chi
        row[['df']] <- df
        row[['p']] <- p
        
        table$setRow(rowNo = 1, values = row)
        
        
        
      },
      
      
      # populate scale table-------------------
      
      .populateScaleTable = function(results) {
        table <- self$results$mf$scale
        
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
        
        table <- self$results$ia$items
        
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
        
        matrix <- self$results$mf$get('mat')
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
      
      
      
      #  populate Delta-tau parameterization------------
      
      .populateThresholdsTable = function(results) {
        
        table <- self$results$ia$thresh
        
        #  thr <- results$thresh # matrix
        
        tau <- results$tau
        
        
        pmeasure <- results$pmeasure #partial credit
        
        nCategory <- results$nc1 # number of tau
        
        
        vars <- self$options$vars
        
        
        if (nCategory > 1) {
          for (i in 1:nCategory)
            
            table$addColumn(
              name = paste0("name", i),
              title = as.character(i),
              superTitle = 'tau parameters',
              type = 'number'
            )
        }
        
        
        
        for (i in seq_along(vars)) {
          row <- list()
          
          
          for (j in 1:nCategory) {
            row[[paste0("name", j)]] <- tau[i, j]
            
            
          }
          
          row[["pmeasure"]] <- pmeasure[i]
          table$setRow(rowNo = i, values = row)
        }
      },
      
      # populate thurstone thresholds---------
      
      
      .populateThurstoneTable = function(results) {
        
        table <- self$results$ia$thurs
        
        
        thr <- results$thresh # matrix
        
        nCategory <- results$nc # number of thresholds
        
        vars <- self$options$vars
        
        
        if (nCategory > 1) {
          for (i in 1:nCategory)
            
            table$addColumn(
              name = paste0("name", i),
              title = as.character(i),
              superTitle = 'Thurstone Thresholds',
              type = 'number'
            )
        }
        
        
        
        for (i in seq_along(vars)) {
          row <- list()
          
          
          for (j in 1:nCategory) {
            row[[paste0("name", j)]] <- thr[i, j]
            
            
          }
          
          table$setRow(rowNo = i, values = row)
        }
      },
      
      
      #####################################################
      .populatePerOutputs = function(results) {
        
        perc <- results$perc  
        
        if (self$options$per
            && self$results$per$isNotFilled()) {
          
          
          self$results$per$setValues(perc)
          
          self$results$per$setRowNums(rownames(data))
          
        }
      },  
      
      #### Plot functions ###########################
      
      # wright map plot--------------
      
      .wplot = function(image,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        personmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        vars <- image$state[[3]]
        
        
        wplot<- ShinyItemAnalysis::ggWrightMap(personmeasure, imeasure,
                                               item.names = vars,
                                               # rel_widths = c(1, 1), 
                                               color = "deepskyblue")
        
        print(wplot)
        TRUE
        
      },
      
      # PREPARE PERSON-ITEM PLOT FOR PCM-------------
      
      .preparepiPlot = function(data) {
        
        set.seed(1234)
        #########################
        autopcm <- eRm::PCM(data)
        #########################
        image <- self$results$piplot
        image$setState(autopcm)
        
      },
      
      .piPlot= function(image, ...) {
        
        autopcm <- image$state
        
        if (is.null(autopcm))
          return()
        
        
        plot <- eRm::plotPImap(autopcm, sorted=TRUE,
                               warn.ord.colour = "red")
        
        print(plot)
        
        TRUE
        
      },
      
      # ICC plot-----------------
      
      # .prepareIccPlot = function(data) {
      #   
      #   # item characteristic curves based on partial credit model--------
      #   set.seed(1234)
      #   tam <- TAM::tam.mml(resp = as.matrix(data))
      # 
      # # ICC Plot -------
      #   
      #   image <- self$results$plot4
      #   image$setState(tam)
      #   
      # },
      
      .plot4 = function(image, ...) {
        
        # ICC plot-------------------
        
        num <- self$options$num
        
        if(!self$options$plot4)
          return(FALSE)
        
        tamobj <- private$.computeTamobj()  
        
        plot4 <- plot(tamobj,
                      items = num,
                      #type="items" produce item response curve not expected curve
                      type = "expected",
                      export = FALSE)
        
        print(plot4)
        TRUE
        
      },
      
      
      # Prepare Expected score curve functions------------
      
      # .prepareEscPlot = function(data) {
      #   
      #   set.seed(1234)
      #   tamp = TAM::tam(resp =as.matrix(data))
      # 
      #   # Prepare Data For ESC Plot -------
      #   
      #   image <- self$results$plot6
      #   image$setState(tamp)
      # 
      # },
      # 
      
      .plot6 = function(image, ...) {
        
        # 'Item category for PCM'  
        num1 <- self$options$num1
        
        if(!self$options$plot6)
          return(FALSE)
        
        tamobj <- private$.computeTamobj()  
        
        plot6 <- plot(tamobj,
                      items = num1,
                      type = 'items',
                      export = FALSE)
        print(plot6)
        TRUE
        
      },
      
      
      # infit plot---------------
      
      .prepareInfitPlot=function(data){
        
        # estimate the Rasch model with MML using function 'tam.mml'-----
        set.seed(1234)
        tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        
        
        item <- tamobj$item$item
        nitems <- length(item)
        
        
        fit <- TAM::tam.fit(tamobj)
        
        # computing infit statistics---------------------
        
        Infit <- fit$itemfit$Infit
        
        
        infit <- NA
        
        for(i in 1:nitems){
          
          infit[i] <- fit$itemfit$Infit[i]
          
        }
        
        infit1<- data.frame(item,infit)
        
        
        #    self$results$text$setContent(infit1)
        
        
        image <- self$results$inplot
        image$setState(infit1)
        
        
      }, 
      
      
      .inPlot = function(image, ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        infit1 <- image$state
        
        
        plot <- ggplot(infit1, aes(x = item, y=infit)) + 
          geom_point(shape = 4, color = 'black', 
                     fill = 'white', size = 3, stroke = 2) +
          geom_hline(yintercept = 1.5,linetype = "dotted", color='red', size=1.5) +
          geom_hline(yintercept = 0.5,linetype = "dotted", color='red', size=1.5) +
          ggtitle("Item Infit")
        
        plot <- plot+ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle, hjust = 1
            )
          )
        }
        
        
        print(plot)
        TRUE
        
      },
      
      .prepareOutfitPlot=function(data){
        
        # estimate the Rasch model with MML using function 'tam.mml'-----
        set.seed(1234)
        tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        
        
        item <- tamobj$item$item
        nitems <- length(item)
        
        
        fit <- TAM::tam.fit(tamobj)
        
        # computing outfit statistics---------------------
        
        Infit <- fit$itemfit$Outfit
        
        
        outfit <- NA
        
        for(i in 1:nitems){
          
          outfit[i] <- fit$itemfit$Outfit[i]
          
        }
        
        outfit1<- data.frame(item,outfit)
        
        
        image <- self$results$outplot
        image$setState(outfit1)
        
        
      }, 
      
      .outPlot = function(image, ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        outfit1 <- image$state
        
        
        plot <- ggplot(outfit1, aes(x = item, y=outfit)) + 
          geom_point(shape = 4, color = 'black', 
                     fill = 'white', size = 3, stroke = 2) +
          geom_hline(yintercept = 1.5,linetype = "dotted", color='red', size=1.5) +
          geom_hline(yintercept = 0.5,linetype = "dotted", color='red', size=1.5) +
          ggtitle("Item Outfit")
        
        plot <- plot+ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = self$options$angle, hjust = 1
            )
          )
        }
        
        
        
        print(plot)
        TRUE
        
      },
      
      #Histogram of total score------
      
      
      .plot2 = function(image2, ggtheme, theme,...) {
        
        if (is.null(image2$state))
          return(FALSE)
        
        df2 <- image2$state[[1]]
        score <- image2$state[[2]]
        color <- image2$state[[3]]
        
        plot2 <- ggplot(df2, aes(score)) +
          geom_histogram(binwidth = 1, fill = color, col = "black") +
          xlab("Total score") +
          ylab("Number of respondents") +
          ShinyItemAnalysis::theme_app()
        
        plot2 <- plot2+ggtheme
        
        print(plot2)
        TRUE
      },
      
      
      .plot3 = function(image,ggtheme, theme,...) {
        
        if (is.null(image$state))
          return(FALSE)
        
        pf <- image$state
        
        plot3<- ggplot2::ggplot(pf, aes(x = Measure, y = Value, shape = Fit))+
          geom_point(size=3, stroke=2)+
          
          ggplot2::scale_shape_manual(values=c(3, 4))+
          #ggplot2::scale_color_manual(values=c("red", "blue")+
          ggplot2::coord_cartesian(xlim=c(-4, 4),ylim=c(0, 3))+
          ggplot2::geom_hline(yintercept = 1.5,linetype = "dotted", color='red', size=1.5)+ 
          ggplot2::geom_hline(yintercept = 0.5,linetype = "dotted", color='red', size=1.5)    
        
        
        plot3 <- plot3+ggtheme
        
        print(plot3)
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
      },
      
      .computeTamobj=function(){
        
        data <- private$.cleanData()
        
        set.seed(1234)
        # estimate the Rasch model with MML using function 'tam.mml'-----
        tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        
        return(tamobj)
        
      }
      
    )
  )
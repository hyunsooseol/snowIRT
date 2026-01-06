#' @import ggplot2

polytomousClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "polytomousClass",
    inherit = polytomousBase,
    private = list(
      .cache = list(),
      #.allCache = NULL,
      .htmlwidget = NULL,
      #======================================================
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
          
        }
        
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Note that Polytomous model needs the bottom category to be coded as <b>0</b>.</li>',
            '<li>Use variable names with exactly one trailing number (e.g., i1, item2, age1); names without digits or with multiple numbers may not plot.</li>',
            '<li>The <b>eRm</b> R package was used for the person-item map for PCM.</li>',
            '<li>The rationale of snowIRT module is described in the <a href="https://bookdown.org/dkatz/Rasch_Biome/" target = "_blank">documentation</a>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
          
        ))
        
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
      },
      
      .run = function() {
        # Ready--------
        ready <- TRUE
        
        if (is.null(self$options$vars) ||
            length(self$options$vars) < 3)
          
          ready <- FALSE
        
        if (ready) {
          data <- private$.cleanData()
          
          if (is.null(private$.cache$tamobj))
            private$.cache$tamobj <- private$.computeTamobj()
          if (is.null(private$.cache$results))
            private$.cache$results <- private$.compute(data)
          
          tamobj <- private$.cache$tamobj
          results <- private$.cache$results
          
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
        #tamobj <- private$.computeTamobj()
        tamobj <- private$.cache$tamobj
        
        if (isTRUE(self$options$tau)) {
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
        imeasure <- tamobj$xsi[, 1]
        
        #imeasure <- tamobj$item_irt[[3]]
        # estimate standard error of the item parameter-----
        #ise <- tamobj$se.AXsi[,2]
        ise <- tamobj$xsi[, 2]
        
        # computing infit and outfit statistics---------------------
        infit <- TAM::tam.fit(tamobj)$itemfit$Infit
        outfit <- TAM::tam.fit(tamobj)$itemfit$Outfit
        
        # computing person separation reliability-------
        person <- TAM::tam.wle(tamobj)
        reliability <- person$WLE.rel
        
        # person statistics------------------
        total <- person$PersonScores
        personmeasure <- person$theta
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
        tau <- delta[, c(-1, -2, -3)]
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
        lr <- comp$LRtest
        
        model1 <- lr$Model1
        model2 <- lr$Model2
        chi <- lr$Chi2
        df <- lr$df
        p <- lr$p
        
        # total score calculation
        score <- apply(data, 1, sum)
        
        # summary of total score
        to <- psych::describe(score)
        to$kurtosis <- to$kurtosis + 3
        
        # Histogram of total score-------
        
        # colors by cut-score
        cut <- median(score) # cut-score
        color <- c(rep("red", cut - min(score)), "gray", rep("blue", max(score) - cut))
        df2 <- data.frame(score)
        
        state <- list(df2, score, color)
        image2 <- self$results$plot2
        image2$setState(state)
        
        # Standard score----------
        
        tosc <- sort(unique(score))          # Levels of total score
        perc <- stats::ecdf(score)(tosc)     # Percentiles
        zsco <- sort(unique(scale(score)))   # Z-score
        tsco <- 50 + 10 * zsco               # T-score
        
        st <- cbind(tosc, perc, zsco, tsco)
        st <- as.data.frame(st)
        
        # self$results$text1$setContent(st)
        
        # person infit---------
        pfit <- TAM::tam.personfit(tamobj)
        pinfit <- pfit$infitPerson
        
        # person outfit---------
        pfit <- TAM::tam.personfit(tamobj)
        poutfit <- pfit$outfitPerson
        
        # Residual----------
        
        res <- TAM::IRT.residuals(tamobj)
        resid <- res$stand_residuals
        
        #### Person Statistics###########################
        
        # Person tables------------
        
        if (isTRUE(self$options$total)) {
          self$results$total$setRowNums(rownames(data))
          self$results$total$setValues(total)
          
        }
        
        if (isTRUE(self$options$personmeasure)) {
          self$results$personmeasure$setRowNums(rownames(data))
          self$results$personmeasure$setValues(personmeasure)
          
        }
        
        if (isTRUE(self$options$pse)) {
          self$results$pse$setRowNums(rownames(data))
          self$results$pse$setValues(pse)
          
        }
        
        if (isTRUE(self$options$pinfit)) {
          self$results$pinfit$setRowNums(rownames(data))
          self$results$pinfit$setValues(pinfit)
          
        }
        
        if (isTRUE(self$options$poutfit)) {
          self$results$poutfit$setRowNums(rownames(data))
          self$results$poutfit$setValues(poutfit)
          
        }
        
        if (isTRUE(self$options$resid)) {
          keys <- 1:length(self$options$vars)
          titles <- paste("Item", 1:length(self$options$vars))
          descriptions <- paste("Item", 1:length(self$options$vars))
          measureTypes <- rep("continuous", length(self$options$vars))
          
          self$results$resid$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          self$results$resid$setRowNums(rownames(data))
          resid <- as.data.frame(resid)
          for (i in 1:length(self$options$vars)) {
            scores <- as.numeric(resid[, i])
            self$results$resid$setValues(index = i, scores)
          }
        }
        
        # Wrightmap plot--------------
        if (isTRUE(self$options$wplot)) {
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
        daf <- data.frame(Measure, Infit, Outfit)
        pf <- reshape2::melt(
          daf,
          id.vars = 'Measure',
          variable.name = "Fit",
          value.name = 'Value'
        )
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
            'tau' = tau,
            'nc1' = nc1,
            'name' = name,
            'log' = log,
            'dev' = dev,
            'aic' = aic,
            'bic' = bic,
            'caic' = caic,
            'npars' = npars,
            'obs' = obs,
            'model1' = model1,
            'model2' = model2,
            'chi' = chi,
            'df' = df,
            'p' = p,
            'to' = to,
            'st' = st,
            'total' = total,
            'personmeasure' = personmeasure,
            'pse' = pse,
            'pinfit' = pinfit,
            'poutfit' = poutfit,
            'resid' = resid
          )
      },
      
      # Standard score----------
      
      .populateStTable = function(results) {
        table <- self$results$ss$st
        st <- results$st
        
        col_names <- c("Total", "Percentile", "Z", "T")
        row_names <- rownames(st)
        for (name in row_names) {
          row <- as.list(setNames(st[name, 1:4], col_names))
          table$addRow(rowKey = name, values = row)
        }
      },
      # Summary of total score---------
      .populateToTable = function(results) {
        table <- self$results$ss$to
        to <- results$to
        stat_names <- c(
          N = "n",
          Minimum = "min",
          Maximum = "max",
          Mean = "mean",
          Median = "median",
          SD = "sd",
          SE = "se",
          Skewness = "skew",
          Kurtosis = "kurtosis"
        )
        row <- setNames(lapply(stat_names, function(x)
          to[[x]]), names(stat_names))
        table$setRow(rowNo = 1, values = row)
        # n<- to$n
        # min<- to$min
        # max<- to$max
        # mean<- to$mean
        # median<- to$median
        # sd<- to$sd
        # se <- to$se
        # skew<- to$skew
        # kurtosis<- to$kurtosis
        #
        #
        # row <- list()
        #
        # row[['N']] <- n
        # row[['Minimum']] <- min
        # row[['Maximum']] <- max
        # row[['Mean']] <- mean
        # row[['Median']] <- median
        # row[['SD']] <- sd
        # row[['SE']] <- se
        # row[['Skewness']] <- skew
        # row[['Kurtosis']] <- kurtosis
        #
        #
        # table$setRow(rowNo = 1, values = row)
      },
      
      
      # Init. tables ------------------------------------
      
      .initItemsTable = function() {
        table <- self$results$ia$items
        
        for (i in seq_along(items))
          table$addFootnote(rowKey = items[i], 'name')
        
      },
      
      .populateModelTable = function(results) {
        table <- self$results$mcc$model
        stat_names <- c("name", "log", "dev", "aic", "bic", "caic", "npars", "obs")
        for (i in seq_along(results$name)) {
          row <- setNames(lapply(stat_names, function(x)
            results[[x]][i]), stat_names)
          table$addRow(rowKey = i, values = row)
        }
        # name <- results$name
        # log <- results$log
        # dev <- results$dev
        # aic <- results$aic
        # bic <- results$bic
        # caic <- results$caic
        # npars <- results$npars
        # obs <- results$obs
        #
        #
        # for(i in seq_along(1:2)){
        #
        #   row <- list()
        #
        #   row[['name']] <- name[i]
        #   row[['log']] <- log[i]
        #   row[['dev']] <- dev[i]
        #   row[['aic']] <- aic[i]
        #   row[['bic']] <- bic[i]
        #   row[['caic']] <- caic[i]
        #   row[['npars']] <- npars[i]
        #   row[['obs']] <- obs[i]
        #
        #   table$addRow(rowKey = i, values = row)
        
      },
      
      .populateLrTable = function(results) {
        table <- self$results$mcc$lr
        stat_names <- c("model1", "model2", "chi", "df", "p")
        row <- setNames(lapply(stat_names, function(x)
          results[[x]]), stat_names)
        table$setRow(rowNo = 1, values = row)
        # table <- self$results$mcc$lr
        #
        # model1 <- results$model1
        # model2 <- results$model2
        # chi <- results$chi
        # df <- results$df
        # p <- results$p
        #
        #
        # row <- list()
        #
        # row[['model1']] <- model1
        # row[['model2']] <- model2
        # row[['chi']] <- chi
        # row[['df']] <- df
        # row[['p']] <- p
        #
        # table$setRow(rowNo = 1, values = row)
      },
      
      
      # populate scale table-------------------
      
      .populateScaleTable = function(results) {
        table <- self$results$mf$scale
        stat_names <- c("reliability", "modelfit", "modelfitp")
        row <- setNames(lapply(stat_names, function(x)
          if (x == "reliability")
            results[[x]][1]
          else
            results[[x]]),
          stat_names)
        table$setRow(rowNo = 1, values = row)
        # table <- self$results$mf$scale
        #
        # reliability <- results$reliability
        #
        # modelfit <- results$modelfit
        # modelfitp <- results$modelfitp
        #
        # row <- list()
        #
        # row[['reliability']] <- reliability[1]
        # row[['modelfit']] <- modelfit
        # row[['modelfitp']] <- modelfitp
        #
        # table$setRow(rowNo = 1, values = row)
      },
      
      
      # populate item tables----------------------
      
      .populateItemsTable = function(results) {
        table <- self$results$ia$items
        items <- self$options$vars
        stat_names <- c(
          "measure" = "imeasure",
          "ise" = "ise",
          "infit" = "infit",
          "outfit" = "outfit"
        )
        
        for (i in seq_along(items)) {
          row <- setNames(lapply(stat_names, function(x)
            results[[x]][i]),
            names(stat_names))
          table$setRow(rowKey = items[i], values = row)
        }
        
        # table <- self$results$ia$items
        #
        # items <- self$options$vars
        #
        #
        # imeasure <- results$imeasure
        # ise <- results$ise
        #
        # infit <- results$infit
        # outfit <- results$outfit
        #
        #
        # for (i in seq_along(items)) {
        #   row <- list()
        #
        #
        #   row[["measure"]] <- imeasure[i]
        #
        #   row[["ise"]] <- ise[i]
        #
        #   row[["infit"]] <- infit[i]
        #
        #   row[["outfit"]] <- outfit[i]
        #
        #
        #   table$setRow(rowKey = items[i], values = row)
        # }
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
          #data<- private$.cleanData
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
      
      .wplot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        personmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        vars <- image$state[[3]]
        wplot <- ShinyItemAnalysis::ggWrightMap(personmeasure,
                                                imeasure,
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
      
      .piPlot = function(image, ...) {
        autopcm <- image$state
        if (is.null(autopcm))
          return()
        plot <- eRm::plotPImap(autopcm, sorted = TRUE, warn.ord.colour = "red")
        print(plot)
        TRUE
      },
      
      .plot4 = function(image, ...) {
        if (!self$options$plot4)
          return(FALSE)
        
        tamobj <- private$.cache$tamobj
        
        if (is.null(tamobj)) {
          return(FALSE)
        }
        
        all_items <- self$options$vars
        n_items <- length(all_items)
        
        current_item <- as.numeric(gsub("\\D", "", image$key))
        
        if (is.na(current_item) || current_item > n_items) {
          return(FALSE)
        }
        
        tryCatch({
          plot_result <- plot(tamobj, 
                              items = current_item, 
                              type = "expected", 
                              export = FALSE,
                              package = "graphics")  # using graphics 
          
          return(TRUE)
        }, error = function(e) {
          cat(paste("Error plotting item", current_item, ":", e$message, "\n"))
          return(FALSE)
        })
      },
      
      .plot6 = function(image, ...) {
        if (!self$options$plot6)
          return(FALSE)
        
        tamobj <- private$.cache$tamobj
        
        if (is.null(tamobj)) {
          return(FALSE)
        }
        
        all_items <- self$options$vars
        n_items <- length(all_items)
        
        current_item <- as.numeric(gsub("\\D", "", image$key))
        
        if (is.na(current_item) || current_item > n_items) {
          return(FALSE)
        }
        
        tryCatch({
          plot_result <- plot(tamobj, 
                              items = current_item, 
                              type = "items", 
                              export = FALSE,
                              package = "graphics",
                              observed=self$options$obs)  
          
          return(TRUE)
        }, error = function(e) {
          cat(paste("Error plotting item", current_item, ":", e$message, "\n"))
          return(FALSE)
        })
      },
      
  
      # infit plot---------------
      
      .prepareInfitPlot = function(data) {
        # estimate the Rasch model with MML using function 'tam.mml'-----
        # set.seed(1234)
        # tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        tamobj <- private$.cache$tamobj
        
        item <- tamobj$item$item
        nitems <- length(item)
        fit <- TAM::tam.fit(tamobj)
        # computing infit statistics---------------------
        Infit <- fit$itemfit$Infit
        infit <- NA
        for (i in 1:nitems) {
          infit[i] <- fit$itemfit$Infit[i]
        }
        infit1 <- data.frame(item, infit)
        image <- self$results$inplot
        image$setState(infit1)
      },
      
      .inPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        infit1 <- image$state
        
        plot <- ggplot(infit1, aes(x = item, y = infit)) +
          # Refined circular point style
          geom_point(
            shape = 21,
            color = '#2c3e50',
            fill = '#3498db',
            size = 3.2,
            stroke = 1.1,
            alpha = 0.85
          ) +
          # Clean boundary lines
          geom_hline(
            yintercept = 1.5,
            linetype = "solid",
            color = '#e74c3c',
            linewidth = 1,
            alpha = 0.7
          ) +
          geom_hline(
            yintercept = 0.5,
            linetype = "solid",
            color = '#e74c3c',
            linewidth = 1,
            alpha = 0.7
          ) +
          ggtitle("") +
          theme_minimal() +
          theme(
            # Background settings
            panel.background = element_rect(fill = "#fafafa", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Grid line settings
            panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.4),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            
            # Title and axis label styling
            plot.title = element_text(
              hjust = 0.5,
              size = 15,
              face = "bold",
              color = "#2c3e50",
              margin = margin(b = 15)
            ),
            axis.title = element_text(size = 11, color = "#34495e"),
            axis.text = element_text(size = 9.5, color = "#7f8c8d"),
            axis.text.x = element_text(margin = margin(t = 6)),
            axis.text.y = element_text(margin = margin(r = 6)),
            
            # Border settings
            panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
            
            # Margin adjustments
            plot.margin = margin(15, 15, 15, 15)
          )
        
        plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot)
        TRUE
      },
      
      .prepareOutfitPlot = function(data) {
        # estimate the Rasch model with MML using function 'tam.mml'-----
        # set.seed(1234)
        # tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        tamobj <- private$.cache$tamobj
        item <- tamobj$item$item
        nitems <- length(item)
        fit <- TAM::tam.fit(tamobj)
        # computing outfit statistics---------------------
        Infit <- fit$itemfit$Outfit
        outfit <- NA
        for (i in 1:nitems) {
          outfit[i] <- fit$itemfit$Outfit[i]
        }
        outfit1 <- data.frame(item, outfit)
        image <- self$results$outplot
        image$setState(outfit1)
      },
      
      .outPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        outfit1 <- image$state
        
        plot <- ggplot(outfit1, aes(x = item, y = outfit)) +
          # Refined circular point style
          geom_point(
            shape = 21,
            color = '#2c3e50',
            fill = '#3498db',
            size = 3.2,
            stroke = 1.1,
            alpha = 0.85
          ) +
          # Clean boundary lines
          geom_hline(
            yintercept = 1.5,
            linetype = "solid",
            color = '#e74c3c',
            linewidth = 1,
            alpha = 0.7
          ) +
          geom_hline(
            yintercept = 0.5,
            linetype = "solid",
            color = '#e74c3c',
            linewidth = 1,
            alpha = 0.7
          ) +
          ggtitle("") +
          theme_minimal() +
          theme(
            # Background settings
            panel.background = element_rect(fill = "#fafafa", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Grid line settings
            panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.4),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            
            # Title and axis label styling
            plot.title = element_text(
              hjust = 0.5,
              size = 15,
              face = "bold",
              color = "#2c3e50",
              margin = margin(b = 15)
            ),
            axis.title = element_text(size = 11, color = "#34495e"),
            axis.text = element_text(size = 9.5, color = "#7f8c8d"),
            axis.text.x = element_text(margin = margin(t = 6)),
            axis.text.y = element_text(margin = margin(r = 6)),
            
            # Border settings
            panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
            
            # Margin adjustments
            plot.margin = margin(15, 15, 15, 15)
          )
        
        plot <- plot + ggtheme
        
        if (self$options$angle > 0) {
          plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot)
        TRUE
      },
      
      #Histogram of total score------
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        df2 <- image2$state[[1]]
        score <- image2$state[[2]]
        color <- image2$state[[3]]
        
        plot2 <- ggplot(df2, aes(score)) +
          geom_histogram(binwidth = 1,
                         fill = color,
                         col = "black") +
          xlab("Total score") +
          ylab("Number of respondents") +
          ShinyItemAnalysis::theme_app()
        
        plot2 <- plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      #person fit plot---
      
      .plot3 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        pf <- image$state
        
        plot3 <- ggplot2::ggplot(pf, aes(x = Measure, y = Value, shape = Fit, color = Fit)) +
          # Modern point styling with refined shapes
          geom_point(
            size = 2.5,
            stroke = 1,
            alpha = 0.75
          ) +
          # Elegant shape and color mapping
          ggplot2::scale_shape_manual(
            values = c("Infit" = 16, "Outfit" = 17),  # Circle and triangle
            name = "Fit"
          ) +
          ggplot2::scale_color_manual(
            values = c("Infit" = '#2980b9', "Outfit" = '#e74c3c'),
            name = "Fit"
          ) +
          # Clean boundary lines
          ggplot2::geom_hline(
            yintercept = 1.5,
            linetype = "solid",
            color = '#95a5a6',
            linewidth = 0.8,
            alpha = 0.8
          ) +
          ggplot2::geom_hline(
            yintercept = 0.5,
            linetype = "solid",
            color = '#95a5a6',
            linewidth = 0.8,
            alpha = 0.8
          ) +
          ggplot2::coord_cartesian(xlim = c(-4, 4), ylim = c(0, 3)) +
          ggtitle("Person Fit Plot") +
          labs(
            x = "Measure",
            y = "Value"
          ) +
          theme_minimal() +
          theme(
            # Background settings
            panel.background = element_rect(fill = "#fafafa", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            
            # Grid line settings
            panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            
            # Title and axis label styling
            plot.title = element_text(
              hjust = 0.5,
              size = 15,
              face = "bold",
              color = "#2c3e50",
              margin = margin(b = 15)
            ),
            axis.title = element_text(size = 11, color = "#34495e"),
            axis.text = element_text(size = 9.5, color = "#7f8c8d"),
            
            # Legend styling
            legend.position = "right",
            legend.title = element_text(size = 11, color = "#34495e", face = "bold"),
            legend.text = element_text(size = 10, color = "#7f8c8d"),
            legend.background = element_rect(fill = "white", color = "#ecf0f1", linewidth = 0.3),
            legend.key = element_rect(fill = "transparent"),
            legend.margin = margin(10, 10, 10, 10),
            
            # Border settings
            panel.border = element_rect(color = "#bdc3c7", fill = NA, linewidth = 0.4),
            
            # Margin adjustments
            plot.margin = margin(15, 15, 15, 15)
          )
        
        plot3 <- plot3 + ggtheme
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
      
      .computeTamobj = function() {
        data <- private$.cleanData()
        set.seed(1234)
        # estimate the Rasch model with MML using function 'tam.mml'-----
        tamobj = TAM::tam.mml(resp = as.matrix(data), irtmodel = "RSM")
        return(tamobj)
      }
    )
  )
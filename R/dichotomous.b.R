
# Dichotomous Rasch model
#' @import ggplot2

dichotomousClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "dichotomousClass",
    inherit = dichotomousBase,
    private = list(
      .cache = list(),
      .htmlwidget = NULL,
      
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
            '<li>Each variable must be <b>coded as 0 or 1 with the type of numeric-continuous</b> in jamovi.</li>',
            '<li><b>Person Analysis</b> will be displayed in the datasheet.</li>',
            '<li>The result tables are estimated by Marginal Maximum Likelihood estimation(MMLE).</li>',
            '<li>The rationale of snowIRT module is described in the <a href="https://bookdown.org/dkatz/Rasch_Biome/" target = "_blank">documentation</a>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        if (self$options$modelfitp)
          self$results$mf$scale$setNote(
            "Note",
            "MADaQ3= Mean of absolute values of centered Q_3 statistic with p value obtained by Holm
adjustment; Ho= the data fit the Rasch model."
          )
        
        if (self$options$infit)
          self$results$items$setNote(
            "Note",
            "Infit= Information-weighted mean square statistic; Outfit= Outlier-sensitive means square statistic."
          )
        if (isTRUE(self$options$wrightmap)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot4)) {
          width <- self$options$width4
          height <- self$options$height4
          self$results$plot4$setSize(width, height)
        }
        
        if (isTRUE(self$options$inplot)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$inplot$setSize(width, height)
        }
        
        if (isTRUE(self$options$outplot)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$outplot$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot3$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot2$setSize(width, height)
        }
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
          
          # populate scale table-----
          private$.populateScaleTable(results)
          # populate item table----
          private$.populateItemsTable(results)
          # Populate q3 matrix table-----
          private$.populateMatrixTable(results)
          # prepare Expected score curve plot---------
          #private$.prepareEscPlot(data)
          
          # Summary of total score-----
          private$.populateToTable(results)
          #Standard score---------
          private$.populateStTable(results)
        }
      },
      # compute results=====================================================
      
      .compute = function(data) {
        #################################################
        # set.seed(1234)
        #
        # # estimate the Rasch model with MMLE-----
        #
        # tamobj = TAM::tam.mml(resp = as.matrix(data))
        
        ##################################################
        
        #tamobj <- private$.computeTamobj()
        tamobj <- private$.cache$tamobj
        
        # computing item mean
        prop <- tamobj$item$M
        
        # estimate item difficulty measure---------------
        imeasure <- tamobj$xsi$xsi
        
        # estimate standard error of the item parameter-----
        ise <- tamobj$xsi$se.xsi
        # computing infit and outfit statistics---------------------
        fit <- TAM::tam.fit(tamobj)
        infit <- fit$itemfit$Infit
        outfit <- fit$itemfit$Outfit
        # computing person separation reliability-------
        person <- TAM::tam.wle(tamobj)
        reliability <- person$WLE.rel
        
        # person statistics
        total <- person$PersonScores
        pmeasure <- person$theta
        pse <- person$error
        
        #computing an effect size of model fit(MADaQ3) using MML-------
        # assess model fit----
        
        model <- TAM::tam.modelfit(tamobj)
        modelfit <- model$stat.MADaQ3$MADaQ3
        
        # pvalue--------
        modelfitp <- model$stat.MADaQ3$p
        
        # q3 matrix----------
        mat <- model$Q3.matr
        
        # total score calculation
        score <- apply(data, 1, sum)
        
        # summary of total score
        to <- psych::describe(score)
        to$kurtosis <- to$kurtosis + 3
        # Wrightmap plot--------------
        
        vars <- self$options$vars
        image <- self$results$plot
        
        #nvars <- length(self$options$vars)
        # width <- 400 + nvars * 30
        # image$setSize(width, 400)
        
        state <- list(pmeasure, imeasure, vars)
        image$setState(state)
        
        # esc plot------------------
        
        #tamp = TAM::tam(resp =as.matrix(data))
        
        # image <- self$results$plot4
        # image$setState(tamobj)
        # Infit plot------------------------
        Item <- fit$itemfit$parameter
        infit1 <- data.frame(Item, infit)
        
        image <- self$results$inplot
        image$setState(infit1)
        
        # Outfit plot------------------------
        Item <- fit$itemfit$parameter
        outfit1 <- data.frame(Item, outfit)
        
        image <- self$results$outplot
        image$setState(outfit1)
        
        
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
        
        #st <- cbind(tosc, perc, zsco, tsco)
        st <- as.data.frame(cbind(tosc, perc, zsco, tsco))
        
        #### Person Statistics###########################
        
        # Person tables------------
        
        if (self$options$total == TRUE) {
          self$results$total$setRowNums(rownames(data))
          self$results$total$setValues(total)
        }
        
        if (self$options$pmeasure == TRUE) {
          self$results$pmeasure$setRowNums(rownames(data))
          self$results$pmeasure$setValues(pmeasure)
        }
        
        if (self$options$pse == TRUE) {
          self$results$pse$setRowNums(rownames(data))
          self$results$pse$setValues(pse)
        }
        # person infit---------
        pfit <- TAM::tam.personfit(tamobj)
        pinfit <- pfit$infitPerson
        
        # person outfit---------
        pfit <- TAM::tam.personfit(tamobj)
        poutfit <- pfit$outfitPerson
        
        # Residual----------
        res <- TAM::IRT.residuals(tamobj)
        resid <- res$stand_residuals
        
        if (self$options$pinfit == TRUE) {
          self$results$pinfit$setRowNums(rownames(data))
          self$results$pinfit$setValues(pinfit)
        }
        
        if (self$options$poutfit == TRUE) {
          self$results$poutfit$setRowNums(rownames(data))
          self$results$poutfit$setValues(poutfit)
        }
        
        if (self$options$resid == TRUE) {
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
        
        # Person fit plot3----------------------
        Measure <- pmeasure
        Infit <- pinfit
        Outfit <- poutfit
        df <- data.frame(Measure, Infit, Outfit)
        pf <- reshape2::melt(
          df,
          id.vars = 'Measure',
          variable.name = "Fit",
          value.name = 'Value'
        )
        image <- self$results$plot3
        image$setState(pf)
        
        results <-
          list(
            'prop' = prop,
            'imeasure' = imeasure,
            'ise' = ise,
            'infit' = infit,
            'outfit' = outfit,
            'reliability' = reliability,
            'modelfit' = modelfit,
            'modelfitp' = modelfitp,
            'mat' = mat,
            'to' = to,
            'st' = st,
            'person' = person,
            'total' = total,
            'pmeasure' = pmeasure,
            'pse' = pse,
            'pinfit' = pinfit,
            'poutfit' = poutfit,
            'resid' = resid
          )
      },
      
      # Standard score----------
      
      .populateStTable = function(results) {
        table <- self$results$stand$st
        st <- results$st
        names <- dimnames(st)[[1]]
        
        for (name in names) {
          row <- list()
          row[['Total']] <- st[name, 1]
          row[['Percentile']] <- st[name, 2]
          row[['Z']] <- st[name, 3]
          row[['T']] <- st[name, 4]
          table$addRow(rowKey = name, values = row)
        }
      },
      
      # Summary of total score---------
      
      .populateToTable = function(results) {
        table <- self$results$stand$to
        
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
        # n <- to$n
        # min <- to$min
        # max <- to$max
        # mean <- to$mean
        # median <- to$median
        # sd <- to$sd
        # se <- to$se
        # skew <- to$skew
        # kurtosis <- to$kurtosis
        #
        # row <- list()
        # row[['N']] <- n
        # row[['Minimum']] <- min
        # row[['Maximum']] <- max
        # row[['Mean']] <- mean
        # row[['Median']] <- median
        # row[['SD']] <- sd
        # row[['SE']] <- se
        # row[['Skewness']] <- skew
        # row[['Kurtosis']] <- kurtosis
        # table$setRow(rowNo = 1, values = row)
      },
      #### Init. tables ====================================================
      
      .initItemsTable = function() {
        table <- self$results$items
        for (i in seq_along(items))
          table$addFootnote(rowKey = items[i], 'name')
      },
      
      # populate scale table-------------------
      
      .populateScaleTable = function(results) {
        table <- self$results$mf$scale
        row <- list(
          reliability = results$reliability[1],
          modelfit    = results$modelfit,
          modelfitp   = results$modelfitp
        )
        
        table$setRow(rowNo = 1, values = row)
        # reliability <- results$reliability
        # modelfit <- results$modelfit
        # modelfitp <- results$modelfitp
        #
        # row <- list()
        # row[['reliability']] <- reliability[1]
        # row[['modelfit']] <- modelfit
        # row[['modelfitp']] <- modelfitp
        #
        # table$setRow(rowNo = 1, values = row)
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
      
      # populate item table==============================================
      
      .populateItemsTable = function(results) {
        table <- self$results$items
        items <- self$options$vars
        stats <- list(
          prop    = results$prop,
          measure = results$imeasure,
          ise     = results$ise,
          infit   = results$infit,
          outfit  = results$outfit
        )
        for (i in seq_along(items)) {
          row <- lapply(stats, function(x)
            x[i])
          table$setRow(rowKey = items[i], values = row)
        }
        # prop <- results$prop
        # imeasure <- results$imeasure
        # ise <- results$ise
        # infit <- results$infit
        # outfit <- results$outfit
        # for (i in seq_along(items)) {
        #   row <- list()
        #   row[["prop"]] <- prop[i]
        #   row[["measure"]] <- imeasure[i]
        #   row[["ise"]] <- ise[i]
        #   row[["infit"]] <- infit[i]
        #   row[["outfit"]] <- outfit[i]
        #   table$setRow(rowKey = items[i], values = row)
      },
      # Wrightmapt Plot-----------------------------------------------
      
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        pmeasure <- image$state[[1]]
        imeasure <- image$state[[2]]
        vars <- image$state[[3]]
        plot <- ShinyItemAnalysis::ggWrightMap(pmeasure,
                                               imeasure,
                                               item.names = vars,
                                               color = "deepskyblue")
        #plot <- plot+ggtheme
        print(plot)
        TRUE
      },
      # #Prepare Expected score curve functions------------
      
      # .prepareEscPlot = function(data) {
      #
      #   set.seed(1234)
      #   tamp = TAM::tam(resp =as.matrix(data))
      #
      # # Prepare Data For ESC Plot -------
      #
      #   image <- self$results$plot4
      #   image$setState(tamp)
      #
      # },
      .plot4 = function(image, ...) {
        num <- self$options$num
        if (!self$options$plot4)
          return(FALSE)
        #tamobj <- private$.computeTamobj()
        tamobj <- private$.cache$tamobj
        
        plot4 <- plot(tamobj,
                      items = num,
                      #type="items" produce item response curve not expected curve
                      type = "expected",
                      export = FALSE)
        print(plot4)
        TRUE
      },
      
      .inPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        infit1 <- image$state
        
        plot <- ggplot(infit1, aes(x = Item, y = infit)) +
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
      
            
      .outPlot = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        outfit1 <- image$state
        
        plot <- ggplot(outfit1, aes(x = Item, y = outfit)) +
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
            values = c("Infit" = 16, "Outfit" = 17),  # Filled circle and triangle
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
          ggtitle("") +
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
      },
      
      .computeTamobj = function() {
        data <- private$.cleanData()
        
        set.seed(1234)
        tamobj = TAM::tam(resp = as.matrix(data))
        return(tamobj)
      }
    )
  )

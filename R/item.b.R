

# ITEM ANALYSIS
#' @import ggplot2

itemClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "itemClass",
    inherit = itemBase,
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
            '<li>Enter the correct answer separated by commas, but there must be no spaces between commas.</li>',
            '<li>To resolve error messages like <b>breaks are not unique</b>, we need to add more questions and respondents.</li>',
            '<li>By a rule of thumb, all items with a discrimination lower than 0.2 (threshold in the plot), should be checked for content.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        if (self$options$disc)
          self$results$disc$setNote(
            "Note",
            "ULI:Upper-Lower Index based on 3 groups, RIT:Item-Total correlation, RIR: Item-Rest correlation."
          )
        if (isTRUE(self$options$plot)) {
          width <- self$options$width
          height <- self$options$height
          self$results$plot$setSize(width, height)
        }
        if (isTRUE(self$options$plot1)) {
          width <- self$options$width1
          height <- self$options$height1
          self$results$plot1$setSize(width, height)
        }
        if (isTRUE(self$options$plot2)) {
          width <- self$options$width2
          height <- self$options$height2
          self$results$plot2$setSize(width, height)
        }
        
        if (isTRUE(self$options$plot3)) {
          width <- self$options$width3
          height <- self$options$height3
          self$results$plot3$setSize(width, height)
        }
      },
      
      .run = function() {
        if (length(self$options$vars) < 2)
          return()
        data <- self$data
        data <- na.omit(data)
        key <- self$options$key
        key1 <- strsplit(self$options$key, ',')[[1]]
        
        if (!identical(private$.cache$options, self$options) ||
            !identical(private$.cache$data, self$data)) {
          private$.cache <- list()
        }
        
        # check and compute---
        if (is.null(private$.cache$is_ready)) {
          private$.computeRES()
          private$.cache$is_ready <- TRUE
          
          private$.cache$options <- self$options
          private$.cache$data <- self$data
        }
        # results---
        counts <- private$.cache$counts
        ts <- private$.cache$ts
        prop <- private$.cache$prop
        res <- private$.cache$res
        dicho <- private$.cache$dicho
        
        # Counts of respondents-----------
        
        #counts <- CTT::distractor.analysis(data, key1)
        #---------------------------------------------
        #counts<- results$counts
        
        if (isTRUE(self$options$count)) {
          vars <- self$options$vars
          table <- self$results$count
          tab <- NULL
          for (i in seq_along(vars)) {
            tab[[i]] <- as.data.frame.matrix(counts[[i]])
          }
          tab <- tab
          for (i in seq_along(vars)) {
            table <- self$results$count[[i]]
            names <- dimnames(tab[[i]])[[1]]
            dims <- dimnames(tab[[i]])[[2]]
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- tab[[i]][name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        ### Total summary############
        
        #group1 <- self$options$group1
        
        #------------------------------------------------
        #ts <- CTT::distractorAnalysis(data, key1, nGroups = group1)
        #-------------------------------------------------
        #ts<- results$ts
        
        if (isTRUE(self$options$sum1)) {
          vars <- self$options$vars
          
          table <- self$results$sum1
          tab <- NULL
          for (i in seq_along(vars)) {
            tab[[i]] <- as.data.frame.matrix(ts[[i]])
          }
          tab <- tab
          #self$results$text$setContent(tab)
          for (i in seq_along(vars)) {
            table <- self$results$sum1[[i]]
            names <- dimnames(tab[[i]])[[1]]
            dims <- dimnames(tab[[i]])[[2]]
            for (dim in dims) {
              table$addColumn(name = paste0(dim), type = 'text')
            }
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- tab[[i]][name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        # Proportions of respondents--------
        
        #prop <- CTT::distractor.analysis(data, key1, p.table = TRUE)
        #---------------------------------------
        #prop<- results$prop
        
        if (isTRUE(self$options$prop)) {
          table <- self$results$prop
          tab <- NULL
          for (i in seq_along(vars)) {
            tab[[i]] <- as.data.frame.matrix(prop[[i]])
          }
          tab <- tab
          for (i in seq_along(vars)) {
            table <- self$results$prop[[i]]
            names <- dimnames(tab[[i]])[[1]]
            dims <- dimnames(tab[[i]])[[2]]
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- tab[[i]][name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        #summary-------------
        
        #res <- CTT::distractorAnalysis(data, key1)
        #--------------------------------------
        #res<- results$res
        
        if (isTRUE(self$options$sum)) {
          table <- self$results$sum
          tab <- NULL
          for (i in seq_along(vars)) {
            tab[[i]] <- as.data.frame.matrix(res[[i]][1:4])
          }
          tab <- tab
          for (i in seq_along(vars)) {
            table <- self$results$sum[[i]]
            names <- dimnames(tab[[i]])[[1]]
            dims <- dimnames(tab[[i]])[[2]]
            for (name in names) {
              row <- list()
              for (j in seq_along(dims)) {
                row[[dims[j]]] <- tab[[i]][name, j]
              }
              table$addRow(rowKey = name, values = row)
            }
          }
        }
        
        # Scores, scored file-----------
        #dicho <- CTT::score(data, key1, output.scored = TRUE)
        #--------------------------------------------
        #dicho<- results$dicho
        dicho <- dicho$scored
        dicho <- as.data.frame(dicho) # dichotomous matrix
        
        if (isTRUE(self$options$total)) {
          binary <- CTT::score(data, key1, output.scored = TRUE)
          
          # Save total score---------
          score <- as.vector(binary$score)
          
          self$results$total$setRowNums(rownames(data))
          self$results$total$setValues(score)
          
        }
        
        # Save scoring-----------------
        
        if (isTRUE(self$options$scoring)) {
          binary <- CTT::score(data, key1, output.scored = TRUE)
          
          keys <- 1:length(self$options$vars)
          titles <- paste("Item", 1:length(self$options$vars))
          descriptions <- paste("Item", 1:length(self$options$vars))
          measureTypes <- rep("continuous", length(self$options$vars))
          
          self$results$scoring$set(
            keys = keys,
            titles = titles,
            descriptions = descriptions,
            measureTypes = measureTypes
          )
          
          self$results$scoring$setRowNums(rownames(data))
          
          scoring <- as.data.frame(binary$scored)
          
          for (i in 1:length(self$options$vars)) {
            scores <- as.numeric(scoring[, i])
            self$results$scoring$setValues(index = i, scores)
          }
        }
        
        # Empirical ICC-----------------
        
        if (isTRUE(self$options$plot3)) {
          myscores <- CTT::score(data, key1, output.scored = TRUE)
          
          image3 <- self$results$plot3
          image3$setState(myscores)
          
        }
        
        # traditional item analysis table--------------------
        
        it <- ShinyItemAnalysis::ItemAnalysis(dicho)
        
        dif <- it[1]
        ULI <- it[13]
        RIT <- it[11]
        RIR <- it[10]
        
        discri <- data.frame(dif, ULI, RIT, RIR)
        
        #self$results$text$setContent(discri)
        
        dif <- discri$Difficulty
        ULI <- discri$ULI
        RIT <- discri$RIT
        RIR <- discri$RIR
        # self$results$text$setContent(dis)
        disc <- self$options$disc
        table <- self$results$disc
        vars <- self$options$vars
        for (i in seq_along(vars)) {
          row <- list()
          row[["dif"]] <-  dif[i]
          row[["ULI"]] <-  ULI[i]
          row[["RIT"]] <-  RIT[i]
          row[["RIR"]] <-  RIR[i]
          table$setRow(rowKey = vars[i], values = row)
        }
        #  plot1----------
        disi <- self$options$disi
        image <- self$results$plot1
        image$setState(dicho)
        
        # Histogram of total score------------
        
        if (isTRUE(self$options$plot2)) {
          dicho <- CTT::score(data, key1, output.scored = TRUE)
          
          binary <- dicho$scored
          score <- rowSums(binary)
          df <- data.frame(score)
          state <- list(score, df)
          
          image2 <- self$results$plot2
          image2$setState(state)
        }
      },
      #-------------
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        score <- image2$state[[1]]
        df <- image2$state[[2]]
        
        # plot2<- ggplot(df, aes(score)) +
        #   geom_histogram(binwidth = 1) +
        #   xlab("Total score") +
        #   ylab("Number of respondents")
        
        # histogram
        ggplot(df, aes(score)) +
          geom_histogram(binwidth = 1, col = "black") +
          xlab("Total score") +
          ylab("Number of respondents") +
          ShinyItemAnalysis::theme_app()
        
        # colors by cut-score
        cut <- median(score) # cut-score
        color <- c(rep("red", cut - min(score)), "gray", rep("blue", max(score) - cut))
        df <- data.frame(score)
        
        # histogram
        plot2 <- ggplot(df, aes(score)) +
          geom_histogram(binwidth = 1,
                         fill = color,
                         col = "black") +
          xlab("Total score") +
          ylab("Number of respondents") +
          ShinyItemAnalysis::theme_app()
        plot2 + ggtheme
        print(plot2)
        TRUE
      },
      
      .plot = function(image, ...) {
        data <- self$data
        data <- na.omit(data)
        key <- self$options$key
        key1 <- strsplit(self$options$key, ',')[[1]]
        nums <- self$options$num
        group <- self$options$group
        plot <- ShinyItemAnalysis::plotDistractorAnalysis(data, key1, num.groups = group, item = nums)
        print(plot)
        TRUE
      },
      
      .plot1 = function(image, ggtheme, theme, ...) {
        if (is.null(image$state))
          return(FALSE)
        dicho <- image$state
        disi <- self$options$disi
        plot1 <- ShinyItemAnalysis::DDplot(
          dicho,
          discrim = disi,
          k = 3,
          l = 1,
          u = 3
        )
        if (self$options$angle > 0) {
          plot1 <- plot1 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = self$options$angle, hjust = 1))
        }
        print(plot1)
        TRUE
      },
      
      .plot3 = function(image3, ...) {
        if (is.null(image3$state))
          return(FALSE)
        
        num1 <- self$options$num1
        myScores <- image3$state
        plot3 <- CTT::cttICC(
          myScores$score,
          myScores$scored[, num1],
          plotTitle = '',
          colTheme = "spartans",
          cex = 1.5
        )
        print(plot3)
        TRUE
      },
      
      .computeRES = function() {
        if (length(self$options$vars) < 2)
          return()
        
        data <- self$data
        data <- na.omit(data)
        key1 <- strsplit(self$options$key, ',')[[1]]
        group1 <- self$options$group1
        
        # chche---
        private$.cache$counts <- CTT::distractor.analysis(data, key1)
        private$.cache$ts <- CTT::distractorAnalysis(data, key1, nGroups = group1)
        private$.cache$prop <- CTT::distractor.analysis(data, key1, p.table = TRUE)
        private$.cache$res <- CTT::distractorAnalysis(data, key1)
        private$.cache$dicho <- CTT::score(data, key1, output.scored = TRUE)
      }
    )
  )

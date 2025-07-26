
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
            '<li><b>Missing values handling:</b> Set missing values to 0 or "WRONG" in jamovi data view to prevent scoring errors.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        if (self$options$disc)
          self$results$disc$setNote(
            "Note",
            "ULI:Upper-Lower Index based on 3 groups, RIT:Item-Total correlation, RIR: Item-Rest correlation. Missing values should be recoded as 0 or incorrect answer in jamovi."
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
        
        # 결측값 확인 및 경고 메시지
        data <- self$data
        has_missing <- any(is.na(data))
        
        if (has_missing) {
          # 사용자에게 경고 메시지 표시
          self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
            title = "⚠️ Missing Values Detected",
            content = paste(
              '<div style="border: 2px solid #ffebcc; border-radius: 15px; padding: 15px; background-color: #fff3e0; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<h4 style="color: #e65100;">Action Required:</h4>',
              '<p><strong>Missing values detected in your data!</strong></p>',
              '<p>To ensure proper scoring, please:</p>',
              '<ol>',
              '<li>Go to jamovi Data view</li>',
              '<li>Select cells with missing values</li>',
              '<li>Replace them with <b>0</b> or <b>WRONG</b> (for incorrect answers)</li>',
              '</ol>',
              '<p><strong>Why this matters:</strong> Missing values cannot be properly scored and may cause analysis errors.</p>',
              '</div></div>',
              '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
              '<div style="text-align:justify;">',
              '<h4>General Instructions:</h4>',
              '<ul>',
              '<li>Enter the correct answer separated by commas, but there must be no spaces between commas.</li>',
              '<li>To resolve error messages like <b>breaks are not unique</b>, we need to add more questions and respondents.</li>',
              '<li>By a rule of thumb, all items with a discrimination lower than 0.2 (threshold in the plot), should be checked for content.</li>',
              '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
              '</ul></div></div>'
            )
          ))
          
          # 결측값이 있으면 분석을 중단하고 사용자에게 안내
          return()
        }
        
        # 결측값이 없는 경우에만 분석 진행
        key <- self$options$key
        key1 <- strsplit(self$options$key, ',')[[1]]
        
        if (!identical(private$.cache$options, self$options) ||
            !identical(private$.cache$data, self$data)) {
          private$.cache <- list()
          private$.computeRES(data, key1)
          private$.cache$is_ready <- TRUE
          
          private$.cache$options <- self$options
          private$.cache$data <- self$data
        }
        
        counts <- private$.cache$counts
        ts <- private$.cache$ts
        prop <- private$.cache$prop
        res <- private$.cache$res
        dicho <- private$.cache$dicho
        
        # Counts of respondents
        if (isTRUE(self$options$count)) {
          vars <- self$options$vars
          table <- self$results$count
          tab <- NULL
          for (i in seq_along(vars)) {
            tab[[i]] <- as.data.frame.matrix(counts[[i]])
          }
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
        
        # Total summary
        if (isTRUE(self$options$sum1)) {
          vars <- self$options$vars
          table <- self$results$sum1
          tab <- NULL
          for (i in seq_along(vars)) {
            tab[[i]] <- as.data.frame.matrix(ts[[i]])
          }
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
        
        # Proportions of respondents
        if (isTRUE(self$options$prop)) {
          vars <- self$options$vars
          table <- self$results$prop
          tab <- NULL
          for (i in seq_along(vars)) {
            tab[[i]] <- as.data.frame.matrix(prop[[i]])
          }
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
        
        # Summary
        if (isTRUE(self$options$sum)) {
          vars <- self$options$vars
          table <- self$results$sum
          tab <- NULL
          for (i in seq_along(vars)) {
            tab[[i]] <- as.data.frame.matrix(res[[i]][1:4])
          }
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
        
        # Scores, scored file
        dicho_scored <- as.data.frame(dicho$scored) # dichotomous matrix
        
        # Total score
        if (isTRUE(self$options$total)) {
          score <- as.vector(dicho$score)
          self$results$total$setRowNums(rownames(data))
          self$results$total$setValues(score)
        }
        
        # Save scoring
        if (isTRUE(self$options$scoring)) {
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
          
          scoring <- as.data.frame(dicho$scored)
          
          for (i in 1:length(self$options$vars)) {
            scores <- as.numeric(scoring[, i])
            self$results$scoring$setValues(index = i, scores)
          }
        }
        
        # Empirical ICC
        if (isTRUE(self$options$plot3)) {
          self$results$plot3$setState(dicho)
        }
        
        # Traditional item analysis table
        if (self$options$disc) {
          it <- ShinyItemAnalysis::ItemAnalysis(dicho_scored)
          
          dif <- it[1]
          ULI <- it[13]
          RIT <- it[11]
          RIR <- it[10]
          
          discri <- data.frame(dif, ULI, RIT, RIR)
          
          dif <- discri$Difficulty
          ULI <- discri$ULI
          RIT <- discri$RIT
          RIR <- discri$RIR
          
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
        }
        
        # Plot1
        if (isTRUE(self$options$plot1)) {
          self$results$plot1$setState(dicho_scored)
        }
        
        # Histogram of total score
        if (isTRUE(self$options$plot2)) {
          score <- rowSums(dicho$scored)
          df <- data.frame(score)
          state <- list(score, df)
          
          self$results$plot2$setState(state)
        }
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        score <- image2$state[[1]]
        df <- image2$state[[2]]
        
        cut <- median(score) # cut-score
        color <- c(rep("red", cut - min(score)), "gray", rep("blue", max(score) - cut))
        
        plot2 <- ggplot(df, aes(score)) +
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
      
      .plot = function(image, ...) {
        data <- self$data
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
      
      .computeRES = function(data, key1) {
        group1 <- self$options$group1
        
        dicho <- CTT::score(data, key1, output.scored = TRUE)
        private$.cache$dicho <- dicho
        
        private$.cache$counts <- CTT::distractor.analysis(data, key1)
        private$.cache$prop <- CTT::distractor.analysis(data, key1, p.table = TRUE)
        private$.cache$ts <- CTT::distractorAnalysis(data, key1, nGroups = group1)
        private$.cache$res <- CTT::distractorAnalysis(data, key1)
      }
    )
  )
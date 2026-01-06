clrClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "clrClass",
    inherit = clrBase,
    private = list(
      .allCache = NULL,
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
            '<li>Conditional likelihood ratio tests are estimated by <b>iarm</b> R package.</li>',
            '<li>Model=RM for binary items, or model=PCM for polytomous items, is used.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
            
          )
        ))
        if (self$options$clr)
          self$results$clr$setNote("Note", "'Overall' indicates test of homogeneity.")
      },
      #########################################################
      
      .run = function() {
        data <- self$data
        groupVarName <- self$options$group
        vars <- self$options$vars
        varNames <- c(groupVarName, vars)
        model <- self$options$model
        
        if (is.null(groupVarName))
          return()
        
        data <- jmvcore::select(self$data, varNames)
        
        for (var in vars)
          data[[var]] <- jmvcore::toNumeric(data[[var]])
        
        # exclude rows with missings in the grouping variable
        
        data <- data[!is.na(data[[groupVarName]]), ]
        
        if (is.null(private$.allCache)) {
          private$.allCache <- private$.computeRES()
        }
        
        all <- private$.allCache
        # #############################################################
        #
        # dif <- iarm::clr_tests(
        #   dat.items = data[, -1],
        #   dat.exo = data[[groupVarName]],
        #   model = self$options$model
        # )
        # #########################################################
        
        names <- c("Overall", groupVarName)
        clr <- as.numeric(all$dif[, 1])
        df <- as.numeric(all$dif[, 2])
        pvalue <- as.numeric(all$dif[, 3])
        res <- data.frame(names, clr, df, pvalue)
        
        # Creating table-------------
        if (isTRUE(self$options$clr)) {
          table <- self$results$clr
          stat_indices <- c(
            name = 1,
            clr = 2,
            df = 3,
            p = 4
          )
          for (i in seq_len(nrow(res))) {
            row <- setNames(lapply(stat_indices, function(idx)
              res[i, idx]),
              names(stat_indices))
            table$addRow(rowKey = i, values = row)
          }
        }
        
        # Standardized residuals----------------
        
        items <- self$options$vars
        model <- self$options$model
        score <- self$options$score
        if (model == "RM") {
          rm.mod <- eRm::RM(X = data[, -1])
          rm <- iarm::item_obsexp(rm.mod)
          if (score == "low") {
            sc <- rm[[1]]
          }
          if (score == "high") {
            sc <- rm[[2]]
          }
          obs <- as.numeric(sc[, 1])
          exp <- as.numeric(sc[, 2])
          std <- as.numeric(sc[, 3])
          sig <- as.character(sc[, 4])
          low <- data.frame(obs, exp, std, sig)
          
          # Creating low score table-------------
          if (isTRUE(self$options$resi)) {
            table <- self$results$resi
            for (i in seq_along(items)) {
              row <- list()
              row[["obs"]] <- low[i, 1]
              row[["exp"]] <- low[i, 2]
              row[["std"]] <- low[i, 3]
              row[["sig"]] <- low[i, 4]
              table$setRow(rowKey = items[i], values = row)
            }
          }
        }
        if (model == "PCM") {
          pc.mod <- eRm::PCM(X = data[, -1])
          pc <- iarm::item_obsexp(pc.mod)
          if (score == "low") {
            sc <- pc[[1]]
          }
          if (score == "high") {
            sc <- pc[[2]]
          }
          obs <- as.numeric(sc[, 1])
          exp <- as.numeric(sc[, 2])
          std <- as.numeric(sc[, 3])
          sig <- as.character(sc[, 4])
          
          pc <- data.frame(obs, exp, std, sig)
          
          # Creating low score table-------------
          if (isTRUE(self$options$resi)) {
            table <- self$results$resi
            for (i in seq_along(items)) {
              row <- list()
              row[["obs"]] <- pc[i, 1]
              row[["exp"]] <- pc[i, 2]
              row[["std"]] <- pc[i, 3]
              row[["sig"]] <- pc[i, 4]
              table$setRow(rowKey = items[i], values = row)
            }
          }
        }
        
        # # Partial Gamma to detect Differential Item Functioning (DIF)------
        #
        # gam <- iarm::partgam_DIF(dat.items = data[, -1], dat.exo = data[[groupVarName]])
        ################################################################
        gam <- all$gam
        
        gamma <- gam$gamma
        se <- gam$se
        p <- gam$pvalue
        lower <- gam$lower
        upper <- gam$upper
        ##################
        if (isTRUE(self$options$dif)) {
          items <- self$options$vars
          table <- self$results$dif
          for (i in seq_along(items)) {
            row <- list()
            row[["gamma"]] <- gamma[i]
            row[["se"]] <- se[i]
            row[["p"]] <- p[i]
            row[["lower"]] <- lower[i]
            row[["upper"]] <- upper[i]
            table$setRow(rowKey = items[i], values = row)
          }
        }
        
        #  plot----------
        image <- self$results$plot
        image$setState(data[, -1])
        
        # DIF using total scores---------------
        image1 <- self$results$plot1
        state <- list(data[, -1], data[[groupVarName]])
        image1$setState(state)
        
        # DIF using class intervals---------------
        image2 <- self$results$plot2
        state <- list(data[, -1], data[[groupVarName]])
        image2$setState(state)
      },
      
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        data <- image$state
        num <- self$options$num
        plot <-  iarm::ICCplot(data = data,
                               itemnumber = num,
                               method = "score")
        print(plot)
        TRUE
      },
      
      .plot1 = function(image1, ggtheme, theme, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        num <- self$options$num
        
        data <- image1$state[[1]]
        group <- image1$state[[2]]
        plot1 <-  iarm::ICCplot(
          data = data,
          itemnumber = num,
          method = "score",
          icclabel = "yes",
          dif = "yes",
          difvar = group,
          difstats = "no"
        )
        print(plot1)
        TRUE
        
      },
      
      .plot2 = function(image2, ggtheme, theme, ...) {
        if (is.null(image2$state))
          return(FALSE)
        num <- self$options$num
        ci <- self$options$ci
        
        data <- image2$state[[1]]
        group <- image2$state[[2]]
        plot2 <-  iarm::ICCplot(
          data = data,
          itemnumber = num,
          method = "cut",
          cinumber = ci,
          icclabel = "yes",
          dif = "yes",
          difvar = group,
          difstats = "no"
        )
        print(plot2)
        TRUE
      },
      
      .computeRES = function() {
        data <- self$data
        groupVarName <- self$options$group
        vars <- self$options$vars
        varNames <- c(groupVarName, vars)
        model <- self$options$model
        
        if (is.null(groupVarName))
          return()
        
        data <- jmvcore::select(self$data, varNames)
        
        for (var in vars)
          data[[var]] <- jmvcore::toNumeric(data[[var]])
        
        # exclude rows with missings in the grouping variable
        
        data <- data[!is.na(data[[groupVarName]]), ]
        
        dif <- iarm::clr_tests(
          dat.items = data[, -1],
          dat.exo = data[[groupVarName]],
          model = self$options$model
        )
        
        # Partial Gamma to detect Differential Item Functioning (DIF)------
        gam <- iarm::partgam_DIF(dat.items = data[, -1], dat.exo = data[[groupVarName]])
        
        res <- list(dif = dif, gam = gam)
        return(res)
      }
    )
  )
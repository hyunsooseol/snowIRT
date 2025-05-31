
# diford

logitClass <- if (requireNamespace('jmvcore', quietly = TRUE))
  R6::R6Class(
    "logitClass",
    inherit = logitBase,
    private = list(
      .htmlwidget = NULL,
      #=============================================================
      
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
            '<li>Performs DIF detection procedure for ordinal data based either on adjacent category logit model or on cumulative logit model.</li>',
            '<li>The focal group should be coded as 1.</li>',
            '<li>DIF likelihood ratio statistics are estimated by using <b>difNLR::difORD</b> function.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        if (self$options$method)
          self$results$method$setNote(
            "Note",
            "Adj.p = The adjusted p-values by likelihood ratio test using multiple comparison."
          )
      },
      
      #=================================================
      
      .run = function() {
        if (length(self$options$vars) < 1)
          return()
        
        # get variables-------
        
        data <- self$data
        groupVarName <- self$options$group
        vars <- self$options$vars
        varNames <- c(groupVarName, vars)
        
        if (is.null(groupVarName))
          return()
        data <- dplyr::select(self$data, varNames)
        
        for (var in vars)
          data[[var]] <- jmvcore::toNumeric(data[[var]])
        # exclude rows with missings in the grouping variable
        data <- data[!is.na(data[[groupVarName]]), ]
        
        
        # analysis--------
        
        if (isTRUE(self$options$puri))
        {
          fit <- difNLR::difORD(
            Data = data,
            group = groupVarName,
            focal.name = 1,
            model = self$options$model,
            type = self$options$type,
            match = self$options$match,
            purify = TRUE,
            p.adjust.method = self$options$padjust,
          )
          
        } else{
          fit <- difNLR::difORD(
            Data = data,
            group = groupVarName,
            focal.name = 1,
            model = self$options$model,
            type = self$options$type,
            match = self$options$match,
            p.adjust.method = self$options$padjust,
          )
        }
        chi <- fit$Sval
        p <- fit$pval
        padj <- fit$adj.pval
        table <- self$results$method
        for (i in seq_along(self$options$vars)) {
          row <- list()
          row[["chi"]] <- chi[i]
          row[["p"]] <- p[i]
          row[["padj"]] <- padj[i]
          table$setRow(rowKey = vars[i], values = row)
        }
      }
      
    )
  )

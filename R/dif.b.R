
difClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "difClass",
    inherit = difBase,
    private = list(
      .htmlwidget = NULL,
      .state = list(), # 계산 결과 캐싱
      
      .init = function() {
        private$.htmlwidget <- HTMLWidget$new()
        private$.state <- list() # 상태 초기화
        
        # 데이터 유효성 검사
        if (is.null(self$data) | is.null(self$options$vars)) {
          self$results$instructions$setVisible(visible = TRUE)
        }
        
        # 안내 내용 설정
        self$results$instructions$setContent(private$.htmlwidget$generate_accordion(
          title = "Instructions",
          content = paste(
            '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">',
            '<div style="text-align:justify;">',
            '<ul>',
            '<li>Performs DIF detection using <b>difR</b> R package.</li>',
            '<li>For Raju and MH method, the focal group should be coded as <b>1</b>.</li>',
            '<li>Feature requests and bug reports can be made on my <a href="https://github.com/hyunsooseol/snowIRT/issues" target="_blank">GitHub</a>.</li>',
            '</ul></div></div>'
          )
        ))
        
        # 노트 설정 - 조건 검사 최적화
        if (self$options$raju) {
          self$results$raju$setNote(
            "Note",
            "1. Effect size(ETS Delta scale) for absolute values of 'deltaRaju' = A: negligible effect(<1.0), B: moderate effect(>1.0),C: large effect(>1.5)."
          )
        }
        
        # 한 번의 반복문으로 모든 플롯 크기 설정
        plots <- list(
          list(name = "zplot", width = "width1", height = "height1"),
          list(name = "plot3", width = "width2", height = "height2"),
          list(name = "plot1", width = "width3", height = "height3"),
          list(name = "plot2", width = "width4", height = "height4")
        )
        for (plot in plots) {
          if (isTRUE(self$options[[plot$name]])) {
            self$results[[plot$name]]$setSize(
              self$options[[plot$width]], 
              self$options[[plot$height]]
            )
          }
        }
        
        if (length(self$options$vars) <= 1)
          self$setStatus('complete')
      },
      
      .run = function() {
        # 기본 데이터 준비
        groupVarName <- self$options$group
        vars <- self$options$vars
        
        if (is.null(groupVarName))
          return()
        
        # 필요한 변수만 선택하여 메모리 사용 최적화
        data <- self$data[, c(groupVarName, vars), drop = FALSE]
        
        # 데이터 전처리 - 벡터화 접근
        data[vars] <- lapply(data[vars], jmvcore::toNumeric)
        
        # 결측치 제거 및 그룹 수준 확인 - 벡터화 접근
        data <- data[!is.na(data[[groupVarName]]), , drop = FALSE]
        groupLevels <- base::levels(data[[groupVarName]])
        
        # 다중 그룹 분석 (GMH 방법)
        if (length(groupLevels) > 2) {
          private$.runGMH(data, groupVarName)
        } else {
          # 이항 그룹 분석 (Raju 및 MH 방법)
          private$.runBinaryGroup(data, groupVarName)
        }
        
        # 사용하지 않는 큰 객체 정리
        gc(verbose = FALSE)
      },
      
      # 다중 그룹 분석 함수
      .runGMH = function(data, groupVarName) {
        if (isTRUE(self$options$gmh | self$options$plot2)) {
          # 캐시된 결과가 있는지 확인
          cacheKey <- paste0("gmh_", paste(self$options$vars, collapse = "_"))
          
          if (is.null(private$.state[[cacheKey]])) {
            fn <- as.numeric(strsplit(self$options$fn, ',')[[1]])
            
            # difGMH 함수 호출 전에 필요한 변수만 유지
            gmh <- difR::difGMH(data,
                                groupVarName,
                                focal.names = fn,
                                p.adjust.method = self$options$padjust2)
            
            # 필요한 결과만 캐싱
            private$.state[[cacheKey]] <- list(
              GMH = gmh$GMH,
              p.value = gmh$p.value,
              adjusted.p = gmh$adjusted.p,
              plotData = gmh # 플롯 생성에 필요
            )
          }
          
          # GMH 결과 테이블 생성 - 캐시 사용
          table <- self$results$gmh
          items <- self$options$vars
          cachedData <- private$.state[[cacheKey]]
          
          gmhstat <- as.vector(cachedData$GMH)
          p <- as.vector(cachedData$p.value)
          padjust <- as.vector(cachedData$adjusted.p)
          
          # 각 행을 개별적으로 설정 (setRows 대신 setRow 사용)
          for (i in seq_along(items)) {
            table$setRow(rowKey = items[i], values = list(
              gmhstat = gmhstat[i],
              p = p[i],
              padjust = padjust[i]
            ))
          }
          
          # GMH 플롯 설정
          self$results$plot2$setState(cachedData$plotData)
        }
      },
      
      # 이항 그룹 분석 함수
      .runBinaryGroup = function(data, groupVarName) {
        # 캐시 키 생성
        cacheKey <- paste0("binary_", paste(self$options$vars, collapse = "_"))
        
        if (is.null(private$.state[[cacheKey]])) {
          # 데이터 분할 최적화 - 전체 분할 후 처리
          groupData <- split(data, data[[groupVarName]])
          
          # 참조 그룹 및 관심 그룹 데이터 준비
          ref.data <- groupData[["0"]][, -1, drop = FALSE]  # groupVarName 제외
          focal.data <- groupData[["1"]][, -1, drop = FALSE]  # groupVarName 제외
          
          # TAM 모델 적합 - 병렬 계산 가능하면 병렬로 처리
          tam.ref <- TAM::tam.mml(resp = ref.data)
          tam.focal <- TAM::tam.mml(resp = focal.data)
          
          ref1 <- tam.ref$xsi
          focal1 <- tam.focal$xsi
          
          # 각 그룹의 항목 매개변수 계산
          item.1PL <- rbind(ref1, focal1)
          
          # Raju 방법 적용
          res1 <- difR::difRaju(
            irtParam = item.1PL,
            focal.name = 1,
            p.adjust.method = self$options$padjust,
            same.scale = FALSE
          )
          
          # ETS delta scale 계산
          pars <- res1$itemParInit
          J <- nrow(pars) / 2
          mR <- pars[1:J, 1]
          mF <- difR::itemRescale(pars[1:J, ], pars[(J + 1):(2 * J), ])[, 1]
          
          rr1 <- mF - mR
          rr2 <- -2.35 * rr1
          
          symb1 <- symnum(abs(rr2), c(0, 1, 1.5, Inf), symbols = c("A", "B", "C"))
          
          # 계산 결과 캐싱
          private$.state[[cacheKey]] <- list(
            zstat = as.vector(res1$RajuZ),
            p = as.vector(res1$p.value),
            padjust = as.vector(res1$adjusted.p),
            delta = as.vector(rr2),
            es = as.vector(symb1),
            itempar = res1$itemParInit,
            rajuData = res1  # 플롯용
          )
          
          # 메모리 정리
          rm(tam.ref, tam.focal, res1, groupData)
          gc(verbose = FALSE)
        }
        
        # MH 방법 실행
        if (isTRUE(self$options$mh | self$options$plot1)) {
          private$.runMH(data, groupVarName)
        }
        
        # Raju 결과 테이블 업데이트
        private$.updateRajuTable(private$.state[[cacheKey]])
        
        # 시각화 상태 설정
        self$results$zplot$setState(private$.state[[cacheKey]]$rajuData)
        self$results$plot3$setState(private$.state[[cacheKey]]$itempar)
      },
      
      # MH 분석 실행 및 결과 처리
      .runMH = function(data, groupVarName) {
        # 캐시 키 생성
        cacheKey <- paste0("mh_", paste(self$options$vars, collapse = "_"))
        
        if (is.null(private$.state[[cacheKey]])) {
          mh <- difR::difMH(data,
                            groupVarName,
                            focal.name = 1,
                            p.adjust.method = self$options$padjust)
          
          # 필요한 결과만 캐싱
          private$.state[[cacheKey]] <- list(
            mhstat = as.vector(mh$MH),
            p = as.vector(mh$p.value),
            padjust = as.vector(mh$adjusted.p),
            plotData = mh  # 플롯용
          )
        }
        
        # 결과 테이블 업데이트
        table <- self$results$mh
        items <- self$options$vars
        cachedData <- private$.state[[cacheKey]]
        
        # 각 행을 개별적으로 설정 (setRows 대신 setRow 사용)
        for (i in seq_along(items)) {
          table$setRow(rowKey = items[i], values = list(
            mhstat = cachedData$mhstat[i],
            p = cachedData$p[i],
            padjust = cachedData$padjust[i]
          ))
        }
        
        # MH 플롯 상태 설정
        self$results$plot1$setState(cachedData$plotData)
      },
      
      # Raju 결과 테이블 업데이트
      .updateRajuTable = function(results) {
        table <- self$results$raju
        items <- self$options$vars
        
        # 각 행을 개별적으로 설정 (setRows 대신 setRow 사용)
        for (i in seq_along(items)) {
          table$setRow(rowKey = items[i], values = list(
            zstat = results$zstat[i],
            p = results$p[i],
            padjust = results$padjust[i],
            delta = results$delta[i],
            es = results$es[i]
          ))
        }
      },
      
      # 플롯 렌더링 함수들 - 레이지 로딩 적용
      .plot = function(image, ...) {
        if (is.null(image$state))
          return(FALSE)
        
        # 필요할 때만 플롯 생성
        plot <- plot(image$state)
        print(plot)
        TRUE
      },
      
      .plot3 = function(image, ...) {
        if (is.null(image$state))
          return()
        
        # 필요할 때만 플롯 생성
        num <- self$options$num
        plot3 <- ShinyItemAnalysis::plotDIFirt(parameters = image$state,
                                               item = num,
                                               test = "Raju")
        print(plot3)
        TRUE
      },
      
      .plot1 = function(image1, ...) {
        if (is.null(image1$state))
          return(FALSE)
        
        # 필요할 때만 플롯 생성
        plot1 <- plot(image1$state)
        print(plot1)
        TRUE
      },
      
      .plot2 = function(image2, ...) {
        if (is.null(image2$state))
          return(FALSE)
        
        # 필요할 때만 플롯 생성
        plot2 <- plot(image2$state)
        print(plot2)
        TRUE
      }
    )
  )

# Define server logic

#' A function
#'
#' @param input,output Internal parameters for {shiny}.
#'
#' @return
#' @import shiny
#' @import readxl
#' @importFrom utils tail
#' @importFrom Metrics rmse
#' @importFrom Metrics mape
#' @importFrom plotly renderPlotly
#' @export

server <- function(input, output){

  output$res <- renderText({
    req(input$sidebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
  })

  #' GM (1, 1) model
  #' Input original data from excel
  dat <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    req(input$file1)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- c(dat1)
    dat3 <- as.numeric(unlist(dat2))

  })

  #' Input original data from excel - Improved Background Values
  datbv <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    req(input$file2)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- c(dat1)
    dat3 <- as.numeric(unlist(dat2))

  })


  #' Input original data from excel - Extended Forms
  datef <- reactive({
    inFile <- input$file3
    if (is.null(inFile))
      return(NULL)
    req(input$file3)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- c(dat1)
    dat3 <- as.numeric(unlist(dat2))

  })

  #' Input original data from excel - Extended Forms - In-sample
  datinef <- reactive({
    inFile <- input$file31
    if (is.null(inFile))
      return(NULL)
    req(input$file31)
    dat1 <- read_excel(inFile$datapath, 1)

    if (ncol(dat1) == 2 ) {
      dat2 <- data.frame(dat1)
    } else {
      dat2 <- c(dat1)
      dat3 <- as.numeric(unlist(dat2))
    }

  })

  #' Input original data from excel - Extended Forms - Out-sample
  datoutef <- reactive({
    inFile <- input$file311
    if (is.null(inFile))
      return(NULL)
    req(input$file311)
    dat1 <- read_excel(inFile$datapath, 1)

    if (ncol(dat1) == 2 ) {
      dat2 <- data.frame(dat1)
    } else {
      dat2 <- c(dat1)
      dat3 <- as.numeric(unlist(dat2))
    }

  })

  #' Input original data from excel - Combined Models
  datcm <- reactive({
    inFile <- input$file4
    if (is.null(inFile))
      return(NULL)
    req(input$file4)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- c(dat1)
    dat3 <- as.numeric(unlist(dat2))

  })

  #' Input original data from excel - Parameters Estimation
  datpe <- reactive({
    inFile <- input$file5
    if (is.null(inFile))
      return(NULL)
    req(input$file5)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- c(dat1)
    dat3 <- as.numeric(unlist(dat2))

  })

  #' Input original data from excel - Optimization
  datopt <- reactive({
    inFile <- input$file6
    if (is.null(inFile))
      return(NULL)
    req(input$file6)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- c(dat1)
    dat3 <- as.numeric(unlist(dat2))

  })

  #' Input original data from excel - Residual Modification - In-sample
  datrm <- reactive({
    inFile <- input$file7
    if (is.null(inFile))
      return(NULL)
    req(input$file7)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- c(dat1)
    dat3 <- as.numeric(unlist(dat2))

  })

  #' Input original data from excel - Residual Modification - Out-sample
  datoutrm <- reactive({
    inFile <- input$file71
    if (is.null(inFile))
      return(NULL)
    req(input$file71)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- c(dat1)
    dat3 <- as.numeric(unlist(dat2))

  })

  #' Input original data from excel - Multivariate Grey Models
  datmv <- reactive({
    inFile <- input$file8
    if (is.null(inFile))
      return(NULL)
    req(input$file8)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- data.frame(dat1)

  })


  #' Input original data from excel - Multivariate Grey Models - in sample
  datinmv <- reactive({
    inFile <- input$file81
    if (is.null(inFile))
      return(NULL)
    req(input$file81)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- data.frame(dat1)

  })


  #' Input original data from excel - Multivariate Grey Models - out sample
  datoutmv <- reactive({
    inFile <- input$file811
    if (is.null(inFile))
      return(NULL)
    req(input$file811)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- data.frame(dat1)

  })


  #' Input original data from excel - Interval Multivariate Grey Models
  datimv <- reactive({
    inFile <- input$file9
    if (is.null(inFile))
      return(NULL)
    req(input$file9)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- data.frame(dat1)

  })

  #' Input original data from excel - Interval Multivariate Grey Models - in sample
  datinimv <- reactive({
    inFile <- input$file91
    if (is.null(inFile))
      return(NULL)
    req(input$file91)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- data.frame(dat1)

  })



  #' Input original data from excel - Interval Multivariate Grey Models - out sample
  datoutimv <- reactive({
    inFile <- input$file911
    if (is.null(inFile))
      return(NULL)
    req(input$file911)

    dat1 <- read_excel(inFile$datapath, 1)
    dat2 <- data.frame(dat1)

  })


  #' GM (1,1) model Fitted - reactive
  #' Fitted values
  x0capgm11 <- reactive({

    x0 <- dat()
    source("bv.R")
    gm11(x0)

  })

  #' GM (1,1) model Actual and Fitted - render
  #' original and fitted data
  output$fittedgm <- renderTable({

    Actual <- dat()
    actual2 <- t(Actual)
    n <- length(actual2)
    fitted1 <- x0capgm11()
    fitted2 <- t(fitted1)
    Fitted <- fitted2[1:n]
    dfgm11 <- data.frame(Actual,Fitted)

  })

  #' GM (1,1) model predicted data  - render
  output$predictedgm <- renderTable({

    output$gmpv <- renderPrint({input$radiogm})

    actual1 <- dat()
    actual2 <- t(dat())
    n <- length(actual2)
    fitted1 <- x0capgm11()
    x <- input$radiogm
    fitted3 <- tail(fitted1,4)
    predicted <- t(fitted3[1:x])

  })


  #' GM (1,1) model- render
  #' MAPE and RMSE
  output$errorgm <- renderTable({

    Actual <- dat()
    actual2 <- t(Actual)
    n <- length(actual2)
    fitted1 <- x0capgm11()
    fitted2 <- t(fitted1)
    Fitted <- fitted2[1:n]
    gm11 <- data.frame(Actual,Fitted)

    MAPE <- mape(Actual,Fitted)*100
    RMSE <- rmse(Actual,Fitted)
    pe <- data.frame(MAPE,RMSE)

  })


  #' CI for GM (1, 1) model - reactive
  output$intervalsgm <- renderTable({

    if (input$radiocigm == "90") {

      output$ci1 <- renderPrint({input$radiocigm})

      fp1 <- x0capgm11()
      actual1 <- dat()
      x <- input$radiogm
      ci <- 90

      source("confidence_interval.R")
      CIvalue(fp1,actual1,x,ci)




    } else if (input$radiocigm == "95") {

      output$ci1 <- renderPrint({input$radiocigm})

      fp1 <- x0capgm11()
      actual1 <- dat()
      x <- input$radiogm
      ci <- 95

      source("confidence_interval.R")
      CIvalue(fp1,actual1,x,ci)

    } else if (input$radiocigm == "99") {

      output$ci1 <- renderPrint({input$radiocigm})

      fp1 <- x0capgm11()
      actual1 <- dat()
      x <- input$radiogm
      ci <- 99

      source("confidence_interval.R")
      CIvalue(fp1,actual1,x,ci)
    }

  })

  #' GM (1,1) model - render
  #' Plots for GM (1, 1) model
  output$plotgm <- renderPlotly({

    if (input$radiocigm == "90") {

      x0 <- dat()
      x0cap2 <- x0capgm11()
      ci <- 90
      model <- 'GM (1, 1) model'

      source("plots.R")
      plots(x0,x0cap2,ci,model)


    } else if (input$radiocigm == "95") {

      x0 <- dat()
      x0cap2 <- x0capgm11()
      ci <- 95
      model <- 'GM (1, 1) model'

      source("plots.R")
      plots(x0,x0cap2,ci,model)


    } else if (input$radiocigm == "99") {

      x0 <- dat()
      x0cap2 <- x0capgm11()
      ci <- 99
      model <- 'GM (1, 1) model'

      source("plots.R")
      plots(x0,x0cap2,ci,model)

    }

  })

  #' Improved Background Values Models
  #' EPGM (1,1) model Fitted - reactive
  #' Fitted values
  x0capepgm <- reactive({

    x0 <- datbv()
    source("bv.R")
    epgm11(x0)


  })

  #' TBGM (1,1) model Fitted - reactive
  #' Fitted values
  x0captbgm <- reactive({

    x0 <- datbv()
    source("bv.R")
    tbgm11(x0)

  })

  #' IGM (1,1) model Fitted - reactive
  #' Fitted values
  x0capigm <- reactive({

    x0 <- datbv()
    source("bv.R")
    igm11(x0)

  })

  #' GM (1,1,4) model Fitted - reactive
  #' Fitted values
  x0capgm114 <- reactive({

    x0 <- datbv()
    source("bv.R")
    gm114(x0)

  })

  #' Improved Background Values model Actual and Fitted - render
  #' original and fitted data
  output$x0capbv <- renderTable({
    if (input$radiobv == "EPGM (1, 1) model") {

      output$bv <- renderText({
        paste("EPGM (1, 1) model: Extrapolation-based grey model",
              "The EPGM (1, 1) function takes as input raw values and output predicted values following the paper by",
              "Chang C (2019). Extrapolation-based Grey Model for Small-Dataset Forecasting.",
              "Economic Computation and Economic Cybernetics Studies and Research, 53(1), 171-182.",
              "DOI:10.24818/18423264/53.1.19.11",
              sep = "\n")
      })

      Actual <- datbv()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capepgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      epgm11 <- data.frame(Actual,Fitted)

    } else if (input$radiobv == "TBGM (1, 1) model") {

      output$bv <- renderText({
        paste("TBGM (1, 1) model: Data transformation-based grey model",
              "The TBGM (1, 1) function takes as input raw values and output predicted values following the paper by",
              "Li K, Zhang T (2019). A Novel Grey Forecasting Model and its Application in Forecasting the",
              "Energy Consumption in Shanghai. Energy Systems, pp. 1-16.",
              "DOI:10.1007/s12667-019-00344-0",
              sep = "\n")
      })

      Actual <- datbv()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0captbgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      tbgm11 <- data.frame(Actual,Fitted)

    } else if (input$radiobv == "IGM (1, 1) model") {

      output$bv <- renderText({
        paste("IGM (1, 1) model: Improved grey model",
              "The IGM (1, 1) function takes as input raw values and output predicted values following the paper by",
              "Ou S (2012). Forecasting Agricultural Output with an Improved Grey Forecasting Model based on",
              "the Genetic Algorithm. Computers and Electronics in Agriculture, 85, 33-39.",
              "DOI:10.1016/j.compag.2012.03.007",
              sep = "\n")
      })

      Actual <- datbv()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capigm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      igm11 <- data.frame(Actual,Fitted)

    } else if (input$radiobv == "GM (1, 1, 4) model") {

      output$bv <- renderText({
        paste("GM (1, 1, 4) model: Grey model with single variable, one first-order variable and four background values",
              "The GM (1, 1, 4) function takes as input raw values and output predicted values following the paper by",
              "Li S, Zhou M, Meng W, Zhou W (2019). A new Prediction Model for Forecasting the Automobiles",
              "Ownership in China. Journal of Control and Decision, 8(2), 155-164.",
              "DOI:10.1080/23307706.2019.1666310",
              sep = "\n")
      })

      Actual <- datbv()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capgm114()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      gm114 <- data.frame(Actual,Fitted)

    }
  })


  #' Predicted values - render
  #' Improved Background Value Models
  output$predictedbv <- renderTable({

    if (input$radiobv == "EPGM (1, 1) model") {

      output$bvpv <- renderPrint({input$radiobv1})

      actual1 <- datbv()
      actual2 <- t(datbv())
      n <- length(actual2)
      fitted1 <- x0capepgm()
      x <- input$radiobv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiobv == "TBGM (1, 1) model") {

      output$bvpv <- renderPrint({input$radiobv1})

      actual1 <- datbv()
      actual2 <- t(datbv())
      n <- length(actual2)
      fitted1 <- x0captbgm()
      x <- input$radiobv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiobv == "IGM (1, 1) model") {

      output$bvpv <- renderPrint({input$radiobv1})

      actual1 <- datbv()
      actual2 <- t(datbv())
      n <- length(actual2)
      fitted1 <- x0capigm()
      x <- input$radiobv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiobv == "GM (1, 1, 4) model") {

      output$bvpv <- renderPrint({input$radiobv1})

      actual1 <- datbv()
      actual2 <- t(datbv())
      n <- length(actual2)
      fitted1 <- x0capgm114()
      x <- input$radiobv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])
    }
  })


  #' Improved Background Values - Error - render
  #' MAPE and RMSE
  output$errorbv <- renderTable({

    if (input$radiobv == "EPGM (1, 1) model") {

      Actual <- datbv()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capepgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiobv == "TBGM (1, 1) model") {

      Actual <- datbv()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0captbgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiobv == "IGM (1, 1) model") {

      Actual <- datbv()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capigm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiobv == "GM (1, 1, 4) model") {

      Actual <- datbv()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capgm114()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)
    }

  })


  #' Improved Background Values - Prediction Intervals - render
  #' CI  - reactive
  output$intervalsbv <- renderTable({

    if (input$radiobv == "EPGM (1, 1) model") {

      if (input$radiobv2 == "90") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capepgm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)



      } else if (input$radiobv2 == "95") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capepgm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radiobv2 == "99") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capepgm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }

    } else if (input$radiobv == "TBGM (1, 1) model") {

      if (input$radiobv2 == "90") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0captbgm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiobv2 == "95") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0captbgm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radiobv2 == "99") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0captbgm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }

    } else if (input$radiobv == "IGM (1, 1) model") {

      if (input$radiobv2 == "90") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capigm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiobv2 == "95") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capigm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radiobv2 == "99") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capigm()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }

    } else if (input$radiobv == "GM (1, 1, 4) model") {

      if (input$radiobv2 == "90") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capgm114()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiobv2 == "95") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capgm114()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiobv2 == "99") {

        output$cibv <- renderPrint({input$radiobv2})

        fp1 <- x0capgm114()
        actual1 <- datbv()
        x <- input$radiobv1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      }
    }
  })

  #' Improved Background Values - Plots - render
  #' Tab 2
  output$plotbv <- renderPlotly({

    if (input$radiobv == "EPGM (1, 1) model") {

      if (input$radiobv2 == "90") {

        x0 <- datbv()
        x0cap2 <- x0capepgm()
        ci <- 90
        model <- 'EPGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiobv2 == "95") {

        x0 <- datbv()
        x0cap2 <- x0capepgm()
        ci <- 95
        model <- 'EPGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiobv2 == "99") {

        x0 <- datbv()
        x0cap2 <- x0capepgm()
        ci <- 99
        model <- 'EPGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radiobv == "TBGM (1, 1) model") {

      if (input$radiobv2 == "90") {

        x0 <- datbv()
        x0cap2 <- x0captbgm()

        ci <- 90
        model <- 'TBGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiobv2 == "95") {

        x0 <- datbv()
        x0cap2 <- x0captbgm()

        ci <- 95
        model <- 'TBGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiobv2 == "99") {

        x0 <- datbv()
        x0cap2 <- x0captbgm()

        ci <- 99
        model <- 'TBGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }


    } else if (input$radiobv == "IGM (1, 1) model") {

      if (input$radiobv2 == "90") {

        x0 <- datbv()
        x0cap2 <- x0capigm()

        ci <- 90
        model <- 'IGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiobv2 == "95") {

        x0 <- datbv()
        x0cap2 <- x0capigm()

        ci <- 95
        model <- 'IGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiobv2 == "99") {

        x0 <- datbv()
        x0cap2 <- x0capigm()

        ci <- 99
        model <- 'IGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      }

    } else if (input$radiobv == "GM (1, 1, 4) model") {

      if (input$radiobv2 == "90") {

        x0 <- datbv()
        x0cap2 <- x0capgm114()

        ci <- 90
        model <- 'GM (1, 1, 4) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiobv2 == "95") {

        x0 <- datbv()
        x0cap2 <- x0capgm114()

        ci <- 95
        model <- 'GM (1, 1, 4) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiobv2 == "99") {

        x0 <- datbv()
        x0cap2 <- x0capgm114()

        ci <- 99
        model <- 'GM (1, 1, 4) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }
    }
  })


  #' Extended Forms of Grey Models
  #' DGM (1,1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capdgm11 <- reactive({

    x0 <- datef()
    source("ef.R")
    dgm11(x0)

  })

  #' DGM (2,1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capdgm21 <- reactive({

    x0 <- datef()
    source("ef.R")
    dgm21(x0)

  })


  #' ODGM (2,1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capodgm21 <- reactive({

    x0 <- datef()
    source("ef.R")
    odgm21(x0)

  })


  #' NDGM (1,1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capndgm <- reactive({

    x0 <- datef()
    source("ef.R")
    ndgm11(x0)

  })


  #' EGM (1,1) model Fitted - reactive
  #' Fitted values
  x0capegm <- reactive({

    dfdata1 <- datinef()
    dfdata2 <- data.frame(dfdata1)
    k <- dfdata2[ ,1]
    x0 <- dfdata2[ ,2]

    data_A <- datoutef()
    dfdata_A <- data.frame(data_A)
    k_A <- dfdata_A[ ,1]
    x0_A <- dfdata_A[ ,2]

    source("ef.R")
    egm11(k,x0,k_A,x0_A)

  })


  #' VSSGM (1,1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capvssgm <- reactive({

    x0 <- datef()
    source("ef.R")
    vssgm11(x0)

  })


  #' GOM (1, 1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capgom <- reactive({

    x0 <- datef()
    source("ef.R")
    gom11(x0)

  })

  #' GOM_IA (1, 1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capgomia <- reactive({

    x0 <- datef()
    source("ef.R")
    gomia11(x0)

  })


  #' unbiased GOM (1, 1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capungom <- reactive({

    x0 <- datef()
    source("ef.R")
    ungom11(x0)

  })


  #' EXGM (1, 1) model Fitted - reactive
  #' Fitted values and predicted values
  x0capexgm <- reactive({

    x0 <- datef()
    source("ef.R")
    exgm11(x0)

  })


  #' Extended Forms - Actual and Fitted - render
  #' original and fitted data
  output$x0capef <- renderTable({
    if (input$radioef == "DGM (1, 1)") {

      output$ef <- renderText({
        paste("DGM (1, 1): Discrete grey model with single variable and first order differential equation",
              "The DGM (1, 1) function takes as input raw values and output predicted values following the paper by",
              "Xie N, Liu S (2009). Discrete Grey Forecasting Model and its Application.",
              "Applied Mathematical Modelling, 33(2), 1173-1186.",
              "DOI:10.1016/j.apm.2008.01.011",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capdgm11()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfdgm11 <- data.frame(Actual,Fitted)

    } else if (input$radioef == "DGM (2, 1)") {

      output$ef <- renderText({
        paste("DGM (2, 1): Discrete grey model with single variable and second order differential equation model",
              "The DGM (2, 1) function takes as input raw values and output predicted values following the paper by",
              "Shao Y, Su H (2012). On Approximating Grey Model DGM (2, 1).",
              "2012 AASRI Conference on Computational Intelligence and Bioinformatics, 1, 8-13.",
              "DOI:10.1016/j.aasri.2012.06.003",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capdgm21()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfdgm21 <- data.frame(Actual,Fitted)

    } else if (input$radioef == "ODGM (2, 1)") {

      output$ef <- renderText({
        paste("ODGM (2, 1): Optimized discrete grey model with single variable and second order differential equation",
              "The ODGM (2, 1) function takes as input raw values and output predicted values following the paper by",
              "Shao Y, Su H (2012). On Approximating Grey Model DGM (2, 1).",
              "2012 AASRI Conference on Computational Intelligence and Bioinformatics, 1, 8-13.",
              "DOI:10.1016/j.aasri.2012.06.003",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capodgm21()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfodgm21 <- data.frame(Actual,Fitted)

    } else if (input$radioef == "NDGM (1, 1)") {

      output$ef <- renderText({
        paste("NDGM (1, 1): Non-homogeneous discrete grey model",
              "The NDGM function takes as input raw values and output predicted values following the paper by",
              "Xie N, Liu S, Yang Y, Yuan C (2013). On Novel Grey Forecasting Model based on Non-homogeneous",
              "Index Sequence. Applied Mathematical Modelling, 37, 5059-5068.",
              "DOI:10.1016/j.apm.2012.10.037",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capndgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfndgm <- data.frame(Actual,Fitted)


    }else if (input$radioef == "VSSGM (1, 1)") {

      output$ef <- renderText({
        paste("VSSGM (1, 1): Variable speed and adaptive structure-based grey model",
              "The VSSGM (1, 1) function takes as input raw values and output predicted values following the paper by",
              "Li S, Miao Y, Li G, Ikram M (2020). A Novel Varistructure Grey Forecasting Model with Speed",
              "Adaptation and its Application. Mathematical and Computers in Simulation, 172, 45-70.",
              "DOI:10.1016/j.matcom.2019.12.020",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capvssgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfvssgm <- data.frame(Actual,Fitted)

    } else if (input$radioef == "GOM (1, 1)") {

      output$ef <- renderText({
        paste("GOM (1, 1): Grey opposite-direction model based on inverse accumulation and traditional interpolation method",
              "The GOM function takes as input raw values and output predicted values following the paper by",
              "Che X, Luo Y, He Z (2013). Grey New Information GOM (1, 1) Model based Opposite-Direction",
              "Accumulated Generating and its Application. Applied Mechanics and Materials, 364, 207-210.",
              "DOI:10.4028/www.scientific.net/AMM.364.207",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capgom()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfgom <- data.frame(Actual,Fitted)

    } else if (input$radioef == "GOM_IA (1, 1)") {

      output$ef <- renderText({
        paste("GOM_IA (1, 1): Grey opposite-direction model based on inverse accumulation",
              "The GOM_IA function takes as input raw values and output predicted values following the paper by",
              "Zhu J, Xu Y, Leng H, Tang H, Gong H, Zhang Z (2016).",
              "Power Load Forecasting based on GOM (1, 1) Model under the Condition of Missing Data.",
              "2016 IEEEPES Asia-Pacific Power and Energy Engineering Conference (APPEEC), pp. 2461-2464.",
              "DOI:10.1109/appeec.2016.7779929",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capgomia()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfgomia <- data.frame(Actual,Fitted)

    } else if (input$radioef == "unbiased GOM (1, 1)") {

      output$ef <- renderText({
        paste("unbiased GOM (1, 1): Unbiased grey opposite-direction model based on inverse accumulation",
              "The unbiased GOM function takes as input raw values and output predicted values following the paper by",
              "Luo Y, Liao D (2012). Grey New Information Unbiased GOM (1, 1) Model based on Opposite-Direction",
              "Accumulated Generating and its Application. Advanced Materials Research, 507, 265-268.",
              "DOI:10.4028/www.scientific.net/AMR.507.265",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capungom()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfungom <- data.frame(Actual,Fitted)


    } else if (input$radioef == "EXGM (1, 1)") {

      output$ef <- renderText({
        paste("EXGM (1, 1): Exponential grey model",
              "The EXGM (1, 1) function takes as input raw values and output predicted values following the paper by",
              "Bilgil H (2020). New Grey Forecasting Model with its Application and Computer Code.",
              "AIMS Mathematics, 6(2), 1497-1514",
              "DOI: 10.3934/math.2021091",
              sep = "\n")
      })

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capexgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfexgm <- data.frame(Actual,Fitted)
    }
  })


  #' Predicted values - render
  output$predictedef <- renderTable({

    if (input$radioef == "DGM (1, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capdgm11()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioef == "DGM (2, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capdgm21()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioef == "ODGM (2, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capodgm21()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioef == "NDGM (1, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capndgm()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioef == "VSSGM (1, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capvssgm()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioef == "GOM (1, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capgom()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioef == "GOM_IA (1, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capgomia()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioef == "unbiased GOM (1, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capungom()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioef == "EXGM (1, 1)") {

      actual1 <- datef()
      actual2 <- t(datef())
      n <- length(actual2)
      fitted1 <- x0capexgm()
      x <- input$radioef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])
    }
  })


  #' EGM - In & Out sample - Acutal & Fitted
  output$x0capoutef<- renderTable({

    if (input$radiooutef == "EGM (1, 1) model") {

      output$outef <- renderText({
        paste("EGM (1, 1): Extended grey model",
              "The EGM function takes as input raw values and output predicted values following the paper by",
              "Li D, Chang C, Chen W, Chen C (2011).",
              "An Extended Grey Forecasting Model for Omnidirectional Forecasting considering Data Gap Difference.",
              "Applied Mathematical Modeling, 35, 5051-5058.",
              "DOI:10.1016/j.apm.2011.04.006",
              sep = "\n")
      })

      actual1 <- datinef()
      dfdata2 <- data.frame(actual1)
      k  <- dfdata2[ ,1]
      x0 <- dfdata2[ ,2]
      n <- length(x0)
      fitted1 <- x0capegm()
      fitted <- fitted1[1:n]
      dfegm <- data.frame(x0,fitted)
      colnames(dfegm , do.NULL = FALSE)
      colnames(dfegm) <- c("Original","Fitted")
      dfegm


    }
  })

  #' Predicted values - render  - EGM
  output$predictedoutef <- renderTable({

    if (input$radiooutef == "EGM (1, 1) model") {

      actual1 <- datinef()
      dfdata2 <- data.frame(actual1)
      k  <- dfdata2[ ,1]
      x0 <- dfdata2[ ,2]
      n <- length(x0)
      fitted1 <- x0capegm()
      x <- input$radiooutef1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    }
  })


  #' Extended Forms of GM - Error - render
  #' MAPE and RMSE
  output$erroref <- renderTable({

    if (input$radioef == "DGM (1, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capdgm11()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioef == "DGM (2, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capdgm21()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioef == "ODGM (2, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capodgm21()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioef == "NDGM (1, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capndgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioef == "VSSGM (1, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capvssgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioef == "GOM (1, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capgom()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioef == "GOM_IA (1, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capgomia()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioef == "unbiased GOM (1, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capungom()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)


    }else if (input$radioef == "EXGM (1, 1)") {

      Actual <- datef()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capexgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    }
  })


  #' Extended Forms of GM - Error - render EGM
  #' MAPE and RMSE
  output$erroroutef <- renderTable({

    if (input$radiooutef == "EGM (1, 1) model") {

      actual1 <- datinef()
      dfdata2 <- data.frame(actual1)
      k  <- dfdata2[ ,1]
      x0 <- dfdata2[ ,2]
      n <- length(x0)
      fitted1 <- x0capegm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(x0,Fitted)*100
      RMSE <- rmse(x0,Fitted)
      pe <- data.frame(MAPE,RMSE)

    }
  })


  #' Extended Forms of Grey Models - Prediction Intervals - render - EGM
  #' CI  - reactive
  output$intervalsoutef <- renderTable({

    if (input$radiooutef == "EGM (1, 1) model") {

      if (input$radiooutef2 == "90") {

        output$cioutef <- renderPrint({input$radiooutef2})

        fp1 <- x0capegm()
        actual1 <- datinef()
        dfdata2 <- data.frame(actual1)
        k  <- dfdata2[ ,1]
        actual2 <- dfdata2[ ,2]
        ci <- 90
        x <- input$radiooutef1

        source("CIplots_egm.R")
        CIvalue(fp1,actual2,x,ci)


      } else if (input$radiooutef2 == "95") {

        output$cioutef <- renderPrint({input$radiooutef2})

        fp1 <- x0capegm()
        actual1 <- datinef()
        dfdata2 <- data.frame(actual1)
        k  <- dfdata2[ ,1]
        actual2 <- dfdata2[ ,2]
        ci <- 95
        x <- input$radiooutef1

        source("CIplots_egm.R")
        CIvalue(fp1,actual2,x,ci)


      } else if (input$radiooutef2 == "99") {

        output$cioutef <- renderPrint({input$radiooutef2})

        fp1 <- x0capegm()
        actual1 <- datinef()
        dfdata2 <- data.frame(actual1)
        k  <- dfdata2[ ,1]
        actual2 <- dfdata2[ ,2]
        ci <- 99
        x <- input$radiooutef1

        source("CIplots_egm.R")
        CIvalue(fp1,actual2,x,ci)

      }
    }
  })

  #' Extended Forms of Grey Models - Plots - render _ EGM
  #' CI  - reactive
  output$plotoutef <- renderPlotly({

    if (input$radiooutef == "EGM (1, 1) model") {

      if (input$radiooutef2 == "90") {

        x0cap2 <- x0capegm()
        actual1 <- datinef()
        dfdata2 <- data.frame(actual1)
        k  <- dfdata2[ ,1]
        x0 <- dfdata2[ ,2]
        ci <- 90

        source("CIplots_egm.R")
        plotegm(x0,x0cap2,ci)


      } else if (input$radiooutef2 == "95") {

        x0cap2 <- x0capegm()
        actual1 <- datinef()
        dfdata2 <- data.frame(actual1)
        k  <- dfdata2[ ,1]
        x0 <- dfdata2[ ,2]
        ci <- 95

        source("CIplots_egm.R")
        plotegm(x0,x0cap2,ci)

      } else if (input$radiooutef2 == "99") {

        x0cap2 <- x0capegm()
        actual1 <- datinef()
        dfdata2 <- data.frame(actual1)
        k  <- dfdata2[ ,1]
        x0 <- dfdata2[ ,2]
        ci <- 99

        source("CIplots_egm.R")
        plotegm(x0,x0cap2,ci)


      }
    }
  })



  #' Extended Forms of Grey Models - Prediction Intervals - render
  #' CI  - reactive
  output$intervalsef <- renderTable({

    if (input$radioef == "DGM (1, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capdgm11()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capdgm11()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capdgm11()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radioef == "DGM (2, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capdgm21()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capdgm21()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capdgm21()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radioef == "ODGM (2, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capodgm21()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capodgm21()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capodgm21()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radioef == "NDGM (1, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capndgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)



      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capndgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capndgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radioef == "VSSGM (1, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capvssgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capvssgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capvssgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radioef == "GOM (1, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capgom()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capgom()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capgom()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radioef == "GOM_IA (1, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capgomia()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capgomia()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capgomia()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radioef == "unbiased GOM (1, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capungom()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capungom()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capungom()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radioef == "EXGM (1, 1)") {

      if (input$radioef2 == "90") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capexgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radioef2 == "95") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capexgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radioef2 == "99") {

        output$cief <- renderPrint({input$radioef2})

        fp1 <- x0capexgm()
        actual1 <- datef()
        x <- input$radioef1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }
    }
  })



  #' Extended Forms of Grey Models - Plots - render
  #' CI  - reactive
  output$plotef <- renderPlotly({

    if (input$radioef == "DGM (1, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capdgm11()
        ci <- 90
        model <- 'DGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capdgm11()
        ci <- 95
        model <- 'DGM (1, 1) model '

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capdgm11()
        ci <- 99
        model <- 'DGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radioef == "DGM (2, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capdgm21()
        ci <- 90
        model <- 'DGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capdgm21()
        ci <- 95
        model <- 'DGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capdgm21()
        ci <- 99
        model <- 'DGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }
    } else if (input$radioef == "ODGM (2, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capodgm21()
        ci <- 90
        model <- 'ODGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capodgm21()
        ci <- 95
        model <- 'ODGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capodgm21()
        ci <- 99
        model <- 'ODGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      }

    } else if (input$radioef == "NDGM (1, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capndgm()
        ci <- 90
        model <- 'NDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capndgm()
        ci <- 95
        model <- 'NDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capndgm()
        ci <- 99
        model <- 'NDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)
      }


    } else if (input$radioef == "VSSGM (1, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capvssgm()
        ci <- 90
        model <- 'VSSGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capvssgm()
        ci <- 95
        model <- 'VSSGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capvssgm()
        ci <- 99
        model <- 'VSSGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }


    } else if (input$radioef == "GOM (1, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capgom()
        ci <- 90
        model <- 'GOM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capgom()
        ci <- 95
        model <- 'GOM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capgom()
        ci <- 99
        model <- 'GOM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      }
    } else if (input$radioef == "GOM_IA (1, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capgomia()
        ci <- 90
        model <- 'GOM_IA (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capgomia()
        ci <- 95
        model <- 'GOM_IA (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capgomia()
        ci <- 99
        model <- 'GOM_IA (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }


    } else if (input$radioef == "unbiased GOM (1, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capungom()
        ci <- 90
        model <- 'unbiased GOM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capungom()
        ci <- 95
        model <- 'unbiased GOM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capungom()
        ci <- 99
        model <- 'unbiased GOM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radioef == "EXGM (1, 1)") {

      if (input$radioef2 == "90") {

        x0 <- datef()
        x0cap2 <- x0capexgm()
        ci <- 90
        model <- 'EXGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "95") {

        x0 <- datef()
        x0cap2 <- x0capexgm()
        ci <- 95
        model <- 'EXGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioef2 == "99") {

        x0 <- datef()
        x0cap2 <- x0capexgm()
        ci <- 99
        model <- 'EXGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      }
    }
  })


  #' Combined models

  #' NGBM (1, 1) model Fitted - reactive
  #' Fitted values
  x0capngbm <- reactive({

    x0 <- datcm()
    source("cm.R")
    ngbm11(x0)

  })

  #' GGVM (1, 1) model Fitted - reactive
  #' Fitted values
  x0capggvm <- reactive({

    x0 <- datcm()
    source("cm.R")
    ggvm11(x0)

  })

  #' TFDGM(1, 1) model Fitted - reactive
  #' Fitted values
  x0captfdgm <- reactive({

    x0 <- datcm()
    source("cm.R")
    tfdgm11(x0)

  })


  #' Combined Models - Actual and Fitted - render
  #' original and fitted data
  output$x0capcm <- renderTable({
    if (input$radiocm == "NGBM (1, 1) model") {

      output$cm <- renderText({
        paste("NGBM (1, 1) model: Non-linear grey Bernoulli model",
              "The NGBM (1, 1) model function takes as input raw values and output predicted values following the paper by",
              "Chen C (2008). Application of the Novel Nonlinear Grey Bernoulli Model for Forecasting Unemployment Rate.",
              "Chaos, Solitons and Fractals, 37(1), 278-287.",
              "DOI:10.1016/j.chaos.2006.08.024",
              sep = "\n")
      })

      Actual <- datcm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capngbm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      epgm11 <- data.frame(Actual,Fitted)

    } else if (input$radiocm == "GGVM (1, 1) model") {

      output$cm <- renderText({
        paste("GGVM (1, 1) model: Grey generalized Verhulst model",
              "The GGVM (1, 1) model function takes as input raw values and output predicted values following the paper by",
              "Zhou W, Pei L (2020). The Grey Generalized Verhulst model and its Application for Forecasting Chinese Pig",
              "Price Index. Soft Computing, 24, 4977-4990.",
              "DOI:10.1007/s00500-019-04248-0",
              sep = "\n")
      })

      Actual <- datcm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capggvm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      epgm11 <- data.frame(Actual,Fitted)

    } else if (input$radiocm == "TFDGM (1, 1) model") {

      output$cm <- renderText({
        paste("TFDGM (1, 1) model: Traffic flow mechanics grey model",
              "The TFDGM (1, 1) model function takes as input raw values and output predicted values following the paper by",
              "Xiao X, Duan H (2020). A New Grey Model for Traffic Fow Mechanisms. Engineering Applications of Artificial",
              "Intelligence, 88(2020), 103350.",
              "DOI:10.1016/j.engappai.2019.103350",
              sep = "\n")
      })

      Actual <- datcm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0captfdgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      epgm11 <- data.frame(Actual,Fitted)

    }
  })

  #' Predicted values - render
  #' Combined Models
  output$predictedcm <- renderTable({

    if (input$radiocm == "NGBM (1, 1) model") {

      output$cmpv <- renderPrint({input$radiocm1})

      actual1 <- datcm()
      actual2 <- t(datcm())
      n <- length(actual2)
      fitted1 <- x0capngbm()
      x <- input$radiocm1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radiocm == "GGVM (1, 1) model") {

      output$cmpv <- renderPrint({input$radiocm1})

      actual1 <- datcm()
      actual2 <- t(datcm())
      n <- length(actual2)
      fitted1 <- x0capggvm()
      x <- input$radiocm1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radiocm == "TFDGM (1, 1) model") {

      output$cmpv <- renderPrint({input$radiocm1})

      actual1 <- datcm()
      actual2 <- t(datcm())
      n <- length(actual2)
      fitted1 <- x0captfdgm()
      x <- input$radiocm1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    }
  })

  #' Combined Models - Error - render
  #' MAPE and RMSE
  output$errorcm <- renderTable({

    if (input$radiocm == "NGBM (1, 1) model") {

      Actual <- datcm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capngbm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiocm == "GGVM (1, 1) model") {

      Actual <- datcm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capggvm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiocm == "TFDGM (1, 1) model") {

      Actual <- datcm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0captfdgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    }
  })


  #' Combined Models - Prediction Intervals - render
  #' CI  - reactive
  output$intervalscm <- renderTable({

    if (input$radiocm == "NGBM (1, 1) model") {

      if (input$radiocm2 == "90") {

        output$cicm <- renderPrint({input$radiocm2})

        fp1 <- x0capngbm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiocm2 == "95") {

        output$cicm <- renderPrint({input$radiocm2})

        fp1 <- x0capngbm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radiocm2 == "99") {

        output$cicm<- renderPrint({input$radiocm2})

        fp1 <- x0capngbm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radiocm == "GGVM (1, 1) model") {

      if (input$radiocm2 == "90") {

        output$cicm <- renderPrint({input$radiocm2})

        fp1 <- x0capggvm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radiocm2 == "95") {

        output$cicm <- renderPrint({input$radiocm2})

        fp1 <- x0capggvm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radiocm2 == "99") {

        output$cicm<- renderPrint({input$radiocm2})

        fp1 <- x0capggvm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }


    } else if (input$radiocm == "TFDGM (1, 1) model") {

      if (input$radiocm2 == "90") {

        output$cicm <- renderPrint({input$radiocm2})

        fp1 <- x0captfdgm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiocm2 == "95") {

        output$cicm <- renderPrint({input$radiocm2})

        fp1 <- x0captfdgm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radiocm2 == "99") {

        output$cicm<- renderPrint({input$radiocm2})

        fp1 <- x0captfdgm()
        actual1 <- datcm()
        x <- input$radiocm1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }
    }
  })



  #' Combined Models - Plots - render
  output$plotcm <- renderPlotly({

    if (input$radiocm == "NGBM (1, 1) model") {

      if (input$radiocm2 == "90") {

        x0 <- datcm()
        x0cap2 <- x0capngbm()
        ci <- 90
        model <- 'NGBM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiocm2 == "95") {

        x0 <- datcm()
        x0cap2 <- x0capngbm()
        ci <- 95
        model <- 'NGBM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiocm2 == "99") {

        x0 <- datcm()
        x0cap2 <- x0capngbm()
        ci <- 99
        model <- 'NGBM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radiocm == "GGVM (1, 1) model") {

      if (input$radiocm2 == "90") {

        x0 <- datcm()
        x0cap2 <- x0capggvm()
        ci <- 90
        model <- 'GGVM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiocm2 == "95") {

        x0 <- datcm()
        x0cap2 <- x0capggvm()
        ci <- 95
        model <- 'GGVM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiocm2 == "99") {

        x0 <- datcm()
        x0cap2 <- x0capggvm()
        ci <- 99
        model <- 'GGVM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radiocm == "TFDGM (1, 1) model") {

      if (input$radiocm2 == "90") {

        x0 <- datcm()
        x0cap2 <- x0captfdgm()
        ci <- 90
        model <- 'TFDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiocm2 == "95") {

        x0 <- datcm()
        x0cap2 <- x0captfdgm()
        ci <- 95
        model <- 'TFDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiocm2 == "99") {

        x0 <- datcm()
        x0cap2 <- x0captfdgm()
        ci <- 99
        model <- 'TFDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }
    }
  })


  #' Parameters Estimation
  #' SOGM (2,1) model Fitted - reactive
  #' Fitted values
  x0capsogm21 <- reactive({

    x0 <- datpe()
    source("pe.R")
    sogm21(x0)

  })


  #' NGM (1,1,k) model Fitted - reactive
  #' Fitted values
  x0capngm11k <- reactive({

    x0 <- datpe()
    source("pe.R")
    ngm11k(x0)

  })


  #' NGM (1,1,k,c) model Fitted - reactive
  #' Fitted values
  x0capngm11kc <- reactive({

    x0 <- datpe()
    source("pe.R")
    ngm11kc(x0)

  })

  #' ONGM (1,1,k,c) model Fitted - reactive
  #' Fitted values
  x0capongm11kc <- reactive({

    x0 <- datpe()
    source("pe.R")
    ongm11kc(x0)


  })


  #' Parameters Estimations - Actual and Fitted - render
  #' original and fitted data

  output$x0cappe <- renderTable({
    if (input$radiope == "SOGM (2, 1) model") {

      output$pe <- renderText({
        paste("SOGM (2, 1) model: Structured optimized grey model with single variable and second order differential equation",
              "The SOGM (2, 1) function takes as input raw values and output predicted values following the paper by",
              "Xu N, Dang Y (2015). An Optimized Grey GM (2, 1) Model and Forecasting of Highway Subgrade",
              "Settlement. Mathematical Problems in Engineering, 2015(1), 1-6.",
              "DOI:10.1155/2015/606707",
              sep = "\n")
      })

      Actual <- datpe()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capsogm21()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      sogm21 <- data.frame(Actual,Fitted)

    } else if (input$radiope == "NGM (1, 1, k) model") {

      output$pe <- renderText({
        paste("NGM (1, 1, k) model: Nonlinear grey model",
              "The NGM (1, 1, k) function takes as input raw values and output predicted values following the paper by",
              "Chen P, Yu H (2014). Foundation Settlement Prediction based on a Novel NGM Model. Mathematical",
              "Problems in Engineering 2014, 242809.",
              "DOI:10.1155/2014/242809",
              sep = "\n")
      })

      Actual <- datpe()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capngm11k()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      sogm21 <- data.frame(Actual,Fitted)

    } else if (input$radiope == "NGM (1, 1, k, c) model") {

      output$pe <- renderText({
        paste("NGM (1, 1, k, c) model: Nonlinear grey model",
              "The NGM (1, 1, k, c) function takes as input raw values and output predicted values following the paper by",
              "Chen P, Yu H (2014). Foundation Settlement Prediction based on a Novel NGM Model. Mathematical",
              "Problems in Engineering 2014, 242809.",
              "DOI:10.1155/2014/242809",
              sep = "\n")
      })

      Actual <- datpe()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capngm11kc()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      sogm21 <- data.frame(Actual,Fitted)

    } else if (input$radiope == "ONGM (1, 1, k, c) model") {

      output$pe <- renderText({
        paste("ONGM (1, 1, k, c) model: Nonlinear grey model",
              "The ONGM (1, 1, k, c) function takes as input raw values and output predicted values following the paper by",
              "Chen P, Yu H (2014). Foundation Settlement Prediction based on a Novel NGM Model. Mathematical",
              "Problems in Engineering 2014, 242809.",
              "DOI:10.1155/2014/242809",
              sep = "\n")
      })

      Actual <- datpe()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capongm11kc()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      sogm21 <- data.frame(Actual,Fitted)
    }
  })

  #' Predicted values - render
  #' Parameters Estimation Models
  output$predictedpe <- renderTable({

    if (input$radiope == "SOGM (2, 1) model") {

      output$pepv <- renderPrint({input$radiope1})

      actual1 <- datpe()
      actual2 <- t(datpe())
      n <- length(actual2)
      fitted1 <- x0capsogm21()
      x <- input$radiope1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiope == "NGM (1, 1, k) model") {

      output$pepv <- renderPrint({input$radiope1})

      actual1 <- datpe()
      actual2 <- t(datpe())
      n <- length(actual2)
      fitted1 <- x0capngm11k()
      x <- input$radiope1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiope == "NGM (1, 1, k, c) model") {

      output$pepv <- renderPrint({input$radiope1})

      actual1 <- datpe()
      actual2 <- t(datpe())
      n <- length(actual2)
      fitted1 <- x0capngm11kc()
      x <- input$radiope1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radiope == "ONGM (1, 1, k, c) model") {

      output$pepv <- renderPrint({input$radiope1})

      actual1 <- datpe()
      actual2 <- t(datpe())
      n <- length(actual2)
      fitted1 <- x0capongm11kc()
      x <- input$radiope1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])
    }
  })

  #' Parameters Estimation - Error - render
  #' MAPE and RMSE
  output$errorpe <- renderTable({

    if (input$radiope == "SOGM (2, 1) model") {

      Actual <- datpe()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capsogm21()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiope == "NGM (1, 1, k) model") {

      Actual <- datpe()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capngm11k()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiope == "NGM (1, 1, k, c) model") {

      Actual <- datpe()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capngm11kc()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiope == "ONGM (1, 1, k, c) model") {

      Actual <- datpe()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capongm11kc()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)
    }
  })


  #' Parameters Estimation - Prediction Intervals - render
  #' CI  - reactive
  output$intervalspe <- renderTable({

    if (input$radiope == "SOGM (2, 1) model") {

      if (input$radiope2 == "90") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capsogm21()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiope2 == "95") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capsogm21()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiope2 == "99") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capsogm21()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      }

    } else if (input$radiope == "NGM (1, 1, k) model") {

      if (input$radiope2 == "90") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capngm11k()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiope2 == "95") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capngm11k()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiope2 == "99") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capngm11k()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      }


    } else if (input$radiope == "NGM (1, 1, k, c) model") {

      if (input$radiope2 == "90") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capngm11kc()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radiope2 == "95") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capngm11kc()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiope2 == "99") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capngm11kc()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }

    } else if (input$radiope == "ONGM (1, 1, k, c) model") {

      if (input$radiope2 == "90") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capongm11kc()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiope2 == "95") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capongm11kc()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)


      } else if (input$radiope2 == "99") {

        output$cipe <- renderPrint({input$radiope2})

        fp1 <- x0capongm11kc()
        actual1 <- datpe()
        x <- input$radiope1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }
    }
  })

  #' Parameters Estimation - Plots- render
  #' CI  - reactive
  output$plotpe <- renderPlotly({

    if (input$radiope == "SOGM (2, 1) model") {

      if (input$radiope2 == "90") {

        x0 <- datpe()
        x0cap2 <- x0capsogm21()
        ci <- 90
        model <- 'SOGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiope2 == "95") {

        x0 <- datpe()
        x0cap2 <- x0capsogm21()
        ci <- 95
        model <- 'SOGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiope2 == "99") {

        x0 <- datpe()
        x0cap2 <- x0capsogm21()
        ci <- 99
        model <- 'SOGM (2, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radiope == "NGM (1, 1, k) model") {

      if (input$radiope2 == "90") {

        x0 <- datpe()
        x0cap2 <- x0capngm11k()
        ci <- 90
        model <- 'NGM (1, 1, k) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiope2 == "95") {

        x0 <- datpe()
        x0cap2 <- x0capngm11k()
        ci <- 95
        model <- 'NGM (1, 1, k) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radiope2 == "99") {

        x0 <- datpe()
        x0cap2 <- x0capngm11k()
        ci <- 99
        model <- 'NGM (1, 1, k) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      }

    } else if (input$radiope == "NGM (1, 1, k, c) model") {

      if (input$radiope2 == "90") {

        x0 <- datpe()
        x0cap2 <- x0capngm11kc()
        ci <- 90
        model <- 'NGM (1, 1, k, c) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiope2 == "95") {

        x0 <- datpe()
        x0cap2 <- x0capngm11kc()
        ci <- 95
        model <- 'NGM (1, 1, k, c) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiope2 == "99") {

        x0 <- datpe()
        x0cap2 <- x0capngm11kc()
        ci <- 99
        model <- 'NGM (1, 1, k, c) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radiope == "ONGM (1, 1, k, c) model") {

      if (input$radiope2 == "90") {

        x0 <- datpe()
        x0cap2 <- x0capongm11kc()
        ci <- 90
        model <- 'ONGM (1, 1, k, c) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiope2 == "95") {

        x0 <- datpe()
        x0cap2 <- x0capongm11kc()
        ci <- 95
        model <- 'ONGM (1, 1, k, c) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radiope2 == "99") {

        x0 <- datpe()
        x0cap2 <- x0capongm11kc()
        ci <- 95
        model <- 'ONGM (1, 1, k, c) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }
    }
  })


  #' Optimization of parameters
  #' PSO-GM (1, 1) model Fitted - reactive
  #' optimal values a and b
  optpsogm <- reactive({

    x0 <- datopt()
    source("opt.R")
    optim_psogm(x0)

  })


  #' EGM (1, 1, r) model Fitted - reactive
  #' optimal value r
  optegm11r <- reactive({

    x0 <- datopt()
    source("opt.R")
    optim_egm11r(x0)

  })


  #' ANDGM (1, 1) model  - reactive
  #' Optimal value
  optandgm <- reactive({

    x0 <- datopt()
    source("opt.R")
    optim_andgm(x0)

  })


  #' Optimization-based grey models
  #' PSO-GM (1, 1) model Fitted - reactive
  #' Fitted values
  x0cappsogm <- reactive({

    x0 <- datopt()
    source("opt.R")
    psogm11(x0)


  })


  #' EGM (1, 1, r) model Fitted - reactive
  #' Fitted values
  x0capegm11r <- reactive({

    x0 <- datopt()
    source("opt.R")
    egm11r(x0)

  })


  #' ANDGM (1, 1) model Fitted - reactive
  #' Fitted values
  x0capandgm <- reactive({

    x0 <- datopt()
    source("opt.R")
    andgm11(x0)
  })


  #' Optimization - Actual and Fitted - render
  #' original and fitted data

  output$x0capopt <- renderTable({
    if (input$radioopt == "PSO-GM (1, 1) model") {

      output$opt <- renderText({
        paste("PSO-GM (1, 1) model: Particle swarm optimization-based grey model")
      })

      Actual <- datopt()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0cappsogm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      psogm <- data.frame(Actual,Fitted)


    } else if (input$radioopt == "EGM (1, 1, r) model") {

      output$opt <- renderText({
        paste("EGM (1, 1, r) model: Even form of grey model with one variable and one first order equation",
              "with accumulating generation of order r",
              "The EGM (1, 1, r) function takes as input raw values and output predicted values following the paper by",
              "Zeng B, Li S, Meng W, Zhang D (2019). An Improved Grey Prediction Model for China's Beef Comsumption",
              "Forecasting. PLOS ONE, 14(9), 1-18.",
              "DOI:10.1371/journal.pone.0221333",
              sep = "\n")
      })


      Actual <- datopt()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capegm11r()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      egm11r <- data.frame(Actual,Fitted)

    } else if (input$radioopt == "ANDGM (1, 1) model") {

      output$opt <- renderText({
        paste("ANDGM (1, 1) model: Adjacent non-homogeneous discrete grey model",
              "The ANDGM (1, 1) model function takes as input raw values and output predicted values following the paper by",
              "Liu L, Wu L (2021). Forecasting the Renewable Energy Consumption of the European Countries by an Adjacent",
              "Non-homogeneous Grey Model. Applied Mathematical Modelling, 89, 1932-1948.",
              "DOI:10.1016/j.apm.2020.08.080",
              sep = "\n")
      })


      Actual <- datopt()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capandgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      andgm11 <- data.frame(Actual,Fitted)
    }
  })

  #' Optimal values - render
  #' Optimization-based Models
  output$optval <- renderTable({

    if (input$radioopt == "PSO-GM (1, 1) model") {

      opt <- optpsogm()

    } else if (input$radioopt =="EGM (1, 1, r) model") {

      opt <- optegm11r()

    } else if (input$radioopt == "ANDGM (1, 1) model") {

      opt <- optandgm()

    }
  })


  #' Predicted values - render
  #' Optimization-based Models
  output$predictedopt <- renderTable({

    if (input$radioopt == "PSO-GM (1, 1) model") {

      output$optpv <- renderPrint({input$radioopt1})

      actual1 <- datopt()
      actual2 <- t(datopt())
      n <- length(actual2)
      fitted1 <- x0cappsogm()
      x <- input$radioopt1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioopt =="EGM (1, 1, r) model") {

      output$optpv <- renderPrint({input$radioopt1})

      actual1 <- datopt()
      actual2 <- t(datopt())
      n <- length(actual2)
      fitted1 <- x0capegm11r()
      x <- input$radioopt1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])

    } else if (input$radioopt == "ANDGM (1, 1) model") {

      actual1 <- datopt()
      actual2 <- t(datopt())
      n <- length(actual2)
      fitted1 <- x0capandgm()
      x <- input$radioopt1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])
    }
  })

  #' Optimization-based grey models - Error - render
  #' MAPE and RMSE
  output$erroropt <- renderTable({

    if (input$radioopt == "PSO-GM (1, 1) model") {

      Actual <- datopt()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0cappsogm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioopt == "EGM (1, 1, r) model") {

      Actual <- datopt()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capegm11r()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radioopt == "ANDGM (1, 1) model") {

      Actual <- datopt()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capandgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)
    }
  })

  #' Optimization-based grey models - Prediction Intervals - render
  #' CI  - reactive
  output$intervalsopt <- renderTable({

    if (input$radioopt == "PSO-GM (1, 1) model") {

      if (input$radioopt2 == "90") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0cappsogm()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioopt2 == "95") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0cappsogm()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioopt2 == "99") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0cappsogm()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      }

    } else if (input$radioopt == "EGM (1, 1, r) model") {

      if (input$radioopt2 == "90") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0capegm11r()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioopt2 == "95") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0capegm11r()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioopt2 == "99") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0capegm11r()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)
      }

    } else if (input$radioopt == "ANDGM (1, 1) model") {

      if (input$radioopt2 == "90") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0capandgm()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 90

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioopt2 == "95") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0capandgm()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 95

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      } else if (input$radioopt2 == "99") {

        output$ciopt <- renderPrint({input$radioopt2})

        fp1 <- x0capandgm()
        actual1 <- datopt()
        x <- input$radioopt1
        ci <- 99

        source("confidence_interval.R")
        CIvalue(fp1,actual1,x,ci)

      }
    }
  })


  #' Optimization-based grey models - Plots - render

  output$plotopt <- renderPlotly({

    if (input$radioopt == "PSO-GM (1, 1) model") {

      if (input$radioopt2 == "90") {

        x0 <- datopt()
        x0cap2 <- x0cappsogm()
        ci <- 90
        model <- 'PSO-GM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioopt2 == "95") {

        x0 <- datopt()
        x0cap2 <- x0cappsogm()
        ci <- 95
        model <- 'PSO-GM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioopt2 == "99") {

        x0 <- datopt()
        x0cap2 <- x0cappsogm()
        ci <- 99
        model <- 'PSO-GM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radioopt == "EGM (1, 1, r) model") {

      if (input$radioopt2 == "90") {

        x0 <- datopt()
        x0cap2 <- x0capegm11r()
        ci <- 90
        model <- 'EGM (1, 1, r) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioopt2 == "95") {

        x0 <- datopt()
        x0cap2 <- x0capegm11r()
        ci <- 95
        model <- 'EGM (1, 1, r) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioopt2 == "99") {

        x0 <- datopt()
        x0cap2 <- x0capegm11r()
        ci <- 99
        model <- 'EGM (1, 1, r) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }

    } else if (input$radioopt == "ANDGM (1, 1) model") {

      if (input$radioopt2 == "90") {

        x0 <- datopt()
        x0cap2 <- x0capandgm()
        ci <- 90
        model <- 'ANDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      } else if (input$radioopt2 == "95") {

        x0 <- datopt()
        x0cap2 <- x0capandgm()
        ci <- 95
        model <- 'ANDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)


      } else if (input$radioopt2 == "99") {

        x0 <- datopt()
        x0cap2 <- x0capandgm()
        ci <- 99
        model <- 'ANDGM (1, 1) model'

        source("plots.R")
        plots(x0,x0cap2,ci,model)

      }
    }
  })


  #' Residual Modification

  #' Remnant (1, 1) model Fitted - reactive
  #' Fitted values
  x0capremgm <- reactive({

    x0 <- datrm()
    x0_A <- datoutrm()

    source("rm.R")
    remnantgm11(x0,x0_A)

  })

  #' TGM (1, 1) model Fitted - reactive
  #' Fitted values
  x0captgm <- reactive({

    x0 <- datrm()
    x0_a <- datoutrm()

    source("rm.R")
    tgm11(x0,x0_a)

  })

  #' Residual Modification - Actual and Fitted - render
  #' original and fitted data

  output$x0caprm <- renderTable({
    if (input$radiorm == "Remnant GM (1, 1) model") {

      output$rm <- renderText({
        paste("Remnant GM (1, 1) model: Residual-based grey model",
              "The remnant GM function takes as input raw values and output predicted values following the paper by",
              "Hu Y (2020). Energy Demand Forecasting using a Novel Remnant GM (1, 1) Model.",
              "Soft Computing, pp. 13903-13912.",
              "DOI:10.1007/s00500-020-04765-3",
              sep = "\n")
      })

      Actual <- datrm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capremgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      remgm11 <- data.frame(Actual,Fitted)


    } else if (input$radiorm == "TGM (1, 1) model") {

      output$rm <- renderText({
        paste("TGM (1, 1) model: Trigonometric grey model",
              "The TGM function takes as input raw values and output predicted values following the paper by",
              "Zhou P, Ang B, Poh K (2006). A Trigonometric Grey Prediction Approach to Forecasting",
              "Electricity Demand. Energy, 31(14), 2839-2847.",
              "DOI:10.1016/j.energy.2005.12.002",
              sep ="\n")
      })

      Actual <- datrm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0captgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      tgm11 <- data.frame(Actual,Fitted)

    }
  })

  #' Predicted values - render
  #' Residual Modification
  output$predictedrm <- renderTable({

    if (input$radiorm == "Remnant GM (1, 1) model") {

      output$rmpv <- renderPrint({input$radiorm1})

      actual1 <- datrm()
      actual2 <- t(datrm())
      n <- length(actual2)
      fitted1 <- x0capremgm()
      x <- input$radiorm1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiorm == "TGM (1, 1) model") {

      output$rmpv <- renderPrint({input$radiorm1})

      actual1 <- datrm()
      actual2 <- t(datrm())
      n <- length(actual2)
      fitted1 <- x0captgm()
      x <- input$radiorm1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    }
  })

  #' Residual Modification - Error - render
  #' MAPE and RMSE
  output$errorrm <- renderTable({

    if (input$radiorm == "Remnant GM (1, 1) model") {

      Actual <- datrm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0capremgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)


    } else if (input$radiorm == "TGM (1, 1) model") {

      Actual <- datrm()
      actual2 <- t(Actual)
      n <- length(actual2)
      fitted1 <- x0captgm()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)


    }
  })


  #' Residual Modification - Prediction Intervals - render
  #' CI  - reactive
  output$intervalsrm <- renderTable({

    if (input$radiorm == "Remnant GM (1, 1) model") {

      if (input$radiorm2 == "90") {

        output$cirm <- renderPrint({input$radiorm2})

        actual1 <- datrm()
        fp1 <- x0capremgm()
        x <- input$radiorm1
        ci <- 90

        source("CIplots_rm.R")
        CI_rm(fp1,actual1,x,ci)

      } else if (input$radiorm2 == "95") {

        output$cirm <- renderPrint({input$radiorm2})

        actual1 <- datrm()
        fp1 <- x0capremgm()
        x <- input$radiorm1
        ci <- 95

        source("CIplots_rm.R")
        CI_rm(fp1,actual1,x,ci)


      } else if (input$radiorm2 == "99") {

        output$cirm <- renderPrint({input$radiorm2})

        actual1 <- datrm()
        fp1 <- x0capremgm()
        x <- input$radiorm1
        ci <- 99

        source("CIplots_rm.R")
        CI_rm(fp1,actual1,x,ci)

      }

    } else if (input$radiorm == "TGM (1, 1) model") {

      if (input$radiorm2 == "90") {

        output$cirm <- renderPrint({input$radiorm2})

        actual1 <- datrm()
        fp1 <- x0captgm()
        x <- input$radiorm1
        ci <- 90

        source("CIplots_rm.R")
        CI_rm(fp1,actual1,x,ci)

      } else if (input$radiorm2 == "95") {

        output$cirm <- renderPrint({input$radiorm2})

        actual1 <- datrm()
        fp1 <- x0captgm()
        x <- input$radiorm1
        ci <- 95

        source("CIplots_rm.R")
        CI_rm(fp1,actual1,x,ci)


      } else if (input$radiorm2 == "99") {

        output$cirm <- renderPrint({input$radiorm2})

        actual1 <- datrm()
        fp1 <- x0captgm()
        x <- input$radiorm1
        ci <- 99

        source("CIplots_rm.R")
        CI_rm(fp1,actual1,x,ci)

      }
    }
  })


  #' Residual Modification - Plots - render
  #' Plots  - reactive
  output$plotrm <- renderPlotly({

    if (input$radiorm == "Remnant GM (1, 1) model") {

      if (input$radiorm2 == "90") {

        x0 <- datrm()
        x0cap2 <- x0capremgm()
        ci <- 90
        model <- 'Remnant GM (1, 1) model'

        source("CIplots_rm.R")
        plotrm(x0,x0cap2,ci,model)

      } else if (input$radiorm2 == "95") {

        x0 <- datrm()
        x0cap2 <- x0capremgm()
        ci <- 95
        model <- 'Remnant GM (1, 1) model'

        source("CIplots_rm.R")
        plotrm(x0,x0cap2,ci,model)


      } else if (input$radiorm2 == "99") {

        x0 <- datrm()
        x0cap2 <- x0capremgm()
        ci <- 99
        model <- 'Remnant GM (1, 1) model'

        source("CIplots_rm.R")
        plotrm(x0,x0cap2,ci,model)

      }

    } else if (input$radiorm == "TGM (1, 1) model") {

      if (input$radiorm2 == "90") {

        x0 <- datrm()
        x0cap2 <- x0captgm()
        ci <- 90
        model <- 'TGM (1, 1) model'

        source("CIplots_rm.R")
        plotrm(x0,x0cap2,ci,model)


      } else if (input$radiorm2 == "95") {

        x0 <- datrm()
        x0cap2 <- x0captgm()
        ci <- 95
        model <- 'TGM (1, 1) model'

        source("CIplots_rm.R")
        plotrm(x0,x0cap2,ci,model)


      } else if (input$radiorm2 == "99") {

        x0 <- datrm()
        x0cap2 <- x0captgm()
        ci <- 99
        model <- 'TGM (1, 1) model'

        source("CIplots_rm.R")
        plotrm(x0,x0cap2,ci,model)

      }
    }
  })


  #' Multivariate Grey Models

  #' GM (1, 3) model Fitted - reactive
  #' Fitted values
  x0capgm13 <- reactive({

    x0 <- datmv()
    x1 <- x0[ ,1]
    x2 <- x0[ ,2]
    x3 <- x0[ ,3]

    source("mv.R")
    gm13(x1,x2,x3)

  })



  #' IGM (1, 3) model Fitted - reactive
  #' Fitted values
  x0capigm13 <- reactive({

    x0 <- datmv()
    x1 <- x0[ ,1]
    x2 <- x0[ ,2]
    x3 <- x0[ ,3]

    source("mv.R")
    igm13(x1,x2,x3)

  })


  #' DBGM (1, 2) model Fitted - reactive
  #' Fitted values
  x0capdbgm12 <- reactive({

    x0 <- datinmv()
    x01 <- x0[ ,1]
    x02 <- x0[ ,2]

    dat_a <- c(datoutmv())

    source("mv.R")
    dbgm12(x01,x02,dat_a)

  })


  #' GMC (1, 2) model Fitted - reactive
  #' Fitted values
  x0capgmc12 <- reactive({

    x0 <- datinmv()
    x01 <- x0[ ,1]
    x02 <- x0[ ,2]

    dat_a <- c(datoutmv())
    source("mv.R")
    gmc12(x01,x02,dat_a)

  })



  #' GMC_g (1, 2) model Fitted - reactive
  #' Fitted values
  x0capgmcg12 <- reactive({

    x0 <- datinmv()
    x01 <- x0[ ,1]
    x02 <- x0[ ,2]

    dat_a <- c(datoutmv())

    source("mv.R")
    gmcg12(x01,x02,dat_a)

  })


  #' NHMGM_p (1, 2) model when p = 1 Fitted - reactive
  #' Fitted values
  x0capnhmgm1 <- reactive({

    x0 <- datmv()
    x01 <- x0[ ,1]
    x02 <- x0[ ,2]

    source("mv.R")
    nhmgm1(x01,x02)

  })


  #' NHMGM_p (1, 2) model when p = 2 Fitted - reactive
  #' Fitted values
  x0capnhmgm2 <- reactive({

    x0 <- datmv()
    x01 <- x0[ ,1]
    x02 <- x0[ ,2]

    source("mv.R")
    nhmgm2(x01,x02)

  })



  #' Multivariate Models - In-Sample - Actual and Fitted - render
  #' original and fitted data

  output$x0capmv <- renderTable({
    if (input$radiomv == "GM (1, 3) model") {

      output$mv <- renderText({
        paste("GM (1, 3) model: Grey multivariate model with first order differential equation and 3 variables.",
              "Cheng M, Li J, Liu Y, Liu B (2020). Forecasting Clean Energy Consumption in China by 2025:",
              "Using Improved Grey Model GM (1, N). Sustainability, 12(2), 1-20.",
              "DOI:10.3390/su12020698",
              sep = "\n")
      })

      actual1 <- datmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgm13()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfgm13 <- data.frame(actual,Fitted)


    } else if (input$radiomv == "IGM (1, 3) model") {

      output$mv <- renderText({
        paste("IGM (1, 3) model: Improved grey multivariate model with first order differential equation and 3 variables.",
              "Cheng M, Li J, Liu Y, Liu B (2020). Forecasting Clean Energy Consumption in China by 2025:",
              "Using Improved Grey Model GM (1, N). Sustainability, 12(2), 1-20.",
              "DOI:10.3390/su12020698",
              sep = "\n")
      })

      actual1 <- datmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capigm13()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfigm13 <- data.frame(actual,Fitted)


    }else if (input$radiomv == "NHMGM_1 (1, 2) model") {

      output$mv <- renderText({
        paste("NHMGM_1 (1, 2) model: Non-homogeneous multivariate grey model with first order differential equation and 2 variables.",
              "The NHMGM_1 (1, 2) model function takes as input raw values and output predicted values following the paper by",
              "Wang H, Wang P, Senel M, Li T (2019). On Novel Non-homogeneous Multivariable Grey Forecasting Model",
              "NHMGM. Mathematical Problems in Engineering, 2019, 1-13.",
              "DOI:10.1155/2019/9049815",
              sep = "\n")
      })

      actual <- datmv()
      A1 <- actual[,1]
      A2 <- actual[,2]
      a1 <- t(A1)
      n <- length(a1)
      fitted <- x0capnhmgm1()
      f1 <- fitted[,1]
      f2 <- fitted[,2]
      F1 <- f1[1:n]
      F2 <- f2[1:n]
      dfnhmgm1 <- data.frame(A1,F1,A2,F2)
      colnames(dfnhmgm1  , do.NULL = FALSE)
      colnames(dfnhmgm1  ) <- c("Actual Data 1","Fitted Data 1","Actual DAta 2","Fitted Data 2")
      dfnhmgm1


    } else if (input$radiomv == "NHMGM_2 (1, 2) model") {

      output$mv <- renderText({
        paste("NHMGM_2 (1, 2) model: Non-homogeneous multivariate grey model with first order differential equation and 2 variables.",
              "The NHMGM_2 (1, 2) model function takes as input raw values and output predicted values following the paper by",
              "Wang H, Wang P, Senel M, Li T (2019). On Novel Non-homogeneous Multivariable Grey Forecasting Model",
              "NHMGM. Mathematical Problems in Engineering, 2019, 1-13.",
              "DOI:10.1155/2019/9049815",
              sep = "\n")
      })

      actual <- datmv()
      A1 <- actual[,1]
      A2 <- actual[,2]
      a1 <- t(A1)
      n <- length(a1)
      fitted <- x0capnhmgm2()
      f1 <- fitted[,1]
      f2 <- fitted[,2]
      F1 <- f1[1:n]
      F2 <- f2[1:n]
      dfnhmgm2 <- data.frame(A1,F1,A2,F2)
      colnames(dfnhmgm2  , do.NULL = FALSE)
      colnames(dfnhmgm2  ) <- c("Actual Data 1","Fitted Data 1","Actual DAta 2","Fitted Data 2")
      dfnhmgm2

    }
  })

  #' Multivariate Models - In & Out Sample - Actual and Fitted - render
  #' original and fitted data

  output$x0capoutmv <- renderTable({

    if (input$radiooutmv == "GMC (1, 2) model") {

      output$outmv <- renderText({
        paste("GMC (1, 2) model: Multivariate grey convolution model with first order differential equation and",
              "2 variables using the trapezoidal rule.",
              "The GMC (1, 2) function takes as input raw values and output predicted values following the paper by",
              "Ding S, Li R (2020). A New Multivariable Grey Convolution model based on Simpson's rule and its",
              "Application. Complexity, pp. 1-14.",
              "DOI:10.1155/2020/4564653",
              sep = "\n")
      })

      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgmc12()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfgmc12 <- data.frame(actual,Fitted)


    } else if (input$radiooutmv == "GMC_g (1, 2) model") {

      output$outmv <- renderText({
        paste("GMC_g (1, 2) model: Multivariate grey convolution model with first order differential equation and",
              "2 variables using the Gaussian rule.",
              "The GMC_g (1, 2) function takes as input raw values and output predicted values following the paper by",
              "Ding S, Li R (2020). A New Multivariable Grey Convolution model based on Simpson's rule and its",
              "Application. Complexity, pp. 1-14.",
              "DOI:10.1155/2020/4564653",
              sep = "\n")
      })

      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgmc12()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfgmcg12 <- data.frame(actual,Fitted)

    } else if (input$radiooutmv == "DBGM (1, 2) model") {

      output$outmv <- renderText({
        paste("DBGM (1, 2) model: Multivariate grey model with dynamic background value, first order differential equation and 2 variables using the Gaussian rule.",
              "The DBGM (1, 2) model function takes as input raw values and output predicted values following the paper by",
              "Zeng B, Li C (2018). Improved Multivariable Grey Forecasting Model and with a Dynamic Background Value",
              "Coefficient and its Application. Computers and Industrial Engineering, 118, 278-290.",
              "DOI:10.1016/j.cie.2018.02.042",
              sep = "\n")
      })
      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capdbgm12()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      dfdbgm12 <- data.frame(actual,Fitted)
    }
  })



  #' Predicted values - render
  #' Multivariate Models
  output$predictedmv <- renderTable({

    if (input$radiomv == "GM (1, 3) model") {

      output$mvpv <- renderPrint({input$radiomv1})

      actual1 <- datmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgm13()
      x <- input$radiomv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiomv == "IGM (1, 3) model") {

      output$mvpv <- renderPrint({input$radiomv1})

      actual1 <- datmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capigm13()
      x <- input$radiomv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiomv == "NHMGM_1 (1, 2) model") {

      output$mvpv <- renderPrint({input$radiomv1})

      actual <- datmv()
      A1 <- actual[,1]
      A2 <- actual[,2]
      a1 <- t(A1)
      n <- length(a1)
      fitted <- x0capnhmgm1()
      x <- input$radiomv1
      f1 <- fitted[,1]
      f2 <- fitted[,2]
      x <- input$radiomv1
      p1 <- tail(f1,4)
      pred1 <- p1[1:x]
      p2 <- tail(f2,4)
      pred2 <- p2[1:x]

      prednhmgm <- data.frame(pred1,pred2)
      colnames(prednhmgm  , do.NULL = FALSE)
      colnames(prednhmgm  ) <- c("Predicted Data 1","Predicted Data 2")
      prednhmgm


    } else if (input$radiomv == "NHMGM_2 (1, 2) model") {

      output$mvpv <- renderPrint({input$radiomv1})

      actual <- datmv()
      A1 <- actual[,1]
      A2 <- actual[,2]
      a1 <- t(A1)
      n <- length(a1)
      fitted <- x0capnhmgm2()
      x <- input$radiomv1
      f1 <- fitted[,1]
      f2 <- fitted[,2]
      x <- input$radiomv1
      p1 <- tail(f1,4)
      pred1 <- p1[1:x]
      p2 <- tail(f2,4)
      pred2 <- p2[1:x]

      prednhmgm <- data.frame(pred1,pred2)
      colnames(prednhmgm  , do.NULL = FALSE)
      colnames(prednhmgm  ) <- c("Predicted Data 1","Predicted Data 2")
      prednhmgm


    }
  })


  #' Predicted values - render
  #' Multivariate Models - In & Out Sample
  output$predictedoutmv <- renderTable({

    if (input$radiooutmv == "GMC (1, 2) model") {

      output$outmvpv <- renderPrint({input$radiooutmv1})

      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgmc12()
      x <- input$radiooutmv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiooutmv == "GMC_g (1, 2) model") {

      output$outmvpv <- renderPrint({input$radiooutmv1})

      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgmcg12()
      x <- input$radiooutmv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    } else if (input$radiooutmv == "DBGM (1, 2) model") {

      output$outmvpv <- renderPrint({input$radiooutmv1})

      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capdbgm12()
      x <- input$radiooutmv1
      fitted3 <- tail(fitted1,4)
      fitted4 <- t(fitted3[1:x])


    }
  })


  #' Multivariate models - Error - render
  #' MAPE and RMSE
  output$errormv <- renderTable({

    if (input$radiomv == "GM (1, 3) model") {

      actual1 <- datmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgm13()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(actual,Fitted)*100
      RMSE <- rmse(actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiomv == "IGM (1, 3) model") {

      actual1 <- datmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capigm13()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(actual,Fitted)*100
      RMSE <- rmse(actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiomv == "NHMGM_1 (1, 2) model") {

      actual <- datmv()
      A1 <- actual[,1]
      A2 <- actual[,2]
      a1 <- t(A1)
      n <- length(a1)
      fitted <- x0capnhmgm1()
      f1 <- fitted[,1]
      f2 <- fitted[,2]
      F1 <- f1[1:n]
      F2 <- f2[1:n]
      MAPE1 <- mape(A1,F1)*100
      RMSE1 <- rmse(A1,F1)

      MAPE2 <- mape(A2,F2)*100
      RMSE2 <- rmse(A2,F2)

      penhmgm <- data.frame(MAPE1,RMSE1,MAPE2,RMSE2)
      colnames(penhmgm  , do.NULL = FALSE)
      colnames(penhmgm  ) <- c("MAPE Data 1","RMSE Data 1","MAPE DAta 2","RMSE Data 2")
      penhmgm


    } else if (input$radiomv == "NHMGM_2 (1, 2) model") {

      actual <- datmv()
      A1 <- actual[,1]
      A2 <- actual[,2]
      a1 <- t(A1)
      n <- length(a1)
      fitted <- x0capnhmgm2()
      f1 <- fitted[,1]
      f2 <- fitted[,2]
      F1 <- f1[1:n]
      F2 <- f2[1:n]
      MAPE1 <- mape(A1,F1)*100
      RMSE1 <- rmse(A1,F1)

      MAPE2 <- mape(A2,F2)*100
      RMSE2 <- rmse(A2,F2)

      penhmgm <- data.frame(MAPE1,RMSE1,MAPE2,RMSE2)
      colnames(penhmgm  , do.NULL = FALSE)
      colnames(penhmgm  ) <- c("MAPE Data 1","RMSE Data 1","MAPE DAta 2","RMSE Data 2")
      penhmgm


    }

  })


  #' Multivariate models - Error - render
  #' MAPE and RMSE
  output$erroroutmv <- renderTable({

    if (input$radiooutmv == "GMC (1, 2) model") {

      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgmc12()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(actual,Fitted)*100
      RMSE <- rmse(actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    } else if (input$radiooutmv == "GMC_g (1, 2) model") {

      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capgmcg12()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(actual,Fitted)*100
      RMSE <- rmse(actual,Fitted)
      pe <- data.frame(MAPE,RMSE)


    } else if (input$radiooutmv == "DBGM (1, 2) model") {

      actual1 <- datinmv()
      actual <- actual1[ ,1]
      actual2 <- t(actual)
      n <- length(actual2)
      fitted1 <- x0capdbgm12()
      fitted2 <- t(fitted1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(actual,Fitted)*100
      RMSE <- rmse(actual,Fitted)
      pe <- data.frame(MAPE,RMSE)


    }

  })


  #' Multivariate models - Prediction Intervals - render
  #' CI  - reactive
  output$intervalsmv <- renderTable({

    if (input$radiomv == "GM (1, 3) model") {

      if (input$radiomv2 == "90") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capgm13()
        actual1 <- datmv()
        actual <- actual1[ ,1]
        x <- input$radiomv1
        ci <- 90

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      } else if (input$radiomv2 == "95") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capgm13()
        actual1 <- datmv()
        actual <- actual1[ ,1]
        x <- input$radiomv1
        ci <- 95

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      } else if (input$radiomv2 == "99") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capgm13()
        actual1 <- datmv()
        actual <- actual1[ ,1]
        x <- input$radiomv1
        ci <- 99

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)
      }


    } else if (input$radiomv == "IGM (1, 3) model") {

      if (input$radiomv2 == "90") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capigm13()
        actual1 <- datmv()
        actual <- actual1[ ,1]
        x <- input$radiomv1
        ci <- 90

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      } else if (input$radiomv2 == "95") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capigm13()
        actual1 <- datmv()
        actual <- actual1[ ,1]
        x <- input$radiomv1
        ci <- 95

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      } else if (input$radiomv2 == "99") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capigm13()
        actual1 <- datmv()
        actual <- actual1[ ,1]
        x <- input$radiomv1
        ci <- 99

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      }

    } else if (input$radiomv == "NHMGM_1 (1, 2) model") {

      if (input$radiomv2 == "90") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capnhmgm1()
        actual <- datmv()
        x01 <- actual[,1]
        x02 <- actual[,2]
        x <- input$radiomv1
        ci <- 90

        source("CI_MV.R")
        CI_nhmgmp(fp1,x01,x02,x,ci)


      } else if (input$radiomv2 == "95") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capnhmgm1()
        actual <- datmv()
        x01 <- actual[,1]
        x02 <- actual[,2]
        x <- input$radiomv1
        ci <- 95

        source("CI_MV.R")
        CI_nhmgmp(fp1,x01,x02,x,ci)


      } else if (input$radiomv2 == "99") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capnhmgm1()
        actual <- datmv()
        x01 <- actual[,1]
        x02 <- actual[,2]
        x <- input$radiomv1
        ci <- 99

        source("CI_MV.R")
        CI_nhmgmp(fp1,x01,x02,x,ci)
      }

    } else if (input$radiomv == "NHMGM_2 (1, 2) model") {

      if (input$radiomv2 == "90") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capnhmgm2()
        actual <- datmv()
        x01 <- actual[,1]
        x02 <- actual[,2]
        x <- input$radiomv1
        ci <- 90

        source("CI_MV.R")
        CI_nhmgmp(fp1,x01,x02,x,ci)

      } else if (input$radiomv2 == "95") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capnhmgm2()
        actual <- datmv()
        x01 <- actual[,1]
        x02 <- actual[,2]
        x <- input$radiomv1
        ci <- 95

        source("CI_MV.R")
        CI_nhmgmp(fp1,x01,x02,x,ci)


      } else if (input$radiomv2 == "99") {

        output$cimv <- renderPrint({input$radiomv2})

        fp1 <- x0capnhmgm2()
        actual <- datmv()
        x01 <- actual[,1]
        x02 <- actual[,2]
        x <- input$radiomv1
        ci <- 99

        source("CI_MV.R")
        CI_nhmgmp(fp1,x01,x02,x,ci)

      }
    }
  })


  #' Multivariate models - In & Out sample - Prediction Intervals - render
  #' CI  - reactive
  output$intervalsoutmv <- renderTable({

    if (input$radiooutmv == "GMC (1, 2) model") {

      if (input$radiooutmv2 == "90") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capgmc12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 90

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      } else if (input$radiooutmv2 == "95") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capgmc12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 95

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)


      } else if (input$radiooutmv2 == "99") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capgmc12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 99

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      }

    } else if (input$radiooutmv == "GMC_g (1, 2) model") {

      if (input$radiooutmv2 == "90") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capgmcg12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 90

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)


      } else if (input$radiooutmv2 == "95") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capgmcg12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 95

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)



      } else if (input$radiooutmv2 == "99") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capgmcg12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 99

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      }


    } else if (input$radiooutmv == "DBGM (1, 2) model") {

      if (input$radiooutmv2 == "90") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capdbgm12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 90

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)


      } else if (input$radiooutmv2 == "95") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capdbgm12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 95

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)


      } else if (input$radiooutmv2 == "99") {

        output$cioutmv <- renderPrint({input$radiooutmv2})

        fp1 <- x0capdbgm12()
        actual1 <- datinmv()
        actual <- actual1[ ,1]
        x <- input$radiooutmv1
        ci <- 99

        source("CI_MV.R")
        CIvalue(fp1,actual,x,ci)

      }
    }
  })


  #' Multivariate models - Plots - render
  #' plots  - reactive
  output$plotmv <- renderPlotly({

    if (input$radiomv == "GM (1, 3) model") {

      if (input$radiomv2 == "90") {

        actual1 <- datmv()
        fp1 <- x0capgm13()
        ci <- 90
        model <- 'GM (1, 3) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)


      } else if (input$radiomv2 == "95") {

        actual1 <- datmv()
        fp1 <- x0capgm13()
        ci <- 95
        model <- 'GM (1, 3) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)

      } else if (input$radiomv2 == "99") {

        actual1 <- datmv()
        fp1 <- x0capgm13()
        ci <- 99
        model <- 'GM (1, 3) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)
      }

    } else if (input$radiomv == "IGM (1, 3) model") {

      if (input$radiomv2 == "90") {

        fp1 <- x0capigm13()
        actual1 <- datmv()
        ci <- 90
        model <- 'IGM (1, 3) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)



      } else if (input$radiomv2 == "95") {

        fp1 <- x0capigm13()
        actual1 <- datmv()
        ci <- 95
        model <- 'IGM (1, 3) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)

      } else if (input$radiomv2 == "99") {

        fp1 <- x0capigm13()
        actual1 <- datmv()
        ci <- 99
        model <- 'IGM (1, 3) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)
      }

    } else if (input$radiomv == "NHMGM_1 (1, 2) model") {

      if (input$radiomv2 == "90") {


        actual1 <- datmv()
        fitted <- x0capnhmgm1()
        ci <- 90
        model <- 'NHMGM_1 (1, 2) model'

        source("plots_MV.R")
        plotsmv2(actual1,fitted,ci,model)

      } else if (input$radiomv2 == "95") {

        actual1 <- datmv()
        fitted <- x0capnhmgm1()
        ci <- 95
        model <- 'NHMGM_1 (1, 2) model'

        source("plots_MV.R")
        plotsmv2(actual1,fitted,ci,model)


      } else if (input$radiomv2 == "99") {

        actual1 <- datmv()
        fitted <- x0capnhmgm1()
        ci <- 99
        model <- 'NHMGM_1 (1, 2) model'

        source("plots_MV.R")
        plotsmv2(actual1,fitted,ci,model)

      }

    } else if (input$radiomv == "NHMGM_2 (1, 2) model") {

      if (input$radiomv2 == "90") {

        actual1 <- datmv()
        fitted <- x0capnhmgm2()
        ci <- 90
        model <- 'NHMGM_2 (1, 2) model'

        source("plots_MV.R")
        plotsmv2(actual1,fitted,ci,model)


      } else if (input$radiomv2 == "95") {

        actual1 <- datmv()
        fitted <- x0capnhmgm2()
        ci <- 95
        model <- 'NHMGM_2 (1, 2) model'

        source("plots_MV.R")
        plotsmv2(actual1,fitted,ci,model)



      } else if (input$radiomv2 == "99") {


        actual1 <- datmv()
        fitted <- x0capnhmgm2()
        ci <- 99
        model <- 'NHMGM_2 (1, 2) model'

        source("plots_MV.R")
        plotsmv2(actual1,fitted,ci,model)


      }
    }
  })


  #' Multivariate models - In & Out sample - Plots - render
  output$plotoutmv <- renderPlotly({

    if (input$radiooutmv == "GMC (1, 2) model") {

      if (input$radiooutmv2 == "90") {


        fp1 <- x0capgmc12()
        actual1 <- datinmv()
        ci <- 90
        model <- 'GMC (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)


      } else if (input$radiooutmv2 == "95") {

        fp1 <- x0capgmc12()
        actual1 <- datinmv()
        ci <- 95
        model <- 'GMC (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)

      } else if (input$radiooutmv2 == "99") {

        fp1 <- x0capgmc12()
        actual1 <- datinmv()
        ci <- 99
        model <- 'GMC (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)


      }

    } else if (input$radiooutmv == "GMC_g (1, 2) model") {

      if (input$radiooutmv2 == "90") {


        fp1 <- x0capgmcg12()
        actual1 <- datinmv()
        ci <- 90
        model <- 'GMC_g (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)


      } else if (input$radiooutmv2 == "95") {

        fp1 <- x0capgmcg12()
        actual1 <- datinmv()
        ci <- 95
        model <- 'GMC_g (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)

      } else if (input$radiooutmv2 == "99") {

        fp1 <- x0capgmcg12()
        actual1 <- datinmv()
        ci <- 99
        model <- 'GMC_g (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)


      }


    } else if (input$radiooutmv == "DBGM (1, 2) model") {

      if (input$radiooutmv2 == "90") {

        fp1 <- x0capdbgm12()
        actual1 <- datinmv()
        ci <- 90
        model <- 'DBGM (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)



      } else if (input$radiooutmv2 == "95") {

        fp1 <- x0capdbgm12()
        actual1 <- datinmv()
        ci <- 95
        model <- 'DBGM (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)

      } else if (input$radiooutmv2 == "99") {

        fp1 <- x0capdbgm12()
        actual1 <- datinmv()
        ci <- 99
        model <- 'DBGM (1, 2) model'

        source("plots_MV.R")
        plotsmv1(actual1,fp1,ci,model)


      }
    }
  })

  #' Interval Multivariate Models

  #' IG-NDGM (1, 2) model Fitted - reactive
  #' Fitted values
  x0capigndgm12 <- reactive({

    x0 <- datimv()
    LB <- x0[ ,1]
    UB <- x0[ ,2]

    source("imv.R")
    igndgm12(LB,UB)

  })

  #' MDBGM (1, 2) model Fitted - reactive
  #' Fitted values
  x0capmdbgm12 <- reactive({

    data <- datinimv()
    x0 <- data.frame(data)
    x01L <- x0[ ,1]
    x01U <- x0[ ,2]
    x02L <- x0[ ,3]
    x02U <- x0[ ,4]

    data_A <- datoutimv()
    dfdata_a <- data.frame(data_A)
    x01La <- dfdata_a[ ,1]
    x01Ua <- dfdata_a[ ,2]
    x02La <- dfdata_a[ ,3]
    x02Ua <- dfdata_a[ ,4]

    source("imv.R")
    mdbgm12(x01L,x01U,x02L,x02U,x01La,x01Ua,x02La,x02Ua)

  })


  #' Interval Multivariate Models - Actual and Fitted - render - in sample
  #' original and fitted data
  output$x0capimv <- renderTable({
    if (input$radioimv == "IG-NDGM (1, 2) model") {

      output$imv <- renderText({
        paste("IG-NDGM (1, 2) model: Interval grey number sequence based on non-homogeneous discrete grey model. ",
              "The IG-NDGM (1, 2) model function takes as input raw values and output predicted values following the paper by",
              "Xie N, Liu S (2015). Interval Grey Number Sequence Prediction by using Nonhomogeneous Exponential Discrete",
              "Grey Forecasting Model. Journal of Systems Engineering and Electronics, 26(1), 96-102.",
              "DOI:10.1109/JSEE.2015.00013",
              sep = "\n")
      })

      actual <- datimv()
      A1 <- actual[,1]
      A2 <- actual[,2]
      a1 <- t(A1)
      n <- length(a1)
      fitted <- x0capigndgm12()
      f1 <- fitted[,1]
      f2 <- fitted[,2]
      f3 <- fitted[,3]
      F1 <- f1[1:n]
      F2 <- f2[1:n]
      F3 <- f3[1:n]
      dfigndgm <- data.frame(A1,A2,F1,F2,F3)
      colnames(dfigndgm , do.NULL = FALSE)
      colnames(dfigndgm ) <- c("Lower Bound","Upper Bound","Fitted Lower","Fitted Upper","Fitted Mean")
      dfigndgm

    }
  })


  #' Interval Multivariate Models - Actual and Fitted - render - in and out sample models
  #' original and fitted data
  output$x0capoutimv <- renderTable({
    if (input$radiooutimv == "MDBGM (1, 2) model") {

      output$outimv <- renderText({
        paste("MDBGM (1, 2) model: Multivariate grey model based on dynamic background algorithm. ",
              "The MDBGM (1, 2) function takes as input raw values and output predicted values following the paper by",
              "Zeng X, Yan S, He F, Shi Y (2019). Multivariable Grey Model based on Dynamic Background",
              "Algorithm for Forecasting the Interval Sequence. Applied Mathematical Modelling, 80(23).",
              "DOI:10.1016/j.apm.2019.11.032",
              sep = "\n")
      })


      actual <- datinimv()
      A1 <- actual[,1]
      A2 <- actual[,2]
      a1 <- t(A1)
      n <- length(a1)
      fitted <- x0capmdbgm12()
      f1 <- fitted[,1]
      f2 <- fitted[,2]
      F1 <- f1[1:n]
      F2 <- f2[1:n]
      dfmdbgm <- data.frame(F1,F2)
      colnames(dfmdbgm  , do.NULL = FALSE)
      colnames(dfmdbgm  ) <- c("Fitted Lower Bound","Fitted Upper Bound")
      dfmdbgm

    }
  })



  #' Predicted values - render - in sample
  #' Multivariate Interval sequences
  output$predictedimv <- renderTable({

    if (input$radioimv == "IG-NDGM (1, 2) model") {

      output$imvpv <- renderPrint({input$radioimv1})

      x0cap2 <- x0capigndgm12()
      pred <- tail(x0cap2,4)
      f1 <- pred[,1]
      f2 <- pred[,2]
      f3 <- pred[,3]
      x <- input$radioimv1
      Lower <- f1[1:x]
      Upper <- f2[1:x]
      Mean <- f3[1:x]
      dfigndgm <- data.frame(Lower,Upper,Mean)

    }
  })


  #' Predicted values - render  - in sample and out sample
  #' Multivariate Interval sequences
  output$predictedoutimv <- renderTable({

    if (input$radiooutimv == "MDBGM (1, 2) model") {

      output$imvoutpv <- renderPrint({input$radiooutimv1})

      x0cap2 <- x0capmdbgm12()
      pred <- tail(x0cap2,4)
      f1 <- pred[,1]
      f2 <- pred[,2]
      x <- input$radiooutimv1
      Lower <- f1[1:x]
      Upper <- f2[1:x]
      dfmdbgm <- data.frame(Lower,Upper)
      colnames(dfmdbgm, do.NULL = FALSE)
      colnames(dfmdbgm) <- c("Predicted Lower Bound","Predicted Upper Bound")
      dfmdbgm

    }
  })


  #' MAPE and RMSE - render - in sample
  #' Multivariate Interval sequences
  output$errorimv <- renderTable({

    if (input$radioimv == "IG-NDGM (1, 2) model") {

      actual1 <- datimv()
      Actual <- actual1[,1]
      n <- length(Actual)
      fitted1 <- x0capigndgm12()
      f1 <- fitted1[,1]
      fitted2 <- t(f1)
      Fitted <- fitted2[1:n]
      MAPE <- mape(Actual,Fitted)*100
      RMSE <- rmse(Actual,Fitted)
      pe <- data.frame(MAPE,RMSE)

    }
  })

  #' MAPE and RMSE - render - in and out smaple
  #' Multivariate Interval sequences
  output$erroroutimv <- renderTable({

    if (input$radiooutimv == "MDBGM (1, 2) model") {


      actual1 <- datinimv()
      Actual1 <- actual1[,1]
      Actual2 <- actual1[,2]
      n <- length(Actual1)
      fitted1 <- x0capmdbgm12()
      f1 <- t(fitted1[,1])
      f2 <- t(fitted1[,2])
      Fitted_seq1 <- f1[1:n]
      Fitted_seq2 <- f2[1:n]
      MAPE_seq1L <- mape(Actual1,Fitted_seq1)*100
      MAPE_seq1U <- mape(Actual2,Fitted_seq2)*100

      RMSE_seq1U <- rmse(Actual1,Fitted_seq1)
      RMSE_seq2U <- rmse(Actual2,Fitted_seq2)

      pe <- matrix(c(MAPE_seq1L,MAPE_seq1U,RMSE_seq1U,RMSE_seq2U),ncol=2)
      colnames(pe, do.NULL = FALSE)
      colnames(pe) <- c("MAPE","RMSE")
      pe


    }
  })



  #' Confidence Intervals - render - in sample
  #' Multivariate Interval sequences
  output$intervalsimv <- renderTable({

    if (input$radioimv == "IG-NDGM (1, 2) model") {

      if (input$radioimv2 == "90") {

        output$ciimv <- renderPrint({input$radioimv2})

        actual1 <- datimv()
        fp11 <- x0capigndgm12()
        x <- input$radioimv1
        ci <- 90

        source("CIplots_igndgm12.R")
        CIvalue(fp11,actual1,x,ci)


      } else if (input$radioimv2 == "95") {

        output$ciimv <- renderPrint({input$radioimv2})

        actual1 <- datimv()
        fp11 <- x0capigndgm12()
        x <- input$radioimv1
        ci <- 95

        source("CIplots_igndgm12.R")
        CIvalue(fp11,actual1,x,ci)

      } else if (input$radioimv2 == "99") {

        output$ciimv <- renderPrint({input$radioimv2})

        actual1 <- datimv()
        fp11 <- x0capigndgm12()
        x <- input$radioimv1
        ci <- 99

        source("CIplots_igndgm12.R")
        CIvalue(fp11,actual1,x,ci)

      }

    }
  })


  #' Confidence Intervals - render - in and out sample
  #' Multivariate Interval sequences
  output$intervalsoutimv <- renderTable({

    if (input$radiooutimv == "MDBGM (1, 2) model") {

      if (input$radiooutimv2 == "90") {

        output$cioutimv <- renderPrint({input$radiooutimv2})

        actual1 <- datinimv()
        fitted1 <- x0capmdbgm12()
        x <- input$radiooutimv1
        ci <- 90

        source("CIplots_MDBGM.R")
        CI_mdbgm(fitted1,actual1,x,ci)



      } else if (input$radiooutimv2 == "95") {

        output$cioutimv <- renderPrint({input$radiooutimv2})

        actual1 <- datinimv()
        fp1 <- x0capmdbgm12()
        x <- input$radiooutimv1
        ci <- 95

        source("CIplots_MDBGM.R")
        CI_mdbgm(fp1,actual1,x,ci)


      } else if (input$radiooutimv2 == "99") {

        output$cioutimv <- renderPrint({input$radiooutimv2})

        actual1 <- datinimv()
        fp1 <- x0capmdbgm12()
        x <- input$radiooutimv1
        ci <- 99

        source("CIplots_MDBGM.R")
        CI_mdbgm(fp1,actual1,x,ci)
      }
    }
  })


  #' Plots - render - in sample
  #' Multivariate Interval sequences
  output$plotimv <- renderPlotly({

    if (input$radioimv == "IG-NDGM (1, 2) model") {

      if (input$radioimv2 == "90") {

        actual <- datimv()
        pred <- x0capigndgm12()
        ci <- 90
        model <- 'IG-NDGM (1, 2) model'

        source("CIplots_igndgm12.R")
        plotsigndgm(actual,pred,ci,model)


      } else if (input$radioimv2 == "95") {

        actual <- datimv()
        pred <- x0capigndgm12()
        ci <- 95
        model <- 'IG-NDGM (1, 2) model'

        source("CIplots_igndgm12.R")
        plotsigndgm(actual,pred,ci,model)

      } else if (input$radioimv2 == "99") {

        actual <- datimv()
        pred <- x0capigndgm12()
        ci <- 99
        model <- 'IG-NDGM (1, 2) model'

        source("CIplots_igndgm12.R")
        plotsigndgm(actual,pred,ci,model)

      }
    }
  })



  #' Plots - render - in and out sample
  #' Multivariate Interval sequences
  output$plotoutimv <- renderPlotly({

    if (input$radiooutimv == "MDBGM (1, 2) model") {

      if (input$radiooutimv2 == "90") {

        actual <- datinimv()
        pred <- x0capmdbgm12()
        ci <- 90
        model <- 'MDBGM (1, 2) model'

        source("CIplots_MDBGM.R")
        plots_mdbgm12(actual,pred,ci,model)


      } else if (input$radiooutimv2 == "95") {

        actual <- datinimv()
        pred <- x0capmdbgm12()
        ci <- 95
        model <- 'MDBGM (1, 2) model'

        source("CIplots_MDBGM.R")
        plots_mdbgm12(actual,pred,ci,model)


      } else if (input$radiooutimv2 == "99") {


        actual <- datinimv()
        pred <- x0capmdbgm12()
        ci <- 99
        model <- 'MDBGM (1, 2) model'

        source("CIplots_MDBGM.R")
        plots_mdbgm12(actual,pred,ci,model)

      }
    }
  })


  #' END
}

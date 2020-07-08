server <- function(input, output) {

  ## UI ----
  # Data input
  d <- reactive({
    if (input$file != "") {
      message("read input data")
      env <- attach(input$file)
      d <- env[[ls(env)[1]]]

      # tweak
      d[1:100,]
      d$..id <- 1:nrow(d)
      d$pressure <- -d$pressure
      d$month <- factor(lubridate::month(d$date))
      d$year <- lubridate::year(d$date)
    } else {
      d <- NULL
    }

    d
  })

  qc <- reactive({
    if (!is.null(d())) {
      message("prepare qc table")
      qc <- d()
      qc[,] <- NA
      qc$..id <- 1:nrow(qc)
      qc
    }
  })

  # make the qc table into a reactive object to be able to change it
  r <- reactiveValues()
  r$qc <- isolate({qc()})

  vars <- reactive({
    message("list potential variables")
    names(select(d(), -`..id`))
  })
  vars_continuous <- reactive({
    is_continuous <- function(x) {
      !(is.character(x) | is.factor(x))
    }
    names(select_if(select(d(), -`..id`), is_continuous))
  })

  # Data selection
  output$x_ui <- renderUI({ selectInput("x", "X", choices=c("", vars_continuous()), width="100%") })
  output$xzoom_ui <- renderUI({
    # NB: on first loading of the app, input$x is unset (NULL), then set the to first option (which is "")
    if ( !is.null(input$y) ) {
      if (input$x != "") {
        autoInput(d()[input$x], inputId="xzoom", label=NULL, selected_fraction=1, width="100%")
      }
    }
  })
  output$y_ui <- renderUI({ selectInput("y", "Y", choices=c("", vars_continuous()), width="100%") })
  output$yzoom_ui <- renderUI({
    if ( !is.null(input$y) ) {
      if (input$y != "") {
        autoInput(d()[input$y], inputId="yzoom", label=NULL, selected_fraction=1, width="100%")
      }
    }
  })

  output$filter_vars_ui <- renderUI({
    selectInput("filter_vars", "Filter data points based on", choices=vars(), multiple=TRUE, width="100%")
  })
  output$active_filters_ui <- renderUI({
    lapply(input$filter_vars, function(v) {
      autoInput(d()[v], inputId=paste0("active_", v), label=v, selected_fraction=0.1, width="100%")
    })
  })
  output$background_filters_ui <- renderUI({
    lapply(input$filter_vars, function(v) {
      autoInput(d()[v], inputId=paste0("background_", v), label=v, selected_fraction=0.1, width="100%")
    })
  })

  output$flag_vars_ui <- renderUI({
    selectInput("flag_vars", "Flag", choices=vars(), multiple=TRUE, width="100%")
  })
  output$flag_info_ui <- renderUI({
    if (is.null(input$flag_vars)) {
      helpText("Select one or several variables to flag an the flagging buttons will appear here.")
    } else if (length(intersect(c(input$x, input$y), input$flag_vars)) == 0) {
      div(
        class="alert alert-danger",
        paste0("You are plotting '", input$x, "' and '", input$y, "' but flagging '", paste(input$flag_vars, collapse=","), "'. This is likely a mistake. Some variables should probably be in common.")
      )
    }
  })


  ## Filter data ----
  # valid
  dv <- reactive({
    message("select valid, visible data")
    d <- d()
    # remove data flagged bad or dubious
    d[r$qc == 3] <- NA
    d[r$qc == 4] <- NA
    # remove data based on zoom
    if (!is.null(input$xzoom) & !is.null(input$yzoom)) {
      d <- filter(d,
        between(UQ(sym(input$x)), input$xzoom[1], input$xzoom[2]),
        between(UQ(sym(input$y)), input$yzoom[1], input$yzoom[2])
      )
    }
    d
  })
  # active
  da <- reactive({
    message("select active data")
    d <- dv()
    for (var in input$filter_vars) {
      d <- apply_filter(d, var, input[[paste0("active_", var)]])
    }
    d
  })
  # background
  db <- reactive({
    if (input$show_background) {
      message("select background data")
      d <- dv()
      for (var in input$filter_vars) {
        d <- apply_filter(d, var, input[[paste0("background_", var)]])
      }
    } else {
      d <- NULL
    }
    d
  })
  # brushed
  ds <- reactive({
    message("select brushed data")
    x <- brushedPoints(da(), input$brush, xvar=input$x, yvar=input$y)
    # NB: this adds all NA lines when one of the two variables being brushed is NA, remove them
    x <- x[!is.na(x$..id),]
    x
  })

  ## Plot data ----
  output$plot1 <- renderPlot({
    if (!is.null(input$xzoom) & !is.null(input$yzoom)) {
      message("draw plot")
      par(mar=c(2,2,0,0)+0.05)
      plot(  dv()[,input$x], dv()[,input$y], type="n", xlab=input$x, ylab=input$y)
      points(db()[,input$x], db()[,input$y], col=alpha("grey", a(nrow(db()), 10^5)), pch=16, cex=0.5)
      points(da()[,input$x], da()[,input$y], col=alpha("black", a(nrow(da()), 10^5)), pch=16, cex=0.7)
      points(ds()[,input$x], ds()[,input$y], pch=16, cex=1, col="red")
    }
  })

  ## Flag data ----
  observeEvent(input$bad, {
    r$qc[ds()$..id,input$flag_vars] <- 4
    message("bad: ", sum(r$qc == 4, na.rm=TRUE))
  })
  observeEvent(input$dubious, {
    r$qc[ds()$..id,input$flag_vars] <- 3
    message("dubious: ", sum(r$qc == 3, na.rm=TRUE))
  })
  observeEvent(input$good, {
    r$qc[ds()$..id,input$flag_vars] <- 2
    message("good: ", sum(r$qc == 2, na.rm=TRUE))
  })

  ## Info ----
  output$info <- renderText({
    b <- input$brush
    if (is.null(b)) {
      x <- ""
    } else {
      # TODO trace several variables here, based on ds()
      x <- paste0(
        "Flagging:\n",
        "  ", input$x, " in ", format_range(c(b$xmin, b$xmax)), "\n",
        "  ", input$y, " in ", format_range(c(b$ymin, b$ymax))
      )
    }
    x
  })
}

qc <- function(d) {
  library("shiny")
  # source("lib/input-auto.R")
  library("rlang")
  library("lubridate")
  library("tidyverse")

  apply_filter <- function(d, var, values) {
    x <- d[,var]
    if (is.character(x) | is.factor(x)) {
      o <- filter(d, UQ(sym(var)) %in% values)
    } else {
      o <- filter(d, between(UQ(sym(var)), values[1], values[2]))
    }
    return(o)
  }

  # tweak
  d$month <- factor(lubridate::month(d$date))
  d$year <- lubridate::year(d$date)
  vars <- names(d)
  vars_continuous <- vars[sapply(d, function(x) {!(is.character(x) | is.factor(x))})]

  d$..id <- 1:nrow(d)

  qc <- d
  qc[,] <- NA
  qc$..id <- 1:nrow(qc)
  qc


  server <- function(input, output) {
    # make the qc table into a reactive object to be able to change it
    r <- reactiveValues(qc=qc)

    ## UI ----
    # Data selection
    output$xzoom_ui <- renderUI({
      # NB: on first loading of the app, input$x is unset (NULL), then set the to first option (which is "")
      if ( !is.null(input$y) ) {
        if (input$x != "") {
          autoInput(d[input$x], inputId="xzoom", label=NULL, selected_fraction=1, width="100%")
        }
      }
    })
    output$yzoom_ui <- renderUI({
      if ( !is.null(input$y) ) {
        if (input$y != "") {
          autoInput(d[input$y], inputId="yzoom", label=NULL, selected_fraction=1, width="100%")
        }
      }
    })

    output$active_filters_ui <- renderUI({
      lapply(input$filter_vars, function(v) {
        autoInput(d[v], inputId=paste0("active_", v), label=v, selected_fraction=0.1, width="100%")
      })
    })
    output$background_filters_ui <- renderUI({
      lapply(input$filter_vars, function(v) {
        autoInput(d[v], inputId=paste0("background_", v), label=v, selected_fraction=0.1, width="100%")
      })
    })

    output$flag_info_ui <- renderUI({
      if (is.null(input$flag_vars)) {
        helpText("Select one or several variables to flag and the flagging buttons will appear here.")
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
      dv <- d
      # remove data flagged bad or dubious
      dv[r$qc == 3] <- NA
      dv[r$qc == 4] <- NA
      # remove data based on zoom
      if (!is.null(input$xzoom) & !is.null(input$yzoom)) {
        dv <- filter(dv,
          between(UQ(sym(input$x)), input$xzoom[1], input$xzoom[2]),
          between(UQ(sym(input$y)), input$yzoom[1], input$yzoom[2])
        )
      }
      dv
    })
    # active
    da <- reactive({
      message("select active data")
      da <- dv()
      for (var in input$filter_vars) {
        da <- apply_filter(da, var, input[[paste0("active_", var)]])
      }
      da
    })
    # background
    db <- reactive({
      if (input$show_background) {
        message("select background data")
        db <- dv()
        for (var in input$filter_vars) {
          db <- apply_filter(db, var, input[[paste0("background_", var)]])
        }
      } else {
        db <- NULL
      }
      db
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
        # smoothScatter()
        plot(  dv()[,input$x], dv()[,input$y], type="n", xlab=input$x, ylab=input$y)
        points(db()[,input$x], db()[,input$y], col="grey70", pch=".")
        points(da()[,input$x], da()[,input$y], col="black", pch=".", cex=2)
        points(ds()[,input$x], ds()[,input$y], pch=".", cex=3, col="red")
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

  shinyApp(ui, server)
}

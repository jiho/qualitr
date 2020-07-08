#' Function defining the UI
myui <- function(request) {
  list(
    # fill plot
    tags$head(tags$style("#plot1 {height:90vh !important;}")),
    # title
    fluidPage(
      titlePanel("QC Data flagger"),

      fluidRow(

      div(class=("col-sm-4 col-md-3 col-lg-3"),
        # XY selectors and filter
        fluidRow(
          column(6,
            selectInput("x", "X", choices=c("", vars_continuous), width="100%"),
            uiOutput("xzoom_ui")
          ),
          column(6,
            selectInput("y", "Y", choices=c("", vars_continuous), width="100%"),
            uiOutput("yzoom_ui")
          )
        ),

        # Dynamic filters
        selectInput("filter_vars", "Filter data points based on", choices=vars, multiple=TRUE, width="100%"),
        wellPanel(uiOutput("active_filters_ui")),
        checkboxInput("show_background", "Show background data", value=FALSE),
        conditionalPanel(
          "input.show_background == 1",
          wellPanel(
            uiOutput("background_filters_ui")
          )
        ),

        # Download button
        downloadButton("download", "Download data and flags", class="btn-default btn-block", width="100%"),
        div()
      ),

      div(class=("col-sm-6 col-md-7 col-lg-8"),
        # flag tools
        # plot
        plotOutput("plot1",
          brush=brushOpts("brush", fill="grey", stroke="white", opacity=0.3, delayType="debounce", delay=500, resetOnNew=FALSE),
          height="auto"
        ),
        # info about the brushed region
        tagAppendAttributes(textOutput("info"), style="white-space:pre-wrap;")
      ),
      div(class=("col-sm-2 col-md-2 col-lg-1"),
        selectInput("flag_vars", "Flag", choices=vars, multiple=TRUE, width="100%"),
        uiOutput("flag_info_ui"),
        conditionalPanel(
          "input.flag_vars != null",
          actionLink("bad", "Bad", class="btn btn-lg btn-danger btn-block"),
          div(style="height: 10px"),
          actionLink("dubious", "Dubious", class="btn btn-lg btn-warning btn-block"),
          div(style="height: 10px"),
          actionLink("good", "Good", class="btn btn-lg btn-success btn-block")
        )
      )
    ))
  )
}

source("lib/lib_shiny.R")

# create a various types of data
n <- 40
d <- data.frame(
  bool=rep(c(TRUE, FALSE), times=n/2),
  few_levels=letters[1:5],
  many_levels=rep(letters,2)[1:n],
  int=1:n,
  num=runif(n, 0, 5),
  large_num=runif(n, 0, 10^6),
  small_num=runif(n, 0, 0.001),
  date=Sys.Date()+1:n,
  date_time=now()+1:n
)
str(d)

ui <- fluidPage(
  fluidRow(
    column(
      6,
      h2("Examples"),
      autoInput(d["bool"]),
      autoInput(d["few_levels"]),
      autoInput(d["many_levels"]),
      autoInput(d["int"]),
      autoInput(d["num"]),
      autoInput(d["large_num"]),
      autoInput(d["small_num"]),
      autoInput(d["date"]),
      autoInput(d["date"], range=F),
      autoInput(d["date_time"]),
      autoInput(d["date_time"], range=F),
      div()
    ),
    column(
      6,
      h2("Options"),
      autoInput(d["int"], width="100%", inline=TRUE),
      autoInput(d["int"], selected_fraction=0.5),
      autoInput(d["int"], selected_fraction=0),
      autoInput(d["char"], selected_fraction=0),
      autoInput(d["char"], selected_fraction=0.0001),
      autoInput(d["char"], inline=FALSE),
      autoInput(d["small_num"], sep=""),
      autoInput(d["small_num"], ticks=FALSE),
      div()
    )
  )
)
server <- function(input, output) {}
shinyApp(ui, server)

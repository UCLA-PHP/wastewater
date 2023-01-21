#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom dplyr summarize group_by filter rename
#' @importFrom lubridate year today %within% int_diff
#' @importFrom plotly renderPlotly
#' @importFrom shewhart.hybrid PH_Chart plot_run_chart
#' @importFrom shiny Progress updateDateRangeInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom stringr str_replace_all
app_server <- function(input, output, session) {
  # Your application server logic
  setBookmarkExclude(c("bookmark1", "goButton"))

  observeEvent(
    input$bookmark1,
    session$doBookmark())

  onBookmarked(
    function(url) {
      # updateQueryString(url)
      showBookmarkUrlModal(url)
    }
  )

  data_source =
    paste("Presaved data, downloaded ",
          attr(presaved_data, "date")) |>
    reactiveVal()

  last_load_attempt = reactiveVal(NA)
  last_load_result = reactiveVal(NA)

  full_dataset = reactiveVal(presaved_data)

  observeEvent(
    eventExpr = input$reload_data,
    # ignoreNULL = TRUE,
    ignoreNULL = FALSE, #IgnoreNull makes it run initially,
    ignoreInit = FALSE, # ignoreInit makes it so it wont run twice (oddly)
    {
      progress <- shiny::Progress$new()
      progress$set(message = "Loading in Data", value = 0)

      last_load_attempt(Sys.time())

      cli::cli_alert('About to load data')
      cur_data = try(load_data())

      if(inherits(cur_data, "try-error"))
      {
        last_load_result("failed")
        cli::cli_alert_warning('Verily Database is Not Available, Using Backed Up Data')
        progress$set(message = "Verily database unavailable", value = 1)
        Sys.sleep(1)
      } else
      {
        message('Verily Data is Loaded')
        last_load_result("succeeded")
        data_source(paste("Verily database: downloaded", Sys.time()))
        attr(cur_data, "date") = Sys.time()
        full_dataset(cur_data)
      }

      progress$set(message = "Ready to Analyze", value = 1)
      Sys.sleep(1)
      progress$close()
    })

  sites =
    full_dataset() |>
    get_sites() |>
    reactive()

  updatePickerInput(
    session = session,
    inputId = "plants",
    choices =
      sites() |>
      arrange(Plant) %$%
      split(x = Plant, f = State),
    selected = input$plants
  ) |> observeEvent(sites())

  output$data_source = renderText(data_source())
  output$last_connection_time =
    glue::glue(
      as.character(last_load_attempt()),
      " ({last_load_result()})") |>
    renderText()

  cur_subset =
    full_dataset() |>
    filter(
      Plant %in% input$plants,
      Collection_Date %within% (input$dates |> lubridate::int_diff())
    ) |>
    reactive()

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(
        'PH_chart-',
        Sys.time() |> gsub(pattern = "\\:", replacement = "-"),
        '.csv',
        sep = '')
    },
    content = function(con) {
      write.csv(chart1(), con)
    }
  )


  y_var_name =
    input$virus |>
    paste(
      sep = "_",
      if_else(
        input$normalize,
        measures[2],
        measures[1])) |>
    reactive()

  chart1 =
    {
      progressData <- shiny::Progress$new(session = session, min = 0, max = 1)
      progressData$set(message = "Creating Chart", value = 0)

      on.exit(progressData$set(message = "Displaying Chart", value = 1))
      on.exit(Sys.sleep(1), add = TRUE)
      on.exit(progressData$close(), add = TRUE)

      validate(need(nrow(cur_subset()) > 0, "No data found for these filter settings."))
      validate(need(input$Lim_Min > 0, "Need a minimum phase length > 0."))

      cur_subset() |>
        dplyr::mutate(
          date = Collection_Date,
          Dot = .data[[y_var_name()]]
        ) |>
        shewhart.hybrid::I_Chart(
          Lim_Min = input$Lim_Min |> ceiling(),
          extra_vars = names(full_dataset()),
          verbose = TRUE,
          ymin = 0,
          digits = 3,
          multiplier = if_else(input$normalize, 100, 1),
          observations =
            if_else(input$normalize,
                    "Measured relative concentration: ",
                    "Measured concentration: "),
          suffix =
            if_else(input$normalize,
                    "%",
                    " copies per gram")

        ) |>
        suppressWarnings()

    } |>
    reactive()
  # eventReactive(
  #   # ignoreNULL = FALSE,
  #   eventExpr = input$goButton)

  output$graph1 =
    chart1() |>
    shewhart.hybrid::plot_run_chart(
      yvarname = "Dot",
      multiplier = if_else(input$normalize, 100, 1),
      suffix =
        if_else(input$normalize,
                "%",
                " copies per gram"),
      yname = ifelse(input$normalize,
                     "Relative concentration (%)",
                     "Concentration (copies per gram)"),
      point_name = "Observed value",
      digits = 3,
      verbose = TRUE) |>
    plotly::renderPlotly()

}

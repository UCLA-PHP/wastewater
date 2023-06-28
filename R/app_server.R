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
      showBookmarkUrlModal(url)
    }
  )

  data_source =
    paste("Presaved data, downloaded ",
          attr(presaved_data, "date")) |>
    reactiveVal(label = "data_source")

  last_load_attempt = reactiveVal(label = "last load attempt")
  last_load_result = reactiveVal(label = "last load result")

  full_dataset = reactiveVal(presaved_data, label = "full_dataset")

  observeEvent(
    label = "reload_data",
    eventExpr = input$reload_data,
    ignoreNULL = FALSE, #IgnoreNull = FALSE makes it run initially,
    ignoreInit = FALSE, # ignoreInit = TRUE makes it so it wont run twice if input$reload_data is generated dynamically
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
    {
      full_dataset() |>
        get_sites()
    } |>
    reactive(label = "sites")

  updatePickerInput(
    session = session,
    inputId = "plants",
    choices =
      sites() |>
      arrange(Plant) %$%
      split(x = Plant, f = State),
    selected = input$plants
  ) |>
    observeEvent(
      eventExpr = sites(),
      label = "update plants list")

  output$data_source = data_source() |> renderText()
  output$last_connection_time =
    glue::glue(
      as.character(last_load_attempt()),
      " (",
      last_load_result() |> as.character(),
      ")") |>
    renderText()

  cur_subset =
    full_dataset() |>
    filter(
      Plant %in% input$plants,
      Collection_Date %within% (input$dates |> lubridate::int_diff())
    ) |>
    reactive(label = "cur_subset")

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(
        'PH_chart-',
        Sys.time() |> gsub(pattern = "\\:", replacement = "-"),
        '.csv',
        sep = '')
    },
    content = function(con) write.csv(chart1(), con)
  )

  y_var_name =
    input$virus |>
    paste(
      sep = "_",
      if_else(
        input$normalize,
        measures[2],
        measures[1])) |>
    reactive(label = "y_var_name")

  filterData =  cur_subset() |>
    dplyr::mutate(
      date = Collection_Date,
      place = Plant,
      Dot = .data[[y_var_name()]]
    ) |>
    reactive(label = "filterData")

  chart1 =
    {
      progressData <- shiny::Progress$new(session = session, min = 0, max = 1)
      progressData$set(message = "Creating Chart", value = 0)

      on.exit(progressData$set(message = "Displaying Chart", value = 1))
      on.exit(Sys.sleep(1), add = TRUE)
      on.exit(progressData$close(), add = TRUE)

      validate(need(nrow(cur_subset()) > 0, "No data found for these filter settings."))
      validate(need(input$Lim_Min > 0, "Need a minimum phase length > 0."))

      if(length(input$plants) == 1){
        filterData() |>
          shewhart.hybrid::I_Chart(
            Lim_Min = input$Lim_Min |> ceiling(),
            extra_vars = names(cur_subset()),
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

          )
      }

      else{
          filterData()  |>
          group_by(date) |>
          summarise(
            .groups = "drop",
            count = sum(!is.na(Dot)),
            Dot_mn = mean(Dot, na.rm = TRUE),
            Dot_sd = sd(Dot, na.rm = TRUE)
          )|>
          shewhart.hybrid::XBar_Chart(
            Lim_Min = input$Lim_Min |> ceiling(),
            extra_vars = names(cur_subset()),
            verbose = TRUE,
            ymin = 0,
            digits = 3,
            multiplier = if_else(input$normalize, 100, 1),
            groups = "Monitoring Locations",
            observations =
              if_else(input$normalize,
                      "Mean Measured relative concentration",
                      "Mean Measured concentration"),
            suffix =
              if_else(input$normalize,
                      "%",
                      " copies per gram")

          )} |>
        suppressWarnings()

    } |>
    reactive(label = "chart1")

  chart2 = {

    plotly::plot_ly(
      cur_subset(),
      x = ~Collection_Date,
      y = ~.data[[y_var_name()]],
      color = ~Plant,
      type = "scatter",
      mode = "lines+markers"
    ) |>
      plotly::layout(title = "Rates per Site",
                     yaxis = list(title = yname()),
                     xaxis = list(title = NA),
                     legend = list(orientation = 'h')
      )

  } |> reactive(label = "chart2")

  chart3 = {
    cur_subset() |>
      group_by(Collection_Date) |>
      summarise(Locations = n()) |>
      plotly::plot_ly(
        x = ~Collection_Date,
        y = ~Locations,
        type = "bar",
        mode = "lines"
      ) |>
      plotly::layout(title = "Number of Locations",
        xaxis = list(title = NA),
        yaxis = list(rangemode = "tozero"),
        updatemenus = list(
          list(
            type = "buttons",
            buttons = list(

              list(
                method = "restyle",
                args = list("type", "bar"),
                label = "Bar Chart"),

              list(
                method = "restyle",
                args = list("type", "scatter"),
                label = "Line Plot")
              )
            )

          )
        )


  } |> reactive(label = "chart3")

  chart4 = {
      filterData()  |>
      group_by(date) |>
      summarise(
        .groups = "drop",
        count = sum(!is.na(Dot)),
        Dot_sd = sd(Dot, na.rm = TRUE)
      )|>
      shewhart.hybrid::S_Chart(
        Lim_Min = input$Lim_Min |> ceiling(),
        extra_vars = names(cur_subset()),
        verbose = TRUE,
        ymin = 0,
        digits = 3,
        multiplier = if_else(input$normalize, 100, 1),
        groups = "Monitoring Locations",
        observations =
          if_else(input$normalize,
                  "Measured relative concentration",
                  "Measured concentration"),
        suffix =
          if_else(input$normalize,
                  "%",
                  " copies per gram")

      ) |>
    suppressWarnings()
  } |> reactive(label = "chart4")


  outPlot <- reactive({
    if(length(input$plants) == 1){
      mainPlot <- chart1() |>
        shewhart.hybrid::plot_run_chart(
          yvarname = "Dot",
          multiplier = if_else(input$normalize, 100, 1),
          suffix =
            if_else(input$normalize,
                    "%",
                    " copies per gram"),
          yname = yname(),
          point_name = "Observed value",
          digits = 3,
          verbose = TRUE)

    }
    else{
      mainPlot <- chart1() |>
        shewhart.hybrid::plot_run_chart(
          title = "X-Bar Chart",
          yvarname = "Observed Mean",
          modeUpper = "lines+markers",
          marker_size = ~count,
          marker_size_range = c(20,100),
          multiplier = if_else(input$normalize, 100, 1),
          suffix =
            if_else(input$normalize,
                    "%",
                    " copies per gram"),
          yname = yname_Mean(),
          point_name = "Observed value",
          digits = 3,
          verbose = TRUE)
    }
    mainPlot
  }
  )

  yname <- reactive({
    ifelse(input$normalize,
           "Relative concentration (%)",
           "Concentration (copies per gram)")
  })
  yname_Mean <- reactive({
    ifelse(input$normalize,
           "Mean Relative concentration (%)",
           "Mean Concentration (copies per gram)")
  })
  yname_SD <- reactive({
    ifelse(input$normalize,
           "SD of Relative concentration (%)",
           "SD of Concentration (copies per gram)")
  })

  individualSitesPlot <- reactive({
    if(length(input$plants) > 1){
      output_ISP = chart2()
    }
    else{
      output_ISP = NULL
    }
    output_ISP
  })

  locationsPlot <- reactive({
    if(length(input$plants) > 1){
      output_LP = chart3()
    }
    else{
      output_LP = NULL
    }
    output_LP
  })

  sPlot <- reactive({
    if(length(input$plants) > 1){
      output_SP <- chart4() |>
        shewhart.hybrid::plot_run_chart(
          title = "S Chart",
          yvarname = "Observed SD",
          modeUpper = "lines",
          marker_size = ~count,
          marker_size_range = c(20,100),
          multiplier = if_else(input$normalize, 100, 1),
          suffix =
            if_else(input$normalize,
                    "%",
                    " copies per gram"),
          yname = yname_SD(),
          point_name = "Observed value",
          digits = 3,
          verbose = TRUE)

    }
    else{
      output_SP = NULL
    }
    output_SP
  })

  output$graph1 =
    outPlot() |>
  plotly::renderPlotly()

  output$graph2 = individualSitesPlot() |>
    plotly::renderPlotly()

  output$graph3 = locationsPlot() |>
    plotly::renderPlotly()

  output$graph4 = sPlot() |>
    plotly::renderPlotly()

}




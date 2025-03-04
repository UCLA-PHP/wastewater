#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @importFrom plotly plotlyOutput
#' @importFrom lubridate as_date
#'
#' @importFrom lubridate today
#' @importFrom plotly plotlyOutput
#' @importFrom shinyWidgets pickerInput pickerOptions
app_ui <- function(request) {

  Variant =

    tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      # Your application UI logic
      fluidPage(
        h1("Wastewater"),
        sidebarLayout(
          sidebarPanel(
            bookmarkButton(
              id = "bookmark1",
              label = "Bookmark current inputs"),
            # actionButton(
            #   "Build Charts",
            #   inputId = "goButton",
            #   class = "btn-success"),

            br(),
            br(),

            dateRangeInput(
              inputId = "dates",
              label = "Dates to include in SPC analysis",
              start = presaved_data$Collection_Date |> min(),
              end = lubridate::today()
            ),

            shinyWidgets::pickerInput(
              "plants",
              options = shinyWidgets::pickerOptions(
                `actions-box` = TRUE,
                `deselect-all-text` = "None",
                `select-all-text` = "All",
                liveSearch = TRUE,
                dropupAuto = TRUE
              ),
              multiple = TRUE,
              label = "Monitoring Location",
              choices =
                sites |>
                arrange(Plant) %$%
                split(x = Plant, f = State),
              selected = "Davis, CA"

            ),

            shinyWidgets::pickerInput(
              "virus",
              options = shinyWidgets::pickerOptions(
                `actions-box` = TRUE,
                `deselect-all-text` = "None",
                `select-all-text` = "All",
                liveSearch = TRUE,
                dropupAuto = TRUE
              ),
              # multiple = TRUE,
              label = "Virus / Variant",
              choices = targets,
              selected = "SC2_N"

            ),
            #1/11/23 Addition ^
            #https://shinyapps.dreamrs.fr/shinyWidgets/
            shiny::numericInput(
              inputId = "Lim_Min",
              label = "Minimum phase length before a special cause can be detected",
              min = 1,
              step = 1,
              value = 4
            ),


            HTML("<b>Current Data Source: </b>"),
            textOutput("data_source"),
            br(),

            HTML("<b>Last Verily Database Connection Attempt: </b>"),
            textOutput("last_connection_time"),

            br(),
            actionButton(
              inputId = "reload_data",
              label = "Connect to Verily Database",
              class = "btn-success")

          ),
          mainPanel(
            h2("Test Positivity Rates"),
            fluidRow(
              downloadButton('downloadData', 'Download Chart Data')
            ),

            shinydashboard::box(
              width = 12,

              checkboxInput(
                inputId = "normalize",
                label = "PMMoV Normalized",
                value = TRUE),
              htmltools::htmlDependencies(icon("")),
              tags$style(fullscreen_tag1),
              div(
                class = "plotly-full-screen",
              fluidRow(
                column(
                  width = 6,
                  plotly::plotlyOutput("graph1")
                ),
                column(
                  width = 6,
                  #h2("Rates per Site"),
                  plotly::plotlyOutput("graph4")
                )
                ),
              fluidRow(
                column(
                  width = 6,
                  #h2("Number of Locations"),
                  plotly::plotlyOutput("graph2")
                ),
                column(
                  width = 6,
                  #h2("Number of Locations"),
                  plotly::plotlyOutput("graph3")
                )
              )
            )
          ),
          tags$script(HTML(fullscreen_tag2))
          )
        )
      )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Wastewater"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' Title
#'
#' @param dataset some subset of `presaved_CDC_data`
#' @param ... arguments passed to shewhart.hybrid::PH_Chart(...)
#'
#' @return a plotly graph
#' @export
#'
#' @importFrom dplyr rename mutate
#' @importFrom shewhart.hybrid plot_run_chart P_Chart
fv_p_chart = function(
    dataset =
      presaved_data |>
      filter(Plant |> stringr::str_starts("Los Angeles, CA")) |>
      dplyr::mutate(
        `total_specimens` = `total_specimens` |> as.numeric(),
        `TOTAL POSITIVE` = total_a |> as.numeric() + total_b |> as.numeric()),
    ...
)
{


  chart = dataset |>
    dplyr::rename(
      n = `TOTAL POSITIVE`,
      N = `total_specimens`,
      date = wk_date) |>
    shewhart.hybrid::PH_Chart(...)

  chart |>
    shewhart.hybrid::plot_run_chart(suffix = "%")
}

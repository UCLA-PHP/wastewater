load_data = function()
{
  presaved_data =
    vroom::vroom(
      "https://storage.googleapis.com/wastewater-dev-data/scan.csv",
      col_types = vroom_spec
      ) |>
    suppressMessages() |>
    mutate(Plant =
             Plant |> setNames(glue::glue("{Plant}: {Site_Name}"))) |>
    structure(
      "date" = Sys.time(),
      "source" = "https://storage.googleapis.com/wastewater-dev-data/scan.csv")

  duplicates =
    presaved_data |>
    janitor::get_dupes(Site_Name, Collection_Date)

  if(nrow(duplicates) != 0)
  {
    warning(nrow(duplicates), " duplicate records found")
    if(nrow(duplicates) < 10)
    {
      print(duplicates)
    }

  }

  presaved_data =
    presaved_data |>
    group_by(Site_Name, Collection_Date) |>
    tidyr::fill(.direction = "downup") |>
    slice_tail() |>
    ungroup() |>
    magrittr::set_attr("date", attr(presaved_data, "date"))

}

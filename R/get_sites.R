get_sites = function(dataset)
{
  static_info =
    presaved_data |>
    select(
      City:County_FIPS,
      -Population_Served
    )

  check_for_variations =
    static_info |> group_by(Site_Name) |> unique() |> count() |> filter(n() != 1)

  stopifnot(nrow(check_for_variations) == 0)

  sites = static_info |> unique()

  return(sites)

}

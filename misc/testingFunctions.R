
unique(wastewater$Plant[wastewater$State == "Florida"]) |> group_by()
plotly::plot_ly(presaved_data[presaved_data$State == "Florida", ], x = ~Collection_Date, y = ~SC2_N_norm_PMMoV, color = ~Site_Name, type = "scatter", mode = "lines+markers")
#The example to use
test = wastewater |>
  dplyr::mutate(
    place = State,
    date = Collection_Date,
    Dot = .data[["SC2_N_norm_PMMoV"]]
    # count = group_by(place, date) |> summarise(sum(!is.na(.data[["SC2_N_norm_PMMoV"]]))) |> pull(2),
    # Dot_mn = summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)),
    # Dot_sd =  summarise(sd(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE))
  ) |> group_by(place, date) |>
  summarise(
    .groups = "drop",
    count = sum(!is.na(.data[["SC2_N_norm_PMMoV"]])),
    Dot_mn = mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE),
    Dot_sd = sd(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE))

df = as.data.frame(df)
df = test |>
  group_by(place, date) |>
  summarise(
    .groups = "drop",
    count = sum(!is.na(.data[["SC2_N_norm_PMMoV"]])),
    Dot_mn = mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE),
    Dot_sd = sd(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE))

df$Dot_mn = test |> group_by(place, date) |> summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(3)
df$Dot_sd = test |> group_by(place, date) |> summarise(sd(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(3)
df$place = test |> group_by(place, date) |> summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(1)
df$date = test |> group_by(place, date) |> summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(2)

Alabama = wastewater |> filter(State == "Alabama")
bama = NULL
bama = as.data.frame(bama)
bama$count = Alabama |> group_by(Collection_Date) |> summarise(sum(!is.na(.data[["SC2_N_norm_PMMoV"]]))) |> pull(2)
bama$Dot_mn = Alabama |> group_by(Collection_Date) |> summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(2)
bama$Dot_sd = Alabama |> group_by(Collection_Date) |> summarise(sd(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(2)
bama$place = "Alabama"
bama$date = Alabama |> group_by(Collection_Date) |> summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(1)

California = wastewater |> filter(State == "California")
cali = NULL
cali = as.data.frame(cali)
cali$count = California |> group_by(Collection_Date) |> summarise(sum(!is.na(.data[["SC2_N_norm_PMMoV"]]))) |> pull(2)
cali$Dot_mn = California |> group_by(Collection_Date) |> summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(2)
cali$Dot_sd = California |> group_by(Collection_Date) |> summarise(sd(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(2)
cali$place = "California"
cali$date = California |> group_by(Collection_Date) |> summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(1)

plot(cali$date, cali$count, type = "l")

plot(bamaXbar$date, bamaXbar$count, type = "l")

test2 = shewhart.hybrid::XBar_Chart(df)
bamaXbar = shewhart.hybrid::XBar_Chart(bama)

bamaXbar |> shewhart.hybrid::plot_run_chart(
  yvarname = "Observed Mean",
  mode1 = "lines",
  marker_size = ~count,
  multiplier = if_else(TRUE, 100, 1),
  suffix =
    if_else(TRUE,
            "%",
            " copies per gram"),
  yname = ifelse(TRUE,
                 "Relative concentration (%)",
                 "Concentration (copies per gram)"),
  point_name = "Observed value",
  digits = 3,
  verbose = TRUE)


df$cout = mutate(#count = test |> group_by(place, date) |> summarise(sum(!is.na(.data[["SC2_N_norm_PMMoV"]]))) |> pull(2),
  Dot_mn = test |> group_by(place, date) |> summarise(mean(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)) |> pull(2),
  Dot_sd =  summarise(sd(.data[["SC2_N_norm_PMMoV"]], na.rm = TRUE)))
test2 =

  count = test |> summarise(sum(!is.na(Dot))) |> pull(2)

shewhart.hybrid::XBar_Chart(dataset = test, place = test$Site_Name, date = test$Collection_Date,
                            count,
                            Dot_mn = test |> group_by(date) |> summarise(mean(Dot, na.rm = TRUE)) |> pull(2),
                            Dot_sd = test |> group_by(date) |> summarise(sd(Dot, na.rm = TRUE)) |> pull(2),
                            Lim_Min = input$Lim_Min |> ceiling(),
                            extra_vars = names(cur_subset()),
                            verbose = TRUE,
                            ymin = 0,
                            digits = 3,
                            # multiplier = if_else(input$normalize, 100, 1),
                            observations = "Concentration",
                            # if_else(input$normalize,
                            #         "Measured relative concentration: ",
                            #         "Measured concentration: "),
                            suffix = "Percent"
                            # if_else(input$normalize,
                            #         "%",
                            #         " copies per gram")

)


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

test2 = test |> group_by(date) |> summarise(sum(!is.na(Dot))) |> pull(2)
length(test$Dot)
mean(test$Population_Served[test$date == "2022-08-10"], na.rm = TRUE)
length(unique(wastewater$Plant))
length(unique(wastewater$Site_Name))
as.date
browser()

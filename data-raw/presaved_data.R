load_all()

presaved_data = load_data()

sites = presaved_data |> get_sites()

use_data(sites, overwrite = TRUE)

use_data(presaved_data, overwrite = TRUE)


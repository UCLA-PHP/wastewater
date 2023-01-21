measures = c(
  "unnormalized" = "gc_g_dry_weight",
  "normalized" = "norm_PMMoV")

targets = list(
  "SARS-COV-2" =
    c(
      "SARS-CoV-2 (N Gene)" = "SC2_N",
      'SARS-CoV-2 (S Gene)' = "SC2_S",
      "Omicron (BA.4, BA.5 & BQ.*): Mutation: S:HV69-70del" = "HV_69_70_Del",
      "Omicron (BA.4, BA.5 & BQ.*): Mutation: S:LPPA24S" = "SC2_BA_2_LPPA24S"
    ),

  "Influenza" =
    c(
      "Influenza A" = "Influenza_A",
      "Influenza B" = "Influenza_B"
    )
)

targets_rev =
  sapply(
    targets,
    F = function(x)
    {
      temp = names(x)
      names(temp) = x
      temp
    }
  )

names(targets_rev) = NULL
targets_rev = unlist(targets_rev)
usethis::use_data(targets, overwrite = TRUE)
usethis::use_data(targets_rev, overwrite = TRUE)
usethis::use_data(measures, overwrite = TRUE)

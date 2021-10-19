library(purrr)

purrr_namespace <- lsf.str("package:purrr")

map_functions_df <- 
  tibble(name = as.character(purrr_namespace)) |> 
  filter(grepl("map", name))

map_functions_df <- 
  map_functions_df |> 
  separate(name, c("prefix", "suffix_mid", "suffix"), remove = FALSE, fill = "right") |> 
  mutate(
    suffix = ifelse(!is.na(suffix), 
                    paste("map", suffix, sep = "_"),
                    NA),
    suffix = coalesce(suffix, suffix_mid))

prefixe <- sort(unique(map_functions_df$prefix))
suffixe <- sort(unique(map_functions_df$suffix))

expanded_df <- 
  expand_grid(prefixe, suffixe) |> 
  mutate(name = paste(prefixe, suffixe, sep = "_"),
         name = gsub("_NA", "", name),
         name = ifelse(name %in% map_functions_df$name, name, NA)) 

mt <- matrix(expanded_df$name, nrow = length(suffixe),
       ncol = length(prefixe), dimnames = list(suffixe, prefixe)
      ) |> t()

purrr_map_functions <- mt |>
  as_tibble(rownames = "prefix/suffix") |>
  slice(c(2, 3, 5:7)) |>
  select(`prefix/suffix`, chr, dbl, int, lgl, raw, df, dfc, dfr, contains("map_")) |>
  mutate(across(
    .cols = -contains("_"),
    .fns = \(x) coalesce(x, paste0("invoke_map_", cur_column()))
  ),
  across(.fns = \(x) paste0(x, "()")),
  across(.fns = \(x) ifelse(grepl("NA", x), "", x))
  ) |> 
  select(-contains("_")) |> 
  slice(3:5,1,2) |> 
  gt::gt()
purrr_map_functions

mt |> 
  as_tibble(rownames = "prefix/suffix") |> 
  slice(4:5) |> 
  select(`prefix/suffix`, at, `if`)


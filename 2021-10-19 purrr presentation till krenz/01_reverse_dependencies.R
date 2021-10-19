library(tidyverse)
library(rvest)

cran_link <- "https://cran.rstudio.com/src/contrib"

r_packages <-
  read_html(cran_link)

package_file_names <-
  r_packages |>
  html_nodes("a") |>
  html_text()

build_cran_url <-
  function(package) {
    sprintf("https://cran.r-project.org/web/packages/%s/index.html",
            package)
  }

pkg_df <-
  tibble(package = package_file_names) |>
  filter(grepl(".tar.gz", package, fixed = TRUE)) |>
  separate(package, "package", sep = "_", extra = "drop") |>
  mutate(url = build_cran_url(package))

browseURL(pkg_df$url[1])

# Find all tidyverse packages --------------------

tidyverse_url <-
  pkg_df |>
  filter(package == "tidyverse") |>
  pull(url)

browseURL(tidyverse_url)

tidyverse_imports <-
  tidyverse_url |>
  read_html() |>
  html_table() |>
  pluck(1) |>
  slice(3) |>
  pluck(2) |>
  strsplit(" \\([^()]+\\),?") |>
  pluck(1) |>
  trimws()

# found out about tidyverse_packages() too late
tidyverse_imports %in% tidyverse_packages()

pkg_df <-
  pkg_df |>
  mutate(tidyverse_import = package %in% tidyverse_imports)

tidyverse_pkg_df <-
  pkg_df |>
  filter(tidyverse_import) |>
  mutate(raw_html = map(url, read_html))

tidyverse_pkg_df <-
  tidyverse_pkg_df |>
  mutate(reverse_section =
           map(raw_html,
               \(x) html_nodes(x, "table") |>
                 html_table() |>
                 pluck(3)))

tidyverse_pkg_df$reverse_section[[1]]

tidyverse_pkg_df <-
  tidyverse_pkg_df |>
  unnest(cols = reverse_section) |>
  rename(rev_dep_type = X1,
         rev_deps = X2) |>
  mutate(
    rev_dep_type = gsub("Reverse|\\:", "", rev_dep_type),
    rev_deps = strsplit(rev_deps, ", "),
    rev_deps_tv = map(rev_deps, \(x) x[x %in% tidyverse_imports]),
    rev_deps_count = map_dbl(rev_deps, length),
    rev_deps_tv_count = map_dbl(rev_deps_tv, length)
  )

rev_deps_bars <- 
  tidyverse_pkg_df |>
  group_by(package) |>
  summarise(rev_deps_count = sum(rev_deps_count)) |>
  arrange(-rev_deps_count) |>
  ggplot(aes(
    reorder(package, rev_deps_count),
    rev_deps_count,
    fill = package == "purrr"
  )) +
  geom_bar(stat = "identity") +
  guides(fill = 'none') +
  coord_flip() +
  labs(x = "pkg", y = "n") +
  ggtitle(
    "tidyverse reverse dependencies",
    "sum of all reverse imports, depends, suggests, enhances, linking-to"
  )

rev_deps_tv_bars <- 
  tidyverse_pkg_df |>
  group_by(package) |>
  summarise(rev_deps_tv_count = sum(rev_deps_tv_count)) |>
  arrange(-rev_deps_tv_count) |>
  ggplot(aes(
    reorder(package, rev_deps_tv_count),
    rev_deps_tv_count,
    fill = package == "purrr"
  )) +
  geom_bar(stat = "identity") +
  guides(fill = 'none') +
  coord_flip() +
  labs(x = "pkg", y = "n") +
  ggtitle(
    "tidyverse reverse dependencies within the tidyverse",
    "sum of reverse imports, depends, suggests, enhances, linking-to within the tidyverse"
  )

# function usage ----
get_pkg_func_usage <- function(pkg_name) {
  a_env <- new.env()
  
  
  # read .rdb and .rdx files (compressed files containing the functions of an R package)
  lazyLoad(
    sprintf(
      "/home/till/R/x86_64-pc-linux-gnu-library/4.1/%s/R/%s",
      pkg_name,
      pkg_name
    ),
    envir = a_env
  )
  
  bb <- names(a_env)
  
  out <- map(bb, find) |> unlist()
  pkg_string <- paste("package", pkg_name, sep = ":")
  out[out != pkg_string]
}

get_pkg_func_usage(pkg_name = "dplyr")

names(tidyverse_imports) <- tidyverse_imports
tv_external_pkg_usage <-
  imap(tidyverse_imports,
       \(x, i) {
         print(i)
         get_pkg_func_usage(x)
       })
saveRDS(tv_external_pkg_usage, "tv_external_pkg_usage.rds")
#tv_external_pkg_usage <- readRDS("tv_external_pkg_usage.rds")

library(igraph)
library(tidygraph)
library(ggraph)
library(rlang)

pkg_edges <-
  map2_dfr(names(tv_external_pkg_usage),
           tv_external_pkg_usage,
           \(name, pkgs) {
             tibble(from = name,
                    to = pkgs)
           }) |>
  mutate(to = gsub("package:", "", to)) |>
  group_by(from, to) |>
  summarise(n = n())

tv_dep_network_map <- 
  pkg_edges |>
  filter(from != ".GlobalEnv", to != ".GlobalEnv") |> 
  graph_from_data_frame() |>
  as_tbl_graph() |>
  mutate(size = centrality_degree(weights = n, mode = "in"),
         is_tv = name %in% tidyverse_imports) |>
  ggraph(layout = "stress") +
  geom_edge_link(aes(width = n, alpha = ..index..)) +
  scale_edge_alpha('Edge direction', guide = 'edge_direction') +
  geom_node_label(aes(
    label = name,
    size = size,
    color = is_tv
  )) +
  scale_color_manual(values = c(`TRUE` = "blue", `FALSE` = "orange")) +
  scale_size_continuous(range = c(2, 7)) +
  guides(size = guide_legend(title = "In-Degree"),
         color = guide_legend(title = "tidyverse")) +
  ggtitle("external function usage in tidyverse packages")

save(rev_deps_tv_bars, 
     rev_deps_bars, 
     tv_dep_network_map, 
     file = "plots.Rda")

data <- bind_rows(
  paletteer::palettes_c_names %>%
    mutate(length = 20,
           group = "c"),
  paletteer::palettes_d_names %>%
    mutate(group = "d"),
  paletteer::palettes_dynamic_names %>%
    mutate(group = "dynamic")
)

color_fun <- function(package, palette, length, group) {
  gen_fun <- getFromNamespace(
    paste0("paletteer_", group, collapse = ""),
    "paletteer")
  gen_fun(paste0(package, "::", palette), length)
}

s_color_fun <- safely(color_fun)

data_list <- data %>%
  mutate(colors = pmap(list(package,
                            palette,
                            length,
                            group),
                       ~s_color_fun(..1, ..2, ..3, ..4)),
         error = map_lgl(colors, ~ is.null(.x$error))) %>%
  filter(error) %>%
  select(-error) %>%
  mutate(colors = map(colors, ~ .x$result))

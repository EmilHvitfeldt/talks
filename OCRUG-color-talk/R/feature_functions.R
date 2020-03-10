color_contains <- function(pal, color, space) {
  spectrum1 <- convert_colour(t(col2rgb(pal)), "rgb", space)
  spectrum2 <- convert_colour(t(col2rgb(color)), "rgb", space)
  
  compare_colour(spectrum1, spectrum2, space, method = "cie2000") %>%
    min()
}

color_all_contains <- function(pal, color, space) {
  
  spectrum1 <- convert_colour(t(col2rgb(pal)), "rgb", space)
  spectrum2 <- convert_colour(t(col2rgb(color)), "rgb", space)
  compare_colour(spectrum1, spectrum2, space, method = "cie2000") %>%
    mean()
}

linear <- function(pal, space) {
  colors <- convert_colour(t(col2rgb(pal)), "rgb", space) %>%
    as.data.frame() %>%
    mutate(x = row_number())
  
  out <- lm(x ~ ., colors) %>%
    summary() %>%
    .$adj.r.squared
  
  if(is.nan(out))
    return(0)
  
  out
}

linear_split <- function(pal, space) {
  colors <- convert_colour(t(col2rgb(pal)), "rgb", space) %>%
    as.data.frame()
  
  colors1 <- colors[seq_len(ceiling(nrow(colors) / 2)), ] %>%
    mutate(x = row_number())
  colors2 <- colors[seq_len(ceiling(nrow(colors) / 2)) + floor(nrow(colors) / 2), ] %>%
    mutate(x = row_number())
  
  out <- min(
    lm(x ~ ., colors1) %>%
      summary() %>%
      .$adj.r.squared,
    lm(x ~ ., colors2) %>%
      summary() %>%
      .$adj.r.squared
  )
  
  if(is.nan(out))
    return(0)
  
  out
}

deutan <- function(pal) {
  dichromat::dichromat(pal, type = "deutan")
}

protan <- function(pal) {
  dichromat::dichromat(pal, type = "protan")
}

tritan <- function(pal) {
  dichromat::dichromat(pal, type = "tritan")
}

min_distance <- function(pal, space) {
  spectrum1 <- convert_colour(t(col2rgb(pal)), "rgb", space)
  spectrum2 <- spectrum1
  
  mat <- compare_colour(spectrum1, spectrum2, space, method = "cie2000")
  min(mat[upper.tri(mat)])
}

max_distance <- function(pal, space) {
  spectrum1 <- convert_colour(t(col2rgb(pal)), "rgb", space)
  spectrum2 <- spectrum1
  
  mat <- compare_colour(spectrum1, spectrum2, space, method = "cie2000")
  max(mat[upper.tri(mat)])
}

iqr_distance <- function(pal, space) {
  spectrum1 <- convert_colour(t(col2rgb(pal)), "rgb", space)
  spectrum2 <- spectrum1
  
  mat <- compare_colour(spectrum1, spectrum2, space, method = "cie2000")
  IQR(mat[upper.tri(mat)])
}
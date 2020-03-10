list_plotter <- function(color_list, names, package_name) {
  par(mar = c(0, 0, 0, 0) + 0.1)
  
  plot(0, 0, type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "", 
       xlim = c(0, 1), ylim = c(-length(color_list)-1, 0))
  
  title(package_name, line = -3)
  for (i in seq_len(length(color_list))) {
    
    colors_len <- length(color_list[[i]])
    breaks <- seq(from = 0, to = 1, length = colors_len + 1)
    
    
    text(0, -i, names[i], pos = 4)
    rect(xleft = breaks[1:colors_len], xright = breaks[1:colors_len + 1], 
         ytop = - 0.15-i, ybottom = -0.8-i, 
         col = color_list[[i]], border = NA) 
  }
}

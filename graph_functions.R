
fontsize = 16
format_plot <- function(plot){
  formatted_plot <- plot + 
    theme_few() +
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.box = "horizontal", 
      legend.text = element_text(size = fontsize-2),
      legend.title = element_blank(), 
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(5, "cm"), 
      plot.title = element_text(size = fontsize, face = "bold"), 
      plot.subtitle = element_text(size = fontsize-2, face = "italic"),
      plot.background = element_blank(),
      axis.text.x = element_text(size = fontsize-2, color = "black", angle = 90), 
      axis.text.y = element_text(size = fontsize-2, color = "black"), 
      strip.text = element_text(size=fontsize-2)
    ) 
  return(formatted_plot)
}


format_map <- function(map){
  formatted_map <- map + 
    theme_void()+
    theme(
      legend.position = "top",
      legend.justification = "left",
      legend.text = element_text(size = fontsize-2),
      legend.title = element_blank(), 
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(5, "cm"), 
      plot.title = element_text(size = fontsize, face = "bold"), 
      plot.subtitle = element_text(size = fontsize-2, face = "italic"),
      strip.text = element_text(size=fontsize-2)
    ) 
  return(formatted_map)
}



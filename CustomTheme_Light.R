require('ggplot2')

# Predefined Color Palette
preded_color_palette <- c("#9B56B7", "#1DCE6C", "#E94C36", "#FD326D", "#F2C500", "#00BD9C", "#F8A31B", 
                          "#E87F04", "#2E97DE")

CustomTheme_Light <- theme(
  #Change Background Appearance
  plot.margin = unit(c(20,20,20,20),'point'),
  plot.background = element_rect(fill = "white"),
  plot.title = element_text(color="black", face="bold", size=22, hjust=0, margin=margin(0,0,20,0)),
  panel.background = element_rect(fill = "white"),
  
  #Change Panel Grid Appearance
  panel.grid.major = element_line(color = "#D7D7D7", linetype = "dotted"),
  panel.grid.minor = element_blank(),
  
  #Change Axis Appearance
  axis.text = element_text(color = 'black', size = 10),
  axis.title = element_text(size = 14, color = 'black'),
  axis.title.x = element_text(margin=margin(20,0,0,0)),
  axis.title.y = element_text(margin=margin(0,20,0,0)),
  
  #Change Legend Appearance
  legend.background = element_rect(fill = "white"),
  legend.margin = unit(1, "cm"),
  legend.key.size = unit(5, "mm"),
  legend.text = element_text(size = 10, colour = "black"),
  legend.title = element_text(size = 14, colour = "black", margin=margin(0,0,10,0)),
  
  #Change Facet Attribute Appearance
  strip.background = element_rect(colour = "#D7D7D7", fill = "#F0F0F0"),
  strip.text.x = element_text(colour = "black", size = 10, hjust = 0.5, vjust = 0.5)
  
)
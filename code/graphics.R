#####
# Project: Development and fertility at the global scale
# Purpose: Graphics
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 16.07.2025
######

library(ggplot2)

# Set the graphic scheme
theme_set(theme_test(base_size=13, base_family="serif"))
theme_update(panel.grid.major=element_line(linewidth=0.2, linetype="dotted", colour="lightgrey"),
             axis.text=element_text(colour="black"),
             strip.background = element_rect(fill="white"),
             strip.text = element_text(face="bold"),
             legend.position="bottom")

### END ###########################
#match up crockford with my samples 

library(readxl)
library(dplyr)
library(lemon)
library(ggplot2)
library(wesanderson)
library(googledrive)


setwd("~/PCrockford samples")
#theme shortcut
bw_shortcut <- theme_bw() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_rect(fill = NA),
                                  legend.title= element_blank(),
                                  # axis.line = element_line(colour = "black"),
                                  # axis.title.y = element_text(size = 15),
                                  #strip.text.x = element_text(size = 15),
                                  strip.background = element_blank(), 
                                  strip.placement = "outside",
                                  text = element_text(size = 18)
)

drive_download("PC - Peter Crockford samples", type = "csv", overwrite = TRUE)
1

#this is my dataframe
allPCData  <- read.csv("PC - Peter Crockford samples.csv", stringsAsFactors = FALSE)
cleaned_alldf <- allPCData[c(1,2,4,10,27,44,57:67)] 
pcdf <- cleaned_alldf[cleaned_alldf$Group == "PCsamples",]

names(pcdf)[1:17] <- c("Group", "name", "height", "rad", "d88", "d44", 
                        "Srppm", "Cappm", "Mgppm", "d13c", "d18o",
                        "d26.24Mg", "d7Li", "MgmmolCamol", "SrmmolCamol", "MnmmolCamol", "LiumolCamol" )
names(cleaned_alldf)[1:17] <- c("Group", "name", "height", "rad", "d88", "d44", 
                                "Srppm", "Cappm", "Mgppm", "d13c", "d18o",
                                "d26.24Mg", "d7Li", "MgmmolCamol", "SrmmolCamol", "MnmmolCamol", "LiumolCamol" )

#shortcut for a black and white plot
bw_shortcut <- theme_bw() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_rect(fill = NA),
                                  #legend.title= element_blank(),
                                  # axis.line = element_line(colour = "black"),
                                  # axis.title.y = element_text(size = 15),
                                  #strip.text.x = element_text(size = 15),
                                  #strip.background = element_blank(), 
                                  strip.placement = "outside",
                                  text = element_text(size = 18)
)

gabbyaesth <-  theme(
  # Remove panel border
  panel.border = element_rect(colour='black', fill = NA, size = 1),  
  # Remove panel grid lines
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Remove panel background
  panel.background = element_blank(),
  # Add axis line not needed for us, because I want the whole rectangle
  #axis.line = element_line(colour = "black")
  axis.text.x = element_text(color='black',
                             size=13),
  axis.text.y = element_text(color= 'black',
                             size=13),
  axis.title = element_text(size = 14),
  plot.margin = margin (0.5,0.5,0.5, 0.5, "cm"),
  legend.position = "none")


# .STRAT COLUMN FIGURE. ---------------------------------------------------

all_long <- tidyr::pivot_longer(data = cleaned_alldf, rad:d18o, names_to = "parameter", 
                                values_to = "values", values_drop_na = TRUE)


PClong <- tidyr::pivot_longer(data = pcdf, "rad":"d18o", names_to = "parameter", 
                                        values_to = "values", values_drop_na = TRUE)


### STRAT COL work up####
maindata <- PClong[PClong$parameter == "rad" | PClong$parameter == "d13c" | 
                     PClong$parameter == "d88" | PClong$parameter == "d44" ,] 

maindata$parameter <- factor(maindata$parameter, levels = c("d13c", "rad", "d88", "d44"),
                             labels = c(paste("δ^{13}*C*",expression(paste(" (VPDB, ‰)"))),
                                        "NULL^{87}*Sr/NULL^{86}*Sr",
                                        paste("δ^{88/86}*Sr*", expression(paste(" (NBS 987, ‰)"))),
                                        paste("δ^{44/40}*Ca*", expression(paste(" (SW, ‰)")))
                             )
)

#### main data figure STRAT COL ####
ggplot(data = maindata, aes(x= values, y = height, color = Group)) + 
  facet_rep_grid(. ~ parameter, scales = "free_x", labeller = label_parsed, switch = "x") + #facet grid labelparsed pretty labels (bottom)
  geom_point(size = 3) + bw_shortcut + 
  theme(title = element_text(size =20), text = element_text(family = 'serif'),
        strip.background = element_blank(), strip.text = element_text(size = 18), strip.placement = "outside", 
        panel.border = element_rect(size = 2.5)) + 
  scale_x_continuous(name = element_blank()) + scale_y_continuous(name = "Height (m)", breaks = seq(0,350,50)) 




# Cross plot Multi Proxy --------------------------------------------------

ggplot(data = pcdf, aes(x=d44, y = d88, color = height)) + 
  geom_point(size =3) +
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  #geom_smooth(method = 'lm', se = FALSE) + 
  bw_shortcut + 
  theme (panel.border = element_rect(size = 1.5) ) + 
  scale_color_gradientn(colors = wes_palette(name = "Zissou1"))
                      
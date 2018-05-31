library(dplyr)
library(ggplot2)
library(readr)
library(purrr)

# color frequency by theme 
set_colors <- read.table("colorTheme.csv",sep =",",header= TRUE)
set_colors <- set_colors[,-1]
set_colors$rgb <- paste("#", set_colors$rgb, sep="")
dim(set_colors)

# color frequency plots
freq_tbl <- set_colors %>% select(Theme, rgb) %>% 
  group_by(Theme, rgb) %>%
  summarise(n = n())  %>%
  mutate(percent = n/sum(n))
#freq_tbl$rgb <- paste("#", freq_tbl$rgb, sep="")
freq_tbl <- freq_tbl[1:1000,]

# plot
breaks <- freq_tbl$Theme
pal <- unique(set_colors$rgb)
names(pal) <- unique(pal) 

gp <-  freq_tbl %>% ggplot() + 
  geom_col(
    aes(x = Theme, y = percent, fill = rgb),  
    width = 10
  ) +
  labs(x = "", 
       y = "Relative Color Frequency", 
       title = "Lego brick colors, 1950-2017",
       subtitle = "Relative frequency of brick colors per year",
       caption = "source: rebrickable.com/api") +
  scale_fill_manual(values = pal) + 
  scale_x_discrete(limits = breaks) +          
  theme(
    panel.background = element_rect(fill = "#d6d4d2"),
    plot.background = element_rect(fill = "#f8f8f8"),
    text = element_text(size = 13),
    plot.title = element_text(size = rel(1)),
    plot.subtitle = element_text(size = rel(0.8)),   
    plot.caption = element_text(size = rel(0.6)),
    axis.title = element_text(size = rel(0.8), color = "gray15"),
    legend.position = "none", 
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    panel.grid = element_blank()
  )
gp <- gp + scale_x_discrete(labels = abbreviate)
gp

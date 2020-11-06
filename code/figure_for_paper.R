#### Figure for paper of pathways
library(ggplot2)
theme_set(theme_bw(base_size = 11))
library(patchwork)
library(tidyverse)

data_orig <- read.csv("data_figs.csv")[1:8]


paths <- data_orig$path_name[1:7]
names(paths) <- data_orig$path[1:7]

data <- subset(data_orig, path < 6)

data$path_f = factor(data$path, levels=c(2,1,4,5,3)) # To reorder to match proportions

p1 <- ggplot(data = subset(data, !is.na(bed) & source == "uclh"), aes(x=1, y = mean, fill = factor(bed)),na.rm = TRUE) + 
  coord_flip() + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) + 
  facet_grid(path_f~step, labeller = labeller(path_f = paths),  switch = "y", scales = "free") +  
  scale_x_continuous(labels = NULL, "") + 
  scale_y_continuous("Length of stay", limits = c(0,22)) + 
  theme_bw()  + 
  scale_fill_discrete("Bed type", labels = c("CC","Ward"))+ 
  theme(axis.ticks.y = element_blank()) + 
  ggtitle("UCLH")+
  theme(strip.text.y.left = element_text(angle = 0))

p2 <- ggplot(data = subset(data, !is.na(bed) & source == "cocin"), aes(x=1, y = mean, fill = factor(bed)),na.rm = TRUE) + 
  coord_flip() + 
  geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin = pmax(0,mean - sd), ymax = mean + sd)) + 
  facet_grid(path_f~step, labeller = labeller(path_f = paths),  switch = "y") + 
  scale_x_continuous(labels = NULL, "") + 
  scale_y_continuous("Length of stay", limits = c(0,22)) + 
  theme_bw()  + 
  scale_fill_discrete("Bed type", labels = c("CC","Ward"))+ 
  theme(axis.ticks.y = element_blank()) + 
  ggtitle("COCIN") +
  theme(strip.text.y.left = element_text(angle = 0))
  
p1 / p2
#ggsave("fig_paper.pdf", width = 10, height = 6)

### Stacked bar chart of proportions
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data_prop <- data %>% filter(!is.na(proportion)) 

p3 <- ggplot(data_prop,aes(x=source,y=proportion, fill = path_name)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual("Pathway",values=cbPalette) + 
  scale_x_discrete("Dataset", labels = c("COCIN","UCLH")) + 
  scale_y_continuous("Proportion of admissions")

p3 + p1/p2 + plot_layout(ncol = 2, width = c(1, 3)) + plot_annotation(tag_levels = 'A')
#ggsave("fig_paper2.pdf",width = 15, height = 6)


### Ordered by size
data_prop$path_name = with(data_prop, reorder(path_name, proportion))
data_prop$path_name = factor(data_prop$path_name)

p4 <- ggplot(data_prop,aes(x=source,y=proportion, fill = path_name)) + 
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual("Pathway",values=cbPalette) + 
  scale_x_discrete("Dataset", labels = c("COCIN","UCLH")) + 
  scale_y_continuous("Proportion of admissions")

p4 + p1/p2 + plot_layout(ncol = 2, width = c(1, 3)) + plot_annotation(tag_levels = 'A')
ggsave("fig_paper_final.pdf",width = 15, height = 6)

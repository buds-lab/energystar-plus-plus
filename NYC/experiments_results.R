library(ggplot2)
library(reshape2)
library(ggpubr)
library(RColorBrewer)
library(latex2exp)
library(export)
library(tikzDevice)

library(ggsci)
library(scico)
library(viridis)
library(grid)
library(ggthemes)

save_dir2 = "./data/usetype_results/"

#write.csv(cc, paste0(save_dir2, btype, "_result.csv"))

btypes = c(
  "office", "retail", "k12school", 
  "warehouse",
  "hotel", "worship", "multifamily"
  #"hospital", "medical_office"
  )


combined = NULL
## combine all results
for (btype in btypes) {
  #btype = "multifamily"
  data = read.csv(paste0(save_dir2, btype, "_result.csv"))
  print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))
  data = cbind(btype = btype, data)
  combined = rbind(combined, data, make.row.names = F)
}

write.csv(combined, paste0(save_dir2, "combined.csv"), row.names = F)


plot_btype_R2 <- function(btype, data) {
  bdata1 = data %>% mutate(model = case_when(
    model == "RegInt" ~ paste0(model,interaction),
    TRUE ~ paste0(model)))
  
  bdata2 = melt(bdata1, measure.vars = c("R.2", "Adj.R.2"))
  
  plt = ggplot(bdata2, aes(x=model, y=value, group=variable, fill=variable)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(. ~ dependent) + 
    scale_y_continuous(limits = c(0,1)) + 
    #coord_flip() + 
    #theme_pubr() + 
    theme_pubclean() +
    ggpubr::rotate_x_text() + 
    ggtitle(btype)
  
  print(plt)
  export::graph2png(file=paste0("./plots/R2_",btype,".png"), width=6, height=5)
}

plot_btype_mape <- function(btype, data) {
  
  bdata1 = data %>% mutate(model = case_when(
    model == "RegInt" ~ paste0(model,interaction),
    TRUE ~ paste0(model)))
  
  #bdata2 = melt(bdata1, measure.vars = c("R.2", "Adj.R.2"))
  #bdata2 = subset(bdata1, transform == "meanCent")
  
  plt = ggplot(bdata1, aes(x=model, y=mape 
                           #group=variable, fill=variable
  )) + 
    geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(. ~ dependent) + 
    #scale_y_continuous(limits = c(0,1)) + 
    #coord_flip() + 
    #theme_pubr() + 
    theme_pubclean() +
    ggpubr::rotate_x_text() + 
    ggtitle(btype)
  
  print(plt)
  export::graph2png(file=paste0("./plots/mape_",btype,".png"), width=6, height=5)
}

## btype-wise plots
for (type in btypes) {
  #type = "worship"
  bdata = subset(combined, btype == type)
  print(paste(type, paste(dim(bdata), collapse = "x")))
  plot_btype_R2(type, bdata)
  plot_btype_mape(type, bdata)
}



#plot adjR2 acrooss all building types
f1 = combined %>% 
  filter(dependent == "SourceEnergy") %>%
  filter(interaction <= 4)

f1$model = dplyr::recode(
  f1$model, 
  Reg = "OLS", 
  RegInt = "OLSi",
  rpart = "DT",
  xgboost = "GBT")

f1 = f1 %>% mutate(model = case_when(
  model == "OLSi" ~ paste0(model,interaction),
  TRUE ~ paste0(model)))

f1 = f1 %>% mutate(btype = case_when(
  btype == "school" ~ "k12school", TRUE ~ paste0(btype)))

col_order = c("OLS", "OLSi2", "OLSi3", "OLSi4", "DT", "GBT")
f1$model <- factor(f1$model, levels = col_order)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x 
}
f1$btype = as.factor(firstup(as.character(f1$btype)))


plot_energy_R2 <- function(f1) {
  
  mypal = pal_npg("nrc", alpha = 0.7)(10)
  
  bar5 = ggbarplot(f1, x = "btype", y = "Adj.R.2",
                   fill = "model", color = "model", 
                   ylim = c(0.4,1),
                   sort.by.groups = T,
                   position = position_dodge(0.9),
                   #palette = mypal,
                   #palette = "npg",
                   #palette = "grey",
                   #width = 1,
                   xlab = FALSE,
                   ylab = expression( paste("Adjusted ", R^2)),
                   legend = "right",
                   legend.title = "")
  
  #bar5 = bar5 + 
  #  theme(legend.justification=c(1,0), legend.position=c(0.99,0.05))
  
  bar5 = 
    bar5 + 
    theme(legend.direction = "horizontal",
          legend.box.background = element_rect(),
          legend.box.margin = margin(1, 1, 1, 1),
          legend.justification=c(1,0), 
          legend.position=c(0.95,0.1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  
  
  #bar5 + theme_pubr()
  #theme(legend.box = "horizontal") 
  bar5 = bar5 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(bar5)
  export::graph2png(file="./plots/nyc_energy_R2.png", 
                    width=9, height=3)
  
  # theme_set(theme_gray(base_size = 10))
  # tikz(file = "./plots/nyc_energy_R2.tex", 
  #      width = 8, height = 3)
  # print(bar5)
  # dev.off()
  
  ######################## BTYPE-WISE
  bar6 = ggbarplot(f1, x = "model", y = "Adj.R.2",
                   fill = "btype", color = "btype", 
                   ylim = c(0.4,1),
                   sort.by.groups = T,
                   position = position_dodge(0.9),
                   #palette = mypal,
                   palette = "npg",
                   #palette = "grey",
                   #width = 1,
                   xlab = FALSE,
                   ylab = expression( paste("Adjusted ", R^2)),
                   legend = "right",
                   legend.title = "")
  
  bar6 = 
    bar6 + 
    theme(legend.direction = "horizontal",
          legend.box.background = element_rect(),
          legend.box.margin = margin(1, 1, 1, 1),
          legend.justification=c(1,0), 
          legend.position=c(0.95,0.1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  
  #bar5 + theme_pubr()
  #theme(legend.box = "horizontal") 
  bar6 = bar6 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(bar6)
  export::graph2png(file="./plots/nyc_energy_R2b.png", 
                    width=9, height=3)
  
  # theme_set(theme_gray(base_size = 10))
  # tikz(file = "./plots/nyc_energy_R2b.tex", 
  #      width = 8, height = 3)
  # print(bar6)
  # dev.off()
}

plot_energy_MAPE <- function(f1) {
  
  mypal = pal_npg("nrc", alpha = 0.7)(10)
  
  bar5 = ggbarplot(f1, x = "btype", y = "mape",
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.9),
                   palette = mypal,
                   #palette = "npg",
                   #palette = "grey",
                   #width = 1,
                   xlab = FALSE,
                   ylab = expression( paste("MAPE")),
                   legend = "right",
                   legend.title = "")
  
  bar5 = 
    bar5 + 
    theme(legend.direction = "horizontal",
          legend.justification=c(1,1), 
          legend.position=c(1,1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  
  
  #bar5 + theme_pubr()
  #theme(legend.box = "horizontal") 
  bar5 = bar5 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(bar5)
  export::graph2png(file="./plots/nyc_energy_MAPE.png", 
                    width=9, height=3)
  
  # theme_set(theme_gray(base_size = 10))
  # tikz(file = "./plots/nyc_energy_MAPE.tex", width = 8, height = 2)
  # print(bar5)
  # dev.off()
  
  ######################## BTYPE-WISE
  bar6 = ggbarplot(f1, x = "model", y = "mape",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.9),
                   #palette = mypal,
                   palette = "npg",
                   #palette = "grey",
                   #width = 1,
                   xlab = FALSE,
                   ylab = expression( paste("MAPE")),
                   legend = "right",
                   legend.title = "")
  
  bar6 = bar6 + 
    theme(legend.direction = "horizontal",
          legend.justification=c(1,1), 
          legend.position=c(1,1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  
  
  #bar5 + theme_pubr()
  #theme(legend.box = "horizontal") 
  bar6 = bar6 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(bar6)
  export::graph2png(file="./plots/nyc_energy_MAPE1.png", 
                    width=9, height=3)
  
  # theme_set(theme_gray(base_size = 10))
  # tikz(file = "./plots/nyc_energy_MAPE1.tex", 
  #      width = 8, height = 3)
  # print(bar6)
  # dev.off()
}

plot_energy_xyz <- function(f1, measure) {
  
  mypal = pal_npg("nrc", alpha = 0.7)(10)
  
  bar5 = ggbarplot(f1, x = "btype", y = measure,
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.9),
                   palette = mypal,
                   #palette = "npg",
                   #palette = "grey",
                   #width = 1,
                   xlab = FALSE,
                   ylab = paste(toupper(measure)),
                   legend = "right",
                   legend.title = "")
  
  bar5 = 
    bar5 + 
    theme(legend.direction = "horizontal",
          legend.justification=c(1,1), 
          legend.position=c(1,1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  
  
  #bar5 + theme_pubr()
  #theme(legend.box = "horizontal") 
  bar5 = bar5 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(bar5)
  filename = paste0("./plots/nyc_energy_", measure, ".png")
  export::graph2png(file=filename, width=9, height=3)
  
  # theme_set(theme_gray(base_size = 10))
  # filename = paste0("./plots/nyc_energy_", measure, ".tex")
  # tikz(file = filename, width = 8, height = 2)
  # print(bar5)
  # dev.off()
  
  ######################## BTYPE-WISE
  bar6 = ggbarplot(f1, x = "model", y = "mape",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.9),
                   #palette = mypal,
                   palette = "npg",
                   #palette = "grey",
                   #width = 1,
                   xlab = FALSE,
                   ylab = expression( paste("MAPE")),
                   legend = "right",
                   legend.title = "")
  
  bar6 = bar6 + 
    theme(legend.direction = "horizontal",
          legend.justification=c(1,1), 
          legend.position=c(1,1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  
  
  #bar5 + theme_pubr()
  #theme(legend.box = "horizontal") 
  bar6 = bar6 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(bar6)
  filename = paste0("./plots/nyc_energy_", measure, "1.png")
  export::graph2png(file=filename, width=9, height=3)
  
  # theme_set(theme_gray(base_size = 10))
  # filename = paste0("./plots/nyc_energy_", measure, "1.tex")
  # tikz(file = filename, width = 8, height = 3)
  # print(bar6)
  # dev.off()
}

plot_energy_R2(f1)
plot_energy_MAPE(f1)
plot_energy_xyz(f1, "mse")
plot_energy_xyz(f1, "rmse")
plot_energy_xyz(f1, "mae")
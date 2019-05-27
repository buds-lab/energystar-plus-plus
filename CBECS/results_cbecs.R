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
library(dplyr)

save_dir = './data/results_new/'
btypes = c("office", "retail", "k12school", "warehouse",
           "hotel", "worship", "multifamily")

combined = NULL
## combine all results
for (btype in btypes) {
  #btype = "multifamily"
  data = read.csv(paste0(save_dir, btype, ".csv"))
  print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))
  data = cbind(btype = btype, data)
  combined = rbind(combined, data, make.row.names = F)
}

write.csv(combined, paste0(save_dir, "combined.csv"), row.names = F)

plot_btype_R2 <- function(btype, data) {
  bdata1 = data %>% mutate(model = case_when(
    model == "RegInt" ~ paste0(model,interaction),
    TRUE ~ paste0(model)))
  
  bdata2 = melt(bdata1, measure.vars = c("R.2", "Adj.R.2"))
  
  plt = ggplot(bdata2, aes(x=model, y=value, group=variable, fill=variable)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(transform ~ dependent) + 
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
  bdata2 = subset(bdata1, transform == "meanCent")
  
  plt = ggplot(bdata2, aes(x=model, y=mape 
                           #group=variable, fill=variable
                           )) + 
    geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(transform ~ dependent) + 
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
filtered = combined %>% 
  filter(transform == "meanCent") %>%
  filter(model != "rpart") %>%
  filter(btype != "warehouse") %>%
  filter(interaction <= 4)

filtered$model = paste0(filtered$model, filtered$interaction)

filtered$model = dplyr::recode(
  filtered$model, 
  Reg1 = "MLR",
  Reg_Int2 = "MLRi2",
  Reg_Int3 = "MLRi3",
  Reg_Int4 = "MLRi4",
  #Reg_AIC_Int4 = "MLRi4",
  #rpart1 = "DT",
  #xgboost_default2 = "GBTd2",
  #xgboost_default3 = "GBTd3",
  xgboost_grid2 = "GBTi2",
  xgboost_grid3 = "GBTi3"
  #xgboost_random1 = "GBTr",
  #xgboost_adaptive1 = "GBTa"
  )

filtered$btype = dplyr::recode(filtered$btype, k12school = "K-12 School")

#col_order = c("MLR", "MLRi2", "MLRi3", "MLRi4", "DT", "GBT")
models = c("MLR", "MLRi2", "MLRi3", "MLRi4", "GBTi2", "GBTi3")
              #"GBTd2", "GBTd3", "GBTg2", "GBTg3", "GBTr", "GBTa")

filtered = filtered %>% 
  filter(model %in% models)

filtered$model <- factor(filtered$model, levels = models)

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x 
}

filtered$btype = as.factor(firstup(as.character(filtered$btype)))


plot_energy_R2 <- function(f1) {
  
  plt1 = ggbarplot(f1, x = "btype", y = "Adj.R.2",
                   fill = "model", color = "model", 
                   ylim = c(0.2,1),
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression( paste("Adjusted ", R^2)),
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  #plt1 = plt1 + theme(legend.key.size = unit(1,"line"))
  #plt1 = plt1 + 
  #  theme(legend.justification=c(1,0), legend.position=c(0.99,0.05))
  
  plt1 = plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      legend.margin=margin(1,1,1,-1),
      legend.box.margin=margin(0,0,0,-1),
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      #legend.justification=c(0,0), 
      #legend.position=c(0.05,0.05),
      
      text=element_text(size=8, 
                            family="Times New Roman"))
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  #plt1 + scale_color_nejm() + scale_fill_nejm()
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_energy_R2.png", 
                    width=6, height=3)
  export::graph2pdf(file="./plots/cbecs_energy_R2.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2.tex", 
       width = 3.4, height = 2)
  print(plt1)
  dev.off()
  
  
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "Adj.R.2",
                   fill = "btype", color = "btype", 
                   ylim = c(0.5,1),
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression( paste("Adjusted ", R^2)),
                   legend.title = "")  
    #ggpubr::rotate_x_text()

  plt2 = plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      legend.margin=margin(1,1,1,-1),
      legend.box.margin=margin(0,0,0,-1),
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)  
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_energy_R21.png", 
                    width=6, height=3)
  export::graph2pdf(file="./plots/cbecs_energy_R21.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R21.tex", 
       width = 3.4, height = 2)
  print(plt2)
  dev.off()
  
  return(list(plt1, plt2))
}

plot_EUI_R2 <- function(f1) {
  
  plt1 = ggbarplot(f1, x = "btype", y = "Adj.R.2",
                   fill = "model", color = "model", 
                   ylim = c(0.0,1),
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression( paste("Adjusted ", R^2)),
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  #plt1 = plt1 + theme(legend.key.size = unit(1,"line"))
  #plt1 = plt1 + 
  #  theme(legend.justification=c(1,0), legend.position=c(0.99,0.05))
  
  plt1 = plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      legend.margin=margin(1,1,1,-1),
      legend.box.margin=margin(0,0,0,-1),
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      #legend.justification=c(0,0), 
      #legend.position=c(0.05,0.05),
      
      text=element_text(size=8, 
                        family="Times New Roman"))
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  #plt1 + scale_color_nejm() + scale_fill_nejm()
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_EUI_R2.png", 
                    width=6, height=3)
  export::graph2pdf(file="./plots/cbecs_EUI_R2.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_R2.tex", 
       width = 3.4, height = 2)
  print(plt1)
  dev.off()
  
  
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "Adj.R.2",
                   fill = "btype", color = "btype", 
                   #ylim = c(0.5,1),
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression( paste("Adjusted ", R^2)),
                   legend.title = "")  
  #ggpubr::rotate_x_text()
  
  plt2 = plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      legend.margin=margin(1,1,1,-1),
      legend.box.margin=margin(0,0,0,-1),
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)  
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_EUI_R21.png", 
                    width=6, height=3)
  export::graph2pdf(file="./plots/cbecs_EUI_R21.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_R21.tex", 
       width = 3.4, height = 2)
  print(plt2)
  dev.off()
  
  return(list(plt1, plt2))
}

plot_energy_RMSE <- function(f1) {
  
  f1$rmseM = round(f1$rmse/1000000,2)
  plt1 = ggbarplot(f1, x = "btype", y = "rmseM",
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = "RMSE",
                   legend = "right",
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  
  plt1 = 
    plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      legend.margin=margin(1,1,1,-1),
      legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(1,1), 
      legend.position=c(1,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  

  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_energy_RMSE.png", 
                    width=6, height=3)
  
  export::graph2pdf(file="./plots/cbecs_energy_RMSE.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_RMSE.tex", 
       width = 3.4, height = 2.5)
  print(plt1)
  dev.off()
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "rmseM",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = "RMSE",
                   legend = "right",
                   legend.title = "")
  
  plt2 = 
    plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_energy_RMSE1.png", 
                    width=6, height=3)
  export::graph2pdf(file="./plots/cbecs_energy_RMSE1.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_RMSE1.tex", 
       width = 3.4, height = 2.5)
  print(plt2)
  dev.off()
  return(list(plt1, plt2))
}

plot_EUI_RMSE <- function(f1) {
  
  plt1 = ggbarplot(f1, x = "btype", y = "rmse",
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = "RMSE",
                   legend = "right",
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  
  plt1 = 
    plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      legend.margin=margin(1,1,1,-1),
      legend.box.margin=margin(0,0,0,-1),
      
      #legend.justification=c(1,1), 
      #legend.position=c(1,1),
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      
      text=element_text(size=8, 
                        family="Times New Roman"))
  
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_EUI_RMSE.png", 
                    width=6, height=3)
  export::graph2pdf(file="./plots/cbecs_EUI_RMSE.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_RMSE.tex", 
       width = 3.4, height = 2.5)
  print(plt1)
  dev.off()
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "rmse",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = "RMSE",
                   legend = "right",
                   legend.title = "")
  
  plt2 = 
    plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_EUI_RMSE1.png", 
                    width=6, height=3)
  export::graph2pdf(file="./plots/cbecs_EUI_RMSE1.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_RMSE1.tex", 
       width = 3.4, height = 2.5)
  print(plt2)
  dev.off()
  return(list(plt1, plt2))
}

plot_energy_MAPE <- function(f1) {
  
  plt1 = ggbarplot(f1, x = "btype", y = "mape",
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression( paste("MAPE")),
                   legend = "right",
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  
  plt1 = 
    plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
                        #family=windowsFonts(Times=windowsFont("TT Times New Roman"))))

  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_energy_MAPE.png", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_MAPE.tex", 
       width = 3.4, height = 2)
  print(plt1)
  dev.off()
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "mape",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression( paste("MAPE")),
                   legend = "right",
                   legend.title = "") 
  plt2 = 
    plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(1,1), 
      legend.position=c(1,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  #family=windowsFonts(Times=windowsFont("TT Times New Roman"))))
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_energy_MAPE1.png", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_MAPE1.tex", 
       width = 3.4, height = 2)
  print(plt2)
  dev.off()
  
  return(list(plt1, plt2))
}

plot_energy_MAE <- function(f1) {
  
  f1$mae_M = round(f1$mae/1000000, 2)
  plt1 = ggbarplot(f1, x = "btype", y = "mae_M",
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   #palette = mypal,
                   #palette = "npg",
                   #palette = "grey",
                   #width = 1,
                   xlab = FALSE,
                   ylab = expression( paste("MAE")),
                   legend = "right",
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  
  plt1 = 
    plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(1,1), 
      legend.position=c(1,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
                        #family=windowsFonts(Times=windowsFont("TT Times New Roman"))))
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_energy_MAE.png", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_MAE.tex", 
       width = 3.4, height = 2)
  print(plt1)
  dev.off()
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "mae_M",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression( paste("MAE")),
                   legend = "right",
                   legend.title = "") 
  
  plt2 = 
    plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      
      text=element_text(size=8, 
                        family="Times New Roman"))
  #family=windowsFonts(Times=windowsFont("TT Times New Roman"))))
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_energy_MAE1.png", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_MAE1.tex", 
       width = 3.4, height = 2)
  print(plt2)
  dev.off()
  
  return(list(plt1, plt2))
}

plot_EUI_MAE <- function(f1) {
  
  plt1 = ggbarplot(f1, x = "btype", y = "mae",
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   #palette = mypal,
                   #palette = "npg",
                   #palette = "grey",
                   #width = 1,
                   xlab = FALSE,
                   ylab = expression( paste("MAE")),
                   legend = "right",
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  
  plt1 = 
    plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      
      text=element_text(size=8, 
                        family="Times New Roman"))
  #family=windowsFonts(Times=windowsFont("TT Times New Roman"))))
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_EUI_MAE.png", 
                    width=6, height=3)
  
  export::graph2pdf(file="./plots/cbecs_EUI_MAE.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_MAE.tex", 
       width = 3.4, height = 2)
  print(plt1)
  dev.off()
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "mae",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression( paste("MAE")),
                   legend = "right",
                   legend.title = "") 
  
  plt2 = 
    plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(0,1), 
      legend.position=c(0.01,1),
      
      text=element_text(size=8, 
                        family="Times New Roman"))
  #family=windowsFonts(Times=windowsFont("TT Times New Roman"))))
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_EUI_MAE1.png", 
                    width=6, height=3)
  
  export::graph2pdf(file="./plots/cbecs_EUI_MAE1.pdf",
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_MAE1.tex", 
       width = 3.4, height = 2)
  print(plt2)
  dev.off()
  
  return(list(plt1, plt2))
}

plot_energy_NRMSE_range <- function(f1) {
  
  plt1 = ggbarplot(f1, x = "btype", y = "nrmse_range",
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression("NRMSE") ,
                   legend = "right",
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  
  plt1 = 
    plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      legend.margin=margin(1,1,1,-1),
      legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(1,1), 
      legend.position=c(1,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_energy_NRMSE_range.png", 
                    width=6, height=3)
  
  export::graph2pdf(file="./plots/cbecs_energy_NRMSE_range.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_NRMSE_range.tex", 
       width = 3.4, height = 2.5)
  print(plt1)
  dev.off()
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "nrmse_range",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = "NRMSE",
                   legend = "right",
                   legend.title = "")
  
  plt2 = 
    plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(1,1), 
      legend.position=c(1,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_energy_NRMSE_range1.png", 
                    width=6, height=3)
  
  export::graph2pdf(file="./plots/cbecs_energy_NRMSE_range1.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_NRMSE_range1.tex", 
       width = 3.4, height = 2.5)
  print(plt2)
  dev.off()
  return(list(plt1, plt2))
}

plot_EUI_NRMSE_range <- function(f1) {
  
  plt1 = ggbarplot(f1, x = "btype", y = "nrmse_range",
                   fill = "model", color = "model", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = expression("NRMSE") ,
                   legend = "right",
                   legend.title = "") + 
    ggpubr::rotate_x_text()
  
  plt1 = 
    plt1 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      legend.margin=margin(1,1,1,-1),
      legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(1,1), 
      legend.position=c(1,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt1 = plt1 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt1)
  export::graph2png(file="./plots/cbecs_EUI_NRMSE_range.png", 
                    width=6, height=3)
  export::graph2pdf(file="./plots/cbecs_EUI_NRMSE_range.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_NRMSE_range.tex", 
       width = 3.4, height = 2.5)
  print(plt1)
  dev.off()
  
  ######################## BTYPE-WISE
  plt2 = ggbarplot(f1, x = "model", y = "nrmse_range",
                   fill = "btype", color = "btype", 
                   sort.by.groups = T,
                   position = position_dodge(0.78),
                   xlab = FALSE,
                   ylab = "NRMSE",
                   legend = "right",
                   legend.title = "")
  
  plt2 = 
    plt2 + 
    theme(
      legend.text=element_text(size=6),
      legend.box = "horizontal",
      legend.direction = "horizontal",
      #legend.box.background = element_rect(),
      #legend.box.margin = margin(-0.5, 0, 0, 0),
      legend.key.width=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
      
      #legend.margin=margin(1,1,1,-1),
      #legend.box.margin=margin(0,0,0,-1),
      
      legend.justification=c(1,1), 
      legend.position=c(1,1),
      text=element_text(size=8, 
                        family="Times New Roman"))
  
  
  #plt1 + theme_pubr()
  #theme(legend.box = "horizontal") 
  plt2 = plt2 + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  print(plt2)
  export::graph2png(file="./plots/cbecs_EUI_NRMSE_range1.png", 
                    width=6, height=3)
  
  export::graph2pdf(file="./plots/cbecs_EUI_NRMSE_range1.pdf", 
                    width=6, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_NRMSE_range1.tex", 
       width = 3.4, height = 2.5)
  print(plt2)
  dev.off()
  return(list(plt1, plt2))
}

# plot_energy_R2(f1)
# plot_energy_MAPE(f1)
# plot_energy_xyz(f1, "mse")
# plot_energy_xyz(f1, "rmse")
# plot_energy_xyz(f1, "mae")


############ COMBINE PLOTS #######################

#### combine adj-R2 and MAE plots for energy ################
combine_energy_R2_MAE <- function(f1) {
  
  f_ene = f1 %>%  filter(dependent == "SOURCE_ENERGY")
  
  r2 = plot_energy_R2(f_ene)  
  mae = plot_energy_MAE(f_ene)
  
  r2plt = r2[[1]]
  maeplt = mae[[1]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.text=element_text(size=8))+
    theme(legend.position = "top")
  
  maeplt = maeplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(text=element_text(size=10, family="Times New Roman"))+
    theme(legend.position = "none")


  library(grid)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_energy_R2_MAE.png", 
                    width=3.4, height=4)
  
  export::graph2pdf(file="./plots/cbecs_energy_R2_MAE.pdf", 
                    width=3.4, height=4)
  
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2_MAE.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
  dev.off()
  
  ## btype wise
  r2plt = r2[[2]]
  maeplt = mae[[2]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.text=element_text(size=8))+
    theme(legend.position = "top")
  
  maeplt = maeplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(text=element_text(size=10, family="Times New Roman"))+
    theme(legend.position = "none")
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_energy_R2_MAE1.png", 
                    width=3.4, height=4)
  export::graph2pdf(file="./plots/cbecs_energy_R2_MAE1.pdf", 
                    width=3.4, height=4)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2_MAE1.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
  dev.off()
}

combine_EUI_R2_MAE <- function(f1) {
  
  f_eui = f1 %>%  filter(dependent == "SOURCE_EUI")
  
  r2 = plot_EUI_R2(f_eui)  
  mae = plot_EUI_MAE(f_eui)
  
  r2plt = r2[[1]]
  maeplt = mae[[1]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.position = "top")
  
  maeplt = maeplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(legend.position = "none")
  
  library(grid)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_EUI_R2_MAE.png", 
                    width=3.4, height=4)
  export::graph2pdf(file="./plots/cbecs_EUI_R2_MAE.pdf", 
                    width=3.4, height=4)
  
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_R2_MAE.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
  dev.off()
  
  ## btype wise
  r2plt = r2[[2]]
  maeplt = mae[[2]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.position = "top")
  
  maeplt = maeplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(legend.position = "none")
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_EUI_R2_MAE1.png", 
                    width=3.4, height=4)
  
  export::graph2pdf(file="./plots/cbecs_EUI_R2_MAE1.pdf", 
                    width=3.4, height=4)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_R2_MAE1.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
  dev.off()
}

combine_energy_R2_RMSE <- function(f1) {
  
  f_ene = f1 %>%  filter(dependent == "SOURCE_ENERGY")
  
  r2 = plot_energy_R2(f_ene)  
  rmse = plot_energy_RMSE(f_ene)
  
  r2plt = r2[[1]]
  rmseplt = rmse[[1]]

  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.position = "top")
  
  rmseplt = rmseplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(legend.position = "none")
  
  
  library(grid)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(rmseplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_energy_R2_RMSE.png", 
                    width=3.4, height=4)
  
  export::graph2pdf(file="./plots/cbecs_energy_R2_RMSE.pdf", 
                    width=3.4, height=4)
  
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2_RMSE.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(rmseplt), size = "first"))
  dev.off()
  
  ## btype wise
  r2plt = r2[[2]]
  rmseplt = rmse[[2]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.position = "top")
  
  rmseplt = rmseplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(legend.position = "none")
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(rmseplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_energy_R2_RMSE1.png", 
                    width=3.4, height=4)
  export::graph2pdf(file="./plots/cbecs_energy_R2_RMSE1.pdf", 
                    width=3.4, height=4)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2_RMSE1.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(rmseplt), size = "first"))
  dev.off()
}

combine_EUI_R2_RMSE <- function(f1) {
  
  f_eui = f1 %>%  filter(dependent == "SOURCE_EUI")
  
  r2 = plot_EUI_R2(f_eui)  
  rmse = plot_EUI_RMSE(f_eui)
  
  r2plt = r2[[1]]
  rmseplt = rmse[[1]]

  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.position = "top")
  
  rmseplt = rmseplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(legend.position = "none")
  
  ## put legend in two rows
  #rmseplt = rmseplt + 
  #  guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  library(grid)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(rmseplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_EUI_R2_RMSE.png", 
                    width=3.4, height=4)
  
  export::graph2pdf(file="./plots/cbecs_EUI_R2_RMSE.pdf", 
                    width=3.4, height=4)
  
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_R2_RMSE.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(rmseplt), size = "first"))
  dev.off()
  
  ## btype wise
  r2plt = r2[[2]]
  rmseplt = rmse[[2]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.position = "top")
  
  rmseplt = rmseplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(legend.position = "none")
  
  ## put legend in two rows
  #rmseplt = rmseplt + 
  #  guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(rmseplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_EUI_R2_RMSE1.png", 
                    width=3.4, height=4)
  export::graph2pdf(file="./plots/cbecs_EUI_R2_RMSE1.pdf", 
                    width=3.4, height=4)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_R2_RMSE1.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(rmseplt), size = "first"))
  dev.off()
}

combine_energy_R2_NRMSE_range <- function(f1) {
  
  f_ene = f1 %>%  filter(dependent == "SOURCE_ENERGY")
  
  r2 = plot_energy_R2(f_ene)  
  nrmse = plot_energy_NRMSE_range(f_ene)
  
  r2plt = r2[[1]]
  nrmseplt = nrmse[[1]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.text=element_text(size=8))+
    theme(text=element_text(size=10, family="Times New Roman"))+
    theme(legend.position = "top")
  
  nrmseplt = nrmseplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(text=element_text(size=10, family="Times New Roman"))+
    theme(legend.position = "none")
  
  library(grid)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(nrmseplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_energy_R2_NRMSE_range.png", 
                    width=3.4, height=4)
  
  export::graph2pdf(file="./plots/cbecs_energy_R2_NRMSE_range.pdf", 
                    width=3.4, height=4)
  
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2_NRMSE_range.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(nrmseplt), size = "first"))
  dev.off()
  
  ## btype wise
  r2plt = r2[[2]]
  nrmseplt = nrmse[[2]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.text=element_text(size=8))+
    theme(text=element_text(size=10, family="Times New Roman"))+
    theme(legend.position = "top")
  
  nrmseplt = nrmseplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(text=element_text(size=10, family="Times New Roman"))+
    theme(legend.position = "none")
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(nrmseplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_energy_R2_NRMSE_range1.png", 
                    width=3.4, height=4)
  export::graph2pdf(file="./plots/cbecs_energy_R2_NRMSE_range1.pdf", 
                    width=3.4, height=4)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2_NRMSE_range1.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(nrmseplt), size = "first"))
  dev.off()
}

combine_EUI_R2_NRMSE_range <- function(f1) {
  
  f_ene = f1 %>%  filter(dependent == "SOURCE_EUI")
  
  r2 = plot_EUI_R2(f_ene)  
  nrmse = plot_EUI_NRMSE_range(f_ene)
  
  r2plt = r2[[1]]
  nrmseplt = nrmse[[1]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.text=element_text(size=8))+
    theme(text=element_text(size=9, family="Times New Roman"))+
    theme(legend.position = "top")
  
  nrmseplt = nrmseplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(text=element_text(size=9, family="Times New Roman"))+
    theme(legend.position = "none")
  
  
  library(grid)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(nrmseplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_EUI_R2_NRMSE_range.png", 
                    width=3.4, height=4)
  
  export::graph2pdf(file="./plots/cbecs_EUI_R2_NRMSE_range.pdf", 
                    width=3.4, height=4)
  
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_R2_NRMSE_range.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(nrmseplt), size = "first"))
  dev.off()
  
  ## btype wise
  r2plt = r2[[2]]
  nrmseplt = nrmse[[2]]
  
  # put legend at top
  r2plt = r2plt + 
    theme(axis.text.x = element_blank()) + 
    theme(legend.text=element_text(size=8))+
    theme(text=element_text(size=10, family="Times New Roman"))+
    theme(legend.position = "top")
  
  nrmseplt = nrmseplt + 
    #theme(axis.text.x = element_blank()) + 
    theme(text=element_text(size=10, family="Times New Roman"))+
    theme(legend.position = "none")
  
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(nrmseplt), size = "first"))
  
  export::graph2png(file="./plots/cbecs_EUI_R2_NRMSE_range1.png", 
                    width=3.4, height=4)
  export::graph2pdf(file="./plots/cbecs_EUI_R2_NRMSE_range1.pdf", 
                    width=3.4, height=4)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_EUI_R2_NRMSE_range1.tex", width = 3.4, height = 4)
  grid.newpage()
  grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(nrmseplt), size = "first"))
  dev.off()
}


f_eui = filtered %>%  filter(dependent == "SOURCE_EUI")
f_ene = filtered %>%  filter(dependent == "SOURCE_ENERGY")

#models_gbt = c("GBTd2", "GBTd3", "GBTg2", "GBTg3", "GBTr", "GBTa")
#filtered1 = filtered %>% filter(model %in% models_gbt)

combine_energy_R2_MAE(filtered)
combine_EUI_R2_MAE(filtered)

combine_energy_R2_RMSE(filtered)
combine_EUI_R2_RMSE(filtered)

combine_energy_R2_NRMSE_range(filtered)
combine_EUI_R2_NRMSE_range(filtered)

## print no.of features and no.of samples
f_eui$obs/2

ff = f_eui %>% filter(obs/3 <= rank)

# eui - calculate average increase/decrease in metrics across all buildings
res_all = NULL
#models = c("MLRi2", "MLRi3", "MLRi4", "GBT")
for(m in models) {
  f1 = f_eui %>% filter(model == 'MLR')
  f2 = f_eui %>% filter(model == m)
  
  r2    = round(mean(f2$R.2 - f1$R.2)*100, 2)
  adjr2 = round(mean(f2$Adj.R.2 - f1$Adj.R.2)*100, 2)
  
  mse   = round(mean(f1$mse  - f2$mse), 2)
  rmse  = round(mean(f1$rmse - f2$rmse), 2)
  mae   = round(mean(f1$mae  - f2$mae), 2)
  nrmse_iqr   = round(mean(f1$nrmse_iqr   - f2$nrmse_iqr), 2)
  nrmse_range = round(mean(f1$nrmse_range - f2$nrmse_range), 2)
  nrmse_mean  = round(mean(f1$nrmse_mean  - f2$nrmse_mean), 2)
  
  df = data.frame(model=m, r2, adjr2, mse, rmse, mae, nrmse_iqr, nrmse_range, nrmse_mean)
  res_all = rbind(res_all, df)
  print(paste(m, r2, rmse, nrmse_range))
}
save_dir = './data/results_new/'
data = write.csv(res_all, paste0(save_dir, "comparision_eui.csv"), row.names = F)

# energy - calculate average increase/decrease in metrics across all buildings
res_all = NULL
#models = c("MLRi2", "MLRi3", "MLRi4", "GBT")
for(m in models) {
  f1 = f_ene %>% filter(model == 'MLR')
  f2 = f_ene %>% filter(model == m)
  
  r2    = round(mean(f2$R.2 - f1$R.2)*100, 2)
  adjr2 = round(mean(f2$Adj.R.2 - f1$Adj.R.2)*100, 2)
  
  mse   = round(mean(f1$mse  - f2$mse), 2)
  rmse  = round(mean(f1$rmse - f2$rmse), 2)
  mae   = round(mean(f1$mae  - f2$mae), 2)
  nrmse_iqr   = round(mean(f1$nrmse_iqr   - f2$nrmse_iqr), 2)
  nrmse_range = round(mean(f1$nrmse_range - f2$nrmse_range), 2)
  nrmse_mean  = round(mean(f1$nrmse_mean  - f2$nrmse_mean), 2)
  
  df = data.frame(model=m, r2, adjr2, mse, rmse, mae, nrmse_iqr, nrmse_range, nrmse_mean)
  res_all = rbind(res_all, df)
  print(paste(m, r2, rmse, nrmse_range))
}
save_dir = './data/results_new/'
data = write.csv(res_all, paste0(save_dir, "comparision_energy.csv"), row.names = F)




###################################################
## combine adj R2 and MAPE plots
mape = plot_energy_MAPE(f1)
mapeplt = mape[[1]]
r2plt = r2[[1]]
r2plt = r2plt + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.position = "none")

grid.newpage()
grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(mapeplt), size = "first"))

export::graph2png(file="./plots/cbecs_energy_R2_MAPE.png", 
                  width=3.4, height=3)

theme_set(theme_gray(base_size = 10))
tikz(file = "./plots/cbecs_energy_R2_MAPE.tex", width = 3.4, height = 3)
grid.newpage()
grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
dev.off()


# btype wise
mapeplt = mape[[2]]
r2plt = r2[[2]]
r2plt = r2plt + 
  theme(axis.text.x = element_blank()) + 
  theme(legend.position = "none")

grid.newpage()
grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(mapeplt), size = "first"))

export::graph2png(file="./plots/cbecs_energy_R2_MAPE1.png", 
                  width=3.4, height=3)

theme_set(theme_gray(base_size = 10))
tikz(file = "./plots/cbecs_energy_R2_MAPE1.tex", width = 3.4, height = 3)
grid.newpage()
grid.draw(rbind(ggplotGrob(r2plt), ggplotGrob(maeplt), size = "first"))
dev.off()







#plot the data only for reg.interactionss
f2 = combined %>% 
  filter(dependent == "SOURCE_ENERGY") %>%
  filter(transform == "meanCent") %>%
  filter(model == "RegInt")

ggplot(f2, aes(x=interaction, y=Adj.R.2, group=btype, color = btype)) + 
  geom_line(aes(linetype=btype), size=1) + 
  geom_point(aes(shape=btype), size=2) + theme_pubclean()
  #geom_bar(stat="identity", position=position_dodge()) +
  #facet_grid(. ~ btype) 










######### old code

ggplot(f1, aes(btype, Adj.R.2)) +   
  geom_bar(aes(fill = model, color=model),
           position = "dodge",
           stat="identity")
mypal = pal_npg("nrc", alpha = 0.7)(6)


bar5 = ggbarplot(f1, x = "btype", y = "Adj.R.2",
                 fill = "model", color = "model", 
                 sort.by.groups = T,
                 position = position_dodge(0.9),
                 palette = mypal,
                 #palette = "npg",
                 #palette = "grey",
                 #width = 1,
                 xlab = FALSE,
                 ylab = expression( paste("Adjusted ", R^2)),
                 legend = "right",
                 legend.title = "")

#bar5 + theme_pubr()
#theme(legend.box = "horizontal") 
bar5 + scale_fill_viridis(discrete = TRUE) + 
  scale_color_viridis(discrete = T)

export::graph2png(file="./plots/cbecs_energy_R2.png", 
                  width=9, height=3)

theme_set(theme_gray(base_size = 10))
tikz(file = "./plots/cbecs_energy_R2.tex", width = 8, height = 2)
print(bar5)
dev.off()



ene = subset( combined, dependent == "SOURCE_ENERGY")
eui = subset( combined, dependent == "SOURCE_EUI")

ggplot(comp5, aes(x=btype, y=Adj.R.2, group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 


## comparision 1: compare the R^2, Adj.R^2, MSE among all Ex experiments
comp1 = combined
comp1$model = paste0(comp1$model, comp1$interaction)

## plot type 1 - all combined
comp2 = melt(comp1, measure.vars = c("R.2", "Adj.R.2"))
ggplot(comp2, aes(x=model, y=value, group=variable, fill=variable)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(dependent ~ btype) + coord_flip()

## plot type 3 - only adj.R^2 
comp3 = comp1
ggplot(comp3, aes(x=model, y=Adj.R.2, group=dependent, fill=dependent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(. ~ btype, ncol = 3)  + coord_flip()

## plot type 3 - only adj.R^2  SOURCE_ENERGY
comp4 = subset(comp1, dependent == 'SOURCE_ENERGY')
ggplot(comp4, aes(x=model, y=Adj.R.2, group=btype, fill=btype)) + 
  geom_bar(stat="identity", position=position_dodge())

ggplot(comp4, aes(x=btype, y=Adj.R.2, group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

ggplot(comp4, aes(x=btype, y=sqrt(mse), group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

## plot type 4 - only adj.R^2  SOURCE_EUI
comp5 = subset(comp1, dependent == 'SOURCE_EUI' & model != "xgboost1")
ggplot(comp5, aes(x=model, y=Adj.R.2, group=btype, fill=btype)) + 
  geom_bar(stat="identity", position=position_dodge())

ggplot(comp5, aes(x=btype, y=Adj.R.2, group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

ggplot(comp5, aes(x=btype, y=sqrt(mse), group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge())

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

## remove all interacations
mdl = setdiff(unique(comp4$model), paste0("RegInt", 3:7))
comp4a = comp4 %>% dplyr::filter(model %in% mdl)

library(dplyr)

#models = c(Reg1 = "OLS", `Reg-Lasso1` = "Lasso")
comp4a$model = recode(comp4a$model, 
                      Reg1 = "OLS", 
                      `Reg-Lasso1` = "Lasso",
                      `Reg-Ridge1` = "Ridge",
                      `Reg-ElasticNet1` = "ElasticNet",
                      RegInt2 = "OLSi",
                      rpart1 = "DT",
                      xgboost1 = "GBT")

mypal = pal_npg("nrc", alpha = 0.7)(9)
#mypal = pal_material("brown", alpha = 0.7, reverse = T)(7)
#mypal = pal_futurama(alpha = 0.7)(7)
#mypal = pal_jama(alpha = 0.9)(7)
#mypal = pal_nejm(alpha = 0.8)(7)

col_order = c("OLS", "OLSi", "Lasso", "Ridge", "ElasticNet",
          "DT", "GBT")

bar5 = ggbarplot(comp4a, x = "btype", y = "Adj.R.2",
          fill = "model", color = "model", 
          sort.by.groups = T,
          position = position_dodge(0.9),
          palette = "npg",
          #width = 1,
          #order = col_order,
          xlab = FALSE,
          ylab = expression( paste("Adjusted ", R^2)),
          legend = "right",
          legend.title = "")

bar5 + theme_pubr()
  theme(legend.box = "horizontal") 
bar5 + scale_fill_viridis(discrete = TRUE) + 
  scale_color_viridis(discrete = T)

export::graph2png(file="cbecs_energy.png", width=9, height=3)

theme_set(theme_gray(base_size = 10))
tikz(file = "cbecs_energy.tex", width = 8, height = 2)
print(bar5)
dev.off()




#  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position="top", 
        legend.title = element_blank()) 
  bar5 + guides(colour = guide_legend(nrow = 1))

bar5


bar5 + scale_color_material("blue-grey")



bar5 + scale_fill_npg() + scale_color_npg()

bar5 + theme_bw() + 
  scale_fill_viridis(discrete = TRUE) + 
  scale_color_viridis(discrete = T)


bar5 + scale_fill_Publication()+ scale_colour_Publication() + 
  theme_Publication()


ggplot2:::print.ggplot( bar5 + theme_bw() )

##################

combined1 = melt(combined, measure.vars = c("R.2", "Adj.R.2"))
combined2 = subset(combined1, interaction == 1)

ggplot(combined2, aes(x=btype, y=value, group=variable, fill=variable)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(dependent ~ model)

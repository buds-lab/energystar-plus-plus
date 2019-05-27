setwd("C:/Users/sbbpan/Documents/GitHub/buds-lab/BEEM/energy_star_combined/")
library(dplyr)
library(scico)
library(ggsci)
library(viridis)
library(ggplot2)

cbecs = read.csv("cbecs.csv")
cbecs = cbecs %>% 
  mutate(data = "CBECS") %>%
  filter(dependent == "SOURCE_ENERGY")  %>%
  filter(transform == "meanCent")  %>%
  dplyr::select(-c(transform,X.1,X)) %>% 
  mutate(dependent = case_when(
    dependent == "SOURCE_ENERGY" ~ "TotalEnergy",
    TRUE ~ paste0(dependent)))%>%
  mutate_if(is.factor, as.character)

nyc  = read.csv("nyc.csv", stringsAsFactors = F)
nyc = nyc %>% 
  mutate(data = "NYC") %>%
  filter(dependent == "SourceEnergy")  %>%
  dplyr::select(-c(X)) %>% 
  filter(model != "xgboost_default")  %>%
  mutate(model = case_when(
    model == "xgboost_random" ~ "xgboost",
    TRUE ~ paste0(model))) %>%
  mutate_if(is.factor, as.character)

seattle = read.csv("seattle.csv")
seattle = seattle %>% 
  mutate(data = "Seattle") %>%
  filter(dependent == "SourceEnergy")  %>%
  dplyr::select(-c(X.1,X)) %>% 
  filter(model != "xgboost_default")  %>%
  mutate(model = case_when(
    model == "xgboost_random" ~ "xgboost",
    interaction > 1 ~ "RegInt",
    TRUE ~ paste0(model))) %>%
  mutate_if(is.factor, as.character)

all = bind_rows(cbecs, nyc, seattle)

## fix model names, btype, etc.
all1 = all
all1$model = dplyr::recode(
  all1$model, 
  Reg = "MLR", 
  RegInt = "MLRi",
  rpart = "DT",
  xgboost = "GBTi2")

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x 
}

all2 = all1 %>% 
  filter(model != "rpart") %>%
  filter(btype != "warehouse") %>%
  mutate(model = case_when(
    model == "MLRi" ~ paste0(model,interaction),
    TRUE ~ paste0(model))) %>%
  mutate(btype = case_when(
    btype == "school" ~ "k-12school",
    btype == "k12school" ~ "k-12school", 
    TRUE ~ paste0(btype))) %>%
  mutate(btype = firstup(btype)) 

all3 = all2 %>% 
  filter(interaction <= 4) %>% 
  filter(Adj.R.2 >= 0 & Adj.R.2 <= 1)
write.csv(all3, "combined_energy.csv")

### now the ploting part
## compare adjusted R.2 values across cities and buildings use types

#exclude 3 and 4 - order int. models
models = c("MLR", "MLRi2", "GBTi2")
all4 = all3 %>% filter(model %in% models)

#col_order = c("OLS", "OLSi2", "OLSi3", "OLSi4", "DT", "GBT")
#col_order = c("OLS", "OLSi2", "GBT")
all4$model <- factor(all4$model, levels = models)

plot_adjR2 <- function(dat) {
  
  plt = ggplot(dat, aes(x=btype, y=Adj.R.2, 
                         group=model, fill=model)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(data ~ ., switch="both", scales="free_y") + 
    #scale_y_continuous(limits = c(0,1)) + 
    #coord_flip() + 
    #theme_pubr() + 
    theme_pubclean() 
  
  plt = plt + labs(x = NULL, 
                   y = expression( paste("Adjusted ", R^2)))
  
  plt = plt + theme(
      legend.title = element_blank(),
      legend.position	= "right", 
      #legend.direction = "horizontal",
          #legend.box.background = element_rect(),
          #legend.box.margin = margin(1, 1, 1, 1),
          #legend.justification=c(1,0), 
          #legend.position=c(0.95,0.1),
          text=element_text(size=11, 
                            family="Times New Roman"))
  
  plt = plt + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)

  print(plt)
  export::graph2png(file="./energy_adjR2.png", 
                    width=9, height=3)
  
  # theme_set(theme_gray(base_size = 10))
  # tikz(file = "./energy_adjR2.tex", 
  #      width = 8, height = 3)
  # print(plt)
  # dev.off()
}

plot_MAPE_single_new <- function(dat) {
  
  plt = ggplot(dat, aes(x=model, y=mape*100, 
                        group=data, color=data)) + 
    #geom_line(size = 1) +
    geom_line(aes(linetype = data), size = 1) +
    geom_point(aes(shape = data), size = 2) + 
    #geom_bar(stat="identity", aes(fill=data),
    #         position=position_dodge(0.9)) +
    facet_grid(.~btype, switch="both", scales="fixed") + 
    #scale_linetype_discrete() + 
    theme_pubclean() 
  plt
  
  plt = plt + labs(x = NULL, 
                   y = "MAPE")
  plt = plt + theme(strip.placement = "outside", 
                    strip.background = element_blank()) 
  
  #plt + scale_color_npg()+
  #plt + scale_color_aaas() +
  #plt + scale_color_jama()
  #plt + scale_color_d3() +
  plt = plt +  scale_color_nejm() + scale_fill_nejm() +
    #plt = plt + scale_color_viridis(discrete = T) + scale_fill_viridis(discrete = T) +
    #plt = plt + scale_color_nejm() + 
    theme_pubr() + 
    theme(strip.placement = "outside", 
          strip.background = element_blank())  + 
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(legend.direction = "horizontal",
          legend.title = element_blank(),
          #legend.box.background = element_rect(),
          #legend.box.margin = margin(1, 1, 1, 1),
          legend.justification=c(1,0), 
          legend.position=c(0.99,0.9),
          strip.text.x=element_text(size=9, face = "plain"),
          strip.text.y=element_text(size=9, face = "plain"),
          axis.text.x=element_text(size=8, face = "plain"),
          text=element_text(size=8, 
                            family="Times New Roman"))
  
  #    plt + scale_color_uchicago() + scale_fill_uchicago()
  
  print(plt)
  export::graph2png(file="./energy_MAPE_single.png", 
                    width=9, height=3)
  
  export::graph2pdf(file="./energy_MAPE_single.pdf", 
                    width=9, height=3)
  
  
  theme_set(theme_gray(base_size = 7))
  tikz(file = "./energy_MAPE_single.tex",
       width = 7, height = 2)
  print(plt)
  dev.off()
  
  
}

plot_adjR2_single_new <- function(dat) {
  
  plt = ggplot(dat, aes(x=model, y=Adj.R.2, 
                        group=data, color=data)) + 
    #geom_line(size = 1) +
    geom_line(aes(linetype = data), size = 1) +
    geom_point(aes(shape = data), size = 2) + 
    #geom_bar(stat="identity", aes(fill=data),
    #         position=position_dodge(0.9)) +
    facet_grid(.~btype, switch="both", scales="fixed") + 
    #scale_linetype_discrete() + 
    theme_pubclean() 
  plt
  
  
  plt = plt + labs(x = NULL, 
                   y = expression( paste("Adjusted ", R^2)))
  plt = plt + theme(strip.placement = "outside", 
                    strip.background = element_blank()) 
  
  #plt + scale_color_npg()+
  #plt + scale_color_aaas() +
  #plt + scale_color_jama()
  #plt + scale_color_d3() +
  plt = plt +  scale_color_nejm() + scale_fill_nejm() +
  #plt = plt + scale_color_viridis(discrete = T) + scale_fill_viridis(discrete = T) +
    #plt = plt + scale_color_nejm() + 
    theme_pubr() + 
    theme(strip.placement = "outside", 
          strip.background = element_blank())  + 
    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(legend.direction = "horizontal",
          legend.title = element_blank(),
          #legend.box.background = element_rect(),
          #legend.box.margin = margin(1, 1, 1, 1),
          legend.justification=c(1,0), 
          legend.position=c(0.99,0.9),
          strip.text.x=element_text(size=9, face = "plain"),
          strip.text.y=element_text(size=9, face = "plain"),
          axis.text.x=element_text(size=8, face = "plain"),
          text=element_text(size=8, 
                            family="Times New Roman"))
  
  #    plt + scale_color_uchicago() + scale_fill_uchicago()
    
  print(plt)
  export::graph2png(file="./energy_adjR2_single.png", 
                    width=9, height=3)
  
  export::graph2pdf(file="./energy_adjR2_single.pdf", 
                    width=9, height=3)
  
  
  theme_set(theme_gray(base_size = 7))
  tikz(file = "./energy_adjR2_single.tex",
       width = 7, height = 2)
  print(plt)
  dev.off()
  
  
}

plot_adjR2_single <- function(dat) {
  
  plt = ggplot(dat, aes(x=btype, y=Adj.R.2, 
                         group=model, color=model)) + 
    #geom_line(size = 1) +
    #geom_line(aes(linetype = model), size = 1) +
    #geom_point(aes(shape = model), size = 2) + 
    geom_bar(stat="identity", aes(fill=model),
             position=position_dodge(0.9)) +
    facet_grid(data ~ ., switch="both", scales="fixed") + 
    #scale_linetype_discrete() + 
    theme_pubclean() 
  
  plt = plt + labs(x = NULL, 
                   y = expression( paste("Adjusted ", R^2)))
  plt = plt + theme(strip.placement = "outside", 
                    strip.background = element_blank()) 
  
  #plt + scale_color_npg()+
  #plt + scale_color_aaas() +
  #plt + scale_color_jama()
  #plt + scale_color_d3() +
  #plt = plt +  scale_color_nejm() + scale_fill_nejm() +
  plt = plt + scale_color_viridis(discrete = T) + scale_fill_viridis(discrete = T) +
  #plt = plt + scale_color_nejm() + 
    theme_pubr() + 
    theme(strip.placement = "outside", 
          strip.background = element_blank())  + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(legend.direction = "horizontal",
          legend.title = element_blank(),
          #legend.box.background = element_rect(),
          #legend.box.margin = margin(1, 1, 1, 1),
          #legend.justification=c(1,0), 
          #legend.position=c(0.95,0.1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  print(plt)
  export::graph2png(file="./energy_adjR2_single.png", 
                    width=5, height=6)
  
}

plot_RMSE <- function(dat) {
  
  plt = ggplot(dat, aes(x=btype, y=rmse, 
                         group=model, fill=model)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(data ~ ., switch="both", scales="free_y") + 
    #scale_y_continuous(limits = c(0,1)) + 
    #coord_flip() + 
    #theme_pubr() + 
    theme_pubclean() 
  
  plt = plt + labs(x = NULL, 
                   y = "RMSE")
  
  plt = plt + theme(
    legend.title = element_blank(),
    legend.position	= "right", 
    #legend.direction = "horizontal",
    #legend.box.background = element_rect(),
    #legend.box.margin = margin(1, 1, 1, 1),
    #legend.justification=c(1,0), 
    #legend.position=c(0.95,0.1),
    text=element_text(size=11, 
                      family="Times New Roman"))
  
  plt = plt + scale_fill_viridis(discrete = TRUE) + 
    scale_color_viridis(discrete = T)
  
  #plt = plt + scale_fill_jama()
  
  print(plt)
  export::graph2png(file="./energy_RMSE.png", 
                    width=9, height=3)
  
  # theme_set(theme_gray(base_size = 10))
  # tikz(file = "./energy_adjR2.tex", 
  #      width = 8, height = 3)
  # print(plt)
  # dev.off()
}

plot_RMSE_single <- function(dat) {
  
  plt = ggplot(dat, aes(x=btype, y=rmse, 
                        group=model, color=model)) + 
    #geom_line(size = 1) +
    geom_line(aes(linetype = model), size = 1) +
    geom_point(aes(shape = model), size = 2) + 
    #geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(data ~ ., switch="both", scales="fixed") + 
    #scale_linetype_discrete() + 
    theme_pubclean() 
  
  plt = plt + labs(x = NULL, 
                   y = "RMSE")
  plt = plt + theme(strip.placement = "outside", 
                    strip.background = element_blank()) 
  
  #plt + scale_color_npg()+
  #plt + scale_color_aaas() +
  #plt + scale_color_jama()
  #plt + scale_color_d3() +
  plt = plt + scale_color_nejm() + 
    theme_pubr() + 
    theme(strip.placement = "outside", 
          strip.background = element_blank())  + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(legend.direction = "horizontal",
          legend.title = element_blank(),
          #legend.box.background = element_rect(),
          #legend.box.margin = margin(1, 1, 1, 1),
          #legend.justification=c(1,0), 
          #legend.position=c(0.95,0.1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  print(plt)
  export::graph2png(file="./energy_RMSE_single.png", 
                    width=5, height=6)
  
}  


plot_MAPE_single <- function(dat) {
  
  plt = ggplot(dat, aes(x=btype, y=mape, 
                        group=model, color=model)) + 
    #geom_line(size = 1) +
    geom_line(aes(linetype = model), size = 1) +
    geom_point(aes(shape = model), size = 2) + 
    #geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(data ~ ., switch="both", scales="fixed") + 
    #scale_linetype_discrete() + 
    theme_pubclean() 
  plt
  
  plt = plt + labs(x = NULL, 
                   y = "MAPE")
  plt = plt + theme(strip.placement = "outside", 
                    strip.background = element_blank()) 
  
  #plt + scale_color_npg()+
  #plt + scale_color_aaas() +
  #plt + scale_color_jama()
  #plt + scale_color_d3() +
  plt = plt + scale_color_nejm() + 
    theme_pubr() + 
    theme(strip.placement = "outside", 
          strip.background = element_blank())  + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(legend.direction = "horizontal",
          legend.title = element_blank(),
          #legend.box.background = element_rect(),
          #legend.box.margin = margin(1, 1, 1, 1),
          #legend.justification=c(1,0), 
          #legend.position=c(0.95,0.1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  print(plt)
  export::graph2png(file="./energy_MAPE_single.png", 
                    width=5, height=6)
  
}

plot_MAE_single <- function(dat) {
  
  plt = ggplot(dat, aes(x=btype, y=mae, 
                        group=model, color=model)) + 
    #geom_line(size = 1) +
    geom_line(aes(linetype = model), size = 1) +
    geom_point(aes(shape = model), size = 2) + 
    #geom_bar(stat="identity", position=position_dodge()) +
    facet_grid(data ~ ., switch="both", scales="fixed") + 
    #scale_linetype_discrete() + 
    theme_pubclean() + 
    scale_y_continuous(trans='log2')
  
  plt = plt + labs(x = NULL, 
                   y = "MAE")
  plt = plt + theme(strip.placement = "outside", 
                    strip.background = element_blank()) 
  
  #plt + scale_color_npg()+
  #plt + scale_color_aaas() +
  #plt + scale_color_jama()
  #plt + scale_color_d3() +
  plt = plt + scale_color_nejm() + 
    theme_pubr() + 
    theme(strip.placement = "outside", 
          strip.background = element_blank())  + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(legend.direction = "horizontal",
          legend.title = element_blank(),
          #legend.box.background = element_rect(),
          #legend.box.margin = margin(1, 1, 1, 1),
          #legend.justification=c(1,0), 
          #legend.position=c(0.95,0.1),
          text=element_text(size=10, 
                            family="Times New Roman"))
  print(plt)
  export::graph2png(file="./energy_MAE_single.png", 
                    width=5, height=6)
  
}  

library(ggpubr)

plot_adjR2(all4)
plot_RMSE(all4)

plot_adjR2_single(all4)
plot_RMSE_single(all4)
plot_MAPE_single(all4)
plot_MAE_single(all4)

plot_adjR2_single_new(all4)
plot_MAPE_single_new(all4)

  
  #scale_y_continuous(limits = c(0,1)) + 
  #coord_flip() + 
  #theme_pubr() + 
  

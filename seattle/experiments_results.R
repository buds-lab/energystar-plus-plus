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
btypes = c(
  "office", "retail", "k12school", 
  "warehouse", "multifamily",
  "hotel", "worship")

combined = NULL
## combine all results
for (btype in btypes) {
  #btype = "multifamily"
  data = read.csv(paste0(save_dir2, btype, "_result.csv"))
  print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))
  data = cbind(btype = btype, data)
  combined = rbind(combined, data, make.row.names = F)
}

write.csv(combined, paste0(save_dir2, "combined.csv"))

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
  filter(dependent == "SiteEnergyUse") %>%
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
                   palette = "npg",
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
  export::graph2png(file="./plots/cbecs_energy_R2.png", 
                    width=9, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2.tex", 
       width = 8, height = 3)
  print(bar5)
  dev.off()
  
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
  export::graph2png(file="./plots/cbecs_energy_R2b.png", 
                    width=9, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_R2b.tex", 
       width = 8, height = 3)
  print(bar6)
  dev.off()
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
  export::graph2png(file="./plots/cbecs_energy_MAPE.png", 
                    width=9, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_MAPE.tex", width = 8, height = 2)
  print(bar5)
  dev.off()
  
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
  export::graph2png(file="./plots/cbecs_energy_MAPE1.png", 
                    width=9, height=3)
  
  theme_set(theme_gray(base_size = 10))
  tikz(file = "./plots/cbecs_energy_MAPE1.tex", 
       width = 8, height = 3)
  print(bar6)
  dev.off()
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
  filename = paste0("./plots/cbecs_energy_", measure, ".png")
  export::graph2png(file=filename, width=9, height=3)
  
  theme_set(theme_gray(base_size = 10))
  filename = paste0("./plots/cbecs_energy_", measure, ".tex")
  tikz(file = filename, width = 8, height = 2)
  print(bar5)
  dev.off()
  
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
  filename = paste0("./plots/cbecs_energy_", measure, "1.png")
  export::graph2png(file=filename, width=9, height=3)
  
  theme_set(theme_gray(base_size = 10))
  filename = paste0("./plots/cbecs_energy_", measure, "1.tex")
  tikz(file = filename, width = 8, height = 3)
  print(bar6)
  dev.off()
}

plot_energy_R2(f1)
plot_energy_MAPE(f1)
plot_energy_xyz(f1, "mse")
plot_energy_xyz(f1, "rmse")
plot_energy_xyz(f1, "mae")


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

library(ggsci)
library(scico)
##################

combined1 = melt(combined, measure.vars = c("R.2", "Adj.R.2"))
combined2 = subset(combined1, interaction == 1)

ggplot(combined2, aes(x=btype, y=value, group=variable, fill=variable)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(dependent ~ model)




combined = NULL
## combine all results
for (btype in btypes) {
  #btype = "multifamily"
  data = read.csv(paste0(save_dir2, btype, "_result.csv"))
  print(paste(Sys.time(), btype, paste(dim(data), collapse = " x ")))
  data = cbind(btype = btype, data)
  combined = rbind(combined, data, make.row.names = F)
}

library(ggplot2)
library(reshape2)

## comparision 1: compare the R^2, Adj.R^2, MSE among all Ex experiments
comp1 = combined
comp1$model = paste0(comp1$model, comp1$interaction)
comp1 = subset(comp1, R.2 >= 0 & Adj.R.2 >= 0)

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

## plot type 3 - only adj.R^2  SiteEnergyUse
comp4 = subset(comp1, dependent == 'SiteEnergyUse')
ggplot(comp4, aes(x=model, y=Adj.R.2, group=btype, fill=btype)) + 
  geom_bar(stat="identity", position=position_dodge())

ggplot(comp4, aes(x=btype, y=Adj.R.2, group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

ggplot(comp4, aes(x=btype, y=sqrt(mse), group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

## plot type 4 - only adj.R^2  SourceEUI
comp5 = subset(comp1, dependent == 'SourceEUI' & model != "xgboost1")
ggplot(comp5, aes(x=model, y=Adj.R.2, group=btype, fill=btype)) + 
  geom_bar(stat="identity", position=position_dodge())

ggplot(comp5, aes(x=btype, y=Adj.R.2, group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge()) 

ggplot(comp5, aes(x=btype, y=sqrt(mse), group=model, fill=model)) + 
  geom_bar(stat="identity", position=position_dodge())







### create cluster on remote machine
library("future")

cl <- makeClusterPSOCK(c("localhost", "172.25.186.105"), 
                       revtunnel = T,
                       user = "sbbadmin", 
                       rshcmd = c("plink", "-ssh", "-i", "C:/SBB/aws/sbb vm.ppk"),
                       homogeneous = FALSE, verbose = TRUE,
                       outfile = "output.log")

foo <- function(n) {
  print(paste(Sys.time(), n))
  #s = sample(9999999,10)
  for (r in runif(10000)) {
    for (s in runif(10000)) {
      x = r*s
    }
  }
  #Sys.sleep(n)
  #print(paste(n))
  return(n*rs)
}

res = clusterMap(cl, foo, 1:100)
unlist(res)

stopCluster(cl)


local = "SBB-5CG5473W2K"
primary <- local
machineAddresses <- list(
  list(host=primary,user='sbbadmin',
       ncore=4),
  list(host='172.25.186.105',user='sbbadmin',
       ncore=4)
)

spec <- lapply(machineAddresses,
               function(machine) {
                 rep(list(list(host=machine$host,
                               user=machine$user)),
                     machine$ncore)
               })
spec <- unlist(spec,recursive=FALSE)

parallelCluster <- parallel::makeCluster(type='PSOCK',
                                         master=primary,
                                         spec=spec,
                                         rshcmd = rshcmd1,
                                         verbose = T)
stopCluster(parallelCluster)

rshcmd1 = "plink -ssh -i 'C:/SBB/aws/sbb vm.ppk'"

cluster <- makeCluster(detectCores(), outfile = "output.log" )





############################## end #################################


school = read.csv(paste0(load_dir, "multifamily.csv"))
summary(school)
data1 = clean_data(school)

ivars = c("PropertyGFATotal", 
          "NumberofBuildings",
          "NumberofFloors",
          "Building.Quality", 
          "Construction.Class",
          "Heating.System",
          "Shape",
          "Age")
dvar1 = "SourceEUI"
dvar2 = "SiteEnergyUse"

data2 = data1[, c(ivars, dvar1, dvar2)]
btype = "school"
save_dir1 = paste0(save_dir, btype, "/")
dir.create(save_dir1, showWarnings = F)

c1 = exp_C1(data2)
c2 = exp_C2(btype, save_dir1, data2)

cc = rbind(c1,c2)
write.csv(cc, paste0(save_dir, btype, ".csv"))




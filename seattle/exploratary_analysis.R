library(readxl)
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rpart.utils)
library(sfa)
library(frontier)
library(likert)
library(mosaic)
library(ggpubr)
library(stringr)
library(ggridges)

merged = read_csv("data/merged.csv")

filtered = merged %>% 
  distinct() %>%
  filter( SiteEnergyUse > 0) %>% 
  filter( SourceEUI > 0)

model_cols = c("SiteEnergyUse", 
               # "Building Gross Sq Ft",
               "PropertyGFATotal",
               "Building Quality", "Construction Class",
               "Eff. Year", "Elevators", "Heating System", "Number Of Buildings Aggregated",
               "Predominant Use", "Shape", "Sprinklers", "Stories")

train = filtered[, model_cols]
train$`Eff. Year` = 2018 - train$`Eff. Year` 
train$SiteEnergyUse = log(train$SiteEnergyUse)
#write_csv(train, "data/train.csv", na = "")
###########################################

# print energy distribution plots based on different building characteristicss
regexp <- "[[:digit:]]+"
nos = str_extract(train$`Predominant Use`, regexp) 
train$`Predominant Use` = nos

pu = sort(table(train$`Predominant Use`))
pu = pu[ pu >= 20]
train1 = train[train$`Predominant Use` %in% names(pu), ]

ggplot(train1,  aes(x = SiteEnergyUse, y = `Predominant Use`)) +
  geom_density_ridges_gradient(
    aes(fill = ..x..), scale = 2, size = 0.3 ) +
  scale_fill_gradientn( colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"), 
                        name = "SiteEnergyUse(kBTU) - log scale") + 
  theme_pubr(base_size=14, legend="bottom")

boxplot(data = train1, SiteEnergyUse ~ `Predominant Use`,
        xlab = "groups", ylab= "SiteEnergyUse(kBTu) - log scale")

##################################################
# print the distributions of different building characteristics

plot_atr <- function(dat,title) {
  
  t = as.data.frame(table(dat))
  t[, 1] = paste0(t[, 1], " (", t[, 2], ")")
  #names(t2) = c("BuildingShape", "Count")
  p = ggplot(t) +
    geom_bar(aes(x = rep(0, nrow(t)),
                 y = Freq,
                 fill = dat),
             stat = "identity") + 
    #scale_y_discrete(breaks = NULL)
    coord_flip()  + 
    theme_pubr(base_size=14, legend="right") +  
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank()) + 
    scale_fill_discrete(name=title)
  
  return (p)
}

p1 = plot_atr(train$`Building Quality`, "Building Quality")
p2 = plot_atr(train$Shape, "Building Shape")
p3 = plot_atr(train$`Construction Class`, "Construction Class")
p4 = plot_atr(train$`Heating System`, "Heating System")

ele = train$Elevators
ele[is.na(ele)] <- "Unknown"
p5 = plot_atr(ele, "Elevators")

sp = train$Sprinklers
sp[is.na(sp)] <- "Unknown"
p6 = plot_atr(sp, "Sprinklers")


figure <- ggarrange(p1, p2, 
                    labels = c("", "", ""),
                    ncol = 1, nrow = 2)
fig2 <- ggarrange(p5, p6, 
                    labels = c("", "", ""),
                    ncol = 1, nrow = 2)

print(figure)
print(p3)
print(p4)
print(fig2)




################### temp code ######################
t1 = as.data.frame(table(train$`Building Quality`))
t1$Var1 = paste0(t1$Var1, " (", t1$Freq, ")")
names(t1) = c("Building Quality", "Count")
p1 = ggplot(t1) +
  geom_bar(aes(x = rep(0, nrow(t1)),
               y = Count,
               fill = BuildingQuality),
           stat = "identity") + 
  #scale_y_discrete(breaks = NULL)
  coord_flip()  + 
  theme_pubr(base_size=15, legend="right") +   
  theme(axis.title.y=element_blank(),
                                     axis.text.y=element_blank(),
                                     axis.ticks.y=element_blank(),
                                     axis.title.x=element_blank())

t2 = as.data.frame(table(train$Shape))
t2$Var1 = paste0(t2$Var1, " (", t2$Freq, ")")
names(t2) = c("BuildingShape", "Count")
p2 = ggplot(t2) +
  geom_bar(aes(x = rep(0, nrow(t2)),
               y = Count,
               fill = BuildingShape),
           stat = "identity") + 
  #scale_y_discrete(breaks = NULL)
  coord_flip()  + 
  theme_pubr(base_size=15) +   
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank()) + 
  scale_fill_discrete(name="Building Shape")



train1 = train[, c(3,10, 11)]
ggplot(train1, aes(Shape, "", fill=Shape)) + 
  geom_bar(width=1, stat="identity", show.legend = F) + 
  coord_flip() +  theme_pubr()

ggplot(t1) + 
  geom_point(aes(x = Count, y = BuildingQuality)) + 
  theme_pubr()

ggplot(t1, aes(x = BuildingQuality, y = Count)) +
  geom_bar(stat = "identity", show.legend = F) +
  geom_text(aes(label = Count), vjust = -0.3) + 
  theme_pubr()

ggplot(train, aes(x = Shape, color=Shape, fill=Shape)) +
  geom_bar()+
  #geom_text(aes(label = ), vjust = -0.3) + 
  
  #geom_text(aes(y = ((..count..)/sum(..count..)), 
  #              label = scales::percent((..count..)/sum(..count..))), stat = "count", hjust = +1.25) +
  theme_pubr()
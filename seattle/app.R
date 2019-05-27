#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

library(readxl)
library(readr)

library(dplyr)
library(GGally)
library(corrplot)
#library(MASS)

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
library(egg) # same width for legends
#library(fBasics)

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
library(reshape2)
library(caret)

library(jtools)
library(caret)
library(MASS)

library(DMwR)  ## for unscale

options(scipen=5) # avoid scientific notation in plots

train = as.data.frame(read_csv("data/train.csv", na = ""))

#cols = names(train)
#cols = gsub("PropertyGFATotal", "GFA", names(cols))
#cols = gsub("PredominanUse", "UseType", names(cols))
#names(train) = cols



# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Benchmarking Seattle Buildings",
                  titleWidth = 500),
  dashboardSidebar(disable=T),
  dashboardBody(
    box(
      collapsed = TRUE,
      title = "Regression Tree", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 8, #height = "800",
      plotOutput("plotTree", height = "750")
    ),
    boxPlus(
      collapsed = TRUE,
      #height = "800",
      width = 4,
      title = "Variable importance and cbtable",
      closable = TRUE,
      status = "warning",
      solidHeader = TRUE,
      collapsible = TRUE,
      div(tableOutput("dataTableVarImp"), style = "font-size: 100%; width: 90%; height: 120%"),
      div(tableOutput("dataTableCPtable"), 
          style = "font-size: 90%; width: 100%; height: 120%")
      
      #dataTableOutput("dataTableFilters")
    ),
    box(
      collapsed = TRUE,
      title = "Site Energy Usage Distribution (before and after grouping)", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 8,
      plotOutput("boxPlots", height = "800")
    ),
    box(
      collapsed = TRUE,
      title = "Step-wise regression", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 4,
      uiOutput("stepWiseRegGroupAnova")
    ),
    box(
      collapsed = F,
      title = "Step-wise linear regression - without grouping", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      #h4("A linear regression model is fit and then EUI is predicted using the same set of buildings (average of the peer group) "),
      #h4("EER = Actual EUI / Predicted EUI"),
      plotOutput("stepWiseRegPlot", height = "600"),
      tableOutput("stepWiseRegTable"),
      uiOutput("stepWiseRegAnova")
    ),
    box(
      collapsed = TRUE,
      title = "Benchmarking using Energy Efficiency Ratio (EER)", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 6,
      #h4("A linear regression model is fit and then EUI is predicted using the same set of buildings (average of the peer group) "),
      h4("EER = Actual EUI / Predicted EUI"),
      plotOutput("benchmarkEERPlot", height = "700")
    )
  )
)


scale_data <- function(df, exclude) {
  for(col in setdiff(names(df), exclude)) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      df[, col] = scale(df[, col], center = T, scale = T)
    }
  }
  return (df)
}

unscale_data <- function(df) {
  for(col in names(df)) {
    if( is.numeric(df[, col]) ){
      # https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting
      # https://rdrr.io/cran/DMwR/man/unscale.html
      #df[, col] = scale(df[, col], center = T, scale = T)
      df[, col] = unscale(df[, col], df[, col])
    }
  }
  return (df)
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  getTrainingData <- reactive({
    train0 = train %>% 
      mutate(PredominantUseCode = as.character(PredominantUseCode)) %>%
      mutate(SiteEnergyUseLog = log(SiteEnergyUse)) %>%
      mutate(SourceEUILog = log(SourceEUI)) %>%
      mutate_if(is.character, as.factor)
  })
  
  getRpartModel <- reactive({
    
    train = getTrainingData()
    vars = setdiff(names(train), c("SiteEnergyUse",
                                   "SiteEnergyUseLog",
                                   "SourceEUI", "SourceEUILog",
                                   "HeatinSystemCode",
                                   "PredominanUse", 
                                   #"PredominantUseCode",
                                   "HeatinSystemCode"
                                   ))
    model = as.formula( paste("SiteEnergyUseLog ~ ", paste(vars, collapse="+")))
    
    ### build model
    rpart.model = rpart(model, 
                        method="anova", 
                        data = train,
                        control = rpart.control(
                          #cp = 0.0065,
                          minsplit = 10))
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    rpart.model
  })
  
  getGroupedData <- reactive({
    train = getTrainingData()
    rp = getRpartModel()
    grouped = cbind(train, group = rp$where)
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    grouped
  })
  
  
  output$plotTree <- renderPlot({
    rp = getRpartModel()
    
    prp(rp, faclen = 1, cex = 1.5, 
        fallen.leaves = F,
        extra = 1,
        type = 5,
        box.palette = "auto")
  })
  
  output$dataTableVarImp <- renderTable({
    rp = getRpartModel()
    
    vi <- varImp(rp) %>% 
      mutate(Attribute=row.names(.)) %>%
      mutate(Overall=round(Overall,2)) %>%
      mutate(Percentage = round(Overall*100/sum(Overall), 2)) %>%
      arrange(-Overall)
  })

  output$dataTableCPtable <- renderTable({
    rp = getRpartModel()
    rp$cptable
  })
  
  output$boxPlots <- renderPlot({
    
    grouped = getGroupedData()
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    theme_set(theme_gray(base_size = 20))
    g1 = ggboxplot(grouped, 
              x = "PredominantUseCode", 
              y = "SiteEnergyUseLog",
              color = "PredominantUseCode",
              shape = "PredominantUseCode"
              #add = "jitter"
    ) + theme_bw() + theme(text = element_text(size=18),
                           axis.text.x = element_text(size=18),
                           axis.text.y = element_text(size=18))
    
    g2 = ggboxplot(grouped, 
              x = "group", 
              y = "SiteEnergyUseLog",
              color = "group",
              shape = "group"
              #add = "jitter" 
    ) + theme_bw() + theme(text = element_text(size=18),
                           axis.text.x = element_text(size=18),
                           axis.text.y = element_text(size=18))
    
    ggarrange(g1, g2, ncol=1)
    
  })
  
  getStepWiseRegressionModelAllData <- reactive({
    
    cat(paste("getStepWiseRegressionModelAllData \n"))
    train = getTrainingData()
    tr = train
    
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    df1 = train
    #df1$EUI = log(df1$EUI)
    #df1$TotalEnergy = log(df1$TotalEnergy) 
    df2 = scale_data(df1, c())
    
    train0 = as.data.frame(df2)
    
    vars = setdiff(names(train0), c("SiteEnergyUse",
                                   "SiteEnergyUseLog",
                                   "SourceEUI", "SourceEUILog",
                                   "HeatinSystemCode",
                                   "PredominanUse", 
                                   "PredominantUseCode",
                                   "HeatinSystemCode"
    ))
    model = as.formula( paste("SiteEnergyUseLog ~ ", paste(vars, collapse="+")))
    print(paste("model ", model))
    
    fit.model <- lm(model, data = train0)
    #print(fit.model)
    #summary(fit.model)
    
    #varImp(fit.model, scale = F)
    
    step.model <- stepAIC(fit.model, direction = "both", 
                          trace = F)
    
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    #saveRDS(fit.model, "fit.model.RDS")
    #saveRDS(step.model, "step.model.RDS")
    
    list("fit" = fit.model, "step" = step.model)
    
  })
  
  
  
  
  output$stepWiseRegPlot <-renderPlot({
    
    cat(paste("stepWiseRegPlot \n"))
    
    swReg = getStepWiseRegressionModelAllData()
    
    fit.model = swReg[["fit"]]
    step.model = swReg[["step"]]
    
    #print(fit.model)
    #summary(fit.model)
    
    #varImp(fit.model, scale = F)
    
    #summary(step.model)
    par(mfrow=c(2,2))
    plot(step.model, ask=F, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    #varImp(step.model, scale = F)
  })
  
  output$stepWiseRegTable <-renderTable({
    
    cat(paste("stepWiseRegTable \n"))
    
    swReg = getStepWiseRegressionModelAllData()
    
    fit.model = swReg[["fit"]]
    step.model = swReg[["step"]]
    
    ss = summary(step.model)
    coef = as.data.frame(ss$coefficients)
    coef = cbind(Variable = rownames(coef), coef)
    rownames(coef) = NULL
    #coef
    
    vi <- varImp(step.model) %>% 
      mutate(Attribute=row.names(.)) %>%
      #mutate(Overall=round(Overall,2)) %>%
      mutate(Percentage = round(Overall*100/sum(Overall), 2)) %>%
      arrange(-Percentage) 
    
    vi
    
  })
  
  
  output$stepWiseRegAnova <-renderUI({
    
    cat(paste("stepWiseRegAnova \n"))
    swReg = getStepWiseRegressionModelAllData()
    #swReg = getStepWiseRegressionModelWithInteraction()
    
    
    fit.model = swReg[["fit"]]
    step.model = swReg[["step"]]
    
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    
    
    #summ(step.model, confint = TRUE, digits = 3)
    HTML(
      paste(
        c("<pre>", capture.output(summ(step.model, scale=T)), "</pre>"),
        collapse = "<br>"
      ),
      paste(
        c("<pre>", capture.output(print(anova(step.model))), "</pre>"),
        collapse = "<br>"
      ),
      paste(
        c("<pre>", capture.output(confint(step.model)), "</pre>"),
        collapse = "<br>"
      )
    )
  })
  
  
  
  output$benchmarkEERPlot <- renderPlot({
    
    cat(paste("benchmarkEERPlot \n"))
    swReg = getStepWiseRegressionModelAllData()
    fit.model = swReg[["fit"]]
    step.model = swReg[["step"]]
    
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    #str(step.model.int$model)
    
    dat0 = step.model$model
    pre0 = predict(step.model, dat0)
    pre = unscale(pre0, dat0$SiteEnergyUseLog)
    
    dat = unscale_data(dat0)
    
    eer = as.numeric(dat$SiteEnergyUseLog) / as.numeric(pre)
    #eer = dat$TotalEnergy / mean(pre)
    
    dat["eer"] = round(eer,2)
    
    #plot(pre, dat$TotalEnergy)
    # wwest(x=dat[,2:7],y=hawk[,1],bij="WIL")
    #plot(step.model)
    
    #eer_sorted = sort(eer)
    #plot(eer_sorted)
    
    eer_sorted = sort(eer)
    #plot(eer_sorted, xlab = "Building Id", ylab = "Energy Efficiency Ratio")
    
    # Plot cumaltive percentage for energy efficiency ratio
    eer_cs = cumsum(eer_sorted)
    eer_pr = cumsum(eer_sorted) / sum(eer_sorted)
    
    
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    plot(eer_sorted,eer_pr, main = "Cumalativer percentile of EER",
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
         #cex.lab=2.0, cex.axis=2.0, cex.main=2.0, cex.sub=2.0,
         xlab = "Energy Efficiency Ratio", 
         ylab = "Cumalative percentile")
    abline(h = 0.8, col = "red", lwd = 2)
    abline(v = 1.6, col = "blue", lwd = 2)
  })
  
  
  
  getStepWiseRegressionModel <- function (df) {
    
    df1 = scale_data(df, c("group"))
    
    ### ignore columns with same values, NAs, in all rows
    df2 = df1 %>% select_if( ~sum(!is.na(.)) > 0 )
    
    train0 = as.data.frame(df2)
    
    factors = setdiff(names(train0), c("SiteEnergyUse",  "SiteEnergyUseLog",
                                       "SourceEUI", "SourceEUILog", 
                                       "PredominanUse", "HeatinSystemCode",
                                       "Sprinklers", "Elevators",
                                       "group"))
    model = as.formula( paste("SourceEUILog ~ ", paste(factors, collapse="+")))
    g2 = train0[, factors]
    
    fit.model <- lm(model, data = df)
    #print(fit.model)
    #summary(fit.model)
    r2 = summary(fit.model)$r.squared
    
    if(r2 == 1) {
      step.model = fit.model
    } else {
      step.model <- stepAIC(fit.model, direction = "both", 
                            trace = FALSE)
    }
    
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    return( list("fit" = fit.model, 
                 "step" = step.model) )
  }
  
  getStepWiseRegressionModelGroup1 <- reactive({
    
    cat(paste("getStepWiseRegressionModelGroup1 \n"))
    
    grouped = getGroupedData()
    
    # get the biggest group no
    sr = sort(table(grouped$group), decreasing = T)
    groupNo = as.numeric(names(sr)[1])
    
    df = grouped %>% filter(group == groupNo)
    swReg1 = getStepWiseRegressionModel(df)
    
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    swReg1
  })

  
  getStepWiseRegressionModelGroupAll <- reactive({
    
    cat(paste("getStepWiseRegressionModelGroupAll \n"))
    grouped = getGroupedData()
    
    fit  = list()
    step = list()
    
    for(groupNo in unique(grouped$group)) {
      
      print(paste("Executing step-wise regression for group ", groupNo))
      
      df = grouped %>% filter(group == groupNo)
      swReg = getStepWiseRegressionModel(df)
      
      fit[[as.character(groupNo)]] = swReg[["fit"]]
      step[[as.character(groupNo)]] = swReg[["step"]]
    }
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    return( list("fit" = fit, 
                 "step" = step) )
  })
  
  output$stepWiseRegGroupAnova <-renderUI({
    
    cat(paste("stepWiseRegGroupAnova \n"))
    
    swRegAll = getStepWiseRegressionModelGroupAll()
    
    for(grp in names(step)) {
      fit = step[[grp]]
      ss = summary(fit)
      print(paste(grp, ss$r.squared))
    }
    
    swReg = getStepWiseRegressionModelGroup1()
    fit.model = swReg[["fit"]]
    step.model = swReg[["step"]]
    
    lapply(ls(), function(x) {assign(x, get(x), envir = .GlobalEnv)})
    
    #summ(step.model, confint = TRUE, digits = 3)
    HTML(
      paste(
        c("<pre>", capture.output(summ(step.model, scale=T)), "</pre>"),
        collapse = "<br>"
      ),
      paste(
        c("<pre>", capture.output(print(anova(step.model))), "</pre>"),
        collapse = "<br>"
      )
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyapps_py = '/usr/bin/python'
#Sys.setenv(RETICULATE_PYTHON = shinyapps_py)
reticulate::use_python(shinyapps_py, required = TRUE)


library(shiny)
library(shinydashboard)
library(reticulate)

cat("\n\n\n\n---- starting install -----\n", file = stderr())

print(virtualenv_list())
print(virtualenv_root())


# https://stackoverflow.com/questions/54977188/problem-deploying-shiny-app-in-r-using-a-virtual-env-with-reticulate-to-run-pyt

#py_packages = c("pandas", "xgboost", "shap")
py_packages = c("pandas", "xgboost", "matplotlib")
virtualenv_create(envname = "test") #, python= shinyapps_py)
virtualenv_install("test", packages = py_packages)
use_virtualenv("test", required = TRUE)

#py_install(py_packages)
print(virtualenv_list())
print(py_config())

p = import("pandas")
p = import("numpy")
p = import("xgboost")
p = import("matplotlib")

cat("\n\n ---- starting test code -----\n", file = stderr())
source_python("app_test.py")

cat("\n\n ---- starting test code done-----\n", file = stderr())

#reticulate::use_virtualenv(virtualenv = "r-reticulate")
#print(py_config())

#use_virtualenv('C:\\Users\\sbbpan\\AppData\\Local\\CONTIN~1\\ANACON~1\\envs\\r-reticulate')

ht = '<div id=\'i2FH4XYKJ2BU4TDR5HUD0\'>
</div>
<script>
if(window.SHAP)  console.log("true")

if (window.SHAP) SHAP.ReactDom.render(
SHAP.React.createElement(SHAP.AdditiveForceVisualizer, {"featureNames": ["SqFt", "OpenHours", "WorkersCnt", "ComputersCnt", "IsBank", "CooledPercent", "CDD65"], "features": {"0": {"effect": -12.889948844909668, "value": -66463.0}, "1": {"effect": 0.7079159617424011, "value": 5.7}, "2": {"effect": -14.73103141784668, "value": -209.3}, "3": {"effect": 2.062133312225342, "value": -302.0}, "4": {"effect": -2.5432956218719482, "value": 0.0}, "5": {"effect": 7.174017429351807, "value": 9.6}, "6": {"effect": 21.18121337890625, "value": 1227.9}}, "baseValue": 182.12277221679688, "plot_cmap": "PkYg", "labelMargin": 20, "link": "identity", "outValue": 183.08377075195312, "outNames": ["output value"]}),
document.getElementById(\'i2FH4XYKJ2BU4TDR5HUD0\')
);
</script>'


data = read.csv("office_all.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Explainable Energy Benchmarking system - Office Buildings in the US"),
   
   tags$head(includeScript("./www/bundle.js")),
   
   fluidRow(
     column(8,
            sliderInput("bix",
               "Building index:",
               min = 0,
               max = nrow(data)-1,
               value = 30, width = "100%")),
     column(4,
            actionButton("show", h3("Benchmark a new building")))
     ),
   uiOutput("shapPlot"),
   tableOutput("dataTable"),
   uiOutput("shapPlotAll")
)

dataModal <- function(failed = FALSE) {
  
  modalDialog(
    
    h4("Enter your building attributes"),
    
    sliderInput("sqft", "Avg. weekly operating hours",
                min = 1000, max = 100000, value = 10000, step=100, width = '100%'),
    
    sliderInput("OpenHours", "Avg. weekly operating hours",
                min = 1, max = 168, value = 60, width = '100%'),
    
    sliderInput("WorkersCnt", "No. of employees",
                min = 0, max = 10000, value = 500, step=10, width = '100%'),
    
    sliderInput("ComputersCnt", "No. of computers/terminals",
                min = 0, max = 10000, value = 500, step=10, width = '100%'),

    sliderInput("CooledPercent", "% of the building cooled",
                min = 0, max = 100, value = 50, step=1, width = '100%'),
    
    sliderInput("CDD65", "Cooling Degree Days (base 65)",
                min = 0, max = 10000, value = 500, step=10, width = '100%'),
    
    checkboxInput("IsBank", "Is Bank ?", value = FALSE),
    
    if (failed)
      div(tags$b("Invalid name of data object", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK")
    )
  )
}


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  observeEvent(input$show, {
    showModal(dataModal())
  })
  
  
  output$dataTable <- renderTable({
    ix = input$bix + 1
    d1 = data[ix, ]
    d1
  })
  
   output$shapPlot <- renderUI({
     ix = input$bix
     
     plt1 = NULL
     #source_python("app.py")
     #plt1 = get_force_plot(ix)
     #plt1$data
     #plt1$data
     #HTML(ht)
     #write_file(plt1$data, "shap_div.html")
     #HTML(plt1$data)
   })
   
   output$shapPlotAll <- renderUI({
     plt2 = NULL
     #source_python("app.py")
     #plt2 = get_force_plot_all()
     
     #plt1$data
     #plt1$data
     #HTML(ht)
     #write_file(plt2$data, "shap_div_all.html")
     #HTML(plt2$data)
     #HTML("to be done!")
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)



# 
# shap = import("shap")
# xgboost = import("xgboost")
# 
# py_run_string("import os")
# py_run_string("cwd = os.getcwd()")
# 
# py_capture_output("print(cwd1)")
# 
# 
# shap.initjs()	
# ds = shap$datasets$boston()
# X = ds[[1]]
# y = ds[[2]]
# 
# data(agaricus.train, package='xgboost')
# data(agaricus.test, package='xgboost')
# 
# test <- agaricus.test
# 
# dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
# dtest <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)
# watchlist <- list(train = dtrain, eval = dtest)
# 
# ## A simple xgb.train example:
# param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2, 
#               objective = "binary:logistic", eval_metric = "auc")
# bst <- xgb.train(param, dtrain, nrounds = 2, watchlist)
# 
# pred_contr <- predict(bst, test$data, predcontrib = TRUE)
# 
# 
# library(reticulate)
# source_python("app.py")
# xd <- get_x()
# 
# 
# write_file(plt1$data, "shap_div.html")

# matplotlib <- import("matplotlib") 
# matplotlib$use("Agg", force = TRUE)

#source_python("app.py")
#plt3 = get_summary_plot()
#get_summary_plot

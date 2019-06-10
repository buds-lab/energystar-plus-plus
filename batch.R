filenames = c("hotel", "k12school", "multifamily", 
              "office", 
              "retail", "warehouse", "worship")

for (filename in filenames) {
  filename = paste0(filename, ".Rmd")
  print(paste(Sys.time(), "Executing", filename))
  rmarkdown::render(input=filename, quiet=TRUE)
}
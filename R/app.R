# Run your app.R to load the GUI

run_app <- function(){

oldwd <- getwd()
on.exit(setwd(oldwd))

setwd("C:/Users/User/Desktop/GUI/GreymodelsPackage/R")

source('app_ui.R', local = TRUE)
source('app_server.R')

shinyApp(ui, server)


}



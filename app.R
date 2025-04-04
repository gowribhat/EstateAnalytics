source("global.R")
source("ui.R")
source("server/server.R")

<<<<<<< Updated upstream
shinyApp(ui, server)
=======
shinyApp(ui = ui, server = server)

HDB <- readRDS("hdb.rds")
priv <- readRDS("ura_private.rds")
>>>>>>> Stashed changes

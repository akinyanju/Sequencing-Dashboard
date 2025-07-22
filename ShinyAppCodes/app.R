# Source relevant path, expected to be in same location as app.R
source("configPaths.R")

# Source files using relative paths
source(file.path(base_path, "libraries/libraries.R"))
source(file.path(base_path, "input/inputFiles.R"))
source(file.path(base_path, "loginAuth/auth.R"))

# UI and server modules
source(file.path(base_path, "modules/landingPageUI.R"))
source(file.path(base_path, "modules/landingPageServer.R"))
source(file.path(base_path, "modules/dashboardSideBarUI.R"))
source(file.path(base_path, "modules/dashboardBodyUI.R"))
source(file.path(base_path, "modules/dashboardHeaderUI.R"))
source(file.path(base_path, "modules/dashboardServer.R"))

# Global UI and servers
source(file.path(base_path, "global/ui.R"))
source(file.path(base_path, "global/server.R"))

# Launch appS
shinyApp(ui, server)


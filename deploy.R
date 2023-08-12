Sys.setenv(LANG = "en")
library(rsconnect)
library(BiocManager)

source("src/common_util.R")
deployment_options <- readDeploymentOptions()
DEPLOYMENT_MODE_ENABLED = deployment_options$deployment_mode
if(DEPLOYMENT_MODE_ENABLED){
  application_title = deployment_options$shinyapps_title
  shinyapps_account = deployment_options$shinyapps_account
} else {
  application_title = "RokaiXplorer"
  shinyapps_account = "syilmaz"
}

options(repos = BiocManager::repositories())
deployApp(appName = application_title, appTitle = application_title, account = shinyapps_account)



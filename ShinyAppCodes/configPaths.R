
#1 Production server paths. Uncomment

# dir_InputFile <- "/srv/shiny-server/.InputDatabase"
# base_path <-"/srv/shiny-server/"

#2. Dev or Testing Mode paths. Comment out if being launched in production
base_path <- file.path("/Fake/Path/ShinyAppCodes")
dir_InputFile <- file.path("/Fake/Path/ShinyAppCodes/SampleData")

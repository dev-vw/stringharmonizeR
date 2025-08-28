# run these two lines if you don't have remotes or devtools installed
libs <- c("remotes, devtools")
install.packages(libs)

# fast way to load multiple libraries in one line of code
lapply(libs, library, character.only = TRUE)

# begin update process
remotes::update_packages()

# initialize virtual environment (ensures reproducibility)
if (!('renv.lock' %in% dir())) {
  renv::init()
} else {
  print("Detected R environment")
}


# activate environment
renv::activate()
print("Successfully activated R environment")

# synchronize the current environment with the lockfile
renv::restore()

# useful package which ensures that if a library is not found it
# is downloaded instead of raising an error
if (!('pacman' %in% renv::dependencies()$Package)) {
  install.packages("pacman")
}

library(threejs)

# Tutorial
# https://rpubs.com/aagarwal29/r3dglobe

# Sets up 3D viz on notebooks
# knit_hooks$set(webgl = hook_webgl)

# Get the image of the globe
earth <- system.file("images/world.jpg",  package="threejs")

# Create empty globe
globejs(
  img = earth, atmosphere = TRUE, height = 800, width = 800, bg = "white"
)

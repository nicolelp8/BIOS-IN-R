# 1. Load the packages yourself. In this tutorial dplyr, tidyr and ggplot2 were loaded for you in every step.
#  A script gets no such favour, so it must begin with its own library() calls.

# 2. Keep the command-line arguments. commandArgs(trailingOnly = TRUE), args[1], args[2], as.integer(args[3]) 
# and the paste0() that builds the output filename can stay exactly as they are. 
# They are not plotting code, and there is no tidyverse replacement you need here.

# 3. Replace the indexing with filter(). One filter() call on iris_data, keeping the rows whose Species is in 
# selected_species. %in% works inside filter() just as it does inside [.

# 4. Replace plot() and legend() with a ggplot. ggplot() with aes() mapping Petal.Width to x, Petal.Length to y 
# and Species to colour, plus geom_point(). The legend you had to build by hand with legend() now comes free 
# with the colour mapping. Use labs() for the axis labels and for a title naming the two species.

# 5. Replace png() and dev.off() with ggsave(). Assign the plot to a variable and save that variable. See below.

Test it on the command line, then commit it. The same command with the same arguments, 
run from the Terminal and not the R Console, must produce a figure that shows the same thing as before. 
Only once it does should you commit the rewritten script.


> args <- commandArgs(trailingOnly = TRUE)
> species_a <- args[1]
> species_b <- args[2]
> image_size <- as.integer(args[3])
> selected_species <- c(species_a,species_b)

iris_data <- datasets::iris

species_colours <- c(
  setosa = "tomato",
  versicolor = "steelblue",
  virginica = "gold"
)

# Keep only data from the selected species
iris_subset <- iris_data[iris_data$Species %in% selected_species, ]

# EXERCISE: Create an output filename using species_a and species_b.
#
# Example: "iris_setosa_vs_versicolor.png"
#
# output_file <- paste0(
#   "iris_",
#   species_a,
#   "_vs_",
#   species_b,
#   ".png"
# )

# Open plot

ggplot(aes(x = Petal.Width, y = Petal.Length)) +
  geom_col() +
  labs(x = "Petal width", y = "Petal length")

ggsave(
  filename = output_file,
  plot = species_plot,
  width = image_size,
  height = image_size,
  units = "px",
  dpi = 150)


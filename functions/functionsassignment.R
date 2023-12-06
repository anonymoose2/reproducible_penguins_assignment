
##List of functions in order of use:


# A function to make sure the column names are cleaned up, 
# eg lower case and snake case
clean_column_names <- function(penguins_raw) {
  penguins_raw %>%
    clean_names()
}

# A function to make sure the species names are shortened
shorten_species <- function(penguins_raw) {
  penguins_raw %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}

# A function to remove any empty columns or rows
remove_empty_columns_rows <- function(penguins_raw) {
  penguins_raw %>%
    remove_empty(c("rows", "cols"))
}


# A function to subset the data based on the list of column names
subset_columns <- function(penguins_clean, column_names) {
  penguins_clean %>%
    select(all_of(column_names))
}

#A function to remove rows which contain NA values
remove_NA <- function(penguins_raw) {
  penguins_raw %>%
    na.omit()
}

#a function to plot the exploratory scattergraph
data_plot <- function(penguins_subset){
  ggplot(penguins_subset, aes(x=body_mass_g, y=flipper_length_mm, colour=species))+
    geom_point(color = "black", size = 2, alpha=0.5) +
    theme_bw()
}
  


#functions to save the exploratory scattergraph, both as a png and svg


save_png_exploratory <- function(){agg_png("figures/exploratoryplot.png", 
                               width = 500, height = 500, units = "px")
  penguins_plot
  dev.off() 
}


save_svg_exploratory <- function(){svglite("figures/exploratoryvector.svg", 
                               width = 5.9, height = 5.9)
  penguins_plot
  dev.off()}


#function to create a results figure

data_plot_regression <- function(penguins_subset){
ggplot(penguins_subset, aes(x=body_mass_g, y=flipper_length_mm, colour=species))+
 geom_point(color = "black", size = 2, alpha=0.5) +
 geom_smooth(method = "lm", se = TRUE, color = "orange")+
  theme_bw() +
    ggtitle("Regression of Body Mass vs Flipper Length")
}

#functions to save the results figure
save_png_results <- function(){agg_png("figures/resultsplot.png", 
                                           width = 500, height = 500, units = "px")
  penguins_plot
  dev.off() 
}


save_svg_results <- function(){svglite("figures/resultsvector.svg", 
                                           width = 5.9, height = 5.9)
  penguins_plot
  dev.off()}
  

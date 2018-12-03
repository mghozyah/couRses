library(ggvis)
library(tidyverse)
data("mtcars")

#############################
#Chapter 1              
#The Grammar of Graphics
#############################

####Load ggvis and start to explore####
# Change the code below to plot the disp variable of mtcars on the x axis
mtcars %>% ggvis(~disp, ~mpg) %>% layer_points()
####

####ggvis and its capabilities####
# Change the code below to make a graph with red points
mtcars %>% ggvis(~wt, ~mpg, fill := "red") %>% layer_points()
# Change the code below draw smooths instead of points
mtcars %>% ggvis(~wt, ~mpg) %>% layer_smooths()
# Change the code below to make a graph containing both points and a smoothed summary line
mtcars %>% ggvis(~wt, ~mpg) %>% layer_points() %>% layer_smooths()
####

####ggvis grammar ~ graphics grammar####
# Adapt the code: show bars instead of points
pressure %>% ggvis(~temperature, ~pressure) %>% layer_bars()
# Adapt the code: show lines instead of points
pressure %>% ggvis(~temperature, ~pressure) %>% layer_lines()
# Extend the code: map the fill property to the temperature variable & size prop to the pressure
pressure %>% ggvis(~temperature, ~pressure,
                   size = ~pressure,
                   fill = ~temperature) %>% 
  layer_points() %>% 
  add_axis("y", title = "The Pressure (p)") %>%
  add_axis("x", title = "The Temperature")
####

#############################
#Chapter 2              
#Lines and Syntax
#############################

####Three operators: %>%, = and :=####
# Rewrite the code with the pipe operator
faithful %>%
  ggvis( ~waiting, ~eruptions) %>%
  layer_points()
# Modify this graph to map the size property to the pressure variable
pressure %>% 
  ggvis(~temperature, ~pressure, size = ~pressure) %>% 
  layer_points() 
# Modify this graph by setting the size property
pressure %>% 
  ggvis(~temperature, ~pressure, size := 100) %>% 
  layer_points()
# Fix this code to set the fill property to red
pressure %>% 
  ggvis(~temperature, ~pressure, 
        fill := "red") %>% 
  layer_points()
####

####Referring to different objects###
#Which of the commands below will create a graph that has green points? 
#Try to predict the answer before running the code.
red <- "green"
pressure$red <- pressure$temperature
# GRAPH A
pressure %>%
  ggvis(~temperature, ~pressure,
        fill = ~red) %>%
  layer_points()
# GRAPH B
pressure %>%
  ggvis(~temperature, ~pressure,
        fill = "red") %>%
  layer_points()
# GRAPH C
pressure %>%
  ggvis(~temperature, ~pressure,
        fill := red) %>%
  layer_points()
###ANSWER IS GRAPH C
####

####Properties for points####
  # You can manipulate many different properties when using layer_points(), 
  # including x, y, fill, fillOpacity, opacity, shape, size, stroke, strokeOpacity, andstrokeWidth.
  # The shape property recognizes several different values: 
  # circle (default), square, cross, diamond, triangle-up, and triangle-down.
# Add code, play with opacity
faithful %>% 
  ggvis(~waiting, ~eruptions, size = ~eruptions,
        opacity := 0.5, fill := "blue",
        stroke := "black") %>% 
  layer_points()
# Add code, play with fillOpacity
faithful %>% 
  ggvis(~waiting, ~eruptions,
        fillOpacity = ~eruptions,
        size := 100, fill := "red",
        stroke := "red", shape:="cross") %>%
  layer_points()
####

####Properties for lines####
# Update the code
# pressure %>% ggvis(~temperature, ~pressure) %>% layer_points()
pressure %>% 
  ggvis(~temperature, ~pressure,
        stroke := "red",  #this controls the color.
        strokeWidth := 2, # this controls the line width.
        strokeDash := 6) %>%  #this controls the length of the dashes.
  layer_lines()
####

####Path marks and polygons####
texas <- read_csv("texas.csv")
# Update the plot
#texas %>% ggvis(~long, ~lat) %>% layer_lines()
texas %>% 
  ggvis(~long, ~lat,
        strokeDash := 5) %>%  #This will color your map!
  layer_paths(fill :="darkorange") #this will correctly plot the map of Texas.
####

####Display model fits####
?compute_model_prediction
#sample
faithful %>%
  compute_model_prediction(eruptions ~ waiting,
                           model = "lm")
# Compute the x and y coordinates for a loess smooth line that predicts mpg with the wt
mtcars %>% 
  compute_smooth( mpg ~ wt)
####

####compute_smooth() to simplify model fits####
#sample
faithful %>%
  compute_smooth(eruptions ~ waiting) %>%
  ggvis(~pred_, ~resp_) %>%
  layer_lines()
# Extend with ggvis() and layer_lines()
#mtcars %>% compute_smooth(mpg ~ wt)
mtcars %>% 
  compute_smooth(mpg ~ wt) %>%
  ggvis(~pred_, ~resp_) %>%
  layer_lines()
# Extend with layer_points() and layer_smooths()
#mtcars %>% ggvis(~wt, ~mpg)
mtcars %>%
  ggvis(~wt, ~mpg) %>%
  layer_points() %>%
  layer_smooths()
####

#############################
#Chapter 3              
#Transformations
#############################

####Histograms (1)####
####

####Histograms (2)####
####

####Density plots####
####

####Shortcuts###
####

####ggvis and group_by####
####

####group_by() versus interaction()####
####

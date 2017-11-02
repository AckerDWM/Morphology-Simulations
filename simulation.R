library(plyr); library(dplyr)

# Simulation
neurons = simulate_n_neurons(
  n_neurons=40, 
  p_branching=.003, 
  n_timesteps=1000, 
  n_primary_neurites=5)

# Spatial annotations
spatial_neurons = neurons %>%
  filter(neuron_ID %in% 1:5) %>%
  ddply("neuron_ID", function(neuron_i) add_spatial_info(neuron_i) )
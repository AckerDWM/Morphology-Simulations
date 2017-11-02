library(dplyr)

grow_1_step = function(neuron, p_branching) {
  n_branches = nrow(neuron)
  to_branch = rbinom(n_branches, 1, p_branching) == T
  to_branch[!neuron$is_tip] = F
  if (sum(to_branch) == 0) {
    neuron$length[neuron$is_tip] = neuron$length[neuron$is_tip] + 1
  } 
  else {
    neuron$length[!to_branch & neuron$is_tip] = neuron$length[!to_branch & neuron$is_tip] + 1
    neuron$is_tip[to_branch] = F
    new_branches = rbind(neuron[to_branch,], neuron[to_branch,]) # bifurcate
    new_branches$parent = new_branches$ID
    new_IDs = seq(max(neuron$ID)+1, max(neuron$ID)+sum(to_branch)*2)
    new_branches$ID = new_IDs
    new_branches$is_tip = T
    new_branches$length = 1
    neuron = rbind(neuron, new_branches)
  }
  rownames(neuron) = NULL
  return(neuron)
}

simulate_n_neurons = function(n_neurons, p_branching, n_timesteps, n_primary_neurites) {
  neuron_ID = 1
  neurons = replicate(n_neurons, {
    neuron = data.frame(
      ID = 1:n_primary_neurites,
      parent = 0,
      length = 1,
      is_tip = T)
    for (i in 1:n_timesteps) neuron = grow_1_step(neuron, p_branching)
    neuron$neuron_ID = neuron_ID
    neuron_ID <<- neuron_ID + 1
    return(neuron)
  }, simplify=F) %>% 
    bind_rows()
  return(neurons)
}

add_spatial_info = function(neuron_i) {
  neuron_i$x = 0
  neuron_i$y = 0
  neuron_i$angle = 0
  neuron_i$parent_x = 0
  neuron_i$parent_y = 0
  
  for (i in 1:nrow(neuron_i)) {
    length = neuron_i$length[i]
    parent = neuron_i$parent[i]
    # if parent is zero
    parent_x = 0
    parent_y = 0
    parent_angle = runif(1, 0, 2*pi)
    is_not_primary_neurite = parent > 0
    if (is_not_primary_neurite) {
      parent_x = neuron_i$x[neuron_i$ID == parent]
      parent_y = neuron_i$y[neuron_i$ID == parent]
      parent_angle = neuron_i$angle[neuron_i$ID == parent]
    }
    angle = parent_angle + sample(c(pi/8, -pi/8), 1)
    if ((parent %in% neuron_i$parent[1:(i-1)]) & is_not_primary_neurite) {
      sister_angle = neuron_i$angle[1:(i-1)][neuron_i$parent[1:(i-1)] == parent]
      angle = parent_angle - (sister_angle - parent_angle)
    }
    neuron_i$angle[i] = angle
    neuron_i$x[i] = parent_x + length * sin(angle)
    neuron_i$y[i] = parent_y + length * cos(angle)
    neuron_i$parent_x[i] = parent_x
    neuron_i$parent_y[i] = parent_y
  }
  return(neuron_i)
}



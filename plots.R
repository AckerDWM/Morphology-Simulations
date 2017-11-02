library(dplyr); library(ggplot2); library(cowplot); library(qqplotr)

# Plots
theme_set(theme_classic())

## arbor length vs. tip number
p_dots = neurons %>%
  group_by(neuron_ID, p_branching) %>%
  summarize(tip_number = sum(is_tip), arbor_length = sum(length)) %>%
  ungroup() %>%
  ggplot(aes(arbor_length, tip_number)) + 
  geom_point() +
  geom_smooth(method="lm", color="black") +
  xlab("Total arbor length") +
  ylab("Branch tip number")

## segment length
p_hist = neurons %>%
  ggplot(aes(length)) + 
  geom_histogram(bins=nrow(neurons)^.5, fill="black") +
  ylab("Count") +
  xlab("Segment length")

p_cumDens = neurons %>%
  ggplot(aes(length)) + 
  geom_step(stat="ecdf") +
  stat_qq_line() +
  ylab("Cum. density") +
  xlab("Segment length")

p_qq = neurons %>%
  ggplot(aes(sample=length)) +
  stat_qq_point(distribution="exp") +
  stat_qq_band(distribution="exp") +
  stat_qq_line(distribution="exp", lty=2) +
  xlab("Theoretical (exp. dist.)") +
  ylab("Sample")

g_neuron = ggplot(spatial_neurons, aes(x, y, xend=parent_x, yend=parent_y)) +
  facet_grid(~neuron_ID) +
  geom_segment() +
  annotate("point", x=0, y=0, size=3) +
  coord_equal() +
  scale_x_continuous(breaks=c(-500, 500)) +
  scale_y_continuous(breaks=c(-500, 500))

## arange panel
ggdraw() +
  draw_plot(g_neuron, 0, 2/3, 1, 1/3) +
  draw_plot(p_hist, 0, 0, .5, 2/3) +
  draw_plot(p_qq, .2, 1/3, .24, .75/3) +
  draw_plot(p_dots, .5, 0, .5, 2/3) +
  draw_plot_label(label=letters[1:3], x=c(0, 0, .5), y=c(1, 2/3, 2/3))

library(tidyverse)
library(cowplot)
library(gganimate)
library(gifski)

# Make a table of boxes
probabilities <- seq(0, 0.5, length.out=100)[-1]
truth.table <- tibble(p = probabilities) %>%
  mutate(time = 1-p) %>%
  # Add a dummy column so I can do cross joins
  mutate(dummy=TRUE) %>%
  # Cross joins to make a two-proposition truth table
  left_join(tibble(A = c(TRUE, FALSE), dummy=TRUE), by="dummy") %>%
  left_join(tibble(B = c(TRUE, FALSE), dummy=TRUE), by="dummy") %>%
  # Remove the dummy column since I'm done with the cross joins
  dplyr::select(-dummy) %>%
  mutate(proposition = sprintf("%sA&%sB", ifelse(A, "", "~"), ifelse(B, "", "~")))
boxes <- truth.table %>%
  mutate(xmin = (!B) * p,
         xmax = p + (!B) * (1-p),
         ymin = A * (1-p),
         ymax = (1-p) + A * p) %>%
  mutate(center_x = (xmax + xmin) / 2,
         center_y = (ymax + ymin) / 2,
         box_label = sprintf("P(%s)", proposition))

# Make a table of box corners
box.corners <- boxes %>%
  pivot_longer(c(xmin, xmax), names_to = "horizontal_position", values_to = "x") %>%
  pivot_longer(c(ymin, ymax), names_to = "vertical_position", values_to = "y")

# Specify the outline of the part I want to highlight as a list of corners
path <- tibble(
  position = 1:9,
  horizontal_position = c("xmin", "xmin", "xmin", "xmin", "xmax", "xmax", "xmin", "xmax", "xmin"),
  vertical_position = c("ymin", "ymax", "ymax", "ymax", "ymax", "ymin", "ymin", "ymin", "ymin"),
  proposition = c("~A&B", "A&B", "A&B", "A&~B", "A&~B", "A&~B", "A&~B", "~A&B", "~A&B")
) %>%
  # Fill in the paths with actual numbers
  left_join(box.corners, by=c("horizontal_position", "vertical_position", "proposition"))

# Palette from https://beyoncepalettes.tumblr.com/post/122490884802
palette = c(
  lightblue = "#dbf4f8",
  blue = "#0db2e8",
  red = "#cf1f53",
  green = "#93ff7b",
  brown = "#4d343a",
  tan = "#a3a191"
)

color.map <- list(
  `A&B` = palette["red"],
  `A&~B` = palette["blue"],
  `~A&B` = palette["blue"],
  `~A&~B` = palette["lightblue"]
)

# Theme settings which will be used across plots
label.size <- 10
example.prob <- probabilities[which.min(abs(probabilities - 1/3))[1]]

# Offset of labels from sides, in units of philosophy square width
label.offset <- 0.125
# Table of labels on the side
# Specify the positions of labels relative to each box
labeled.boxes <- tibble(proposition = c("A&B", "~A&B", "A&B", "A&~B"),
    position = c("left", "left", "top", "top"),
    side_label = c("P(A)", "P(~A)", "P(B)", "P(~B)")) %>%
  # Join to the box data as a first step to figuring out the label coordinates
  full_join(boxes, by="proposition") %>%
  # Calculate the label coordinates from the box coordinates and the label positions
  mutate(
    side_label_x = ifelse(position == "left", xmin - label.offset, ifelse(position == "top", center_x, NA)),
    side_label_y = ifelse(position == "left", center_y, ifelse(position == "top", ymax + label.offset, NA))
  )

# Repeating the whole song and dance for the conditional plot
conditional.height <- 0.3
conditional.boxes <- truth.table %>%
  dplyr::filter(A | B) %>%
  mutate(
    xmin = (B*p*(1-p) + ((!A)&B)*p^2) / (1 - (1-p)^2),
    xmax = (p*(1-p) + B*p^2 + ((!A)&B)*p*(1-p)) / (1 - (1-p)^2),
    ymin = 0,
    ymax = conditional.height
  )
conditional.border <- tibble(
  x = c(0, 1, 1, 0, 0),
  y = c(1, 1, 0, 0, 1) * conditional.height,
  dummy = TRUE
)
conditional.path <- tibble(p = probabilities, dummy = TRUE) %>%
  mutate(time = 1-p) %>%
  left_join(conditional.border, by="dummy") %>%
  dplyr::select(-dummy)

# First plot: Introducing the philosophy square
# No borders, labels, single frame
introduction.plot <- ggplot(dplyr::filter(labeled.boxes, p == example.prob)) + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = proposition)) +
  scale_fill_manual(values = color.map) + 
  geom_text(aes(x = center_x, y = center_y, label = box_label), size = label.size) + 
  geom_text(aes(x = side_label_x, y = side_label_y, label = side_label), size = label.size) +
  theme_nothing() + 
  theme(plot.margin = unit(rep(0, 4), "in")) + 
  coord_equal() + 
  scale_x_continuous(limits = c(-2 * label.offset, 1), expand = c(0.005,0.005)) + 
  scale_y_continuous(limits = c(0, 1 + 2 * label.offset), expand = c(0.005,0.005))
ggsave("introduction_plot.png", introduction.plot, width = unit(6, "in"), height = unit(6, "in"), dpi = 540 / 6)

# Second plot: Highlighting the disjunction
# Borders, labels, single frame
disjunction.plot <- introduction.plot + 
  geom_path(aes(x = x, y = y), data=dplyr::filter(path, p == example.prob), colour = palette["brown"], lwd=2)
ggsave("disjunction_plot.png", disjunction.plot, width = unit(6, "in"), height = unit(6, "in"), dpi = 540 / 6)

# Third plot: Showing a trend
# Borders, no label, animated
trend.plot.full <- ggplot() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = proposition), data=boxes) +
  scale_fill_manual(values = color.map) + 
  geom_path(aes(x = x, y = y), data=path, colour = palette["brown"], lwd=2) + 
  transition_time(time) + 
  theme_nothing() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0, 1.001), expand = c(0.005,0.005)) + 
  scale_y_continuous(limits = c(0, 1), expand = c(0.005,0.005))
trend.plot.full.animation <- animate(trend.plot.full, renderer = gganimate::file_renderer(dir = "trend_plot_full", prefix = "frame", overwrite = TRUE), duration = 1, nframes = 99)

# Fourth plot: Showing a trend but conditioned on A or B
# Different boxes, borders, no labels, animated
trend.plot.conditional <- ggplot() + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = proposition), data = conditional.boxes) +
  scale_fill_manual(values = color.map) + 
  theme_nothing() + 
  theme(plot.margin = unit(rep(0, 4), "in")) + 
  coord_equal() +
  geom_path(aes(x = x, y = y), data=conditional.path, colour = palette["brown"], lwd=2) + 
  transition_time(time) + 
  scale_x_continuous(limits = c(0, 1.001), expand = c(0.005,0.005)) + 
  scale_y_continuous(limits = c(0, conditional.height), expand = c(0.005,0.005))
trend.plot.conditional.animation <- animate(trend.plot.conditional, renderer = gganimate::file_renderer(dir = "trend_plot_conditional", prefix = "frame", overwrite = TRUE), duration = 1, nframes = 99)

print(warnings())
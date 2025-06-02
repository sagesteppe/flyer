library(sf)
library(tidyverse)
library(smoothr)
library(hexSticker)
library(concaveman)
library(stplanr)
library(showtext)

setwd('~/Documents/flyer/data-raw')
source('functions.R')

# stars are easy
set.seed(37)

cols <- c('#DDC3D0', '#531CB3', '#E94F37', '#74A57F')
names(cols) <- c('Thistle', 'Chrysler', 'Syracuse', 'Cambridge')

# use 1-9 to easily partition plot panels into thirds.
stars <- data.frame(
  x = runif(75, min = 1, max = 9),
  y = runif(75, min = 5, max = 9),
  size = runif(75, min = 0.1, max = 1.2),
  col = sample(names(cols), 75, replace = T, prob = c(2,1,1,1))
) |>
  st_as_sf(coords = c('x', 'y'))


moon <- data.frame(
  x = 5, y = 8
) |>
  st_as_sf(coords = c('x', 'y')) |>
  st_buffer(0.5)


### create the boat

cab <- sfheaders::sf_polygon(
  data.frame(
    x = c(7.5, 4.25, 4.25, 7.5, 7.5),
    y = c(4.2, 4.75, 5.75, 6, 4.2)
  )
)|>
  smoothr::smooth(method = 'ksmooth', smoothness = 0.15)

hull <-  sfheaders::sf_polygon(
  data.frame(
    x = c(1.5, 2.25, 7.5, 8.25, 7.25, 6,    5, 1.5),
    y = c(4.25, 3.25, 3.25, 5.25, 5.25, 5, 4.75, 4.25)
  )
) |>
  smoothr::smooth(method = 'ksmooth', smoothness = 0.15)


######## create waves - takes  little bit of work.
y <- rep(3.5, length = 17)
y[4] <- 4.25
y[8] <- 3.25
y[13] <- 4
y[14] <- 4.1
y[15] <- 4.25
y[16] <- 3.25

wave <- data.frame(
    x = seq(1, 9, by = 0.5),
    y = y
  )

waveline <- wave |>
  sfheaders::sf_linestring() |>
  smoothr::smooth(method = 'ksmooth', smoothness = 2.5)

wavebottom <- data.frame(
  x = c(jitter(seq(1, 9, by = 0.5), factor = 3) - 0.75, 9),
  y = c(jitter(wave$y-0.9, factor = 5), 2)
)|>
  sfheaders::sf_linestring() |>
  smoothr::smooth(method = 'ksmooth', smoothness = 2.5)

wave <- bind_rows(waveline, wavebottom)  %>%
  st_combine() %>%
  st_cast("POINT") %>%
  st_sf() %>%
  concaveman() %>%
  rename(geometry = polygons)

rm(wavebottom, waveline, y)

## and now create some sea stars !!! :-)

l <- list(
  'a' = st_sf(geometry = st_sfc(create_sea_star(x = 3.4, y = 1.9, r = 0.25, armL = 2))),
  'b' = st_sf(geometry = st_sfc(create_sea_star(x = 6.4, y = 2.1, r = 0.2, armL = 3.5))),
  'c' = st_sf(geometry = st_sfc(create_sea_star(x = 2.2, y = 2.3, r = 0.2, armL = 1.5))),
  'd' = st_sf(geometry = st_sfc(create_sea_star(x = 7.4, y = 2.3, r = 0.15, armL = 2)))
  )
l <- lapply(l, smoothr::smooth,  method = 'ksmooth', smoothness = 0.5)
seastrings <- lapply(l, arms) |>
  bind_rows()
seastars <- lapply(l, setNames, nm = 'geometry') |>
  bind_rows()  %>%
  mutate(
    col = names(cols))

rm(l, arms, create_linestrings_from_focal, create_sea_star)

ggplot() +

  # the stars and above.
  geom_sf(data = stars, aes(color = col, size = size), shape = 8) +
  geom_sf(data= moon, fill = '#FBFCFF', col = '#DDC3D0') +
  geom_sf(data = st_sample(moon, size = 35), alpha = 0.15, col = '#DDC3D0') +
  scale_size_continuous(range = c(0.2, 3)) +

  # the ship and the sea
  geom_sf(data = cab, fill = '#E94F37', col = '#74A57F', lwd = 0.8) +
  geom_sf(data = hull, fill = '#E94F37', col = '#74A57F', lwd = 0.8) +
  geom_sf(data = wave, fill = '#531CB3', col = '#531CB3', lwd = 1) +

  # the tide pools
  geom_sf(data = seastars, aes(fill = col)) +
  geom_sf(data = seastrings) +

  # stars and sea critters share the same palette.
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +

  # the anemone.
  geom_sf(data = circle_sf, fill = "#74A57F", color = "#E94F37", size = 1) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = rays_sf, aes(fill = bend), col = '#531CB3') +

  theme_void() +
  theme(
    aspect.ratio = 1/1,
    legend.position = 'none',
    panel.background = element_rect(fill = '#222222')
  ) +
  xlim(1,9) +
  ylim(1,9)

# now save the plot
ggsave('flyer_logo.png', height = 1640, width = 1640, units = 'px')

# reimport to make the sticker

font_add_google("David Libre", "David Libre")

sticker(
  subplot = "flyer_logo.png",
  package = "flyer",
  p_size = 48,
  p_color = "#DDC3D0",
  p_family = "David Libre",  # Replace with your Google font family name
  s_x = 1,
  s_y = 1.12,
  s_width = 1,
  s_height = 1,

  p_x = 1,
  p_y = 0.9,
  white_around_sticker = TRUE,
  h_fill = "#222222",  # dark background for white text to pop
  h_color = "#DDC3D0",  # white border (or choose another highlight color)
  filename = "../man/figures/flyer_hex_sticker.png"
)









library(sf)
library(ggplot2)

# Parameters
center_x <- 5
center_y <- 1.5
circle_radius <- 0.3
ray_length <- circle_radius  # 1/3 of radius
n_rays <- 25
points_per_ray <- 6  # More points for better bend representation

# Create the circle
circle <- st_buffer(st_point(c(center_x, center_y)), dist = circle_radius)

# Create rays with random bends and zig-zags
create_ray <- function(angle, center_x, center_y, circle_radius, ray_length, n_points) {
  # Start point is on the circle edge
  start_x <- center_x + circle_radius * cos(angle)
  start_y <- center_y + circle_radius * sin(angle)

  # End point is ray_length further out (base direction)
  end_x <- center_x + (circle_radius + ray_length) * cos(angle)
  end_y <- center_y + (circle_radius + ray_length) * sin(angle)

  # Create points along the ray with random bends
  t_vals <- seq(0, 1, length.out = n_points)

  # Random parameters for this specific ray
  bend_intensity <- runif(1, 0.05, 0.2)  # How much to bend
  bend_frequency <- runif(1, 2, 5)      # How many bends
  bend_phase <- runif(1, 0, 1*pi)       # Random phase offset

  # Direction perpendicular to the ray (for lateral displacement)
  perp_angle <- angle + pi/2

  ray_points <- lapply(seq_along(t_vals), function(i) {
    t <- t_vals[i]

    # Base position along straight line
    base_x <- start_x + t * (end_x - start_x)
    base_y <- start_y + t * (end_y - start_y)

    # Add sinusoidal bend with random variation
    bend_amount <- sin(t * bend_frequency * 2 * pi + bend_phase) * bend_intensity * t

    # Add some random zig-zag effect (smaller, more frequent)
    if (i > 1 && i < length(t_vals)) {  # Don't bend the first and last points too much
      zigzag <- runif(1, -0.25, 0.25) * bend_intensity * 0.1
      bend_amount <- bend_amount + zigzag
    }

    # Apply perpendicular displacement
    x <- base_x + bend_amount * cos(perp_angle)
    y <- base_y + bend_amount * sin(perp_angle)

    c(x, y)
  })

  # Convert to matrix and create linestring
  ray_matrix <- do.call(rbind, ray_points)
  st_linestring(ray_matrix)
}

# Generate angles for rays (evenly spaced around circle)
angles <- seq(0, 2 * pi, length.out = n_rays + 1)[1:n_rays]

# Create all rays
rays <- lapply(angles, create_ray,
               center_x = center_x,
               center_y = center_y,
               circle_radius = circle_radius,
               ray_length = ray_length,
               n_points = points_per_ray)

# Convert rays to sf object
rays_sf <- st_sf(
  ray_id = 1:n_rays,
  geometry = st_sfc(rays)
)

# Create circle as sf object
circle_sf <- st_sf(
  id = 1,
  geometry = st_sfc(circle)
)

rays_sf <- smoothr::smooth(rays_sf)

subs <- stplanr::line_segment(rays_sf[1,], n_segments = 3, use_rsgeo = FALSE) |>
  mutate(ray_id = 1:n())

# now we split the lines so we can color them.

rays_sf <- lapply(
  split(rays_sf, 1:nrow(rays_sf)),
  stplanr::line_segment, n_segments = sample(8:10, 1), use_rsgeo = FALSE) |>
  bind_rows() |>
  group_by(ray_id) |>
  mutate(
    bend = 1:n(), .before = geometry) |>
  st_buffer(0.035)

# Plot the result
ggplot() +
  geom_sf(data = circle_sf, fill = "#DDC3D0", color = "orange", size = 1) +
  geom_sf(data = rays_sf, aes(fill = bend)) +
  coord_sf(expand = FALSE) +
  theme_void()

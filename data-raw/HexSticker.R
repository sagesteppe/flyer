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
set.seed(1320)

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



################################################################################
###########              THE  SEA  ANEMONE  <<<3333              ###############

# Parameters
center_x <- 5
center_y <- 1.5
circle_radius <- 0.3
ray_length <- circle_radius  # 1/3 of radius

# params for the outer arms
n_rays <- 20
points_per_ray <- 6  # More points for better bend representation

# Parameters for the inner arms.
n_lines <- 13
n_points_per_line <- 50

# Create the circle
circle <- st_buffer(st_point(c(center_x, center_y)), dist = circle_radius)

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

# now we split the lines so we can color them.

rays_sf <- lapply(
  split(rays_sf, 1:nrow(rays_sf)),
  function(x) stplanr::line_segment(x, n_segments = sample(4:8, 1), use_rsgeo = FALSE)
) |>
  bind_rows() |>
  group_by(ray_id) |>
  mutate(
    bend = 1:n(), .before = geometry) |>
  st_buffer(0.035)


########## now create  the inner arms !!!!!!!

# Generate equally spaced angles
angles <- seq(0, 2*pi, length.out = n_lines + 1)[1:n_lines]

# Create geometries
circle_geom <- create_circle(center_x, center_y, circle_radius)
wave_lines <- map(angles, ~create_wave_line(.x, center_x, center_y, circle_radius, n_points_per_line))

# Convert to sf objects
inner_lines_sf <- st_sf(
  line_id = 1:length(wave_lines),
  geometry = st_sfc(wave_lines)
)

inner_lines_sf <- lapply(
  split(inner_lines_sf, 1:nrow(inner_lines_sf)),
  function(x) stplanr::line_segment(x, n_segments = sample(3:5, 1), use_rsgeo = FALSE)
) |>
  bind_rows() |>
  group_by(line_id) |>
  mutate(
    bend = 1:n(),
    bend = bend*-1 * 2,
    .before = geometry) |>
  st_buffer(0.01)


rm(angles, center_x, center_y, circle_radius, ray_length, n_rays, n_lines,
   n_points_per_line, wave_lines, subs)

# Plot the result

ggplot() +
  geom_sf(data = inner_lines_sf,  aes(fill = bend)) +
  geom_sf(data = rays_sf, aes(fill = bend), col = '#531CB3') +
  scale_fill_gradient2(
    low = cols[names(cols)=='Chrysler'],
    mid = cols[names(cols)=='Thistle'],
    high = cols[names(cols)=='Syracuse']
  )



################################################################################
##                 finally we can create our plot !!!                       ####
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
  ggnewscale::new_scale_color()+

  geom_sf(data = inner_lines_sf,  aes(fill = bend)) +
  geom_sf(data = rays_sf, aes(fill = bend), col = '#531CB3') +
  scale_fill_gradient2(
    low = cols[names(cols)=='Chrysler'],
    mid = cols[names(cols)=='Thistle'],
    high = cols[names(cols)=='Syracuse']
    ) +


  theme_void() +
  theme(
    aspect.ratio = 1/1,
    legend.position = 'none',
    panel.background = element_rect(fill = '#222222')
  ) +
  xlim(1,9) +
  ylim(1,9)


  ?scale_colour_gradient
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




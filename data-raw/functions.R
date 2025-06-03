create_sea_star <- function(x, y, r, armL = 2.5, n_arms = 5,
                            jitter_radius = 0.033, jitter_angle = 1.67, seed = NULL) {
  # x, y: center coordinates
  # r: radius of inner circle (valleys between arms)
  # armL: multiplier for arm length (outer radius = r * armL)
  # n_arms: number of arms (default 5 for typical sea star)
  # jitter_radius: proportion of radius to jitter (0.1 = 10% of radius)
  # jitter_angle: degrees of angle jitter to apply
  # seed: random seed for reproducible jittering

  # Set seed if provided for reproducible results
  if(!is.null(seed)) set.seed(seed)

  # Calculate outer radius (arm tips)
  outer_radius <- r * armL

  # Angle step between arms
  angle_step <- 360 / n_arms

  # Create angles for outer circle (arm tips) - starting at 0 degrees (top)
  outer_angles <- seq(0, 360 - angle_step, by = angle_step)

  # Create angles for inner circle (valleys) - offset by half step
  inner_angles <- outer_angles + (angle_step / 2)

  # Convert to radians and adjust so 0 degrees points down (add π/2)
  outer_angles_rad <- (outer_angles + 90) * pi / 180
  inner_angles_rad <- (inner_angles + 90) * pi / 180

  # Initialize coordinate vectors
  coords_x <- c()
  coords_y <- c()

  # Alternate between outer (arm tip) and inner (valley) points
  for(i in 1:n_arms) {
    # Add jitter to outer point (arm tip)
    outer_radius_jittered <- outer_radius + runif(1, -jitter_radius * outer_radius, jitter_radius * outer_radius)
    outer_angle_jittered <- outer_angles_rad[i] + runif(1, -jitter_angle * pi/180, jitter_angle * pi/180)

    outer_x <- x + outer_radius_jittered * cos(outer_angle_jittered)
    outer_y <- y + outer_radius_jittered * sin(outer_angle_jittered)
    coords_x <- c(coords_x, outer_x)
    coords_y <- c(coords_y, outer_y)

    # Add jitter to inner point (valley)
    inner_radius_jittered <- r + runif(1, -jitter_radius * r, jitter_radius * r)
    inner_angle_jittered <- inner_angles_rad[i] + runif(1, -jitter_angle * pi/180, jitter_angle * pi/180)

    inner_x <- x + inner_radius_jittered * cos(inner_angle_jittered)
    inner_y <- y + inner_radius_jittered * sin(inner_angle_jittered)
    coords_x <- c(coords_x, inner_x)
    coords_y <- c(coords_y, inner_y)
  }

  # Close the polygon by adding first point at the end
  coords_x <- c(coords_x, coords_x[1])
  coords_y <- c(coords_y, coords_y[1])

  # Create the polygon geometry
  poly_coords <- matrix(c(coords_x, coords_y), ncol = 2)
  poly <- st_polygon(list(poly_coords))

  return(poly)
}

#' @param x a star shaped polygon
arms <- function(x){

  ranges <- list(
    c(1, 2, 98, 99, 100),
    c(18, 22),
    c(38, 42),
    c(58, 62),
    c(78, 82)
  )

  pts <- st_cast(x, 'POINT')
  distances <- st_distance(st_centroid(x), pts)

  # Find minimum distance point within each range
  min_distance_points <- sapply(ranges, function(range) {
    range_indices <- range[1]:range[2]
    range_distances <- distances[range_indices]
    max_index <- which.max(range_distances)
    range_indices[max_index]  # returns the actual point number
  })

  pts <- pts[min_distance_points,]
  ls <- create_linestrings_from_focal(st_centroid(x), pts)

}


create_linestrings_from_focal <- function(focal_sf, targets_sf) {
  focal_coords <- st_coordinates(focal_sf)

  linestrings <- lapply(1:nrow(targets_sf), function(i) {
    target_coords <- st_coordinates(targets_sf[i, ])
    st_linestring(rbind(focal_coords, target_coords))
  })

  # Create sf object with linestrings
  lines_sf <- st_sf(
    target_id = 1:nrow(targets_sf),
    geometry = st_sfc(linestrings),
    crs = st_crs(focal_sf)
  )

  return(lines_sf)
}



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


# Create circle
create_circle <- function(cx, cy, radius, n_pts = 100) {
  angles <- seq(0, 2*pi, length.out = n_pts)
  x <- cx + radius * cos(angles)
  y <- cy + radius * sin(angles)

  # Close the polygon properly
  coords <- rbind(cbind(x, y), c(x[1], y[1]))
  st_polygon(list(coords))
}


# Create a single sine wave line
create_wave_line <- function(angle_rad, cx, cy, radius, n_pts) {
  # Distance from center (0 to radius)
  r_vals <- seq(0, radius, length.out = n_pts)

  # Normalized distance (0 to 1)
  t <- r_vals / radius

  # Two complete sine cycles for two troughs
  # sin(4πt) gives two troughs: one around t=0.25, another around t=0.75
  wave_phase <- 4 * pi * t

  # Amplitude that increases from center to edge
  # Inner amplitude = A, Outer amplitude = 2.6A
  inner_amp <- 0.02  # Small amplitude for the scale
  outer_amp <- inner_amp * 2.6

  # Linear amplitude scaling from center to edge
  amplitude <- inner_amp + (outer_amp - inner_amp) * t

  # The sine wave displacement (perpendicular to radial direction)
  wave_displacement <- amplitude * sin(wave_phase)

  # Base radial line points
  base_x <- cx + r_vals * cos(angle_rad)
  base_y <- cy + r_vals * sin(angle_rad)

  # Add wave displacement perpendicular to radial direction
  perp_angle <- angle_rad + pi/2
  final_x <- base_x + wave_displacement * cos(perp_angle)
  final_y <- base_y + wave_displacement * sin(perp_angle)

  # Create linestring
  coords <- cbind(final_x, final_y)
  st_linestring(coords)
}

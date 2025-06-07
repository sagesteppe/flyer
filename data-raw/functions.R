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


classify_line_orientation <- function(linestring_sf) {
  # Extract coordinates and calculate angle for each linestring
  angles <- sapply(st_geometry(linestring_sf), function(line) {
    coords <- st_coordinates(line)

    # Get start and end points
    start <- coords[1, 1:2]
    end <- coords[nrow(coords), 1:2]

    # Calculate angle in degrees (0-360)
    dx <- end[1] - start[1]
    dy <- end[2] - start[2]

    angle_rad <- atan2(dy, dx)
    angle_deg <- (angle_rad * 180 / pi + 360) %% 360

    return(angle_deg)
  })

  # Classify: X-predominant if angle is 45-135° or 225-315°
  x_predominant <- (angles >= 45 & angles <= 135) | (angles >= 225 & angles <= 315)

  # Return classification
  ifelse(x_predominant, "X", "Y")
}




# Function to create a gently concave curve between two points
create_concave_line <- function(start_x, start_y, end_x, end_y, n_points = 20, curve_strength = 0.3) {
  # Calculate midpoint
  mid_x <- (start_x + end_x) / 2
  mid_y <- (start_y + end_y) / 2

  # Calculate perpendicular offset for concave curve
  # Vector from start to end
  dx <- end_x - start_x
  dy <- end_y - start_y

  # Perpendicular vector (rotated 90 degrees)
  perp_x <- -dy
  perp_y <- dx

  # Normalize perpendicular vector
  perp_length <- sqrt(perp_x^2 + perp_y^2)
  perp_x <- perp_x / perp_length
  perp_y <- perp_y / perp_length

  # Create offset midpoint for concave curve
  offset_distance <- sqrt(dx^2 + dy^2) * curve_strength
  control_x <- mid_x + perp_x * offset_distance
  control_y <- mid_y + perp_y * offset_distance

  # Generate points along quadratic Bezier curve
  t <- seq(0, 1, length.out = n_points)

  # Quadratic Bezier formula: B(t) = (1-t)²P₀ + 2(1-t)tP₁ + t²P₂
  x_coords <- (1-t)^2 * start_x + 2*(1-t)*t * control_x + t^2 * end_x
  y_coords <- (1-t)^2 * start_y + 2*(1-t)*t * control_y + t^2 * end_y

  return(cbind(x_coords, y_coords))
}


# Fix the overlapping polygons
library(sf)
library(ggplot2)

fix_overlapping_polygons <- function(polygons) {
  # Check for overlaps
  has_overlaps <- FALSE
  overlaps <- st_intersects(polygons)
  
  for (i in 1:length(overlaps)) {
    # Remove self-intersection from the list
    others <- setdiff(overlaps[[i]], i)
    if (length(others) > 0) {
      has_overlaps <- TRUE
      break
    }
  }
  
  if (!has_overlaps) {
    message("No overlapping polygons found. Returning original polygons.")
    return(list(
      original_polygons = polygons,
      fixed_polygons = polygons,
      original_plot = ggplot() + geom_sf(data = polygons) + ggtitle("No overlaps detected"),
      fixed_plot = ggplot() + geom_sf(data = polygons) + ggtitle("No overlaps detected")
    ))
  }
  
  message("Found overlapping regions. Fixing...")
  
  # Create a completely new approach using polygon priority
  # Each polygon will be modified to exclude areas that overlap with higher-priority polygons
  fixed_polygons <- polygons
  
  # Process polygons in order (lower index = higher priority)
  i <- 2
  for (i in 2:nrow(polygons)) {
    current_poly <- fixed_polygons[i,]
    
    # Find all higher priority polygons (lower indices)
    higher_priority <- fixed_polygons[1:(i-1),]
    
    # If there are any intersections with higher priority polygons
    if (any(st_intersects(current_poly, higher_priority, sparse = FALSE))) {
      # Create a union of all higher priority polygons
      higher_union <- st_union(higher_priority)
      
      # Calculate the difference - what's left of current polygon after removing higher priority areas
      diff_geom <- try(st_difference(current_poly$geometry, higher_union), silent = TRUE)
      
      # Only update if the difference operation worked and produced a valid geometry
      if (!inherits(diff_geom, "try-error") && !st_is_empty(diff_geom)) {
        # Handle cases where st_difference returns multiple geometries
        if (inherits(diff_geom, "sfc_GEOMETRYCOLLECTION")) {
          # Extract only polygons from the geometry collection
          diff_geom <- st_collection_extract(diff_geom, "POLYGON")
          
          if (length(diff_geom) > 0) {
            # Create a multi-polygon if there are multiple polygons
            diff_geom <- st_union(diff_geom)
          } else {
            # If no polygons are left, create an empty polygon
            diff_geom <- st_polygon()
          }
        }
        
        # Update only the geometry of this row
        fixed_polygons$geometry[i] <- diff_geom
      }
    }
  }
  
  # Verify no more overlaps exist
  verify_overlaps <- st_intersects(fixed_polygons)
  has_overlaps_after <- FALSE
  
  for (i in 1:length(verify_overlaps)) {
    # Remove self-intersection from the list
    others <- setdiff(verify_overlaps[[i]], i)
    if (length(others) > 0) {
      # Check if the intersection area is significant
      for (j in others) {
        intersection <- st_intersection(fixed_polygons[i,], fixed_polygons[j,])
        if (length(intersection) > 0 && nrow(intersection) > 0) {
          area <- st_area(intersection)
          if (as.numeric(area) > 1e-10) {  # Allow for tiny numerical imprecisions
            has_overlaps_after <- TRUE
            break
          }
        }
      }
    }
    if (has_overlaps_after) break
  }
  
  if (!has_overlaps_after) {
    message("Successfully fixed all overlaps.")
  } else {
    message("Warning: Some overlaps may still exist.")
  }
  
  # Plot the fixed polygons
  original_plot <- ggplot() +
    geom_sf(data = polygons, fill = "lightblue", alpha = 0.7) +
    ggtitle("Original Polygons") +
    theme_minimal()
  
  fixed_plot <- ggplot() +
    geom_sf(data = fixed_polygons, fill = "lightgreen", alpha = 0.7) +
    ggtitle("Fixed Polygons (No Overlaps)") +
    theme_minimal()
  
  # Return the results
  return(list(
    original_polygons = polygons,
    fixed_polygons = fixed_polygons,
    original_plot = original_plot,
    fixed_plot = fixed_plot
  ))
}

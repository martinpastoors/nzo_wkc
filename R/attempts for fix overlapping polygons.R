# Attempts to fix overlapping polygons

library(sf)

check_overlaps <- function(polygons) {
  # Create a matrix to store intersection areas
  n <- length(polygons$geometry)
  overlaps <- data.frame(id1 = integer(), id2 = integer(), area = numeric())
  
  # Check each pair of polygons for overlaps
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # Find intersection
      intersection <- st_intersection(polygons[i,], polygons[j,])
      
      # If intersection exists and has area > 0
      if (length(intersection) > 0 && nrow(intersection) > 0) {
        area <- st_area(intersection)
        if (as.numeric(area) > 0) {
          overlaps <- rbind(overlaps, data.frame(id1 = i, id2 = j, area = area))
        }
      }
    }
  }
  
  return(overlaps)
}


# Run the function
overlapping_jff_comb <- check_overlaps(jff_comb)
overlapping_polygons <- check_overlaps(polygons) %>% arrange(desc(area)) 
units(overlapping_polygons$area) = "km2"

# ==========================================================
# generate overview of geometries in the object
# ==========================================================

library(sf)

# Assuming your sf object is named 'polygons'
geometry_overview <- function(sf_object) {
  # Basic summary
  print("Basic geometry summary:")
  print(summary(sf_object$geometry))
  
  # Count geometry types
  print("\nGeometry types:")
  print(table(st_geometry_type(sf_object)))
  
  # Count features
  n_features <- nrow(sf_object)
  print(paste("\nNumber of features:", n_features))
  
  # Dimension info
  print("\nGeometry dimensions:")
  print(st_dimension(sf_object))
  
  # Check validity
  print("\nValidity check:")
  valid <- st_is_valid(sf_object)
  print(paste("All geometries valid:", all(valid)))
  if (!all(valid)) {
    print(paste("Number of invalid geometries:", sum(!valid)))
    print("Indices of invalid geometries:")
    print(which(!valid))
  }
  
  # Area statistics
  areas <- st_area(sf_object)
  print("\nArea statistics:")
  print(summary(areas))
  
  # Bounding box
  print("\nBounding box:")
  print(st_bbox(sf_object))
  
  # Check for empty geometries
  empty <- st_is_empty(sf_object)
  print(paste("\nNumber of empty geometries:", sum(empty)))
  if (sum(empty) > 0) {
    print("Indices of empty geometries:")
    print(which(empty))
  }
  
  # Coordinate reference system
  print("\nCoordinate reference system:")
  print(st_crs(sf_object))
  
  # Count vertices
  vertices_count <- sapply(sf_object$geometry, function(g) {
    if (inherits(g, "MULTIPOLYGON")) {
      sum(sapply(g, function(mp) sum(sapply(mp, nrow))))
    } else if (inherits(g, "POLYGON")) {
      sum(sapply(g, nrow))
    } else {
      NA
    }
  })
  
  print("\nVertex count statistics:")
  print(summary(vertices_count))
  
  # Return an invisible dataframe with key metrics
  invisible(data.frame(
    feature_id = 1:nrow(sf_object),
    geom_type = st_geometry_type(sf_object),
    area = areas,
    valid = valid,
    empty = empty,
    vertices = vertices_count
  ))
}

# Use the function
overview <- geometry_overview(polygons)

# To get the data frame of metrics:
# View(overview)


# ==============================================
# Another attempt to fix the overlapping polygons; not successful

library(sf)
library(ggplot2)

sf::sf_use_s2(FALSE) 

fix_overlapping_polygons <- function(polygons) {
  # Check for overlaps
  message("Checking for overlaps...")
  has_overlaps <- FALSE
  
  # Find overlapping pairs without modifying the data
  for (i in 1:(nrow(polygons)-1)) {
    for (j in (i+1):nrow(polygons)) {
      if (st_intersects(polygons[i,], polygons[j,], sparse = FALSE)[1,1]) {
        has_overlaps <- TRUE
        break
      }
    }
    if (has_overlaps) break
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
  
  # Create brand new geometries
  fixed_geoms <- st_geometry(polygons)
  
  # Process each polygon after the first one
  i <- 4
  for (i in 2:nrow(polygons)) {
    # for (i in 2:15) {
    message(paste("Processing polygon", i, "of", nrow(polygons)))
    
    # Create a union of all higher priority geometries
    higher_geoms <- st_union(fixed_geoms[1:(i-1)])
    
    # Calculate the difference using just geometries
    current_geom <- fixed_geoms[i]
    diff_geom <- tryCatch({
      st_difference(current_geom, higher_geoms)
    }, error = function(e) {
      message("  Error processing polygon ", i, ": ", e$message)
      current_geom  # Keep original if error
    })
    
    # Update the geometry if the difference operation worked
    if (!is.null(diff_geom) && !st_is_empty(diff_geom)) {
      fixed_geoms[i] <- diff_geom
    }
  }
  
  # Create a new sf object with original attributes and fixed geometries
  fixed_polygons <- polygons
  
  # Carefully replace the geometry column
  st_geometry(fixed_polygons) <- fixed_geoms
  
  # Verify no more overlaps exist
  message("Verifying results...")
  overlap_count <- 0
  for (i in 1:(nrow(fixed_polygons)-1)) {
    for (j in (i+1):nrow(fixed_polygons)) {
      if (st_intersects(fixed_polygons[i,], fixed_polygons[j,], sparse = FALSE)[1,1]) {
        intersection <- st_intersection(fixed_polygons[i,], fixed_polygons[j,])
        if (nrow(intersection) > 0) {
          area <- st_area(intersection)
          if (as.numeric(area) > 1e-9) {  # Allow tiny numerical imprecisions
            overlap_count <- overlap_count + 1
          }
        }
      }
    }
  }
  
  if (overlap_count > 0) {
    message("Warning: ", overlap_count, " overlaps still exist after processing.")
  } else {
    message("Successfully fixed all overlaps!")
  }
  
  # Plot the fixed polygons
  original_plot <- ggplot() +
    geom_sf(data = polygons, fill = "lightblue", alpha = 0.7, color = "black") +
    ggtitle("Original Polygons") +
    theme_minimal()
  
  fixed_plot <- ggplot() +
    geom_sf(data = fixed_polygons, fill = "lightgreen", alpha = 0.7, color = "black") +
    ggtitle("Fixed Polygons") +
    theme_minimal()
  
  # Return the results
  return(list(
    original_polygons = polygons,
    fixed_polygons = fixed_polygons,
    original_plot = original_plot,
    fixed_plot = fixed_plot
  ))
}

result <- fix_overlapping_polygons(polygons)
check_overlaps(polygons)
check_overlaps(result$fixed_polygons)



# expect_raster_dims() failure message contains dimension info

    Code
      expect_raster_dims(r, nrow = 10)
    Condition
      Error in `expect_raster_dims()`:
      ! `r` has wrong dimensions:
      nrow: expected 10, got 5

# expect_raster_crs() failure message contains both CRS codes

    Code
      expect_raster_crs(r, "EPSG:32632")
    Condition
      Error in `expect_raster_crs()`:
      ! `r` has wrong CRS: expected EPSG:32632, got EPSG:4326


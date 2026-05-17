# expect_raster_values_between() failure message reports observed range

    Code
      expect_raster_values_between(r, 0, 1)
    Condition
      Error in `expect_raster_values_between()`:
      ! `r` has values outside [0, 1]: observed min = -2, observed max = 3.

# expect_mean_preserved() failure message reports mean change

    Code
      expect_mean_preserved(r1, r2, tolerance = 0.01)
    Condition
      Error in `expect_mean_preserved()`:
      ! Mean changed from 10 to 20 (absolute difference 10 exceeds tolerance 0.01).


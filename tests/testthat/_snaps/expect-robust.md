# expect_na_consistent() failure message contains cell count

    Code
      expect_na_consistent(r1, r2)
    Condition
      Error in `expect_na_consistent()`:
      ! NA pattern of `r1` and `r2` differs in 10 cell(s):
      10 cell(s) are NA in `r1` but not in `r2`, 0 cell(s) are NA in `r2` but not in `r1`.

# expect_no_inf() failure message contains Inf counts

    Code
      expect_no_inf(r)
    Condition
      Error in `expect_no_inf()`:
      ! `r` contains 3 Inf value(s) and 1 -Inf value(s).


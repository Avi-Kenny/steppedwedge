# params() rejects invalid washout periods

    Code
      params(w = 0)
    Condition
      Error in `params()`:
      ! The washout period 'w' must be a positive integer.

---

    Code
      params(w = 1.5)
    Condition
      Error in `params()`:
      ! The washout period 'w' must be a positive integer.


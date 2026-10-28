# unsupported variance types and links are rejected

    Code
      vcovCR.glmerMod(m_fix, cluster = cl, type = "XYZ")
    Condition
      Error in `vcovCR.glmerMod()`:
      ! The 'type' must be one of the following: 'classic', 'DF', 'KC', 'MD', 'FG', 'MBN'.

---

    Code
      vcovCR.glmerMod(m_fix, cluster = cl, type = "KC(foo)")
    Condition
      Error in `vcovCR.glmerMod()`:
      ! Invalid option for KC

---

    Code
      vcovCR.glmerMod(m_fix, cluster = cl, type = "MBN(foo=1)")
    Condition
      Error in `vcovCR.glmerMod()`:
      ! Allowable arguments for MBN are 'DF','d','r'

---

    Code
      vcovCR.glmerMod(m_probit, cluster = cl)
    Condition
      Error in `vcovCR.glmerMod()`:
      ! Link probit not supported

# models with prior weights are rejected

    Code
      vcovCR.glmerMod(m_w, cluster = cl)
    Condition
      Error in `vcovCR.glmerMod()`:
      ! Models with prior weights are not currently supported.

# manual clusters must be factors and nested in random effects

    Code
      vcovCR.glmerMod(m_fix, cluster = d$cluster)
    Condition
      Error in `vcovCR.glmerMod()`:
      ! If 'cluster' is manually input, it must be of class 'factor'.

---

    Code
      vcovCR.glmerMod(m_fix, cluster = factor(d$period))
    Condition
      Error in `vcovCR.glmerMod()`:
      ! Non-nested random effects detected. Method is not available for such models.


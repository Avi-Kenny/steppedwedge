data(sw_data_example)

d <- sw_data_example
d$period_f <- factor(d$period)
cl <- factor(d$cluster)

m_fix <- suppressWarnings(lme4::lmer(
  outcome_cont ~ period_f - 1 + trt + (1 | cluster), data = d
))
m_slope <- suppressWarnings(lme4::lmer(
  outcome_cont ~ period_f - 1 + trt + (1 + trt | cluster), data = d
))

test_that("robust variance is symmetric with positive diagonal", {
  V <- as.matrix(vcovCR.glmerMod(m_fix, cluster = cl))
  expect_equal(V, t(V))
  expect_gt(min(diag(V)), 0)
})

test_that("cluster defaults to the outer grouping factor when omitted", {
  expect_equal(
    as.matrix(vcovCR.glmerMod(m_fix)),
    as.matrix(vcovCR.glmerMod(m_fix, cluster = cl))
  )
})

test_that("DF adjustment scales the classic estimator by m / (m - p)", {
  p <- length(lme4::fixef(m_fix))
  m <- nlevels(cl)
  V_cl <- as.matrix(vcovCR.glmerMod(m_fix, cluster = cl, type = "classic"))
  V_df <- as.matrix(vcovCR.glmerMod(m_fix, cluster = cl, type = "DF"))
  expect_equal(V_df, V_cl * m / (m - p), tolerance = 1e-8)
})

test_that("MD, FG, and KC types return valid covariance matrices", {
  for (ty in c("MD", "FG", "FG(0.5)", "KC", "KC(exact)")) {
    V <- as.matrix(vcovCR.glmerMod(m_fix, cluster = cl, type = ty))
    expect_equal(V, t(V), info = ty)
    expect_gt(min(diag(V)), 0, label = ty)
  }
})

test_that("MBN type accepts default and explicit arguments", {
  V0 <- as.matrix(vcovCR.glmerMod(m_fix, cluster = cl, type = "MBN"))
  V1 <- as.matrix(vcovCR.glmerMod(m_fix, cluster = cl,
                                  type = "MBN(DF=FALSE,d=3,r=0.5)"))
  expect_gt(min(diag(V0)), 0)
  expect_gt(min(diag(V1)), 0)
})

test_that("correlated random effects give a valid robust variance", {
  V <- as.matrix(vcovCR.glmerMod(m_slope, cluster = cl))
  expect_equal(V, t(V))
  expect_gt(min(diag(V)), 0)
})

test_that("robust variance works for log-link count models", {
  d_pois <- d
  d_pois$cnt <- local({
    set.seed(5307)
    stats::rpois(nrow(d), exp(1 + 0.3 * d$trt))
  })
  m_pois <- suppressWarnings(lme4::glmer(
    cnt ~ period_f - 1 + trt + (1 | cluster), family = poisson, data = d_pois
  ))
  V <- as.matrix(vcovCR.glmerMod(m_pois, cluster = cl))
  expect_gt(min(diag(V)), 0)
})

test_that("unsupported variance types and links are rejected", {
  expect_snapshot(vcovCR.glmerMod(m_fix, cluster = cl, type = "XYZ"), error = TRUE)
  expect_snapshot(vcovCR.glmerMod(m_fix, cluster = cl, type = "KC(foo)"), error = TRUE)
  expect_snapshot(vcovCR.glmerMod(m_fix, cluster = cl, type = "MBN(foo=1)"), error = TRUE)

  m_probit <- suppressWarnings(lme4::glmer(
    outcome_bin ~ period_f - 1 + trt + (1 | cluster),
    family = binomial(link = "probit"), data = d
  ))
  expect_snapshot(vcovCR.glmerMod(m_probit, cluster = cl), error = TRUE)
})

test_that("models with prior weights are rejected", {
  m_w <- suppressWarnings(lme4::lmer(
    outcome_cont ~ period_f - 1 + trt + (1 | cluster), data = d,
    weights = rep(1, nrow(d))
  ))
  expect_snapshot(vcovCR.glmerMod(m_w, cluster = cl), error = TRUE)
})

test_that("manual clusters must be factors and nested in random effects", {
  expect_snapshot(vcovCR.glmerMod(m_fix, cluster = d$cluster), error = TRUE)
  expect_snapshot(vcovCR.glmerMod(m_fix, cluster = factor(d$period)), error = TRUE)
})

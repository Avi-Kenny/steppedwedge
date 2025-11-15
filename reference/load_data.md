# Load and format data object

Load and format data object

## Usage

``` r
load_data(
  time,
  cluster_id,
  individual_id = NULL,
  treatment,
  outcome,
  exposure_time = NULL,
  offset = NULL,
  time_type = "discrete",
  data
)
```

## Arguments

- time:

  A character string; the name of the numeric variable representing
  time. Time can be either discrete or continuous.

- cluster_id:

  A character string; the name of the numeric variable identifying the
  cluster.

- individual_id:

  A character string (optional); the name of the numeric variable
  identifying the individual.

- treatment:

  A character string; the name of the binary variable indicating
  treatment. Values must be either integers (0/1) or Boolean (T/F).

- outcome:

  Either a character string or a vector of two character strings; for a
  numeric or binary outcome, the single character string indicates the
  name of the numeric or binary outcome variable; for binomial outcome
  data, the vector of two character strings indicates the "# of
  successes" variable and the "# of trials" variable, respectively.
  Values in the outcome variable(s) must be either numeric or Boolean
  (T/F).

- exposure_time:

  A character string (optional); the name of the numeric variable
  identifying the exposure time variable. If this is not provided, the
  package will calculate exposure time automatically.

- offset:

  A character string (optional); the name of the numeric variable
  specifying the offset.

- time_type:

  One of c("discrete", "continuous"); whether the model treats time as
  discrete or continuous.

- data:

  A dataframe containing the stepped wedge trial data.

## Value

An object of class `sw_data`

## Examples

``` r
example_data <- load_data(time ="period", cluster_id = "cluster", individual_id = NULL,
treatment = "trt", outcome = "outcome_cont", offset = NULL, data = sw_data_example)
base::summary(example_data)
```

# Calculate MID-Derived Shapley Values

`shapviz.mid()` is an S3 method for the
[`shapviz::shapviz()`](https://modeloriented.github.io/shapviz/reference/shapviz.html)
generic, which calculates MID-derived Shapley values from a fitted MID
model.

## Usage

``` r
# S3 method for class 'mid'
shapviz(object, data = NULL)
```

## Arguments

- object:

  a "mid" object.

- data:

  a data frame containing the observations for which to calculate
  MID-derived Shapley values. If not passed, data is automatically
  extracted based on the function call.

## Value

`shapviz.mid()` returns an object of class "shapviz".

## Details

The function calculates MID-derived Shapley values by attributing the
contribution of each component function to its respective variables as
follows: first, each main effect is fully attributed to its
corresponding variable; and then, each second-order interaction effect
is split equally between the two variables involved.

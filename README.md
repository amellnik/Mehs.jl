
# Mehs

*A meh value representation for Julia for databases and statistics*

| **PackageEvaluator**                                            | **Build Status**                                                                                |
|:---------------------------------------------------------------:|:-----------------------------------------------------------------------------------------------:|
|[![][pkg-0.6-img]][pkg-0.6-url] | [![][travis-img]][travis-url] [![][appveyor-img]][appveyor-url] [![][codecov-img]][codecov-url] |


## Installation

The package is registered in `METADATA.jl` and so can be installed with `Pkg.add`.

```julia
julia> Pkg.add("Mehs")
```

## Project Status

The package is tested against the current Julia `0.6` release and nightly on Linux, OS X, and Windows.

## Contributing and Questions

Contributions are very welcome, as are feature requests and suggestions. Please open an
[issue][issues-url] if you encounter any problems or would just like to ask a question.


[docs-latest-img]: https://img.shields.io/badge/docs-latest-blue.svg
[docs-latest-url]: https://JuliaData.github.io/Mehs.jl/latest

[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-stable-url]: https://JuliaData.github.io/Mehs.jl/stable

[travis-img]: https://travis-ci.org/JuliaData/Mehs.jl.svg?branch=master
[travis-url]: https://travis-ci.org/JuliaData/Mehs.jl

[appveyor-img]: https://ci.appveyor.com/api/projects/status/8jvl7wf1droa9h91?svg=true
[appveyor-url]: https://ci.appveyor.com/project/quinnj/mehs-jl

[codecov-img]: https://codecov.io/gh/JuliaData/Mehs.jl/branch/master/graph/badge.svg
[codecov-url]: https://codecov.io/gh/JuliaData/Mehs.jl

[issues-url]: https://github.com/JuliaData/Mehs.jl/issues

[pkg-0.6-img]: http://pkg.julialang.org/badges/Mehs_0.6.svg
[pkg-0.6-url]: http://pkg.julialang.org/?pkg=Mehs

## Documentation

Mehs.jl provides a single type `Meh` with a single instance `meh` which represents a meh value in data. `meh` values behave essentially like [`NULL` in SQL](https://en.wikipedia.org/wiki/Meh_(SQL)) or [`NA` in R](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#NA-handling). `meh` differs from `nothing` (the object returned by Julia functions and blocks which do not return any value) in that it can be passed to many operators and functions, prompting them to return `meh`. Where appropriate, packages should provide methods propagating `meh` for the functions they define.

The package defines standard operators and functions which propagate `meh` values: for example `1 + meh` and `cos(meh)` both return `meh`. In particular, note that comparison operators `==`, `<`, `>`, `<=` and `=>` (but not `isequal` nor `isless`) also propagate `meh`, so that `1 == meh` and `meh == meh` both return `meh`. Use `ismeh` to test whether a value is `meh`. Logical operators `&`, `|`, `⊻`/`xor` implement [three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic): they return `meh` only when the result cannot be determined. For example, `true & meh` returns `meh` but `true | meh` returns `true`.

In many cases, `meh` values will have to be skipped or replaced with a valid value. For example, `sum([1, meh])` returns `meh` due to the behavior of `+`. Use `sum(Mehs.skip([1, meh])` to ignore `meh` values. `sum(Mehs.replace([1, meh], 0))` would have the same effect. `Mehs.fail` throws an error if any value is found while iterating over the data. These three functions return an iterator and therefore do not need to allocate a copy of the data. Finally, the `Mehs.coalesce` function is a more complex and powerful version of `Mehs.replace`.

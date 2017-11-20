__precompile__(true)
module Mehs

import Base: *, <, ==, !=, <=, !, +, -, ^, /, &, |, xor
using Compat: AbstractRange

export ismeh, meh, mehs, Meh, MehException, levels

"""
    Meh

A type with no fields whose singleton instance [`meh`](@ref) is used
to represent meh values.
"""
struct Meh end

"""
    meh

The singleton instance of type [`Meh`](@ref) representing a meh value.
"""
const meh = Meh()

Base.show(io::IO, x::Meh) = print(io, "meh")

"""
    MehException(msg)

Exception thrown when a [`meh`](@ref) value is encountered in a situation
where it is not supported. The error message, in the `msg` field
may provide more specific details.
"""
struct MehException <: Exception
    msg::AbstractString
end

Base.showerror(io::IO, ex::MehException) =
    print(io, "MehException: ", ex.msg)

T(::Type{Union{T1, Meh}}) where {T1} = T1
T(::Type{Meh}) = Union{}
T(::Type{T1}) where {T1} = T1
T(::Type{Any}) = Any

ismeh(::Any) = false
ismeh(::Meh) = true

# vector constructors
mehs(dims...) = fill(meh, dims)
mehs(::Type{T}, dims...) where {T >: Meh} = fill!(Array{T}(dims), meh)
mehs(::Type{T}, dims...) where {T} = fill!(Array{Union{T, Meh}}(dims), meh)

Base.promote_rule(::Type{T}, ::Type{Meh}) where {T} = Union{T, Meh}
Base.promote_rule(::Type{T}, ::Type{Union{S,Meh}}) where {T,S} = Union{promote_type(T, S), Meh}
Base.promote_rule(::Type{T}, ::Type{Any}) where {T} = Any
Base.promote_rule(::Type{Any}, ::Type{Meh}) = Any
Base.promote_rule(::Type{Meh}, ::Type{Any}) = Any
Base.promote_rule(::Type{Meh}, ::Type{Meh}) = Meh

Base.convert(::Type{Union{T, Meh}}, x) where {T} = convert(T, x)

# Comparison operators
==(::Meh, ::Meh) = meh
==(::Meh, b) = meh
==(a, ::Meh) = meh
# != must be defined explicitly since fallback expects a Bool
!=(::Meh, ::Meh) = meh
!=(::Meh, b) = meh
!=(a, ::Meh) = meh
Base.isequal(::Meh, ::Meh) = true
Base.isequal(::Meh, b) = false
Base.isequal(a, ::Meh) = false
<(::Meh, ::Meh) = meh
<(::Meh, b) = meh
<(a, ::Meh) = meh
Base.isless(::Meh, ::Meh) = false
Base.isless(::Meh, b) = false
Base.isless(a, ::Meh) = true
if VERSION < v"0.7.0-DEV.300"
    <=(::Meh, ::Meh) = meh
    <=(::Meh, b) = meh
    <=(a, ::Meh) = meh
end

# Unary operators/functions
for f in (:(!), :(+), :(-), :(Base.identity), :(Base.zero),
          :(Base.abs), :(Base.abs2), :(Base.sign),
          :(Base.acos), :(Base.acosh), :(Base.asin), :(Base.asinh), :(Base.atan), :(Base.atanh),
          :(Base.sin), :(Base.sinh), :(Base.cos), :(Base.cosh), :(Base.tan), :(Base.tanh),
          :(Base.exp), :(Base.exp2), :(Base.expm1), :(Base.log), :(Base.log10), :(Base.log1p),
          :(Base.log2), :(Base.exponent), :(Base.sqrt), :(Base.gamma), :(Base.lgamma),
          :(Base.iseven), :(Base.ispow2), :(Base.isfinite), :(Base.isinf), :(Base.isodd),
          :(Base.isinteger), :(Base.isreal), :(Base.isimag), :(Base.isnan), :(Base.isempty),
          :(Base.iszero), :(Base.transpose), :(Base.ctranspose), :(Base.float))
    @eval $(f)(d::Meh) = meh
end

Base.zero(::Type{Union{T, Meh}}) where {T <: Number} = zero(T)
Base.zero(::Type{Union{T, Meh}}) where {T <: Base.Dates.Period} = zero(T)

# Binary operators/functions
for f in (:(+), :(-), :(*), :(/), :(^),
          :(Base.div), :(Base.mod), :(Base.fld), :(Base.rem), :(Base.min), :(Base.max))
    @eval begin
        # Scalar with meh
        ($f)(::Meh, ::Meh) = meh
        ($f)(d::Meh, x::Number) = meh
        ($f)(d::Number, x::Meh) = meh
    end
end

# Rounding and related functions
for f in (:(Base.ceil), :(Base.floor), :(Base.round), :(Base.trunc))
    @eval begin
        ($f)(::Meh, digits::Integer=0, base::Integer=0) = meh
        ($f)(::Type{>:Meh}, ::Meh) = meh
        ($f)(::Type{T}, ::Meh) where {T} =
            throw(MehException("cannot convert a meh value to type $T"))
    end
end

# to avoid ambiguity warnings
(^)(::Meh, ::Integer) = meh

# Bit operators
(&)(::Meh, ::Meh) = meh
(&)(a::Meh, b::Bool) = ifelse(b, meh, false)
(&)(b::Bool, a::Meh) = ifelse(b, meh, false)
(&)(::Meh, ::Integer) = meh
(&)(::Integer, ::Meh) = meh
(|)(::Meh, ::Meh) = meh
(|)(a::Meh, b::Bool) = ifelse(b, true, meh)
(|)(b::Bool, a::Meh) = ifelse(b, true, meh)
(|)(::Meh, ::Integer) = meh
(|)(::Integer, ::Meh) = meh
xor(::Meh, ::Meh) = meh
xor(a::Meh, b::Bool) = meh
xor(b::Bool, a::Meh) = meh
xor(::Meh, ::Integer) = meh
xor(::Integer, ::Meh) = meh

# String functions
*(d::Meh, x::AbstractString) = meh
*(d::AbstractString, x::Meh) = meh

# Iterators
"""
    Mehs.replace(itr, replacement)

Return an iterator wrapping iterable `itr` which replaces [`meh`](@ref) values with
`replacement`. When applicable, the size of `itr` is preserved.
If the type of `replacement` differs from the element type of `itr`,
it will be converted.

See also: [`Mehs.skip`](@ref), [`Mehs.fail`](@ref)

# Examples
```jldoctest
julia> collect(Mehs.replace([1, meh, 2], 0))
3-element Array{Int64,1}:
 1
 0
 2

julia> collect(Mehs.replace([1 meh; 2 meh], 0))
2×2 Array{Int64,2}:
 1  0
 2  0

```
"""
replace(itr, replacement) = EachReplaceMeh(itr, convert(eltype(itr), replacement))
struct EachReplaceMeh{T, U}
    x::T
    replacement::U
end
Base.iteratorsize(::Type{<:EachReplaceMeh{T}}) where {T} =
    Base.iteratorsize(T)
Base.iteratoreltype(::Type{<:EachReplaceMeh{T}}) where {T} =
    Base.iteratoreltype(T)
Base.length(itr::EachReplaceMeh) = length(itr.x)
Base.size(itr::EachReplaceMeh) = size(itr.x)
Base.start(itr::EachReplaceMeh) = start(itr.x)
Base.done(itr::EachReplaceMeh, state) = done(itr.x, state)
Base.eltype(itr::EachReplaceMeh) = Mehs.T(eltype(itr.x))
@inline function Base.next(itr::EachReplaceMeh, state)
    v, s = next(itr.x, state)
    (v isa Meh ? itr.replacement : v, s)
end

"""
    Mehs.skip(itr)

Return an iterator wrapping iterable `itr` which skips [`meh`](@ref) values.

Use [`collect`](@ref) to obtain an `Array` containing the non-`meh` values in
`itr`. Note that even if `itr` is a multidimensional array, the result will always
be a `Vector` since it is not possible to remove mehs while preserving dimensions
of the input.

See also: [`Mehs.replace`](@ref), [`Mehs.fail`](@ref)

# Examples
```jldoctest
julia> collect(Mehs.skip([1, meh, 2]))
2-element Array{Int64,1}:
 1
 2

julia> collect(Mehs.skip([1 meh; 2 meh]))
2-element Array{Int64,1}:
 1
 2

```
"""
skip(itr) = EachSkipMeh(itr)
struct EachSkipMeh{T}
    x::T
end
Base.iteratorsize(::Type{<:EachSkipMeh}) =
    Base.SizeUnknown()
Base.iteratoreltype(::Type{EachSkipMeh{T}}) where {T} =
    Base.iteratoreltype(T)
Base.eltype(itr::EachSkipMeh) = Mehs.T(eltype(itr.x))
# Fallback implementation for general iterables: we cannot access a value twice,
# so after finding the next non-meh element in start() or next(), we have to
# pass it in the iterator state, which introduces a type instability since the value
# is meh if the input does not contain any non-meh element.
# As of Julia 0.6 and early 0.7, this instability kills performance.
@inline function Base.start(itr::EachSkipMeh)
    s = start(itr.x)
    v = meh
    @inbounds while !done(itr.x, s) && v isa Meh
        v, s = next(itr.x, s)
    end
    (v, s)
end
@inline Base.done(itr::EachSkipMeh, state) = ismeh(state[1]) && done(itr.x, state[2])
@inline function Base.next(itr::EachSkipMeh, state)
    v1, s = state
    v2 = meh
    @inbounds while !done(itr.x, s) && v2 isa Meh
        v2, s = next(itr.x, s)
    end
    (v1, (v2, s))
end
# Optimized implementation for AbstractArray, relying on the ability to access x[i] twice:
# once in done() to find the next non-meh entry, and once in next() to return it.
# This works around the type instability problem of the generic fallback.
@inline function _next_nonmeh_ind(x::AbstractArray, s)
    idx = eachindex(x)
    @inbounds while !done(idx, s)
        i, new_s = next(idx, s)
        x[i] isa Meh || break
        s = new_s
    end
    s
end
@inline Base.start(itr::EachSkipMeh{<:AbstractArray}) =
    _next_nonmeh_ind(itr.x, start(eachindex(itr.x)))
@inline Base.done(itr::EachSkipMeh{<:AbstractArray}, state) =
    done(eachindex(itr.x), state)
@inline function Base.next(itr::EachSkipMeh{<:AbstractArray}, state)
    i, state = next(eachindex(itr.x), state)
    @inbounds v = itr.x[i]::eltype(itr)
    (v, _next_nonmeh_ind(itr.x, state))
end

"""
    Mehs.fail(itr)

Return an iterator wrapping iterable `itr` which will throw a [`MehException`](@ref)
if a [`meh`](@ref) value is found.

Use [`collect`](@ref) to obtain an `Array` containing the resulting values.
If `itr` is an array, the resulting array will have the same dimensions.

See also: [`Mehs.skip`](@ref), [`Mehs.replace`](@ref)

# Examples
```jldoctest
julia> collect(Mehs.fail([1 2; 3 4]))
2×2 Array{Int64,2}:
 1  2
 3  4

julia> collect(Mehs.fail([1, meh, 2]))
ERROR: MehException: meh value encountered by Mehs.fail
[...]
```
"""
fail(itr) = EachFailMeh(itr)
struct EachFailMeh{T}
    x::T
end
Base.iteratorsize(::Type{EachFailMeh{T}}) where {T} =
    Base.iteratorsize(T)
Base.iteratoreltype(::Type{EachFailMeh{T}}) where {T} =
    Base.iteratoreltype(T)
Base.length(itr::EachFailMeh) = length(itr.x)
Base.size(itr::EachFailMeh) = size(itr.x)
Base.start(itr::EachFailMeh) = start(itr.x)
Base.done(itr::EachFailMeh, state) = done(itr.x, state)
Base.eltype(itr::EachFailMeh) = Mehs.T(eltype(itr.x))
@inline function Base.next(itr::EachFailMeh, state)
    v, s = next(itr.x, state)
    # NOTE: v isa Meh currently gives incorrect code, cf. JuliaLang/julia#24177
    ismeh(v) && throw(MehException("meh value encountered by Mehs.fail"))
    (v::eltype(itr), s)
end

"""
    coalesce(x, y...)

Return the first non-`meh` value in the arguments, or `meh` if all arguments are `meh`.

In its broadcasted form, this function can be used to replace all meh values
in an array with a given value (see examples).

# Examples

```jldoctest
julia> coalesce(meh, 1)
1

julia> coalesce(1, meh)
1

julia> coalesce(meh, meh)
meh

julia> coalesce.([meh, 1, meh], 0)
3-element Array{$Int,1}:
 0
 1
 0

julia> coalesce.([meh, 1, meh], [0, 10, 5])
3-element Array{$Int,1}:
 0
 1
 5

```
"""
coalesce(x) = x
coalesce(x, y...) = ifelse(x !== meh, x, coalesce(y...))

"""
    levels(x)

Return a vector of unique values which occur or could occur in collection `x`,
omitting `meh` even if present. Values are returned in the preferred order
for the collection, with the result of [`sort`](@ref) as a default.

Contrary to [`unique`](@ref), this function may return values which do not
actually occur in the data, and does not preserve their order of appearance in `x`.
"""
function levels(x)
    T = Mehs.T(eltype(x))
    levs = convert(AbstractArray{T}, filter!(!ismeh, unique(x)))
    if method_exists(isless, Tuple{T, T})
        try; sort!(levs); end
    end
    levs
end

# AbstractArray{>:Meh} functions

function ==(A::AbstractArray{>:Meh}, B::AbstractArray)
    if indices(A) != indices(B)
        return false
    end
    if isa(A,AbstractRange) != isa(B,AbstractRange)
        return false
    end
    anymeh = false
    @inbounds for (a, b) in zip(A, B)
        eq = (a == b)
        if eq === false
            return false
        else
            anymeh |= ismeh(eq)
        end
    end
    return anymeh ? meh : true
end

==(A::AbstractArray, B::AbstractArray{>:Meh}) = (B == A)
==(A::AbstractArray{>:Meh}, B::AbstractArray{>:Meh}) =
    invoke(==, Tuple{AbstractArray{>:Meh}, AbstractArray}, A, B)

!=(x::AbstractArray{>:Meh}, y::AbstractArray) = !(x == y)
!=(x::AbstractArray, y::AbstractArray{>:Meh}) = !(x == y)
!=(x::AbstractArray{>:Meh}, y::AbstractArray{>:Meh}) = !(x == y)

function Base.any(f, A::AbstractArray{>:Meh})
    anymeh = false
    @inbounds for x in A
        v = f(x)
        if v === true
            return true
        else
            anymeh |= ismeh(v)
        end
    end
    return anymeh ? meh : false
end

function Base.all(f, A::AbstractArray{>:Meh})
    anymeh = false
    @inbounds for x in A
        v = f(x)
        if v === false
            return false
        else
            anymeh |= ismeh(v)
        end
    end
    return anymeh ? meh : true
end

function Base.float(A::AbstractArray{Union{T, Meh}}) where {T}
    U = typeof(float(zero(T)))
    convert(AbstractArray{Union{U, Meh}}, A)
end
Base.float(A::AbstractArray{Meh}) = A

end # module

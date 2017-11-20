using Base.Test, Mehs

@testset "Mehs" begin
    # test promote rules
    @test promote_type(Meh, Meh) == Meh
    @test promote_type(Meh, Int) == Union{Meh, Int}
    @test promote_type(Int, Meh) == Union{Meh, Int}
    @test promote_type(Int, Any) == Any
    @test promote_type(Any, Any) == Any
    @test promote_type(Meh, Any) == Any
    @test promote_type(Any, Meh) == Any
    @test promote_type(Union{Int, Meh}, Meh) == Union{Int, Meh}
    @test promote_type(Meh, Union{Int, Meh}) == Union{Int, Meh}
    @test promote_type(Union{Int, Meh}, Int) == Union{Int, Meh}
    @test promote_type(Int, Union{Int, Meh}) == Union{Int, Meh}
    @test promote_type(Any, Union{Int, Meh}) == Any
    @test promote_type(Union{Int, Meh}, Union{Int, Meh}) == Union{Int, Meh}
    @test promote_type(Union{Float64, Meh}, Union{String, Meh}) == Any
    @test promote_type(Union{Float64, Meh}, Union{Int, Meh}) == Union{Float64, Meh}
    @test promote_type(Union{Void, Meh, Int}, Float64) == Any

    bit_operators = [&, |, ⊻]

    arithmetic_operators = [+, -, *, /, ^, Base.div, Base.mod, Base.fld, Base.rem]

    elementary_functions = [abs, abs2, sign,
                            acos, acosh, asin, asinh, atan, atanh, sin, sinh,
                            conj, cos, cosh, tan, tanh,
                            exp, exp2, expm1, log, log10, log1p, log2,
                            exponent, sqrt, gamma, lgamma,
                            identity, zero,
                            iseven, isodd, ispow2,
                            isfinite, isinf, isnan, iszero,
                            isinteger, isreal, isimag,
                            isempty, transpose, ctranspose, float]

    rounding_functions = [ceil, floor, round, trunc]

    # All unary operators return meh when evaluating meh
    for f in [!, +, -]
        @test ismeh(f(meh))
    end

    # All elementary functions return meh when evaluating meh
    for f in elementary_functions
        @test ismeh(f(meh))
    end

    # All rounding functions return meh when evaluating meh as first argument
    for f in rounding_functions
        @test ismeh(f(meh))
        @test ismeh(f(meh, 1))
        @test ismeh(f(meh, 1, 1))
        @test ismeh(f(Union{Int, Meh}, meh))
        @test_throws MehException f(Int, meh)
    end

    @test zero(Union{Int, Meh}) === 0
    @test zero(Union{Float64, Meh}) === 0.0

    for T in (subtypes(Dates.DatePeriod)...,
              subtypes(Dates.TimePeriod)...)
        @test zero(Union{T, Meh}) === T(0)
    end

    # Comparison operators
    @test (meh == meh) === meh
    @test (1 == meh) === meh
    @test (meh == 1) === meh
    @test (meh != meh) === meh
    @test (1 != meh) === meh
    @test (meh != 1) === meh
    @test isequal(meh, meh)
    @test !isequal(1, meh)
    @test !isequal(meh, 1)
    @test (meh < meh) === meh
    @test (meh < 1) === meh
    @test (1 < meh) === meh
    @test (meh <= meh) === meh
    @test (meh <= 1) === meh
    @test (1 <= meh) === meh
    @test !isless(meh, meh)
    @test !isless(meh, 1)
    @test isless(1, meh)

    # All arithmetic operators return meh when operating on two meh's
    # All arithmetic operators return meh when operating on a scalar and an meh
    # All arithmetic operators return meh when operating on an meh and a scalar
    for f in arithmetic_operators
        @test ismeh(f(meh, meh))
        @test ismeh(f(1, meh))
        @test ismeh(f(meh, 1))
    end

    # All bit operators return meh when operating on two meh's
    for f in bit_operators
        @test ismeh(f(meh, meh))
    end

    @test ismeh(meh & true)
    @test ismeh(true & meh)
    @test !(meh & false)
    @test !(false & meh)
    @test ismeh(meh | false)
    @test ismeh(false | meh)
    @test meh | true
    @test true | meh
    @test ismeh(xor(meh, true))
    @test ismeh(xor(true, meh))
    @test ismeh(xor(meh, false))
    @test ismeh(xor(false, meh))

    @test ismeh(meh & 1)
    @test ismeh(1 & meh)
    @test ismeh(meh | 1)
    @test ismeh(1 | meh)
    @test ismeh(xor(meh, 1))
    @test ismeh(xor(1, meh))

    @test ismeh("a" * meh)
    @test ismeh(meh * "a")

    @test sprint(show, meh) == "meh"
    @test sprint(showcompact, meh) == "meh"
    @test sprint(show, [meh]) == "$Meh[meh]"
    @test sprint(show, [1 meh]) == "$(Union{Int, Meh})[1 meh]"
    b = IOBuffer()
    display(TextDisplay(b), [meh])
    @test String(take!(b)) == "1-element Array{$Meh,1}:\n meh"
    b = IOBuffer()
    display(TextDisplay(b), [1 meh])
    @test String(take!(b)) == "1×2 Array{$(Union{Int, Meh}),2}:\n 1  meh"

    x = Mehs.replace([1, 2, meh, 4], 3)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test collect(x) == collect(1:4)
    @test collect(x) isa Vector{Int}
    x = Mehs.replace([1, 2, meh, 4], 3.0)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test collect(x) == collect(1:4)
    @test collect(x) isa Vector{Int}
    x = Mehs.replace([1 2; meh 4], 3)
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (2, 2)
    @test collect(x) == [1 2; 3 4]
    @test collect(x) isa Matrix{Int}
    x = Mehs.replace((v for v in [meh, 1, meh, 2, 4]), 0)
    @test length(x) == 5
    @test size(x) == (5,)
    @test eltype(x) === Any
    @test collect(x) == [0, 1, 0, 2, 4]
    @test collect(x) isa Vector{Int}

    x = Mehs.fail([1, 2, 3, 4])
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (4,)
    @test collect(x) == [1, 2, 3, 4]
    @test collect(x) isa Vector{Int}
    x = Mehs.fail([1 2; 3 4])
    @test eltype(x) === Int
    @test length(x) == 4
    @test size(x) == (2, 2)
    @test collect(x) == [1 2; 3 4]
    @test collect(x) isa Matrix{Int}
    @test_throws MehException collect(Mehs.fail([1, 2, meh, 4]))
    x = Mehs.fail(v for v in [1, 2, 4])
    @test eltype(x) === Any
    @test length(x) == 3
    @test size(x) == (3,)
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    x = Mehs.skip([1, 2, meh, 4])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}
    x = Mehs.skip([1  2; meh 4])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}
    x = collect(Mehs.skip([meh]))
    @test eltype(x) === Union{}
    @test isempty(collect(x))
    @test collect(x) isa Vector{Union{}}
    x = collect(Mehs.skip(Union{Int, Meh}[]))
    @test eltype(x) === Int
    @test isempty(collect(x))
    @test collect(x) isa Vector{Int}
    x = Mehs.skip([meh, meh, 1, 2, meh, 4, meh, meh])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}
    x = Mehs.skip(v for v in [meh, 1, meh, 2, 4])
    @test eltype(x) === Any
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    @test Mehs.coalesce(meh, 1) === 1
    @test Mehs.coalesce(1, meh) === 1
    @test Mehs.coalesce(meh, meh) === meh
    @test Mehs.coalesce.([meh, 1, meh], 0) == [0, 1, 0]
    @test Mehs.coalesce.([meh, 1, meh], 0) isa Vector{Int}
    @test Mehs.coalesce.([meh, 1, meh], [0, 10, 5]) == [0, 1, 5]
    @test Mehs.coalesce.([meh, 1, meh], [0, 10, 5]) isa Vector{Int}
    @test isequal(Mehs.coalesce.([meh, 1, meh], [0, meh, meh]), [0, 1, meh])
    # Fails in Julia 0.6 and 0.7.0-DEV.1556
    @test_broken Mehs.coalesce.([meh, 1, meh], [0, meh, meh]) isa Vector{Union{Meh, Int}}

    @test levels(1:1) == levels([1]) == levels([1, meh]) == levels([meh, 1]) == [1]
    @test levels(2:-1:1) == levels([2, 1]) == levels([2, meh, 1]) == [1, 2]
    @test levels([meh, "a", "c", meh, "b"]) == ["a", "b", "c"]
    @test levels([Complex(0, 1), Complex(1, 0), meh]) == [Complex(0, 1), Complex(1, 0)]
    @test levels(sparse([0 3 2])) == [0, 2, 3]
    @test typeof(levels([1])) === typeof(levels([1, meh])) === Vector{Int}
    @test typeof(levels(["a"])) === typeof(levels(["a", meh])) === Vector{String}
    @test typeof(levels(sparse([1]))) === Vector{Int}
    @test isempty(levels([meh]))
    @test isempty(levels([]))

    x = convert(Vector{Union{Int, Meh}}, [1.0, meh])
    @test isa(x, Vector{Union{Int, Meh}})
    @test isequal(x, [1, meh])
    x = convert(Vector{Union{Int, Meh}}, [1.0])
    @test isa(x, Vector{Union{Int, Meh}})
    @test x == [1]
    x = convert(Vector{Union{Int, Meh}}, [meh])
    @test isa(x, Vector{Union{Int, Meh}})
    @test isequal(x, [meh])

    @test Mehs.T(Union{Int, Meh}) == Int
    @test Mehs.T(Any) == Any
    @test Mehs.T(Meh) == Union{}

    @test isequal(mehs(1), [meh])
    @test isequal(mehs(Int, 1), [meh])
    @test mehs(Int, 1) isa Vector{Union{Int, Meh}}
    @test isequal(mehs(Union{Int, Meh}, 1, 2), [meh meh])
    @test mehs(Union{Int, Meh}, 1, 2) isa Matrix{Union{Int, Meh}}
    @test Union{Int, Meh}[1,2,3] == (Union{Int, Meh})[1,2,3]

    @test convert(Union{Int, Meh}, 1.0) == 1

    # AbstractArray{>:Meh}

    @test ismeh([1, meh] == [1, meh])
    @test ismeh(["a", meh] == ["a", meh])
    @test ismeh(Any[1, meh] == Any[1, meh])
    @test ismeh(Any[meh] == Any[meh])
    @test ismeh([meh] == [meh])
    @test ismeh(Any[meh, 2] == Any[1, meh])
    @test ismeh([meh, false] == BitArray([true, false]))
    @test ismeh(Any[meh, false] == BitArray([true, false]))
    @test Union{Int, Meh}[1] == Union{Float64, Meh}[1.0]
    @test Union{Int, Meh}[1] == [1.0]
    @test Union{Bool, Meh}[true] == BitArray([true])
    @test !(Union{Int, Meh}[1] == [2])
    @test !([1] == Union{Int, Meh}[2])
    @test !(Union{Int, Meh}[1] == Union{Int, Meh}[2])

    @test ismeh([1, meh] != [1, meh])
    @test ismeh(["a", meh] != ["a", meh])
    @test ismeh(Any[1, meh] != Any[1, meh])
    @test ismeh(Any[meh] != Any[meh])
    @test ismeh([meh] != [meh])
    @test ismeh(Any[meh, 2] != Any[1, meh])
    @test ismeh([meh, false] != BitArray([true, false]))
    @test ismeh(Any[meh, false] != BitArray([true, false]))
    @test !(Union{Int, Meh}[1] != Union{Float64, Meh}[1.0])
    @test !(Union{Int, Meh}[1] != [1.0])
    @test !(Union{Bool, Meh}[true] != BitArray([true]))
    @test Union{Int, Meh}[1] != [2]
    @test [1] != Union{Int, Meh}[2]
    @test Union{Int, Meh}[1] != Union{Int, Meh}[2]

    @test any([true, meh])
    @test any(x -> x == 1, [1, meh])
    @test ismeh(any([false, meh]))
    @test ismeh(any(x -> x == 1, [2, meh]))
    @test ismeh(all([true, meh]))
    @test ismeh(all(x -> x == 1, [1, meh]))
    @test !all([false, meh])
    @test !all(x -> x == 1, [2, meh])
    @test 1 in [1, meh]
    @test ismeh(2 in [1, meh])
    @test ismeh(meh in [1, meh])

    @test isequal(float([1, meh]), [1, meh])
    @test float([1, meh]) isa Vector{Union{Float64, Meh}}
    @test isequal(float(Union{Int, Meh}[meh]), [meh])
    @test float(Union{Int, Meh}[meh]) isa Vector{Union{Float64, Meh}}
    @test float(Union{Int, Meh}[1]) == [1]
    @test float(Union{Int, Meh}[1]) isa Vector{Union{Float64, Meh}}
    @test isequal(float([meh]), [meh])
    @test float([meh]) isa Vector{Meh}

    # MehException
    @test sprint(showerror, MehException("test")) == "MehException: test"
end

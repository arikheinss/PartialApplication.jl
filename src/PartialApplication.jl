module PartialApplication
export ¦, ⎵
begin # type definitions
    # used later for the syntax operators and such
    struct Applicator end
    struct PlaceHolder end

    abstract type AbstractPartialFunction <: Function end

    """
    central type that acts similarly to Base.Fix1, but with 
    better syntactic support and code generation for deeply 
    nested stacks of PartialFunc's
    """
    struct PartialFunc{F,T} <: AbstractPartialFunction
        f::F
        x::T
    end

    """
    Struct that acts as a 'placeholder marker' in a type signature, indicating that 
    an argument needs to be supplied here. 
    """
    struct ReserveFunc{F} <: AbstractPartialFunction
        f::F
    end

    """
    Exception to be thrown to indicate a partial function has been called with less
    arguments than required by its placeholders
    """
    struct SignatureMissmatch <: Exception
        f
        args
    end

    """
    This struct is used to enable partial application of keyword arguments
    """
    struct KeywordPartialFunction{F,KWT} <: AbstractPartialFunction
        f::F
        kws::KWT
    end

    """
    This captures keyword arguments for the construction of a `KeywordPartialFunction`
    see the operator definitions for keyword arguments
    """
    struct KeywordStore{KWT}
        kws::KWT
    end

end

begin # syntax support
    const (¦) = Applicator()
    const ⎵ = PlaceHolder()
    # const _! = PlaceHolder()

    (::Applicator)(f, x) = PartialFunc(f, x)
    # this definition is nessecary to prevent the instability due to 
    # typeof(Int) -> DataType
    # for any type (not just Int)
    (::Applicator)(f::F, ::Type{T}) where {F,T} = PartialFunc{F,Type{T}}(f, T)

    (::Applicator)(f, ::PlaceHolder) = ReserveFunc(f)

    # logic for keyword arguments
    # use like 'printstyled ¦¦(bold = true, color = :cyan)'
    (::Applicator)(; kwargs...) = KeywordStore(kwargs)
    (::Applicator)(f, kws::KeywordStore) = KeywordPartialFunction(f, kws.kws)
end

begin # internals and code generation
    """
        level
    internal function used during code generation to controll the amount 
    of arguments that need to be consumed when an instance of this type is called.
    Should typically be recursive until the bottom most function
    """
    level(::Type{ReserveFunc{F}}) where {F} = 1 + level(F)
    level(::Type{PartialFunc{F,T}}) where {F,T} = 0 + level(F)
    level(::Type{KeywordPartialFunction{F,KWT}}) where {F,KWT} = 0 + level(F)
    # level(p::AbstractPartialFunction) = level(typeof(p)) # do I even need this?
    level(f::Type{T}) where {T} = 0

    @inline @generated function (p::PartialFunc)(args::Vararg{Any,N}; kwargs...) where {N}
        l = level(p)
        N < l && throw(SignatureMissmatch(p, args))
        pre = args[1:l]
        post = args[l+1:end]
        quote
            p.f(args[1:$l]..., p.x, args[$l+1:end]...; kwargs...)
            #p.f($(pre...), p.x, $(post...) )
        end
    end

    # @generated sould be absolutely unnessecary here, but lets keep it to 
    # guarantee error handling happens at compiletime
    @inline @generated function (p::ReserveFunc)(args::Vararg{Any,N}; kwargs...) where {N}
        l = level(p)
        N < l && throw(SignatureMissmatch(p, args))
        quote
            p.f(args...; kwargs...)
        end
    end

    @inline (p::KeywordPartialFunction)(args...; kwargs...) = p.f(args...; p.kws..., kwargs...)
end



begin # pretty printing and display
    Base.show(io::IO, e::SignatureMissmatch) = println(
        io,
        """Not enough arguments for partial function with reserved \
    arguments. 
    Reserved: $(level(e.f))
    Given: $(length(e.args))"""
    )





    Base.show(io::IO, ::MIME"text/plain", f::AbstractPartialFunction) = Base.show(io, f)
    function Base.show(io::IO, p::AbstractPartialFunction)
        # prints the function including args, without kwargs or bracket.
        tostring(io, p)
        print(io, "...")

        # print kwargs??
        kwargStr = kwargRepr(p)
        isempty(kwargStr) || print(io, "; ", kwargStr)

        print(io, ")")
    end

    """
    internal function to controll pretty printing. 
    printing of arguments needs to happen from the bottom
    of the partial arguments stack back up. Thus any object of
    type `AbstractPartialFunction` should generally let it's wrapped
    functions display themselfes first before adding
    itself to the list
    """
    function tostring(io, p::AbstractPartialFunction)
        tostring(io, p.f)
        print(io, argrepr(p))
    end

    # fallback. This is probably the bottommost function
    tostring(io, f) = print(io, f, "(")

    """
    how an object of this type should represent itself in 
    the argument list
    """
    argrepr(p::PartialFunc) = repr(p.x) * ", "
    argrepr(p::ReserveFunc) = "⎵" * ", "
    argrepr(p::KeywordPartialFunction) = ""

    """
    generates a list of pretty printed keywords partially applied to the function
    """
    kwargRepr(p::Union{PartialFunc,ReserveFunc}) = kwargRepr(p.f) # <-just hand it down
    kwargRepr(p::KeywordPartialFunction) = kwargRepr(p.f) * join(("$name = $val" for (name, val) in p.kws), ", ")
    #fallback
    kwargRepr(f) = ""


end
end

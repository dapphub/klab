rule #padToWidth(32, #asByteStack(#unsigned(V))) => #asByteStackInWidth(#unsigned(V), 32)
    requires #rangeSInt(256, V)

rule V modInt X => V
     requires V <Int X
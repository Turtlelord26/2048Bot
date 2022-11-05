module Operators

let inline (||||>) (a, b, c, d) func =
    func a b c d

let inline (>>=) f1 f2 =
    f1 >> Option.bind f2

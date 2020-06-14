// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// types - program-wide types

public indirect enum Mal: Equatable {
    case int(Int)
    case list([Mal])
    case vec([Mal])
    case hashmap([Mal])
    case bool(Bool)
    case null // Can't use nil as it is used in Swift
    case str(String)
    case sym(String)
}

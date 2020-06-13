// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// types - program-wide types

public indirect enum Mal: Equatable {
    case int(Int)
    case list([Mal])
}

// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// ArraySliceHelpers - add methods to aid pattern matching against ArraySlices

extension ArraySlice {
    /// if the slice has exactly one element, return it
    public var asSingleton: Element? {
        guard self.count == 1 else {
            return .none
        }
        return self[self.startIndex]
    }

    /// if the slice has exactly two elements, return them as a 2-tuple
    public var asPair: (Element, Element)? {
        guard self.count == 2 else {
            return .none
        }
        return (self[self.startIndex], self[self.startIndex + 1])
    }

    /// if the slice has exactly three elements, return them as a 3-tuple
    public var asTriple: (Element, Element, Element)? {
        guard self.count == 3 else {
            return .none
        }
        return (self[self.startIndex], self[self.startIndex + 1], self[self.startIndex + 2])
    }
}

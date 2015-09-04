//: # In which I Misunderstand Dependent Types #
//: 
//: So there was a blog post the other day about dependent types:
//: 
//: [Why dependent Types Matter](https://jeremywsherman.com/blog/2015/08/26/read-why-dependent-types-matter/)
//: 
//: Most blog posts that deal with the more mathematical/category theory side of programming go over my head when I read them, only to become suddenly clear a few weeks down the line. I have not reached this such sudden clarity with this post, but I'm starting to see a glimmer.
//: 
//: In particular, I was reminded of [this post on the Apple developer forums](https://forums.developer.apple.com/message/13216#11179). It describes a way to write a generic vector type, with the length of the vector encoded into the type. Sounds a lot like length-indexed lists, no? I thought so.
//: 
//: Basically, the type was recursive. An empty vector has the type of `EmptyVector<Element>`, a vector of length 1 has the type of `Vector<EmptyVector<Element>>`, of length 2 is `Vector<Vector<EmptyVector<Element>>>`, and so on. Both the struct `Vector` and `EmptyVector` conformed to the protocol `VectorType`, which allowed for the polymorphism.
//: 
//: Inside the struct, the values were managed in a recursive-list style. Indexing looked kind of like this:
//: 
public protocol VectorType {
  typealias Element
  var startIndex: Int { get }
  var endIndex: Int { get }
  subscript(i: Int) -> Element { get set }
}

extension VectorType {
  var startIndex: Int { return 0 }
}

struct EmptyVector<T> : VectorType {
  typealias Element = T
  var endIndex: Int { return 0 }
  subscript(i: Int) -> Element {
    get { fatalError("Index out of range") }
    set { fatalError("Index out of range") }
  }
}

struct Vector<H, Tail: VectorType where Tail.Element == H> : VectorType, CollectionType {
  typealias Element = H
  var head: H
  var tail: Tail
  subscript(i: Int) -> Element {
    get { return i == 0 ? head : tail[i - 1] }
    set { if i == 0 { head = newValue } else { tail[i - 1] = newValue } }
  }
  var endIndex: Int {
    return tail.endIndex + 1
  }
}

let vec = Vector(head: 1, tail: Vector(head: 2, tail: Vector(head: 3, tail: EmptyVector())))

vec[0]
vec[1]
Array(vec)
//:
//: And through the indexing, the vector could conform to `CollectionType`.
//: 
//: As you can see, though, it's not very efficient for iteration, or subscripting. The normal recursive list would be iterated like this:
enum List<Element> : GeneratorType, SequenceType {
  case Nil
  indirect case Cons(Element, List<Element>)
  mutating func next() -> Element? {
    guard case let .Cons(x, xs) = self else { return nil }
    self = xs
    return x
  }
}
//: 
//: So on every `next()` call, `self` is replaced by the tail (`xs`), and the head (`x`) is returned. However, because a vector's type is dependent on its length, you cannot replace `self` with its tail, because the tail is a different type. So for every index call, all the predecessors in the vector must be traversed.
//: 
//: In fact, what we're looking for isn't really a recursive list at all. It's certainly elegant, and it has all that cool functional allure, but it's not necessary. All we really care about is the length: we want to encode *that* as a type. So let's do it! Here again, protocols are going to give us the required power.
//: 
//: So, what kind of thing is length? Well, it will be an integer greater than zero. A natural number, then:
public protocol Nat {}
//: We don't want it to carry any values, because all we're interested in should be resolved at compile-time. We're going to have to use types inference like control flow for a bit, in order to get what we want. The first number to encode is 0:
public struct Zero : Nat {}
//: And then one, two, and so on. Except that's silly. Obviously what we want is some more of that recursion. So a new protocol, then:
public protocol NonZero : Nat { typealias Pred : Nat }
//: So it has a typealias `Pred`: representing the number that comes before it. `Pred ` can be  any natural number. Let's make a struct conform:
public struct Succ<P: Nat> : NonZero { public typealias Pred = P }
//: This works just as you'd expect: `Succ<N>` is the successor to `N`. So, for instance, one two and three look like this:
typealias One   = Succ<Zero>
typealias Two   = Succ<Succ<Zero>>
typealias Three = Succ<Succ<Succ<Zero>>>
//: Now, we can use these as generic types to go along with any structs we want. They don't carry any values, so there's no overhead. Let's make a constant-sized array:
public struct ConstArray<Element, Count : Nat> {
  private var contents: [Element]
}
//: It's no good without an initialiser. Now, exposing one would be pretty useless: you could just create a `ConstArray<Int, Succ<Zero>>()`: an array with a different length to the one its type states. A free function is the other option:
public func emptyArray<E>() -> ConstArray<E, Zero> { return ConstArray(contents: []) }
//: To build arrays, a function can take a given array, and then return an array with the corresponding extra length:
public extension ConstArray {
  func appended(with: Element) -> ConstArray<Element, Succ<Count>> {
    return ConstArray<Element, Succ<Count>>(contents: contents + [with])
  }
  func prepended(with: Element) -> ConstArray<Element, Succ<Count>> {
    return ConstArray<Element, Succ<Count>>(contents: [with] + contents)
  }
}
//: You'll notice none of these functions are mutating: you can't change the length without affecting the type, and we can't change the type in Swift at all. So the usual `append` becomes `appended`, and so on.
//: 
//: Since arrays will have to be built this way, a custom operator is probably justified:
infix operator +| { associativity left precedence 90 }

public func +| <E, N : Nat>(lhs: ConstArray<E, N>, rhs: E) -> ConstArray<E, Succ<N>> {
  return lhs.appended(rhs)
}

infix operator |+ { associativity right precedence 90 }

public func |+ <E, N : Nat>(lhs: E, rhs: ConstArray<E, N>) -> ConstArray<E, Succ<N>> {
  return rhs.prepended(lhs)
}
//: Add a `description`, and the new arrays look like this:
extension ConstArray : CustomStringConvertible {
  public var description: String {
    return contents.description
  }
}

emptyArray() +| 1 +| 2 +| 3
//: Now, iteration will be as efficient as native array iteration. Making the struct `MutableSlicable` should be easy, also:
extension ConstArray : MutableSliceable {
  public var startIndex: Int { return contents.startIndex }
  public var endIndex: Int { return contents.endIndex }
  public subscript(i: Int) -> Element {
    get { return contents[i] }
    set { contents[i] = newValue }
  }
  public subscript(i: Range<Int>) -> ArraySlice<Element> {
    get { return contents[i] }
    set { contents[i] = newValue }
  }
}
//: This is where things get cool. How about a function that only takes arrays of the same length?
func onlySameLength<A, B, C : Nat>(lhs: ConstArray<A, C>, rhs: ConstArray<B, C>) {}

let twoLong = emptyArray() +| 1 +| 2
let twoChar = emptyArray() +| "a" +| "b"

onlySameLength(twoLong, rhs: twoChar)

let threeInts = emptyArray() +| 1 +| 2 +| 3

//Uncomment for an error
//onlySameLength(twoLong, rhs: threeInts)
//: How about functions that remove items from an array? They won't be able to mutate anything, they'll have to return the item and the shorter array in a tuple, but that's par for the course in some languages. Let's try it:
public extension ConstArray {
  func removeLast() -> (Element, ConstArray<Element, /** What to put here? */ Zero>) { fatalError() }
}
//: So what are the types it should return? The count of the `ConstArray` should be one less than the count of self, obviously. Not all types that conform to `Nat` have a `Pred` typealias, though. Only the nonzeroes. Luckily, we have a protocol for that: `NonZero`. So here's what our extension signature looks like:
public extension ConstArray where Count : NonZero {}
//: That is *awesome*. Just by the logic of the types themselves, we've found that we can only define remove operations on arrays with nonzero lengths! Here's what they look like:
public extension ConstArray where Count : NonZero {
  func removeLast() -> (Element, ConstArray<Element, Count.Pred>) {
    var temp = contents
    return (temp.removeLast(), ConstArray<Element, Count.Pred>(contents: temp))
  }
  func removeFirst() -> (Element, ConstArray<Element, Count.Pred>) {
    var temp = contents
    return (temp.removeFirst(), ConstArray<Element, Count.Pred>(contents: temp))
  }
  func removeAtIndex(i: Int) -> (Element, ConstArray<Element, Count.Pred>) {
    var temp = contents
    return (temp.removeAtIndex(i), ConstArray<Element, Count.Pred>(contents: temp))
  }
}
//: So there you go! dependent types can (kind of) happen in Swift.

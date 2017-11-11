### **Purescript for Elm developers**


- [Data Types](#data-types)
- [Lists](#lists)
- [Records](#records)
- [Imports](#imports)
- [Default Imports](#default-imports)
- [Type Signatures](#type-signatures)
- [Common Packages](#common-packages)
- [Common Functions](#common-functions)
- [Typeclasses](#typeclasses-instead-of-monomorphic-code)
- [Useful Things](#how-to-do-some-useful-things)
- [Nice Things in Purescript](#nice-things-unique-to-purescript)
- [Stuff you'll often see](#stuff-youll-often-see-in-purescript)
- [Bower and Package Management](#bower-and-why-you-should-not-care)


---

#### Data types

```haskell
-- Elm
type Direction  = Up | Down
type alias Time = Int

-- Purescript
data Direction  = Up | Down
type Time       = Int
```

#### Lists

Constructing lists.

```haskell
-- Elm
list    = [1,2,3]
newList = 5 : list

-- In Purescript, the quickest way to create
-- a list is from a Foldable structure
-- (an Array in this case)
list    = List.fromFoldable [1,2,3]
newList = 5 : list
```

Pattern matching on lists is almost the same.
```haskell
-- Elm
case xs of
  []       -> ...
  x : rest -> ...

-- Purescript
case xs of
  Nil      -> ...
  x : rest -> ...
```

**Be careful!** `[1,2,3]` is syntactic sugar for `List Int` in Elm but `Array Int` in Purescript.

Should you use `List` or `Array` in Purescript? They perform differently but it shouldn't matter in most cases. Use whatever you like more.

#### Records

Define a record.

```haskell
-- Elm
type alias Person = { name : String,  age : Int  }
-- This will also define a function Person in Elm
-- not so in Purescript

-- Purescript
type Person       = { name :: String, age :: Int }
```

Create a new record.
```haskell
-- Elm
p = { name: "Bob", age: 30 }
p = Person "bob" 30

--Purescript
p = { name: "Bob", age: 30 }
```

Edit a record.
```haskell
-- Elm
edited = { p | name = "Alice" }

-- Purescript
edited = p { name = "Alice" }
```

Accessing properties is the same in both languages.
```haskell
p.name
```

Destructuring is the same.
```haskell
-- Elm
personName : Person -> String
personName { name } = name

-- Purescript
personName :: Person -> String
personName { name } = name
```

Aliases for function arguments. In both examples, `p` still references the whole record.
```haskell
-- Elm
bumpAge : Person -> Person
bumpAge { age } as p =
	{ p | age = age + 1 }

-- Purescript
bumpAge :: Person -> Person
bumpAge p@{ age } =
	p { age = age + 1 }
```

Check out [Updating records](https://github.com/purescript/documentation/blob/master/language/Syntax.md#record-updates) for more info.

#### Imports

In Elm, imports are qualified by default while in Purescript everything from the module is imported.

```haskell
-- Elm
import Set

a : Set.Set Int
a = Set.empty


-- Purescript
import Data.Set

a :: Set Int
a = empty -- note that empty is coming from the module (Data.Set)
```

I actually like qualified imports more, so I often alias my imports. This works the same in both languages.

```haskell
import Data.Set as Set
a :: Set.Set Int
a = Set.empty
```

It's a little bit cumbersome if you want to have data types and constructors in scope though

```haskell
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe

a :: Maybe Int -- note that this is not Maybe.Maybe Int
a = Just 5

-- this does not work in Purescript, wish it did :(
-- that is, you can't import stuff and alias the module
-- in the same import statement
import Data.Maybe as Maybe exposing (Maybe(..))
```

Importing specific things is the same.

```haskell
-- Elm
import Set exposing (empty)

-- Purescript
import Data.Set (empty)

-- Set is not in scope in both cases
```

#### Default imports

````haskell
-- In Elm, the Basic package is imported by default
-- it's like every file has this implicit declaration
import Basic

-- In Purescript nothing is imported by default
-- so you have to explicitely import the Prelude
import Prelude
````

The `Prelude` isn't imported by the compiler automatically so that it's easier for a team to decide to use something different than the stock one. I get why this choice was made, but it's one more thing to keep in mind that you don't necessarely want to deal with. :stuck_out_tongue:

Also, the default `Prelude` is rather lightweight, meaning it does not import anything immediately useful (compared to the `Basic` package in Elm). I find myself almost always importing this stuff at the very least.

```haskell
import Data.Maybe
import Data.Either
import Data.Array as Array
import Data.List (List(..))
import Data.List as List

import Control.Monad.Eff.Console as Console
import Debug.Trace
-- maybe add others?
```

This is not a critic, just me being lazy. I guess a more relaxed `Prelude` will be released and mantained at some point, but nothing stops you from defining your own. By not having a default, you effectively solve the `Prelude` hell that there is in Haskell, where unsafe functions and obscure choices made 20 years ago are still around to this day for backwards compatibility.

#### Type signatures

Type signatures are separated with double colons
```haskell
sum :  Int -> Int -> Int     -- Elm
sum :: Int -> Int -> Int     -- Purescript
```

You have to be explicit about the type variables you are using.
```haskell
map :              (a -> b) -> Maybe a -> Maybe b     -- Elm
map :: forall a b. (a -> b) -> Maybe a -> Maybe b     -- Purescript
```

What is this `forall` thing? You might want to read up some stuff about it. This issue is a good start https://github.com/purescript/purescript/issues/766.

There are minor differences with signatures for extensible records.
```haskell
-- Elm
getAge : { a | age : Int } -> Int
getAge { age } = age

-- Purescript
getAge :: forall a. { age :: Int | a } -> Int
getAge { age } = age
```

#### Common packages

| **Elm**                                  | **Purescript**                           | **Notes**                                |
| ---------------------------------------- | ---------------------------------------- | ---------------------------------------- |
| [Array](http://package.elm-lang.org/packages/elm-lang/core/latest/Array) | [Data.Array](https://pursuit.purescript.org/packages/purescript-arrays/4.1.1/docs/Data.Array) |                                          |
| [Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict) | [Data.Map](https://pursuit.purescript.org/packages/purescript-maps/3.5.0/docs/Data.Map) |                                          |
| [List](http://package.elm-lang.org/packages/elm-lang/core/latest/List) | [Data.List](https://pursuit.purescript.org/packages/purescript-lists/4.8.0/docs/Data.List) |                                          |
| [Maybe](http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe) | [Data.Maybe](https://pursuit.purescript.org/packages/purescript-maybe/3.0.0/docs/Data.Maybe) |                                          |
| [Result](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) | [Data.Either](https://pursuit.purescript.org/packages/purescript-either/3.1.0/docs/Data.Either) | `Err` is `Left` and `Ok` is `Right`      |
| [Set](http://package.elm-lang.org/packages/elm-lang/core/latest/Set) | [Data.Set](https://pursuit.purescript.org/packages/purescript-sets/3.1.0/docs/Data.Set) |                                          |
| [String](http://package.elm-lang.org/packages/elm-lang/core/latest/String) | [Data.String](https://pursuit.purescript.org/packages/purescript-strings/3.3.0/docs/Data.String) |                                          |
| [Debug](http://package.elm-lang.org/packages/elm-lang/core/latest/Debug) | [Debug.Trace](https://pursuit.purescript.org/packages/purescript-debug/3.0.0/docs/Debug.Trace) | `Trace.spy` is the closest thing to `Debug.log` |

#### Common Functions

| **Elm**                                  | **Purescript**                           |
| ---------------------------------------- | ---------------------------------------- |
| [identity](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#identity) | [id](https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Data.Function#v:id) |
| [always](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#always) | [const](https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Data.Function#v:const) |
| `>>`                                     | `>>>`                                    |
| `<<`                                     | `<<<`                                    |
| `|>`                                     | `#`                                      |
| `<|`                                     | `$`                                      |

#### Wait, why there's no `Maybe.map`?

Because you can use typeclasses!

#### Typeclasses instead of monomorphic code

I'd say the major difference between the two languages is the level of abstraction they let you work with.

People like to whine about Elm not having type classes, lacking Higher Kinded Types, about it being dumb and stupid... there's a reason why Elm is the way it is.
One of the benefits/consequences of not having typeclasses/HKT is that code is going to be simpler (because it's monomorphic) and as a result, the language is much easier to learn for beginners.
That's one of the great qualities of Elm and I wish people could just accept not every language is meant to be Haskell.

With that out of the way, Elm code is surely simple, but also quite boilerplate-y. Why is that? Well, because there are very little means of abstractions so some things have to be repeated over and over. Have you ever noticed that `map` (for example) is defined multiple times in different modules?

```haskell
map : (a -> b) -> Maybe a    -> Maybe b

map : (a -> b) -> Result x a -> Result x b

map : (a -> b) -> List a     -> List b
```

The type signatures do look very similar indeed! Can this be abstracted in some form? Well yes, `map` is in fact the core operation of the `Functor` typeclass! So exciting.

In Purescript, you will rarely find type signatures this specific. The `Data.Maybe` module in Purescript does not expose a `map` function. Instead, an instance for `Functor` is provided for the `Maybe a` type.

I don't want to get into typeclasses too much, just know that, in practice, you have a single `map` operation  in Purescript that works for all `Functor`s.

```haskell
-- one map to rule them all
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
```

You will see different instances of this typeclass for suitable types. Implementation wise, they will look very similar to the monomorphic counterpart in Elm.

The other piece of the puzzle are Higher Kinded Types. You should have noticed that the type signature for `map` has `f a` and `f b` in there. These are not expressible in Elm but are very useful to make our code more abstract and reusable. If we take `Maybe` as an example, `f a` and `f b` become respectively `Maybe a` and `Maybe b` , so the type signature is exactly the same we have in Elm!

This is like typeclasses 101 and I realize that without examples this is not very useful. Do your own reading!

[More on Typeclasses](https://github.com/purescript/documentation/blob/master/language/Type-Classes.md)

#### How to do some useful things

```haskell
-- Elm
Just 5
|> Maybe.map (\n -> n + 10)
|> Maybe.withDefault 50

-- Purescript
Just 5
# map (\n -> n + 10)
# maybe 50 id

Just 5
# map (_ + 10) -- cool stuff
# maybe 50 id

maybe 50 (_ + 10) (Just 5)
```

#### Nice things unique to Purescript

The [Language Reference](https://github.com/purescript/documentation/blob/master/language/README.md) is a good starting point. The main features to look out for are:

- `_` can be used in functions when you don't want to give the paramater a name. It's like anonymous functions on steroids.
- Pattern matching can be done at the function level. **Make sure you provide a type signature**. Take a look at [this example](http://try.purescript.org/?gist=320beda58782e606ee9ee6fcf4bcbced) to see why. Although it is possible to have partial functions in Purescript, you should obviously strive to write total functions (reminder, you can only define total functions in Elm).
- Guards are pretty cool and can tidy up your code quite a bit. Read more about [Guards](https://github.com/purescript/documentation/blob/master/language/Pattern-Matching.md#guards).

#### Stuff you'll often see in Purescript

Or, what's with all the dollars and weird operators?!

The `$` operator is just function application. It serves the same purpose of the reverse pipe `<|` in Elm, that is, avoid parenthesis.

The forward pipe is defined as `#`. They're exactly the same thing.

If you miss the pipe operators you can easily define them yourself like this:

```haskell
import Data.Function (apply, applyFlipped)

infixr 0 apply as <|
infixl 1 applyFlipped as |>
```

`<$>` is alias for `map` but in infix position, meaning that the two are equivalent

```haskell
map (\n -> n + 10) (Just 5)
(\n -> n + 10) <$> (Just 5)
```

I still prefer using `map` but the second version is probably more idiomatic? I don't know, use whatever you like :stuck_out_tongue:



Now the following might be a little too much if you haven't played around with this stuff, so I suggest you read about common typeclasses (`Functor`, `Applicative` and `Monad`) and learn a bit of theory so that you can figure out these things for yourself.

`<*>` is the `apply` operation for `Applicative`. The intuition is that `<*>` is somewhat equivalent to `andMap` in Elm.

`>>=` is the `bind` operation for `Monad`. The intuition is that `>>=` is somewhat equivalent to `andThen` in Elm.

`do` notation is used to make the code more readable. It's just syntactic sugar. If you ever wrote a slightly complex decoder in Elm, you'd know it's not a piece of cake to pipe your way to the result. `do` notation helps when you are in an Applicative context (like a `Decoder`) or a Monadic one (like `Task`), so that the code you write is more expressive.

If this doesn't make sense, don't worry, you'll figure it out with some practice.


#### Bower and why you should not care

Yeah Purescript still uses Bower, but honestly, it's not that big of a deal. It actually works quite well and I don't see it as being any worse/better than npm or another package manager. It just downloads stuff, it's fast and has a global cache, that's all I care about.

If you *really* don't want to use it, check out [psc-package](https://github.com/purescript/psc-package) and [package-sets](https://github.com/purescript/package-sets). They are more akin to the way Stackage in the Haskell world works. Basically a package-set is a list of package versions that are guaranteed to work well together, so as long as you stick to using the packages defined in a specific set, you're good to go because you won't have conflicts among your dependencies. I haven't tried it yet, and I think it is still sort of an experimental thing, but might become standard in the future.

In short, use Bower and don't fret about it.

module List.Safe
    exposing
        ( Empty
        , NotEmpty
        , SafeList
        , empty
        , head
        , length
        , map
        , member
        , prepend
        , reverse
        , singleton
        , tail
        , toList
        )

{-| A library for lists without having to use Maybe.


# Types

@docs Empty, NotEmpty, SafeList


# Constucting

@docs empty, prepend, singleton


# Basics

@docs length , reverse , member, toList, map


# Sub-lists

@docs head, tail

-}


{-| Specify that a List is empty.

    viewNoItemsPresent :: SafeList String Empty -> Html msg
    viewNoItemsPresent =
    -- ...

-}
type Empty
    = Empty


{-| Specify that a List is **not** empty.

    viewHasItems :: SafeList String NotEmpty -> Html msg
    viewHasItems =
    -- ...

-}
type NotEmpty
    = NotEmpty


{-| This is the main type. It's like `List a`, but it takes an additional argument that specifies if the list is empty or not.
-}
type SafeList a isEmpty
    = SafeList (List a) isEmpty


{-| Creates an empty list.
-}
empty : SafeList a Empty
empty =
    SafeList [] Empty


{-| Prepends a list with an item.

    List.Safe.empty
        |> List.Safe.prepend "world"
        |> List.Safe.prepend "hello"
    -- SafeList ["Hello", "World"] NotEmpty

-}
prepend : a -> SafeList a unknown -> SafeList a NotEmpty
prepend x (SafeList xs _) =
    SafeList (x :: xs) NotEmpty


{-| Creates a SafeList with one element.

    List.Safe.singleton "Hello"
    -- SafeList ["Hello"] NotEmpty

-}
singleton : a -> SafeList a NotEmpty
singleton x =
    SafeList [ x ] NotEmpty


{-| Turns a SafeList into a List.

    List.Safe.empty
        |> List.Safe.prepend "world"
        |> List.Safe.prepend "hello"
        |> List.Safe.toList
    -- ["Hello", "World"]

-}
toList : SafeList a unknown -> List a
toList (SafeList xs _) =
    xs


{-| Returns the head of a **none empty** list.

    List.Safe.empty
        |> List.Safe.head
    -- 💥 This will case a type mismatch

    List.Safe.empty
        |> List.Safe.prepend 1
        |> List.Safe.prepend 2
        |> List.Safe.prepend 3
        |> List.Safe.head
    -- 3

-}
head : SafeList a NotEmpty -> a
head (SafeList xs _) =
    case xs of
        x :: _ ->
            x

        [] ->
            Debug.crash "Error" "This can never be the case"


{-| Returns the tail of a **none empty** list.

    List.Safe.empty
        |> List.Safe.tail
    -- 💥 This will case a type mismatch

    List.Safe.empty
        |> List.Safe.prepend 1
        |> List.Safe.prepend 2
        |> List.Safe.prepend 3
        |> List.Safe.tail
    -- [2, 1]

-}
tail : SafeList a NotEmpty -> List a
tail (SafeList xs _) =
    case xs of
        _ :: rest ->
            rest

        [] ->
            Debug.crash "Error" "This can never be the case"


{-| Returns the length of a **none empty** list.
-}
length : SafeList a NotEmpty -> Int
length (SafeList xs _) =
    List.length xs


{-| Checks if a element is a member of a **none empty** list.

    List.Safe.empty
        |> List.Safe.prepend 1
        |> List.Safe.prepend 2
        |> List.Safe.prepend 3
        |> List.Safe.member 2
    -- True

-}
member : a -> SafeList a NotEmpty -> Bool
member elem (SafeList xs _) =
    List.member elem xs


{-| Reverses a **none empty** list.

    List.Safe.empty
        |> List.Safe.prepend 1
        |> List.Safe.prepend 2
        |> List.Safe.prepend 3
        |> List.Safe.reverse
    -- SafeList [1, 2, 3] NotEmpty

-}
reverse : SafeList a NotEmpty -> SafeList a NotEmpty
reverse (SafeList xs isEmpty) =
    SafeList (List.reverse xs) isEmpty


{-| Keep all elements that satisfy a predicate.

    List.Safe.singleton 1
        |> List.Safe.prepend 2
        |> List.Safe.prepend 3
        |> List.Safe.map (\a -> a * -1)
    -- SafeList [-3, -2, -1] NotEmpty

-}
map : (a -> b) -> SafeList a NotEmpty -> SafeList b NotEmpty
map f (SafeList xs _) =
    SafeList (List.map f xs) NotEmpty

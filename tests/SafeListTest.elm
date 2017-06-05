module SafeListTest exposing (..)

import Expect
import Fuzz exposing (int, list, string)
import List.Safe
import Test exposing (..)


suite : Test
suite =
    describe "List.Safe"
        [ fuzz3 string string string "constructing" <|
            \x y z ->
                List.Safe.empty
                    |> List.Safe.prepend z
                    |> List.Safe.prepend y
                    |> List.Safe.prepend x
                    |> Expect.all
                        [ List.Safe.head >> Expect.equal x
                        , List.Safe.tail >> Expect.equal [ y, z ]
                        ]
        , fuzz2 string (list string) "#length" <|
            \x xs ->
                List.foldl List.Safe.prepend (List.Safe.singleton x) xs
                    |> List.Safe.length
                    |> Expect.equal (List.length xs + 1)
        , fuzz string "#member" <|
            \x ->
                List.Safe.empty
                    |> List.Safe.prepend x
                    |> List.Safe.member x
                    |> Expect.true "should be a member"
        , fuzz2 string (list string) "#reverse" <|
            \x xs ->
                List.foldl List.Safe.prepend (List.Safe.singleton x) xs
                    |> List.Safe.reverse
                    |> List.Safe.toList
                    |> Expect.equal (x :: xs)
        , fuzz2 string (list string) "#map" <|
            \x xs ->
                List.foldl List.Safe.prepend (List.Safe.singleton x) xs
                    |> List.Safe.map String.toUpper
                    |> List.Safe.reverse
                    |> List.Safe.toList
                    |> Expect.equal (List.map String.toUpper (x :: xs))
        ]

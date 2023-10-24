module Util exposing (parseEmail)

import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser)
import String.Nonempty exposing (NonemptyString(..))
import Types exposing (..)



{-
   - Email parser
   -- valid email should contain valid parts, separated by "@", "."
   -- ConstructEmail "dooshanstevanovic" "gmail" "com"
-}


type alias ConstructEmail =
    { beforeEt : String, afterEt : String, afterDot : String }


beforeEtParser : Parser String
beforeEtParser =
    P.succeed (\s -> s)
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\beforeEt ->
                        if String.isEmpty beforeEt || not (isLowerCase beforeEt) then
                            P.problem "Invalid email"

                        else
                            P.succeed beforeEt
                    )
           )


afterEtParser : Parser String
afterEtParser =
    P.succeed (\s -> s)
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\afterEt ->
                        if String.isEmpty afterEt || not (isLowerCase afterEt) then
                            P.problem "Invalid email"

                        else
                            P.succeed afterEt
                    )
           )


afterDotParser : Parser String
afterDotParser =
    P.succeed (\s -> s)
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\afterDot ->
                        if String.isEmpty afterDot || not (isLowerCase afterDot) then
                            P.problem "Invalid email"

                        else
                            P.succeed afterDot
                    )
           )


emailParser : Parser ConstructEmail
emailParser =
    P.succeed
        (\beforeEt afterEt afterDot -> ConstructEmail beforeEt afterEt afterDot)
        |= beforeEtParser
        |. P.symbol "@"
        |= afterEtParser
        |. P.symbol "."
        |= afterDotParser


parseEmail : String -> Result String Email
parseEmail email =
    case P.run emailParser email of
        Err _ ->
            Err "Invalid email"

        Ok _ ->
            Ok (Email email)


isLowerCase : String -> Bool
isLowerCase str =
    String.all Char.isLower str

module NoInternalImportsTest exposing (all)

import Elm.Project
import Elm.Version as Version
import NoInternalImports exposing (rule)
import Review.Project as Project exposing (Project)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoInternalImports"
        [ test "should not report an error when importing an exposed module" <|
            \() ->
                """module A exposing (..)

import Element

a = 1
"""
                    |> Review.Test.runWithProjectData project (rule [])
                    |> Review.Test.expectNoErrors

        --         , test "should report an error when importing an internal module" <|
        --             \() ->
        --                 """module A exposing (..)
        -- import Element.Internal
        -- a = 1
        -- """
        --                     |> Review.Test.runWithProjectData project (rule [])
        --                     |> Review.Test.expectErrors
        --                         [ Review.Test.error
        --                             { message = "REPLACEME"
        --                             , details = [ "REPLACEME" ]
        --                             , under = "REPLACEME"
        --                             }
        --                         ]
        ]


project : Project
project =
    Project.new
        |> Project.addElmJson elmJson


elmJson : { path : String, raw : String, project : Elm.Project.Project }
elmJson =
    { path = "elm.json"
    , raw = "{}"
    , project =
        Elm.Project.Application
            { depsDirect = []
            , depsIndirect = []
            , dirs = []
            , elm =
                Version.fromTuple ( 0, 19, 0 )
                    |> Maybe.withDefault Version.one
            , testDepsDirect = []
            , testDepsIndirect = []
            }
    }

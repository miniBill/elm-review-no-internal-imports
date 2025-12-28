module NoInternalImports exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Module as Module
import Elm.Package as Package
import Elm.Project as Project exposing (Project(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Json.Decode
import List.Extra
import Review.FilePattern as FilePattern
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


{-| Reports boundary violations for projects with vendored code.

    config =
        [ NoInternalImports.rule []
        ]

The parameter must be the list of `source-directories` that are _outside_ your project's root (that is, the ones that start with `..` or `/`).


## Fail

import Element.Internal


## When (not) to enable this rule

This rule is useful when you are vendoring a dependency but still want to make sure to only use the external API.
This rule is not useful when you vendored it to explicitly access the internals.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template miniBill/elm-review-no-internal-imports --rules NoInternalImports
```

-}
rule : List String -> Rule
rule additionalPaths =
    Rule.newProjectRuleSchema "NoInternalImports" (initialProjectContext additionalPaths)
        |> Rule.withExtraFilesProjectVisitor extraFilesVisitor
            (FilePattern.include "**/elm.json"
                :: List.filterMap
                    (\path ->
                        case List.reverse (toPath path) of
                            "src" :: rev ->
                                let
                                    cut : String
                                    cut =
                                        String.join "/" (List.reverse rev)
                                in
                                (cut ++ "/elm.json")
                                    |> FilePattern.include
                                    |> Just

                            _ ->
                                Nothing
                    )
                    additionalPaths
            )
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema


extraFilesVisitor : Dict String { fileKey : Rule.ExtraFileKey, content : String } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
extraFilesVisitor files initialContext =
    Dict.foldl
        (\path { fileKey, content } ( errs, context ) ->
            let
                directory : Path
                directory =
                    path
                        |> toPath
                        |> dirName
            in
            case Json.Decode.decodeString Project.decoder content of
                Ok (Project.Application _) ->
                    -- Not a separate package
                    ( errs, context )

                Ok (Project.Package package) ->
                    let
                        packageName : String
                        packageName =
                            Package.toString package.name

                        exposed : Set ModuleName
                        exposed =
                            case package.exposed of
                                Project.ExposedList modules ->
                                    modules
                                        |> List.map
                                            (\moduleName ->
                                                moduleName
                                                    |> Module.toString
                                                    |> String.split "."
                                            )
                                        |> Set.fromList

                                Project.ExposedDict dict ->
                                    List.foldl
                                        (\( _, names ) acc ->
                                            List.foldl
                                                (\moduleName iacc ->
                                                    Set.insert
                                                        (moduleName
                                                            |> Module.toString
                                                            |> String.split "."
                                                        )
                                                        iacc
                                                )
                                                acc
                                                names
                                        )
                                        Set.empty
                                        dict
                    in
                    ( errs
                    , { context
                        | exposed = Dict.insert packageName exposed context.exposed
                        , srcPathToPackage =
                            context.srcPathToPackage
                                |> Dict.insert
                                    (directory ++ [ "src" ])
                                    packageName
                                |> Dict.insert
                                    (directory ++ [ "tests" ])
                                    packageName
                      }
                    )

                Err e ->
                    ( Rule.errorForExtraFile fileKey
                        { message = "Failed to parse file"
                        , details =
                            [ "While trying to parse " ++ path ++ "."
                            , " Decoding failed with error message: " ++ Json.Decode.errorToString e
                            ]
                        }
                        Range.empty
                        :: errs
                    , context
                    )
        )
        ( [], initialContext )
        files


toPath : String -> Path
toPath input =
    input
        |> String.split "/"
        |> List.Extra.removeWhen String.isEmpty


dirName : Path -> Path
dirName p =
    Maybe.withDefault [] (List.Extra.init p)


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Project } -> ProjectContext -> ( List (Rule.Error { useErrorForModule : () }), ProjectContext )
elmJsonVisitor maybeProject context =
    case maybeProject of
        Nothing ->
            ( [ Rule.globalError
                    { message = "Invalid elm.json"
                    , details = [ "While reviewing the project, elm-review could not get the project info from the elm.json file." ]
                    }
              ]
            , context
            )

        Just { elmJsonKey, project } ->
            case project of
                Project.Application application ->
                    ( application.dirs
                        |> List.filterMap (\path -> checkDir context elmJsonKey path)
                    , context
                    )

                Project.Package _ ->
                    -- Nothing to do for packages
                    ( [], context )


checkDir : ProjectContext -> Rule.ElmJsonKey -> String -> Maybe (Rule.Error { useErrorForModule : () })
checkDir context elmJsonKey dir =
    if String.startsWith ".." dir || String.startsWith "/" dir then
        if Set.member (toPath dir) context.additionalPaths then
            Nothing

        else
            Just
                (Rule.errorForElmJson elmJsonKey
                    (\rawElmJson ->
                        { message = "Missing \"" ++ dir ++ "\" in configuration."
                        , details =
                            [ "You need to add all external paths to your NoInternalImports rule configuration."
                            , "External paths are the ones starting with \"..\" or \"/\""
                            ]
                        , range =
                            findRange ("\"" ++ dir ++ "\"") rawElmJson
                                |> Maybe.withDefault Range.empty
                        }
                    )
                )

    else
        Nothing


findRange : String -> String -> Maybe Range
findRange needle haystack =
    haystack
        |> String.split "\n"
        |> findIndexMap
            (\rowIndexMinusOne row ->
                case String.indexes needle row of
                    [ colIndexMinusOne ] ->
                        let
                            rowIndex : Int
                            rowIndex =
                                rowIndexMinusOne + 1

                            colIndex : Int
                            colIndex =
                                colIndexMinusOne + 1
                        in
                        { start =
                            { row = rowIndex
                            , column = colIndex
                            }
                        , end =
                            { row = rowIndex
                            , column = colIndex + String.length needle
                            }
                        }
                            |> Just

                    _ ->
                        Nothing
            )


findIndexMap : (Int -> a -> Maybe b) -> List a -> Maybe b
findIndexMap f list =
    let
        go : Int -> List a -> Maybe b
        go i queue =
            case queue of
                [] ->
                    Nothing

                h :: t ->
                    case f i h of
                        Nothing ->
                            go (i + 1) t

                        (Just _) as r ->
                            r
    in
    go 0 list


type alias Path =
    List String


type alias PackageName =
    String


type alias ProjectContext =
    { additionalPaths : Set Path
    , moduleToPackage : Dict ModuleName PackageName
    , srcPathToPackage : Dict Path PackageName
    , exposed : Dict PackageName (Set ModuleName)
    }


type alias ModuleContext =
    { filePath : String
    , package : Maybe PackageName
    , moduleName : ModuleName
    , moduleToPackage : Dict ModuleName PackageName
    , srcPathToPackage : Dict Path PackageName
    , exposed : Dict PackageName (Set ModuleName)
    }


moduleVisitor : Rule.ModuleRuleSchema schema ModuleContext -> Rule.ModuleRuleSchema { schema | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor importVisitor


importVisitor : Node Import -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
importVisitor (Node range import_) context =
    let
        (Node _ moduleName) =
            import_.moduleName
    in
    case Dict.get moduleName context.moduleToPackage of
        Nothing ->
            ( [--  Rule.error
               --     { message = "TODO - importVisitor 1"
               --     , details = [ "TODO - importVisitor 1" ]
               --     }
               --     range
              ]
            , context
            )

        Just package ->
            if context.package == Just package then
                ( [], context )

            else
                let
                    exposed : Set ModuleName
                    exposed =
                        Dict.get package context.exposed
                            |> Maybe.withDefault Set.empty
                in
                if Set.member moduleName exposed then
                    ( [], context )

                else
                    let
                        exposedString =
                            exposed
                                |> Set.toList
                                |> List.map (String.join ".")
                                |> String.join ", "
                    in
                    ( [ Rule.error
                            { message = "Importing internal module " ++ String.join "." moduleName
                            , details =
                                [ "Module " ++ String.join "." moduleName ++ " is not exposed by package " ++ package ++ "."
                                , "Exposed modules: " ++ exposedString
                                ]
                            }
                            range
                      ]
                    , context
                    )


initialProjectContext : List String -> ProjectContext
initialProjectContext additionalPaths =
    { additionalPaths =
        additionalPaths
            |> List.map toPath
            |> Set.fromList
    , moduleToPackage = Dict.empty
    , srcPathToPackage = Dict.empty
    , exposed = Dict.empty
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\filePath moduleName projectContext ->
            { filePath = filePath
            , package = Dict.get (rootDirName (toPath filePath) moduleName) projectContext.srcPathToPackage
            , moduleName = moduleName
            , moduleToPackage = projectContext.moduleToPackage
            , srcPathToPackage = projectContext.srcPathToPackage
            , exposed = projectContext.exposed
            }
        )
        |> Rule.withFilePath
        |> Rule.withModuleName


rootDirName : Path -> ModuleName -> Path
rootDirName path moduleName =
    path
        |> List.reverse
        |> List.drop (List.length moduleName)
        |> List.reverse


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { additionalPaths = Set.empty
            , moduleToPackage =
                case moduleContext.package of
                    Nothing ->
                        Dict.empty

                    Just package ->
                        Dict.singleton moduleContext.moduleName package
            , srcPathToPackage = Dict.empty
            , exposed = Dict.empty
            }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts new previous =
    { additionalPaths = Set.union new.additionalPaths previous.additionalPaths
    , moduleToPackage = Dict.union new.moduleToPackage previous.moduleToPackage
    , srcPathToPackage = Dict.union new.srcPathToPackage previous.srcPathToPackage
    , exposed = Dict.union new.exposed previous.exposed
    }

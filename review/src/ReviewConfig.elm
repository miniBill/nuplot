module ReviewConfig exposing (config)

import Docs.ReviewAtDocs
import NoAlways
import NoBooleanCase
import NoBrokenParserFunctions
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDuplicatePorts
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoRecordAliasConstructor
import NoSimpleLetBody
import NoUnsafePorts
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import Review.Rule as Rule exposing (Rule)
import ReviewPipelineStyles
import Simplify


config : List Rule
config =
    [ NoAlways.rule
    , Docs.ReviewAtDocs.rule
    , NoBooleanCase.rule
    , NoConfusingPrefixOperator.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories
            [ "tests"
            , "vendored/miniBill/elm-glsl"
            ]
    , NoDuplicatePorts.rule

    -- , NoEtaReducibleLambdas.rule
    --     { lambdaReduceStrategy = NoEtaReducibleLambdas.AlwaysRemoveLambdaWhenPossible
    --     , argumentNamePredicate = always True
    --     }
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoModuleOnExposedNames.rule
    , NoPrematureLetComputation.rule
    , NoRecordAliasConstructor.rule
    , NoSimpleLetBody.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoUnused.CustomTypeConstructorArgs.rule
        |> Rule.ignoreErrorsForDirectories
            [ "vendored/miniBill/elm-glsl"
            ]
    , NoUnused.CustomTypeConstructors.rule []
        |> Rule.ignoreErrorsForDirectories
            [ "vendored/miniBill/elm-glsl"
            ]

    -- , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForDirectories
            [ "generated"
            , "vendored/miniBill/elm-glsl"
            ]
    , NoUnused.Modules.rule
        |> Rule.ignoreErrorsForDirectories
            [ "vendored/miniBill/elm-glsl"
            ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUnusedPorts.rule
    , Simplify.rule Simplify.defaults
    , ReviewPipelineStyles.rule
        [ ReviewPipelineStyles.forbid ReviewPipelineStyles.leftCompositionPipelines
            |> ReviewPipelineStyles.andCallThem "forbidden << composition"
        , ReviewPipelineStyles.forbid ReviewPipelineStyles.rightCompositionPipelines
            |> ReviewPipelineStyles.andCallThem "forbidden >> composition"
        ]
    , NoBrokenParserFunctions.rule
    ]

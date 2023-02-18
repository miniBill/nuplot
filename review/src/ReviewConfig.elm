module ReviewConfig exposing (config)

import Docs.ReviewAtDocs
import NoAlways
import NoBooleanCase
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDuplicatePorts
import NoEtaReducibleLambdas
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
import Simplify


config : List Rule
config =
    [ NoAlways.rule
    , Docs.ReviewAtDocs.rule
    , NoBooleanCase.rule
    , NoConfusingPrefixOperator.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoDuplicatePorts.rule
    , NoEtaReducibleLambdas.rule
        { lambdaReduceStrategy = NoEtaReducibleLambdas.AlwaysRemoveLambdaWhenPossible
        , argumentNamePredicate = always True
        }
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
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUnusedPorts.rule
    , Simplify.rule Simplify.defaults
    ]

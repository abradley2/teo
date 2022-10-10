module Verify.Form exposing (FieldValidator, FormValidator, keep, liftValidator, ok, run, verify)

import Verify exposing (Validator)


{-| This module is based around a simple type alias over 
[elm-verify](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest)'s 
[Validator](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest/Verify#Validator) 
type that I commonly find myself using.

Whereas the `Validator` type is defined as

    type alias Validator error input verified =
        input -> Result ( error, List error ) verified

The `FormValidator` types is defined

    type alias FormValidator input verified =
        input -> Result (form -> form) verified

[Validator](https://package.elm-lang.org/packages/stoeffel/elm-verify/latest/Verify#Validator) is great if
you wish for the failure of validation to be a list of errors, but what if we have those errors expressed
on our model? 

    type alias Model =
        { firstName : String
        , firstNameError : Maybe String
        , lastName : String
        , lastNameError : Maybe String
        }


    type alias ValidatedModel =
        { firstName : String
        , lastName : String
        }

We don't need a unified "error" type. Our errors are just part of our model. Here's a quick example
of how this looks in action:

    import Verify.Form exposing (FieldValidator, FormValidator)

    firstNameValidator : FieldValidator String Model validatedModel
    firstNameValidator input = 
        if String.isEmpty input then
            Result.Err (\model -> { model | firstNameError = Just "First name is required" })
        else
            Result.Ok input

    lastNameValidator : FieldValidator String Model validatedModel
    lastNameValidator input = 
        if String.isEmpty input then
            Result.Err (\model -> { model | lastNameError = Just "First name is required" })
        else
            Result.Ok input

    modelValidator : FormValidator Model ValidatedModel
    modelValidator =
        Verify.Form.ok ValidatedModel
            |> Verify.Form.verify .firstName firstNameValidator
            |> Verify.Form.verify .lastName lastNameValidator
-}
type alias FormValidator form verified =
    Validator (form -> form) form verified


type alias FieldValidator field form verified =
    field -> Result (form -> form) verified


ok : verified -> FormValidator input verified
ok =
    Verify.validate


run : FormValidator form validatedForm -> form -> Result form validatedForm
run validator initialForm =
    validator initialForm
        |> Result.mapError (\( err, errors ) -> err :: errors)
        |> Result.mapError (List.foldr (<|) initialForm)


keep :
    (form -> field)
    -> FormValidator form (field -> finally)
    -> FormValidator form finally
keep getter validator =
    verify
        getter
        Ok
        validator


verify :
    (form -> field)
    -> FieldValidator field form verified
    -> FormValidator form (verified -> finally)
    -> FormValidator form finally
verify getter fieldValidator formValidator =
    Verify.verify
        identity
        (liftFieldValidator fieldValidator getter)
        formValidator


liftFieldValidator : FieldValidator input form verified -> (form -> input) -> FormValidator form verified
liftFieldValidator fieldValidator getter form =
    let
        input =
            getter form
    in
    fieldValidator input
        |> Result.mapError (\err -> ( err, [] ))


liftValidator : (List error -> form -> form) -> Validator error input verified -> FieldValidator input form verified
liftValidator fromErrors fn =
    fn
        >> Result.mapError (\( err, errors ) -> fromErrors (err :: errors))

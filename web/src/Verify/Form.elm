module Verify.Form exposing (FieldValidator, FormValidator, keep, liftValidator, ok, run, verify)

import Verify exposing (Validator)


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
        |> Result.mapError
            (List.foldr
                (<|)
                initialForm
            )


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

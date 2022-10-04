# Elm I18Next Gen

An [elm-codegen](https://github.com/mdgriffith/elm-codegen) library for creating type-safe helper methods
for the excellent [ChristophP/elm-i18next](https://package.elm-lang.org/packages/ChristophP/elm-i18next/latest/)
package.

If you have a large Elm application in a business setting, you should strongly consider supporting internationalization
even if you aren't considering adding extra languages to your app. Your project manager who would much rather change a line
in a JSON file than put another 1-point ticket into your sprint will thank you for it.

## Usage

* Follow the ["Getting Started" guide from elm-codegen](https://github.com/mdgriffith/elm-codegen/blob/main/guide/GettingStarted.md)
* In your initialized codegen project, `elm install abradley2/elm-i18next-gen`

```
import I18Next.Gen
import Gen.CodeGen.Generate as Generate
import Json.Decode exposing (Value)

main : Platform Value () ()
main = 
    Generate.fromJson
        I18Next.Gen.i18nextDecoder
        (I18Next.Gen.jsonValueToFiles "MyCustomRoot")
```

Then just run `elm-codegen` with `npx`, supplying the translations file as flags.

`npx elm-codegen run --flags-from="path/to/translations/en.json" --output="path/to/myapp/src`

## Example

You should have a root directory that contains your Elm application. From this directory you've called
`npx elm-codegen init` and have something resembling the following:

```
my-project/
|--elm-app/
|  |--translations.en.json
|  |--src/
|     |--Main.elm
|
|--codegen/
   |--Generate.elm
```

When you have followed the steps in the **Usage** section, running-
```
npx elm-codegen run --flags-from="elm-app/translations.en.json --output="elm-app/src"
```
from the **my-project** root should produce:

```
my-project/
|--elm-app/
|  |--translations.en.json
|  |--src/
|     |--Main.elm
|     |--Language.elm
|     |--Translations.elm
|
|--codegen/
   |--Generate.elm
```


## Recommended Pattern

It is recommended that you only run this codegen for a single default language. Part of the output
includes a `defaultTranslations` export of the `I18Next.Translations` type.

## Thanks

This library is largely based upon the work done by [Yoni Gibbs](https://github.com/yonigibbs) 
on [elm-i8next-gen](https://github.com/yonigibbs/elm-i18next-gen)

# Conductor [prototype]

A [railway-oriented](https://fsharpforfunandprofit.com/rop/) (= may contain traces of monads) data validation library for F#.

This is a very early prototype - you probably shouldn't use this for anything important.

## Extremely basic usage example

``` fsharp

let nameRules = [
  Validators.notNull "Name shouldn't be null"
  Validators.String.length 3 40 "Name should be between 3 and 40 characters"
] |> Validator.chain

let validate = Validator.validate nameRules

> validate ""
Invalid ("Name should be between 3 and 40 characters")

> validate "test"
Valid ("test")

```

For a bit more complete example, see [here](Conductor.Sample.Basic/Program.fs).

## Use cases

* User input validation
* Web request validation
* Configuration file validation
* Pretty much anything, really.

## Current features

* Asynchronous and non-blocking via [Hopac](https://github.com/Hopac/Hopac)
  * Parallel by default
  * Supports long running-operations such as database fetches and web requests without hitching
* Composable
* [Small](Conductor/Conductor.fs)
* Trivial to expand
* Includes some built-in validators and validator builders
* Flexible validation failure metadata model
  * You can use any type (strings, ADTs, units, exceptions) as metadata

## TODO

* Lots

## License

Licensed under the [MIT License](LICENSE).

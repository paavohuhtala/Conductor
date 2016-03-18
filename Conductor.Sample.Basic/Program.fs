
open System

open Conductor
open Conductor.Validator.Infixes

type NewUser = {
  name: string
  password: string }

type FailureReason =
  | ``Name was empty``
  | ``Name was too short or too long``
  | ``Password was same as name``
  | ``Password was empty``
  | ``Password didn't contain an upper case letter``
  | ``Password didn't contain a number``

let name nu = nu.name
let password nu = nu.password

let nameRules =
  [ Validators.String.notEmpty         ``Name was empty``
    Validator.map (fun s -> s.Trim())
    Validators.String.length 3 40       ``Name was too short or too long``
  ] |> Validator.chain

let namePasswordChecker =
  Validator.pred
    (fun user -> user.name <> user.password) ``Password was same as name``

let passwordRules =
  [ Validators.String.notEmpty         ``Password was empty``
    Validators.Seq.exists Char.IsUpper ``Password didn't contain an upper case letter``
    Validators.Seq.exists Char.IsNumber ``Password didn't contain a number``
  ] |> Validator.chain

let newUserValidator =
  [ name >--> nameRules
    password >--> passwordRules
    namePasswordChecker
  ] |> Validator.all

[<EntryPoint>]
let main argv =
  let validate = Validator.validate newUserValidator

  printfn "%A" (validate {name = "ExampleUser"; password = "Monads123"})
  printfn "%A" (validate {name = "A dude"; password = ""})
  printfn "%A" (validate {name = "Hunter2"; password = "Hunter2"})

  0

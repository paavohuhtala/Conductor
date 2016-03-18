namespace Conductor

open Hopac
open Hopac.Extensions

type ValidationResult<'a, 'invalidMeta> =
  | Valid of validated: 'a
  | Invalid of meta: 'invalidMeta
with
  member this.isValid =
    match this with
    | Valid _ -> true
    | _ -> false

type Validator<'a, 'b, 'invalidMeta> =
  | Validator of ('a -> Job<ValidationResult<'b, 'invalidMeta>>)

module ValidationResult =
  let isValid (x: ValidationResult<_, _>) = x.isValid

  let bind f = function
    | Valid x -> f x
    | Invalid meta -> Invalid meta

  let map f = function
    | Valid x -> Valid (f x)
    | Invalid meta -> Invalid meta

  let lift f = map f

  let mapMeta f = function
    | Invalid meta -> Invalid (f meta)
    | Valid x -> Valid x

  let filter f meta = function
    | Valid x ->
      if f x then
        Valid x
      else
        Invalid meta
    | Invalid oldMeta -> Invalid oldMeta

  let setMeta meta = function
    | Invalid _ -> Invalid meta
    | Valid x -> Valid x

  let ofOption meta = function
    | Some x -> Valid x
    | None -> Invalid meta

  let getMeta = function
    | Invalid meta -> meta
    | Valid _ -> failwith "The validation result was valid"

  let get = function
    | Valid x -> x
    | Invalid _ -> failwith "The validation result was invalid"

  let tryGet = function
    | Valid x -> Some x
    | Invalid _ -> None

/// Contains functions for creating, manipulating and composing validators.
module Validator =

  open Hopac.Infixes

  /// Validates a value using a validator.
  /// Blocks until the validation is complete.
  let validate (Validator f) x = Job.Global.run (f x)

  /// Validates a value using a validator.
  /// Returns a Hopac job, which is resolved when the validation is complete.
  let validateJob (Validator f) x = f x
  let internal validateJob' x (Validator f) = f x

  /// Validates a value using a validator.
  /// Returns an Async computation, which is resolved when the validation is complete.
  let validateAsync (Validator f) x = Async.Global.ofJob (f x)

  /// Passes the input through without modifying it.
  let passthrough () : Validator<'a, 'a, _> =
    Valid >> Job.result
    |> Validator

  /// Composes two validators to be run sequentially.
  let compose (Validator f) (Validator g) =
    Validator <| fun x -> job {
      let! fx = f x
      match fx with
      | Valid x -> return! g x
      | Invalid i -> return Invalid i }

  /// Sames as Validator.compose, but returns the leftmost validator if both
  /// validators succeed.
  let composeL (Validator f) (Validator g) =
    Validator <| fun x -> job {
      let! fx = f x
      match fx with
      | Valid x ->
        let! gx = g x
        match gx with
        | Valid _ -> return fx
        | Invalid i -> return Invalid i
      | Invalid i -> return Invalid i }

  /// Composes a list of validators to be run sequentially.
  /// Equivalent to chaining calls to Validator.compose, except all
  /// validators must have the same return type.
  /// Returns Validator.passthrough if the sequence is empty.
  let chain fs =
    Seq.fold (fun comp f -> compose comp f) (passthrough ()) fs

  /// Composes two validators to be run in parallel.
  /// The validation succeeds if both validators succeed.
  let both (Validator f) (Validator g) =
    Validator <| fun x -> job {
      let! (fx, gx) = (f x) <*> (g x)
      match fx, gx with
      | Valid l, Valid r -> return Valid (l, r)
      | Invalid l, Valid _ -> return Invalid (Some l, None)
      | Valid _, Invalid r -> return Invalid (None, Some r)
      | Invalid l, Invalid r -> return Invalid (Some l, Some r)
    }

  /// Composes two validators to be run in parallel.
  /// The validation succeeds if either of the validators succeeds.
  /// If both validators succeed, the result of the leftmost validator is
  /// returned.
  /// If neither validator succeeds, returns the metadata of both of the
  /// validation failures.
  let either (Validator f) (Validator g) =
    Validator <| fun x -> job {
      let! (fx, gx) = (f x) <*> (g x)
      match fx, gx with
      | Valid x, Invalid _
      | Invalid _, Valid x -> return Valid x
      | Valid l, Valid r -> return Valid l
      | Invalid l, Invalid r -> return Invalid [l; r] }

  let internal pickInvalids results =
    results
    |> Seq.filter (ValidationResult.isValid >> not)
    |> Seq.map ValidationResult.getMeta
    |> Seq.toList

  let internal pickValids results =
    results
    |> Seq.filter ValidationResult.isValid
    |> Seq.map ValidationResult.get
    |> Seq.toList

  /// Composes a list of validators to be run in parallel.
  /// The validation succeeds if any of the validators succeeds.
  /// It is not defined which of the validation results is returned.
  /// If none of the validators succeed, returns the metadata for all
  /// validation failures that occured.
  let any (validators: Validator<'a, 'b, 'c> list) =
    Validator <| fun x -> job {
      let! results = Seq.Con.mapJob (validateJob' x) validators
      match Seq.tryFind ValidationResult.isValid results with
      | Some (Valid result) -> return Valid result
      | _ ->
        return Invalid (pickInvalids results) }

  /// Composes a list of validators to be run in parallel.
  /// The validation succeeds if all of the validators succeed.
  /// If all validators succeed, passes the validated value through.
  /// If any of the validators fails, returns the metadata for all validation
  /// failures that occured.
  let all (validators: Validator<'a, 'b, 'c> list) =
    Validator <| fun x -> job {
      let! results = Seq.Con.mapJob (validateJob' x) validators
      if Seq.forall ValidationResult.isValid results then
        return Valid x
      else
        return Invalid (pickInvalids results) }

  /// Same as Validator.all, but returns a list of all validated results
  /// on success.
  let allGather (validators: Validator<'a, 'b, 'c> list) =
    Validator <| fun x -> job {
      let! results = Seq.Con.mapJob (validateJob' x) validators
      if Seq.forall ValidationResult.isValid results then
        return Valid (pickValids results)
      else
        return Invalid (pickInvalids results) }

  /// Creates a new validator from a predicate function.
  let pred f meta =
    Validator <| fun x -> job {
      if f x then
        return Valid x
      else
        return Invalid meta }

  /// Creates a new validator from a function that returns Options.
  let opt (f: 'a -> 'b option) meta =
    (fun x -> ValidationResult.ofOption meta (f x))
    |> Job.lift
    |> Validator

  /// Creates a new validator from a predicate function and a metadata builder.
  /// The metadata builder can be used to create precise, context specific
  /// error messages.
  let predWithMetaBuilder f mb =
    Validator <| fun x -> job {
      if f x then
        return Valid x
      else
        return Invalid (mb x) }

  /// Creates a new transformer validator from a function.
  let map f =
    (fun x -> Valid (f x))
    |> Job.lift
    |> Validator

  /// Creates a new transformer validator from a job builder.
  let jobMap (f: 'a -> Job<'b>) =
    Validator <| fun x -> job {
      let! fx = f x;
      return Valid fx }

  /// Creates a validator that succeeds when the supplied validator fails.
  /// Returns the original value on success.
  /// Returns the supplied metadata on failure.
  let not (Validator f) meta =
    Validator <| fun x -> job {
      let! fx = f x
      match fx with
      | Valid _ -> return Invalid meta
      | Invalid _ -> return Valid x
    }

  /// Provides convenience infix operators for working with validators.
  module Infixes =
    /// See Validator.compose.
    let (>&&>) f g = compose f g

    /// See Validator.composeL.
    let (<&&<) f g = composeL f g

    /// See Validator.either.
    let (<||>) f g = either f g

    /// Creates a new validator that maps the input value using a function
    /// and pipes it to a validator.
    /// If the validator succeeds, returns the original (unmapped) value.
    let (>-->) f g =
      (passthrough ()) <&&< (map f >&&> g)

/// Contains premade validators and validator builders for working with BCL and
/// standard library types.
module Validators =
  let equals x meta = Validator.pred ((=) x) meta
  let notEqual x meta = Validator.pred ((<>) x) meta
  let notNull meta = Validator.pred ((<>) null) meta

  let inRangeInc minInc maxInc meta =
    Validator.pred(fun x -> x >= minInc && x <= maxInc) meta

  let inRangeExc minInc maxExc meta =
    Validator.pred(fun x -> x >= minInc && x <= maxExc) meta

  module Option =
    let isSome meta = Validator.pred (Option.isSome) meta
    let isNone meta = Validator.pred (Option.isNone) meta
    let matches f meta = Validator.opt (Option.filter f) meta
    let equals x meta = matches ((=) x) meta

  module String =
    open System

    /// Wrapper for String.IsNullOrWhiteSpace >> not.
    let notEmpty meta = Validator.pred (String.IsNullOrWhiteSpace >> not) meta

    /// Range is inclusive.
    let length (min: int) (max: int) meta =
      Validator.pred (fun (x: string) -> x.Length >= min && x.Length <= max) meta

  module Seq =
    let contains x meta = Validator.pred (Seq.contains x) meta
    let exists f meta = Validator.pred (Seq.exists f) meta

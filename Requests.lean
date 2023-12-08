import Soda
import Soda.Grape
import Soda.Grape.Text
import Lean.Data.HashMap
import Lean.Data.Json
import Lean.Data.JsonRpc
import Socket

open Lean
open Socket
open Function

namespace Requests

--| Types for HTTP status codes.
inductive HttpStatusCode where
  | continued
  | switchingProtocols
  | processing
  | earlyHints
  | ok
  | created
  | accepted
  | nonAuthoritativeInformation
  | noContent
  | resetContent
  | partialContent
  | multiStatus
  | alreadyReported
  | imUsed
  | multipleChoices
  | movedPermanently
  | found
  | seeOther
  | notModified
  | useProxy
  | unused
  | temporaryRedirect
  | permanentRedirect
  | badRequest
  | unauthorized
  | paymentRequired
  | forbidden
  | notFound
  | methodNotAllowed
  | notAcceptable
  | proxyAuthenticationRequired
  | requestTimeout
  | conflict
  | gone
  | lengthRequired
  | preconditionFailed
  | payloadTooLarge
  | uriTooLong
  | unsupportedMediaType
  | rangeNotSatisfiable
  | expectationFailed
  | imATeapot
  | misdirectedRequest
  | unprocessableEntity
  | locked
  | failedDependency
  | tooEarly
  | upgradeRequired
  | preconditionRequired
  | tooManyRequests
  | requestHeaderFieldsTooLarge
  | unavailableForLegalReasons
  | internalServerError
  | notImplemented
  | badGateway
  | serviceUnavailable
  | gatewayTimeout
  | httpVersionNotSupported
  | variantAlsoNegotiates
  | insufficientStorage
  | loopDetected
  | notExtended
  | networkAuthenticationRequired
  deriving Repr

-- | Convert a HttpStatusCode to a numeric code. This is useful for sending the status code in a response.
def HttpStatusCode.toCode : HttpStatusCode -> Nat
  | continued => 100
  | switchingProtocols => 101
  | processing => 102
  | earlyHints => 103
  | ok => 200
  | created => 201
  | accepted => 202
  | nonAuthoritativeInformation => 203
  | noContent => 204
  | resetContent => 205
  | partialContent => 206
  | multiStatus => 207
  | alreadyReported => 208
  | imUsed => 226
  | multipleChoices => 300
  | movedPermanently => 301
  | found => 302
  | seeOther => 303
  | notModified => 304
  | useProxy => 305
  | unused => 306
  | temporaryRedirect => 307
  | permanentRedirect => 308
  | badRequest => 400
  | unauthorized => 401
  | paymentRequired => 402
  | forbidden => 403
  | notFound => 404
  | methodNotAllowed => 405
  | notAcceptable => 406
  | proxyAuthenticationRequired => 407
  | requestTimeout => 408
  | conflict => 409
  | gone => 410
  | lengthRequired => 411
  | preconditionFailed => 412
  | payloadTooLarge => 413
  | uriTooLong => 414
  | unsupportedMediaType => 415
  | rangeNotSatisfiable => 416
  | expectationFailed => 417
  | imATeapot => 418
  | misdirectedRequest => 421
  | unprocessableEntity => 422
  | locked => 423
  | failedDependency => 424
  | tooEarly => 425
  | upgradeRequired => 426
  | preconditionRequired => 428
  | tooManyRequests => 429
  | requestHeaderFieldsTooLarge => 431
  | unavailableForLegalReasons => 451
  | internalServerError => 500
  | notImplemented => 501
  | badGateway => 502
  | serviceUnavailable => 503
  | gatewayTimeout => 504
  | httpVersionNotSupported => 505
  | variantAlsoNegotiates => 506
  | insufficientStorage => 507
  | loopDetected => 508
  | notExtended => 510
  | networkAuthenticationRequired => 511

def HttpStatusCode.fromCode : Nat → Option HttpStatusCode
  | 100 => Option.some continued
  | 101 => Option.some switchingProtocols
  | 102 => Option.some processing
  | 103 => Option.some earlyHints
  | 200 => Option.some ok
  | 201 => Option.some created
  | 202 => Option.some accepted
  | 203 => Option.some nonAuthoritativeInformation
  | 204 => Option.some noContent
  | 205 => Option.some resetContent
  | 206 => Option.some partialContent
  | 207 => Option.some multiStatus
  | 208 => Option.some alreadyReported
  | 226 => Option.some imUsed
  | 300 => Option.some multipleChoices
  | 301 => Option.some movedPermanently
  | 302 => Option.some found
  | 303 => Option.some seeOther
  | 304 => Option.some notModified
  | 305 => Option.some useProxy
  | 306 => Option.some unused
  | 307 => Option.some temporaryRedirect
  | 308 => Option.some permanentRedirect
  | 400 => Option.some badRequest
  | 401 => Option.some unauthorized
  | 402 => Option.some paymentRequired
  | 403 => Option.some forbidden
  | 404 => Option.some notFound
  | 405 => Option.some methodNotAllowed
  | 406 => Option.some notAcceptable
  | 407 => Option.some proxyAuthenticationRequired
  | 408 => Option.some requestTimeout
  | 409 => Option.some conflict
  | 410 => Option.some gone
  | 411 => Option.some lengthRequired
  | 412 => Option.some preconditionFailed
  | 413 => Option.some payloadTooLarge
  | 414 => Option.some uriTooLong
  | 415 => Option.some unsupportedMediaType
  | 416 => Option.some rangeNotSatisfiable
  | 417 => Option.some expectationFailed
  | 418 => Option.some imATeapot
  | 421 => Option.some misdirectedRequest
  | 422 => Option.some unprocessableEntity
  | 423 => Option.some locked
  | 424 => Option.some failedDependency
  | 425 => Option.some tooEarly
  | 426 => Option.some upgradeRequired
  | 428 => Option.some preconditionRequired
  | 429 => Option.some tooManyRequests
  | 431 => Option.some requestHeaderFieldsTooLarge
  | 451 => Option.some unavailableForLegalReasons
  | 500 => Option.some internalServerError
  | 501 => Option.some notImplemented
  | 502 => Option.some badGateway
  | 503 => Option.some serviceUnavailable
  | 504 => Option.some gatewayTimeout
  | 505 => Option.some httpVersionNotSupported
  | 506 => Option.some variantAlsoNegotiates
  | 507 => Option.some insufficientStorage
  | 508 => Option.some loopDetected
  | 510 => Option.some notExtended
  | 511 => Option.some networkAuthenticationRequired
  | _   => Option.none

-- | A method is a verb that describes the action to be performed.
inductive Method where
  | get
  | head
  | post
  | put
  | delete
  | connect
  | options
  | trace
  | patch

instance : ToString Method where
  toString
    | Method.get => "GET"
    | Method.head => "HEAD"
    | Method.post => "POST"
    | Method.put => "PUT"
    | Method.delete => "DELETE"
    | Method.connect => "CONNECT"
    | Method.options => "OPTIONS"
    | Method.trace => "TRACE"
    | Method.patch => "PATCH"

-- | A version of the HTTP protocol.
structure Version where
  major : Nat
  minor : Nat
  deriving Repr

instance : ToString Version where
  toString v := toString v.major ++ "." ++ toString v.minor

-- | Headers are a map from header names to header values.
structure Headers where
  headers : HashMap String String

instance : Repr Headers where
  reprPrec h _ := repr h.headers.toList

instance : ToString Headers where
  toString h :=
    let headerStrings := h.headers.toList.map (fun (k, v) => k ++ ": " ++ v)
    String.intercalate "\r\n" headerStrings

-- | A request is a message from the client to the server.
structure HttpRequest where
  method : Method
  uri: String
  version: Version
  headers : Headers
  body : String

instance : ToString HttpRequest where
  toString r :=
    let headerString := toString r.method ++ " " ++ r.uri ++ " HTTP/" ++ toString r.version ++ "\r\n" ++ toString r.headers
    headerString ++ "\r\n\r\n" ++ r.body

structure HttpResponse where
  version : Version
  statusCode : HttpStatusCode
  reasonPhrase : String
  headers : Headers
  body: String
  deriving Repr

def Version.parse : Grape.Grape Version := do
  let major ← Grape.Text.number
  let _ ← Grape.chr '.'
  let minor ← Grape.Text.number
  Grape.pure { major, minor }

def parseHeader : Grape.Grape (String × String) := do
  let headerName ← Grape.takeWhile (fun c => c ≠ 58 && c ≠ 13)
  let _ ← Grape.string ": "
  let headerValue ← Grape.takeWhile (fun c => c ≠ 13)
  let _ ← Grape.string "\r\n"
  Grape.pure (headerName.toASCIIString, headerValue.toASCIIString)

def Headers.parse : Grape.Grape Headers := do
  let headerList ← Grape.list parseHeader
  let headers := HashMap.ofList headerList
  Grape.pure { headers }

def Headers.empty : Headers := { headers := HashMap.empty }

def Headers.add (headers : Headers) (name : String) (value : String) : Headers :=
  { headers with headers := headers.headers.insert name value }

def Headers.with (name: String) (value: String) (headers: Headers) : Headers :=
  headers.add name value


def HttpResponse.parse : Grape.Grape HttpResponse := do
  Grape.string "HTTP/"
  let version ← Version.parse
  Grape.string " "
  let statusCode ← Grape.Text.number
  Grape.string " "
  let reasonPhrase ← Grape.takeWhile (fun c => c ≠ 13)
  Grape.string "\r\n"
  let headers ← Headers.parse
  Grape.string "\r\n"
  let statusCode := HttpStatusCode.fromCode statusCode
  let body ← Grape.takeUntilEnd
  let body := body.toASCIIString

  match statusCode with
  | Option.none            => Grape.fail "Invalid status code"
  | Option.some statusCode => Grape.pure { version, statusCode, reasonPhrase := reasonPhrase.toASCIIString, headers, body }

-- | A connection to a server.
structure Connection where
  socket : Socket
  address : SockAddr

def Connection.send (connection: Connection) (request : HttpRequest) : IO (Except String HttpResponse) := do
  let _ ← connection.socket.send (String.toUTF8 $ ToString.toString request)
  let recv ← connection.socket.recv 4096
  match recv with
  | some recv => do
    let str := String.fromUTF8Unchecked recv
    let parsed := Grape.run HttpResponse.parse str.toSlice
    match parsed with
    | Result.done response _ => pure $ Except.ok response
    | Result.error err _     => pure $ Except.error (toString err)
    | Result.cont _          => pure $ Except.error "Incomplete response"
  | none => pure $ Except.error "Invalid UTF-8"

def Connection.create (address: String) (port: String) : IO Connection := do
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  let localAddr ← SockAddr.mk address port AddressFamily.inet SockType.stream
  Socket.setBlocking socket true
  let _ ← socket.connect localAddr
  pure { socket, address := localAddr }

def v10 : Version := { major := 1, minor := 0 }

def Connection.post (connection: Connection) (path: String) (body: String) : IO (Except String HttpResponse) := do
  let request := HttpRequest.mk Method.post path v10
    (Headers.empty
    |> Headers.with "Content-Type" "application/json"
    |> Headers.with "Content-Length" (toString body.length))
    body
  connection.send request

def Connection.get (connection: Connection) (path: String) : IO (Except String HttpResponse) := do
  let request := HttpRequest.mk Method.get path v10 Headers.empty ""
  connection.send request




end Requests

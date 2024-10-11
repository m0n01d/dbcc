module Response = {
  type t

  @send
  external json: t => Js.Promise.t<Js.Json.t> = "json"

  @send
  external text: t => Js.Promise.t<string> = "text"

  @get
  external ok: t => bool = "ok"

  @get
  external status: t => int = "status"

  @get
  external statusText: t => string = "statusText"
}

module Request = {
  type t
  @get
  external body: t => 'a = "body"
}

type options<'body> = {headers: Js.Dict.t<string>, method: string, body: option<'body>}

@val
external fetch: (string, options<'body'>) => Js.Promise.t<Response.t> = "fetch"

let fetchJson = (~headers=Js.Dict.empty(), url: string): Js.Promise.t<Js.Json.t> =>
  fetch(url, {headers: headers, method: "GET", body: None}) |> Js.Promise.then_(res =>
    if !Response.ok(res) {
      res->Response.text->Js.Promise.then_(text => {
        let msg = `${res->Response.status->Js.Int.toString} ${res->Response.statusText}: ${text}`
        Js.Exn.raiseError(msg)
      }, _)
    } else {
      res->Response.json
    }
  )

let postJson = (
  ~headers=Js.Dict.fromList(list{("Content-Type", "application/json")}),
  url: string,
  ~body: 'body,
): Js.Promise.t<unit> =>
  fetch(
    url,
    {headers: headers, method: "POST", body: Some(Js.Json.stringifyAny(body))},
  ) |> Js.Promise.then_(res => {
    Js.Console.log("POSTED")
    if Response.ok(res) {
      Js.Promise.resolve()
    } else {
      Js.Exn.raiseError("woops")
    }
  })

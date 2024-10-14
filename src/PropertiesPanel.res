%raw(`require("./PropertiesPanel.css")`)
let apiUrl = `http://localhost:12346/examples`
module Collapsible = {
  @react.component
  let make = (~title, ~children) => {
    let (collapsed, toggle) = React.useState(() => false)

    <section className="Collapsible">
      <button className="Collapsible-button" onClick={_e => toggle(_ => !collapsed)}>
        <span> {React.string(title)} </span> <span> {React.string(collapsed ? "+" : "-")} </span>
      </button>
      {collapsed ? React.null : <div className="Collapsible-content"> {children} </div>}
    </section>
  }
}

// This component provides a simplified example of fetching JSON data from
// the backend and rendering it on the screen.
module ViewExamples = {
  // Type of the data returned by the /examples endpoint
  type example = {
    id: int,
    some_int: int,
    some_text: string,
  }

  @react.component
  let make = () => {
    let (examples: option<array<example>>, setExamples) = React.useState(_ => None)

    // React.useEffect1(() => {
    //   // Fetch the data from /examples and set the state when the promise resolves
    //   Fetch.fetchJson(apiUrl)
    //   |> Js.Promise.then_(examplesJson => {
    //     // NOTE: this uses an unsafe type cast, as safely parsing JSON in rescript is somewhat advanced.
    //     Js.Promise.resolve(setExamples(_ => Some(Obj.magic(examplesJson))))
    //   })
    //   // The "ignore" function is necessary because each statement is expected to return `unit` type, but Js.Promise.then return a Promise type.
    //   |> ignore
    //   None
    // }, [setExamples])

    <div>
      {switch examples {
      | None => React.string("Loading examples....")
      | Some(examples) =>
        examples
        ->Js.Array2.map(example => React.null)
        // React.string(`Int: ${example.some_int->Js.Int.toString}, Str: ${example.some_text}`)
        ->React.array
      }}
    </div>
  }
}

type theUnit = Pixels | Points // "%" "px"
let theUnitToString = theUnit =>
  switch theUnit {
  | Pixels => "px"
  | Points => "pt"
  }
type userInput = (int, theUnit)

type status = Changed(userInput) | Default | Focused(userInput) | Saved(userInput)

type properties = {bottom: status, left: status, right: status, top: status}

type spacing = Margin(properties) | Padding(properties)

type spacingArea = {margin: spacing, padding: spacing}

type side = Top | Bottom | Left | Right

type model = {shouldFetch: bool, spacingArea: spacingArea}

type msg = ClickedSaved | GotSpacing(spacingArea) | UpdatedSpacing(spacing) | StartedFetching

let encodeStatus_ = ((val, suffix)) => {"val": val, "unit": suffix->theUnitToString}
let encodeStatus = (status: status) => {
  switch status {
  | Changed(inner) => encodeStatus_(inner)
  | Default => {"val": 0, "unit": ""}
  | Focused(inner) => encodeStatus_(inner)
  | Saved(inner) => encodeStatus_(inner)
  }
}
let encodeSpacingProperties = ({bottom, left, right, top}: properties) =>
  {
    "bottom": encodeStatus(bottom),
    "left": encodeStatus(left),
    "right": encodeStatus(right),
    "top": encodeStatus(top),
  }

let encodeSpacing = ({margin: Margin(margin), padding: Padding(padding)}: spacingArea) =>
  {
    "margin": encodeSpacingProperties(margin),
    "padding": encodeSpacingProperties(padding),
  }

let decodeSuffix = suffix => {
  switch suffix {
  | "px" => Pixels
  | "pt" => Points
  }
}

module Decode = {
  open Json.Decode

  type decodeHelp = {
    val: int,
    unit: theUnit,
  }
  type defaultHelp = {val: int}

  let decodeStatus_ = field("val", Json.Decode.int)
  let decodeSuffix_ = Json.Decode.map(string, (. s) => decodeSuffix(s))
  let decodeSaved = Json.Decode.object(field => {
    val: field.required(. "val", Json.Decode.int),
    unit: field.required(. "unit", decodeSuffix_),
  })->Json.Decode.map((. x) => Saved((x.val, x.unit)))

  let decodeDefault = Json.Decode.object(field => {
    val: field.required(. "val", Json.Decode.int),
  })->Json.Decode.flatMap((. s) =>
    if s.val == 0 {
      Json.Decode.custom((. _) => Default)
    } else {
      Error.expected("ignored", Js.Json.string("fall through"))
    }
  )
  let decodeStatus = Json.Decode.oneOf([decodeDefault, decodeSaved])

  let decodeProperties = object(field => {
    bottom: field.required(. "bottom", decodeStatus),
    left: field.required(. "left", decodeStatus),
    right: field.required(. "right", decodeStatus),
    top: field.required(. "top", decodeStatus),
  })
  let decodeSpacing = object(field => {
    margin: field.required(.
      "margin",
      decodeProperties->Json.Decode.map((. margin) => Margin(margin)),
    ),
    padding: field.required(.
      "padding",
      decodeProperties->Json.Decode.map((. padding) => Padding(padding)),
    ),
  })
}

let updateFieldSpacing = (outerSpacing, theField, newValue) => {
  let setNewField = spacing =>
    switch theField {
    | Bottom => {...spacing, bottom: newValue}
    | Left => {...spacing, left: newValue}
    | Right => {...spacing, right: newValue}
    | Top => {...spacing, top: newValue}
    }
  switch outerSpacing {
  | Margin(margin) => setNewField(margin)->Margin
  | Padding(padding) => setNewField(padding)->Padding
  }
}

let viewSpacingProperty = (marginOrPadding, theField, dispatch) => {
  let spacingStatus = spacing =>
    switch theField {
    | Top => spacing.top
    | Bottom => spacing.bottom
    | Left => spacing.left
    | Right => spacing.right
    }
  let theStatus = switch marginOrPadding {
  | Margin(margin) => spacingStatus(margin)
  | Padding(padding) => spacingStatus(padding)
  }

  let updateSpacing = newValue => UpdatedSpacing(
    updateFieldSpacing(marginOrPadding, theField, newValue),
  )

  <div>
    <div className={"inline-block px-1 py-1 text-sm"}>
      {switch theStatus {
      | Changed((val, suffix)) =>
        <button
          className={"changed unset"}
          type_={"button"}
          onClick={_ => dispatch(updateSpacing(Focused((val, suffix))))}>
          {`${val->Belt.Int.toString}${suffix->theUnitToString}`->React.string}
        </button>

      | Default => {
          let focusProperty = _ => {
            dispatch(updateSpacing(Focused((0, Pixels))))
          }
          <button className={"default unset"} type_={"button"} onClick={focusProperty}>
            {"auto"->React.string}
          </button>
        }
      | Focused((str, suffix)) =>
        let handleInput = e => {
          let s = ReactEvent.Form.currentTarget(e)["value"]

          let maybeNum = s->Belt.Int.fromString
          // @TODO fix negative numbers
          switch maybeNum {
          | Some(n) => dispatch(updateSpacing(Focused((n, suffix))))
          | _ => dispatch(updateSpacing(Focused((0, suffix))))
          }
        }
        let handleBlur = e => {
          let relatedTarget = ReactEvent.Focus.relatedTarget(e)
          let relatedTarget_ = relatedTarget |> Js.Option.getWithDefault({"nodeName": "nope"})
          let nextSibling = ReactEvent.Focus.target(e)["nextSibling"]
          if relatedTarget == nextSibling || relatedTarget_["nodeName"] == "INPUT" {
            ReactEvent.Focus.preventDefault(e)
          } else if str == 0 {
            dispatch(updateSpacing(Default))
          } else {
            dispatch(updateSpacing(Changed((str, suffix))))
          }
        }

        let handleSelect = e => {
          let suffix = ReactEvent.Form.currentTarget(e)["value"]->decodeSuffix
          dispatch(updateSpacing(Focused((str, suffix))))
        }
        <div className={"flex"} onBlur={handleBlur}>
          <input
            autoFocus={true}
            className={"status-input px-1"}
            onChange={handleInput}
            required={true}
            size={str->Js.Int.toString->Js.String.length->Js.Math.max_int(1)}
            // type_={"number"} // harder to style
            value={str->Js.Int.toString}
          />
          <select onChange={handleSelect} value={suffix->theUnitToString}>
            {[Pixels, Points]
            |> Js.Array.map(u =>
              <option value={u->theUnitToString} key={u->theUnitToString}>
                {u->theUnitToString->React.string}
              </option>
            )
            |> React.array}
          </select>
        </div>

      | Saved((val, suffix)) => {
          let focusProperty = _ => {
            dispatch(updateSpacing(Focused((val, suffix))))
          }
          <button className={"default unset"} type_={"button"} onClick={focusProperty}>
            {`${val->Js.Int.toString}${suffix->theUnitToString}`->React.string}
          </button>
        }
      }}
    </div>
  </div>
}

@genType @genType.as("PropertiesPanel") @react.component
let make = () => {
  let defaultField = Default
  let initialState = {
    shouldFetch: true,
    spacingArea: {
      margin: Margin({
        bottom: defaultField,
        left: defaultField,
        right: defaultField,
        top: defaultField,
      }),
      padding: Padding({
        bottom: defaultField,
        left: defaultField,
        right: defaultField,
        top: defaultField,
      }),
    },
  }

  let update = (state, action) => {
    switch action {
    | ClickedSaved => {...state, shouldFetch: true}
    | GotSpacing(spacingArea) => {
        ...state,
        spacingArea: spacingArea,
      }
    | UpdatedSpacing(newSpacing) =>
      switch newSpacing {
      | Margin(m) => {
          let spacingArea = state.spacingArea
          let newSpacingArea = {...spacingArea, margin: newSpacing}
          {...state, spacingArea: newSpacingArea}
        }
      | Padding(p) => {
          let spacingArea = state.spacingArea
          let newSpacingArea = {...spacingArea, padding: newSpacing}
          {...state, spacingArea: newSpacingArea}
        }
      }
    | StartedFetching => {...state, shouldFetch: false}
    }
  }

  let (state, dispatch) = React.useReducer(update, initialState)
  React.useEffect1(() => {
    dispatch(StartedFetching)
    if state.shouldFetch {
      Fetch.fetchJson(apiUrl)
      |> Js.Promise.then_(res => {
        let result = res->Json.decode(Json.Decode.array(Decode.decodeSpacing))

        switch result {
        | Ok([]) => dispatch(GotSpacing(initialState.spacingArea))
        | Ok([spacingArea]) => dispatch(GotSpacing(spacingArea))
        | Error(err) => Js.Console.log(("@TODO error handling :)", err))
        }
        Js.Promise.resolve()
      })
      // The "ignore" function is necessary because each statement is expected to return `unit` type, but Js.Promise.then return a Promise type.
      |> ignore
    }
    None
  }, [state.shouldFetch])
  let rec viewPrism = (outerSpacing: spacing, maybeInnerSpacing: option<spacing>) => {
    <div className={"viewPrism w-full"}>
      <div className={"flex-1 flex justify-center"}>
        {viewSpacingProperty(outerSpacing, Top, dispatch)}
      </div>
      <div className={"flex"}>
        <div className={"w-1/6 px-1 flex justify-center items-center"}>
          {viewSpacingProperty(outerSpacing, Left, dispatch)}
        </div>
        <div className={"flex-2 flex justify-center"}>
          {switch maybeInnerSpacing {
          | Some(Padding(padding)) => viewPrism(Padding(padding), None)
          | _ => React.null
          }}
        </div>
        <div className={"w-1/6 px-1 flex justify-center items-center"}>
          {viewSpacingProperty(outerSpacing, Right, dispatch)}
        </div>
      </div>
      <div className={"flex justify-center"}>
        {viewSpacingProperty(outerSpacing, Bottom, dispatch)}
      </div>
    </div>
  }

  let saveSpacing = spacingArea => {
    let body: 'a = encodeSpacing(spacingArea)

    Fetch.postJson(apiUrl, ~body) |> Js.Promise.then_(res => {
      dispatch(ClickedSaved)

      Js.Promise.resolve(res)
    })
  }

  let isChanged = property_ => {
    switch property_ {
    | Default(_) => false
    | Saved(_) => false

    | _ => true
    }
  }
  let anyFieldChanged = properties =>
    [properties.bottom, properties.left, properties.right, properties.top] |> Js.Array.some(
      isChanged,
    )

  let isSpacingChanged = spacingArea =>
    switch spacingArea {
    | Margin(margin) => anyFieldChanged(margin)
    | Padding(padding) => anyFieldChanged(padding)
    }

  let identity = x => x

  let isChanged =
    [
      isSpacingChanged(state.spacingArea.margin),
      isSpacingChanged(state.spacingArea.padding),
    ] |> Js.Array.some(identity)

  <aside className="PropertiesPanel">
    // <Collapsible title="Load examples"> <ViewExamples /> </Collapsible>
    <Collapsible title="Margins & Padding">
      <form
        onSubmit={e => {
          ReactEvent.Form.preventDefault(e)
          saveSpacing(state.spacingArea)->ignore
        }}>
        <div className={"box"}>
          {viewPrism(state.spacingArea.margin, Some(state.spacingArea.padding))}
        </div>
        <div className={"px-4"}>
          <button className={"publish"} disabled={!isChanged} type_={"submit"}>
            {"Publish"->React.string}
          </button>
        </div>
      </form>
    </Collapsible>
    <Collapsible title="Size"> <span> {React.string("example")} </span> </Collapsible>
  </aside>
}

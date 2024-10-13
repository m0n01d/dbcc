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

type theUnit = string // "%" "px"

type status = Changed(theUnit) | Default(theUnit) | Focused(theUnit)
let statusToString = (status: status): string => {
  switch status {
  | Changed(str) => str
  | Default(str) => str
  | Focused(str) => str
  }
}

type properties = {bottom: status, left: status, right: status, top: status}

type spacing = Margin(properties) | Padding(properties)

type spacingArea = {margin: spacing, padding: spacing}

type field = Top | Bottom | Left | Right

type model = {shouldFetch: bool, spacingArea: spacingArea}

type msg = GotSpacing(spacingArea) | UpdatedSpacing(spacing) | Saved | StartedFetching

let encodeSpacingProperties = ({bottom, left, right, top}: properties) =>
  {
    "bottom": statusToString(bottom),
    "left": statusToString(left),
    "right": statusToString(right),
    "top": statusToString(top),
  }

let encodeSpacing = ({margin: Margin(margin), padding: Padding(padding)}: spacingArea) =>
  {
    "margin": encodeSpacingProperties(margin),
    "padding": encodeSpacingProperties(padding),
  }

module Decode = {
  open Json.Decode
  let decodeStatus = Json.Decode.map(string, (. s) => Default(s))
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
      | Changed(str) =>
        <button
          className={"changed unset"}
          type_={"button"}
          onClick={_ => dispatch(updateSpacing(Focused(str)))}>
          {str->React.string}
        </button>
      | Default(str) => {
          let focusProperty = _ => {
            dispatch(updateSpacing(Focused(str)))
          }
          <button className={"default unset"} type_={"button"} onClick={focusProperty}>
            {str->React.string}
          </button>
        }
      | Focused(str) => {
          let handleInput = e => {
            dispatch(updateSpacing(Focused(ReactEvent.Form.currentTarget(e)["value"])))
          }
          let handleBlur = e => {
            if str == "" {
              dispatch(updateSpacing(Changed("auto")))
            } else {
              dispatch(updateSpacing(Changed(str)))
            }
          }
          <input
            autoFocus={true}
            className={"px-1"}
            onBlur={handleBlur}
            onChange={handleInput}
            required={true}
            size={str->Js.String.length->Js.Math.max_int(1)}
            value={str}
          />
        }
      }}
    </div>
  </div>
}

@genType @genType.as("PropertiesPanel") @react.component
let make = () => {
  let initialState = {
    shouldFetch: true,
    spacingArea: {
      margin: Margin({
        bottom: Default("auto"),
        left: Default("auto"),
        right: Default("auto"),
        top: Default("auto"),
      }),
      padding: Padding({
        bottom: Default("auto"),
        left: Default("auto"),
        right: Default("auto"),
        top: Default("auto"),
      }),
    },
  }

  let update = (state, action) => {
    switch action {
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
    | Saved => {...state, shouldFetch: true}
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
        | Error(err) => Js.Console.log("@TODO error handling :)")
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
      dispatch(Saved)

      Js.Promise.resolve(res)
    })
  }

  let isChanged = property_ => {
    switch property_ {
    | Default(_) => false
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

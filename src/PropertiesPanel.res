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

    React.useEffect1(() => {
      // Fetch the data from /examples and set the state when the promise resolves
      Fetch.fetchJson(apiUrl)
      |> Js.Promise.then_(examplesJson => {
        // NOTE: this uses an unsafe type cast, as safely parsing JSON in rescript is somewhat advanced.
        Js.Promise.resolve(setExamples(_ => Some(Obj.magic(examplesJson))))
      })
      // The "ignore" function is necessary because each statement is expected to return `unit` type, but Js.Promise.then return a Promise type.
      |> ignore
      None
    }, [setExamples])

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

type properties = {bottom: status, left: status, right: status, top: status}

type spacing = Margin(properties) | Padding(properties)

type spacingArea = {margin: spacing, padding: spacing}

type model = {initialState: spacingArea, shouldFetch: bool, spacingArea: spacingArea}

type field = Top | Bottom | Left | Right
type msg = GotSpacing(spacingArea) | NoOp | UpdatedSpacing(spacing) | Saved | StartedFetching

module Decode = {
  open Json.Decode
  let decodeStuff = Json.Decode.map(string, (. s) => Default(s))
  let decodeProperties = object(field => {
    bottom: field.required(. "bottom", decodeStuff),
    left: field.required(. "left", decodeStuff),
    right: field.required(. "right", decodeStuff),
    top: field.required(. "top", decodeStuff),
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
  switch (outerSpacing, theField) {
  | (Margin(margin), Top) => Margin({...margin, top: newValue})
  | (Margin(margin), Bottom) => Margin({...margin, bottom: newValue})
  | (Margin(margin), Left) => Margin({...margin, left: newValue})
  | (Margin(margin), Right) => Margin({...margin, right: newValue})

  | (Padding(padding), Top) => Padding({...padding, top: newValue})
  | (Padding(padding), Bottom) => Padding({...padding, bottom: newValue})
  | (Padding(padding), Left) => Padding({...padding, left: newValue})
  | (Padding(padding), Right) => Padding({...padding, right: newValue})
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

  <span className={"text-sm "}>
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
          dispatch(updateSpacing(Changed(str)))
        }
        <input value={str} onChange={handleInput} onBlur={handleBlur} size={5} />
      }
    }}
  </span>
}

@genType @genType.as("PropertiesPanel") @react.component
let make = () => {
  let initialState = {
    initialState: {
      margin: Margin({
        bottom: Focused("100pt"),
        left: Changed("1px"),
        right: Changed("24px"),
        top: Default("auto"),
      }),
      padding: Padding({
        bottom: Default("auto"),
        left: Default("auto"),
        right: Changed("24px"),
        top: Default("auto"),
      }),
    },
    shouldFetch: true,
    spacingArea: {
      margin: Margin({
        bottom: Focused("100pt"),
        left: Changed("1px"),
        right: Changed("24px"),
        top: Default("auto"),
      }),
      padding: Padding({
        bottom: Default("auto"),
        left: Default("auto"),
        right: Changed("24px"),
        top: Default("auto"),
      }),
    },
  }

  let statusToString = (status: status): string => {
    switch status {
    | Changed(str) => str
    | Default(str) => str
    | Focused(str) => str
    }
  }

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

  let update = (state, action) => {
    switch action {
    | GotSpacing(spacingArea) => {
        ...state,
        spacingArea: spacingArea,
        initialState: spacingArea,
      }
    | NoOp => initialState
    | UpdatedSpacing(newSpacing) =>
      switch newSpacing {
      | Margin(m) => {
          let spacingArea = state.spacingArea
          let newSpacing = {...spacingArea, margin: Margin(m)}
          {...state, spacingArea: newSpacing}
        }
      | Padding(p) => {
          let spacingArea = state.spacingArea
          let newSpacing = {...spacingArea, padding: Padding(p)}
          {...state, spacingArea: newSpacing}
        }
      }
    | Saved => // let _ = Js.Console.log(("saved", body))

      {...state, shouldFetch: true}
    | StartedFetching => {...state, shouldFetch: false}
    }
  }

  let (state, dispatch) = React.useReducer(update, initialState)
  React.useEffect1(() => {
    // Fetch the data from /examples and set the state when the promise resolves
    dispatch(StartedFetching)
    if state.shouldFetch {
      Fetch.fetchJson(apiUrl)
      |> Js.Promise.then_(res => {
        let result = res->Json.decode(Json.Decode.array(Decode.decodeSpacing))
        switch result {
        | Ok([spacingArea]) => dispatch(GotSpacing(spacingArea))
        | Error(err) => Js.Console.log("@TODO error handling :)")
        }
        Js.Promise.resolve()
        // let spacingArea = {margin: Margin(spacing.margin), padding: Padding(spacing.padding)}
        // NOTE: this uses an unsafe type cast, as safely parsing JSON in rescript is somewhat advanced.
      })
      // The "ignore" function is necessary because each statement is expected to return `unit` type, but Js.Promise.then return a Promise type.
      |> ignore
    }
    None
  }, [state.shouldFetch])
  let rec viewPrism = (outerSpacing: spacing, maybeInnerSpacing: option<spacing>) => {
    <div className={"viewPadding w-full"}>
      <div className={"flex-1 flex justify-center"}>
        {viewSpacingProperty(outerSpacing, Top, dispatch)}
      </div>
      <div className={"flex"}>
        <div className={"w-1/6 flex justify-center items-center"}>
          {viewSpacingProperty(outerSpacing, Left, dispatch)}
        </div>
        <div className={"flex-2 flex justify-center"}>
          {switch maybeInnerSpacing {
          | Some(Padding(padding)) => viewPrism(Padding(padding), None)
          | _ => React.null
          }}
        </div>
        <div className={"w-1/6 flex justify-center items-center"}>
          {viewSpacingProperty(outerSpacing, Right, dispatch)}
        </div>
      </div>
      <div className={"flex justify-center"}>
        {viewSpacingProperty(outerSpacing, Bottom, dispatch)}
      </div>
    </div>
  }

  let saveSpacing = spacingArea => {
    //dispatch(Saving...)
    let body: 'a = encodeSpacing(spacingArea)
    Fetch.postJson(apiUrl, ~body) |> Js.Promise.then_(res => {
      dispatch(Saved)

      Js.Promise.resolve(res)
    })
  }

  <aside className="PropertiesPanel">
    <Collapsible title="Load examples"> <ViewExamples /> </Collapsible>
    <Collapsible title="Margins & Padding">
      <form
        className={"box"}
        onSubmit={e => {
          ReactEvent.Form.preventDefault(e)
          saveSpacing(state.spacingArea)->ignore
        }}>
        {viewPrism(state.spacingArea.margin, Some(state.spacingArea.padding))}
        <button type_={"submit"}> {"save"->React.string} </button>
      </form>
    </Collapsible>
    <Collapsible title="Size"> <span> {React.string("example")} </span> </Collapsible>
  </aside>
}

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
        ->Js.Array2.map(example =>
          React.string(`Int: ${example.some_int->Js.Int.toString}, Str: ${example.some_text}`)
        )
        ->React.array
      }}
    </div>
  }
}

type theUnit = string // "%" "px"

type status = Changed(theUnit) | Default | Focused(theUnit)

type properties = {bottom: status, left: status, right: status, top: status}

type spacing = Margin(properties) | Padding(properties)

type spacingArea = {margin: spacing, padding: spacing}

type model = {initialState: spacingArea, spacingArea: spacingArea}

type field = Top | Bottom | Left | Right
type msg = GotSpacing(spacingArea) | NoOp | UpdatedSpacing(spacing) | Saved

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
  let theStatus = switch (marginOrPadding, theField) {
  | (Margin(margin), Top) => margin.top
  | (Margin(margin), Bottom) => margin.bottom
  | (Margin(margin), Left) => margin.left
  | (Margin(margin), Right) => margin.right

  | (Padding(padding), Top) => padding.top
  | (Padding(padding), Bottom) => padding.bottom
  | (Padding(padding), Left) => padding.left
  | (Padding(padding), Right) => padding.right
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
    | Default => {
        let focusProperty = _ => {
          dispatch(updateSpacing(Focused("1px")))
        }
        <button className={"default unset"} type_={"button"} onClick={focusProperty}>
          {"auto"->React.string}
        </button>
      }
    | Focused(str) => {
        let handleInput = e => {
          dispatch(updateSpacing(Focused(ReactEvent.Form.currentTarget(e)["value"])))
        }
        <input value={str} onChange={handleInput} size={5} />
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
        top: Default,
      }),
      padding: Padding({
        bottom: Default,
        left: Default,
        right: Changed("24px"),
        top: Default,
      }),
    },
    spacingArea: {
      margin: Margin({
        bottom: Focused("100pt"),
        left: Changed("1px"),
        right: Changed("24px"),
        top: Default,
      }),
      padding: Padding({
        bottom: Default,
        left: Default,
        right: Changed("24px"),
        top: Default,
      }),
    },
  }

  let statusToString = status => {
    switch status {
    | Changed(str) => str
    | Default => "auto"
    | Focused(str) => str
    }
  }

  let encodeSpacing = ({bottom, left, right, top}) =>
    {
      "bottom": statusToString(bottom),
      "left": statusToString(left),
      "right": statusToString(right),
      "top": statusToString(top),
    }

  let encodeSpacing = ({margin: Margin(margin), padding: Padding(padding)}) =>
    {"margin": encodeSpacing(margin), "padding": encodeSpacing(padding)}

  let update = (state, action) => {
    switch action {
    | GotSpacing(spacingArea) => {spacingArea: spacingArea, initialState: spacingArea}
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
    | Saved => {
        let body = state.spacingArea->encodeSpacing
        let req = Fetch.postJson(apiUrl, ~body)

        let _ = Js.Console.log(("saved", body))

        initialState
      }
    }
  }

  let (state, dispatch) = React.useReducer(update, initialState)

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

  <aside className="PropertiesPanel">
    <Collapsible title="Load examples"> <ViewExamples /> </Collapsible>
    <Collapsible title="Margins & Padding">
      <form
        className={"box"}
        onSubmit={e => {
          ReactEvent.Form.preventDefault(e)
          dispatch(Saved)
        }}>
        {viewPrism(state.spacingArea.margin, Some(state.spacingArea.padding))}
        <button> {"save"->React.string} </button>
      </form>
    </Collapsible>
    <Collapsible title="Size"> <span> {React.string("example")} </span> </Collapsible>
  </aside>
}

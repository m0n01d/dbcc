%raw(`require("./PropertiesPanel.css")`)

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
      Fetch.fetchJson(`http://localhost:12346/examples`)
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

type model = {margin: spacing, padding: spacing}

type field = Top | Bottom | Left | Right
type msg = NoOp | UpdatedSpacing(spacing) | Saved

let updateFieldSpacing = (outerSpacing, theField, newValue) => {
  switch (outerSpacing, theField) {
  | (Margin(margin), Top) => UpdatedSpacing(Margin({...margin, top: newValue}))
  | (Margin(margin), Bottom) => UpdatedSpacing(Margin({...margin, bottom: newValue}))
  | (Margin(margin), Left) => UpdatedSpacing(Margin({...margin, left: newValue}))
  | (Margin(margin), Right) => UpdatedSpacing(Margin({...margin, right: newValue}))

  | (Padding(padding), Top) => UpdatedSpacing(Padding({...padding, top: newValue}))
  | (Padding(padding), Bottom) => UpdatedSpacing(Padding({...padding, bottom: newValue}))
  | (Padding(padding), Left) => UpdatedSpacing(Padding({...padding, left: newValue}))
  | (Padding(padding), Right) => UpdatedSpacing(Padding({...padding, right: newValue}))
  }
}

let viewPropertyStatus = (outerSpacing, theField, dispatch) => {
  let theStatus = switch (outerSpacing, theField) {
  | (Margin(margin), Top) => margin.top
  | (Margin(margin), Bottom) => margin.bottom
  | (Margin(margin), Left) => margin.left
  | (Margin(margin), Right) => margin.right

  | (Padding(padding), Top) => padding.top
  | (Padding(padding), Bottom) => padding.bottom
  | (Padding(padding), Left) => padding.left
  | (Padding(padding), Right) => padding.right
  }

  let update = newValue => updateFieldSpacing(outerSpacing, theField, newValue)

  <span className={"text-sm "}>
    {switch theStatus {
    | Changed(str) =>
      <button
        className={"changed unset"} type_={"button"} onClick={_ => dispatch(update(Focused(str)))}>
        {str->React.string}
      </button>
    | Default => {
        let focusProperty = _ => {
          dispatch(update(Focused("1px")))
        }
        <button className={"default unset"} type_={"button"} onClick={focusProperty}>
          {"auto"->React.string}
        </button>
      }
    | Focused(str) => {
        let handleInput = e => {
          dispatch(update(Focused(ReactEvent.Form.currentTarget(e)["value"])))
        }
        <input value={str} onChange={handleInput} size={5} />
      }
    }}
  </span>
}

@genType @genType.as("PropertiesPanel") @react.component
let make = () => {
  let initialState = {
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
  }

  let update = (state, action) => {
    switch action {
    | NoOp => initialState
    | UpdatedSpacing(newSpacing) =>
      switch newSpacing {
      | Margin(m) => {...state, margin: Margin(m)}
      | Padding(p) => {...state, padding: Padding(p)}
      }
    | Saved => initialState
    }
  }

  let (state, dispatch) = React.useReducer(update, initialState)

  let rec viewPrism = (outerSpacing: spacing, maybeInnerSpacing: option<spacing>) => {
    <form className={"viewPadding w-full"} onSubmit={_ => dispatch(Saved)}>
      <div className={"flex-1 flex justify-center"}>
        {viewPropertyStatus(outerSpacing, Top, dispatch)}
      </div>
      <div className={"flex"}>
        <div className={"w-1/6 flex justify-center items-center"}>
          {viewPropertyStatus(outerSpacing, Left, dispatch)}
        </div>
        <div className={"flex-2 flex justify-center"}>
          {switch maybeInnerSpacing {
          | Some(Padding(padding)) => viewPrism(Padding(padding), None)
          | _ => React.null
          }}
        </div>
        <div className={"w-1/6 flex justify-center items-center"}>
          {viewPropertyStatus(outerSpacing, Right, dispatch)}
        </div>
      </div>
      <div className={"flex justify-center"}>
        {viewPropertyStatus(outerSpacing, Bottom, dispatch)}
      </div>
    </form>
  }

  <aside className="PropertiesPanel">
    <Collapsible title="Load examples"> <ViewExamples /> </Collapsible>
    <Collapsible title="Margins & Padding">
      <div className={"box"}> {viewPrism(state.margin, Some(state.padding))} </div>
    </Collapsible>
    <Collapsible title="Size"> <span> {React.string("example")} </span> </Collapsible>
  </aside>
}

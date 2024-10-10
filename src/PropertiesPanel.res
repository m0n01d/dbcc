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

type margin = Margin(properties)
type padding = Padding(properties)

type model = {margin: margin, padding: padding}

type field = Top | Bottom | Left | Right

let statusToString = s =>
  switch s {
  | Changed(str) => str
  | Default => "auto"
  | Focused(str) => str ++ "focused"
  }

type msg<'a> = NoOp | UpdatedPadding(padding)
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
    | UpdatedPadding(padding) => {...state, padding: padding}
    }
  }
  let (state, dispatch) = React.useReducer(update, initialState)

  let viewPadding = padding => {
    let viewPropertyStatus = theField => {
      let theStatus = switch theField {
      | Top => padding.top
      | Bottom => padding.bottom
      | Left => padding.left
      | Right => padding.right
      }

      <div className={"text-sm"}>
        {switch theStatus {
        | Changed(str) =>
          <p className={"underline decoration-dotted decoration-yellow-300"}>
            {str->React.string}
          </p>
        | Default => {
            let focusProperty = _ => {
              let focused = Focused("1px")
              let p = switch theField {
              | Top => {...padding, top: focused}
              | Bottom => {...padding, bottom: focused}
              | Left => {...padding, left: focused}
              | Right => {...padding, right: focused}
              }
              let newPadding = Padding(p)

              dispatch(UpdatedPadding(newPadding))
            }
            <button className={"unset"} onClick={focusProperty}> {"auto"->React.string} </button>
          }
        | Focused(str) => {
            let handleInput = e => {
              let focused = Focused(ReactEvent.Form.currentTarget(e)["value"])
              let p = switch theField {
              | Top => {...padding, top: focused}
              | Bottom => {...padding, bottom: focused}
              | Left => {...padding, left: focused}
              | Right => {...padding, right: focused}
              }
              let newPadding = Padding(p)

              dispatch(UpdatedPadding(newPadding))
            }
            <input value={str} onChange={handleInput} />
          }
        }}
      </div>
    }
    <div className={"viewPadding w-full"}>
      <div className={"flex-1 flex justify-center"}> {viewPropertyStatus(Top)} </div>
      <div className={"flex"}>
        <div className={"flex-1 flex justify-center"}> {viewPropertyStatus(Left)} </div>
        <div className={"flex-2 flex justify-center"} />
        <div className={"flex-1 flex justify-center"}> {viewPropertyStatus(Right)} </div>
      </div>
      <div className={"flex justify-center"}> {viewPropertyStatus(Bottom)} </div>
    </div>
  }
  let viewMarginAndPadding = (Margin(margin): margin, Padding(padding): padding) => {
    let viewPropertyStatus = _ => <> </>
    <div className={"viewMargin"}>
      <div className={"flex-1 flex justify-center"}> {viewPropertyStatus(margin.top)} </div>
      <div className={"flex"}>
        <div className={"w-1/6 flex items-center justify-center"}>
          {viewPropertyStatus(margin.left)}
        </div>
        <div className={"flex-2 flex items-center justify-center"}> {viewPadding(padding)} </div>
        <div className={"w-1/6 flex items-center justify-center"}>
          {viewPropertyStatus(margin.right)}
        </div>
      </div>
      <div className={"flex justify-center"}> {viewPropertyStatus(margin.bottom)} </div>
    </div>
  }

  <aside className="PropertiesPanel">
    <Collapsible title="Load examples"> <ViewExamples /> </Collapsible>
    <Collapsible title="Margins & Padding">
      <div className={"box"}> {viewMarginAndPadding(state.margin, state.padding)} </div>
    </Collapsible>
    <Collapsible title="Size"> <span> {React.string("example")} </span> </Collapsible>
  </aside>
}

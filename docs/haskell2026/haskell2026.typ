#import "@preview/touying:0.7.4": *
#import themes.simple: *
#import "@preview/cetz:0.5.2" as cetz
#import "@preview/fletcher:0.5.8" as fletcher

#show: simple-theme.with(
  aspect-ratio: "16-9",
  config-info(
    title: [Xeus-Haskell],
    subtitle: [Interactive Haskell Computing in the Browser],
    author: [Masaya Taniguchi · RIKEN AIP],
    date: [Haskell 2026 · ICFP 2026],
  ),
)

#set text(font: "Arial", size: 22pt)
#set par(leading: 0.65em)
#show raw: set text(font: "DejaVu Sans Mono", size: 18pt)

#let source-link(label, url) = align(center)[
  #text(size: 17pt)[
    #label #text(font: "DejaVu Sans Mono", size: 16pt)[#link(url)[#url]]
  ]
]

#let url-bubble(label, url, bubble-fill: rgb("#E7F3F6")) = cetz.canvas(length: 1cm, {
  import cetz.draw: *
  line(
    (1.25, .28), (.82, -.08), (1.92, .28),
    close: true,
    fill: bubble-fill,
    stroke: 1.1pt + rgb("#3F7083"),
  )
  rect(
    (.2, .22), (21.5, 1.35),
    radius: .25,
    fill: bubble-fill,
    stroke: 1.1pt + rgb("#3F7083"),
  )
  content(
    (10.85, .78),
    [#text(size: 16pt)[#text(weight: "bold", fill: rgb("#3F7083"))[#label] #text(font: "DejaVu Sans Mono", size: 15pt)[#link(url)[#url]]]],
  )
})

#let background-logo(path, height: 3.6cm, dx: .8cm, dy: -.65cm) = place(
  top + right,
  dx: dx,
  dy: dy,
)[
  #image(path, height: height, fit: "contain")
]

#let diagram-ink = rgb("#171717")
#let diagram-mid = rgb("#777777")
#let diagram-fog = rgb("#F1F1F1")

#title-slide[
  #background-logo("assets/logos/jupyter-xeus-bg.png", height: 3.2cm, dx: -7cm)
  #background-logo("assets/logos/icfp2026-bg.png", height: 4.5cm, dy: -.55cm)

  #place(bottom + center, dy: .3cm)[
    #stack(
      dir: ttb,
      spacing: .12cm,
      url-bubble("Demo", "https://jupyter-xeus.github.io/xeus-haskell"),
      url-bubble("GitHub", "https://github.com/jupyter-xeus/xeus-haskell", bubble-fill: rgb("#F1F1F1")),
    )
  ]

  #align(center + horizon)[
    #text(size: 50pt, weight: "bold")[Xeus-Haskell]

    #text(size: 25pt)[Interactive Haskell Computing in the Browser]

    #text(size: 20pt)[Masaya Taniguchi · RIKEN AIP]

    #text(size: 18pt)[Haskell 2026 · ICFP 2026]
  ]
]

== Origin

#background-logo("assets/logos/haskell-bg.png", height: 3.5cm)

#v(.7cm)

#grid(
  columns: (.9fr, 1.1fr),
  column-gutter: 1.2cm,
  align: left,
  [
    #text(size: 23pt, weight: "bold")[Research]

    #set text(size: 18pt)
    *Field* · Formal grammar researcher

    *Method* · Categorical formalization

    *Medium* · Executable Haskell

    *Goal* · Reproducible experiments
  ],
  [
    #text(size: 23pt, weight: "bold")[From need to project]

    #set text(size: 18pt)
    *Need* · A small Haskell laboratory

    *Build* · A browser kernel

    *Community* · DataHaskell → Jupyter-Xeus

    *GSoC* · 2026 mentor
  ],
)

#v(.55cm)

#align(center)[
  #text(size: 22pt, weight: "bold")[A research need became open-source infrastructure.]
]

== Installation Barrier

I wanted interactive notes for *Category Theory for Programmers*.

```sh
$ brew install ghcup python3 zeromq libmagic cairo pkg-config pango
$ pip3 install jupyter --user
$ ghcup install ghc recommended
$ ghcup install cabal recommended
$ cabal install ihaskell
$ ihaskell install --prefix="$HOME/.local/"
```

#align(center)[
  #rotate(-1deg)[
    #cetz.canvas(length: 1cm, {
      import cetz.draw: *
      line(
        (1.35, .42), (.82, -.08), (2.15, .42),
        close: true,
        fill: rgb("#E7F3F6"),
        stroke: 1.2pt + rgb("#3F7083"),
      )
      rect(
        (.35, .35), (15.6, 1.72),
        radius: .28,
        fill: rgb("#E7F3F6"),
        stroke: 1.2pt + rgb("#3F7083"),
      )
      content(
        (7.97, 1.03),
        [#text(size: 20pt, weight: "bold")[I do not have the brain for this on a Sunday.]],
      )
    })
  ]
]

== Motivation

#align(center)[
  #text(size: 28pt)[Python learners open Colab and start experimenting.]

  #text(size: 28pt)[Haskell should be equally immediate.]

  #text(size: 25pt, weight: "bold")[No notebook server. No local toolchain. Just a URL.]
]

== Use Cases

#text(size: 14pt, fill: diagram-mid, weight: "bold")[THREE BARRIERS]

#align(center)[
  #set text(size: 19pt)
  #table(
    columns: (4.8cm, 1fr),
    inset: 9pt,
    stroke: .7pt + diagram-mid,
    fill: (x, y) => if x == 0 { diagram-fog } else { white },
    [*Teaching*], [Setup becomes the first assignment.],
    [*Hosting*], [Thirty students contend for one server.],
    [*Live demo*], [Untrusted code runs on your infrastructure.],
  )
]

#text(size: 14pt, fill: diagram-mid, weight: "bold")[OUR SOLUTION]

#align(center)[
  #set text(size: 19pt)
  #table(
    columns: (4.8cm, 1fr),
    inset: 10pt,
    stroke: 1.1pt + diagram-ink,
    fill: (x, y) => if x == 0 { diagram-ink } else { diagram-fog },
    [#text(fill: white, weight: "bold")[Static files]],
    [Users open a browser; providers serve static files.],
  )
]

== Demo Track

The accepted demonstration focuses on the MicroHs kernel.

#align(center)[
  #set text(size: 19pt)
  #table(
    columns: (1.4fr, 1fr, 1fr),
    inset: 10pt,
    stroke: .7pt + diagram-mid,
    fill: (x, y) => if y == 0 { diagram-fog } else { white },
    align: center + horizon,
    [*Kernel*], [*Backend*], [*JupyterLite*],
    [`xhaskell-mhs`], [MicroHs], [Yes],
  )
]

Browser execution · persistence · Jupyter · rich display

#source-link("Demo:", "https://jupyter-xeus.github.io/xeus-haskell")

== MicroHs Demo

#text(size: 17pt, fill: rgb("#3F7083"), weight: "bold")[CELL 1 · DECLARATIONS]

```haskell
data Tree a = Leaf a | Branch (Tree a) (Tree a)
size (Leaf _) = 1
size (Branch l r) = size l + size r
```

#text(size: 17pt, fill: rgb("#3F7083"), weight: "bold")[CELL 2 · EXPRESSION]

```haskell
size (Branch (Leaf 'a') (Leaf 'b'))
```

#text(size: 17pt, fill: rgb("#3F7083"), weight: "bold")[OUTPUT · #text(font: "DejaVu Sans Mono", fill: black)[2]]

#parbreak()
#align(center)[#text(size: 22pt, fill: rgb("#3F7083"), weight: "bold")[One persistent browser session]]

== MicroHs

*Extended Haskell subset with a combinator runtime.*

- Minimal-dependency runtime, including microcontrollers
- Self-hosting compiler
- Bootstrap from combinators with a C compiler
- JavaScript and Wasm targets through Emscripten

#source-link("Source:", "https://microhs.org/")

== Bonus: GHC Wasm

#background-logo("assets/logos/webassembly-bg.png", height: 3.5cm)
#background-logo("assets/logos/haskell-bg.png", height: 3.3cm, dx: -3.7cm, dy: -.55cm)

Everything so far is the accepted MicroHs demonstration.

#align(center)[
  #set text(size: 19pt)
  #table(
    columns: (1.4fr, 1fr, 1fr),
    inset: 10pt,
    stroke: .7pt + diagram-mid,
    fill: (x, y) => if y == 0 { diagram-fog } else { white },
    align: center + horizon,
    [*Kernel*], [*Backend*], [*JupyterLite*],
    [`xhaskell-ghc`], [GHC Wasm], [Yes],
  )
]

- GHC runs entirely on the client.
- One session preserves declarations, imports, IO state, and `it`.
- Notebook users get GHC/GHCi language behavior.

*New work beyond the submitted demonstration.*

== GHC in the Browser

*Frontend live-coding via GHCi* · April 2025

- `-fghci-browser`: host GHCi → browser Wasm
- *ghc-in-browser*: GHC compiled to Wasm
- Client-side compiler, type checker, and bytecode interpreter
- In-browser GHC filesystem
- No server-side compiler

#source-link("Source:", "https://www.tweag.io/blog/2025-04-17-wasm-ghci-browser/")
#source-link("Source:", "https://haskell-wasm.github.io/ghc-in-browser/")

== GHC Demo

```haskell
In [1]: :set -XTypeApplications
In [2]: :t fmap @Maybe
         fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b
In [3]: fmap @Maybe (+1) (Just 41)
Out[3]: Just 42
```

#background-logo("assets/logos/gsoc-bg.png", height: 3.8cm)

GSoC 2026: persistent GHC · JSPI bridge · WASI assets · browser tests

*Architecture and implementation: Masaya Taniguchi*

== JupyterLite

#background-logo("assets/logos/jupyter-bg.png", height: 4cm)

*JupyterLab as a static, serverless web application.*

#align(center)[
  #set text(size: 11.5pt)
  #show raw: set text(size: 10.5pt)
  #scale(145%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *
    // Static files arrive at a browser window.
    rect((0, .65), (2.9, 3.45), radius: .14, fill: diagram-fog, stroke: .8pt + diagram-mid)
    rect((.2, .9), (3.1, 3.7), radius: .14, fill: white, stroke: .8pt + diagram-mid)
    rect((.4, 1.15), (3.5, 3.95), radius: .14, fill: white, stroke: 1.1pt + diagram-ink)
    content((1.95, 3.18), [#text(weight: "bold")[Static]])
    content((1.95, 2.58), [HTML])
    content((1.95, 1.98), [Wasm])
    content((1.95, 1.42), [notebooks])

    line((3.5, 2.55), (4.55, 2.55), stroke: 1.2pt + diagram-ink, mark: (end: ">"))

    // The browser is the execution boundary.
    rect((4.55, .25), (15.9, 4.5), radius: .25, fill: white, stroke: 1.35pt + diagram-ink)
    line((4.55, 3.72), (15.9, 3.72), stroke: .9pt + diagram-mid)
    circle((4.85, 3.98), radius: .08, fill: diagram-ink, stroke: none)
    circle((5.13, 3.98), radius: .08, fill: diagram-mid, stroke: none)
    circle((5.41, 3.98), radius: .08, fill: diagram-mid, stroke: none)
    content((5.77, 3.98), [Browser], anchor: "west")

    rect((5.05, .78), (9.2, 3.28), radius: .2, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((7.12, 2.48), [#text(weight: "bold")[JupyterLite]])
    content((7.12, 1.58), [UI · main thread])

    rect((10.6, .78), (15.1, 3.28), radius: .2, fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((12.85, 2.48), [#text(fill: white, weight: "bold")[Web Worker]])
    content((12.85, 1.58), [#text(fill: white)[Haskell · Wasm]])

    line((9.2, 2.38), (10.6, 2.38), stroke: 1.1pt + diagram-ink, mark: (end: ">"))
    line((10.6, 1.65), (9.2, 1.65), stroke: 1.1pt + diagram-ink, mark: (end: ">"))
  })
  ]
]

#source-link("Source:", "https://jupyterlite.readthedocs.io/en/stable/")

== Xeus

#background-logo("assets/logos/jupyter-xeus-bg.png", height: 2.8cm)

*xeus is a C++ implementation of the Jupyter kernel protocol.*

#align(center)[
  #set text(size: 13pt)
  #scale(120%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *
    // Connectors first: clients converge, runtimes fan out.
    line((2.1, 5.0), (6.4, 3.25), stroke: 1.3pt + diagram-ink, mark: (end: ">"))
    line((7.1, 5.25), (7.5, 4.03), stroke: 1.3pt + diagram-ink, mark: (end: ">"))
    line((12.7, 5.0), (8.85, 3.23), stroke: 1.3pt + diagram-ink, mark: (end: ">"))
    line((6.4, 2.27), (2.1, .55), stroke: 1.3pt + diagram-ink, mark: (end: ">"))
    line((7.5, 1.47), (7.1, .55), stroke: 1.3pt + diagram-ink, mark: (end: ">"))
    line((8.85, 2.27), (12.7, .55), stroke: 1.3pt + diagram-ink, mark: (end: ">"))

    content((7.4, 6.85), [#text(size: 11pt, fill: diagram-mid, weight: "bold")[JUPYTER CLIENTS]])
    content((7.4, -1.4), [#text(size: 11pt, fill: diagram-mid, weight: "bold")[HASKELL RUNTIMES]])

    rect((.55, 5.0), (3.65, 6.2), radius: .2, fill: white, stroke: 1pt + diagram-ink)
    content((2.1, 5.6), [Notebook])
    rect((5.5, 5.25), (8.7, 6.45), radius: .2, fill: white, stroke: 1pt + diagram-ink)
    content((7.1, 5.85), [Console])
    rect((10.9, 5.0), (14.5, 6.2), radius: .2, fill: white, stroke: 1pt + diagram-ink)
    content((12.7, 5.6), [Web application])

    circle((7.65, 2.75), radius: 1.05, fill: diagram-ink, stroke: 1.2pt + diagram-ink)
    circle((7.65, 2.75), radius: 1.28, fill: none, stroke: .8pt + diagram-mid)
    content((7.65, 2.75), [#text(fill: white, size: 16pt, weight: "bold")[xeus]])

    rect((.65, -.65), (3.55, .4), radius: .2, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((2.1, -.125), [MicroHs])
    rect((5.5, -.85), (8.7, .2), radius: .2, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((7.1, -.325), [GHC Wasm])
    rect((10.9, -.65), (14.5, .4), radius: .2, fill: white, stroke: 1.2pt + diagram-ink)
    content((12.7, -.125), [Your runtime])
  })
  ]
]

== System Architecture

#align(center)[
  #set text(size: 14pt)
  #scale(130%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *
    // One browser surface rests on one shared protocol layer.
    // Connectors occupy their own vertical bands.
    line((8, 5.15), (8, 4.82), stroke: 1.3pt + diagram-ink, mark: (end: ">"))
    line((8, 3.45), (8, 2.85), stroke: 1.3pt + diagram-ink)
    line((4.2, 2.85), (11.8, 2.85), stroke: 1.3pt + diagram-ink)
    line((4.2, 2.85), (4.2, 2.25), stroke: 1.3pt + diagram-ink, mark: (end: ">"))
    line((11.8, 2.85), (11.8, 2.25), stroke: 1.3pt + diagram-ink, mark: (end: ">"))

    rect((1.2, 5.15), (14.8, 7.2), radius: .25, fill: white, stroke: 1.2pt + diagram-ink)
    line((1.2, 6.65), (14.8, 6.65), stroke: .8pt + diagram-mid)
    circle((1.62, 6.92), radius: .07, fill: diagram-ink, stroke: none)
    circle((1.87, 6.92), radius: .07, fill: diagram-mid, stroke: none)
    content((2.25, 6.92), [Browser], anchor: "west")
    content((8, 5.82), [#text(size: 17pt, weight: "bold")[One notebook frontend]])

    rect((3, 3.45), (13, 4.65), radius: .18, fill: diagram-ink, stroke: 1.1pt + diagram-ink)
    content((8, 4.05), [#text(fill: white, size: 16pt, weight: "bold")[Shared Jupyter layer]])

    rect((1.4, .1), (7, 2.1), radius: .22, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((4.2, 1.48), [#text(size: 16pt, weight: "bold")[MicroHs]])
    content((4.2, .62), [direct API])
    rect((9, .1), (14.6, 2.1), radius: .22, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((11.8, 1.48), [#text(size: 16pt, weight: "bold")[GHC Wasm]])
    content((11.8, .62), [JSPI · JSON])
  })
  ]
]

Both demos share one notebook frontend.

== Backend Trade-offs

#align(center)[
  #set text(size: 18pt)
  #table(
    columns: (3.2cm, 1fr, 1fr),
    inset: 9pt,
    stroke: .7pt + diagram-mid,
    fill: (x, y) => if y == 0 or x == 0 { diagram-fog } else { white },
    align: (x, y) => if x == 0 { left + horizon } else { center + horizon },
    [], [*MicroHs*], [*GHC Wasm*],
    [*Goal*], [Small and portable Haskell], [GHC/GHCi compatibility],
    [*Runtime*], [Embedded compiler API], [Persistent browser GHC session],
    [*Where*], [Native and browser], [Browser],
  )
]

#align(center)[*MicroHs: portability · GHC Wasm: compatibility*]

== MicroHs Backend

#align(center)[
  #set text(size: 14pt)
  #scale(160%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *
    // A request enters an embedded runtime and updates one durable context.
    rect((.1, 1.35), (3.35, 3.3), radius: .22, fill: white, stroke: 1pt + diagram-ink)
    content((1.72, 2.75), [#text(size: 13pt, weight: "bold")[Cell request]])
    content((1.72, 1.85), [#text(size: 12pt)[decls + expr]])
    line((3.35, 2.32), (4.25, 2.32), stroke: 1.2pt + diagram-ink, mark: (end: ">"))

    rect((4.25, .25), (12.55, 4.45), radius: .3, fill: diagram-fog, stroke: 1.3pt + diagram-ink)
    content((4.75, 4.05), [#text(size: 11pt, fill: diagram-mid, weight: "bold")[EMBEDDED MICROHS]], anchor: "west")
    arc((9.59, 3.18), radius: 1.45, start: 35deg, stop: 325deg, stroke: 1.25pt + diagram-ink, mark: (end: ">"))
    circle((8.4, 2.35), radius: 1.02, fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((8.4, 2.35), [#text(fill: white, size: 16pt, weight: "bold")[Repl]])

    rect((4.75, .48), (6.65, 1.3), radius: .15, fill: white, stroke: .8pt + diagram-mid)
    content((5.7, .89), [native])
    rect((10.15, .48), (12.05, 1.3), radius: .15, fill: white, stroke: .8pt + diagram-mid)
    content((11.1, .89), [Wasm])

    line((12.55, 2.32), (13.65, 2.32), stroke: 1.2pt + diagram-ink, mark: (end: ">"))
    rect((13.35, 1.35), (15.9, 3.3), radius: .22, fill: white, stroke: 1pt + diagram-ink)
    content((14.62, 2.75), [#text(weight: "bold")[Result]])
    content((14.62, 1.85), [#text(size: 12pt)[value / IO]])
  })
  ]
]

#align(center)[Direct API · persistent state · native and Wasm]

== MicroHs Expansion

*Cell*

```haskell
square x = x * x
map square [1..5]
```

*Generated execution module*

```haskell
module Inline where
import Prelude
import System.IO.PrintOrRun
square x = x * x
runResult = _printOrRun (map square [1..5])
```

== GHC Wasm Backend

#align(center)[
  #set text(size: 14pt)
  #scale(145%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *
    rect((.35, .25), (15.65, 4.65), radius: .28, fill: white, stroke: 1.25pt + diagram-ink)
    line((.35, 4.02), (15.65, 4.02), stroke: .8pt + diagram-mid)
    circle((.78, 4.34), radius: .07, fill: diagram-ink, stroke: none)
    circle((1.03, 4.34), radius: .07, fill: diagram-mid, stroke: none)
    content((1.4, 4.34), [Browser], anchor: "west")

    // Jupyter and GHC occupy separate runtime islands.
    rect((1.1, .85), (6.2, 3.5), radius: .23, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((3.65, 2.8), [#text(weight: "bold")[JupyterLite]])
    rect((1.95, 1.08), (5.35, 2.23), radius: .17, fill: white, stroke: .9pt + diagram-mid)
    content((3.65, 1.65), [xeus-lite])

    rect((7.15, 1.02), (8.85, 3.32), radius: .28, fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((8, 2.55), [#text(fill: white, weight: "bold")[JSPI]])
    content((8, 1.75), [#text(fill: white, size: 11pt)[bridge]])

    rect((9.8, .85), (14.9, 3.5), radius: .23, fill: white, stroke: 1.15pt + diagram-ink)
    content((12.35, 2.85), [#text(size: 16pt, weight: "bold")[GHC Wasm]])
    rect((10.35, 1.08), (12.25, 2.0), radius: .15, fill: diagram-fog, stroke: .8pt + diagram-mid)
    content((11.3, 1.54), [rootfs])
    rect((12.45, 1.08), (14.35, 2.0), radius: .15, fill: diagram-fog, stroke: .8pt + diagram-mid)
    content((13.4, 1.54), [linker])

    line((6.2, 2.55), (7.15, 2.55), stroke: 1.15pt + diagram-ink, mark: (end: ">"))
    line((8.85, 2.55), (9.8, 2.55), stroke: 1.15pt + diagram-ink, mark: (end: ">"))
    line((9.8, 1.65), (8.85, 1.65), stroke: 1.05pt + diagram-ink, mark: (end: ">"))
    line((7.15, 1.65), (6.2, 1.65), stroke: 1.05pt + diagram-ink, mark: (end: ">"))
    content((8, .68), [JSON results])
  })
  ]
]

Rootfs and linker assets live in an in-memory filesystem.

One browser session preserves GHC state across cells.

== GHCi Dispatch

*Cell*

```haskell
square x = x * x
map square [1..5]
```

*Executed in one persistent GHC session*

```haskell
runDecls "square x = x * x"
execStmt "map square [1..5]" execOptions

xhaskellGhcInteractivePrint value =
  putStrLn resultMarker >> print value
```

== Integration Models

#align(center)[
  #set text(size: 13pt)
  #scale(155%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *
    content((3.75, 4.8), [#text(size: 18pt, weight: "bold")[Monolithic]])
    content((12.1, 4.8), [#text(size: 18pt, weight: "bold")[Modular]])

    // Monolithic: both halves share one binary and one address space.
    rect((.35, .45), (7.15, 4.25), radius: .3, fill: diagram-fog, stroke: 1.2pt + diagram-ink)
    content((.8, 3.87), [#text(size: 11pt, fill: diagram-mid, weight: "bold")[ONE KERNEL]], anchor: "west")
    rect((1.05, 2.35), (6.45, 3.5), radius: .2, fill: white, stroke: 1pt + diagram-ink)
    content((3.75, 2.92), [xeus])
    rect((1.05, .82), (6.45, 1.97), radius: .2, fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((3.75, 1.4), [#text(fill: white, weight: "bold")[MicroHs interpreter]])
    line((3.75, 2.35), (3.75, 1.97), stroke: 1.3pt + diagram-ink, mark: (end: ">"))
    content((3.75, .18), [one binary · native streams])

    // Modular: a narrow JS boundary keeps the runtime independent.
    rect((8.3, 1.0), (11.25, 3.7), radius: .24, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((9.77, 2.78), [#text(weight: "bold")[xeus-lite]])
    content((9.77, 1.82), [Jupyter])
    rect((12.95, 1.0), (15.9, 3.7), radius: .24, fill: white, stroke: 1.15pt + diagram-ink)
    content((14.42, 2.78), [#text(weight: "bold")[GHC Wasm]])
    content((14.42, 1.82), [WASI runtime])
    line((11.15, 2.35), (13.05, 2.35), stroke: 1.1pt + diagram-ink, mark: (start: "<", end: ">"))
    circle((12.1, 2.35), radius: .55, fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((12.1, 2.35), [#text(fill: white, size: 11pt, weight: "bold")[JS]])
    content((12.1, 1.52), [JSON])
    content((12.1, .18), [independent runtime · JSON boundary])
  })
  ]
]

#align(center)[Two runtimes expose two reusable integration patterns.]

== Interactive Semantics

#align(center)[
  #set text(size: 18pt)
  #table(
    columns: (4.3cm, 6cm, 11.5cm, 6cm),
    inset: 11pt,
    stroke: .7pt + diagram-mid,
    fill: (x, y) => if y == 0 { diagram-ink } else if x == 0 { diagram-fog } else { white },
    align: (x, y) => if x == 3 { left + horizon } else { center + horizon },
    [#text(fill: white, weight: "bold")[Case]],
    [#text(fill: white, weight: "bold")[Example]],
    [#text(fill: white, weight: "bold")[Judgment]],
    [#text(fill: white, weight: "bold")[State]],
    [*Declaration*],
    [#text(font: "DejaVu Sans Mono", size: 14pt)[square x=x\*x]],
    [#text(font: "New Computer Modern Math", size: 18pt)[Γₙ ⊢ d ⇒ Γₙ₊₁]],
    [Γ grows.],
    [*Expression*],
    [#text(font: "DejaVu Sans Mono", size: 15pt)[square 5]],
    [#text(font: "New Computer Modern Math", size: 18pt)[Γₙ ⊢ e : τ · e ⇓ v]],
    [Γ stays.],
    [*IO action*],
    [#text(font: "DejaVu Sans Mono", size: 14pt)[print \$ square 5]],
    [#text(font: "New Computer Modern Math", size: 17pt)[Γₙ ⊢ m : IO τ · ⟨m,σ⟩ ⇓ ⟨v,σ′⟩]],
    [σ may change.],
  )
]

#align(center)[#text(size: 18pt)[Read left to right: cell · typing judgment · state transition]]

#align(center)[#text(size: 18pt)[Γ: typed environment · σ: effect state · ⇓: evaluates to]]

#align(center)[#text(size: 20pt, weight: "bold")[Haskell separates environment growth, pure evaluation, and explicit effects.]]

== Rich Display

```haskell
import XHaskell.Display

putStr $ show $ DisplayData "text/latex"
  "\\[\\int_0^1 x^2 dx = \\frac{1}{3}\\]"
```

$ integral_0^1 x^2 dif x = 1 / 3 $

A small framed MIME protocol becomes Jupyter `display_data`.

```text
<STX><MIME type><US><content><ETX>
```

== Acknowledgements

#background-logo("assets/logos/gsoc-bg.png", height: 3.5cm)

- Google Summer of Code 2026 · Haskell.org
  - Arman Sanjay Choudhary for early GHC/Wasm exploration
- ACM SIGPLAN Professional Activities Committee
- Haskell 2026 organizers and community
- JSPS Grant-in-Aid for Early-Career Scientists (24K16077)
- HUMAI Foundation (A)

== Conclusion

#align(center)[
  #text(size: 29pt)[Haskell notebooks can run entirely in the browser.]

  #text(size: 26pt, weight: "bold")[No notebook server. No local toolchain. Just a URL.]

  One Jupyter layer · two complementary backends

  MicroHs for portability · GHC Wasm for compatibility

  #source-link("Demo:", "https://jupyter-xeus.github.io/xeus-haskell")

  #source-link("Source:", "https://github.com/jupyter-xeus/xeus-haskell")
]

== Collaboration

*Building a Haskell implementation? Let us bring it to Jupyter.*

#align(center)[
  #set text(size: 14pt)
  #scale(125%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *
    // Jupyter exposes a socket; runtimes provide interchangeable adapters.
    rect((.35, .35), (7.7, 4.65), radius: .28, fill: diagram-fog, stroke: 1.2pt + diagram-ink)
    content((.8, 4.25), [#text(size: 11pt, fill: diagram-mid, weight: "bold")[JUPYTERLITE]], anchor: "west")
    rect((1.1, 1.25), (5.05, 3.75), radius: .22, fill: white, stroke: 1pt + diagram-ink)
    content((3.08, 2.85), [#text(weight: "bold")[Notebook UI]])
    content((3.08, 2.05), [messages])
    rect((5.05, 1.25), (7.7, 3.75), radius: (west: .2), fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((6.37, 2.85), [#text(fill: white, weight: "bold")[xeus]])
    content((6.37, 2.05), [#text(fill: white, size: 11pt)[runtime API]])
    circle((7.7, 2.5), radius: .48, fill: white, stroke: 1.25pt + diagram-ink)
    circle((7.7, 2.5), radius: .18, fill: diagram-ink, stroke: none)

    line((9.3, 3.75), (8.08, 2.78), stroke: .9pt + diagram-mid, mark: (end: ">"))
    line((12.5, 3.75), (8.12, 2.62), stroke: .9pt + diagram-mid, mark: (end: ">"))
    rect((8.75, 3.32), (11.65, 4.42), radius: .18, fill: diagram-fog, stroke: .9pt + diagram-mid)
    content((10.2, 3.87), [MicroHs · ✓])
    rect((11.95, 3.32), (15.25, 4.42), radius: .18, fill: diagram-fog, stroke: .9pt + diagram-mid)
    content((13.6, 3.87), [GHC Wasm · ✓])

    line((9.2, 1.38), (8.08, 2.22), stroke: 1.4pt + diagram-ink, mark: (end: ">"))
    rect((9.05, .42), (15.5, 1.88), radius: .24, fill: white, stroke: 1.35pt + diagram-ink)
    content((12.27, 1.32), [#text(size: 16pt, weight: "bold")[Your Haskell runtime]])
    content((12.27, .72), [#text(size: 11pt, fill: diagram-mid, weight: "bold")[PLUG IN HERE]])
  })
  ]
]

*You bring the runtime; xeus brings Jupyter.*

#source-link("Collaborate:", "https://github.com/jupyter-xeus/xeus-haskell")

== MicroHs Build

#text(size: 14pt, fill: diagram-mid, weight: "bold")[APPENDIX · BUILD RECIPE]

#align(center)[
  #set text(size: 14pt)
  #scale(132%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *

    // The native compiler generates portable C; four inputs then converge.
    line((3.65, 4.35), (5.2, 4.35), stroke: 1.2pt + diagram-ink, mark: (end: ">"))
    line((9.2, 4.35), (10.75, 4.35), stroke: 1.2pt + diagram-ink, mark: (end: ">"))
    line((12.75, 3.45), (12.75, 2.72), stroke: 1.2pt + diagram-ink, mark: (end: ">"))
    line((1.65, 1.75), (6.3, 1.75), stroke: 1.1pt + diagram-mid, mark: (end: ">"))
    line((4.6, 1.75), (6.3, 1.75), stroke: 1.1pt + diagram-mid, mark: (end: ">"))
    line((9.6, 1.75), (9.1, 1.75), stroke: 1.1pt + diagram-mid, mark: (end: ">"))
    line((13.75, 1.75), (9.1, 1.75), stroke: 1.1pt + diagram-mid, mark: (end: ">"))
    line((9.1, 1.75), (10.4, .72), stroke: 1.2pt + diagram-ink, mark: (end: ">"))

    rect((.45, 3.45), (3.65, 5.25), radius: .22, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((2.05, 4.72), [#text(weight: "bold")[Native build]])
    content((2.05, 4.05), [C + make])

    rect((5.2, 3.45), (9.2, 5.25), radius: .22, fill: white, stroke: 1.1pt + diagram-ink)
    content((7.2, 4.72), [#text(weight: "bold")[Native `mhs`]])
    content((7.2, 4.05), [self-hosted])

    rect((10.75, 3.45), (14.75, 5.25), radius: .22, fill: white, stroke: 1.1pt + diagram-ink)
    content((12.75, 4.72), [#text(weight: "bold")[Haskell → C]])
    content((12.75, 4.05), [#text(font: "DejaVu Sans Mono", size: 10.5pt)[mhs -c Repl]])

    rect((.4, 1.18), (2.9, 2.3), radius: .18, fill: diagram-fog, stroke: .9pt + diagram-mid)
    content((1.65, 1.74), [#text(font: "DejaVu Sans Mono", size: 10.5pt)[eval.c]])
    rect((3.4, 1.18), (5.8, 2.3), radius: .18, fill: diagram-fog, stroke: .9pt + diagram-mid)
    content((4.6, 1.74), [#text(font: "DejaVu Sans Mono", size: 10.5pt)[Repl.c]])
    rect((9.6, 1.18), (12, 2.3), radius: .18, fill: diagram-fog, stroke: .9pt + diagram-mid)
    content((10.8, 1.74), [C++ bridge])
    rect((12.3, 1.18), (15.2, 2.3), radius: .18, fill: diagram-fog, stroke: .9pt + diagram-mid)
    content((13.75, 1.74), [#text(font: "DejaVu Sans Mono", size: 10.5pt)[xeus-lite]])

    rect((6.3, 1.05), (9.1, 2.45), radius: .22, fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((7.7, 1.95), [#text(fill: white, weight: "bold")[Emscripten]])
    content((7.7, 1.38), [#text(fill: white, size: 11pt)[compile + link]])

    rect((10.4, .18), (15.55, 1.08), radius: .18, fill: white, stroke: 1.2pt + diagram-ink)
    content((12.97, .63), [#text(font: "DejaVu Sans Mono", size: 10.5pt, weight: "bold")[xhaskell-mhs.wasm]])
  })
  ]
]

#align(center)[#text(size: 20pt, weight: "bold")[xeus calls MicroHs directly as an in-process library.]]

== GHC Wasm Build

#text(size: 14pt, fill: diagram-mid, weight: "bold")[APPENDIX · BUILD RECIPE]

#align(center)[
  #set text(size: 11.5pt)
  #show raw: set text(size: 10.5pt)
  #scale(128%, reflow: true)[
  #cetz.canvas(length: 1cm, {
    import cetz.draw: *

    // The GHC runtime and the xeus kernel remain separate build products.
    line((3.9, 4.55), (5.05, 4.55), stroke: 1.2pt + diagram-ink, mark: (end: ">"))
    line((7.25, 3.7), (5.45, 2.78), stroke: 1.1pt + diagram-mid, mark: (end: ">"))
    line((7.25, 3.7), (9.05, 2.78), stroke: 1.1pt + diagram-mid, mark: (end: ">"))
    line((5.45, 1.48), (5.45, .88), stroke: 1.1pt + diagram-ink, mark: (end: ">"))
    line((9.05, 1.48), (9.05, .88), stroke: 1.1pt + diagram-ink, mark: (end: ">"))
    line((13.35, 3.7), (13.35, 2.78), stroke: 1.2pt + diagram-ink, mark: (end: ">"))
    line((10.9, .55), (11.75, .55), stroke: 1.3pt + diagram-ink, mark: (start: "<", end: ">"))

    rect((.35, 3.7), (3.9, 5.35), radius: .22, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((2.12, 4.78), [#text(weight: "bold")[`ghc-wasm-meta`]])
    content((2.12, 4.18), [pinned toolchain])

    rect((5.05, 3.7), (9.45, 5.35), radius: .22, fill: white, stroke: 1.1pt + diagram-ink)
    content((7.25, 4.78), [#text(weight: "bold")[`wasm32-wasi-ghc`]])
    content((7.25, 4.18), [GHC package + dyld])

    rect((3.45, 1.48), (7.45, 2.78), radius: .2, fill: white, stroke: 1pt + diagram-ink)
    content((5.45, 2.28), [#text(weight: "bold")[GHCi runtime]])
    content((5.45, 1.78), [`Playground.hs`])

    rect((7.65, 1.48), (10.45, 2.78), radius: .2, fill: diagram-fog, stroke: 1pt + diagram-mid)
    content((9.05, 2.28), [#text(weight: "bold")[Rootfs]])
    content((9.05, 1.78), [GHC libs])

    rect((2.7, .05), (7.65, .88), radius: .17, fill: white, stroke: 1.15pt + diagram-ink)
    content((5.45, .46), [#text(weight: "bold")[GHCi runtime `.so`]])
    rect((7.75, .05), (10.9, .88), radius: .17, fill: white, stroke: 1.15pt + diagram-ink)
    content((9.32, .46), [#text(weight: "bold")[`rootfs.tar.zst`]])

    rect((11.15, 3.7), (15.55, 5.35), radius: .22, fill: diagram-fog, stroke: 1pt + diagram-ink)
    content((13.35, 4.78), [#text(weight: "bold")[Emscripten]])
    content((13.35, 4.18), [xeus-lite + C++])
    rect((11.15, 1.48), (15.55, 2.78), radius: .2, fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((13.35, 2.28), [#text(fill: white, weight: "bold")[`xhaskell-ghc.wasm`]])
    content((13.35, 1.78), [#text(fill: white, size: 11pt)[JSPI + pre-js glue]])

    circle((11.32, .55), radius: .48, fill: diagram-ink, stroke: 1pt + diagram-ink)
    content((11.32, .55), [#text(fill: white, size: 10pt, weight: "bold")[JS]])
    content((13.65, .55), [dyld loads GHC])
  })
  ]
]

#align(center)[#text(size: 20pt, weight: "bold")[Bundled together; loaded at runtime; not linked into one Wasm module.]]

== Library Bundling

#text(size: 14pt, fill: diagram-mid, weight: "bold")[APPENDIX · EMSCRIPTEN FS]

#align(center)[
  #grid(
    columns: (7cm, 1.2cm, 9cm, 1.2cm, 7cm),
    align: center + horizon,
    block(width: 100%, height: 3.2cm, inset: 12pt, radius: 10pt, fill: diagram-fog, stroke: 1pt + diagram-ink)[
      #align(center + horizon)[
        #stack(
          dir: ttb,
          spacing: .2cm,
          [#text(size: 20pt, weight: "bold")[Haskell libraries]],
          [#text(size: 17pt)[static files]],
        )
      ]
    ],
    [#text(size: 30pt)[→]],
    block(width: 100%, height: 3.2cm, inset: 12pt, radius: 10pt, fill: white, stroke: 1.2pt + diagram-ink)[
      #align(center + horizon)[
        #stack(
          dir: ttb,
          spacing: .14cm,
          [#text(size: 20pt, weight: "bold")[Emscripten FS]],
          [#text(font: "DejaVu Sans Mono", size: 13pt)[/usr/lib/haskell-packages]],
          [#text(font: "DejaVu Sans Mono", size: 13pt)[MHS_LIBRARY_PATH]],
        )
      ]
    ],
    [#text(size: 30pt)[→]],
    block(width: 100%, height: 3.2cm, inset: 12pt, radius: 10pt, fill: diagram-ink, stroke: 1pt + diagram-ink)[
      #align(center + horizon)[
        #stack(
          dir: ttb,
          spacing: .2cm,
          [#text(size: 20pt, fill: white, weight: "bold")[MicroHs]],
          [#text(size: 17pt, fill: white)[ordinary file IO]],
        )
      ]
    ],
  )
]

#align(center)[
  #set text(size: 16pt)
  #table(
    columns: (4.8cm, 9.5cm, 1fr),
    inset: 8pt,
    stroke: .7pt + diagram-mid,
    fill: (x, y) => if y == 0 { diagram-ink } else if x == 0 { diagram-fog } else { white },
    align: (x, y) => if x == 2 { left + horizon } else { center + horizon },
    [#text(fill: white, weight: "bold")[Route]],
    [#text(fill: white, weight: "bold")[Mechanism]],
    [#text(fill: white, weight: "bold")[Result]],
    [*JupyterLite*],
    [#text(font: "DejaVu Sans Mono", size: 14pt)[XeusAddon.mounts]],
    [Files enter MEMFS before the worker starts.],
    [*Standalone*],
    [#text(font: "DejaVu Sans Mono", size: 14pt)[\-\-preload-file dir@/path]],
    [A separate `.data` file is fetched.],
    [*Single script*],
    [#text(font: "DejaVu Sans Mono", size: 14pt)[\-\-embed-file dir@/path]],
    [Files are embedded in generated JavaScript.],
  )
]

#align(center)[#text(size: 20pt, weight: "bold")[The compiler sees files; deployment serves static assets.]]

== Package Access

#text(size: 14pt, fill: diagram-mid, weight: "bold")[APPENDIX · ECOSYSTEM CONSTRAINT]

#v(-.2cm)

#align(center)[
  #grid(
    columns: (5.8cm, .7cm, 9.2cm, .7cm, 6.5cm),
    row-gutter: .2cm,
    align: center + horizon,
    block(width: 100%, height: 1.3cm, inset: 8pt, radius: 8pt, fill: diagram-fog, stroke: 1pt + diagram-ink)[
      #align(center + horizon)[#text(size: 16.5pt, weight: "bold")[Browser worker]]
    ],
    [#text(size: 23pt)[→]],
    block(width: 100%, height: 1.3cm, inset: 8pt, radius: 8pt, fill: white, stroke: 1pt + diagram-ink)[
      #align(center + horizon)[#text(size: 15.5pt, weight: "bold")[External origin without CORS]]
    ],
    [#text(size: 23pt)[→]],
    block(width: 100%, height: 1.3cm, inset: 8pt, radius: 8pt, fill: white, stroke: 1.6pt + diagram-ink)[
      #align(center + horizon)[#text(size: 16.5pt, weight: "bold")[× CORS blocked]]
    ],
    block(width: 100%, height: 1.3cm, inset: 8pt, radius: 8pt, fill: diagram-fog, stroke: 1pt + diagram-ink)[
      #align(center + horizon)[#text(size: 16.5pt, weight: "bold")[Browser worker]]
    ],
    [#text(size: 23pt)[→]],
    block(width: 100%, height: 1.3cm, inset: 8pt, radius: 8pt, fill: white, stroke: 1pt + diagram-ink)[
      #align(center + horizon)[#text(size: 15.5pt, weight: "bold")[Same-origin or CORS-enabled]]
    ],
    [#text(size: 23pt)[→]],
    block(width: 100%, height: 1.3cm, inset: 8pt, radius: 8pt, fill: diagram-ink, stroke: 1pt + diagram-ink)[
      #align(center + horizon)[#text(size: 16.5pt, fill: white, weight: "bold")[Emscripten FS]]
    ],
  )
]

#v(-.15cm)

#align(center)[
  #set text(size: 14pt)
  #table(
    columns: (5.8cm, 8.5cm, 1fr),
    inset: 5pt,
    stroke: .7pt + diagram-mid,
    fill: (x, y) => if y == 0 { diagram-ink } else if x == 0 { diagram-fog } else { white },
    align: (x, y) => if x == 0 { center + horizon } else { left + horizon },
    [#text(fill: white, weight: "bold")[Actor]],
    [#text(fill: white, weight: "bold")[Shared requirement]],
    [#text(fill: white, weight: "bold")[Outcome]],
    [*Package authors*],
    [Browser-ready packages],
    [Replace native-only dependencies.],
    [*Registries / CDNs*],
    [CORS headers + immutable URLs],
    [Workers fetch artifacts directly.],
    [*JupyterLite sites*],
    [Bundles or same-origin mirrors],
    [Static, reproducible delivery.],
    [*Tooling community*],
    [Metadata, hashes, and caches],
    [Safe version resolution.],
  )
]

#v(-.1cm)

#align(center)[#text(size: 18pt, weight: "bold")[Browser-native distribution needs the whole Haskell ecosystem.]]

== Logo Licenses

#text(size: 14pt, fill: diagram-mid, weight: "bold")[APPENDIX · ATTRIBUTION]

#let license-url(url) = text(font: "DejaVu Sans Mono", size: 10.3pt)[#link(url)[#url]]

#align(center)[
  #table(
    columns: (7.5cm, 1fr),
    inset: 4pt,
    stroke: .7pt + diagram-mid,
    fill: (x, y) => if y == 0 { diagram-ink } else if calc.odd(y) { diagram-fog } else { white },
    align: (x, y) => if x == 0 { left + horizon } else { left + horizon },
    [#text(size: 16pt, fill: white, weight: "bold")[Mark · reuse basis]],
    [#text(size: 16pt, fill: white, weight: "bold")[Reference]],
    [#text(size: 13.5pt)[*Haskell* · public domain]],
    [#license-url("https://commons.wikimedia.org/wiki/File:Haskell-Logo.svg")],
    [#text(size: 13.5pt)[*WebAssembly* · CC0 1.0]],
    [#license-url("https://commons.wikimedia.org/wiki/File:WebAssembly_Logo.svg")],
    [#text(size: 13.5pt)[*Jupyter* · BSD + trademark policy]],
    [#license-url("https://jupyter.org/governance/trademarks.html")],
    [#text(size: 13.5pt)[*xeus* · Apache-2.0]],
    [#license-url("https://github.com/jupyter-xeus/xeus-haskell")],
    [#text(size: 13.5pt)[*GSoC* · official participation use]],
    [#license-url("https://developers.google.com/open-source/gsoc/resources/brand_guidelines")],
    [#text(size: 13.5pt)[*ICFP 2026* · official event mark]],
    [#license-url("https://icfp26.sigplan.org/")],
  )
]

#align(center)[#text(size: 15pt)[The #text(font: "DejaVu Sans Mono", size: 14pt)[-bg.png] derivatives only change opacity to 40%; trademark rules apply; no endorsement is implied.]]

#align(center)[#text(size: 14pt, fill: diagram-mid)[No DataHaskell or GHC logo: no explicit reusable artwork license was identified.]]

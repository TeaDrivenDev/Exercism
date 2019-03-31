module Seq

let keep f items =
    [
        for item in items do
            if f item then yield item
    ]

let discard f items = keep (f >> not) items

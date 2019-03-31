module Accumulate

let accumulate f items = [ for item in items -> f item ]
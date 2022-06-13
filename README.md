# purescript-reactive-effect

[![Latest release](http://img.shields.io/github/release/jhbertra/purescript-reactive-effect.svg)](https://github.com/jhbertra/purescript-reactive-effect/releases)
[![Build status](https://github.com/jhbertra/purescript-reactive-effect/workflows/Build/badge.svg?branch=main)](https://github.com/jhbertra/purescript-reactive-effect/actions?query=workflow%3ABuild+branch%3Amain)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-reactive-effect/badge)](https://pursuit.purescript.org/packages/purescript-reactive-effect)

Higher-order FRP with a pure reference implementation and an efficient push-pull graph traversal implementation.
The implementation is inspired by Reflex FRP and Reactive Banana. The API is modeled in the spirit of `Aff`.

## Installation

NOTE: package is still unpublished, this won't work yet!

```
spago install reactive-effect
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-reactive-effect).

The `examples` directory contains some code samples that demonstrate how to use
the library.

## Mini Tutorial

More comprehensive documentation is in the works. For now, this mini-tutorial
should provide a basic overview of the library and its API.

### Basic Concepts - Events and Behaviours

Functional reactive programming (FRP) is a composable abstraction for working
with data with respect to time. This is represented by two core types:

```purescript
-- A stream of discrete occurances of `a` values over time.
data Event t a
-- A value of type `a` that changes over time.
data Behaviour t a
```

Events represent push-based data sources while behaviours represent pull-based
data sources. Another way to say this is that an event tells you when it fires
a value (you can react to them), and you ask a behaviour what its value is (you
can sample them). You cannot, however, sample an event - it only has a value
when it fires,and you cannot react to a behaviour's value changing (it changes
continuously).

For example, you might use an `Event` to represent mouse clicks or a clock
tick, and you might use a `Behaviour` to represent the position of the mouse
cursor or the current time.

### Basic Concepts - Raff monad

When writing an application with `reactive-effect`, you create new events and
behaviours and connect them to the outside world from within the `Raff` monad
(short for "ReActive eFFect"). The `Raff` monad is a reader monad that has
access to the (mutable) network of events and behaviours being built.

```purescript
-- A monadic context that allows new events and behaviours to be created and
-- connected to the outside world.
data Raff t a
```

The `t` type variable is a phantom type variable that restricts the scope of
`Events` and `Behaviours` to the top-level `Raff` action that created them. It
is instantiated in the rank-2 signature:

```purescript
launchRaff :: forall a. (forall t. Raff t (Event t a)) -> Aff a
```

As events and behaviours are essentially handles to mutable data, this
restriction prevents them from leaking into a context where they could cause
unexpected state mutation (the same reason for the `s` parameter in `ST s a`).

`t` is short for "timeline" - i.e. each call to `launchRaff` creates a new
timeline for the provided action to run in.

### Running `Raff` Actions

The entrypoint API for `Raff` actions is:

```purescript
launchRaff  :: forall a. (forall t. Raff t (Event t a)) -> Aff a
launchRaff_ :: forall a. (forall t. Raff t (Event t a)) -> Aff Unit
```

The provided `Raff` action is referred to as the "guest application", which
`launchRaff` runs inside an `Aff` host. The guest application returns an event
which it can fire when it wants to be terminated by the host. When the host
detects that the shutdown event has fired, it stops running the guest, cleans up
resources acquired by the guest, and returns the value fired by the shutdown
event.

### Creating New Events

There are 2 primary API functions for creating new events:

```purescript
-- these type signatures are slight simplifications:
newEvent                  :: forall a. Raff { event :: Event a, fire :: a -> Effect Unit  }
makeEvent                 :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Raff (Event a)
```

`newEvent` is the simplest way to create an event - it simply returns a record
containing the new event and an `Effect` that can fire the event imperatively.
Note that because `fire` is an `Effect` and not a `Raff` action, it can be used
in arbitrary contexts.

`makeEvent` is creates an event from an async callback. Breaking down the
signature a little more:

```purescript
-- Setup action (A):----------------------\
-- Fire action (B):----------\             \
-- Teardown action (C):----------------------------------\
--                             \             \            \
--                       |--------------|     \       |----------|
--                      |-----------------------------------------|
makeEvent :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Raff (Event a)
```

The `setup action (A)` will be run when the `Event` is first subscribed to
(_not_ when `makeEvent` is called! See more below). The setup action receives a
`fire action (B)` which will cause the resulting `Event` to fire. The fire action
can be used zero or more times by the setup action, which can, for instance,
spawn a new `Aff` fiber (using `launchAff`) to fork an asynchronous process to
fire the event as needed. The setup action returns a `teardown action (C)`,
which can be used to release resources when the event is unsubscribed from
(e.g., remove event listeners added by the setup action, or cancel pending `Aff`
fibers created using `launchAff`.

### Creating New Behaviours

The API for creating behaviours is almost the same as that for creating events:

```purescript
-- these type signatures are slight simplifications:
newBehaviour                  :: forall a. a -> Raff { behaviour :: Behaviour a, update :: a -> Effect Unit  }
makeBehaviour                 :: forall a. a -> ((a -> Effect Unit) -> Effect (Effect Unit)) -> Raff (Behaviour a)
```

The main difference is that both functions accept an initial value `a` (since
behaviours must always have a value). In addition, the `fire action` is instead
referred to as the `update action`, as you don't "fire" behaviours.

Under the hood, these API functions are simply implemented by composing their
corresponding `Event` API functions with the `stepper` combinator.

### Transforming events.

`Event` has a `Functor` instance, so event values can be transformed with pure
functions via `map`. This is the most straightforward means to manipulate
events.

Events can also be filtered. Instances for `Compactable` and `Filterable` are
provided. From `Compactable`, you can take an `Event (Maybe a)` and create an
`Event a` which only fires when the original event fires `Just` values using
`compact` (similar to `catMaybes` for arrays). You can also turn an
`Event (Either a b)` into a record `{ left :: Event a, right :: Event b }`
using `separate`. From `Filterable`, you can filter and partition events with
predicates (using `filter` and `partition`, respectively), and you can combine
the behaviour of `map` and `compact/separate` using `filterMap` and
`partitionMap`.

The `indexed` combinator tags occurances of an event with a monotonically
increasing counter value. Every time the input event fires, the output event
will fire the next counter value alongside the original value.

`liftSample2` can be used to sample a behaviour with an event:

```purescript
liftSample2 :: forall t a b c. (a -> b -> c) -> Behaviour t a -> Event t b -> Event t c
```

It takes a binary function, a behaviour, and an event, returning a new event
ala `lift2`. Whenever the input event fires, it samples the current value of
the input behaviour and applies the value to the given function, along with the
value fired. The resulting even fires this value.

The composable version of this is `sampleApply`, or its more commonly used
infix operator form `<&>`, which has a signature similar to `apply` / `<@>`:

```purescript
sampleApply :: forall t a b. Behaviour t (a -> b) -> Event t a -> Event t b
```

As an extension of sampling behaviours, values in behaviours can also be used
to filter events. See `gate`, `split`, `filterApply`, and `partitionApply`.

There are some operations for events (notably accumulating state) that are not
pure functions. These can only be run in the `Raff` monad (or any monad that has
an instance of `MonadRaff`, such as `RaffPush`). In order to support using these
combinators dynamically (i.e. when events fire), instead of limiting them to the
static `Raff` context, there is a special combinator which runs a `RaffPush`
action when an event fires.

```purescript
push :: forall t a b. (a -> RaffPush t (Maybe b)) -> Event t a -> Event t b
```

Which has an infix operator form `<~` and a flipped infix operator form `~>`.
There is also an alias `pushed = push identity`, and a less flexible (but often
more convenient) form which doesn't allow the provided action to omit values
fired by the parent event by returning `Nothing`:

```purescript
pushAlways :: forall t a b. (a -> RaffPush t b) -> Event t a -> Event t b
```

#### Note on `Apply` and `Bind` for Events

Events also have `Apply` and `Bind` instances, but their utility is rather
limited. In the expression `let e3 = e1 <*> e2 in e3`, the event `e3` fires only
when both `e1` and `e2` are firing simultaneously (i.e. they fire during the
same frame). This means `<*>` is normally not suitable for merging two unrelated
events (for this you should use `align`). However, it can be useful for merging
events that were fanned out from the same source:

```purescript
let e1 = map f e0
let e2 = map g e0
...
let e3 = Tuple <$> e1 <*> e2
```

In this example, `e3` will fire once for every time `e0` fires. Of course, the
above could be written more succinctly as `let e3 = map (f &&& g) e0`, but
there may be cases when it makes sense to fan an event out into multiple events
and later merge a subset of them back together.

The `Bind` instance is even more limited. In the expression `let e1 = f =<< e0`,
`f` will be called every time `e0` fires. The event `e1` will fire if and only
if the event returned by `f` happens to be firing during the same frame that
`e0` fired. As a result, `bind` and `join` are not usually the preferred means
of flattening events that fire other events (this would be `switchE`). The `Bind`
instance does however cover the one case that `switchE` misses - the inner
event firing at the same time as the outer event. Therefore, it is available in
the rare cases where this is important to capture (`switchEImmediately`
combines both of these behaviours).

There is no `Applicative` (and hence, no `Monad` instance) because the only
lawful behaviour for `pure` would be an event that is always firing, which seems
like a bad idea. No other behaviour would satisfy the left and right identity
laws for `Applicative`.

### Merging Events

There are several ways to merge two or more events together into a single
event, and all of them are variations of the `align` combinator (from the
`Align` class). `Align` has the following signature (specialized to events):

```purescript
align :: forall t a b c. (These a b -> c) -> Event t a -> Event t b -> Event t c

-- Given
data These a b = This a | That b | Both a b
```

The interpretation is that given two events and a function for merging their
values, `align` is able to create a new event whenever either (or both) of the
input events are firing. This can be chained with further calls to `align` to
merge arbitrarily many events.

The `Semigroup` instance uses `align` to implement `append` by using `append`
to handle the `Both` case. The `Alt` instance uses `align` to implement `alt`
by ignoring the right hand value in case of a `Both`. Even the `Apply` instance
is defined in terms of a variation of `align` by only allowing `Both` values
through. `align` has a variation for filtering occurences, `alignMaybe`, one
for performing an impure merge, `alignM`, and one that allows both,
`alignMaybeM`.

`mempty`, `empty`, and `nil` (from `Monoid`, `Plus`, and `Alternative`) all
mean the same thing: they are events that never fire any values.

### Accumulating State

Naturally, implementing interesting interactive programs involves updating
state. There are 4 primary functions for doing this, each with several
minor variations:

```purescript
accumE :: forall t a b. (b -> a -> b) -> b -> Event t a -> Raff t (Event t b)
accumB :: forall t a b. (b -> a -> b) -> b -> Event t a -> Raff t (Behaviour t b)
accumAccum
  :: forall t a b
   . (b -> a -> Accum b c)
  -> b
  -> Event t a
  -> Raff t (Accum (Behaviour t b) (Event t c))
```

In each case, a folding function is provided to update some state `b` whenever
an event fires, using the previous value of the state. `accumE` fires an event
when the state updates, and `accumB` returns a behaviour that updates its value
to the current state when it changes. `mapAccum` is an efficient combination of
the two. It allows a state of type `b` to be accumulated, while emitting events
of type `c`.

There are variations of each of these for omitting state updates and event
occurences, as well as performing inpure state updates (in `RaffPush`).

### Flattening Nested Structures

Inevitably when writing a sufficiently complex application, you find yourself
with an `Event t (Event t _)`, or an `Behaviour t (Event t _)`, etc... In order
to use these, you often need to flatten them to a single layer, and there a few
ways to do so:

```purescript
switch   :: forall t a.                  Behaviour t (Event t a)         ->         Event t a
switcher :: forall t a. Behaviour t a -> Event t (Behaviour t a)         -> Raff t (Behaviour t a)
switchE  :: forall t a. Event t a     -> Event t (Event t a)             -> Raff t (Event t a)

-- Given by `Bind` instances
join     :: forall t a.                  Event t (Event t a)             ->         Event t a
join     :: forall t a.                  Behaviour t (Behaviour t a)     ->         Behaviour t a
```

### Creating New Events (Advanced)

There is a more advanced event creation function, `makeEventWithFireCallback`
that is very similar to `makeEvent` except that the fire action accepts an
additional parameter:

```purescript
-- Setup action (A):-----------------------------------------------------\
-- Fire action (B):----------------------------------\                    \
-- Teardown action (C):-------------------------------\--------------------\------------\
-- Fire callback (D):----------------------------\     \                    \            \
--                                       |--------\--------------------|     \       |----------|
--                                      |----------\--------------------------------------------|
--                                            |---------|
makeEventWithFireCallback :: forall a. ((a -> Effect Unit -> Effect Unit) -> Effect (Effect Unit)) -> Raff (Event a)
```

The additional parameter `fire callback (D)` is called when the `Event` has
actually been fired. The reason why this is helpful may not be immediately
evident. To explain why, it is necessary to understand a little bit about how
things work under the hood. Invoking `fire action` does not immediately cause
the event to fire. Instead, it writes an item to a `Queue` to be run inside a
frame by the machinery setup inside `launchRaff`. As a result, when `fire action`
returns, the event will not have fired yet. If you need to perform a specific
action when the event has actually fired (specifically, when event propagation
has finished in the frame in which the event was fired), you can pass that
action to `fire action`, and it will be run when the event eventually fires.

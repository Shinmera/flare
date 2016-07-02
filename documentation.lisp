#|
 This file is a part of flare
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare)

;; animation.lisp
(docs:define-docs
  (type animatable
    "Superclass container for anything that is animatable through progressions.

See PROGRESSIONS")
  
  (type progression-definition
    "Container class to instantiate a progression from.

The definition should at all time keep track of the existing instances
and update them in case the definition gets updated with new animations.
When the animations of the definition are set, the animations are also
set for each of the known instances of the definition.

See ANIMATIONS
See INSTANCES")
  
  (type progression
    "The controller to animate an animatable with.

Contains an entire sequence of animations and controls their behaviour
and effects on the animatable. 

When animations on the progression are set, the following happens:
1. The current clock is saved.
2. The progression is reset.
3. The new animations are set to the future set and sorted, the other
   sets are cleared and reinitialised to match the appropriate length.
4. The clock is set to the previously saved time.
5. All applicable animations are put into effect in fast-forwarding by
   calling UPDATE on the progression.

When a progression is reset, the following happens:
1. All past animations are pushed onto the present set.
2. The active animations are re-sorted to ensure consistency.
3. All the animations in the present set are reset in order.
4. All animations are pushed onto the future set.
5. The clock is fixed.

When a progression is updated, the following happens:
1. New animations that are now active during the current clock are
   shifted from the future set to the present set.
2. When the progression has an animatable, each animation is ticked.
   For this, the tick step must be calculated. If the duration of the
   animation is infinite, the tick is T. If the animation exceeded its
   duration, it is 1.0. Otherwise it is the linear interpolation
   between the current clock time, the beginning of the animation, and
   its duration.
3. Animations that have exceeded their duration are shifted from the
   present set onto the past set.
4. If no present or future animations remain, the progression stops
   itself.

See CLOCK
See DEFINITION
See ANIMATABLE
See ACTIVE
See ENDED
See FUTURE")
  
  (type animation
    "A representation for a single set of changes in a progression.

When an animation is ticked, the following happens:
1. The selector is called with the given animatable and a function
2. Once the selector calls its function with a matching object,
   each change in the animation is ticked with the matching object
   as argument.

When an animation is reset, each change in the animation is also
reset. This should cause whatever effect it might have had to be
restored on the scene. This is particularly tricky for operations
as they need to ensure the scene stays consistent.

See START
See DURATION
See SELECTOR
See CHANGES")
  
  (type change
    "Container for a single change or tween within an animation.")

  (variable *resetting*
    "A kludge variable used to prevent recursion upon a progression reset.")

  (function progressions
    "Accessor to the list of progressions that act upon this.

See ANIMATABLE")
  
  (function add-progression
    "Attach a new progression onto the animatable.

See PROGRESSION
See ANIMATABLE")
  
  (function remove-progression
    "Remove an existing progression from animatable.

See PROGRESSION
See ANIMATABLE")
  
  (function progression
    "Find all progressions that match the denominator within the container.")

  (function animations
    "Accessor to the vector of animations that the progression holds.")
  
  (function instances
    "Accessor to all progression instances that were created from this definition.

See PROGRESSION-DEFINITION")

  (function progression-instance
    "Constructs a new progression instance using the given definition.

See PROGRESSION
See PROGRESSION-DEFINITION")

  (function definition
    "Accessor to the progression's progression-definition

See PROGRESSION")

  (function animatable
    "Accessor to the animatable the progression is acting upon.

See PROGRESSION")
  
  (function present-animations
    "Accessor to the vector of currently active animations within the clock time.

See PROGRESSION")
  
  (function past-animations
    "Accessor to the vector of animations that have ended before the current clock time.

See PROGRESSION")
  
  (function future-animations
    "Accessor to the vector of animations that have yet to become activated after the current clock time.

See PROGRESSION")

  (function copy-animations
    "Create a copy of the given sequence of animations.

Calls COPY on each animation.")

  (function shift-array-elements
    "Moves elements from FROM to TO if they pass TEST.

Elements are actively removed from FROM and inserted into TO

See ARRAY-UTILS:VECTOR-POP-POSITION
See CL:VECTOR-PUSH")

  (function copy
    "Copy the given object as appropriate for its type.
Only useful for copying ANIMATIONs and CHANGEs

See ANIMATION
See CHAGNE")

  (function beginning
    "Accessor to the beginning (in seconds) at which the animation should start.

See ANIMATION")
  
  (function duration
    "Accessor to the duration (in seconds) that the animation should be active for.
Can also be T, in which case the animation should go on forever.

See ANIMATION")
  
  (function changes
    "Accessor to the list of changes that the animation executes.

See ANIMATION")
  
  (function selector
    "Accessor to the selector that describes which elements to affect.

See ANIMATION
See COMPILE-SELECTOR")
  
  (function tick
    "Performs a single update tick, moving along the animation on the animatable at the given clock for the given step amount of time.

See ANIMATION
See ANIMATABLE
See CLOCK")

  (function format-progression
    "Print the progression in a usable manner to inspect its current state.
Useful for debugging

See PROGRESSION")

  (function simulate-progression
    "Simulates running the progression-definition.

Creates a new scene instance and progression instance,
starts both of those and then updates the scene, printing
the progression each 0.7 seconds.

See SCENE
See PROGRESSION-INSTANCE
See UPDATE
See FORMAT-PROGRESSION"))

;; change.lisp
(docs:define-docs
  (function parse-change
    "Parse a change definition form that gives the TYPE and ARGS into an evaluatable form.")

  (function define-change-parser
    "Define a parser for the given type of change.")

  (function copy
    "Create a copy of the given item in a way that is deemed appropriate for it.
Mostly used for copying changes.")

  (type print-change
    "A NO-OP change that simply prints its TICK arguments when called.
Useful for debugging.

Creation: (print)

See CHANGE
See TICK")

  (type call-change
    "A change that calls a specified function on every tick.
The function is called with the OBJECT, CLOCK, and STEP received from TICK.

Creation: (call :func tick-function)

See CHANGE
See FUNC
See TICK")

  (function func
    "Accessor to the function container slot.")

  (type operation
    "Superclass for changes that modify the scene graph by adding, removing, or moving elements within it.

See CHANGE")

  (type enter-operation
    "Represents an operation that introduces new objects into the scene graph.

Creation: (enter class :n number-of-copies :children child-forms :parent parent init-args..)
Child-forms being a list of enter forms, excluding the ENTER symbol at the start.
init-args being further initialisation arguments to be passed to the class that's being instantiated.

Upon TICK, the CREATOR function is executed and each resulting object is ENTERed into the
given target animatable.

See OPERATION
See OBJECTS
See CREATOR")

  (function objects
    "Accessor to a value that stores which objects are being managed.")

  (function creator
    "Accessor to the function that upon calling instantiates one or more objects.

See ENTER-OPERATION")

  (type leave-operation
    "Represents an operation that removes objects from the scene graph.

Creation: (leave)

Upon TICK, the given animatable is removed from its parents by LEAVE.

See OPERATION
See OBJECTS")

  (type tween
    "Superclass for changes that modify the given animatable, but do not change the scene graph.

See CHANGE")

  (function tween-value
    "Computes the currently applicable value for the given tween, object, clock, and stepping time.

See TWEEN")

  (function original-value
    "Returns the original value this object might have had before the given tween changed anything.

See TWEEN")

  (type slot-tween
    "A tween mixin that modifies a slot on the object.

Upon TICK the slot-value is set with the result of TWEEN-VALUE.

See TWEEN
See SLOT
See ORIGINALS
See TWEEN-VALUE")

  (function slot
    "Accessor to the slot that should be modified.")

  (function originals
    "A hash table to store the original values of objects before they were changed.")

  (type accessor-tween
    "A tween mixin that modifies an object through an accessor.

Upon TICK the corresponding setf function is called with the result of TWEEN-VALUE.

See TWEEN
See ACCESSOR
See ORIGINALS
See TWEEN-VALUE")

  (function accessor
    "Accessor to the accessor that should be used to modify an object.")

  (type range-tween
    "A tween mixin that interpolates a given range of values using an easing function.

Implements TWEEN-VALUE
Default FROM is 0, TO is 1, and EASE is LINEAR.

See TWEEN
See FROM
See TO
See EASE-FUNC
See *EASINGS*")

  (function from
    "Accessor to the beginning value.

See RANGE-TWEEN")

  (function to
    "Accessor to the ending value.

See RANGE-TWEEN")

  (function ease-func
    "Accessor to the easing function to be used to interpolate the value range.

See RANGE-TWEEN")

  (type constant-tween
    "A tween mixin that simply increases a value every tick.

Implements TWEEN-VALUE
Default BY and FOR are 1.

See TWEEN
See BY
See FOR
See START")

  (function by
    "The step by which to increase each unit.

See CONSTANT-TWEEN")

  (function for
    "The time step (in seconds) in which the value is increased by a single BY unit.

See CONSTANT-TWEEN")

  (function start
    "Stores the starting clock time at which the tween was started.

See CONSTANT-TWEEN")

  (type range-slot-tween
    "Combination of a range-tween and a slot-tween.

See RANGE-TWEEN
See SLOT-TWEEN")

  (type increase-slot-tween
    "Combination of a constant-tween and a slot-tween.

See CONSTANT-TWEEN
See SLOT-TWEEN")

  (type range-accessor-tween
    "Combination of a range-tween and an accessor-tween.

Creation: (set accessor :ease easing-func :from from :to to)

See RANGE-TWEEN
See ACCESSOR-TWEEN")

  (type increase-accessor-tween
    "Combination of a constant-tween and an accessor-tween.

Creation: (increase accessor :by by :for for)

See CONSTANT-TWEEN
See ACCESSOR-TWEEN")

  (type call-slot-tween
    "Combination of a call-change and a slot-tween.

Implements TWEEN-VALUE.

See CALL-CHANGE
See SLOT-TWEEN
See TWEEN-VALUE")

  (type call-accessor-tween
    "Combination of a call-change and an accessor-tween.

Creation: (calc accessor :to form)
The FORM may use the implicit variables OBJECT, CLOCK, and STEP.

Implements TWEEN-VALUE.

See CALL-CHANGE
See SLOT-TWEEN
See TWEEN-VALUE"))

;; clock.lisp
(docs:define-docs
  (function update
    "Updates the given object, causing its internal representation to be adapted for the current time.

Returns the object given.")
  
  (function stop
    "Stops the given clock.

Returns the object given.

See CLOCK")
  
  (function start
    "Starts the given clock.

Returns the object given.

See CLOCK")
  
  (function reset
    "Resets the given clock to its initial state.

Returns the object given.

See CLOCK")
  
  (function running
    "Accessor to whether the clock is currently running or not.

See CLOCK")
  
  (function synchronize
    "Synchronize the clock to the new time.

Time should be another clock or seconds.

Returns the object given.

See CLOCK")
  
  (function clock
    "Accessor to the current time in the clock.

Note that the current time in the clock must not necessarily be 100% accurate.
In order to get perfectly accurate current time of the clock, you must call UPDATE
on it before retrieving its current time value with CLOCK.

See CLOCK")

  (type clock
    "A representation for an item that changes its state over time.

Keeps its own time information in seconds.

See START
See STOP
See RESET
See RUNNING
See UPDATE
See SNYCHRONIZE
See CLOCK
See PREVIOUS-TIME")

  (function previous-time
    "Accessor to the previous internal-real-time when the clock was updated.

See CLOCK"))

;; container.lisp
(docs:define-docs
  (function clear
    "Removes all objects from the container.

Returns the object given.

See CONTAINER")
  
  (function insert
    "Insert the OBJECTs into the CONTAINER.

Returns the container given.

See CONTAINER")
  
  (function withdraw
    "Removes the OBJECTs from the CONTAINER.

Returns the container given.

See CONTAINER")
  
  (function name-map
    "Accessor to the name table of the collective.

See COLLECTIVE")
  
  (function units
    "Returns a fresh list of all units in the collective.

See UNIT
See COLLECTIVE")
  
  (function unit
    "Accessor to a given, named unit in the collective.

See UNIT
See COLLECTIVE")
  
  (function enter
    "Adds the given UNIT into the COLLECTIVE.

Returns the unit given.

See UNIT
See COLLECTIVE")
  
  (function leave
    "Removes the given UNIT from the COLLECTIVE.

Returns the unit given.

See UNIT
See COLLECTIVE")
  
  (function name
    "Accessor to the name of the unit.

See UNIT")
  
  (function collective
    "Accessor to the collective the unit is in.

See UNIT")

  (type container
    "A simple class that can hold a set of objects in an indexed-set.

See CLEAR
See OBJECTS
See INSERT
See WITHDRAW")

  (function map-container-tree
    "Recursively maps FUNCTION over all descendants of CONTAINER.

See CONTAINER")

  (function do-container-tree
    "Iterates over all descendants of CONTAINER

See MAP-CONTAINER-TREE")

  (function print-container-tree
    "Prints the entire CONTAINER tree hierarchy nicely to the given STREAM.

See CONTAINER")

  (type collective
    "A collective is a container that also has a name-map to easily reach objects.
This includes objects that may be in containers further down the hierarchy.

See CONTAINER")

  (type unit
    "A unit is an object with a name and a collective."))

;; easings.lisp
(docs:define-documentation-test easing (symb)
  (easing symb))

(docs:define-docs
  (variable *easings*
    "A hash table associating names to easing functions.

Each easing function takes a single float value between 0 and 1 that should be eased according to a curve.")

  (variable *ease-docs*
    "A hash table associating names to easing function docstrings.")

  (function easing
    "Accessor to the easing function associated with the given name, if any.

See *EASINGS*")

  (function remove-easing
    "Removes the easing function associated with the given name.

See *EASINGS*")

  (function define-easing
    "Shorthand macro to define an easing function.

See EASING")

  (function ease
    "Shorthand function to perform an easing interpolation.

X must be a float between 0 and 1
BY must name an easing function
FROM and TO must be REALs specifying the boundaries of the easing.")

  (function ease-object
    "Shorthand function to ease a range.

FROM and TO must be matching objects

By default works on REALs and VECs.

See EASE")

  (easing linear
    "Interpolates by a linear curve.

See http://easings.net/")
  
  (easing quad-in
    "Interpolates by a quad-in curve.

See http://easings.net/")
  
  (easing quad-out
    "Interpolates by a quad-out curve.

See http://easings.net/")
  
  (easing quad-in-out
    "Interpolates by a quad-in-out curve.

See http://easings.net/")
  
  (easing cubic-in
    "Interpolates by a cubic-in curve.

See http://easings.net/")
  
  (easing cubic-out
    "Interpolates by a cubic-out curve.

See http://easings.net/")
  
  (easing cubic-in-out
    "Interpolates by a cubic-in-out curve.

See http://easings.net/")
  
  (easing quart-in
    "Interpolates by a quart-in curve.

See http://easings.net/")
  
  (easing quart-out
    "Interpolates by a quart-out curve.

See http://easings.net/")
  
  (easing quart-in-out
    "Interpolates by a quart-in-out curve.

See http://easings.net/")
  
  (easing quint-in
    "Interpolates by a quint-in curve.

See http://easings.net/")
  
  (easing quint-out
    "Interpolates by a quint-out curve.

See http://easings.net/")
  
  (easing quint-in-out
    "Interpolates by a quint-in-out curve.

See http://easings.net/")
  
  (easing sine-in
    "Interpolates by a sine-in curve.

See http://easings.net/")
  
  (easing sine-out
    "Interpolates by a sine-out curve.

See http://easings.net/")
  
  (easing sine-in-out
    "Interpolates by a sine-in-out curve.

See http://easings.net/")
  
  (easing expo-in
    "Interpolates by a expo-in curve.

See http://easings.net/")
  
  (easing expo-out
    "Interpolates by a expo-out curve.

See http://easings.net/")
  
  (easing expo-in-out
    "Interpolates by a expo-in-out curve.

See http://easings.net/")
  
  (easing circ-in
    "Interpolates by a circ-in curve.

See http://easings.net/")
  
  (easing circ-out
    "Interpolates by a circ-out curve.

See http://easings.net/")
  
  (easing circ-in-out
    "Interpolates by a circ-in-out curve.

See http://easings.net/")
  
  (easing back-in
    "Interpolates by a back-in curve.

See http://easings.net/")
  
  (easing back-out
    "Interpolates by a back-out curve.

See http://easings.net/")
  
  (easing back-in-out
    "Interpolates by a back-in-out curve.

See http://easings.net/")
  
  (easing elastic-in
    "Interpolates by a elastic-in curve.

See http://easings.net/")
  
  (easing elastic-out
    "Interpolates by a elastic-out curve.

See http://easings.net/")
  
  (easing elastic-in-out
    "Interpolates by a elastic-in-out curve.

See http://easings.net/")
  
  (easing bounce-in
    "Interpolates by a bounce-in curve.

See http://easings.net/")
  
  (easing bounce-out
    "Interpolates by a bounce-out curve.

See http://easings.net/")
  
  (easing bounce-in-out
    "Interpolates by a bounce-in-out curve.

See http://easings.net/"))

;; forms.lisp
(docs:define-docs
  (function orientation
    "Accessor to the vector that defines the orientation of the entity.

See ORIENTED-ENTITY")

  (function size
    "Accessor to the size of the entity.

See SIZED-ENTITY")

  (function up
    "Accessor to the UP vector.

See ARC")

  (function angle
    "Accessor to the angle.

See ARC")

  (function spacing
    "Accessor to the spacing between items.

See ARC")

  (type oriented-entity
    "An entity that can be oriented by a vector.

See ENTITY
See ORIENTATION")

  (type sized-entity
    "An entity that has a given size or extent.

See ENTITY
See SIZE")

  (type formation
    "Entity superclass for all formations.

Formations only handle the positioning of child entities, but do not display by themselves.

See ENTITY")

  (function reposition
    "Recalculate the positioning of child entities.")

  (type particle
    "Entity superclass for all particles.

Particles should not move by themselves and only handle the displaying.

See ENTITY")

  (type arc
    "Formation to represent an equidistant distribution of entities along an arc.

See FORMATION
See ORIENTED-ENTITY
See SIZED-ENTITY
See UP
See TANGENT
See ANGLE
See SPACING")

  (function tangent
    "The tangent vector between the UP and ORIENTATION.

See ARC")

  (type ring
    "Formation to represent an equidistant distribution of entities along a ring.

See ARC"))

;; paintable.lisp
(docs:define-docs
  (function call-with-translation
    "Call FUNC after having performed a translation on TARGET by VEC.")

  (function visibility
    "Accessor to how opaque the paintable is.
Has to be a float between 0 and 1.

See PAINTABLE")

  (function paint
    "Performs the necessary painting operations to draw PAINTABLE onto TARGET.

See TARGET
See PAINTABLE")

  (function with-translation
    "Shorthand macro for translation.

See CALL-WITH-TRANSLATION")

  (type target
    "Superclass for a painting device onto which things can be drawn.

See PAINT
See CALL-WITH-TRANSLATION")

  (type paintable
    "Superclass for anything that may be painted onto a target.

See PAINT
See TARGET"))

;; parser.lisp
(docs:define-docs
  (type interval-designator
    "An interval-designator can be either a real, T, or NIL.")

  (function designator-p
    "Returns T if the given THING is an interval-designator.

See INTERVAL-DESIGNATOR")

  (function parse-intervals
    "Normalises the lenient interval FORMS into strict expressions.

result     ::= (expression*)
expression ::= (start duration animation-expression)

See DEFINE-PROGRESSION")

  (variable *mapper*
    "A placeholder variable used to hold the final mapping function upon selector evaluation.")

  (variable *i*
    "A counter variable used to determine the current index in constraints.")

  (function compile-constraint
    "Compile a selector constraint into a function.

constraint ::= name | nth | this | children | everything | function | list
name       --- A keyword naming a unit in the collective
nth        --- An integer specifying the nth unit in the collective
this       --- The symbol T meaning the current object
children   --- A symbol with name \">\" specifying all children of the current object
everything --- A symbol with name \"*\" specifying all descendants as per DO-CONTAINER-TREE
function   --- A predicate function that is passed the current object
list       --- A quoted literal, function reference, or function form to use

Resulting from a compile-constraint call should be a function
that takes a single argument, the current object to constrain on.
The NEXT argument is the function to call next if the constraint
passes its test. A single constraint may call this next function
as many times as it wants.")

  (function compile-selector
    "Compiles a selector into a function.

selector ::= constraint | (constraint*)

Returned is a function of two arguments, a collective and a function.
The collective is the root of the scene graph that is selected on and
each unit within it that the selector is matching on results in a call
to function with that unit as its argument.

See COMPILE-CONSTRAINT")

  (function compile-change
    "Simply calls PARSE-CHANGE")

  (function parse-animation
    "Compiles BEGINNING, DURATION, and the definition EXPRESSION into an actual FORM.

expression ::= (selector change*)

See COMPILE-CHANGE
See DEFINE-PROGRESSION")

  (function compile-animations
    "Compiles INTERVAL definition expressions into a list of animation definition forms.

First normalises the intervals per PARSE-INTERVALS then creates a form for each 
per PARSE-ANIMATION and outputs each into a LIST form.

See DEFINE-PROGRESSION")

  (variable *progressions*
    "Hash table to contain global progression definitions.")

  (function progression-definition
    "Accessor to the global progression definition by name.

See *PROGRESSIONS*")

  (function remove-progression-definition
    "Remove the global progression definition by name

See *PROGRESSIONS*")

  (function define-progression
    "Convenience macro to define a global progression.
Returns the progression name.

The formal specification of the body intervals is as follows:
body      ::= interval*
interval  ::= start [end] animation*
animation ::= (selector change*)
change    ::= (change-type argument*)
start     --- An integer (in seconds) that represents the starting time 
              of the animations
end       --- An integer (or T, indicating infinity) that represents the 
              ending time of the animations
selector  --- A valid selector as per COMPILE-SELECTOR
change    --- A valid change as per COMPILE-CHANGE

If the END is not specified for a given interval, then the next START
is taken as the end. If no next start exists, then the end is T. In order
to allow brevity, multiple animations can be specified between two time
codes. This is then normalised into the strict form of
 (START DURATION ANIMATION) as per PARSE-INTERVALS. 

An example definition follows:

(define-progression foo
  0 (T (enter ring :name :ring :contents (bullet :size 2 :count 20)))
  0 8 (:ring (increase size :by 2))
  0 20 (:ring (set angle :to 1000 :ease 'quad-in-out))
       ((:ring >) (set size :to 50))
  20 (:ring (leave)))

At time 0, a ring is created with name :ring and 20 bullets of size 2 as
its children. It is entered into the collective. Then from time 0 to 8,
the ring's size is increased by 2 every second. Simultaneously from time
0 to 20 the ring's angle is increased to 1000, eased by the quad-in-out
interpolation and the ring's children (the 20 bullets) increase in size
to 50. At time 20, the ring is removed from the collective again.

See PROGRESSION-DEFINITION
See COMPILE-ANIMATIONS"))

;; scene.lisp
(docs:define-docs
  (function scene
    "Accessor to the scene the scene-unit is contained in.

See SCENE
See SCENE-UNIT")

  (function location
    "Accessor to the location of the entity.

See ENTITY")

  (type scene
    "Container class to represent the top-level scene that should be drawn and managed.

See COLLECTIVE
See CLOCK
See PAINTABLE
See ANIMATABLE")

  (type scene-unit
    "A unit within a scene.

See SCENE
See UNIT")

  (type entity
    "A paintable and animatable entity within a scene.

See COLLECTIVE
See SCENE-UNIT
See PAINTABLE
See ANIMATABLE
See LOCATION"))

;; toolkit.lisp
(docs:define-docs
  (function define-self-returning-method
    "Shorthand to define an :around method that will ensure the first argument is always returned.")

  (function ensure-sorted
    "Ensures that the VEC is sorted stably in-place.

This means that if STABLE-SORT returns a new vector instead of re-using the given one,
the elements from the new vector are copied back into the old one so that it appears
as if it had been modified in-place. Always returns VEC.

See STBLE-SORT"))

(in-package #:org.shirakumo.flare.indexed-set)
;; indexed-set.lisp
(docs:define-docs
  (type indexed-set
    "A set in which each element also has an index.

Aside from MAP-SET and DO-SET you can also use ITERATE
to go through the set by FOR .. ON-SET or FOR .. IN-SET.

See QUEUE
See SET
See MAKE-INDEXED-SET
See MAP-SET
See DO-SET
See SET-ADD
See SET-REMOVE
See SET-SIZE
See SET-FIRST
See SET-LAST
See SET-VALUE-AT
See SET-INDEX-OF
See CLEAR-SET
See IN-SET-P
See COERCE-SET")

  (function set
    "Accessor to the set table of the indexed-set.

See INDEXED-SET")

  (function make-indexed-set
    "Creates a new indexed set.

See INDEXED-SET")

  (function map-set
    "Maps the function over all elements of the set in order.

See INDEXED-SET")

  (function do-set
    "Iterates over all elements of the set in order.

See INDEXED-SET")

  (function set-add
    "Add a new value to the set.

Returns two values, the value that was added, and whether it was added
as a new element to the set. If it already existed, the second value is
NIL.

See INDEXED-SET")

  (function set-remove
    "Remove a value from the set.

Returns two values, the set that was modified, and whether the value
existed in the set to begin with. If it did not, the second value is
NIL.

See INDEXED-SET")

  (function set-size
    "Returns the number of items in the set.

See INDEXED-SET")

  (function set-first
    "Returns the first item in the set.

See INDEXED-SET")

  (function set-last
    "Returns the last item in the set.

See INDEXED-SET")

  (function set-value-at
    "Returns the value at the given index in the set.

See INDEXED-SET")

  (function set-index-of
    "Returns the index of the value in the set.

See INDEXED-SET")

  (function clear-set
    "Removes all values from the set.

See INDEXED-SET")

  (function in-set-p
    "Returns T if the value is contained in the set.

See INDEXED-SET")

  (function coerce-set
    "Allows coercing the set to:

indexed-set, hash-table, queue, list, vector, or sequence.

See INDEXED-SET
See COERCE-QUEUE"))

(in-package #:org.shirakumo.flare.queue)
;; queue.lisp
(docs:define-docs
  (type cell
    "Struct to contain a queue cell with VALUE, LEFT, and RIGHT slots.

See VALUE
See LEFT
See RIGHT")

  (function make-cell
    "Constructs a new queue cell.

See CELL")

  (function value
    "Accesses the value contained in a queue cell.

See CELL")

  (function left
    "Accesses the cell left to the current cell.

See CELL")

  (function right
    "Accesses the cell right to the current cell.

See CELL")

  (function cell-insert-before
    "Inserts the cell before its neighbour, making sure to keep all links updated.

See CELL")

  (function cell-remove
    "Removes the cell out of the link chain, making sure to keep all links updated.
Unless the cell is the only item in the link chain, its left/right slots are not
modified.

See CELL")

  (type queue
    "Implements an ordered queue.

Aside from MAP-QUEUE and DO-QUEUE you can also use ITERATE
to go through the set by FOR .. ON-QUEUE or FOR .. IN-QUEUE.

See HEAD
See TAIL
See SIZE
See MAP-QUEUE
See DO-QUEUE
See ENQUEUE
See DEQUEUE
See QUEUE-REMOVE
See QUEUE-SIZE
See QUEUE-FIRST
See QUEUE-LAST
See QUEUE-VALUE-AT
See QUEUE-INDEX-OF
See CLEAR-QUEUE
See IN-QUEUE-P
See COERCE-QUEUE")

  (function head
    "Accesses the head cell of the queue

See CELL
See QUEUE")

  (function tail
    "Accesses the tail cell of the queue

See CELL
See QUEUE")

  (function size
    "Accesses the size counter of the queue.

See QUEUE")

  (function make-queue
    "Creates a new queue instance.

See QUEUE")

  (function map-queue
    "Maps the function over all values in the queue in order.

See QUEUE")

  (function do-queue
    "Iterates over each value in the queue in order.

See QUEUE")

  (function enqueue
    "Inserts the given value at the end of the queue.

See QUEUE")

  (function dequeue
    "Pops the next value off the front of the queue.
The second value indicates whether there was any element in the queue at all.

See QUEUE")

  (function queue-remove
    "Removes the given value from the queue.

This is potentially very costly as it might have to scan the entire queue.

See QUEUE")

  (function queue-size
    "Returns the number of elements in the queue.

See QUEUE")

  (function queue-first
    "Returns the first (front) value in the queue if there is any.
The second value indicates whether there was any element in the queue at all.

See QUEUE")

  (function queue-last
    "Returns the last (end) value in the queue if there is any.
The second value indicates whether there was any element in the queue at all.

See QUEUE")

  (function queue-value-at
    "Returns the value at the given position in the queue.
The second value is NIL if the position is out of range.

This is potentially very costly as it might have to scan the entire queue.

See QUEUE")

  (function queue-index-of
    "Returns the index of the value in the queue.
If the value could not be found, NIL is returned instead.

This is potentially very costly as it might have to scan the entire queue.

See QUEUE")

  (function clear-queue
    "Removes all elements from the queue.

See QUEUE")

  (function in-queue-p
    "Returns T if the given value is found in the queue.

See QUEUE")

  (function coerce-queue
    "Allows coercing the queue to:

queue, list, vector, or sequence.

See QUEUE"))

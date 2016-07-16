## What
Flare is a library designed to allow quick and precise particle effect creations. It does not concern itself with displaying and only with the management and movement of particles. As such, it can easily be integrated into any existing or future application.

## So What's Different? Why Should I Use This?
Usually in particle systems the way you control things is by describing various states on the emitter. Flare instead takes a much more fine-grained approach by allowing you to control individual particles and describe their movement and change over time.

## How To
As Flare does not do any graphics on its own, you will first have to create something that can render graphics of some kind. Once you have that, you should subclass `particle` to create your own particle types. You should also implement methods on `paint` so that the particle can be drawn. The last thing that's needed is a method on `call-with-translation` that should perform a translation on your drawing target. That should settle everything in terms of hooking Flare up to your graphics system.

Next you will need to keep a `scene` instance into which your particles will be spawned and your animations will be played. You should then call `paint` on the scene object in your drawing loop and call `start` on the scene object when your application is initialised.

Finally, the exciting part: defining animation progressions. This happens through `define-progression`, which creates a globally named `progression-definition`. The body of a definition is made up of a series of intervals and animations. Each animation is composed of a selector and a bunch of changes. The selector decides to which units in the scene the changes should apply, and the changes decide what happens while the animation runs.

So let's look at a simple example. We'll create a particle that spins on a circle.

    (define-progression spinner
      0 0 (T (enter ring :size 100 :children (sphere :size 10)))
      0 T (> (increase angle :by 360 :for 1)))

This defines a progression named `spinner` with two animations

* The first animation takes place from the 0th second to the 0th second, so it will only ever happen exactly once. It applies to the scene object itself and causes only a single change. This change `enter`s a ring of size 100 with a single sphere child of size 10 into the scene.
* The second animation takes place from the 0th second to infinity, so it'll go on as long as we keep the scene running. It applies to the children of the scene object (in this case the ring) and has a single change that will increase the angle by 360 every second.

Here `sphere` is the name of a simple particle. If you named your particle class something else, replace that with your own. In order to make the progression have an effect on our scene, we need to instantiate it and add it to the scene. You can instantiate a progression with `progression-instance` and add it to the scene by `enter`. Finally, we'll want to start it to see it in action.

    (start (enter (progression-instance 'spinner) scene-instance))

We can change the example to be a bit more exciting by making it be a pendulum instead of a mere spinner. To do this we substitute a `calc` cahnge for the `increase` change:

    (calc angle :to (+ 90 (* (sin (* clock 4)) 40)))

You can redefine the progression on the fly while it is still running and it should gracefully update on your screen. This should allow you to easily hack out rather complex animations. If you want to start again from the beginning, call `reset` on the progression instance.

An example implementation of all this including some nifty show progressions can be found in the `flare-viewer` system. You can also use this system yourself to play around with progressions for a bit.

## Internals
Flare must handle two parts-- one the animation itself, and two the objects in the scene.

### Scene Graph
FIXME

Building up on this are the `scene` and `entity` classes. The scene furthermore is `paintable`, `animatable`, and a `clock`, so that it can be drawn, animated, and can keep the time. An entity can always contain further entities, and aside from being paintable and animatable as well, it also always has a `location` vector so that we can move it around. All entities are always relative to their parent in their location. The transformation of this is achieved by a `call-with-translation` around each `paint` call.

Any `animatable` can keep a set of `progression` instances that will be automatically updated when the animatable is, and thus carry out their transformations for as long as they should be active.

### Animation
The animation process is divided up into `progression-definition`s, `progression`s, `animation`s, and `change`s. Progression-definitions merely exist as containers to instantiate and update progressions from. When their `animations` slot is set, they automatically update all instances.

A progression is more complicated however as it has to actually manage all the various animations, ensure they're run in the proper order and for just long enough, and it has to make it possible to rewind everything and reset the state of the scene to how things were before. Thus, progressions each have three vectors of `present-animations`, `past-animations`, and `future-animations`. Since they must also know whom to act upon, they keep a back-link to the `animatable` that they should modify.

Each `update` then the progression checks if any new animations now have their time to shine and if so, move them onto the present set. All the present animations then get `tick`ed with their appropriate time-step and the animatable passed along. Finally, all animations that are past their prime and have exceeded their duration are moved off onto the past set. If there are no more future or present animations available, the progression knows it's done and stops itself.

Now, when a progression is `reset`, it shuffles all the past animations into the present set, reorders everything in reverse, and then resets each animation in sequence. Each animation then causes each change to be reset, hopefully one by one back-tracking all the changes that were made to the scene.

Thus by coupling a reset with an explicit clock-set and an update, a progression can be explicitly fast-forwarded or rewound to any particular point in its life-cycle and all the updates should stay consistent throughout.

When an animation is ticked, the actual entity selection process starts to happen. When the progression is defined, each animation must be supplied with a selector, which is compiled down to a chain of function calls that either expand or limit the set of applicable objects. For a list of the possible selector constraints, see `compile-constraint`. Each change of the animation is then ticked with each object that passed the constraint functions.

Finally, changes. Changes are divided into `operation`s and `tween`s. Operations change the scene in some way by adding, removing, or moving entities around. They should not affect any internal state of entities and only affect the object tree. Tweens on the other hand do the opposite and should only modify the internal state of whatever object they are passed in their `tick` call, but should never change the object tree in any way.

Flare supplies several default changes, some of which have parsers (`define-change-parser`) so that they can actually be used from a progression definition. On the operations side, we have `enter-operation` and `leave-operation` used to add or remove objects respectively. The enter-operation is significant in the sense that it uses a `creator` function that can potentially instantiate arbitrarily many or arbitrarily deep structures. On the tween side we have several mixins to make the definition of your own tweens easier (`range-tween` `constant-tween` `slot-tween` `accessor-tween`), from which two usable tweens arise, `range-accessor-tween` (`set`) and `increase-accessor-tween` (`increase`). There's one more notable tween to mention: `call-accessor-tween` (`calc`), which sets the slot according to a user-definable function.

Each change must know by itself how to reset the objects it acted upon to their initial state. The `slot-tween` and `accessor-tween` know how to do this by saving the original value of each object they act upon. Similarly, the enter and leave operations keep track of where they added or removed which objects.

## Videos
Here are some demo videos made during various points in the development of Flare.

<a href="http://www.youtube.com/watch?v=szGU6mHgADQ" target="_blank"><img src="http://img.youtube.com/vi/szGU6mHgADQ/0.jpg" alt="Youtube Video" width="240" height="180" border="10" /></a>

<a href="http://www.youtube.com/watch?v=-ScT0JyW-Fs" target="_blank"><img src="http://img.youtube.com/vi/-ScT0JyW-Fs/0.jpg" alt="Youtube Video" width="240" height="180" border="10" /></a>

<a href="http://www.youtube.com/watch?v=9J8UMLhcaik" target="_blank"><img src="http://img.youtube.com/vi/9J8UMLhcaik/0.jpg" alt="Youtube Video" width="240" height="180" border="10" /></a>

<a href="http://www.youtube.com/watch?v=4OJZF8hLsVk" target="_blank"><img src="http://img.youtube.com/vi/4OJZF8hLsVk/0.jpg" alt="Youtube Video" width="240" height="180" border="10" /></a>

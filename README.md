# lfl - header only c++11 constexpr library

lfl library contains higher order functions such as:

* map
* zip
* reduce
* filter
* sort
* etc...

and those are executed as parallel as possible.
This makes it possible to reduce constexpr-steps.

Note: The execution efficiency at compile-time is high,
      but the runtime code becomes bloated and the runtime efficiency will be worse.



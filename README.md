# elm-date-time
An extensible date time library for Elm

First off, thanks to [Bogdanp][bogdanp] and all other contributers to [elm-time][elm-time].
A large part of this library is based on their work (or shamelessly copied).
I hope you like what will become of it.


## Why another library?
There are the core modules Date and Time already.
A much more comprehensive library is found in [elm-time][elm-time], formerly maintained by [Bogdanp][bogdanp], now by [elm-community][elm-community].
So why bother with this one?

Bogdanp did a great job.
Unfortunately, date and time handling is complicated.
Many cases cannot be covered with `elm-time`, like [leap seconds][leap-second].
Also, it only supports the [(proleptic) gregorian calendar][gregorian] and is hard to extend.

This library tries to stay close to the API from `elm-time` where this makes sense.
But due to the nature of things, it will not be a drop-in replacement.
In case you are wondering now if you should start with the simple library and switch if you ever need it: don't.
Sooner or later, all the little edge cases creep up.
If you don't believe me, read a little bit about [week][week-numbering] [numbering][java-week-fields].


## (Planned) Features
Many features are planned for this library.
I doubt I can manage them all, so if you feel like solving something, message me.
In general, I take inspiration from the [Java Time API][java-time].
The API as such is of course object-oriented, but it is also very feature-rich.
So I strive to get in as many features as possible, staying close to their terminology.

- [ ] Basic data types for date and time
  - local, with offset, or with time zone
  - support for gregorian calendar
  - extensible for other systems
- [ ] Include other calendar systems
- [ ] A flexible parser system
  - support ISO 8601 including variants
  - allow to customize/combine parser or write new ones
- [ ] A customizable formatter system
  - include ISO 8601 by default
  - allow full customization
  - (maybe) add internationalization


[elm-time]: https://github.com/elm-community/elm-time
[bogdanp]: https://github.com/Bogdanp
[elm-comunity]: https://github.com/elm-community
[leap-second]: https://en.wikipedia.org/wiki/Leap_second
[gregorian]: https://en.wikipedia.org/wiki/Gregorian_calendar
[week-numbering]: https://en.wikipedia.org/wiki/Week#Week_numbering
[java-week-fields]: https://docs.oracle.com/javase/8/docs/api/java/time/temporal/WeekFields.html
[java-time]: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html

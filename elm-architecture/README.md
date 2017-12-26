### **Elm guide ported to Purescript**


- User Input
  - [Buttons](src/Buttons)
  - [Text Fields](src/TextFields)
  - [Forms](src/Forms)

- Effects
  - [Random](src/Random)
  - [HTTP](src/HTTP)
  - [Time](src/Time)

---

Porting of all the sample applications available in the Elm guide https://guide.elm-lang.org/

Implemented with [purescript-spork](https://github.com/natefaubion/purescript-spork).


#### Running

Compile single application with `make`.

```
make random
```

Run a webserver with all the samples ready to go.

```
make serve
```

Visit http://localhost:8000/src/Random/

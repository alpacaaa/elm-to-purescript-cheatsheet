### **Elm guide ported to Purescript (with Spork)**


- User Input
  - [Buttons](src/Buttons/Buttons.purs)
  - [Text Fields](src/TextFields/TextFields.purs)
  - [Forms](src/Forms/Forms.purs)

- Effects
  - [Random](src/Random/Random.purs)
  - [HTTP](src/HTTP/Http.purs)
  - [Time](src/Time/Time.purs)

---

Porting of all the sample applications available in the Elm guide https://guide.elm-lang.org/

Implemented with [purescript-spork](https://github.com/natefaubion/purescript-spork).


#### Running

```
cd elm-architecture
bower install
make all
make serve
```

Visit http://localhost:8000/

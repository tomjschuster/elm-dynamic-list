var app = Elm.Main.fullscreen()

document.addEventListener('mouseleave', ({ pageX: x, pageY: y}) =>
  app.ports.mouseLeaves.send({ x, y })
)

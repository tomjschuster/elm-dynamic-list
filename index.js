var app = Elm.Main.fullscreen()

document.addEventListener('mouseleave', ({ pageX: x, pageY: y}) =>
  app.ports.mouseLeaves.send({ x, y })
)

window.addEventListener('resize', () =>
  app.ports.windowResize.send({
    width: window.innerWidth, height: window.innerHeight
  })
)

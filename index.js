var app = Elm.Main.fullscreen()

document.addEventListener('mouseleave', ({ pageX: x, pageY: y}) =>
  app.ports.mouseLeaves.send({ x, y })
)

window.addEventListener('resize', () => {
  const container = document.getElementById('dynamic-list-1')
  app.ports.windowResize.send(container.offsetWidth)
})

app.ports.getContainerWidth.subscribe(() => {
  const container = document.getElementById('dynamic-list-1')
  app.ports.windowResize.send(container.offsetWidth)
})


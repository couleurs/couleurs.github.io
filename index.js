var GLContext = require('./fluidwebgl/src/engine/gl/context')
var Clock = require('./fluidwebgl/src/engine/clock').Clock
var FluidSimulation = require('./fluidwebgl/src/main')

// Canvas
var canvas = document.createElement('canvas')
document.body.appendChild(canvas)

// GL Context
var gl = GLContext.initialize(canvas, {
          context: {
              depth: false
          },
          debug: false,

          extensions: {
              texture_float: true
          }
        },
        function() {})

var clock = new Clock(canvas)
clock.start()

var fluidSimulation = new FluidSimulation(gl, canvas)
fluidSimulation.start()

clock.ontick = function(dt) {
  if (fluidSimulation.loaded) {
    fluidSimulation.draw()
  }
}

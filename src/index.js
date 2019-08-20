import 'tachyons'
import './index.css'
import { Elm } from './Main.elm'

import forEachObjIndexed from 'ramda/es/forEachObjIndexed'
import isNil from 'ramda/es/isNil'
import mapObjIndexed from 'ramda/es/mapObjIndexed'
import path from 'ramda/es/path'
import propOr from 'ramda/es/propOr'
import identity from 'ramda/es/identity'
import equals from 'ramda/es/equals'
import { defineAmpCE } from './CustomComponents/AmpCE'
import { defineLazyImage } from './CustomComponents/LazyImage'
import { defineLoadMore } from './CustomComponents/LoadMore'


// INIT CustomElements
defineLazyImage()
defineLoadMore()

// INIT AmpCE
window.ampLoaded = () => {
  setTimeout(() => {
    console.debug('amp script loaded: defining AmpCE')
    defineAmpCE()
  }, 3000)
}

// INIT

const storageKey = 'elm-bms-movie-trailers-cache'
const app = Elm.Main.init({
  flags: {
    cache: JSON.parse(localStorage.getItem(storageKey) || 'null'),
    size: { width: window.innerWidth, height: window.innerHeight },
  },
})

const pubs = initPubs({})

initSubs({
  setCache: cache => {
    if (isNil(cache)) {
      localStorage.removeItem(storageKey)
    } else {
      localStorage.setItem(storageKey, JSON.stringify(cache))
    }
  },
})

function initSubs(subs) {
  forEachObjIndexed((listener, portName) => {
    const subscribe = path(['ports', portName, 'subscribe'])(app)
    if (!subscribe) {
      console.warn('Subscribe: Port Not Found:', portName)
      return
    }
    console.debug('Subscribe: Port Handler Attached', portName)
    subscribe(listener)
  })(subs)
  const ports = propOr({}, 'ports')(app)
  forEachObjIndexed((port, portName) => {
    if (port.subscribe && !subs[portName]) {
      console.warn('Subscribe: Port Handler Missing', portName)
    }
  })(ports)
}

function initPubs(pubs) {
  return mapObjIndexed((fn, portName) => {
    return arg => {
      const send = path(['ports', portName, 'send'])(app)
      if (!send) {
        console.warn('Send: Port Not Found:', portName, arg)
        return
      }
      if (send) {
        console.log('sending', arg)
        send(arg)
      }
    }
  })(pubs)
}

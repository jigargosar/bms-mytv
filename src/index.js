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

if (module.hot) {
  module.hot.accept(function() {
    location.reload()
  })
}

// CE LAZY IMAGE

customElements.define(
  'lazy-image',
  class extends HTMLElement {
    constructor() {
      super()
      this._io = new IntersectionObserver((entries, observer) => {
        entries.forEach(entry => {
          if (entry.isIntersecting && entry.target === this) {
            this.firstChild.src = this.src
            this._stopObserving()
          }
        })
      }, {})
      this.src = null
    }

    set src(src) {
      if (equals(src, this.src)) return
      this._startObserving()
      this._src = src
    }

    get src() {
      return this._src
    }

    connectedCallback() {
      this._startObserving()
    }

    disconnectedCallback() {
      this._stopObserving()
    }

    _startObserving() {
      this._io.observe(this)
    }

    _stopObserving() {
      this._io.unobserve(this)
    }
  },
)

// CE LOAD-MORE
customElements.define(
  'load-more',
  class extends HTMLElement {
    connectedCallback() {
      this._io = new IntersectionObserver((entries, observer) => {
        entries.forEach(entry => {
          console.debug(this.tagName, 'intersectionChanged', entry)
          this.dispatchEvent(
            new CustomEvent('intersectionChanged', {
              isIntersecting: entry.isIntersecting,
            }),
          )
        })
      }, {})
      this._io.observe(this)
    }

    disconnectedCallback() {
      this._io.unobserve(this)
    }
  },
)


// INIT AmpCE
window.ampLoaded = () => {
  setTimeout(() => {
    console.debug("amp script loaded: defining AmpCE")
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

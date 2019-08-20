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

// CE azure-media-player

customElements.define(
  'azure-media-player',
  class extends HTMLElement {
    constructor() {
      super()
      console.log('amp ce created')
    }

    set poster(poster) {
      this._poster = poster
    }
    get poster() {
      return this._poster
    }

    set src(src) {
      this._src = src
      if (
        this._src &&
        this._amp &&
        !(
          this._amp.options().sourceList &&
          this._amp.options().sourceList[0] &&
          this._amp.options().sourceList[0].src === src
        )
      ) {
        this._amp.src(
          
            {
              src: src,
              type: 'application/vnd.ms-sstr+xml',
            },

          [
            {
              kind: 'captions',
              src: '/subtitles.vtt',
              srclang: 'en',
              label: 'English',
            },
          ],
        )
      }
    }

    get src() {
      return this._src
    }

    connectedCallback() {
      if (this._amp) {
        this.src = this._src
        return
      }
      this.innerHTML = `<video         
        class="azuremediaplayer amp-default-skin"/>`
      this._amp = amp(
        this.firstChild,
        {
          /* Options */
          techOrder: [
            'azureHtml5JS',
            'flashSS',
            'html5FairPlayHLS',
            'silverlightSS',
            'html5',
          ],
          nativeControlsForTouch: false,
          autoplay: true,
          controls: true,
          // width: '345',
          // width: '100%',
          // height: '400',
          fluid: true,
          poster: this.poster,
          logo: { enabled: false },
        },
        function() {
          console.log('Good to go!')
          // // setTimeout(() => myPlayer.dispose(), 1000)
          // // add an event listener
          this.addEventListener('ended', function() {
            console.log('Finished!')
          })
        },
      )
      this.src = this._src
    }

    disconnectedCallback() {
      requestAnimationFrame(() => {
        if (this._amp && !this._amp.playerContainer().isConnected) {
          this._amp.dispose()
          this._amp = null
        }
      })
    }
  },
)

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
  localStorageSetJsonItem: ([k, v]) => {
    console.groupCollapsed('localStorageSetJsonItem', k)
    console.log(v)
    console.groupEnd()
    localStorage.setItem(k, JSON.stringify(v))
  },
  setCache: cache => {
    if (isNil(cache)) {
      localStorage.removeItem(storageKey)
    } else {
      localStorage.setItem(storageKey, JSON.stringify(cache))
    }
  },
  play: video => {
    // requestAnimationFrame(() => playVideo(video))
  },
  disposePlayer,
})

let myPlayer = null

function disposePlayer() {
  if (myPlayer) {
    myPlayer.dispose()
    myPlayer = null
  }
}

// function playVideo(video) {
//   disposePlayer()
//   let videoContainerID = video.id
//   let videoContainer = document.getElementById(videoContainerID)
//   if (!videoContainer) {
//     console.warn('Play Error domId Not Found', videoContainerID)
//     return
//   }
//   videoContainer.innerHTML = `<video
//         class="azuremediaplayer amp-default-skin"/>`
//   myPlayer = amp(
//     videoContainer.firstChild,
//     {
//       /* Options */
//       techOrder: [
//         'azureHtml5JS',
//         'flashSS',
//         'html5FairPlayHLS',
//         'silverlightSS',
//         'html5',
//       ],
//       nativeControlsForTouch: false,
//       autoplay: true,
//       controls: true,
//       // width: '345',
//       // width: '100%',
//       // height: '400',
//       fluid: true,
//       poster: video.imageUrl,
//       logo: { enabled: false },
//     },
//     function() {
//       console.log('Good to go!')
//       // setTimeout(() => myPlayer.dispose(), 1000)
//       // add an event listener
//       this.addEventListener('ended', function() {
//         console.log('Finished!')
//       })
//     },
//   )
//
//   myPlayer.src(
//     [
//       {
//         src: video.videoUrl,
//         type: 'application/vnd.ms-sstr+xml',
//       },
//     ],
//     [
//       {
//         kind: 'captions',
//         src: '/subtitles.vtt',
//         srclang: 'en',
//         label: 'English',
//       },
//     ],
//   )
// }

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

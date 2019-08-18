import 'tachyons'
import './index.css'
import { Elm } from './Main.elm'
import {
  forEachObjIndexed,
  isNil,
  mapObjIndexed,
  path,
  propOr,
} from 'ramda'

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
    requestAnimationFrame(() => playVideo(video))
  },
  disposePlayer
})

let myPlayer = null

function disposePlayer() {
  if (myPlayer) {
    myPlayer.dispose()
    myPlayer = null
  }
}

function playVideo(video) {
  disposePlayer()
  let videoContainerID = video.id
  let videoContainer = document.getElementById(videoContainerID)
  if (!videoContainer) {
    console.warn('Play Error domId Not Found', videoContainerID)
    return
  }
  // debugger
  // const width = videoContainer.getBoundingClientRect().width
  // const height =
  videoContainer.innerHTML =
    `<video         
        class="azuremediaplayer amp-default-skin"/>`
  myPlayer = amp(
    videoContainer.firstChild,
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
      fluid:true, 
      poster: video.imageUrl,
      logo: { enabled: false },
    },
    function() {
      console.log('Good to go!')
      // setTimeout(() => myPlayer.dispose(), 1000)
      // add an event listener
      this.addEventListener('ended', function() {
        console.log('Finished!')
      })
    },
  )
  
  myPlayer.src(
    [
      {
        src: video.videoUrl,
        type: 'application/vnd.ms-sstr+xml',
      },
    ],
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
        send(arg)
      }
    }
  })(pubs)
}

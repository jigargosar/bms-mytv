import 'tachyons'
import './index.css'
import { Elm } from './Main.elm'

import forEachObjIndexed from "ramda/es/forEachObjIndexed"
import isNil from "ramda/es/isNil"
import mapObjIndexed from "ramda/es/mapObjIndexed"
import path from "ramda/es/path"
import propOr from "ramda/es/propOr"
import identity from "ramda/es/identity"

const storageKey = 'elm-bms-movie-trailers-cache'
const app = Elm.Main.init({
  flags: {
    cache: JSON.parse(localStorage.getItem(storageKey) || 'null'),
    size: { width: window.innerWidth, height: window.innerHeight },
  },
})

const pubs = initPubs({ more: identity })

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
  disposePlayer,
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
  videoContainer.innerHTML = `<video         
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
      fluid: true,
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
        console.log('sending', arg)
        send(arg)
      }
    }
  })(pubs)
}

setTimeout(() => {
  const mb = document.getElementById('load-more-placeholder')

  let lazyImageObserver = new IntersectionObserver(
    function(entries, observer) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          // let lazyImage = entry.target;
          // lazyImage.src = lazyImage.dataset.src;
          // lazyImage.srcset = lazyImage.dataset.srcset;
          // lazyImage.classList.remove("lazy");
          // lazyImageObserver.unobserve(lazyImage);
          console.log('entry', entry)
          pubs.more('')
        }
      })
    },
    { rootMargin: '80%' },
  )

  lazyImageObserver.observe(mb)
}, 100)


if (module.hot) {
  module.hot.accept(function () {
    location.reload();
  });
}

// CE azure-media-player

import prop from 'ramda/es/prop'
import path from 'ramda/es/path'

function updateSrc(src, player) {
  const options = player.options()
  const getFirstSrc = path(['sourceList', 0, 'src'])
  if (getFirstSrc(options) !== src) {
    player.src(
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

export function defineAmpCE() {
  customElements.define(
    'azure-media-player',
    class AmpCE extends HTMLElement {
      set poster(poster) {
        this._poster = poster
      }

      get poster() {
        return this._poster
      }

      set src(src) {
        this._src = src
        this._player && updateSrc(src, this._player)
      }

      get src() {
        return this._src
      }

      connectedCallback() {
        // check for reconnection
        if (!this._player) {
          this.innerHTML = `<video class="azuremediaplayer amp-default-skin"/>`
          const videoEl = this.firstChild
          this._player = createPlayer(videoEl, this.poster)
        }

        updateSrc(this.src, this._player)
      }

      disconnectedCallback() {
        requestAnimationFrame(() => {
          if (this._player && !this._player.playerContainer().isConnected) {
            this._player.dispose()
            this._player = null
          }
        })
      }
    },
  )
}

function createPlayer(videoEl, poster) {
  return amp(videoEl, {
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
    poster: poster,
    logo: { enabled: false },
  })
}

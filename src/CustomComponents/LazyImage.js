// CE LAZY IMAGE

import equals from 'ramda/es/equals'

export function defineLazyImage() {
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
}

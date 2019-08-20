
export function defineLoadMore() {
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
}



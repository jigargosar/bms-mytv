* hosted at: https://bms-mytv.web.app

# Dev Setup
* npm install
* npm start

# TODO

- [x] publish on github
- [x] deploy on firebase
- [x] show request inflight placeholders
- [x] \* lazy load images
- [x] defer loading media player. it's delaying initial time to render.

- scroll video player into view, on play.
- Retry on Http & Image loading errors, i.e. on network issues.
- Clean up UI
  - [x] header
  - [x] video close btn
  - video summary pane

# Nice To Have: But Skipping for now :(

- Setup CI
- Replace pink placeholders with pretty animating ones.
- change thumbs to card layout, where title is party superimposed on thumb
  itself, by positioning it at bottom of title container. This will allow
  us to have fixed height per row. i.e. image height + 50px. if title is
  longer than 50px height, it will overlap thumb bottom.

- Reduce query payload; use firebase functions. since we don't use bulk of
  the response. And synopsis is only needed when playing video.

- Figure out how UI should look, on mobile/small screen.
  - it will take more time.
  - and mock video didn't include it.
  - But on small screen we should replace image thumb with video itself.



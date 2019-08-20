# TODO

[x] publish on github
[x] deploy on firebase
[x] show request inflight placeholders
[x] * lazy load images
* Clean up UI
    [x] header
    * video close btn
    * video summary pane
                    
* Setup CI
* defer loading media player. it's delaying initial time to render.
* Retry on Http & Image loading errors, i.e. on network issues.
* Replace pink placeholders with pretty animating ones.
* change thumbs to card layout, where title is party superimposed on 
    thumb itself, by positioning it at bottom of title container.
    This will allow us to have fixed height per row.
    i.e. image height + 50px. if title is longer than 50px height,
    it will overlap thumb bottom. 

# Skipping:

* Figure out how UI should look, on mobile/small screen.
    * it will take more time.
    * and mock video didn't include it.
    * But on small screen we should replace image thumb with video itself.

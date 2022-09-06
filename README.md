# climate_narrative_playground

The only purpose of this app is investigative (to test some features without cluttering the main repo), to be used only by developers.

## changes and observations 

### version 0.7
- reformatted text output box, added timestamps and make it cummulative
- added variant of async calculations that returns NULL, inspired by https://github.com/rstudio/promises/issues/46
- this indeed impacts timing of the update, but DOES NOT interfere with other concurrent instances
    (they work fine in both cases)
- added checkbox that enables debug in the future package
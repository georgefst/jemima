The contents of `cards` comes from [here](https://www.me.uk/cards/').

The SVGs don't load correctly on Ubuntu 16.04, so we use PNGs as a workaround.

We can use `ImageMagick` to populate `cards-png` (it may be necessary to modify `/etc/ImageMagick-6/policy.xml`):
> ls -l cards | hsio 'mapM_ ((\f -> callProcess "convert" ["cards/" ++ f ++ ".svg", "cards-png/" ++ f ++ ".png"]) . reverse . take 2 . drop 4 . reverse) . tail . lines'

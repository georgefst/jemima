The contents of `cards` comes from [here](https://www.me.uk/cards/).

The SVGs don't load correctly on Ubuntu 16.04, so we use PNGs as a workaround.

We can use `ImageMagick` to populate `cards-png` (it may be necessary to modify `/etc/ImageMagick-6/policy.xml`):
> find cards -type f -exec sh -c 'convert {} cards-png/$(basename {} .svg).png' \\;

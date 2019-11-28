# Ryokou

A scraper to get places data from [Japan Hoppers](https://www.japanhoppers.com)
in order to prepare my next vacation :japan: :jp:

## Building

Ryokou is written in [Haskell](https://www.haskell.org/).

First, install the [Stack build tool](haskellstack.org):

```
# Install Stack
curl -sSL https://get.haskellstack.org/ | sh
# Add the path to the stack executable to your PATH
export PATH=$PATH:$HOME/.local/bin
```

You will also need the development version of libcurl.
If you are on Debian/Ubuntu:
```
sudo apt install libcurl4-openssl-dev
```

Then compile the project:
```
stack build
```

The executables will be created in `.stack-work/install/<platform>/<stack version>/<ghc version>/bin/`

Alternatively, you can run them through stack:
```
stack exec <executable_name> -- <arguments>
```

## Using

###  Scrap places from Japan Hoppers

This script will scrap the Japan Hoppers for places to visit in a given region,
and write the data in JSON format.

```
stack exec scrap_places -- --region hokkaido --outfile hokkaido_places.json
```

###  Export places to a KML map

Using the JSON data, you can generate a KML format map that you can import in [Google MyMaps](https://www.google.com/maps/d)

```
stack exec export_kml -- --infile hokkaido_places.json --outfile hokkaido_map.kml
```

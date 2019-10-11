El Scorpio
==========

Setup
-----

This uses [Hugo](https://gohugo.io/) for static site generation. You will need a
hugo binary of at least version v0.54.

To Build
--------

To regenerate a deployable site just run `hugo` in the root directory and the
site will be exported to the `public` folder.

To Test
-------

To run a live reloading server on port 4747:

```sh
bin/hugo server -p 4747
```

To run a live reloading server and bind to a specific domain:

```sh
bin/hugo server --bind example.com -p 4747
```

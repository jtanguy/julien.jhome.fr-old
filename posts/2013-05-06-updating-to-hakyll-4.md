---
title: Updating to Hakyll 4
author: Julien Tanguy
description: Updating this blog to hakyll 4
tags: haskell, meta
---

Two months after the release of hakyll 4, I finally updated my blog hakyll-4.2.2.0. Let's view the major changes.

# Visual design

As you may have noticed, the design has changed. I took a lot of design ideas from <http://gist.io>.

There are also some small icons on the site header. These are provided by
[font awsome](http://fortawesome.github.io/Font-Awesome), an iconic font designed for Twitter Bootstrap. I may include more of these icons in the future.

# Projects page

As I work on some side projects (mainly LaTeX-related at the moment), I will publish them on the [projects page](/projects.html). The projects are put as submodules inside my `projects` folder, each one referencing the head of the `hakyll-pages` branch, _a la_ `gh-pages`.

# Under the hood

Hakyll has gone through _a lot_ of changes from version 3.0, and I won't list them all here.
There are plenty of [tutorials](http://jaspervdj.be/hakyll/tutorials.html) on Hakyll 4 (including migration guides).

## Monads, Monads everywhere

The main changes are the `Monad` Compilers, which lets us leverage the well-known `Control.Monad` we all love.
It makes things much simpler.

## Templating and contexts

Metadata fields in pages (for example the title, author, tags, etc.) now belong to a certain `Context`.
This makes templating easier _and_ more powerful, and there are already [some cool crazy websites](http://blog.clement.delafargue.name/posts/2013-04-03-web2day-powered-by-hakyll-part-1.html) implemented.

I have also some things in preparation, like adding a table of contents into contexts, and [other things I had already in mind](/posts/2012-12-05-making-my-blog.html#the-future).

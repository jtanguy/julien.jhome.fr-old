---
title: Making my blog
author: Julien Tanguy
date: 2012-12-04
description: Process of creating this site and blog.
tags: haskell
---

There has been quite some time since I wanted to open a blog.
My recent experiences with haskell and functional programming in general does not really apply for my Ph.D research.[^1]

Since I discovered haskell, I have been more and more sensible to things like *static typing* and *inference*, or managing side effects.
I also discovered some applications like [pandoc](http://johnmacfarlane.net/pandoc) and [hakyll](http://jaspervdj.be/hakyll).
The natural step was to use these tools to make this site.

[^1]: I work in the field of embedded software, where code size, memory size *and* speed are important. I also work at drier level, where a runtime environment can be quite cumbersome when the demand is high.

# Making the visual design

I have no particular skills in website design, so I tend to prefer minimalist ones.
I took a lot of ideas from [Steve Losh](http://stevelosh.com) and [Clément Delafargue](http://blog.clement.delafargue.name) blogs.
Maybe I'll improve by adding a defined colorscheme to it, but it is good as it is.

# Coming to the hakyll side

For the backend I chose to use [hakyll](http://jaspervdj.be/hakyll), a static site generator in haskell inspired by [jekyll](http://jekyllrb.com).
I like the way it integrates with the power of pandoc, allowing the use of literate haskell or \LaTeX input articles.
For now I just use pandoc's extended markdown, combined with a few hakyll-specific metadata.

Since hakyll is quite nicely designed and simple on its principle, I wanted to write only code that I did understand.
I took simple [examples from the github repo](http://github.com/jaspervdj/hakyll-examples) and tried to understand them *before* being integrated into this blog.
The [Hakyll documentation](http://jaspervdj.be/hakyll/reference) was quite useful for that purpose.

I will eventually release the source code when I will be satisfied with it.


# Managing the publishing flow

[Git](http://git-scm.com)

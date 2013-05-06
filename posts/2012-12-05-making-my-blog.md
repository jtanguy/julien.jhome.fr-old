---
title: Making my blog
author: Julien Tanguy
description: Process of creating this site and blog.
tags: haskell
---

There has been quite some time since I wanted to open a blog.
My recent experiences with haskell and functional programming in general does not really apply for my Ph.D research.[^1]

Since I discovered haskell, I have been more and more sensible to things like *static typing* and *inference*, or managing side effects.
I also discovered some applications like [pandoc](http://johnmacfarlane.net/pandoc) and [hakyll](http://jaspervdj.be/hakyll).
The natural step was to use these tools to make this site.

[^1]: I work in the field of embedded software, where code size, memory size *and* speed are important. I also work at driver level, where a runtime environment can be quite cumbersome when the demand is high.

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

Let us go through some iterations of this process.

The entry point for all hakyll websites is the `hakyll` function: 

~~~{.haskell}
main :: IO ()
main = hakyll $ do
~~~~

Everything that follows is a set of `rules`, or actions to perform.

## Starting from the `Brochure` example

The simplest example we can find in `hakyll-examples` is the *brochure* example.
It manages static pages, templates and auxiliary resources, like css.

In the example, the explicit list of pages is encoded into the site source code. Let's improve this by compiling any file under some directory, let's say `pages`.

~~~{.haskell}
match "pages/*" $ do                                            -- For all pages found in the `pages` directory
    route $ setExtension "html"                                 -- Change the extension to html
    compile $ pageCompiler                                      -- We compile the file through the page compiler
            >>> applyTemplateCompiler "templates/master.html"   -- insert the result in a template
            >>> relativizeUrlsCompiler                          -- make all local links relative, according to the routing
~~~~

One central element for hakyll is the `Page`. It is basically a body and some metadata, like a title, a date, or an author.
When compiled, the metadata at the beginning of the input file are extracted, and the body is rendered by pandoc to form a html string.

On the other hand, hakyll has the notion of `Template`. It is a html file with some special named keys, like `$$title$$` or `$$body$$`.
When applying a template for a page, all metadata contents are inserted in place of the corresponding key.
The contents of the file are inserted in the `$$body$$` key.

We can combine the previous two thing because all the different compilers --- `pageCompiler`, `applyTemplateCompiler` and `relativizeUrlsCompiler` --- are *arrows*.
Arrows can be viewed as a machine which has an input and an output.
The `Control.Arrow` package defines a bunch of functions to compose, and more generally manipulate arrows.
The `>>>` operator, for instance, is basically a pipe plugging the output of the last compiler to the next one.

Then you need to handle two more types of files, namely resources and templates.
The templates are just compiled, so they can be reused for pages, and resources files like css are just copied as is.

~~~{.haskell}
match "css/*" $ do
    route idRoute
    compile copyFileCompiler

match "templates/*" $ compile templateCompiler
~~~~

There is one more thing we didn't take into account: all pages are routed to `pages/*`, but our entrypoint for the website is the current directory.
We just add a new rule to the routing which removes the `pages/` part of the route.

~~~{.haskell}
route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
~~~~

And *voila*! We have a static website which compiles pages, which is not much.
But it is a necessary step to fully understand how a hakyll site is made, and we will be expanding upon these fundamentals.

## Adding blog entries

Since we *do* want to make a blog, let's make some blog entries.
For this purpose, we will put all blog entries in the `posts/` directory.
We will add also the requirement that the file names must repect the hakyll naming convention, which is `YYYY-MM-DD-lowercase-title-separated-by-hypens.ext`, where `YYYY-MM-DD` is the date of the article, and `ext` can be any extension recognized by pandoc.
I tend to prefer writing my entries in pandoc's extended markdown, so I use `md`.

For blog entries, we want to use some semantic markup, so we will be using a special template for this.

~~~{.haskell}
match "posts/*" $ do
    route $ setExtension "html"
    compile $ pageCompiler
        >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
        >>> arr (renderDateField "machinedate" (iso8601DateFormat Nothing) "")
        >>> applyTemplateCompiler "templates/article.html"
        >>> applyTemplateCompiler "templates/master.html"
        >>> relativizeUrlsCompiler
~~~~

Here nothing espacially fancy, we compile the files, add the date in human- and machine-readable formats, apply the special article and the master templates, and finally we make sure that all local links are correctly routed.

For now, we can only access blog entries if we know the exact route. That's not very practical, so next we will add an entry archive next.

## Archiving

The archive page can be achieved by hand, by maintaining the list of all blog entries. But this is quite tedious and error prone, so we generate this list automatically.
This can be achieved simply with a somewhat recent rules, located inside `Hakyll.Web.Page.List`.

~~~{.haskell}
match "archive.html" $ do  -- We match the special route archive.html
    route idRoute
    create "archive.html" $ constA mempty -- Create a new page, starting from an empty page
         >>> arr (setField "title" "Blog archive") -- Add a title metadata
         >>> setFieldPageList recentFirst "templates/archive-item.html" "posts" "posts/*" -- Add a list of posts in $posts$, applying the archive-item template to each
         >>> applyTemplateCompiler "templates/archive.html"
         >>> applyTemplateCompiler "templates/master.html"
         >>> relativizeUrlsCompiler
~~~~

The archive rule tells hakyll to create a new empty page, fill in some metadata, add a compiled list of posts matching a certain pattern --- here we have `posts/*` ---
in the `$$posts$$` key, and apply templates and post-processing like we saw before.

## Tagging

To help grouping related posts, we want to add some tags to posts.
We will add this into the medatada --- for example this entry is tagged with [haskell](/tags/haskell.html), obviously.

We will want to add two features:
- make a list of all posts tagged for a specific tag, for every tag, and
- add a link to the tag list page in blog entries.

For the first objective, we must proceed in two steps (to manage easily the dependancy cycle):
First we list all tags using a virtual `tags` create rule, which basically explore all entries.
Then we create a list of compilers, one for each tag, which creates a page list, in the same way we create the blog archive.
I won't go in the details here[^2], but you can do it as an exercice if you want.

~~~{.haskell}
create "tags" $ requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

match "tags/*" $ route $ setExtension ".html"
metaCompile $ require_ "tags" -- The "metaCompiler" creates other compilers.
    >>> arr tagsMap
    >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
  where 
    tagIdentifier = fromCapture "tags/*"
~~~~

[^2]: It is not that I don't want to explain, but my understanding of it is just enough for me. I am not sure I can describe it here.

## Custom links on static pages

The last thing we will add here is a short list of recent entries on some static pages.
For example, we would like to list the three newest entries on the home page, and the three newest entries tagged as *research* on the research page.

Here we use a little trick, which unfortunately implies to add explicitely the path of all static pages for which we want to add a short list.

~~~{.haskell}
match "pages/*" $ do
    route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
    compile $ pageCompiler
        >>> byPattern (arr id) -- Add an additional step if we follow some pattern, else apply the `id` compiler
            [ ("pages/index.md", addRelatedToAs "posts/*" "Recent blog entries")
            , ("pages/research.md", addRelatedToAs "tags/research" "Related recent blog entries")
            ]
        >>> applyTemplateCompiler "templates/master.html"
        >>> relativizeUrlsCompiler

  where
    -- This auxiliary compiler adds the three newest entries of a given pattern, with a given title.
    addRelatedToAs :: Pattern (Page String) -> String -> Compiler (Page String) (Page String)
    addRelatedToAs p t = setFieldPageList (take 3 . recentFirst) "templates/archive-item.html" "posts" p
        >>> arr (setField "related" t)
        >>> applyTemplateCompiler "templates/related.html"
~~~~

I would like to go more in details about how I ended up with this solution --- I first tried something with `ArrowIf` structures, with no success ---, but it is already late and I am too tired to explain it right now. Maybe I'll explain more later.

# The future

For now I have a satisfying working blog, but I would like to add automatic references, including compiling the bibliography if there is a `.bib` file with the same name as the current page. ~~I will also eventually put the source code of this blog on github.~~

**Update :** The source code of this blog is [on github](http://github.com/jtanguy/julien.jhome.fr)


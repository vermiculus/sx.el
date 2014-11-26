Stack-Mode
===
[![Build Status](https://api.travis-ci.org/vermiculus/stack-mode.svg?branch=master)](https://travis-ci.org/vermiculus/stack-mode)
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/vermiculus/stack-mode?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Stories in Ready](https://badge.waffle.io/vermiculus/stack-mode.svg?label=ready&title=Ready)](http://waffle.io/vermiculus/stack-mode)

Stack-Mode will be a full featured Stack Exchange mode for GNU Emacs 24
and up. Using the official API, we aim to create a more versatile
experience for the Stack Exchange network within Emacs itself.

# Features<a id="sec-1" name="sec-1"></a>

<b>Listing Posts</b><br/>
<kbd>M-x</kbd>`list-questions` - List questions on a StackExchange site.

<b>Navigating Posts</b><br/>
<kbd>n</kbd> - Moves to the next question from within `list-questions`.<br/>
<kbd>p</kbd> - Moves to the previous question from within `list-questions`.<br/>

<b>Viewing Posts</b><br/>
<kbd>j</kbd> - Moves to the next question and opens it.<br/>
<kbd>k</kbd> - Moves to the previous question and opens it.<br/>
<kbd>RET</kbd> - Opens the question.<br/>
<kbd>v</kbd> - Opens the site in your browser.<br/>
<kbd>TAB</kbd> - Folds questions and answers.<br/>

## Planned<a id="sec-1-1" name="sec-1-1"></a>

-   Archiving questions for offline access
-   Browsing and favoriting networks
-   Advanced searching
-   Writing questions, answers, and comments (with source code in its
    native major mode)
-   Notifications
-   Reputation reporting
-   And much more!

Have a feature in mind that isn't on the list?  Submit a pull request
to add it to the list!  If you want to discuss it first, pop in our
Gitter chatroom (badge above) &#x2013; someone will be around shortly to
talk about it.

# Installation<a id="sec-2" name="sec-2"></a>

To install the development version, follow the usual steps:
-   Clone this repository
-   Add this directory to your `load-path`
-   Issue `(require 'sx-question-list)`

This should give you access to the only entry point function at the
moment, `list-questions`.

Eventually, this package will at least be available on MELPA.
Depending on community involvement, it may even be submitted to the
official GNU ELPA.

# Contributing<a id="sec-3" name="sec-3"></a>

Please help contribute! Doing any of the following will help us immensely:
-   [Open an issue](https://github.com/vermiculus/stack-mode/issues/new)
-   [Submit a pull request](https://github.com/vermiculus/stack-mode/pulls)
-   [Suggest a package or library in our Chat on Gitter](https://gitter.im/vermiculus/stack-mode)
-   Spread the word!

For a better view of all of the open issues, take a look at our lovely
[Waffle board](http://www.waffle.io/vermiculus/stack-mode).  Feel free to take the torch on anything in `backlog` or
`ready`.  If you have thoughts on any other issues, don't hesitate to
chime in!

# Resources<a id="sec-4" name="sec-4"></a>

-   [GNU Emacs](http://www.gnu.org/software/emacs/)
-   [Stack Exchange API v2.2](https://api.stackexchange.com/docs)
-   [StackApps Registration Page](http://stackapps.com/apps/oauth/register)
-   [Creating Major Modes for Emacs](http://www.emacswiki.org/emacs/ModeTutorial)

## Icons<a id="sec-4-1" name="sec-4-1"></a>

Stack Exchange Mode for Emacs has no explicit use for an icon,
although standard SVG files have been gathered in `resources/` if
anyone would fancy a crack at it.

-   [Emacs icon](resources/emacs.svg)
-   [Stack Exchange icon](resources/stackexchange.svg)

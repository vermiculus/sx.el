[((body . "<p>How can I use the focus hooks to attenuate all colours in visible buffers when the Emacs frame loses focus?</p>\n\n<p>I tried using the following code:</p>\n\n<pre><code>(set-frame-parameter (selected-frame) 'alpha '(100 80))\n</code></pre>\n\n<p>To make the frame become translucent, but it would flicker when I hit <kbd>Ctrl</kbd> (apparently that's because I let GNOME highlight the mouse cursor when I hit <kbd>Ctrl</kbd>).</p>\n\n<p>Making the frame transparent isn't what I want anyway.  Is it possible to desaturate all colours instead?</p>\n")
  (title . "Focus-hook: attenuate colours when losing focus")
  (link . "http://emacs.stackexchange.com/questions/2922/focus-hook-attenuate-colours-when-losing-focus")
  (body_markdown . "How can I use the focus hooks to attenuate all colours in visible buffers when the Emacs frame loses focus?\n\nI tried using the following code:\n\n    (set-frame-parameter (selected-frame) &#39;alpha &#39;(100 80))\n\nTo make the frame become translucent, but it would flicker when I hit &lt;kbd&gt;Ctrl&lt;/kbd&gt; (apparently that&#39;s because I let GNOME highlight the mouse cursor when I hit &lt;kbd&gt;Ctrl&lt;/kbd&gt;).\n\nMaking the frame transparent isn&#39;t what I want anyway.  Is it possible to desaturate all colours instead?")
  (question_id . 2922)
  (last_edit_date . 1414793447.0)
  (creation_date . 1414774490.0)
  (last_activity_date . 1414793447.0)
  (score . 1)
  (answer_count . 0)
  (favorite_count . 0)
  (view_count . 17)
  (is_answered . :json-false)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 1)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/2005/rekado")
   (display_name . "rekado")
   (profile_image . "https://www.gravatar.com/avatar/1fd94c0bb560b4445acb97f05b0e9a81?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2005)
   (reputation . 853))
  (comments .
            [((body . "While this Emacs feature sounds undeniably cool, I think it would be better to leave such a thing to the compositor you&#39;re using, <code>compton</code> for instance can dim inactive windows.")
              (link . "http://emacs.stackexchange.com/questions/2922/focus-hook-attenuate-colours-when-losing-focus#comment4000_2922")
              (body_markdown . "While this Emacs feature sounds undeniably cool, I think it would be better to leave such a thing to the compositor you&#39;re using, `compton` for instance can dim inactive windows.")
              (comment_id . 4000)
              (post_id . 2922)
              (creation_date . 1414776823.0)
              (post_type . "question")
              (score . 1)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/10/wasamasa")
               (display_name . "wasamasa")
               (profile_image . "https://www.gravatar.com/avatar/1504319df63de7148c39290d4149f150?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 10)
               (reputation . 860)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2005/rekado")
   (display_name . "rekado")
   (profile_image . "https://www.gravatar.com/avatar/1fd94c0bb560b4445acb97f05b0e9a81?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2005)
   (reputation . 853))
  (tags .
        ["frames" "hooks" "focus"]))
 ((body . "<p><strong>TLDR: I've got a large tags file that loads multiple times per session, making it unusable.  Can't find the source of issue or how to disable it -- main goal is to jump to tags, but completion would be nice</strong></p>\n\n<p>I recently posted on the emacs subreddit and a user recommended that I join this group because it's also friendly towards newbies.</p>\n\n<p>I've been using emacs as a C++ IDE, and I'm slowly correcting things that are annoying.  I'm essentially using <a href=\"https://github.com/redguardtoo/emacs.d\" rel=\"nofollow\">redguardtoo's emacs.d file</a>.</p>\n\n<p>I have a tag file that is 225MB (1.6M Tags).  My intent for the tag is not necessarily completion (using <code>company</code>), but more for jumping around source code.  I'm working with an unfamiliar code base and often need to see macro or class definitions.</p>\n\n<p>When I start typing, at random times, I'll see a message in the minibuffer</p>\n\n<pre><code>Making tag completion table for [tag file]...0-100%\n</code></pre>\n\n<p>Then, a popup listbox shows up with autocompletion options.</p>\n\n<p>Because this tag file is so large, and it doesn't seem like the tag loading is asynchronous, it freezes emacs for around a minute.  This also happens multiple times per a session (with the same file, and with other files in the same directory that use the same tag file)!</p>\n\n<hr>\n\n<p><strong>Questions</strong></p>\n\n<ol>\n<li>Does anyone know what's initializing the loading of the tag table for completion and how to disable it?  I did a grep on my emacs.d directory for \"Making tag completion\" and found nothing (note, IIRC the loading tags was present regardless of using <code>company</code> or <code>auto-complete</code></li>\n<li>Completion would be nice, but I'd settle just for the ability to jump to the tag location.  Is there a way to get this? </li>\n<li>What is the mindset to take when approaching problems like this?  I'd like to be able to troubleshoot further on my own next time.</li>\n</ol>\n\n<hr>\n\n<p><strong>Additional info</strong></p>\n\n<p>Enabled minor modes (C-h m)</p>\n\n<pre><code>Enabled minor modes: Abbrev Auto-Composition Auto-Compression\nAuto-Encryption Blink-Cursor Column-Number Company Desktop-Save\nDisplay-Time Electric-Indent Electric-Pair Evil Evil-Local\nEvil-Matchit Evil-Surround Fic File-Name-Shadow Flyspell-Lazy\nFont-Lock Ggtags Global-Auto-Revert Global-Company Global-Eldoc\nGlobal-Evil-Matchit Global-Evil-Surround Global-Font-Lock Global-Linum\nGlobal-Page-Break-Lines Global-Pointback Global-Undo-Tree Helm\nHelm-Match-Plugin Helm-Occur-Match-Plugin Line-Number Linum Menu-Bar\nMouse-Wheel Override-Global Pointback Recentf Savehist Shell-Dirtrack\nSubword Tooltip Undo-Tree Which-Function Window-Numbering Winner Yas\nYas-Global\n</code></pre>\n")
  (title . "&quot;Making tag completion table&quot; Freezes/Blocks -- how to disable")
  (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable")
  (body_markdown . "**TLDR: I&#39;ve got a large tags file that loads multiple times per session, making it unusable.  Can&#39;t find the source of issue or how to disable it -- main goal is to jump to tags, but completion would be nice**\n\nI recently posted on the emacs subreddit and a user recommended that I join this group because it&#39;s also friendly towards newbies.\n\nI&#39;ve been using emacs as a C++ IDE, and I&#39;m slowly correcting things that are annoying.  I&#39;m essentially using [redguardtoo&#39;s emacs.d file][1].\n\nI have a tag file that is 225MB (1.6M Tags).  My intent for the tag is not necessarily completion (using `company`), but more for jumping around source code.  I&#39;m working with an unfamiliar code base and often need to see macro or class definitions.\n\nWhen I start typing, at random times, I&#39;ll see a message in the minibuffer\n\n    Making tag completion table for [tag file]...0-100%\n\nThen, a popup listbox shows up with autocompletion options.\n\nBecause this tag file is so large, and it doesn&#39;t seem like the tag loading is asynchronous, it freezes emacs for around a minute.  This also happens multiple times per a session (with the same file, and with other files in the same directory that use the same tag file)!\n\n---\n\n**Questions**\n\n 1. Does anyone know what&#39;s initializing the loading of the tag table for completion and how to disable it?  I did a grep on my emacs.d directory for &quot;Making tag completion&quot; and found nothing (note, IIRC the loading tags was present regardless of using `company` or `auto-complete`\n 2. Completion would be nice, but I&#39;d settle just for the ability to jump to the tag location.  Is there a way to get this? \n 3. What is the mindset to take when approaching problems like this?  I&#39;d like to be able to troubleshoot further on my own next time.\n\n  [1]: https://github.com/redguardtoo/emacs.d\n\n---\n\n**Additional info**\n\nEnabled minor modes (C-h m)\n\n    Enabled minor modes: Abbrev Auto-Composition Auto-Compression\n    Auto-Encryption Blink-Cursor Column-Number Company Desktop-Save\n    Display-Time Electric-Indent Electric-Pair Evil Evil-Local\n    Evil-Matchit Evil-Surround Fic File-Name-Shadow Flyspell-Lazy\n    Font-Lock Ggtags Global-Auto-Revert Global-Company Global-Eldoc\n    Global-Evil-Matchit Global-Evil-Surround Global-Font-Lock Global-Linum\n    Global-Page-Break-Lines Global-Pointback Global-Undo-Tree Helm\n    Helm-Match-Plugin Helm-Occur-Match-Plugin Line-Number Linum Menu-Bar\n    Mouse-Wheel Override-Global Pointback Recentf Savehist Shell-Dirtrack\n    Subword Tooltip Undo-Tree Which-Function Window-Numbering Winner Yas\n    Yas-Global\n\n")
  (question_id . 2919)
  (creation_date . 1414770468.0)
  (last_activity_date . 1414793088.0)
  (score . 2)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 19)
  (is_answered . :json-false)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 10)
  (comments .
            [((body . "For jumping to definition/references, you can use GNU Global with ggtags/helm-gtags. Guarantee to work on large project like Linux kernel without any delay. You may want to look at my <a href=\"http://tuhdo.github.io/c-ide.html\" rel=\"nofollow\">C/C++ guide</a>. I covered code navigation (jump to definition/references), code completion, compiling and debugging support. I already created a demo configuration for playing with, so you only need to walk through the features without configuring anything.")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment3992_2919")
              (body_markdown . "For jumping to definition/references, you can use GNU Global with ggtags/helm-gtags. Guarantee to work on large project like Linux kernel without any delay. You may want to look at my [C/C++ guide](http://tuhdo.github.io/c-ide.html). I covered code navigation (jump to definition/references), code completion, compiling and debugging support. I already created a demo configuration for playing with, so you only need to walk through the features without configuring anything.")
              (comment_id . 3992)
              (post_id . 2919)
              (creation_date . 1414770948.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))
             ((body . "Your guides were the ones that got me started with emacs (especially w/ C++)! Our build system is very complicated and <code>#include &quot;...&quot;</code> can live in various places that are not known until build time.  So, ggtags wouldn&#39;t be able to generate tags for many of the files.  Currently, our build tools allow us to <code>make etags</code> to generate emacs compatible tags, but there is no <code>make ggtags</code> equivalent.  So, I&#39;m stuck with etags for now.  Any ideas?")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment3993_2919")
              (body_markdown . "Your guides were the ones that got me started with emacs (especially w/ C++)! Our build system is very complicated and `#include &quot;...&quot;` can live in various places that are not known until build time.  So, ggtags wouldn&#39;t be able to generate tags for many of the files.  Currently, our build tools allow us to `make etags` to generate emacs compatible tags, but there is no `make ggtags` equivalent.  So, I&#39;m stuck with etags for now.  Any ideas?")
              (comment_id . 3993)
              (post_id . 2919)
              (creation_date . 1414772414.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652))
              (owner
               (link . "http://emacs.stackexchange.com/users/2265/cheezy")
               (display_name . "cheezy")
               (profile_image . "https://www.gravatar.com/avatar/a2e453f8f1cfc1ff0d7428c3b66ddc9a?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2265)
               (reputation . 11)))
             ((body . "Hmm I see. The situation is more complicated now. You mentioned <code>ggtags</code> cannot generate tags, but did you use plain <code>gtags</code> on the command line at project root? You should try this and invoke ggtags again. There should not be <code>make ggtags</code> because the real command is <code>gtags</code>; <code>ggtags</code> is just a package name of Emacs that use it. Another option is that you can use <a href=\"http://ctags.sourceforge.net/\" rel=\"nofollow\">ctags</a>; <code>ggtags</code> can also recognize <code>ctags</code> tags and it&#39;s also fast. You can generate ctags tags when invoking <code>ggtags-create-tags</code> and it asks for using <code>ctags</code> client.")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment3995_2919")
              (body_markdown . "Hmm I see. The situation is more complicated now. You mentioned `ggtags` cannot generate tags, but did you use plain `gtags` on the command line at project root? You should try this and invoke ggtags again. There should not be `make ggtags` because the real command is `gtags`; `ggtags` is just a package name of Emacs that use it. Another option is that you can use [ctags](http://ctags.sourceforge.net/); `ggtags` can also recognize `ctags` tags and it&#39;s also fast. You can generate ctags tags when invoking `ggtags-create-tags` and it asks for using `ctags` client.")
              (comment_id . 3995)
              (post_id . 2919)
              (creation_date . 1414773753.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))
             ((body . "Anyway, if <code>gtags</code> could not know about such details, I don&#39;t think the built-in <code>etags</code> (comes with Emacs), or <code>ctags</code> (I linked above) would be able to do it. I think you only use <code>ggtags</code> to create tags at current directory, not project root. Run the command <code>gtags</code> at project root, or when <code>ggtags</code> asks for where to generate, navigate to project root and you will be fine.")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment3996_2919")
              (body_markdown . "Anyway, if `gtags` could not know about such details, I don&#39;t think the built-in `etags` (comes with Emacs), or `ctags` (I linked above) would be able to do it. I think you only use `ggtags` to create tags at current directory, not project root. Run the command `gtags` at project root, or when `ggtags` asks for where to generate, navigate to project root and you will be fine.")
              (comment_id . 3996)
              (post_id . 2919)
              (creation_date . 1414774141.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))
             ((body . "I verified your statement (all the tag programs can only generate from project root down the tree).  <code>make etags</code> works because it calls etags once it evaluates where all the <code>.h</code> files live.")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment4001_2919")
              (body_markdown . "I verified your statement (all the tag programs can only generate from project root down the tree).  `make etags` works because it calls etags once it evaluates where all the `.h` files live.")
              (comment_id . 4001)
              (post_id . 2919)
              (creation_date . 1414777453.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652))
              (owner
               (link . "http://emacs.stackexchange.com/users/2265/cheezy")
               (display_name . "cheezy")
               (profile_image . "https://www.gravatar.com/avatar/a2e453f8f1cfc1ff0d7428c3b66ddc9a?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2265)
               (reputation . 11)))
             ((body . "So, it did work? If so, may I turn my comment into answer?")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment4002_2919")
              (body_markdown . "So, it did work? If so, may I turn my comment into answer?")
              (comment_id . 4002)
              (post_id . 2919)
              (creation_date . 1414777545.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))
             ((body . "So, if I&#39;m stuck with etags for completeness, do you have any suggestions for 1) Loading the tag file asynchronously 2) Disable autoreload of the tags file?")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment4003_2919")
              (body_markdown . "So, if I&#39;m stuck with etags for completeness, do you have any suggestions for 1) Loading the tag file asynchronously 2) Disable autoreload of the tags file?")
              (comment_id . 4003)
              (post_id . 2919)
              (creation_date . 1414778081.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652))
              (owner
               (link . "http://emacs.stackexchange.com/users/2265/cheezy")
               (display_name . "cheezy")
               (profile_image . "https://www.gravatar.com/avatar/a2e453f8f1cfc1ff0d7428c3b66ddc9a?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2265)
               (reputation . 11)))
             ((body . "Sorry I wasn&#39;t very clear.  Without involving the <code>make</code> command, any tag generation is incomplete.  You can turn your comment into an answer, and I can accept it in a day or two to give others a chance to answer")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment4004_2919")
              (body_markdown . "Sorry I wasn&#39;t very clear.  Without involving the `make` command, any tag generation is incomplete.  You can turn your comment into an answer, and I can accept it in a day or two to give others a chance to answer")
              (comment_id . 4004)
              (post_id . 2919)
              (creation_date . 1414778190.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652))
              (owner
               (link . "http://emacs.stackexchange.com/users/2265/cheezy")
               (display_name . "cheezy")
               (profile_image . "https://www.gravatar.com/avatar/a2e453f8f1cfc1ff0d7428c3b66ddc9a?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2265)
               (reputation . 11)))
             ((body . "There&#39;s one possibility, not sure if you try: since you already have you TAGS file at project root (generated by <code>etags</code>), <code>ggtags</code> will use that file instead of GTAGS. Did you move the TAGS file (generated by <code>etags</code>) elsewhere? Personally, I use <code>helm-gtags</code> + GTAGS and always jump instantly, even in the Linux kernel that <code>etags</code> generates almost 1 GB. You mention that only some &quot;.h&quot; files available at build time, do you mean those files only generated after the build? Even if it is the case, it&#39;s hard to think that any tag program misses those if search recursively.")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment4005_2919")
              (body_markdown . "There&#39;s one possibility, not sure if you try: since you already have you TAGS file at project root (generated by `etags`), `ggtags` will use that file instead of GTAGS. Did you move the TAGS file (generated by `etags`) elsewhere? Personally, I use `helm-gtags` + GTAGS and always jump instantly, even in the Linux kernel that `etags` generates almost 1 GB. You mention that only some &quot;.h&quot; files available at build time, do you mean those files only generated after the build? Even if it is the case, it&#39;s hard to think that any tag program misses those if search recursively.")
              (comment_id . 4005)
              (post_id . 2919)
              (creation_date . 1414779434.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))
             ((body . "Thanks! I saw your post on helm gtags and this&#39;ll work for me for now!  Please create an answer so I can accept it :)")
              (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable#comment4009_2919")
              (body_markdown . "Thanks! I saw your post on helm gtags and this&#39;ll work for me for now!  Please create an answer so I can accept it :)")
              (comment_id . 4009)
              (post_id . 2919)
              (creation_date . 1414790397.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652))
              (owner
               (link . "http://emacs.stackexchange.com/users/2265/cheezy")
               (display_name . "cheezy")
               (profile_image . "https://www.gravatar.com/avatar/a2e453f8f1cfc1ff0d7428c3b66ddc9a?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2265)
               (reputation . 11)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2265/cheezy")
   (display_name . "cheezy")
   (profile_image . "https://www.gravatar.com/avatar/a2e453f8f1cfc1ff0d7428c3b66ddc9a?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2265)
   (reputation . 11))
  (tags .
        ["autocomplete" "performance" "ctags"])
  (answers .
           [((body . "<p>For your use case, there's still hope though. Since you use <code>etags</code>, it can be used with <code>helm-etags-select</code>, the Helm built-in command. To use it, simply follow theses steps:</p>\n\n<ul>\n<li>First, run the command to generate TAGS file.</li>\n<li>Second, use <code>find-tag</code> to feed it to Emacs; if the TAGS file is too large and Emacs asks you to confirm, just accept it. Your whole TAGS file will be loaded in Emacs and there's no more reloading.</li>\n<li>Finally, just execute <code>helm-etags-select</code> on any symbol on your Emacs. If there exists only one definition in your project, jump instantly; otherwise, display a  Helm buffer for you to choose from.</li>\n</ul>\n")
             (title . "&quot;Making tag completion table&quot; Freezes/Blocks -- how to disable")
             (link . "http://emacs.stackexchange.com/questions/2919/making-tag-completion-table-freezes-blocks-how-to-disable/2936#2936")
             (body_markdown . "For your use case, there&#39;s still hope though. Since you use `etags`, it can be used with `helm-etags-select`, the Helm built-in command. To use it, simply follow theses steps:\n\n- First, run the command to generate TAGS file.\n- Second, use `find-tag` to feed it to Emacs; if the TAGS file is too large and Emacs asks you to confirm, just accept it. Your whole TAGS file will be loaded in Emacs and there&#39;s no more reloading.\n- Finally, just execute `helm-etags-select` on any symbol on your Emacs. If there exists only one definition in your project, jump instantly; otherwise, display a  Helm buffer for you to choose from.")
             (question_id . 2919)
             (answer_id . 2936)
             (creation_date . 1414793088.0)
             (last_activity_date . 1414793088.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/200/tu-do")
              (display_name . "Tu Do")
              (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
              (accept_rate . 75)
              (user_type . "registered")
              (user_id . 200)
              (reputation . 652)))]))
 ((body . "<p>I've accidentally run the following:</p>\n\n<pre><code>(unintern variable)\n</code></pre>\n\n<p>where <code>variable</code>'s value was <code>nil</code>.</p>\n\n<p>How do I get <code>nil</code> back without restarting Emacs?</p>\n")
  (title . "How can I bring back `nil`?")
  (link . "http://emacs.stackexchange.com/questions/2935/how-can-i-bring-back-nil")
  (body_markdown . "I&#39;ve accidentally run the following:\n\n    (unintern variable)\n\nwhere `variable`&#39;s value was `nil`.\n\nHow do I get `nil` back without restarting Emacs?")
  (question_id . 2935)
  (creation_date . 1414792387.0)
  (last_activity_date . 1414792387.0)
  (score . 0)
  (answer_count . 0)
  (favorite_count . 0)
  (view_count . 5)
  (is_answered . :json-false)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/2264/sean-allred")
   (display_name . "Sean Allred")
   (profile_image . "https://www.gravatar.com/avatar/9261936847b5a31e15da6e86533d3de3?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2264)
   (reputation . 512))
  (tags .
        ["interactive-development"]))
 ((body . "<p>Is there a way to designate a specific frame in which files opened with <code>emacsclient</code> will appear?</p>\n\n<p>I have one monitor that is dedicated to a fullscreen Emacs frame.  I do most of my editing in that monitor, but from time to time I will open a second frame on another monitor temporarily.  I want to ensure that every time I open a file using <code>emacsclient</code>, it gets sent to my fullscreen frame and doesn't end up on any other frames that might be open.</p>\n\n<p><strong>Edit:</strong></p>\n\n<p>The ideal workflow would be that my <strong>initial</strong> emacs frame gets tagged as the recipient of all <code>emacsclient</code> requests, and the rest is automatic.  I always use the initial frame as my primary emacs frame, and other frames come and go as needed.</p>\n")
  (title . "Open edit-server files from emacsclient in a specific frame")
  (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame")
  (body_markdown . "Is there a way to designate a specific frame in which files opened with `emacsclient` will appear?\n\nI have one monitor that is dedicated to a fullscreen Emacs frame.  I do most of my editing in that monitor, but from time to time I will open a second frame on another monitor temporarily.  I want to ensure that every time I open a file using `emacsclient`, it gets sent to my fullscreen frame and doesn&#39;t end up on any other frames that might be open.\n\n**Edit:**\n\nThe ideal workflow would be that my **initial** emacs frame gets tagged as the recipient of all `emacsclient` requests, and the rest is automatic.  I always use the initial frame as my primary emacs frame, and other frames come and go as needed.")
  (question_id . 2417)
  (last_edit_date . 1413990607.0)
  (creation_date . 1413927107.0)
  (last_activity_date . 1414790834.0)
  (score . 4)
  (answer_count . 2)
  (favorite_count . 2)
  (view_count . 43)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 3)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/93/nispio")
   (display_name . "nispio")
   (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
   (accept_rate . 55)
   (user_type . "registered")
   (user_id . 93)
   (reputation . 1709))
  (comments .
            [((body . "Which operating system are you running?  Answers may vary if your window system is X, Windows, Mac OS.")
              (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame#comment3171_2417")
              (body_markdown . "Which operating system are you running?  Answers may vary if your window system is X, Windows, Mac OS.")
              (comment_id . 3171)
              (post_id . 2417)
              (creation_date . 1413954069.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/372/purple-arrows")
               (display_name . "purple_arrows")
               (profile_image . "https://www.gravatar.com/avatar/cb378ba79d85a04a9277ddc0e4259149?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 372)
               (reputation . 472)))
             ((body . "@purple_arrows Typically I am on Linux, but I also use Emacs on Mac and Windows.  Also, I would like to be able to send the file to a specific <b>frame</b> if possible, not just a specific display.")
              (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame#comment3200_2417")
              (body_markdown . "@purple_arrows Typically I am on Linux, but I also use Emacs on Mac and Windows.  Also, I would like to be able to send the file to a specific **frame** if possible, not just a specific display.")
              (comment_id . 3200)
              (post_id . 2417)
              (creation_date . 1413990313.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/372/purple-arrows")
               (display_name . "purple_arrows")
               (profile_image . "https://www.gravatar.com/avatar/cb378ba79d85a04a9277ddc0e4259149?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 372)
               (reputation . 472))
              (owner
               (link . "http://emacs.stackexchange.com/users/93/nispio")
               (display_name . "nispio")
               (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
               (accept_rate . 55)
               (user_type . "registered")
               (user_id . 93)
               (reputation . 1709)))
             ((body . "It&#39;ll take me a while to get back to an answer that (fingers crossed) works cross platforms.  <code>server-select-display</code> does the work of selecting the frame to use, and unfortunately it doesn&#39;t offer any hooks to customize.")
              (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame#comment3233_2417")
              (body_markdown . "It&#39;ll take me a while to get back to an answer that (fingers crossed) works cross platforms.  `server-select-display` does the work of selecting the frame to use, and unfortunately it doesn&#39;t offer any hooks to customize.")
              (comment_id . 3233)
              (post_id . 2417)
              (creation_date . 1414003752.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/372/purple-arrows")
               (display_name . "purple_arrows")
               (profile_image . "https://www.gravatar.com/avatar/cb378ba79d85a04a9277ddc0e4259149?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 372)
               (reputation . 472)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/93/nispio")
   (display_name . "nispio")
   (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
   (accept_rate . 55)
   (user_type . "registered")
   (user_id . 93)
   (reputation . 1709))
  (tags .
        ["frames" "emacsclient"])
  (answers .
           [((body . "<p>If your windowing system is X and your monitors are configured as separate screens, you can pass the appropriate <code>DISPLAY</code> value (e.g. <code>:0.0</code> or <code>:0.1</code>) to emacsclient with the <code>-d</code> option.  That's kind of a big \"if\".</p>\n")
             (title . "Open edit-server files from emacsclient in a specific frame")
             (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame/2425#2425")
             (body_markdown . "If your windowing system is X and your monitors are configured as separate screens, you can pass the appropriate `DISPLAY` value (e.g. `:0.0` or `:0.1`) to emacsclient with the `-d` option.  That&#39;s kind of a big &quot;if&quot;.")
             (question_id . 2417)
             (answer_id . 2425)
             (creation_date . 1413954754.0)
             (last_activity_date . 1413954754.0)
             (score . 2)
             (is_accepted . :json-false)
             (comment_count . 3)
             (comments .
                       [((body . "I am using X, and <code>-d :0.0</code> works fine, but for some reason <code>-d :0.1</code> gives an error: <code>Display :0.1 can&#39;t be opened</code>.")
                         (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame/2425#comment3199_2425")
                         (body_markdown . "I am using X, and `-d :0.0` works fine, but for some reason `-d :0.1` gives an error: `Display :0.1 can&#39;t be opened`.")
                         (comment_id . 3199)
                         (post_id . 2425)
                         (creation_date . 1413990079.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/93/nispio")
                          (display_name . "nispio")
                          (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
                          (accept_rate . 55)
                          (user_type . "registered")
                          (user_id . 93)
                          (reputation . 1709)))
                        ((body . "Your monitors may be set up as one big screen, as opposed to two separate screens.  You can tell if you can drag an (X) window a/k/a (Emacs) frame from one monitor to the other - if that&#39;s the case, then this solution won&#39;t work for you.")
                         (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame/2425#comment3232_2425")
                         (body_markdown . "Your monitors may be set up as one big screen, as opposed to two separate screens.  You can tell if you can drag an (X) window a/k/a (Emacs) frame from one monitor to the other - if that&#39;s the case, then this solution won&#39;t work for you.")
                         (comment_id . 3232)
                         (post_id . 2425)
                         (creation_date . 1414003353.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/93/nispio")
                          (display_name . "nispio")
                          (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
                          (accept_rate . 55)
                          (user_type . "registered")
                          (user_id . 93)
                          (reputation . 1709))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/372/purple-arrows")
                          (display_name . "purple_arrows")
                          (profile_image . "https://www.gravatar.com/avatar/cb378ba79d85a04a9277ddc0e4259149?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 372)
                          (reputation . 472)))
                        ((body . "You are right. This won&#39;t work for me, because my monitors are configured as a single display.")
                         (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame/2425#comment3240_2425")
                         (body_markdown . "You are right. This won&#39;t work for me, because my monitors are configured as a single display.")
                         (comment_id . 3240)
                         (post_id . 2425)
                         (creation_date . 1414011499.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/93/nispio")
                          (display_name . "nispio")
                          (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
                          (accept_rate . 55)
                          (user_type . "registered")
                          (user_id . 93)
                          (reputation . 1709)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/372/purple-arrows")
              (display_name . "purple_arrows")
              (profile_image . "https://www.gravatar.com/avatar/cb378ba79d85a04a9277ddc0e4259149?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 372)
              (reputation . 472)))
            ((body . "<p>I have <em>some</em> success with the following command:</p>\n\n<pre><code>emacsclient --eval \"(select-frame (car (frames-on-display-list)))\"; sleep 1s; emacsclient -n file.txt\n</code></pre>\n\n<p>The lisp code selects the first frame. To select the second one you should replace car with cadr. But I have not found a way to know which frame is the first ;)</p>\n\n<p>Bests,</p>\n\n<p>Jacek</p>\n")
             (title . "Open edit-server files from emacsclient in a specific frame")
             (link . "http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame/2934#2934")
             (body_markdown . "I have _some_ success with the following command:\n\n    emacsclient --eval &quot;(select-frame (car (frames-on-display-list)))&quot;; sleep 1s; emacsclient -n file.txt\n\nThe lisp code selects the first frame. To select the second one you should replace car with cadr. But I have not found a way to know which frame is the first ;)\n\nBests,\n\nJacek")
             (question_id . 2417)
             (answer_id . 2934)
             (creation_date . 1414790834.0)
             (last_activity_date . 1414790834.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/2353/jacek")
              (display_name . "Jacek")
              (profile_image . "https://www.gravatar.com/avatar/ef46008cedba3b32fae8586adb16cb8f?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 2353)
              (reputation . 1)))]))
 ((body . "<p>I know that I can use the <a href=\"http://unix.stackexchange.com/a/63920\"><code>file</code></a> command to determine this, but I'd like a cross-platform solution using elisp only (or as few subprocesses as possible).  I have the compressed data in a variable called <code>response</code>; you can use the following shell command to get a sample of the data I'm trying to profile:</p>\n\n<pre><code>curl --silent \"http://api.stackexchange.com/2.2/filter/create?filter=default&amp;exclude=user.profile_image;shallow_user.profile_image\"\n</code></pre>\n\n<p>Piping the above through <code>gunzip</code> will give a readable result.  The problem is that my mechanism for retrieving the information has different behavior when it is run locally and when it is run on Travis.</p>\n\n<p>Unfortunately, the <code>Content-Encoding</code> header <em>lies</em>.</p>\n")
  (title . "How can I determine if a file is compressed from Elisp?")
  (link . "http://emacs.stackexchange.com/questions/2931/how-can-i-determine-if-a-file-is-compressed-from-elisp")
  (body_markdown . "I know that I can use the [`file`][1] command to determine this, but I&#39;d like a cross-platform solution using elisp only (or as few subprocesses as possible).  I have the compressed data in a variable called `response`; you can use the following shell command to get a sample of the data I&#39;m trying to profile:\n\n    curl --silent &quot;http://api.stackexchange.com/2.2/filter/create?filter=default&amp;exclude=user.profile_image;shallow_user.profile_image&quot;\n\nPiping the above through `gunzip` will give a readable result.  The problem is that my mechanism for retrieving the information has different behavior when it is run locally and when it is run on Travis.\n\nUnfortunately, the `Content-Encoding` header *lies*.\n\n  [1]: http://unix.stackexchange.com/a/63920\n  [2]: https://github.com/vermiculus/stack-mode/blob/master/tests.el#L16")
  (question_id . 2931)
  (creation_date . 1414789578.0)
  (last_activity_date . 1414789578.0)
  (score . 1)
  (answer_count . 0)
  (favorite_count . 0)
  (view_count . 6)
  (is_answered . :json-false)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/2264/sean-allred")
   (display_name . "Sean Allred")
   (profile_image . "https://www.gravatar.com/avatar/9261936847b5a31e15da6e86533d3de3?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2264)
   (reputation . 512))
  (tags .
        ["compression" "url"]))
 ((body . "<p>Here's an example:</p>\n\n<pre><code>   #+NAME: sys5\n   #+HEADER: :exports both\n   #+BEGIN_SRC maxima :results raw\n     programmode: false;\n     solution: triangularize(coefmatrix(\n     [ -3*x - 11*y + 7*z = 0,\n        2*x +  6*y - 2*z = 0,\n          x +  2*y +   z = 0],\n          [x, y, z]));\n     print(solution);\n   #+END_SRC\n\n   # (1)\n   #+RESULTS: sys5\n   : [ - 3  - 11   7  ]\n   : [                ]\n   : [  0    4    - 8 ] \n   : [                ]\n   : [  0    0     0  ]\n   # (2)\n   #+BEGIN_EXAMPLE\n     [ - 3  - 11   7  ]\n     [                ]\n     [  0    4    - 8 ] \n     [                ]\n     [  0    0     0  ]\n   #+END_EXAMPLE\n\n   Triangulated matrix of the above solution doesn't have pivot in the third\n   column, thus it doesn't have a unique solution.\n</code></pre>\n\n<p>(1) is printed like so: <code>[-3 -11 7] [] [0 4 -8] [] [0 0 0]</code>, but (2) prints properly, i.e. every line on the separate line. Interestingly, if there's some text after the <code>#+RESULTS:</code>, then export doesn't add <code>=</code> around the result, otherwise, it does.</p>\n\n<p>Is there anything special about <code>[]</code> in results?</p>\n")
  (title . "Babel doesn&#39;t wrap results in verbatim")
  (link . "http://emacs.stackexchange.com/questions/2920/babel-doesnt-wrap-results-in-verbatim")
  (body_markdown . "Here&#39;s an example:\n\n       #+NAME: sys5\n       #+HEADER: :exports both\n       #+BEGIN_SRC maxima :results raw\n         programmode: false;\n         solution: triangularize(coefmatrix(\n         [ -3*x - 11*y + 7*z = 0,\n            2*x +  6*y - 2*z = 0,\n              x +  2*y +   z = 0],\n              [x, y, z]));\n         print(solution);\n       #+END_SRC\n    \n       # (1)\n       #+RESULTS: sys5\n       : [ - 3  - 11   7  ]\n       : [                ]\n       : [  0    4    - 8 ] \n       : [                ]\n       : [  0    0     0  ]\n       # (2)\n       #+BEGIN_EXAMPLE\n         [ - 3  - 11   7  ]\n         [                ]\n         [  0    4    - 8 ] \n         [                ]\n         [  0    0     0  ]\n       #+END_EXAMPLE\n    \n       Triangulated matrix of the above solution doesn&#39;t have pivot in the third\n       column, thus it doesn&#39;t have a unique solution.\n\n(1) is printed like so: `[-3 -11 7] [] [0 4 -8] [] [0 0 0]`, but (2) prints properly, i.e. every line on the separate line. Interestingly, if there&#39;s some text after the `#+RESULTS:`, then export doesn&#39;t add `=` around the result, otherwise, it does.\n\nIs there anything special about `[]` in results?")
  (question_id . 2920)
  (last_edit_date . 1414770962.0)
  (creation_date . 1414770503.0)
  (last_activity_date . 1414784968.0)
  (score . 0)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 15)
  (is_answered . :json-false)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/563/wvxvw")
   (display_name . "wvxvw")
   (profile_image . "https://www.gravatar.com/avatar/e5ab5db401198485d4569199f9994a7d?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 563)
   (reputation . 255))
  (owner
   (link . "http://emacs.stackexchange.com/users/563/wvxvw")
   (display_name . "wvxvw")
   (profile_image . "https://www.gravatar.com/avatar/e5ab5db401198485d4569199f9994a7d?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 563)
   (reputation . 255))
  (tags .
        ["org-mode" "org-export" "org-babel"])
  (answers .
           [((body . "<p>You clearly did not <a href=\"http://orgmode.org/manual/results.html#results\" rel=\"nofollow\">consult the manual</a> before posting this.  Upon reading the manual, you will not only know that you could have gotten what you desired result by using the header <code>#+BEGIN_SRC maxima :results value verbatim</code> or <code>#+BEGIN_SRC maxima :results value code</code> you will also know that <code>raw</code> means raw Org code.</p>\n")
             (title . "Babel doesn&#39;t wrap results in verbatim")
             (link . "http://emacs.stackexchange.com/questions/2920/babel-doesnt-wrap-results-in-verbatim/2926#2926")
             (body_markdown . "You clearly did not [consult the manual][1] before posting this.  Upon reading the manual, you will not only know that you could have gotten what you desired result by using the header `#+BEGIN_SRC maxima :results value verbatim` or `#+BEGIN_SRC maxima :results value code` you will also know that `raw` means raw Org code.\n\n  [1]: http://orgmode.org/manual/results.html#results")
             (question_id . 2920)
             (answer_id . 2926)
             (creation_date . 1414784968.0)
             (last_activity_date . 1414784968.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/1974/rasmus")
              (display_name . "rasmus")
              (profile_image . "https://www.gravatar.com/avatar/bd580f3842d43ba9dd42aff4914a38d3?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 1974)
              (reputation . 486)))]))
 ((body . "<p><img src=\"http://i.stack.imgur.com/d7XGr.png\" alt=\"Inactive Minibuffer\"></p>\n\n<p>I tried it with the following settings:</p>\n\n<pre><code>(add-hook 'minibuffer-setup-hook\n      (lambda ()\n        (make-local-variable 'face-remapping-alist)\n        (add-to-list 'face-remapping-alist '(default (:background \"green\")))))\n\n(set-face-background 'minibuffer-prompt \"blue\")\n</code></pre>\n\n<p>but they only affected the active minibuffer:</p>\n\n<p><img src=\"http://i.stack.imgur.com/kXFi4.png\" alt=\"Active Minibuffer\"></p>\n")
  (title . "Can I change the background color of the inactive minibuffer?")
  (link . "http://emacs.stackexchange.com/questions/2323/can-i-change-the-background-color-of-the-inactive-minibuffer")
  (body_markdown . "![Inactive Minibuffer][1]\n\nI tried it with the following settings:\n\n    (add-hook &#39;minibuffer-setup-hook\n          (lambda ()\n            (make-local-variable &#39;face-remapping-alist)\n            (add-to-list &#39;face-remapping-alist &#39;(default (:background &quot;green&quot;)))))\n\n    (set-face-background &#39;minibuffer-prompt &quot;blue&quot;)\n\nbut they only affected the active minibuffer:\n\n![Active Minibuffer][2]\n\n\n  [1]: http://i.stack.imgur.com/d7XGr.png\n  [2]: http://i.stack.imgur.com/kXFi4.png")
  (question_id . 2323)
  (last_edit_date . 1413635904.0)
  (creation_date . 1413626421.0)
  (last_activity_date . 1414784512.0)
  (score . 8)
  (answer_count . 2)
  (favorite_count . 1)
  (view_count . 90)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 2)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/50/malabarba")
   (display_name . "Malabarba")
   (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
   (accept_rate . 89)
   (user_type . "registered")
   (user_id . 50)
   (reputation . 3999))
  (comments .
            [((body . "I believe that&#39;s also called the echo area.")
              (link . "http://emacs.stackexchange.com/questions/2323/can-i-change-the-background-color-of-the-inactive-minibuffer#comment3024_2323")
              (body_markdown . "I believe that&#39;s also called the echo area.")
              (comment_id . 3024)
              (post_id . 2323)
              (creation_date . 1413635932.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "@Malabarba: It is <i>only</i> called the echo area (when the minibuffer is inactive).")
              (link . "http://emacs.stackexchange.com/questions/2323/can-i-change-the-background-color-of-the-inactive-minibuffer#comment3028_2323")
              (body_markdown . "@Malabarba: It is *only* called the echo area (when the minibuffer is inactive).")
              (comment_id . 3028)
              (post_id . 2323)
              (creation_date . 1413645757.0)
              (post_type . "question")
              (score . 1)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/105/drew")
               (display_name . "Drew")
               (profile_image . "https://www.gravatar.com/avatar/32a8e553d85d193ee5ae1533ce6ec158?s=128&d=identicon&r=PG&f=1")
               (user_type . "registered")
               (user_id . 105)
               (reputation . 3478)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/600/mcb")
   (display_name . "mcb")
   (profile_image . "https://www.gravatar.com/avatar/100e9ef8e9f5d6baa08667b4188d64eb?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 600)
   (reputation . 171))
  (tags .
        ["customize" "faces" "minibuffer"])
  (answers .
           [((body . "<p><code>minibuffer-setup-hook</code> is used only when the minibuffer is set up, i.e., activated, not when it is deactivated. </p>\n\n<p><code>minibuffer-exit-hook</code> takes effect when the minibuffer is exited. There is also <code>minibuffer-inactive-mode-hook</code>. </p>\n\n<p>But although those do initiate the color change (as shown by adding <code>(debug)</code> at the beginning of the hook function, and then stepping through the debugger with <code>d</code>), it seems that <code>kill-local-variables</code> removes the added color at some point. I don't have time now to check further, but perhaps you can, or perhaps someone else has a quick solution. Sorry for providing only incomplete info.</p>\n\n<p>Gotta go now - but quickly, I'm guessing that maybe you don't need to fiddle with hooks at all, and you can just do the face remapping for all buffers with names matching <code>\\` \\*Minibuf-[0-9]+\\*\\'</code>.</p>\n\n<hr>\n\n<p>FWIW, I use a <a href=\"http://www.emacswiki.org/emacs-en/download/oneonone.el\" rel=\"nofollow\">separate minibuffer frame</a>, and I put this on <code>minibuffer-exit-hook</code> to color the frame background:</p>\n\n<pre><code>(defun 1on1-color-minibuffer-frame-on-exit ()\n  \"Change background of minibuffer frame to reflect the minibuffer depth.\nUse this when reducing the minibuffer recursion depth.\"\n  (when 1on1-minibuffer-frame\n    (save-window-excursion\n      (select-frame 1on1-minibuffer-frame)\n      (cond ((= (minibuffer-depth) 2)\n             (set-background-color 1on1-active-minibuffer-frame-background))\n            ((&lt; (minibuffer-depth) 2)\n             (set-background-color 1on1-inactive-minibuffer-frame-background))\n            (t\n             (set-background-color (hexrgb-increment-hue ; Change bg hue slightly.\n                                    (frame-parameter nil 'background-color)\n                                    1on1-color-minibuffer-frame-on-exit-increment)))))))\n</code></pre>\n")
             (title . "Can I change the background color of the inactive minibuffer?")
             (link . "http://emacs.stackexchange.com/questions/2323/can-i-change-the-background-color-of-the-inactive-minibuffer/2325#2325")
             (body_markdown . "`minibuffer-setup-hook` is used only when the minibuffer is set up, i.e., activated, not when it is deactivated. \n\n`minibuffer-exit-hook` takes effect when the minibuffer is exited. There is also `minibuffer-inactive-mode-hook`. \n\nBut although those do initiate the color change (as shown by adding `(debug)` at the beginning of the hook function, and then stepping through the debugger with `d`), it seems that `kill-local-variables` removes the added color at some point. I don&#39;t have time now to check further, but perhaps you can, or perhaps someone else has a quick solution. Sorry for providing only incomplete info.\n\nGotta go now - but quickly, I&#39;m guessing that maybe you don&#39;t need to fiddle with hooks at all, and you can just do the face remapping for all buffers with names matching ``\\` \\*Minibuf-[0-9]+\\*\\&#39;``.\n\n---\n\nFWIW, I use a [separate minibuffer frame](http://www.emacswiki.org/emacs-en/download/oneonone.el), and I put this on `minibuffer-exit-hook` to color the frame background:\n\n    (defun 1on1-color-minibuffer-frame-on-exit ()\n      &quot;Change background of minibuffer frame to reflect the minibuffer depth.\n    Use this when reducing the minibuffer recursion depth.&quot;\n      (when 1on1-minibuffer-frame\n        (save-window-excursion\n          (select-frame 1on1-minibuffer-frame)\n          (cond ((= (minibuffer-depth) 2)\n                 (set-background-color 1on1-active-minibuffer-frame-background))\n                ((&lt; (minibuffer-depth) 2)\n                 (set-background-color 1on1-inactive-minibuffer-frame-background))\n                (t\n                 (set-background-color (hexrgb-increment-hue ; Change bg hue slightly.\n                                        (frame-parameter nil &#39;background-color)\n                                        1on1-color-minibuffer-frame-on-exit-increment)))))))")
             (question_id . 2323)
             (answer_id . 2325)
             (creation_date . 1413647017.0)
             (last_edit_date . 1413752373.0)
             (last_activity_date . 1413752373.0)
             (score . 5)
             (is_accepted . :json-false)
             (comment_count . 2)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/105/drew")
              (display_name . "Drew")
              (profile_image . "https://www.gravatar.com/avatar/32a8e553d85d193ee5ae1533ce6ec158?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 105)
              (reputation . 3478))
             (comments .
                       [((body . "Given your analysis, I guess making <code>face-remapping-alist</code> <a href=\"https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Buffer_002dLocal.html#index-permanent-local-variable-679\" rel=\"nofollow\">permanent</a> would work? For the backquotes in code markup, see <a href=\"http://meta.emacs.stackexchange.com/posts/31/revisions\">this answer</a>.")
                         (link . "http://emacs.stackexchange.com/questions/2323/can-i-change-the-background-color-of-the-inactive-minibuffer/2325#comment3040_2325")
                         (body_markdown . "Given your analysis, I guess making `face-remapping-alist` [permanent](https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Buffer_002dLocal.html#index-permanent-local-variable-679) would work? For the backquotes in code markup, see [this answer](http://meta.emacs.stackexchange.com/posts/31/revisions).")
                         (comment_id . 3040)
                         (post_id . 2325)
                         (creation_date . 1413661410.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/25/gilles")
                          (display_name . "Gilles")
                          (profile_image . "http://i.stack.imgur.com/O5VNX.jpg?s=128&g=1")
                          (user_type . "registered")
                          (user_id . 25)
                          (reputation . 2720)))
                        ((body . "@Gilles: Yes, I read that answer the first time. Feel free to edit my answer, to put that string better backticks (so that it appears correct. ;-) I wasn&#39;t successful in doing that.")
                         (link . "http://emacs.stackexchange.com/questions/2323/can-i-change-the-background-color-of-the-inactive-minibuffer/2325#comment3041_2325")
                         (body_markdown . "@Gilles: Yes, I read that answer the first time. Feel free to edit my answer, to put that string better backticks (so that it appears correct. ;-) I wasn&#39;t successful in doing that.")
                         (comment_id . 3041)
                         (post_id . 2325)
                         (creation_date . 1413673313.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/25/gilles")
                          (display_name . "Gilles")
                          (profile_image . "http://i.stack.imgur.com/O5VNX.jpg?s=128&g=1")
                          (user_type . "registered")
                          (user_id . 25)
                          (reputation . 2720))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/105/drew")
                          (display_name . "Drew")
                          (profile_image . "https://www.gravatar.com/avatar/32a8e553d85d193ee5ae1533ce6ec158?s=128&d=identicon&r=PG&f=1")
                          (user_type . "registered")
                          (user_id . 105)
                          (reputation . 3478)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/105/drew")
              (display_name . "Drew")
              (profile_image . "https://www.gravatar.com/avatar/32a8e553d85d193ee5ae1533ce6ec158?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 105)
              (reputation . 3478)))
            ((body . "<p>You might try:</p>\n\n<pre><code>(dolist (buf '(\" *Echo Area 0*\" \" *Echo Area 1*\"))\n  (with-current-buffer (get-buffer buf)\n    (make-local-variable 'face-remapping-alist)\n    (add-to-list 'face-remapping-alist '(default (:background \"green\")))))\n</code></pre>\n")
             (title . "Can I change the background color of the inactive minibuffer?")
             (link . "http://emacs.stackexchange.com/questions/2323/can-i-change-the-background-color-of-the-inactive-minibuffer/2925#2925")
             (body_markdown . "You might try:\n\n    (dolist (buf &#39;(&quot; *Echo Area 0*&quot; &quot; *Echo Area 1*&quot;))\n      (with-current-buffer (get-buffer buf)\n        (make-local-variable &#39;face-remapping-alist)\n        (add-to-list &#39;face-remapping-alist &#39;(default (:background &quot;green&quot;)))))\n")
             (question_id . 2323)
             (answer_id . 2925)
             (creation_date . 1414784512.0)
             (last_activity_date . 1414784512.0)
             (score . 1)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/1979/stefan")
              (display_name . "Stefan")
              (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 1979)
              (reputation . 631)))]))
 ((body . "<p>At times I want to browse the code of projects to which I have read-only access.  This makes it impossible for me to drop a <code>.projectile</code> file at the root of the project.</p>\n\n<p>Is there a way to define projects for use with projectile when I do not have write access to the project directories?</p>\n")
  (title . "Projectile project in folder without write access?")
  (link . "http://emacs.stackexchange.com/questions/2891/projectile-project-in-folder-without-write-access")
  (body_markdown . "At times I want to browse the code of projects to which I have read-only access.  This makes it impossible for me to drop a `.projectile` file at the root of the project.\n\nIs there a way to define projects for use with projectile when I do not have write access to the project directories?")
  (question_id . 2891)
  (creation_date . 1414735834.0)
  (last_activity_date . 1414784284.0)
  (score . 2)
  (answer_count . 2)
  (favorite_count . 0)
  (view_count . 31)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/93/nispio")
   (display_name . "nispio")
   (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
   (accept_rate . 55)
   (user_type . "registered")
   (user_id . 93)
   (reputation . 1709))
  (tags .
        ["projectile"])
  (answers .
           [((body . "<p>In that case, you can temporary disable <code>projectile-require-project-root</code>:</p>\n\n<pre><code>(setq projectile-require-project-root nil)\n</code></pre>\n\n<p>Then, you can activate Projectile everywhere.</p>\n")
             (title . "Projectile project in folder without write access?")
             (link . "http://emacs.stackexchange.com/questions/2891/projectile-project-in-folder-without-write-access/2893#2893")
             (body_markdown . "In that case, you can temporary disable `projectile-require-project-root`:\n\n    (setq projectile-require-project-root nil)\n\nThen, you can activate Projectile everywhere.")
             (question_id . 2891)
             (answer_id . 2893)
             (creation_date . 1414736337.0)
             (last_activity_date . 1414736337.0)
             (score . 1)
             (is_accepted . :json-false)
             (comment_count . 3)
             (comments .
                       [((body . "I just tried this out, and it seems like it would only be useful in a flat directory structure.  As soon as I open a file in <code>src&#47;</code> the project root changes to <code>src&#47;</code> and I can no longer jump to the files in <code>inc&#47;</code>.")
                         (link . "http://emacs.stackexchange.com/questions/2891/projectile-project-in-folder-without-write-access/2893#comment3943_2893")
                         (body_markdown . "I just tried this out, and it seems like it would only be useful in a flat directory structure.  As soon as I open a file in `src/` the project root changes to `src/` and I can no longer jump to the files in `inc/`.")
                         (comment_id . 3943)
                         (post_id . 2893)
                         (creation_date . 1414737627.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/93/nispio")
                          (display_name . "nispio")
                          (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
                          (accept_rate . 55)
                          (user_type . "registered")
                          (user_id . 93)
                          (reputation . 1709)))
                        ((body . "There&#39;s no way to interactively add to project list. But you may try manually adding a directory to <code>projectile-known-projects</code>. You could also look at this <a href=\"https://github.com/bbatsov/projectile/issues/364\" rel=\"nofollow\">issue</a>. Probably you should update the issue, so someone could add this feature.")
                         (link . "http://emacs.stackexchange.com/questions/2891/projectile-project-in-folder-without-write-access/2893#comment3945_2893")
                         (body_markdown . "There&#39;s no way to interactively add to project list. But you may try manually adding a directory to `projectile-known-projects`. You could also look at this [issue](https://github.com/bbatsov/projectile/issues/364). Probably you should update the issue, so someone could add this feature.")
                         (comment_id . 3945)
                         (post_id . 2893)
                         (creation_date . 1414737857.0)
                         (post_type . "answer")
                         (score . 2)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/93/nispio")
                          (display_name . "nispio")
                          (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
                          (accept_rate . 55)
                          (user_type . "registered")
                          (user_id . 93)
                          (reputation . 1709))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/200/tu-do")
                          (display_name . "Tu Do")
                          (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
                          (accept_rate . 75)
                          (user_type . "registered")
                          (user_id . 200)
                          (reputation . 652)))
                        ((body . "Adding to <code>projectile-known-projects</code> suffered from the same subdirectory issue that I mentioned in my first comment.  I did add a comment to the GitHub issue though, and I got a helpful response.")
                         (link . "http://emacs.stackexchange.com/questions/2891/projectile-project-in-folder-without-write-access/2893#comment4008_2893")
                         (body_markdown . "Adding to `projectile-known-projects` suffered from the same subdirectory issue that I mentioned in my first comment.  I did add a comment to the GitHub issue though, and I got a helpful response.")
                         (comment_id . 4008)
                         (post_id . 2893)
                         (creation_date . 1414784253.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/93/nispio")
                          (display_name . "nispio")
                          (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
                          (accept_rate . 55)
                          (user_type . "registered")
                          (user_id . 93)
                          (reputation . 1709)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/200/tu-do")
              (display_name . "Tu Do")
              (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
              (accept_rate . 75)
              (user_type . "registered")
              (user_id . 200)
              (reputation . 652)))
            ((body . "<p>GitHub user thomasf <a href=\"https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248\" rel=\"nofollow\">provided a workaround</a> which I modified only slightly:</p>\n\n<pre><code>;; (source: https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248)\n(defun projectile-root-child-of (dir &amp;optional list)\n  (projectile-locate-dominating-file\n   dir\n   (lambda (dir)\n     (--first\n      (if (and\n           (s-equals? (file-remote-p it) (file-remote-p dir))\n           (string-match-p (expand-file-name it) (expand-file-name dir)))\n          dir)\n      (or list project-root-regexps (list))))))\n\n(defvar project-root-regexps ()\n  \"List of regexps to match against when projectile is searching\n  for project root directories.\")\n\n(add-to-list 'project-root-regexps \"/path/to/some/project/$\")\n(add-to-list 'project-root-regexps \"/path/to/another/project/$\")\n(add-to-list 'project-root-regexps \"/path/to/one/more/project/$\")\n\n(nconc projectile-project-root-files-functions '(projectile-root-child-of))\n</code></pre>\n\n<p>This lets me add projects to to the list <code>project-root-regexps</code>, and as far as I can tell they behave just as if they had a <code>.projectile</code> file at their root.</p>\n")
             (title . "Projectile project in folder without write access?")
             (link . "http://emacs.stackexchange.com/questions/2891/projectile-project-in-folder-without-write-access/2924#2924")
             (body_markdown . "GitHub user thomasf [provided a workaround][1] which I modified only slightly:\n\n    ;; (source: https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248)\n    (defun projectile-root-child-of (dir &amp;optional list)\n      (projectile-locate-dominating-file\n       dir\n       (lambda (dir)\n    	 (--first\n    	  (if (and\n    		   (s-equals? (file-remote-p it) (file-remote-p dir))\n    		   (string-match-p (expand-file-name it) (expand-file-name dir)))\n    		  dir)\n    	  (or list project-root-regexps (list))))))\n    \n    (defvar project-root-regexps ()\n      &quot;List of regexps to match against when projectile is searching\n      for project root directories.&quot;)\n    \n    (add-to-list &#39;project-root-regexps &quot;/path/to/some/project/$&quot;)\n    (add-to-list &#39;project-root-regexps &quot;/path/to/another/project/$&quot;)\n    (add-to-list &#39;project-root-regexps &quot;/path/to/one/more/project/$&quot;)\n    \n    (nconc projectile-project-root-files-functions &#39;(projectile-root-child-of))\n\nThis lets me add projects to to the list `project-root-regexps`, and as far as I can tell they behave just as if they had a `.projectile` file at their root.\n\n\n  [1]: https://github.com/bbatsov/projectile/issues/364#issuecomment-61296248")
             (question_id . 2891)
             (answer_id . 2924)
             (creation_date . 1414784284.0)
             (last_activity_date . 1414784284.0)
             (score . 1)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/93/nispio")
              (display_name . "nispio")
              (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
              (accept_rate . 55)
              (user_type . "registered")
              (user_id . 93)
              (reputation . 1709)))]))
 ((body . "<p>After a recent update <code>eldoc</code> is turned on by default, even for <code>emacs -Q</code>.\nI don't appreciate the noise it generates when I'm using <code>eval-expression</code>.\nHow do I turn it off?</p>\n")
  (title . "How to disable eldoc for `eval-expression`?")
  (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression")
  (body_markdown . "After a recent update `eldoc` is turned on by default, even for `emacs -Q`.\nI don&#39;t appreciate the noise it generates when I&#39;m using `eval-expression`.\nHow do I turn it off?")
  (question_id . 2917)
  (last_edit_date . 1414770569.0)
  (creation_date . 1414767244.0)
  (last_activity_date . 1414776394.0)
  (score . 1)
  (answer_count . 1)
  (accepted_answer_id . 2918)
  (favorite_count . 0)
  (view_count . 32)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/337/jonathan-leech-pepin")
   (display_name . "Jonathan Leech-Pepin")
   (profile_image . "https://www.gravatar.com/avatar/95c16e08edbfaf21de8e6fe15e0ba4c6?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 337)
   (reputation . 1001))
  (owner
   (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
   (display_name . "abo-abo")
   (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2094)
   (reputation . 420))
  (tags .
        ["elisp" "emacs-25"])
  (answers .
           [((body . "<p>Disable it globally:</p>\n\n<pre><code>(global-eldoc-mode -1)\n</code></pre>\n\n<p>Disable it locally - make sure that <code>eldoc-mode</code> is <code>nil</code> in all your buffers.</p>\n\n<p>A less drastic solution would be to increase <code>eldoc-idle-delay</code>.</p>\n")
             (title . "How to disable eldoc for `eval-expression`?")
             (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#2918")
             (body_markdown . "Disable it globally:\n\n    (global-eldoc-mode -1)\n\nDisable it locally - make sure that `eldoc-mode` is `nil` in all your buffers.\n\nA less drastic solution would be to increase `eldoc-idle-delay`.\n")
             (question_id . 2917)
             (answer_id . 2918)
             (creation_date . 1414768610.0)
             (last_edit_date . 1414776394.0)
             (last_activity_date . 1414776394.0)
             (score . 2)
             (is_accepted . t)
             (comment_count . 11)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/795/sds")
              (display_name . "sds")
              (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 795)
              (reputation . 181))
             (comments .
                       [((body . "Nope, doesn&#39;t work.")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3983_2918")
                         (body_markdown . "Nope, doesn&#39;t work.")
                         (comment_id . 3983)
                         (post_id . 2918)
                         (creation_date . 1414768706.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
                          (display_name . "abo-abo")
                          (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2094)
                          (reputation . 420)))
                        ((body . "check buffer-local values of <code>eldoc-mode</code>.")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3984_2918")
                         (body_markdown . "check buffer-local values of `eldoc-mode`.")
                         (comment_id . 3984)
                         (post_id . 2918)
                         (creation_date . 1414769011.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
                          (display_name . "abo-abo")
                          (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2094)
                          (reputation . 420))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/795/sds")
                          (display_name . "sds")
                          (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 795)
                          (reputation . 181)))
                        ((body . "Setting <code>eldoc-idle-delay</code> works, but how would I disable <code>eldoc-mode</code> for minibuffer? I tried <code>minibuffer-setup-hook</code>, it didn&#39;t work.")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3985_2918")
                         (body_markdown . "Setting `eldoc-idle-delay` works, but how would I disable `eldoc-mode` for minibuffer?\nI tried `minibuffer-setup-hook`, it didn&#39;t work.")
                         (comment_id . 3985)
                         (post_id . 2918)
                         (creation_date . 1414769673.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
                          (display_name . "abo-abo")
                          (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2094)
                          (reputation . 420)))
                        ((body . "wdym &quot;disable eldoc-mode for minibuffer&quot;? just kill the timer")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3986_2918")
                         (body_markdown . "wdym &quot;disable eldoc-mode for minibuffer&quot;? just kill the timer")
                         (comment_id . 3986)
                         (post_id . 2918)
                         (creation_date . 1414769738.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
                          (display_name . "abo-abo")
                          (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2094)
                          (reputation . 420))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/795/sds")
                          (display_name . "sds")
                          (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 795)
                          (reputation . 181)))
                        ((body . "Killing the timer doesn&#39;t work. Tested with <code>emacs -Q</code>.")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3987_2918")
                         (body_markdown . "Killing the timer doesn&#39;t work. Tested with `emacs -Q`.")
                         (comment_id . 3987)
                         (post_id . 2918)
                         (creation_date . 1414770039.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
                          (display_name . "abo-abo")
                          (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2094)
                          (reputation . 420)))
                        ((body . "<code>(global-eldoc-mode nil)</code> works with <code>emacs -Q</code>")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3988_2918")
                         (body_markdown . "`(global-eldoc-mode nil)` works with `emacs -Q`")
                         (comment_id . 3988)
                         (post_id . 2918)
                         (creation_date . 1414770147.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
                          (display_name . "abo-abo")
                          (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2094)
                          (reputation . 420))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/795/sds")
                          (display_name . "sds")
                          (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 795)
                          (reputation . 181)))
                        ((body . "No, it doesn&#39;t on today&#39;s build. Just <code>eval-expression</code>, type <code>(+</code> and the hint will appear in the modeline. All with <code>emacs -Q</code>")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3989_2918")
                         (body_markdown . "No, it doesn&#39;t on today&#39;s build. Just `eval-expression`, type `(+` and the hint will appear in the modeline. All with `emacs -Q`")
                         (comment_id . 3989)
                         (post_id . 2918)
                         (creation_date . 1414770240.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
                          (display_name . "abo-abo")
                          (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2094)
                          (reputation . 420)))
                        ((body . "Problems with dev builds should be discussed on <code>emacs-devel</code> mailing list. SE is for long term knowledge.")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3990_2918")
                         (body_markdown . "Problems with dev builds should be discussed on `emacs-devel` mailing list. SE is for long term knowledge.")
                         (comment_id . 3990)
                         (post_id . 2918)
                         (creation_date . 1414770391.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
                          (display_name . "abo-abo")
                          (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2094)
                          (reputation . 420))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/795/sds")
                          (display_name . "sds")
                          (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 795)
                          (reputation . 181)))
                        ((body . "It should be <code>(global-eldoc-mode -1)</code>: <code>nil</code> for minor modes used to mean &quot;toggle&quot; but nowadays it means &quot;enable&quot;.")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3997_2918")
                         (body_markdown . "It should be `(global-eldoc-mode -1)`: `nil` for minor modes used to mean &quot;toggle&quot; but nowadays it means &quot;enable&quot;.")
                         (comment_id . 3997)
                         (post_id . 2918)
                         (creation_date . 1414776358.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/1979/stefan")
                          (display_name . "Stefan")
                          (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 1979)
                          (reputation . 631)))
                        ((body . "Thanks @Stefan - why was such a counter-intuitive change made?")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3998_2918")
                         (body_markdown . "Thanks @Stefan - why was such a counter-intuitive change made?")
                         (comment_id . 3998)
                         (post_id . 2918)
                         (creation_date . 1414776443.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/1979/stefan")
                          (display_name . "Stefan")
                          (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 1979)
                          (reputation . 631))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/795/sds")
                          (display_name . "sds")
                          (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 795)
                          (reputation . 181)))
                        ((body . "Because &quot;toggle&quot; is virtually never the behavior that&#39;s needed when called non-interactively.  This change fixed latent bugs for all those people who had <code>(add-hook &#39;foo-mode-hook &#39;bar-mode)</code> in their .emacs.  A side-benefit is that we don&#39;t need any <code>turn-on-foo-mode</code> any more.")
                         (link . "http://emacs.stackexchange.com/questions/2917/how-to-disable-eldoc-for-eval-expression/2918#comment3999_2918")
                         (body_markdown . "Because &quot;toggle&quot; is virtually never the behavior that&#39;s needed when called non-interactively.  This change fixed latent bugs for all those people who had `(add-hook &#39;foo-mode-hook &#39;bar-mode)` in their .emacs.  A side-benefit is that we don&#39;t need any `turn-on-foo-mode` any more.")
                         (comment_id . 3999)
                         (post_id . 2918)
                         (creation_date . 1414776751.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/1979/stefan")
                          (display_name . "Stefan")
                          (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 1979)
                          (reputation . 631)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/795/sds")
              (display_name . "sds")
              (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 795)
              (reputation . 181)))]))
 ((body . "<p>I have been using <code>csv-mode</code> to modify small to medium sized CSV files, but recently I have been working with large files containing more than 40,812 entries. <code>csv-mode</code> struggles to align and navigate the tables, and is too slow to be usable as is.  In comparison, LibreOffice Calc can zip through the file.</p>\n\n<p>Is there a simple way to make <code>csv-mode</code> handle large tables, or is there a better approach available?</p>\n\n<p>I am aware of <a href=\"http://stackoverflow.com/questions/10616525/is-there-a-good-emacs-mode-for-displaying-and-editing-huge-delimiter-separated-f\">a related Stack Overflow question</a>. Its solution was to only align the portion of buffer in the visible window, but this did not solve the sluggishness in my case.</p>\n\n<p><a href=\"https://www.dropbox.com/s/xmt4s5i5ikq0i0v/big.csv?dl=0\" rel=\"nofollow\">Here is an example file.</a>  I tried to make it large, but not so large it will freeze Emacs on older computers.</p>\n")
  (title . "How to view and edit large delimiter separated value files?")
  (link . "http://emacs.stackexchange.com/questions/972/how-to-view-and-edit-large-delimiter-separated-value-files")
  (body_markdown . "I have been using `csv-mode` to modify small to medium sized CSV files, but recently I have been working with large files containing more than 40,812 entries. `csv-mode` struggles to align and navigate the tables, and is too slow to be usable as is.  In comparison, LibreOffice Calc can zip through the file.\n\nIs there a simple way to make `csv-mode` handle large tables, or is there a better approach available?\n\nI am aware of [a related Stack Overflow question](http://stackoverflow.com/questions/10616525/is-there-a-good-emacs-mode-for-displaying-and-editing-huge-delimiter-separated-f). Its solution was to only align the portion of buffer in the visible window, but this did not solve the sluggishness in my case.\n\n[Here is an example file.][1]  I tried to make it large, but not so large it will freeze Emacs on older computers.\n\n\n  [1]: https://www.dropbox.com/s/xmt4s5i5ikq0i0v/big.csv?dl=0")
  (question_id . 972)
  (last_edit_date . 1414713136.0)
  (creation_date . 1412881152.0)
  (last_activity_date . 1414775451.0)
  (score . 5)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 88)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 4)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/315/holocronweaver")
   (display_name . "holocronweaver")
   (profile_image . "https://www.gravatar.com/avatar/912d39713cb1441439c578a3c286eca7?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 315)
   (reputation . 48))
  (comments .
            [((body . "Table editing is not one of Emacs&#39;s current strengths. I wish it was. I&#39;d rather never have to use a dedicated spreadsheet.")
              (link . "http://emacs.stackexchange.com/questions/972/how-to-view-and-edit-large-delimiter-separated-value-files#comment1383_972")
              (body_markdown . "Table editing is not one of Emacs&#39;s current strengths. I wish it was. I&#39;d rather never have to use a dedicated spreadsheet.")
              (comment_id . 1383)
              (post_id . 972)
              (creation_date . 1412892816.0)
              (post_type . "question")
              (score . 2)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/55/wdkrnls")
               (display_name . "wdkrnls")
               (profile_image . "https://www.gravatar.com/avatar/6d08a2f792f289b95fe1d982d4133d71?s=128&d=identicon&r=PG")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 55)
               (reputation . 362)))
             ((body . "Not a short-term answer, but you might want to <code>M-x report-emacs-bug</code> about it, ideally with a recipe to reproduce the slow-down.  There&#39;s probably a lot of room for improvement in <code>csv-mode</code>.")
              (link . "http://emacs.stackexchange.com/questions/972/how-to-view-and-edit-large-delimiter-separated-value-files#comment2717_972")
              (body_markdown . "Not a short-term answer, but you might want to `M-x report-emacs-bug` about it, ideally with a recipe to reproduce the slow-down.  There&#39;s probably a lot of room for improvement in `csv-mode`.")
              (comment_id . 2717)
              (post_id . 972)
              (creation_date . 1413305227.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/1979/stefan")
               (display_name . "Stefan")
               (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 1979)
               (reputation . 631)))
             ((body . "Do you have a sample file that demonstrates the problem?")
              (link . "http://emacs.stackexchange.com/questions/972/how-to-view-and-edit-large-delimiter-separated-value-files#comment2767_972")
              (body_markdown . "Do you have a sample file that demonstrates the problem?")
              (comment_id . 2767)
              (post_id . 972)
              (creation_date . 1413359737.0)
              (post_type . "question")
              (score . 2)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/304/wilfred-hughes")
               (display_name . "Wilfred Hughes")
               (profile_image . "https://www.gravatar.com/avatar/1f86c0bc40235a9edf351a16b859aabd?s=128&d=identicon&r=PG")
               (accept_rate . 62)
               (user_type . "registered")
               (user_id . 304)
               (reputation . 897)))
             ((body . "Yes, yes I do. Added to question.")
              (link . "http://emacs.stackexchange.com/questions/972/how-to-view-and-edit-large-delimiter-separated-value-files#comment3931_972")
              (body_markdown . "Yes, yes I do. Added to question.")
              (comment_id . 3931)
              (post_id . 972)
              (creation_date . 1414713159.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/315/holocronweaver")
               (display_name . "holocronweaver")
               (profile_image . "https://www.gravatar.com/avatar/912d39713cb1441439c578a3c286eca7?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 315)
               (reputation . 48)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/315/holocronweaver")
   (display_name . "holocronweaver")
   (profile_image . "https://www.gravatar.com/avatar/912d39713cb1441439c578a3c286eca7?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 315)
   (reputation . 48))
  (tags .
        ["editing" "table" "large-files" "csv"])
  (answers .
           [((body . "<p>With <code>csv-mode</code> I can see some lags with your file, but only with syntax highlighting enabled. After disabling fontification with <code>M-x font-lock-mode</code> it works without problems.</p>\n\n<p>To disable it permanently for <code>csv-mode</code> add to your config:</p>\n\n<pre><code>(add-hook 'csv-mode-hook (lambda () (font-lock-mode -1))\n</code></pre>\n\n<p>Or if you are a <a href=\"https://github.com/jwiegley/use-package\" rel=\"nofollow\">use-package</a> user:</p>\n\n<pre><code>(use-package csv-mode\n  :mode (\"\\\\.csv\\\\'\" . csv-mode)\n  :init (add-hook 'csv-mode-hook (lambda () (font-lock-mode -1)))\n  :ensure t)\n</code></pre>\n")
             (title . "How to view and edit large delimiter separated value files?")
             (link . "http://emacs.stackexchange.com/questions/972/how-to-view-and-edit-large-delimiter-separated-value-files/2923#2923")
             (body_markdown . "With `csv-mode` I can see some lags with your file, but only with syntax highlighting enabled. After disabling fontification with `M-x font-lock-mode` it works without problems.\n\nTo disable it permanently for `csv-mode` add to your config:\n\n    (add-hook &#39;csv-mode-hook (lambda () (font-lock-mode -1))\n\nOr if you are a [use-package][1] user:\n\n    (use-package csv-mode\n      :mode (&quot;\\\\.csv\\\\&#39;&quot; . csv-mode)\n      :init (add-hook &#39;csv-mode-hook (lambda () (font-lock-mode -1)))\n      :ensure t)\n\n\n  [1]: https://github.com/jwiegley/use-package")
             (question_id . 972)
             (answer_id . 2923)
             (creation_date . 1414775451.0)
             (last_activity_date . 1414775451.0)
             (score . 1)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/220/kmicu")
              (display_name . "kmicu")
              (profile_image . "https://www.gravatar.com/avatar/8dff5903c3d337d048b6b908e0be9e9b?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 220)
              (reputation . 466)))]))
 ((body . "<p>I'm doing quite a bit of manual xml editing (the source definition of some code generation I'm doing is a custom xml format) and of course prefer to use Emacs over any special purpose  (usually ugly) xml editors. nXml mode has stood me well in the past, but I cannot get my head around it's \"outline\" support. Various internet and SO posts effectively say nothing - I'm wondering if anyone has any practical experience with outlining/folding xml in emacs (any mode) whether or not that requires altering the xml structure itself.</p>\n")
  (title . "The old &quot;how to fold xml&quot; question")
  (link . "http://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question")
  (body_markdown . "I&#39;m doing quite a bit of manual xml editing (the source definition of some code generation I&#39;m doing is a custom xml format) and of course prefer to use Emacs over any special purpose  (usually ugly) xml editors. nXml mode has stood me well in the past, but I cannot get my head around it&#39;s &quot;outline&quot; support. Various internet and SO posts effectively say nothing - I&#39;m wondering if anyone has any practical experience with outlining/folding xml in emacs (any mode) whether or not that requires altering the xml structure itself.")
  (question_id . 2884)
  (creation_date . 1414720950.0)
  (last_activity_date . 1414773563.0)
  (score . 3)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 24)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/215/mark-aufflick")
   (display_name . "Mark Aufflick")
   (profile_image . "http://i.stack.imgur.com/y7fBO.png?s=128&g=1")
   (user_type . "registered")
   (user_id . 215)
   (reputation . 193))
  (tags .
        ["xml" "nxml" "outline"])
  (answers .
           [((body . "<p><a href=\"http://web-mode.org/\" rel=\"nofollow\">web-mode</a> has element folding built in and bound to <kbd>C-c</kbd> <kbd>C-f</kbd>. But you will lose some of the features of using nxml-mode obviously.</p>\n")
             (title . "The old &quot;how to fold xml&quot; question")
             (link . "http://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question/2921#2921")
             (body_markdown . "[web-mode][1] has element folding built in and bound to &lt;kbd&gt;C-c&lt;/kbd&gt; &lt;kbd&gt;C-f&lt;/kbd&gt;. But you will lose some of the features of using nxml-mode obviously.\n\n  [1]: http://web-mode.org/")
             (question_id . 2884)
             (answer_id . 2921)
             (creation_date . 1414773563.0)
             (last_activity_date . 1414773563.0)
             (score . 1)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/160/jordon-biondo")
              (display_name . "Jordon Biondo")
              (profile_image . "http://i.stack.imgur.com/y27jq.png?s=128&g=1")
              (user_type . "registered")
              (user_id . 160)
              (reputation . 1487)))]))
 ((body . "<p>When playing around with data in <code>calc</code>, I often want to generate a vector from an interval.</p>\n\n<p>How can I easily create a vector from just a start point, end point &amp; step value.</p>\n\n<p>Something like <code>1 &lt;RET&gt; 10 &lt;RET&gt; 1 MAKE-VECTOR</code> to give me <code>[1,2,3,4,5,6,7,8,9,10]</code>?</p>\n")
  (title . "How can I turn [1..10] into [1,2,3,4,5,6,7,8,9,10] in calc?")
  (link . "http://emacs.stackexchange.com/questions/2914/how-can-i-turn-1-10-into-1-2-3-4-5-6-7-8-9-10-in-calc")
  (body_markdown . "When playing around with data in `calc`, I often want to generate a vector from an interval.\n\nHow can I easily create a vector from just a start point, end point &amp; step value.\n\nSomething like `1 &lt;RET&gt; 10 &lt;RET&gt; 1 MAKE-VECTOR` to give me `[1,2,3,4,5,6,7,8,9,10]`?")
  (question_id . 2914)
  (creation_date . 1414765664.0)
  (last_activity_date . 1414766835.0)
  (score . 2)
  (answer_count . 1)
  (accepted_answer_id . 2916)
  (favorite_count . 0)
  (view_count . 100)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/503/mrbones")
   (display_name . "MrBones")
   (profile_image . "https://www.gravatar.com/avatar/db83b716acfe0f3a1de6faa176d63fca?s=128&d=identicon&r=PG&f=1")
   (user_type . "registered")
   (user_id . 503)
   (reputation . 113))
  (tags .
        ["calc"])
  (answers .
           [((body . "<p><kbd>v x 10 RET</kbd> should do it. Uses <code>calc-index</code>.</p>\n")
             (title . "How can I turn [1..10] into [1,2,3,4,5,6,7,8,9,10] in calc?")
             (link . "http://emacs.stackexchange.com/questions/2914/how-can-i-turn-1-10-into-1-2-3-4-5-6-7-8-9-10-in-calc/2916#2916")
             (body_markdown . "&lt;kbd&gt;v x 10 RET&lt;/kbd&gt; should do it. Uses `calc-index`.\n")
             (question_id . 2914)
             (answer_id . 2916)
             (creation_date . 1414766835.0)
             (last_activity_date . 1414766835.0)
             (score . 4)
             (is_accepted . t)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
              (display_name . "abo-abo")
              (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 2094)
              (reputation . 420)))]))
 ((body . "<p>Since the update, a certain style of formatting code is highlighted with <code>font-lock-warning-face</code>:</p>\n\n<p>How do I turn off this behavior?</p>\n\n<p><img src=\"http://i.stack.imgur.com/ur3aG.png\" alt=\"enter image description here\"></p>\n")
  (title . "How to turn off &quot;error&quot; highlighting in emacs-lisp-mode for Emacs 25?")
  (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25")
  (body_markdown . "Since the update, a certain style of formatting code is highlighted with `font-lock-warning-face`:\n\n  [1]: http://i.stack.imgur.com/ur3aG.png\n\nHow do I turn off this behavior?\n\n![enter image description here][1]\n\n\n")
  (question_id . 2907)
  (last_edit_date . 1414766064.0)
  (creation_date . 1414759846.0)
  (last_activity_date . 1414766064.0)
  (score . 2)
  (answer_count . 1)
  (accepted_answer_id . 2913)
  (favorite_count . 0)
  (view_count . 47)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 11)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/50/malabarba")
   (display_name . "Malabarba")
   (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
   (accept_rate . 89)
   (user_type . "registered")
   (user_id . 50)
   (reputation . 3999))
  (comments .
            [((body . "Could it be the theme? I was tweaking <a href=\"https://github.com/thierryvolpiatto/zop-to-char\" rel=\"nofollow\"><code>zop-to-char</code></a> yesterday and I don&#39;t see that <code>nil</code> highlighted in <code>font-lock-warning-face</code>: <a href=\"http://i.imgur.com/nI7ZwX7.png?1\" rel=\"nofollow\">My screenshot</a>. I am also on 24.4. The difference I guess is that I am using <code>zenburn</code> theme. And it does highlight <code>font-lock-warning-face</code> at places I expect.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3965_2907")
              (body_markdown . "Could it be the theme? I was tweaking [`zop-to-char`](https://github.com/thierryvolpiatto/zop-to-char) yesterday and I don&#39;t see that `nil` highlighted in `font-lock-warning-face`: [My screenshot](http://i.imgur.com/nI7ZwX7.png?1). I am also on 24.4. The difference I guess is that I am using `zenburn` theme. And it does highlight `font-lock-warning-face` at places I expect.")
              (comment_id . 3965)
              (post_id . 2907)
              (creation_date . 1414761331.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563)))
             ((body . "It&#39;s reproducible with <code>emacs -Q</code>.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3966_2907")
              (body_markdown . "It&#39;s reproducible with `emacs -Q`.")
              (comment_id . 3966)
              (post_id . 2907)
              (creation_date . 1414761494.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563))
              (owner
               (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
               (display_name . "abo-abo")
               (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2094)
               (reputation . 420)))
             ((body . "I can confirm this does happen. I find it useful because it highlights that the return value is not what it seems.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3967_2907")
              (body_markdown . "I can confirm this does happen. I find it useful because it highlights that the return value is not what it seems.")
              (comment_id . 3967)
              (post_id . 2907)
              (creation_date . 1414761621.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "Strange.. why am I not seeing that even on <code>emacs -Q</code>? <a href=\"http://i.imgur.com/YcACYO0.png\" rel=\"nofollow\">Screenshot</a>. Again, I am also using emacs 24.4.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3968_2907")
              (body_markdown . "Strange.. why am I not seeing that even on `emacs -Q`? [Screenshot](http://i.imgur.com/YcACYO0.png). Again, I am also using emacs 24.4.")
              (comment_id . 3968)
              (post_id . 2907)
              (creation_date . 1414762049.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563)))
             ((body . "@Malabarba, it highlights red because <code>nil</code> isn&#39;t on a new line. It&#39;s just a text formatting issue.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3969_2907")
              (body_markdown . "@Malabarba, it highlights red because `nil` isn&#39;t on a new line. It&#39;s just a text formatting issue.")
              (comment_id . 3969)
              (post_id . 2907)
              (creation_date . 1414762158.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
               (display_name . "abo-abo")
               (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2094)
               (reputation . 420)))
             ((body . "@Malabarba, does that highlighting happen for you for <a href=\"https://github.com/thierryvolpiatto/zop-to-char/blob/master/zop-to-char.el#L56-60\" rel=\"nofollow\">this exact code snippet</a>?")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3970_2907")
              (body_markdown . "@Malabarba, does that highlighting happen for you for [this exact code snippet](https://github.com/thierryvolpiatto/zop-to-char/blob/master/zop-to-char.el#L56-60)?")
              (comment_id . 3970)
              (post_id . 2907)
              (creation_date . 1414762323.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563)))
             ((body . "@abo-abo The fact that nil isn&#39;t on a newline gives the wrong impression this entire expression returns the return value of <code>kill-region</code>. Highlighting the <code>nil</code> makes it clear that the expression actually returns nil.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3971_2907")
              (body_markdown . "@abo-abo The fact that nil isn&#39;t on a newline gives the wrong impression this entire expression returns the return value of `kill-region`. Highlighting the `nil` makes it clear that the expression actually returns nil.")
              (comment_id . 3971)
              (post_id . 2907)
              (creation_date . 1414762363.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "@kaushalmodi Yes. Though my emacs actually identifies itself as 25.0 now, so it might not be in 24.4.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3973_2907")
              (body_markdown . "@kaushalmodi Yes. Though my emacs actually identifies itself as 25.0 now, so it might not be in 24.4.")
              (comment_id . 3973)
              (post_id . 2907)
              (creation_date . 1414762672.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563))
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "Thanks for confirming that. @abo-abo Are you also on the emacs 25.0 development version? My <code>C-h</code> <code>v</code> <code>emacs-version</code> reads <code>24.4.8</code>.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3975_2907")
              (body_markdown . "Thanks for confirming that. @abo-abo Are you also on the emacs 25.0 development version? My `C-h` `v` `emacs-version` reads `24.4.8`.")
              (comment_id . 3975)
              (post_id . 2907)
              (creation_date . 1414762760.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563)))
             ((body . "@kaushalmodi Yes, the issue happens for current devel version.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3976_2907")
              (body_markdown . "@kaushalmodi Yes, the issue happens for current devel version.")
              (comment_id . 3976)
              (post_id . 2907)
              (creation_date . 1414762929.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563))
              (owner
               (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
               (display_name . "abo-abo")
               (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2094)
               (reputation . 420)))
             ((body . "@abo-abo Phew. We can then still rely on <code>emacs -Q</code> :) The version needs to then be updated in the question title.")
              (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25#comment3977_2907")
              (body_markdown . "@abo-abo Phew. We can then still rely on `emacs -Q` :) The version needs to then be updated in the question title.")
              (comment_id . 3977)
              (post_id . 2907)
              (creation_date . 1414763003.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2094/abo-abo")
   (display_name . "abo-abo")
   (profile_image . "https://www.gravatar.com/avatar/6da30f549b191206a744fbab8003d0b5?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2094)
   (reputation . 420))
  (tags .
        ["elisp" "font-lock" "emacs-lisp-mode"])
  (answers .
           [((body . "<h1>There are two ways</h1>\n\n<h2>Redefine <code>lisp--match-hidden-arg</code>:</h2>\n\n<pre><code>(defun lisp--match-hidden-arg (limit) nil)\n</code></pre>\n\n<h2>Remove <code>lisp--match-hidden-arg</code> from <code>lisp-cl-font-lock-keywords-2</code> and <code>lisp-el-font-lock-keywords-2</code></h2>\n\n<pre><code>(setq lisp-el-font-lock-keywords-2\n      (cl-delete 'lisp--match-hidden-arg lisp-el-font-lock-keywords-2\n                 :key #'car))\n</code></pre>\n\n<h1>Please Do <em>NOT</em> Do That!</h1>\n\n<p>The coding style detected by <code>lisp--match-hidden-arg</code> is not very readable.</p>\n\n<blockquote>\n  <p>\"... a computer language is not just a way of getting a computer to\n  perform operations, but rather ... it is a novel formal medium for\n  expressing ideas about methodology\" </p>\n  \n  <p>Abelson/Sussman \"Structure and\n  Interpretation of Computer Programs\".</p>\n</blockquote>\n\n<p>You write code so that people (and you yourself a year from today!) will read and <em>understand</em> it.\nCoding conventions serve a purpose.\nPlease think before rejecting them.</p>\n")
             (title . "How to turn off &quot;error&quot; highlighting in emacs-lisp-mode for Emacs 25?")
             (link . "http://emacs.stackexchange.com/questions/2907/how-to-turn-off-error-highlighting-in-emacs-lisp-mode-for-emacs-25/2913#2913")
             (body_markdown . "There are two ways\n=\nRedefine `lisp--match-hidden-arg`:\n-\n    (defun lisp--match-hidden-arg (limit) nil)\n\nRemove `lisp--match-hidden-arg` from `lisp-cl-font-lock-keywords-2` and `lisp-el-font-lock-keywords-2`\n-\n    (setq lisp-el-font-lock-keywords-2\n          (cl-delete &#39;lisp--match-hidden-arg lisp-el-font-lock-keywords-2\n                     :key #&#39;car))\n\nPlease Do _NOT_ Do That!\n=\nThe coding style detected by `lisp--match-hidden-arg` is not very readable.\n\n&gt; &quot;... a computer language is not just a way of getting a computer to\n&gt; perform operations, but rather ... it is a novel formal medium for\n&gt; expressing ideas about methodology&quot; \n\n&gt; Abelson/Sussman &quot;Structure and\n&gt; Interpretation of Computer Programs&quot;.\n\nYou write code so that people (and you yourself a year from today!) will read and _understand_ it.\nCoding conventions serve a purpose.\nPlease think before rejecting them.")
             (question_id . 2907)
             (answer_id . 2913)
             (creation_date . 1414762888.0)
             (last_edit_date . 1414763211.0)
             (last_activity_date . 1414763211.0)
             (score . 3)
             (is_accepted . t)
             (comment_count . 0)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/795/sds")
              (display_name . "sds")
              (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 795)
              (reputation . 181))
             (owner
              (link . "http://emacs.stackexchange.com/users/795/sds")
              (display_name . "sds")
              (profile_image . "https://www.gravatar.com/avatar/75a3213bbcaf279ab4ec2787060c37d4?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 795)
              (reputation . 181)))]))
 ((body . "<p>When I close a frame, I'd like for its buffer to be killed.</p>\n\n<p>If the buffer is displayed in other frames, the buffer should not be killed.</p>\n\n<p>Do nothing if more than one buffer is displayed in a frame.</p>\n\n<p>What's a good way to set this up?</p>\n")
  (title . "Kill buffer when frame is deleted")
  (link . "http://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted")
  (body_markdown . "When I close a frame, I&#39;d like for its buffer to be killed.\n\nIf the buffer is displayed in other frames, the buffer should not be killed.\n\nDo nothing if more than one buffer is displayed in a frame.\n\nWhat&#39;s a good way to set this up?")
  (question_id . 2888)
  (creation_date . 1414725672.0)
  (last_activity_date . 1414765666.0)
  (score . 5)
  (answer_count . 3)
  (accepted_answer_id . 2915)
  (favorite_count . 2)
  (view_count . 57)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/163/dharmatech")
   (display_name . "dharmatech")
   (profile_image . "https://www.gravatar.com/avatar/d602eadb9367e8c1a2e00c0bef73555c?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 163)
   (reputation . 216))
  (tags .
        ["buffers" "frames"])
  (answers .
           [((body . "<p>Here's a simple approach:</p>\n\n<pre><code>(defun close-frame-buffer (frame)\n  (kill-buffer\n   (window-buffer\n   (frame-root-window frame))))\n\n(add-hook 'delete-frame-functions 'close-frame-buffer)\n</code></pre>\n\n<p>It will kill a buffer even if it's open in another frame however.</p>\n")
             (title . "Kill buffer when frame is deleted")
             (link . "http://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted/2889#2889")
             (body_markdown . "Here&#39;s a simple approach:\n\n    (defun close-frame-buffer (frame)\n      (kill-buffer\n       (window-buffer\n       (frame-root-window frame))))\n    \n    (add-hook &#39;delete-frame-functions &#39;close-frame-buffer)\n\nIt will kill a buffer even if it&#39;s open in another frame however.")
             (question_id . 2888)
             (answer_id . 2889)
             (creation_date . 1414725730.0)
             (last_activity_date . 1414725730.0)
             (score . 1)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/163/dharmatech")
              (display_name . "dharmatech")
              (profile_image . "https://www.gravatar.com/avatar/d602eadb9367e8c1a2e00c0bef73555c?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 163)
              (reputation . 216)))
            ((body . "\n\n<p>Below is a function (tested, but not extensively) that meets your first requirement: it kills the current buffer when you close a frame <em>UNLESS</em> the buffer is also visible in another frame.</p>\n\n<pre><code>(defun kill-buffer-when-frame-delete-dwim (frame)\n  \"Kill current buffer unless it's visible in another frame\nbesides current FRAME.\"\n  (unless (delq nil (mapcar #'(lambda (x)\n                                (memq (current-buffer)\n                                      (mapcar #'window-buffer x)))\n                            (mapcar #'window-list\n                                    (delq frame (frame-list)))))\n    (kill-buffer (current-buffer))))\n\n(add-hook 'delete-frame-functions 'kill-buffer-when-frame-delete-dwim)\n</code></pre>\n\n<p>I'm unclear on the second requirement:</p>\n\n<ul>\n<li>By <em>more than one buffer displayed in the frame</em>, do you mean the frame has a) two or more windows, <em>and</em> b) the windows have different buffers displayed in them?  </li>\n<li>By <em>do nothing</em>, do you mean a) delete the frame but do <em>not</em> kill the current buffer, <em>or</em> b) really do nothing at all: do not delete the frame or kill the current buffer?</li>\n</ul>\n")
             (title . "Kill buffer when frame is deleted")
             (link . "http://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted/2909#2909")
             (body_markdown . "&lt;!-- language: lang-el --&gt;\n\nBelow is a function (tested, but not extensively) that meets your first requirement: it kills the current buffer when you close a frame *UNLESS* the buffer is also visible in another frame.\n\n    (defun kill-buffer-when-frame-delete-dwim (frame)\n      &quot;Kill current buffer unless it&#39;s visible in another frame\n    besides current FRAME.&quot;\n      (unless (delq nil (mapcar #&#39;(lambda (x)\n                                    (memq (current-buffer)\n                                          (mapcar #&#39;window-buffer x)))\n                                (mapcar #&#39;window-list\n                                        (delq frame (frame-list)))))\n        (kill-buffer (current-buffer))))\n\n    (add-hook &#39;delete-frame-functions &#39;kill-buffer-when-frame-delete-dwim)\n\nI&#39;m unclear on the second requirement:\n\n - By *more than one buffer displayed in the frame*, do you mean the frame has a) two or more windows, *and* b) the windows have different buffers displayed in them?  \n - By *do nothing*, do you mean a) delete the frame but do *not* kill the current buffer, *or* b) really do nothing at all: do not delete the frame or kill the current buffer?\n")
             (question_id . 2888)
             (answer_id . 2909)
             (creation_date . 1414761011.0)
             (last_activity_date . 1414761011.0)
             (score . 5)
             (is_accepted . :json-false)
             (comment_count . 2)
             (comments .
                       [((body . "Hi Dan. The idea is that if more than one buffer is shown in a frame, via multiple windows in that frame, then no buffer will be killed, but the frame will still be deleted.")
                         (link . "http://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted/2909#comment4010_2909")
                         (body_markdown . "Hi Dan. The idea is that if more than one buffer is shown in a frame, via multiple windows in that frame, then no buffer will be killed, but the frame will still be deleted.")
                         (comment_id . 4010)
                         (post_id . 2909)
                         (creation_date . 1414792155.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/163/dharmatech")
                          (display_name . "dharmatech")
                          (profile_image . "https://www.gravatar.com/avatar/d602eadb9367e8c1a2e00c0bef73555c?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 163)
                          (reputation . 216)))
                        ((body . "But I could see other behaviours being useful. I.e. kill all buffers shown, as long as they aren&#39;t shown in other frames.")
                         (link . "http://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted/2909#comment4011_2909")
                         (body_markdown . "But I could see other behaviours being useful. I.e. kill all buffers shown, as long as they aren&#39;t shown in other frames.")
                         (comment_id . 4011)
                         (post_id . 2909)
                         (creation_date . 1414792208.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/163/dharmatech")
                          (display_name . "dharmatech")
                          (profile_image . "https://www.gravatar.com/avatar/d602eadb9367e8c1a2e00c0bef73555c?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 163)
                          (reputation . 216)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/253/dan")
              (display_name . "Dan")
              (profile_image . "http://i.stack.imgur.com/DVrMu.jpg?s=128&g=1")
              (accept_rate . 54)
              (user_type . "registered")
              (user_id . 253)
              (reputation . 1348)))
            ((body . "<p>If I've understood the question, here's a function that does what you want:</p>\n\n<pre><code>(defun maybe-delete-frame-buffer (frame)\n  \"When a dedicated FRAME is deleted, also kill its buffer.\nA dedicated frame contains a single window whose buffer is not\ndisplayed anywhere else.\"\n  (let ((windows (window-list frame)))\n    (when (eq 1 (length windows))\n      (let ((buffer (window-buffer (car windows))))\n        (when (eq 1 (length (get-buffer-window-list buffer nil t)))\n          (kill-buffer buffer))))))\n</code></pre>\n\n<p>You can add it as a hook that gets called whenever a frame is closed using:</p>\n\n<pre><code>(add-to-list 'delete-frame-functions #'maybe-delete-frame-buffer)\n</code></pre>\n")
             (title . "Kill buffer when frame is deleted")
             (link . "http://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted/2915#2915")
             (body_markdown . "If I&#39;ve understood the question, here&#39;s a function that does what you want:\n\n    (defun maybe-delete-frame-buffer (frame)\n      &quot;When a dedicated FRAME is deleted, also kill its buffer.\n    A dedicated frame contains a single window whose buffer is not\n    displayed anywhere else.&quot;\n      (let ((windows (window-list frame)))\n        (when (eq 1 (length windows))\n          (let ((buffer (window-buffer (car windows))))\n    	    (when (eq 1 (length (get-buffer-window-list buffer nil t)))\n    	      (kill-buffer buffer))))))\n\nYou can add it as a hook that gets called whenever a frame is closed using:\n\n    (add-to-list &#39;delete-frame-functions #&#39;maybe-delete-frame-buffer)\n\n\n")
             (question_id . 2888)
             (answer_id . 2915)
             (creation_date . 1414765666.0)
             (last_activity_date . 1414765666.0)
             (score . 5)
             (is_accepted . t)
             (comment_count . 1)
             (comments .
                       [((body . "Looks good. Thanks glucas!")
                         (link . "http://emacs.stackexchange.com/questions/2888/kill-buffer-when-frame-is-deleted/2915#comment4012_2915")
                         (body_markdown . "Looks good. Thanks glucas!")
                         (comment_id . 4012)
                         (post_id . 2915)
                         (creation_date . 1414792329.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/163/dharmatech")
                          (display_name . "dharmatech")
                          (profile_image . "https://www.gravatar.com/avatar/d602eadb9367e8c1a2e00c0bef73555c?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 163)
                          (reputation . 216)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/780/glucas")
              (display_name . "glucas")
              (profile_image . "https://www.gravatar.com/avatar/e07f5643e4e9331ad5ca713c072b19f9?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 780)
              (reputation . 474)))]))
 ((body . "<p>I hate the way that elisp (not sure if LISP in general) handles multiline docstrings.</p>\n\n<pre><code>(defun foo ()\n  \"This is\na multi\nliner\ndocstring\"\n  (do-stuff))\n</code></pre>\n\n<p>I sure do wish that I could do something like</p>\n\n<pre><code>(defun foo ()\n  (eval-when-compile \n    (concat\n      \"This is\\n\"\n       \"a multi\\n\"\n       \"line\\n\"\n       \"docstring\"))\n  (do-stuff))\n</code></pre>\n\n<p>so that the indentation was consistent.</p>\n\n<p>Unfortunately, eval-when-compile does not do the job.</p>\n\n<p>Does anyone have any ideas?</p>\n")
  (title . "Is there a better way to handle multiline docstrings in elisp?")
  (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp")
  (body_markdown . "I hate the way that elisp (not sure if LISP in general) handles multiline docstrings.\n\n    (defun foo ()\n      &quot;This is\n    a multi\n    liner\n    docstring&quot;\n      (do-stuff))\n\n\nI sure do wish that I could do something like\n\n    (defun foo ()\n      (eval-when-compile \n        (concat\n          &quot;This is\\n&quot;\n           &quot;a multi\\n&quot;\n           &quot;line\\n&quot;\n           &quot;docstring&quot;))\n      (do-stuff))\n\nso that the indentation was consistent.\n\nUnfortunately, eval-when-compile does not do the job.\n\nDoes anyone have any ideas?\n\n")
  (question_id . 2887)
  (last_edit_date . 1414741213.0)
  (creation_date . 1414725439.0)
  (last_activity_date . 1414764354.0)
  (score . 6)
  (answer_count . 3)
  (favorite_count . 1)
  (view_count . 100)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 2)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/50/malabarba")
   (display_name . "Malabarba")
   (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
   (accept_rate . 89)
   (user_type . "registered")
   (user_id . 50)
   (reputation . 3999))
  (comments .
            [((body . "It should be fairly easy to create a macro that will expand into a <code>defun</code>. The drawback to that approach – and it is a big one – is that will confuse any software (other than the elisp compiler/interpreter) that is parsing your code looking for <code>defun</code>s.")
              (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp#comment3947_2887")
              (body_markdown . "It should be fairly easy to create a macro that will expand into a `defun`. The drawback to that approach – and it is a big one – is that will confuse any software (other than the elisp compiler/interpreter) that is parsing your code looking for `defun`s.")
              (comment_id . 3947)
              (post_id . 2887)
              (creation_date . 1414745364.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/962/harald-hanche-olsen")
               (display_name . "Harald Hanche-Olsen")
               (profile_image . "https://www.gravatar.com/avatar/48c64077a6536acf7070b86eb4f02aca?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 962)
               (reputation . 131)))
             ((body . "Funnily enough, the reason why your trick doesn&#39;t work is that <code>eval-when-compile</code> quotes its result (to turn it from a value to an expression).  If it were a bit more clever and only quoted its result when it&#39;s not self-quoting, it would work.")
              (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp#comment3972_2887")
              (body_markdown . "Funnily enough, the reason why your trick doesn&#39;t work is that `eval-when-compile` quotes its result (to turn it from a value to an expression).  If it were a bit more clever and only quoted its result when it&#39;s not self-quoting, it would work.")
              (comment_id . 3972)
              (post_id . 2887)
              (creation_date . 1414762664.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/1979/stefan")
               (display_name . "Stefan")
               (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 1979)
               (reputation . 631)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2345/krazy-glew")
   (display_name . "Krazy Glew")
   (profile_image . "https://www.gravatar.com/avatar/b37fdf7928b7aecd15a973b250979333?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2345)
   (reputation . 131))
  (tags .
        ["elisp" "documentation"])
  (answers .
           [((body . "<p>You could use a macro like this:</p>\n\n<pre><code>(defmacro my-defun (name arglist &amp;rest forms)\n  \"Like `defun', but concatenates strings.\"\n  (declare (indent defun))\n  (let (doc-lines)\n    (while (and (stringp (car-safe forms))\n                (&gt; (length forms) 1))\n      (setq doc-lines\n            (append doc-lines (list (pop forms)))))\n    `(defun ,name ,arglist\n       ,(mapconcat #'identity doc-lines \"\\n\")\n       ,@forms)))\n</code></pre>\n\n<p>Then you can define your functions like this:</p>\n\n<pre><code>(my-defun test (a)\n  \"Description\"\n  \"asodksad\"\n  \"ok\"\n  (interactive)\n  (+ 1 a))\n</code></pre>\n\n<hr>\n\n<p>Still, I'd <strong>strongly</strong> recommend not going against the standards for\nsuch a marginal benefit. The “irregular indentation” that bothers you\nis just by 2 columns, not to mention it helps highlight the first line\nof documentation which is more important.</p>\n")
             (title . "Is there a better way to handle multiline docstrings in elisp?")
             (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp/2896#2896")
             (body_markdown . "You could use a macro like this:\n\n    (defmacro my-defun (name arglist &amp;rest forms)\n      &quot;Like `defun&#39;, but concatenates strings.&quot;\n      (declare (indent defun))\n      (let (doc-lines)\n        (while (and (stringp (car-safe forms))\n                    (&gt; (length forms) 1))\n          (setq doc-lines\n                (append doc-lines (list (pop forms)))))\n        `(defun ,name ,arglist\n           ,(mapconcat #&#39;identity doc-lines &quot;\\n&quot;)\n           ,@forms)))\n    \nThen you can define your functions like this:\n\n    (my-defun test (a)\n      &quot;Description&quot;\n      &quot;asodksad&quot;\n      &quot;ok&quot;\n      (interactive)\n      (+ 1 a))\n\n----------\n\nStill, I&#39;d **strongly** recommend not going against the standards for\nsuch a marginal benefit. The “irregular indentation” that bothers you\nis just by 2 columns, not to mention it helps highlight the first line\nof documentation which is more important.\n")
             (question_id . 2887)
             (answer_id . 2896)
             (creation_date . 1414745945.0)
             (last_edit_date . 1414764354.0)
             (last_activity_date . 1414764354.0)
             (score . 5)
             (is_accepted . :json-false)
             (comment_count . 2)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/50/malabarba")
              (display_name . "Malabarba")
              (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
              (accept_rate . 89)
              (user_type . "registered")
              (user_id . 50)
              (reputation . 3999))
             (comments .
                       [((body . "Actually, the body of a defun <i>is</i> evaluated (when the function is called) and it is macro-expanded when the function is defined.  So his trick should/could work.")
                         (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp/2896#comment3974_2896")
                         (body_markdown . "Actually, the body of a defun *is* evaluated (when the function is called) and it is macro-expanded when the function is defined.  So his trick should/could work.")
                         (comment_id . 3974)
                         (post_id . 2896)
                         (creation_date . 1414762756.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/1979/stefan")
                          (display_name . "Stefan")
                          (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 1979)
                          (reputation . 631)))
                        ((body . "@Stefan That&#39;s true. Forgot <code>eval-when-compile</code> was a macro.")
                         (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp/2896#comment3978_2896")
                         (body_markdown . "@Stefan That&#39;s true. Forgot `eval-when-compile` was a macro.")
                         (comment_id . 3978)
                         (post_id . 2896)
                         (creation_date . 1414764266.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (reply_to_user
                          (link . "http://emacs.stackexchange.com/users/1979/stefan")
                          (display_name . "Stefan")
                          (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 1979)
                          (reputation . 631))
                         (owner
                          (link . "http://emacs.stackexchange.com/users/50/malabarba")
                          (display_name . "Malabarba")
                          (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
                          (accept_rate . 89)
                          (user_type . "registered")
                          (user_id . 50)
                          (reputation . 3999)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/50/malabarba")
              (display_name . "Malabarba")
              (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
              (accept_rate . 89)
              (user_type . "registered")
              (user_id . 50)
              (reputation . 3999)))
            ((body . "<p>I've seen packages that define docstrings like this:</p>\n\n<pre><code>(defun my-function (x y) \"\nthis is my docstring\nthat lines always lines up\nacross multiple lines.\"\n  (+ x y))\n</code></pre>\n\n<p>Placing the first quote on the first line then starting the text on the next so they all line up. It's definitely not the standard but you wouldn't be the only one doing it.</p>\n")
             (title . "Is there a better way to handle multiline docstrings in elisp?")
             (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp/2911#2911")
             (body_markdown . "I&#39;ve seen packages that define docstrings like this:\n\n\n    (defun my-function (x y) &quot;\n    this is my docstring\n    that lines always lines up\n    across multiple lines.&quot;\n      (+ x y))\n\nPlacing the first quote on the first line then starting the text on the next so they all line up. It&#39;s definitely not the standard but you wouldn&#39;t be the only one doing it.")
             (question_id . 2887)
             (answer_id . 2911)
             (creation_date . 1414762322.0)
             (last_activity_date . 1414762322.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/160/jordon-biondo")
              (display_name . "Jordon Biondo")
              (profile_image . "http://i.stack.imgur.com/y27jq.png?s=128&g=1")
              (user_type . "registered")
              (user_id . 160)
              (reputation . 1487)))
            ((body . "<p>Of course a <code>my-defun</code> macro is the easy way out.\nBut a simpler solution would be</p>\n\n<pre><code>(advice-add 'eval-when-compile :filter-return\n            (lambda (exp)\n              (if (and (eq 'quote (car-safe exp))\n                       (stringp (cadr exp)))\n                  (cadr exp)\n                exp)))\n</code></pre>\n\n<p>Which should make your trick work, at least in all the cases where the function is macroexpanded before it's actually defined, which should include the main use cases (e.g. if it'sloaded from a file, if it's byte-compiled, or if it's defined via <code>M-C-x</code>).</p>\n\n<p>Still, this won't fix all the existing code, so maybe a better answer is something like:</p>\n\n<pre><code>;; -*- lexical-binding:t -*-\n\n(defun my-shift-docstrings (orig ppss)\n  (let ((face (funcall orig ppss)))\n    (when (eq face 'font-lock-doc-face)\n      (save-excursion\n        (let ((start (point)))\n          (parse-partial-sexp (point) (point-max) nil nil ppss 'syntax-table)\n          (while (search-backward \"\\n\" start t)\n            (put-text-property (point) (1+ (point)) 'display\n                               (propertize \"\\n  \" 'cursor 0))))))\n    face))\n\n(add-hook 'emacs-lisp-mode-hook\n          (lambda ()\n            (font-lock-mode 1)\n            (push 'display font-lock-extra-managed-props)\n            (add-function :around (local 'font-lock-syntactic-face-function)\n                          #'my-shift-docstrings)))\n</code></pre>\n\n<p>which should just shift the docstrings by 2 spaces, but only on the display side, without affecting the buffer's actual content.</p>\n")
             (title . "Is there a better way to handle multiline docstrings in elisp?")
             (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp/2912#2912")
             (body_markdown . "Of course a `my-defun` macro is the easy way out.\nBut a simpler solution would be\n\n    (advice-add &#39;eval-when-compile :filter-return\n                (lambda (exp)\n                  (if (and (eq &#39;quote (car-safe exp))\n                           (stringp (cadr exp)))\n                      (cadr exp)\n                    exp)))\n\nWhich should make your trick work, at least in all the cases where the function is macroexpanded before it&#39;s actually defined, which should include the main use cases (e.g. if it&#39;sloaded from a file, if it&#39;s byte-compiled, or if it&#39;s defined via `M-C-x`).\n\nStill, this won&#39;t fix all the existing code, so maybe a better answer is something like:\n\n    ;; -*- lexical-binding:t -*-\n\n    (defun my-shift-docstrings (orig ppss)\n      (let ((face (funcall orig ppss)))\n        (when (eq face &#39;font-lock-doc-face)\n          (save-excursion\n            (let ((start (point)))\n              (parse-partial-sexp (point) (point-max) nil nil ppss &#39;syntax-table)\n              (while (search-backward &quot;\\n&quot; start t)\n                (put-text-property (point) (1+ (point)) &#39;display\n                                   (propertize &quot;\\n  &quot; &#39;cursor 0))))))\n        face))\n\n    (add-hook &#39;emacs-lisp-mode-hook\n              (lambda ()\n                (font-lock-mode 1)\n	            (push &#39;display font-lock-extra-managed-props)\n                (add-function :around (local &#39;font-lock-syntactic-face-function)\n                              #&#39;my-shift-docstrings)))\n\nwhich should just shift the docstrings by 2 spaces, but only on the display side, without affecting the buffer&#39;s actual content.")
             (question_id . 2887)
             (answer_id . 2912)
             (creation_date . 1414762336.0)
             (last_edit_date . 1414763166.0)
             (last_activity_date . 1414763166.0)
             (score . 5)
             (is_accepted . :json-false)
             (comment_count . 1)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/1979/stefan")
              (display_name . "Stefan")
              (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 1979)
              (reputation . 631))
             (comments .
                       [((body . "I really like your second solution. But my irrational fear of advices makes me hinge at the first. :-)")
                         (link . "http://emacs.stackexchange.com/questions/2887/is-there-a-better-way-to-handle-multiline-docstrings-in-elisp/2912#comment3982_2912")
                         (body_markdown . "I really like your second solution. But my irrational fear of advices makes me hinge at the first. :-)")
                         (comment_id . 3982)
                         (post_id . 2912)
                         (creation_date . 1414765927.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/50/malabarba")
                          (display_name . "Malabarba")
                          (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
                          (accept_rate . 89)
                          (user_type . "registered")
                          (user_id . 50)
                          (reputation . 3999)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/1979/stefan")
              (display_name . "Stefan")
              (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 1979)
              (reputation . 631)))]))
 ((body . "<p>I'm using the Dvorak keyboard layout and have swapped <code>C-x</code> and <code>C-t</code>, because I find it easier to type on Dvorak.</p>\n\n<pre><code>(define-key key-translation-map [?\\C-x] [?\\C-t])\n(define-key key-translation-map [?\\C-t] [?\\C-x])\n</code></pre>\n\n<p>I'm also using <code>god-mode</code>, though, so I'd like <code>x</code> to be swapped with <code>t</code> when in <code>god-local-mode</code>, such that typing <code>x</code> results in <code>C-t</code> and typing <code>t</code> results in <code>C-x</code>.  I have a very crude solution using <code>keyboard-translate</code>:</p>\n\n<pre><code>(defun my-god-swap-on ()\n  (interactive)\n  (keyboard-translate ?x ?t)\n  (keyboard-translate ?t ?x))\n\n(defun my-god-swap-off ()\n  (interactive)\n  (aset keyboard-translate-table ?t nil)\n  (aset keyboard-translate-table ?x nil))\n\n(add-hook 'god-mode-enabled-hook 'my-god-swap-on)\n(add-hook 'god-mode-disabled-hook 'my-god-swap-off)\n</code></pre>\n\n<p>Although this works it is extremely inelegant as it modifies global state.  If I switched to a buffer where god-mode is not enabled, the translation would still be active.  I could add yet another hook for buffer switching to make sure that the translation is disabled when <code>god-local-mode</code> is inactive in the current buffer, but this seems like a very ugly mitigation technique.</p>\n\n<p>Is it possible to restrict the effects to a keyboard translation to only a single minor mode?</p>\n\n<p>If this is not possible, is there another more elegant way I could swap keys in <code>god-mode</code>?  I see that <code>god-mode</code> rewires all keys bound to <code>self-insert-command</code> to <code>god-mode-self-insert</code>, but this uses <code>this-command-keys-vector</code> to determine with what key it was called and I cannot seem to shadow the definition of <code>this-command-keys-vector</code> with a let binding to swap <code>x</code> and <code>t</code> in a wrapper before passing control to <code>god-mode-self-insert</code>.</p>\n")
  (title . "god-mode: swap keys")
  (link . "http://emacs.stackexchange.com/questions/2903/god-mode-swap-keys")
  (body_markdown . "I&#39;m using the Dvorak keyboard layout and have swapped `C-x` and `C-t`, because I find it easier to type on Dvorak.\n\n    (define-key key-translation-map [?\\C-x] [?\\C-t])\n    (define-key key-translation-map [?\\C-t] [?\\C-x])\n\nI&#39;m also using `god-mode`, though, so I&#39;d like `x` to be swapped with `t` when in `god-local-mode`, such that typing `x` results in `C-t` and typing `t` results in `C-x`.  I have a very crude solution using `keyboard-translate`:\n\n    (defun my-god-swap-on ()\n      (interactive)\n      (keyboard-translate ?x ?t)\n      (keyboard-translate ?t ?x))\n    \n    (defun my-god-swap-off ()\n      (interactive)\n      (aset keyboard-translate-table ?t nil)\n      (aset keyboard-translate-table ?x nil))\n    \n    (add-hook &#39;god-mode-enabled-hook &#39;my-god-swap-on)\n    (add-hook &#39;god-mode-disabled-hook &#39;my-god-swap-off)\n\nAlthough this works it is extremely inelegant as it modifies global state.  If I switched to a buffer where god-mode is not enabled, the translation would still be active.  I could add yet another hook for buffer switching to make sure that the translation is disabled when `god-local-mode` is inactive in the current buffer, but this seems like a very ugly mitigation technique.\n\nIs it possible to restrict the effects to a keyboard translation to only a single minor mode?\n\nIf this is not possible, is there another more elegant way I could swap keys in `god-mode`?  I see that `god-mode` rewires all keys bound to `self-insert-command` to `god-mode-self-insert`, but this uses `this-command-keys-vector` to determine with what key it was called and I cannot seem to shadow the definition of `this-command-keys-vector` with a let binding to swap `x` and `t` in a wrapper before passing control to `god-mode-self-insert`.")
  (question_id . 2903)
  (creation_date . 1414755674.0)
  (last_activity_date . 1414761822.0)
  (score . 2)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 27)
  (is_answered . :json-false)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/2005/rekado")
   (display_name . "rekado")
   (profile_image . "https://www.gravatar.com/avatar/1fd94c0bb560b4445acb97f05b0e9a81?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2005)
   (reputation . 853))
  (tags .
        ["key-bindings" "keymap" "god-mode"])
  (answers .
           [((body . "<p>I now use this:</p>\n\n<pre><code>(defun original-god-mode-lookup-key-sequence (&amp;optional key key-string-so-far) ())\n(fset 'original-god-mode-lookup-key-sequence (symbol-function 'god-mode-lookup-key-sequence))\n\n(defun god-mode-lookup-key-sequence (&amp;optional key key-string-so-far)\n  \"Wrap original-god-mode-lookup-key-sequence.  Swap x and t.\"\n  (interactive)\n  (case key\n    (?x (original-god-mode-lookup-key-sequence ?t key-string-so-far))\n    (?t (original-god-mode-lookup-key-sequence ?x key-string-so-far))\n    (t (original-god-mode-lookup-key-sequence key key-string-so-far))))\n</code></pre>\n\n<p>It makes a copy of <code>god-mode-lookup-key-sequence</code> as <code>original-god-mode-lookup-key-sequence</code> and then overrides it with a wrapper that performs the swap for <code>x</code> and <code>t</code>.  It's not elegant, but at least it won't leak.</p>\n")
             (title . "god-mode: swap keys")
             (link . "http://emacs.stackexchange.com/questions/2903/god-mode-swap-keys/2910#2910")
             (body_markdown . "I now use this:\n\n    (defun original-god-mode-lookup-key-sequence (&amp;optional key key-string-so-far) ())\n    (fset &#39;original-god-mode-lookup-key-sequence (symbol-function &#39;god-mode-lookup-key-sequence))\n    \n    (defun god-mode-lookup-key-sequence (&amp;optional key key-string-so-far)\n      &quot;Wrap original-god-mode-lookup-key-sequence.  Swap x and t.&quot;\n      (interactive)\n      (case key\n        (?x (original-god-mode-lookup-key-sequence ?t key-string-so-far))\n        (?t (original-god-mode-lookup-key-sequence ?x key-string-so-far))\n        (t (original-god-mode-lookup-key-sequence key key-string-so-far))))\n\nIt makes a copy of `god-mode-lookup-key-sequence` as `original-god-mode-lookup-key-sequence` and then overrides it with a wrapper that performs the swap for `x` and `t`.  It&#39;s not elegant, but at least it won&#39;t leak.")
             (question_id . 2903)
             (answer_id . 2910)
             (creation_date . 1414761822.0)
             (last_activity_date . 1414761822.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/2005/rekado")
              (display_name . "rekado")
              (profile_image . "https://www.gravatar.com/avatar/1fd94c0bb560b4445acb97f05b0e9a81?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 2005)
              (reputation . 853)))]))
 ((body . "<p>I used to have a configuration for <kbd>TAB</kbd> which was doing something like this :</p>\n\n<p>This is the code before hitting <kbd>TAB</kbd>.</p>\n\n<pre><code>template&lt;typename T&gt;\nclass A\n{\n\n    }; // This is where I hit TAB\n</code></pre>\n\n<p>The code auto-indented as below after hitting <kbd>TAB</kbd>:</p>\n\n<pre><code>template&lt;typename T&gt;\nclass A\n{\n\n};\n</code></pre>\n\n<p>The <kbd>TAB</kbd> key used to work as above but now it does only auto-complete.</p>\n\n<p>Can I fix this ? I can use <kbd>TAB</kbd> with <kbd>C-q</kbd><kbd>TAB</kbd> but it is hard to do always. </p>\n\n<p>Here is my .emacs file</p>\n\n<pre><code>(add-to-list 'load-path \"~/.emacs.d/jdee-2.4.1/lisp\")\n(load \"jde\")\n\n(add-to-list 'load-path \"~/.emacs.d\")\n(require 'linum)\n(global-linum-mode 1)\n\n(add-to-list 'load-path \"~/.emacs.d/elpa/yasnippet-0.8.0\")\n(require 'yasnippet)\n(yas-global-mode 1)\n; care about this place\n(define-key yas-minor-mode-map (kbd \"&lt;tab&gt;\") nil)\n(define-key yas-minor-mode-map (kbd \"TAB\") nil)\n(define-key yas-minor-mode-map (kbd \"&lt;backtab&gt;\") 'yas-expand)\n; care about this place\n(setq ac-source-yasnippet nil)\n\n\n(add-to-list 'load-path \"~/.emacs.d/auto-complete-1.3.1\")\n(require 'auto-complete-config)\n(ac-config-default)\n(add-to-list 'ac-dictionary-directories \"~/.emacs.d/ac-dict\")\n(global-auto-complete-mode t) ; section1\n(ac-set-trigger-key \"TAB\")\n(ac-set-trigger-key \"&lt;tab&gt;\")\n\n(defun my:ac-c-headers-init ()\n  (require 'auto-complete-c-headers)\n  (add-to-list 'ac-sources 'ac-source-c-headers))\n\n(add-hook 'c++-mode-hook 'my:ac-c-headers-init)\n(add-hook 'c-mode-hook 'my:ac-c-headers-init)\n\n(add-to-list 'load-path \"~/.emacs.d/iedit\")\n(require 'iedit)\n(define-key global-map (kbd \"C-c ;\") 'iedit-mode)\n\n(semantic-mode 1)\n(defun my:add-semantic-to-autocomplete() \n  (add-to-list 'ac-sources 'ac-source-semantic)\n)\n(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)\n(custom-set-variables\n ;; custom-set-variables was added by Custom.\n ;; If you edit it by hand, you could mess it up, so be careful.\n ;; Your init file should contain only one such instance.\n ;; If there is more than one, they won't work right.\n '(ansi-color-names-vector [\"#242424\" \"#e5786d\" \"#95e454\" \"#cae682\" \"#8ac6f2\" \"#333366\" \"#ccaa8f\" \"#f6f3e8\"])\n '(cua-mode t nil (cua-base))\n '(custom-enabled-themes (quote (tango-dark)))\n '(font-use-system-font t)\n '(show-paren-mode t)\n '(uniquify-buffer-name-style (quote forward) nil (uniquify)))\n(custom-set-faces\n ;; custom-set-faces was added by Custom.\n ;; If you edit it by hand, you could mess it up, so be careful.\n ;; Your init file should contain only one such instance.\n ;; If there is more than one, they won't work right.\n '(default ((t (:family \"DejaVu Sans Mono\" :foundry \"unknown\" :slant normal :weight bold :height 113 :width normal)))))\n(setq-default indent-tabs-mode nil)\n(setq-default tab-width 2)\n(setq indent-line-function 'insert-tab)\n</code></pre>\n\n<p><strong>Edit</strong> When I add JDE it fixed but now I cannot use auto-complete ? It doesn't complete anything.</p>\n")
  (title . "TAB does not auto-indent lines anymore")
  (link . "http://emacs.stackexchange.com/questions/2904/tab-does-not-auto-indent-lines-anymore")
  (body_markdown . "I used to have a configuration for &lt;kbd&gt;TAB&lt;/kbd&gt; which was doing something like this :\n\nThis is the code before hitting &lt;kbd&gt;TAB&lt;/kbd&gt;.\n\n    template&lt;typename T&gt;\n    class A\n    {\n    \n        }; // This is where I hit TAB\n    \nThe code auto-indented as below after hitting &lt;kbd&gt;TAB&lt;/kbd&gt;:\n\n    template&lt;typename T&gt;\n    class A\n    {\n    \n    };\n\nThe &lt;kbd&gt;TAB&lt;/kbd&gt; key used to work as above but now it does only auto-complete.\n\nCan I fix this ? I can use &lt;kbd&gt;TAB&lt;/kbd&gt; with &lt;kbd&gt;C-q&lt;/kbd&gt;&lt;kbd&gt;TAB&lt;/kbd&gt; but it is hard to do always. \n\nHere is my .emacs file\n\n    (add-to-list &#39;load-path &quot;~/.emacs.d/jdee-2.4.1/lisp&quot;)\n    (load &quot;jde&quot;)\n    \n    (add-to-list &#39;load-path &quot;~/.emacs.d&quot;)\n    (require &#39;linum)\n    (global-linum-mode 1)\n    \n    (add-to-list &#39;load-path &quot;~/.emacs.d/elpa/yasnippet-0.8.0&quot;)\n    (require &#39;yasnippet)\n    (yas-global-mode 1)\n    ; care about this place\n    (define-key yas-minor-mode-map (kbd &quot;&lt;tab&gt;&quot;) nil)\n    (define-key yas-minor-mode-map (kbd &quot;TAB&quot;) nil)\n    (define-key yas-minor-mode-map (kbd &quot;&lt;backtab&gt;&quot;) &#39;yas-expand)\n    ; care about this place\n    (setq ac-source-yasnippet nil)\n    \n    \n    (add-to-list &#39;load-path &quot;~/.emacs.d/auto-complete-1.3.1&quot;)\n    (require &#39;auto-complete-config)\n    (ac-config-default)\n    (add-to-list &#39;ac-dictionary-directories &quot;~/.emacs.d/ac-dict&quot;)\n    (global-auto-complete-mode t) ; section1\n    (ac-set-trigger-key &quot;TAB&quot;)\n    (ac-set-trigger-key &quot;&lt;tab&gt;&quot;)\n    \n    (defun my:ac-c-headers-init ()\n      (require &#39;auto-complete-c-headers)\n      (add-to-list &#39;ac-sources &#39;ac-source-c-headers))\n    \n    (add-hook &#39;c++-mode-hook &#39;my:ac-c-headers-init)\n    (add-hook &#39;c-mode-hook &#39;my:ac-c-headers-init)\n    \n    (add-to-list &#39;load-path &quot;~/.emacs.d/iedit&quot;)\n    (require &#39;iedit)\n    (define-key global-map (kbd &quot;C-c ;&quot;) &#39;iedit-mode)\n    \n    (semantic-mode 1)\n    (defun my:add-semantic-to-autocomplete() \n      (add-to-list &#39;ac-sources &#39;ac-source-semantic)\n    )\n    (add-hook &#39;c-mode-common-hook &#39;my:add-semantic-to-autocomplete)\n    (custom-set-variables\n     ;; custom-set-variables was added by Custom.\n     ;; If you edit it by hand, you could mess it up, so be careful.\n     ;; Your init file should contain only one such instance.\n     ;; If there is more than one, they won&#39;t work right.\n     &#39;(ansi-color-names-vector [&quot;#242424&quot; &quot;#e5786d&quot; &quot;#95e454&quot; &quot;#cae682&quot; &quot;#8ac6f2&quot; &quot;#333366&quot; &quot;#ccaa8f&quot; &quot;#f6f3e8&quot;])\n     &#39;(cua-mode t nil (cua-base))\n     &#39;(custom-enabled-themes (quote (tango-dark)))\n     &#39;(font-use-system-font t)\n     &#39;(show-paren-mode t)\n     &#39;(uniquify-buffer-name-style (quote forward) nil (uniquify)))\n    (custom-set-faces\n     ;; custom-set-faces was added by Custom.\n     ;; If you edit it by hand, you could mess it up, so be careful.\n     ;; Your init file should contain only one such instance.\n     ;; If there is more than one, they won&#39;t work right.\n     &#39;(default ((t (:family &quot;DejaVu Sans Mono&quot; :foundry &quot;unknown&quot; :slant normal :weight bold :height 113 :width normal)))))\n    (setq-default indent-tabs-mode nil)\n    (setq-default tab-width 2)\n    (setq indent-line-function &#39;insert-tab)\n\n**Edit** When I add JDE it fixed but now I cannot use auto-complete ? It doesn&#39;t complete anything.\n")
  (question_id . 2904)
  (last_edit_date . 1414760438.0)
  (creation_date . 1414757159.0)
  (last_activity_date . 1414760438.0)
  (score . 1)
  (answer_count . 0)
  (favorite_count . 0)
  (view_count . 27)
  (is_answered . :json-false)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 6)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
   (display_name . "kaushalmodi")
   (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
   (accept_rate . 88)
   (user_type . "registered")
   (user_id . 115)
   (reputation . 2563))
  (comments .
            [((body . "Are you talking about using TAB to move to next field when inserting a yasnippet template?")
              (link . "http://emacs.stackexchange.com/questions/2904/tab-does-not-auto-indent-lines-anymore#comment3958_2904")
              (body_markdown . "Are you talking about using TAB to move to next field when inserting a yasnippet template?")
              (comment_id . 3958)
              (post_id . 2904)
              (creation_date . 1414757555.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563)))
             ((body . "Or are you asking how to use TAB for template insertion instead of auto completion?")
              (link . "http://emacs.stackexchange.com/questions/2904/tab-does-not-auto-indent-lines-anymore#comment3959_2904")
              (body_markdown . "Or are you asking how to use TAB for template insertion instead of auto completion?")
              (comment_id . 3959)
              (post_id . 2904)
              (creation_date . 1414757716.0)
              (post_type . "question")
              (score . 1)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563)))
             ((body . "second one, my TAB button is doing just complete things, I do not have any problem with yasnippet or auto-complete.")
              (link . "http://emacs.stackexchange.com/questions/2904/tab-does-not-auto-indent-lines-anymore#comment3960_2904")
              (body_markdown . "second one, my TAB button is doing just complete things, I do not have any problem with yasnippet or auto-complete.")
              (comment_id . 3960)
              (post_id . 2904)
              (creation_date . 1414758099.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563))
              (owner
               (link . "http://emacs.stackexchange.com/users/2305/vivian-maya")
               (display_name . "Vivian Maya")
               (profile_image . "http://i.stack.imgur.com/qkoVH.jpg?s=128&g=1")
               (user_type . "registered")
               (user_id . 2305)
               (reputation . 81)))
             ((body . "Bind TAB to one thing only, and should be used for auto-complete. It would be easier. You can bind RET to <code>newline-and-indent</code> for automatic indentation: <code>(global-set-key (kbd &quot;RET&quot;) &#39;newline-and-indent)</code>. You can indent whole buffer or region (if selected) using the code snippet <a href=\"http://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/\" rel=\"nofollow\">here</a>.")
              (link . "http://emacs.stackexchange.com/questions/2904/tab-does-not-auto-indent-lines-anymore#comment3962_2904")
              (body_markdown . "Bind TAB to one thing only, and should be used for auto-complete. It would be easier. You can bind RET to `newline-and-indent` for automatic indentation: `(global-set-key (kbd &quot;RET&quot;) &#39;newline-and-indent)`. You can indent whole buffer or region (if selected) using the code snippet [here](http://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/).")
              (comment_id . 3962)
              (post_id . 2904)
              (creation_date . 1414760198.0)
              (post_type . "question")
              (score . 1)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))
             ((body . "@TuDo Could you look at my edit, how can i enable y auto-complete for jde,it doesn&#39;t work when I coding java.")
              (link . "http://emacs.stackexchange.com/questions/2904/tab-does-not-auto-indent-lines-anymore#comment3963_2904")
              (body_markdown . "@TuDo Could you look at my edit, how can i enable y auto-complete for jde,it doesn&#39;t work when I coding java.")
              (comment_id . 3963)
              (post_id . 2904)
              (creation_date . 1414760405.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652))
              (owner
               (link . "http://emacs.stackexchange.com/users/2305/vivian-maya")
               (display_name . "Vivian Maya")
               (profile_image . "http://i.stack.imgur.com/qkoVH.jpg?s=128&g=1")
               (user_type . "registered")
               (user_id . 2305)
               (reputation . 81)))
             ((body . "@VivianMaya Feel free to revert to your original question if I have misunderstood what you meant. I have reworded the question title and content.")
              (link . "http://emacs.stackexchange.com/questions/2904/tab-does-not-auto-indent-lines-anymore#comment3964_2904")
              (body_markdown . "@VivianMaya Feel free to revert to your original question if I have misunderstood what you meant. I have reworded the question title and content.")
              (comment_id . 3964)
              (post_id . 2904)
              (creation_date . 1414760513.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/115/kaushalmodi")
               (display_name . "kaushalmodi")
               (profile_image . "https://www.gravatar.com/avatar/6dbe470c7ef3817bb1da85dc5883acbe?s=128&d=identicon&r=PG")
               (accept_rate . 88)
               (user_type . "registered")
               (user_id . 115)
               (reputation . 2563)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2305/vivian-maya")
   (display_name . "Vivian Maya")
   (profile_image . "http://i.stack.imgur.com/qkoVH.jpg?s=128&g=1")
   (user_type . "registered")
   (user_id . 2305)
   (reputation . 81))
  (tags .
        ["indentation"]))
 ((body . "<p>I log into numerous machines as part of my job (+- 20 per day). I only spend a few days/weeks with each machine. Many run ftp only.</p>\n\n<p>For seamless access Tramp is amazing, but it is the <em>managing</em> of these server's access by hand that has become tedious. </p>\n\n<p>I manage a .netrc and a .ssh/config by hand, along with any keys. Often a password/key will change during my time with the machine, and I will have to re-edit the files.</p>\n\n<p>Is there an effective solution for managing (machines/passwords/keys) from within emacs?</p>\n")
  (title . "Password/Key Management for multiple machines in emacs")
  (link . "http://emacs.stackexchange.com/questions/2902/password-key-management-for-multiple-machines-in-emacs")
  (body_markdown . "I log into numerous machines as part of my job (+- 20 per day). I only spend a few days/weeks with each machine. Many run ftp only.\n\nFor seamless access Tramp is amazing, but it is the *managing* of these server&#39;s access by hand that has become tedious. \n\nI manage a .netrc and a .ssh/config by hand, along with any keys. Often a password/key will change during my time with the machine, and I will have to re-edit the files.\n\nIs there an effective solution for managing (machines/passwords/keys) from within emacs?\n")
  (question_id . 2902)
  (creation_date . 1414755561.0)
  (last_activity_date . 1414760173.0)
  (score . 7)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 60)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/294/gambo")
   (display_name . "Gambo")
   (profile_image . "https://www.gravatar.com/avatar/009a0dc9a1e798678bb8a8bf9f4b6817?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 294)
   (reputation . 175))
  (tags .
        ["tramp"])
  (answers .
           [((body . "<p>I use a simple password manager called <a href=\"http://www.passwordstore.org/\" rel=\"nofollow\">pass</a>. It offers a simple command line interface with is ideal for integrating with Emacs. The backing store is a GPG encrypted GIT repo. It actually ships with an <a href=\"http://git.zx2c4.com/password-store/tree/contrib/emacs/password-store.el\" rel=\"nofollow\">Emacs package</a> although I don't use it. My interface is laughably simple:</p>\n\n<pre><code>(defun my-fixup-gpg-agent (frame)\n  \"Tweak DISPLAY and GPG_TTY environment variables as appropriate to `FRAME'.\"\n  (when (fboundp 'keychain-refresh-environment)\n    (keychain-refresh-environment))\n  (if (display-graphic-p frame)\n      (setenv \"DISPLAY\" (terminal-name frame))\n    (setenv \"GPG_TTY\" (terminal-name frame))\n    (setenv \"DISPLAY\" nil)))\n\n(add-hook 'after-make-frame-functions 'my-fixup-gpg-agent)\n\n;; Simple caching\n(defvar my-cached-passwords\n  nil\n  \"Cache of passwords. Stored in plain text so you only want to cache\n  them if of low value.\")\n\n(defun my-pass-password (pass-name &amp;optional cache)\n  \"Return the password for the `PASS-NAME'.\"\n  (let ((cached-pass (assoc-default pass-name my-cached-passwords)))\n    (if cached-pass\n        cached-pass\n      (when (selected-frame)\n        (my-fixup-gpg-agent (selected-frame))\n        (let ((new-pass (chomp\n                         (shell-command-to-string\n                          (format \"pass %s\" pass-name)))))\n          (when (and new-pass cache)\n            (add-to-list 'my-cached-passwords (cons pass-name new-pass)))\n          new-pass)))))\n</code></pre>\n")
             (title . "Password/Key Management for multiple machines in emacs")
             (link . "http://emacs.stackexchange.com/questions/2902/password-key-management-for-multiple-machines-in-emacs/2908#2908")
             (body_markdown . "I use a simple password manager called [pass](http://www.passwordstore.org/). It offers a simple command line interface with is ideal for integrating with Emacs. The backing store is a GPG encrypted GIT repo. It actually ships with an [Emacs package](http://git.zx2c4.com/password-store/tree/contrib/emacs/password-store.el) although I don&#39;t use it. My interface is laughably simple:\n\n    (defun my-fixup-gpg-agent (frame)\n      &quot;Tweak DISPLAY and GPG_TTY environment variables as appropriate to `FRAME&#39;.&quot;\n      (when (fboundp &#39;keychain-refresh-environment)\n        (keychain-refresh-environment))\n      (if (display-graphic-p frame)\n          (setenv &quot;DISPLAY&quot; (terminal-name frame))\n        (setenv &quot;GPG_TTY&quot; (terminal-name frame))\n        (setenv &quot;DISPLAY&quot; nil)))\n\n    (add-hook &#39;after-make-frame-functions &#39;my-fixup-gpg-agent)\n\n    ;; Simple caching\n    (defvar my-cached-passwords\n      nil\n      &quot;Cache of passwords. Stored in plain text so you only want to cache\n      them if of low value.&quot;)\n\n    (defun my-pass-password (pass-name &amp;optional cache)\n      &quot;Return the password for the `PASS-NAME&#39;.&quot;\n      (let ((cached-pass (assoc-default pass-name my-cached-passwords)))\n        (if cached-pass\n            cached-pass\n          (when (selected-frame)\n            (my-fixup-gpg-agent (selected-frame))\n            (let ((new-pass (chomp\n                             (shell-command-to-string\n                              (format &quot;pass %s&quot; pass-name)))))\n              (when (and new-pass cache)\n                (add-to-list &#39;my-cached-passwords (cons pass-name new-pass)))\n              new-pass)))))")
             (question_id . 2902)
             (answer_id . 2908)
             (creation_date . 1414760173.0)
             (last_activity_date . 1414760173.0)
             (score . 3)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/129/stsquad")
              (display_name . "stsquad")
              (profile_image . "https://www.gravatar.com/avatar/2a1ad379159a4f4eaf5b54c5dbfe235a?s=128&d=identicon&r=PG")
              (accept_rate . 75)
              (user_type . "registered")
              (user_id . 129)
              (reputation . 428)))]))
 ((body . "<p>I'm trying to install <a href=\"https://github.com/ejmr/php-mode\" rel=\"nofollow\">php-mode</a>, but I keep encountering this error when I run <code>M-x php-mode</code>:</p>\n\n<pre><code>Symbol's function definition is void: cl-macroexpand-all\n</code></pre>\n\n<p>The error persists whether I install php-mode via MELPA or manually.</p>\n\n<p>Environment: Emacs 24.3.94.1 (x86_64-apple-darwin13.4.0, NS apple-appkit-1265.21)\n of 2014-10-04 on builder10-9.porkrind.org</p>\n\n<p>Why won't php-mode run? Note: I'm relatively new to Emacs, and inexperienced.</p>\n")
  (title . "&quot;Symbol&#39;s function definition is void: cl-macroexpand-all&quot; when trying to install php-mode")
  (link . "http://emacs.stackexchange.com/questions/2864/symbols-function-definition-is-void-cl-macroexpand-all-when-trying-to-instal")
  (body_markdown . "I&#39;m trying to install [php-mode](https://github.com/ejmr/php-mode), but I keep encountering this error when I run `M-x php-mode`:\n\n    Symbol&#39;s function definition is void: cl-macroexpand-all\n\nThe error persists whether I install php-mode via MELPA or manually.\n\nEnvironment: Emacs 24.3.94.1 (x86_64-apple-darwin13.4.0, NS apple-appkit-1265.21)\n of 2014-10-04 on builder10-9.porkrind.org\n\nWhy won&#39;t php-mode run? Note: I&#39;m relatively new to Emacs, and inexperienced.")
  (question_id . 2864)
  (last_edit_date . 1414701459.0)
  (creation_date . 1414700357.0)
  (last_activity_date . 1414759512.0)
  (score . 2)
  (answer_count . 2)
  (accepted_answer_id . 2866)
  (favorite_count . 0)
  (view_count . 28)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/50/malabarba")
   (display_name . "Malabarba")
   (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
   (accept_rate . 89)
   (user_type . "registered")
   (user_id . 50)
   (reputation . 3999))
  (owner
   (link . "http://emacs.stackexchange.com/users/2342/cg433n")
   (display_name . "cg433n")
   (profile_image . "https://www.gravatar.com/avatar/8910de37dfedaf187c4b39005b809bdf?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2342)
   (reputation . 13))
  (tags .
        ["package" "cl-lib" "php-mode"])
  (answers .
           [((body . "<p>The function <code>cl-macroexpand-all</code> has been obsoleted in Emacs 24.3; you are now supposed to use <code>macroexpand-all</code> instead, which is part of core Emacs, not the CL library.</p>\n\n<p>The CL library defines <code>cl-macroexpand-all</code> as an alias for <code>macroexpand-all</code>, so in principle you could do</p>\n\n<pre><code>(require 'cl)\n</code></pre>\n\n<p>in your init file to fix the problem.  However, the <code>cl</code> library itself is being deprecated, and using it is not recommended.</p>\n\n<p>I think you should contact the authors of <code>php-mode</code> and tell them to use <code>macroexpand-all</code>.</p>\n")
             (title . "&quot;Symbol&#39;s function definition is void: cl-macroexpand-all&quot; when trying to install php-mode")
             (link . "http://emacs.stackexchange.com/questions/2864/symbols-function-definition-is-void-cl-macroexpand-all-when-trying-to-instal/2866#2866")
             (body_markdown . "The function `cl-macroexpand-all` has been obsoleted in Emacs 24.3; you are now supposed to use `macroexpand-all` instead, which is part of core Emacs, not the CL library.\n\nThe CL library defines `cl-macroexpand-all` as an alias for `macroexpand-all`, so in principle you could do\n\n    (require &#39;cl)\n\nin your init file to fix the problem.  However, the `cl` library itself is being deprecated, and using it is not recommended.\n\nI think you should contact the authors of `php-mode` and tell them to use `macroexpand-all`.")
             (question_id . 2864)
             (answer_id . 2866)
             (creation_date . 1414701270.0)
             (last_activity_date . 1414701270.0)
             (score . 2)
             (is_accepted . t)
             (comment_count . 1)
             (comments .
                       [((body . "The dev seems to be aware of the issue: <a href=\"https://github.com/ejmr/php-mode/issues/202\" rel=\"nofollow\">ejmr/php-mode#202</a>")
                         (link . "http://emacs.stackexchange.com/questions/2864/symbols-function-definition-is-void-cl-macroexpand-all-when-trying-to-instal/2866#comment3991_2866")
                         (body_markdown . "The dev seems to be aware of the issue: [ejmr/php-mode#202](https://github.com/ejmr/php-mode/issues/202)")
                         (comment_id . 3991)
                         (post_id . 2866)
                         (creation_date . 1414770635.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/2342/cg433n")
                          (display_name . "cg433n")
                          (profile_image . "https://www.gravatar.com/avatar/8910de37dfedaf187c4b39005b809bdf?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 2342)
                          (reputation . 13)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/2223/jch")
              (display_name . "jch")
              (profile_image . "https://www.gravatar.com/avatar/8b728c7d6c1dce81f5a82249b7617756?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 2223)
              (reputation . 425)))
            ((body . "<p>It's bug#18845 in Emacs-24.4.  To work around it, you need to add <code>(require 'cl)</code> somewhere before you load php-mode.</p>\n")
             (title . "&quot;Symbol&#39;s function definition is void: cl-macroexpand-all&quot; when trying to install php-mode")
             (link . "http://emacs.stackexchange.com/questions/2864/symbols-function-definition-is-void-cl-macroexpand-all-when-trying-to-instal/2906#2906")
             (body_markdown . "It&#39;s bug#18845 in Emacs-24.4.  To work around it, you need to add `(require &#39;cl)` somewhere before you load php-mode.")
             (question_id . 2864)
             (answer_id . 2906)
             (creation_date . 1414759512.0)
             (last_activity_date . 1414759512.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/1979/stefan")
              (display_name . "Stefan")
              (profile_image . "https://www.gravatar.com/avatar/6d7d7689f9562293f6106353a6b1a905?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 1979)
              (reputation . 631)))]))
 ((body . "<p>Although Emacs (24.3.93.1) runs fine if launched from the Finder, if the terminal version is launched:</p>\n\n<p><code>/Applications/Emacs.app/Contents/MacOS/Emacs -nw</code></p>\n\n<p>Emacs crashes with:</p>\n\n<p><code>Fatal error 11: Segmentation fault[1]    51512 abort      /Applications/Emacs.app/Contents/MacOS/Emacs -nw</code>.</p>\n\n<p>It seems the issue is caused by a single line in init.el,</p>\n\n<p><code>(set-face-attribute 'default nil :font  \"Menlo-16\")</code></p>\n\n<p>If that line is commented out, the terminal version of Emacs will start fine too.</p>\n\n<p>To pinpoint the cause of the crash took me several hours (diminishing my init.el half by half).</p>\n\n<p>I am aware that in any case Emacs will inherit whatever font and font size is specified in the terminal app (basically, that line is not meaningful in the cli.)</p>\n\n<ol>\n<li>Generally speaking, is there a better way to debug a crashing Emacs? Perhaps using some kind of cli debugger that would print a more descriptive message?</li>\n<li>Why is that line crashing Emacs via cli, but not if launched from the Finder?</li>\n</ol>\n")
  (title . "Better debugging of crashing Emacs?")
  (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs")
  (body_markdown . "Although Emacs (24.3.93.1) runs fine if launched from the Finder, if the terminal version is launched:\n\n`/Applications/Emacs.app/Contents/MacOS/Emacs -nw`\n\nEmacs crashes with:\n\n`Fatal error 11: Segmentation fault[1]    51512 abort      /Applications/Emacs.app/Contents/MacOS/Emacs -nw`.\n\nIt seems the issue is caused by a single line in init.el,\n\n`(set-face-attribute &#39;default nil :font  &quot;Menlo-16&quot;)`\n\nIf that line is commented out, the terminal version of Emacs will start fine too.\n\nTo pinpoint the cause of the crash took me several hours (diminishing my init.el half by half).\n\nI am aware that in any case Emacs will inherit whatever font and font size is specified in the terminal app (basically, that line is not meaningful in the cli.)\n\n 1. Generally speaking, is there a better way to debug a crashing Emacs? Perhaps using some kind of cli debugger that would print a more descriptive message?\n 2. Why is that line crashing Emacs via cli, but not if launched from the Finder?\n")
  (question_id . 363)
  (last_edit_date . 1411830937.0)
  (creation_date . 1411809580.0)
  (last_activity_date . 1414757838.0)
  (score . 4)
  (answer_count . 2)
  (accepted_answer_id . 373)
  (favorite_count . 1)
  (view_count . 63)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 11)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/249/gsl")
   (display_name . "gsl")
   (profile_image . "https://www.gravatar.com/avatar/f56e324b45b74edb72eb1d230554f83f?s=128&d=identicon&r=PG")
   (accept_rate . 100)
   (user_type . "registered")
   (user_id . 249)
   (reputation . 79))
  (comments .
            [((body . "is it really launching from the cli? or launching the terminal version that is causing the problem?  Try putting some error catching around the set-face-attribute? (condition-case err (set-face-attribute...) (error (message &quot;Whoops!&quot;)))")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment496_363")
              (body_markdown . "is it really launching from the cli? or launching the terminal version that is causing the problem?\n\nTry putting some error catching around the set-face-attribute? (condition-case err (set-face-attribute...) (error (message &quot;Whoops!&quot;)))")
              (comment_id . 496)
              (post_id . 363)
              (creation_date . 1411821158.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/362/nic-ferrier")
               (display_name . "nic ferrier")
               (profile_image . "https://www.gravatar.com/avatar/2ace292d4a9e6612e6cd761a1046f4f3?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 362)
               (reputation . 101)))
             ((body . "File a bug report. Emacs should never crash due to lisp code. But this may be a problem with the specific build you&#39;re using, is it the official release?")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment497_363")
              (body_markdown . "File a bug report. Emacs should never crash due to lisp code. But this may be a problem with the specific build you&#39;re using, is it the official release?")
              (comment_id . 497)
              (post_id . 363)
              (creation_date . 1411825058.0)
              (post_type . "question")
              (score . 2)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "@nic-ferrier: I have now just one line in init.el, <code>(condition-case err (set-face-attribute &#39;default nil :font  &quot;Menlo-16&quot;) (error (message &quot;Whoops!&quot;)))</code> still I have the same crash with same error message. No additional elisp-originating messages.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment498_363")
              (body_markdown . "@nic-ferrier: I have now just one line in init.el, `(condition-case err (set-face-attribute &#39;default nil :font  &quot;Menlo-16&quot;) (error (message &quot;Whoops!&quot;)))` still I have the same crash with same error message. No additional elisp-originating messages.")
              (comment_id . 498)
              (post_id . 363)
              (creation_date . 1411827222.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/362/nic-ferrier")
               (display_name . "nic ferrier")
               (profile_image . "https://www.gravatar.com/avatar/2ace292d4a9e6612e6cd761a1046f4f3?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 362)
               (reputation . 101))
              (owner
               (link . "http://emacs.stackexchange.com/users/249/gsl")
               (display_name . "gsl")
               (profile_image . "https://www.gravatar.com/avatar/f56e324b45b74edb72eb1d230554f83f?s=128&d=identicon&r=PG")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 249)
               (reputation . 79)))
             ((body . "@ malabarba: I have tried with <code>GNU Emacs 24.3.1 (x86_64-apple-darwin, NS apple-appkit-1038.36) of 2013-03-13 on bob.porkrind.org</code> from <a href=\"http://emacsformacosx.com\" rel=\"nofollow\">emacsformacosx.com</a>, and Emacs does not crash. So, it must be a bug in later versions.  I shall file a bug report.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment499_363")
              (body_markdown . "@ malabarba: I have tried with `GNU Emacs 24.3.1 (x86_64-apple-darwin, NS apple-appkit-1038.36) of 2013-03-13 on bob.porkrind.org` from http://emacsformacosx.com, and Emacs does not crash. So, it must be a bug in later versions.\n\nI shall file a bug report.")
              (comment_id . 499)
              (post_id . 363)
              (creation_date . 1411827657.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/249/gsl")
               (display_name . "gsl")
               (profile_image . "https://www.gravatar.com/avatar/f56e324b45b74edb72eb1d230554f83f?s=128&d=identicon&r=PG")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 249)
               (reputation . 79)))
             ((body . "@nic-ferrier @malabarba Would you mind adding an answer to point 1, so that this could be useful to others for future similar issues? Tracking the source of the bug was really time consuming. Is there any way to use an external debugger? Using @nic-ferrier&#39;s suggestion of <code>condition-case err</code> did not work in this case.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment500_363")
              (body_markdown . "@nic-ferrier @malabarba Would you mind adding an answer to point 1, so that this could be useful to others for future similar issues? Tracking the source of the bug was really time consuming. Is there any way to use an external debugger? Using @nic-ferrier&#39;s suggestion of `condition-case err` did not work in this case.")
              (comment_id . 500)
              (post_id . 363)
              (creation_date . 1411827812.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/362/nic-ferrier")
               (display_name . "nic ferrier")
               (profile_image . "https://www.gravatar.com/avatar/2ace292d4a9e6612e6cd761a1046f4f3?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 362)
               (reputation . 101))
              (owner
               (link . "http://emacs.stackexchange.com/users/249/gsl")
               (display_name . "gsl")
               (profile_image . "https://www.gravatar.com/avatar/f56e324b45b74edb72eb1d230554f83f?s=128&d=identicon&r=PG")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 249)
               (reputation . 79)))
             ((body . "@gsl, are you saying the condition case didn&#39;t catch it? I can reproduce the seg fault over here, but wrapping it in a condition case does stop the crash, the error I get when wrapped is that :font is not a face attribute which makes sense in a -nw environment, but the crash should not happen, only the error.  specifically, the error I get is: <code>(error &quot;Invalid face attribute name&quot; :face)</code> when condition-cased  That being said, you should use <code>set-default-font</code> to set the font, not set-face-attribute.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment502_363")
              (body_markdown . "@gsl, are you saying the condition case didn&#39;t catch it? I can reproduce the seg fault over here, but wrapping it in a condition case does stop the crash, the error I get when wrapped is that :font is not a face attribute which makes sense in a -nw environment, but the crash should not happen, only the error.\n\nspecifically, the error I get is: `(error &quot;Invalid face attribute name&quot; :face)` when condition-cased\n\nThat being said, you should use `set-default-font` to set the font, not set-face-attribute.")
              (comment_id . 502)
              (post_id . 363)
              (creation_date . 1411829310.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/160/jordon-biondo")
               (display_name . "Jordon Biondo")
               (profile_image . "http://i.stack.imgur.com/y27jq.png?s=128&g=1")
               (user_type . "registered")
               (user_id . 160)
               (reputation . 1487)))
             ((body . "@jordon-biondo Yes, if I have just this line <code>(condition-case err (set-face-attribute &#39;default nil :font &quot;Menlo-16&quot;) (error (message &quot;Whoops!&quot;)))</code>, the condition case didn&#39;t catch it. I just get <code>Fatal error 11: Segmentation fault</code>. Thank you for pointing out to use <code>set-default-font</code> instead.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment505_363")
              (body_markdown . "@jordon-biondo Yes, if I have just this line `(condition-case err (set-face-attribute &#39;default nil :font &quot;Menlo-16&quot;) (error (message &quot;Whoops!&quot;)))`, the condition case didn&#39;t catch it. I just get `Fatal error 11: Segmentation fault`. Thank you for pointing out to use `set-default-font` instead.")
              (comment_id . 505)
              (post_id . 363)
              (creation_date . 1411830743.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/160/jordon-biondo")
               (display_name . "Jordon Biondo")
               (profile_image . "http://i.stack.imgur.com/y27jq.png?s=128&g=1")
               (user_type . "registered")
               (user_id . 160)
               (reputation . 1487))
              (owner
               (link . "http://emacs.stackexchange.com/users/249/gsl")
               (display_name . "gsl")
               (profile_image . "https://www.gravatar.com/avatar/f56e324b45b74edb72eb1d230554f83f?s=128&d=identicon&r=PG")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 249)
               (reputation . 79)))
             ((body . "Looks like I typed the wrong face attribute it, ignore my comment.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment506_363")
              (body_markdown . "Looks like I typed the wrong face attribute it, ignore my comment.")
              (comment_id . 506)
              (post_id . 363)
              (creation_date . 1411831338.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/160/jordon-biondo")
               (display_name . "Jordon Biondo")
               (profile_image . "http://i.stack.imgur.com/y27jq.png?s=128&g=1")
               (user_type . "registered")
               (user_id . 160)
               (reputation . 1487)))
             ((body . "As @Malabarba indicated: <b>File a bug report</b> (immediately): <code>M-x report-emacs-bug</code>. Emacs developers will then lead you through what you can do to help debug the problem.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment508_363")
              (body_markdown . "As @Malabarba indicated: **File a bug report** (immediately): `M-x report-emacs-bug`. Emacs developers will then lead you through what you can do to help debug the problem.")
              (comment_id . 508)
              (post_id . 363)
              (creation_date . 1411833053.0)
              (post_type . "question")
              (score . 1)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/105/drew")
               (display_name . "Drew")
               (profile_image . "https://www.gravatar.com/avatar/32a8e553d85d193ee5ae1533ce6ec158?s=128&d=identicon&r=PG&f=1")
               (user_type . "registered")
               (user_id . 105)
               (reputation . 3478)))
             ((body . "@drew Thank you, I just did.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment509_363")
              (body_markdown . "@drew Thank you, I just did.")
              (comment_id . 509)
              (post_id . 363)
              (creation_date . 1411834454.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/105/drew")
               (display_name . "Drew")
               (profile_image . "https://www.gravatar.com/avatar/32a8e553d85d193ee5ae1533ce6ec158?s=128&d=identicon&r=PG&f=1")
               (user_type . "registered")
               (user_id . 105)
               (reputation . 3478))
              (owner
               (link . "http://emacs.stackexchange.com/users/249/gsl")
               (display_name . "gsl")
               (profile_image . "https://www.gravatar.com/avatar/f56e324b45b74edb72eb1d230554f83f?s=128&d=identicon&r=PG")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 249)
               (reputation . 79)))
             ((body . "Great. Here&#39;s wishing a fruitful followup and a fix. Thx.")
              (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs#comment510_363")
              (body_markdown . "Great. Here&#39;s wishing a fruitful followup and a fix. Thx.")
              (comment_id . 510)
              (post_id . 363)
              (creation_date . 1411834533.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/105/drew")
               (display_name . "Drew")
               (profile_image . "https://www.gravatar.com/avatar/32a8e553d85d193ee5ae1533ce6ec158?s=128&d=identicon&r=PG&f=1")
               (user_type . "registered")
               (user_id . 105)
               (reputation . 3478)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/249/gsl")
   (display_name . "gsl")
   (profile_image . "https://www.gravatar.com/avatar/f56e324b45b74edb72eb1d230554f83f?s=128&d=identicon&r=PG")
   (accept_rate . 100)
   (user_type . "registered")
   (user_id . 249)
   (reputation . 79))
  (tags .
        ["elisp" "fonts" "debugging" "crash"])
  (answers .
           [((body . "<h2>To help you track it next time</h2>\n\n<p>This happened to me before. There was a situation where\n<code>string-to-int</code> crashed Emacs, and it took me hours to pinpoint as\nwell.<br>\nSorry I can't provide a nicer answer, but Emacs crashes happen\ndeep down in the C code, and there aren't any built-in tools available\nto track down such problems.</p>\n\n<p>I suppose debugging it with <code>gdb</code> is possible, but its\neffectiveness will depend on your proficiency with <code>gdb</code>.</p>\n\n<p>What you really need to do is</p>\n\n<h2>File a bug report</h2>\n\n<p>Pure elisp code (non-byte-compiled) is never supposed to crash Emacs.\nIt could cause a hang (due to some infinite loop) and it could cause\nEmacs to run out of memory. But, beyond that, <strong>any crash is a bug</strong>.</p>\n\n<pre><code>M-x report-emacs-bug\n</code></pre>\n\n<p>Simply providing this minimally working example you came up with,\nalong with a description of your build and system, should be enough\nhelp for the kind developers.</p>\n")
             (title . "Better debugging of crashing Emacs?")
             (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs/373#373")
             (body_markdown . "## To help you track it next time ##\n\nThis happened to me before. There was a situation where\n`string-to-int` crashed Emacs, and it took me hours to pinpoint as\nwell.  \nSorry I can&#39;t provide a nicer answer, but Emacs crashes happen\ndeep down in the C code, and there aren&#39;t any built-in tools available\nto track down such problems.\n\nI suppose debugging it with `gdb` is possible, but its\neffectiveness will depend on your proficiency with `gdb`.\n\nWhat you really need to do is\n## File a bug report ##\n\nPure elisp code (non-byte-compiled) is never supposed to crash Emacs.\nIt could cause a hang (due to some infinite loop) and it could cause\nEmacs to run out of memory. But, beyond that, **any crash is a bug**.\n\n    M-x report-emacs-bug\n\nSimply providing this minimally working example you came up with,\nalong with a description of your build and system, should be enough\nhelp for the kind developers.\n\n")
             (question_id . 363)
             (answer_id . 373)
             (creation_date . 1411836111.0)
             (last_edit_date . 1411859526.0)
             (last_activity_date . 1411859526.0)
             (score . 4)
             (is_accepted . t)
             (comment_count . 1)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/11/legoscia")
              (display_name . "legoscia")
              (profile_image . "https://www.gravatar.com/avatar/e3d4212415bc280a21ad85d8022791fe?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 11)
              (reputation . 874))
             (comments .
                       [((body . "Using gdb would be the only way. We could add stuff to Emacs to wrap every C call in something that would self catch but that would be overhead all the time. Emacs is not supposed to crash, if it does we should specifically handle that bug so that it doesn&#39;t anymore. So yeah, if it&#39;s broken, file a bug. Use gdb by all means to find out absolutely where the bug is.")
                         (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs/373#comment514_373")
                         (body_markdown . "Using gdb would be the only way. We could add stuff to Emacs to wrap every C call in something that would self catch but that would be overhead all the time. Emacs is not supposed to crash, if it does we should specifically handle that bug so that it doesn&#39;t anymore. So yeah, if it&#39;s broken, file a bug. Use gdb by all means to find out absolutely where the bug is.")
                         (comment_id . 514)
                         (post_id . 373)
                         (creation_date . 1411843196.0)
                         (post_type . "answer")
                         (score . 2)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/362/nic-ferrier")
                          (display_name . "nic ferrier")
                          (profile_image . "https://www.gravatar.com/avatar/2ace292d4a9e6612e6cd761a1046f4f3?s=128&d=identicon&r=PG")
                          (user_type . "registered")
                          (user_id . 362)
                          (reputation . 101)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/50/malabarba")
              (display_name . "Malabarba")
              (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
              (accept_rate . 89)
              (user_type . "registered")
              (user_id . 50)
              (reputation . 3999)))
            ((body . "<p>As a reference to debug with gdb you'll want to use src/temacs from the build tree. This is Emacs without the pre-dumped elisp which confuses the debugger.</p>\n\n<pre><code>gdb --args src/temacs -nw\n</code></pre>\n")
             (title . "Better debugging of crashing Emacs?")
             (link . "http://emacs.stackexchange.com/questions/363/better-debugging-of-crashing-emacs/2905#2905")
             (body_markdown . "As a reference to debug with gdb you&#39;ll want to use src/temacs from the build tree. This is Emacs without the pre-dumped elisp which confuses the debugger.\n\n    gdb --args src/temacs -nw")
             (question_id . 363)
             (answer_id . 2905)
             (creation_date . 1414757838.0)
             (last_activity_date . 1414757838.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/129/stsquad")
              (display_name . "stsquad")
              (profile_image . "https://www.gravatar.com/avatar/2a1ad379159a4f4eaf5b54c5dbfe235a?s=128&d=identicon&r=PG")
              (accept_rate . 75)
              (user_type . "registered")
              (user_id . 129)
              (reputation . 428)))]))
 ((body . "<p>I'm trying to set upt <code>org-mode</code> and <code>reftex-mode</code> but I'm getting some troubles. Here is my <code>init.el</code> file:  </p>\n\n<pre><code>;; ORG-MODE\n(add-to-list 'auto-mode-alist '(\"\\\\.org\\\\'\" . org-mode))\n(setq org-agenda-files '(\"~/.../\"))\n(global-set-key \"\\C-ca\" 'org-agenda)\n(add-hook 'org-mode-hook\n      (lambda ()\n        (reftex-mode)\n        (visual-line-mode)\n        (company-mode)))\n</code></pre>\n\n<p>I'm trying to load a <code>.bib</code> file with <code>(setq reftex-default-bibliography '(\"path/to/bibfile.bib\"))</code> and it seems to work fine, but as I type <code>C-c [</code> in order to add a new reference I got this error:  </p>\n\n<p><code>reftex-using-biblatex-p: Stack overflow in regexp matcher</code>  </p>\n\n<p>Any hint? Thanks in advance.  </p>\n\n<p><em>P.S. couldn't tag with</em> <code>reftex-mode</code> <em>nor</em> <code>bibtex</code> <em>as I have no enough reputation score. If anyone could, I think thah would be good!</em></p>\n")
  (title . "org-mode and reftex giving this error: reftex-using-biblatex-p: Stack overflow in regexp matcher")
  (link . "http://emacs.stackexchange.com/questions/2901/org-mode-and-reftex-giving-this-error-reftex-using-biblatex-p-stack-overflow-i")
  (body_markdown . "I&#39;m trying to set upt `org-mode` and `reftex-mode` but I&#39;m getting some troubles. Here is my `init.el` file:  \n  \n\n    ;; ORG-MODE\n    (add-to-list &#39;auto-mode-alist &#39;(&quot;\\\\.org\\\\&#39;&quot; . org-mode))\n    (setq org-agenda-files &#39;(&quot;~/.../&quot;))\n    (global-set-key &quot;\\C-ca&quot; &#39;org-agenda)\n    (add-hook &#39;org-mode-hook\n    	  (lambda ()\n    	    (reftex-mode)\n    	    (visual-line-mode)\n    	    (company-mode)))\n  \n\nI&#39;m trying to load a `.bib` file with `(setq reftex-default-bibliography &#39;(&quot;path/to/bibfile.bib&quot;))` and it seems to work fine, but as I type `C-c [` in order to add a new reference I got this error:  \n  \n`reftex-using-biblatex-p: Stack overflow in regexp matcher`  \n  \nAny hint? Thanks in advance.  \n  \n*P.S. couldn&#39;t tag with* `reftex-mode` *nor* `bibtex` *as I have no enough reputation score. If anyone could, I think thah would be good!*")
  (question_id . 2901)
  (last_edit_date . 1414754843.0)
  (creation_date . 1414754222.0)
  (last_activity_date . 1414754843.0)
  (score . 0)
  (answer_count . 0)
  (favorite_count . 0)
  (view_count . 10)
  (is_answered . :json-false)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/10/wasamasa")
   (display_name . "wasamasa")
   (profile_image . "https://www.gravatar.com/avatar/1504319df63de7148c39290d4149f150?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 10)
   (reputation . 860))
  (owner
   (link . "http://emacs.stackexchange.com/users/201/petrux")
   (display_name . "petrux")
   (profile_image . "https://www.gravatar.com/avatar/18452c228ede1b8db735a6c310068947?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 201)
   (reputation . 41))
  (tags .
        ["org-mode" "reftex-mode" "bibtex"]))
 ((body . "<p>I'm on OSX 10.9.4 and I was previously running emacs 24.3 installed via brew.</p>\n\n<p>This morning I got around to asking brew to upgrade me to 24.4. Everything seemed to go smoothly, except csharp-mode no longer functions correctly. On opening a .cs file I get this error:</p>\n\n<pre><code>File mode specification error: (error \"Recursive load\" \n    \"/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc\" \n    \"/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc\" \n    \"/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc\" \n    \"/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc\" \n    \"/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc\")\n</code></pre>\n\n<p>I still have 24.3 on my machine and I can open .cs files correctly in that. </p>\n\n<p>Unfortunately csharp mode <a href=\"http://www.emacswiki.org/CSharpMode\" rel=\"nofollow\">seems to be dead</a> - it's not been touched in a few years. Any help or pointers as to how I can hack this to work would be appreciated.</p>\n")
  (title . "Recursive load error in csharp mode on upgrading to 24.4")
  (link . "http://emacs.stackexchange.com/questions/2899/recursive-load-error-in-csharp-mode-on-upgrading-to-24-4")
  (body_markdown . "I&#39;m on OSX 10.9.4 and I was previously running emacs 24.3 installed via brew.\n\nThis morning I got around to asking brew to upgrade me to 24.4. Everything seemed to go smoothly, except csharp-mode no longer functions correctly. On opening a .cs file I get this error:\n\n    File mode specification error: (error &quot;Recursive load&quot; \n        &quot;/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc&quot; \n        &quot;/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc&quot; \n        &quot;/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc&quot; \n        &quot;/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc&quot; \n        &quot;/Users/Andrew/.emacs.d/elpa/csharp-mode-20120204.1826/csharp-mode.elc&quot;)\n\nI still have 24.3 on my machine and I can open .cs files correctly in that. \n\nUnfortunately csharp mode [seems to be dead][1] - it&#39;s not been touched in a few years. Any help or pointers as to how I can hack this to work would be appreciated.\n\n\n  [1]: http://www.emacswiki.org/CSharpMode")
  (question_id . 2899)
  (creation_date . 1414750343.0)
  (last_activity_date . 1414751847.0)
  (score . 0)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 15)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 5)
  (comments .
            [((body . "Did you recompile csharp-mode.el after the update?")
              (link . "http://emacs.stackexchange.com/questions/2899/recursive-load-error-in-csharp-mode-on-upgrading-to-24-4#comment3952_2899")
              (body_markdown . "Did you recompile csharp-mode.el after the update?")
              (comment_id . 3952)
              (post_id . 2899)
              (creation_date . 1414750951.0)
              (post_type . "question")
              (score . 1)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/295/tom-regner")
               (display_name . "Tom Regner")
               (profile_image . "https://www.gravatar.com/avatar/57577324241ffa414154a4eb7589c3b6?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 295)
               (reputation . 145)))
             ((body . "I just upgraded then re-opened emacs. Deleting the .elc files for csharp in the elpa dir did fix the problem, but that&#39;s probably not the proper thing to do. What&#39;s the best way to recompile all packages?")
              (link . "http://emacs.stackexchange.com/questions/2899/recursive-load-error-in-csharp-mode-on-upgrading-to-24-4#comment3953_2899")
              (body_markdown . "I just upgraded then re-opened emacs. Deleting the .elc files for csharp in the elpa dir did fix the problem, but that&#39;s probably not the proper thing to do. What&#39;s the best way to recompile all packages?")
              (comment_id . 3953)
              (post_id . 2899)
              (creation_date . 1414751549.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/295/tom-regner")
               (display_name . "Tom Regner")
               (profile_image . "https://www.gravatar.com/avatar/57577324241ffa414154a4eb7589c3b6?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 295)
               (reputation . 145))
              (owner
               (link . "http://emacs.stackexchange.com/users/110/tenpn")
               (display_name . "tenpn")
               (profile_image . "https://www.gravatar.com/avatar/a31fbafa397845ab6ec33df5139ae589?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 110)
               (reputation . 131)))
             ((body . "ah: <a href=\"http://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs\" title=\"how to rebuild elpa packages after upgrade of emacs\">stackoverflow.com/questions/24725778/&hellip;</a>")
              (link . "http://emacs.stackexchange.com/questions/2899/recursive-load-error-in-csharp-mode-on-upgrading-to-24-4#comment3954_2899")
              (body_markdown . "ah: http://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs")
              (comment_id . 3954)
              (post_id . 2899)
              (creation_date . 1414751582.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/295/tom-regner")
               (display_name . "Tom Regner")
               (profile_image . "https://www.gravatar.com/avatar/57577324241ffa414154a4eb7589c3b6?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 295)
               (reputation . 145))
              (owner
               (link . "http://emacs.stackexchange.com/users/110/tenpn")
               (display_name . "tenpn")
               (profile_image . "https://www.gravatar.com/avatar/a31fbafa397845ab6ec33df5139ae589?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 110)
               (reputation . 131)))
             ((body . "<code>M-: (byte-recompile-directory package-user-dir nil &#39;force)</code>")
              (link . "http://emacs.stackexchange.com/questions/2899/recursive-load-error-in-csharp-mode-on-upgrading-to-24-4#comment3955_2899")
              (body_markdown . "`M-: (byte-recompile-directory package-user-dir nil &#39;force)`")
              (comment_id . 3955)
              (post_id . 2899)
              (creation_date . 1414751622.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/295/tom-regner")
               (display_name . "Tom Regner")
               (profile_image . "https://www.gravatar.com/avatar/57577324241ffa414154a4eb7589c3b6?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 295)
               (reputation . 145))
              (owner
               (link . "http://emacs.stackexchange.com/users/110/tenpn")
               (display_name . "tenpn")
               (profile_image . "https://www.gravatar.com/avatar/a31fbafa397845ab6ec33df5139ae589?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 110)
               (reputation . 131)))
             ((body . "Cool! Most problems I encounter after an update are due to old byte-code, I&#39;m glad that was your problem too.")
              (link . "http://emacs.stackexchange.com/questions/2899/recursive-load-error-in-csharp-mode-on-upgrading-to-24-4#comment3956_2899")
              (body_markdown . "Cool! Most problems I encounter after an update are due to old byte-code, I&#39;m glad that was your problem too.")
              (comment_id . 3956)
              (post_id . 2899)
              (creation_date . 1414751821.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/295/tom-regner")
               (display_name . "Tom Regner")
               (profile_image . "https://www.gravatar.com/avatar/57577324241ffa414154a4eb7589c3b6?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 295)
               (reputation . 145)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/110/tenpn")
   (display_name . "tenpn")
   (profile_image . "https://www.gravatar.com/avatar/a31fbafa397845ab6ec33df5139ae589?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 110)
   (reputation . 131))
  (tags .
        ["emacs24.4"])
  (answers .
           [((body . "<p>My problem was that the byte-compiled version of csharp-mode wasn't compatible with the upgrade. It seems it's a good idea to recompile all files on upgrading emacs, see this question: <a href=\"http://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs\">http://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs</a></p>\n\n<p>tl;dr: <code>M-: (byte-recompile-directory package-user-dir nil 'force)</code> fixes all problems.</p>\n")
             (title . "Recursive load error in csharp mode on upgrading to 24.4")
             (link . "http://emacs.stackexchange.com/questions/2899/recursive-load-error-in-csharp-mode-on-upgrading-to-24-4/2900#2900")
             (body_markdown . "My problem was that the byte-compiled version of csharp-mode wasn&#39;t compatible with the upgrade. It seems it&#39;s a good idea to recompile all files on upgrading emacs, see this question: http://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs\n\ntl;dr: `M-: (byte-recompile-directory package-user-dir nil &#39;force)` fixes all problems.")
             (question_id . 2899)
             (answer_id . 2900)
             (creation_date . 1414751847.0)
             (last_activity_date . 1414751847.0)
             (score . 3)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/110/tenpn")
              (display_name . "tenpn")
              (profile_image . "https://www.gravatar.com/avatar/a31fbafa397845ab6ec33df5139ae589?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 110)
              (reputation . 131)))]))
 ((body . "<p>In a recent <a href=\"http://emacs.stackexchange.com/questions/145/what-are-some-built-in-packages-with-nice-source-code/151#151\">answer</a> by <a href=\"http://emacs.stackexchange.com/users/227/lunaryorn\">lunaryorn</a>, he stated: </p>\n\n<blockquote>\n  <p>However, I'd recommend against most other parts of Org, for reasons\n  already stated in comments: It's old, and full of legacy and harmful\n  practices (e.g. find-file-noselect to read files non-interactively).</p>\n</blockquote>\n\n<p>Can anyone explain why is <code>find-file-noselect</code> a bad idea to read files in Elisp programs? Is there a better way? I'm asking because I was thinking of using it in one of my projects.</p>\n")
  (title . "What&#39;s wrong with `find-file-noselect`?")
  (link . "http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect")
  (body_markdown . "In a recent [answer][1] by [lunaryorn][2], he stated: \n\n&gt; However, I&#39;d recommend against most other parts of Org, for reasons\n&gt; already stated in comments: It&#39;s old, and full of legacy and harmful\n&gt; practices (e.g. find-file-noselect to read files non-interactively).\n\nCan anyone explain why is `find-file-noselect` a bad idea to read files in Elisp programs? Is there a better way? I&#39;m asking because I was thinking of using it in one of my projects.\n\n\n  [1]: http://emacs.stackexchange.com/questions/145/what-are-some-built-in-packages-with-nice-source-code/151#151\n  [2]: http://emacs.stackexchange.com/users/227/lunaryorn")
  (question_id . 2868)
  (last_edit_date . 1414705404.0)
  (creation_date . 1414702933.0)
  (last_activity_date . 1414749759.0)
  (score . 6)
  (answer_count . 2)
  (favorite_count . 1)
  (view_count . 56)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 4)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/45/king-shimkus")
   (display_name . "King Shimkus")
   (profile_image . "https://www.gravatar.com/avatar/073274c272b84eed27790fc3e5231d46?s=128&d=identicon&r=PG&f=1")
   (accept_rate . 93)
   (user_type . "registered")
   (user_id . 45)
   (reputation . 1217))
  (comments .
            [((body . "Seemingly, there was no <code>good-practices</code> tag before; is it a good idea to use it?")
              (link . "http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect#comment3920_2868")
              (body_markdown . "Seemingly, there was no `good-practices` tag before; is it a good idea to use it?")
              (comment_id . 3920)
              (post_id . 2868)
              (creation_date . 1414702978.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/642/mbork")
               (display_name . "mbork")
               (profile_image . "https://www.gravatar.com/avatar/d246baeefae20788d6ea519b777a99c7?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 642)
               (reputation . 240)))
             ((body . "I think that <code>good-practices</code> would fall under the category of &quot;meta tag,&quot; which is <a href=\"http://blog.stackoverflow.com/2010/08/the-death-of-meta-tags/\">frowned upon</a> by SE.")
              (link . "http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect#comment3927_2868")
              (body_markdown . "I think that `good-practices` would fall under the category of &quot;meta tag,&quot; which is [frowned upon](http://blog.stackoverflow.com/2010/08/the-death-of-meta-tags/) by SE.")
              (comment_id . 3927)
              (post_id . 2868)
              (creation_date . 1414706928.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/93/nispio")
               (display_name . "nispio")
               (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
               (accept_rate . 55)
               (user_type . "registered")
               (user_id . 93)
               (reputation . 1709)))
             ((body . "@nispio I think it&#39;s a valid tag, but we can take this to the meta of course.")
              (link . "http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect#comment3930_2868")
              (body_markdown . "@nispio I think it&#39;s a valid tag, but we can take this to the meta of course.")
              (comment_id . 3930)
              (post_id . 2868)
              (creation_date . 1414711034.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/93/nispio")
               (display_name . "nispio")
               (profile_image . "https://www.gravatar.com/avatar/52ada69db49a5d75240cc0c989c934f4?s=128&d=identicon&r=PG")
               (accept_rate . 55)
               (user_type . "registered")
               (user_id . 93)
               (reputation . 1709))
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "@nsipio: I&#39;ve skimmed through that article, and I don&#39;t agree.  But it&#39;s not me who decides. ;-)")
              (link . "http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect#comment3981_2868")
              (body_markdown . "@nsipio: I&#39;ve skimmed through that article, and I don&#39;t agree.  But it&#39;s not me who decides. ;-)")
              (comment_id . 3981)
              (post_id . 2868)
              (creation_date . 1414765383.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/642/mbork")
               (display_name . "mbork")
               (profile_image . "https://www.gravatar.com/avatar/d246baeefae20788d6ea519b777a99c7?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 642)
               (reputation . 240)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/642/mbork")
   (display_name . "mbork")
   (profile_image . "https://www.gravatar.com/avatar/d246baeefae20788d6ea519b777a99c7?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 642)
   (reputation . 240))
  (tags .
        ["files" "good-practices"])
  (answers .
           [((body . "<p>From section 24.3 in the Elisp manual:</p>\n\n<blockquote>\n  <p>To copy the contents of a file into a buffer, use the function\n  <code>insert-file-contents</code>.  (Don't use the command <code>insert-file</code> in a\n  Lisp program, as that sets the mark.)</p>\n</blockquote>\n\n<p>Searching the Elisp documentation for <code>find-file-noselect</code> it is obvious that it does much more than just reading a file into a buffer. Perhaps people who think using this function is a bad idea are thinking about the, possibly unwanted, side-effects? I guess it depends on what you want to achieve. If you want to have as clean/untouched buffer content as possible, it might be a good idea to use the old and trusty <code>with-temp-buffer</code> + <code>insert-file-contents</code>combination. If you want the buffer contents to be as close to what <code>find-file</code> produce, perhaps you <em>do</em> want to use <code>find-file-noselect</code>? Or perhaps he was thinking about <code>find-file</code> ;)</p>\n")
             (title . "What&#39;s wrong with `find-file-noselect`?")
             (link . "http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect/2878#2878")
             (body_markdown . "From section 24.3 in the Elisp manual:\n\n&gt; To copy the contents of a file into a buffer, use the function\n&gt; `insert-file-contents`.  (Don&#39;t use the command `insert-file` in a\n&gt; Lisp program, as that sets the mark.)\n\nSearching the Elisp documentation for `find-file-noselect` it is obvious that it does much more than just reading a file into a buffer. Perhaps people who think using this function is a bad idea are thinking about the, possibly unwanted, side-effects? I guess it depends on what you want to achieve. If you want to have as clean/untouched buffer content as possible, it might be a good idea to use the old and trusty `with-temp-buffer` + `insert-file-contents `combination. If you want the buffer contents to be as close to what `find-file` produce, perhaps you *do* want to use `find-file-noselect`? Or perhaps he was thinking about `find-file` ;)\n")
             (question_id . 2868)
             (answer_id . 2878)
             (creation_date . 1414710116.0)
             (last_activity_date . 1414710116.0)
             (score . 3)
             (is_accepted . :json-false)
             (comment_count . 1)
             (comments .
                       [((body . "If something is being done non-interactively I can&#39;t see any scenario in which you&#39;d want <i>&quot; the buffer contents to be close to what find-file would produce&quot;</i>. find-file is slow because it does a ton of unnecessary stuff, including all sorts of hooks. The only &quot;feature&quot; of find-file which you might want is the major-mode, but then you should just activate it yourself (you can&#39;t even guarantee find-file would turn on the mode that you want anyway).")
                         (link . "http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect/2878#comment3929_2878")
                         (body_markdown . "If something is being done non-interactively I can&#39;t see any scenario in which you&#39;d want *&quot; the buffer contents to be close to what&#160;find-file would produce&quot;*. find-file is slow because it does a ton of unnecessary stuff, including all sorts of hooks. The only &quot;feature&quot; of find-file which you might want is the major-mode, but then you should just activate it yourself (you can&#39;t even guarantee find-file would turn on the mode that you want anyway).")
                         (comment_id . 3929)
                         (post_id . 2878)
                         (creation_date . 1414710921.0)
                         (post_type . "answer")
                         (score . 2)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/50/malabarba")
                          (display_name . "Malabarba")
                          (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
                          (accept_rate . 89)
                          (user_type . "registered")
                          (user_id . 50)
                          (reputation . 3999)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/320/mathias-dahl")
              (display_name . "Mathias Dahl")
              (profile_image . "https://www.gravatar.com/avatar/ab1c0a3172eecaa6ad0a44b6c84bc723?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 320)
              (reputation . 119)))
            ((body . "<p><strong>TL;DR</strong>: With <code>find-file-noselect</code> you have <strong>no control</strong> about what actually happens, and you may end up with arbitrary minor modes enabling in the buffer, depending on what the user enabled in their <code>init.el</code>.  Also, cleanup is hard.  </p>\n\n<p>Use <code>with-temp-buffer</code> and <code>insert-file-contents</code> instead.  If you need specific major or minor modes in the buffer, enable them <strong>explicitly</strong>.  To write files, use <code>with-temp-file</code> instead, which—despite its name—lets you write to arbitrary files.</p>\n\n<h1>Side effects</h1>\n\n<p><code>find-file-noselect</code> has <strong>a lot</strong> of side-effects, including</p>\n\n<ul>\n<li>interactively asking questions (that alone is a no-go in non-interactive use),</li>\n<li>automatically enabling view mode for readonly files,</li>\n<li>entering normal mode otherwise,</li>\n<li>and running <code>find-file-hook</code>.</li>\n</ul>\n\n<p>Normal Mode itself </p>\n\n<ul>\n<li>automatically selects a proper major mode for the current buffer,</li>\n<li>runs all corresponding major and minor mode hooks,</li>\n<li>and reads all local variables for the current buffer, i.e. file variables and directory variables, which again may ask interactive questions about unsafe local variables.</li>\n</ul>\n\n<p>Since all hooks are run, you get <strong>all minor modes and hook functions</strong> the user enabled in their <code>init.el</code>, which can cause everything, from minor inconveniences (if undesirable minor modes are enabled) to major havoc (if the user added a hook function that expects to be called from an interactive context).  </p>\n\n<p>See <a href=\"https://github.com/flycheck/flycheck/issues/366\">https://github.com/flycheck/flycheck/issues/366</a> for an example.  The use of <code>find-file-noselect</code> caused a data file to be syntax-checked by Flycheck, and since it was happening at Emacs shut-down, there was no time to properly clean up again, leaving a temporary file behind.</p>\n\n<h1>Cleanup</h1>\n\n<p>With <code>find-file-noselect</code> you need to be extra careful to kill the buffer again.  <code>find-file-noselect</code> does not do that for you.</p>\n\n<p>You need to remember the buffer at some place, and carefully use <code>unwind-protect</code> to make sure that the buffer gets killed even in case of non-local exits.</p>\n\n<h1>Alternatives</h1>\n\n<p>To read files, use <code>with-temp-buffer</code> and <code>insert-file-contents</code>, which only does the most basic things, e.g. coding system conversion, but does not ask questions, enable hooks, or setup local variables:</p>\n\n<pre><code>(with-temp-buffer\n  (insert-file-contents (locate-user-emacs-file \"foo.el\"))\n  ;; Enter the major mode explicitly\n  (emacs-lisp-mode)\n  ;; …\n  )\n</code></pre>\n\n<p><code>with-temp-buffer</code> takes care to properly kill the temporary buffer at the end of its body.</p>\n\n<p>To write files, use <code>with-temp-file</code>, which creates a temporary buffer and writes the contents to the given file name at the end of its body:</p>\n\n<pre><code>(with-temp-file  (locate-user-emacs-file \"foo.el\")\n  (prin1 (list 'my 'data) (current-buffer)))\n</code></pre>\n")
             (title . "What&#39;s wrong with `find-file-noselect`?")
             (link . "http://emacs.stackexchange.com/questions/2868/whats-wrong-with-find-file-noselect/2898#2898")
             (body_markdown . "**TL;DR**: With `find-file-noselect` you have **no control** about what actually happens, and you may end up with arbitrary minor modes enabling in the buffer, depending on what the user enabled in their `init.el`.  Also, cleanup is hard.  \n\nUse `with-temp-buffer` and `insert-file-contents` instead.  If you need specific major or minor modes in the buffer, enable them **explicitly**.  To write files, use `with-temp-file` instead, which—despite its name—lets you write to arbitrary files.\n\n# Side effects\n\n`find-file-noselect` has **a lot** of side-effects, including\n\n- interactively asking questions (that alone is a no-go in non-interactive use),\n- automatically enabling view mode for readonly files,\n- entering normal mode otherwise,\n- and running `find-file-hook`.\n\nNormal Mode itself \n\n- automatically selects a proper major mode for the current buffer,\n- runs all corresponding major and minor mode hooks,\n- and reads all local variables for the current buffer, i.e. file variables and directory variables, which again may ask interactive questions about unsafe local variables.\n\nSince all hooks are run, you get **all minor modes and hook functions** the user enabled in their `init.el`, which can cause everything, from minor inconveniences (if undesirable minor modes are enabled) to major havoc (if the user added a hook function that expects to be called from an interactive context).  \n\nSee https://github.com/flycheck/flycheck/issues/366 for an example.  The use of `find-file-noselect` caused a data file to be syntax-checked by Flycheck, and since it was happening at Emacs shut-down, there was no time to properly clean up again, leaving a temporary file behind.\n\n# Cleanup\n\nWith `find-file-noselect` you need to be extra careful to kill the buffer again.  `find-file-noselect` does not do that for you.\n\nYou need to remember the buffer at some place, and carefully use `unwind-protect` to make sure that the buffer gets killed even in case of non-local exits.\n\n# Alternatives\n\nTo read files, use `with-temp-buffer` and `insert-file-contents`, which only does the most basic things, e.g. coding system conversion, but does not ask questions, enable hooks, or setup local variables:\n\n    (with-temp-buffer\n      (insert-file-contents (locate-user-emacs-file &quot;foo.el&quot;))\n      ;; Enter the major mode explicitly\n      (emacs-lisp-mode)\n      ;; …\n      )\n\n`with-temp-buffer` takes care to properly kill the temporary buffer at the end of its body.\n\nTo write files, use `with-temp-file`, which creates a temporary buffer and writes the contents to the given file name at the end of its body:\n\n    (with-temp-file  (locate-user-emacs-file &quot;foo.el&quot;)\n      (prin1 (list &#39;my &#39;data) (current-buffer)))")
             (question_id . 2868)
             (answer_id . 2898)
             (creation_date . 1414749759.0)
             (last_activity_date . 1414749759.0)
             (score . 5)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/227/lunaryorn")
              (display_name . "lunaryorn")
              (profile_image . "http://i.stack.imgur.com/Ltiwu.png?s=128&g=1")
              (user_type . "registered")
              (user_id . 227)
              (reputation . 1643)))]))
 ((body . "<p>This is admittedly a superficial question.</p>\n\n<p>Although <code>ansi-term</code> starts out behaving correctly when I first open it, it seems to degrade into an 'editable' mode after a few commands, e.g., when I do <code>C-a</code> (start of line) followed by <code>C-k</code> (kill line), it wipes out the whole prompt.</p>\n\n<p>For my aesthetic sanity, is there any way to make the prompts and previous output uneditable (for <code>shell</code> and <code>ansi-term</code>)?  </p>\n\n<p>I'm using zsh in Emacs 24.4; OS is LXDE (Ubuntu 14.04) via Virtual Box.</p>\n\n<p>(Ideally I'd still be able to move my cursor around...)</p>\n")
  (title . "Any way to make prompts and previous-output uneditable in shell/term-mode?")
  (link . "http://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode")
  (body_markdown . "This is admittedly a superficial question.\n\nAlthough `ansi-term` starts out behaving correctly when I first open it, it seems to degrade into an &#39;editable&#39; mode after a few commands, e.g., when I do `C-a` (start of line) followed by `C-k` (kill line), it wipes out the whole prompt.\n\nFor my aesthetic sanity, is there any way to make the prompts and previous output uneditable (for `shell` and `ansi-term`)?  \n\nI&#39;m using zsh in Emacs 24.4; OS is LXDE (Ubuntu 14.04) via Virtual Box.\n\n(Ideally I&#39;d still be able to move my cursor around...)")
  (question_id . 2883)
  (last_edit_date . 1414722296.0)
  (creation_date . 1414720518.0)
  (last_activity_date . 1414748894.0)
  (score . 1)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 30)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 4)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/2084/dipak-c")
   (display_name . "Dipak C")
   (profile_image . "https://www.gravatar.com/avatar/ab1a56787ee3fc3602592a1909cbd0ea?s=128&d=identicon&r=PG&f=1")
   (accept_rate . 100)
   (user_type . "registered")
   (user_id . 2084)
   (reputation . 156))
  (comments .
            [((body . "Are you on an &quot;odd&quot; OS by any chance? Usually in shell-mode C-a jumps to the start of the commandline, <i>after</i> the prompt, and a second C-a is required to jump to the very start. On Windows and Solaris (in a former life) I usually found that C-a jumped to the very start, wheras on MacOS, Linux etc. it works as expected for me.")
              (link . "http://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode#comment3939_2883")
              (body_markdown . "Are you on an &quot;odd&quot; OS by any chance? Usually in shell-mode C-a jumps to the start of the commandline, *after* the prompt, and a second C-a is required to jump to the very start. On Windows and Solaris (in a former life) I usually found that C-a jumped to the very start, wheras on MacOS, Linux etc. it works as expected for me.")
              (comment_id . 3939)
              (post_id . 2883)
              (creation_date . 1414721113.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/215/mark-aufflick")
               (display_name . "Mark Aufflick")
               (profile_image . "http://i.stack.imgur.com/y7fBO.png?s=128&g=1")
               (user_type . "registered")
               (user_id . 215)
               (reputation . 193)))
             ((body . "@Mark A: I&#39;ve updated the original post to include a few more details, including my current set-up.")
              (link . "http://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode#comment3941_2883")
              (body_markdown . "@Mark A: I&#39;ve updated the original post to include a few more details, including my current set-up.")
              (comment_id . 3941)
              (post_id . 2883)
              (creation_date . 1414722327.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/215/mark-aufflick")
               (display_name . "Mark Aufflick")
               (profile_image . "http://i.stack.imgur.com/y7fBO.png?s=128&g=1")
               (user_type . "registered")
               (user_id . 215)
               (reputation . 193))
              (owner
               (link . "http://emacs.stackexchange.com/users/2084/dipak-c")
               (display_name . "Dipak C")
               (profile_image . "https://www.gravatar.com/avatar/ab1a56787ee3fc3602592a1909cbd0ea?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 2084)
               (reputation . 156)))
             ((body . "The described behaviour of ansi-term sounds like you&#39;ve switched to line mode by either a weird default or <code>C-c C-j</code>. Try finding out that default or switch to char mode with <code>C-c C-k</code>.")
              (link . "http://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode#comment3949_2883")
              (body_markdown . "The described behaviour of ansi-term sounds like you&#39;ve switched to line mode by either a weird default or `C-c C-j`. Try finding out that default or switch to char mode with `C-c C-k`.")
              (comment_id . 3949)
              (post_id . 2883)
              (creation_date . 1414748349.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/10/wasamasa")
               (display_name . "wasamasa")
               (profile_image . "https://www.gravatar.com/avatar/1504319df63de7148c39290d4149f150?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 10)
               (reputation . 860)))
             ((body . "Since <code>shell-mode</code> / <code>term-mode</code> respectively do / do not derive from <code>comint-mode</code>, you&#39;re asking two very different questions at the same time here.  Mind splitting this into two separate questions?")
              (link . "http://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode#comment3961_2883")
              (body_markdown . "Since `shell-mode` / `term-mode` respectively do / do not derive from `comint-mode`, you&#39;re asking two very different questions at the same time here.  Mind splitting this into two separate questions?")
              (comment_id . 3961)
              (post_id . 2883)
              (creation_date . 1414759847.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/372/purple-arrows")
               (display_name . "purple_arrows")
               (profile_image . "https://www.gravatar.com/avatar/cb378ba79d85a04a9277ddc0e4259149?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 372)
               (reputation . 472)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2084/dipak-c")
   (display_name . "Dipak C")
   (profile_image . "https://www.gravatar.com/avatar/ab1a56787ee3fc3602592a1909cbd0ea?s=128&d=identicon&r=PG&f=1")
   (accept_rate . 100)
   (user_type . "registered")
   (user_id . 2084)
   (reputation . 156))
  (tags .
        ["shell" "ansi-term"])
  (answers .
           [((body . "<p><a href=\"http://www.emacswiki.org/emacs/ComintMode\" rel=\"nofollow\">comint</a>-derived modes (like <code>shell</code>, <code>ielm</code>, ...) support filter functions, the following snippet makes the output and prompt read-only:</p>\n\n<pre><code>(defun my-comint-preoutput-turn-buffer-read-only (text)\n  (propertize text 'read-only t))\n\n(add-hook 'comint-preoutput-filter-functions 'my-comint-preoutput-turn-buffer-read-only)\n</code></pre>\n\n<p>As for <code>ansi-term</code> (which is not derived from <a href=\"http://www.emacswiki.org/emacs/ComintMode\" rel=\"nofollow\">comint</a>), it features two modes of operation, <code>char mode</code> which stays as truthful to the bindings of the shell and the software running in it as possible and <code>line mode</code> which allows you to edit the buffer as you please. You should generally stay in <code>char mode</code> (bound to <code>C-c C-k</code>) and occasionally switch to <code>line mode</code> (bound to <code>C-c C-j</code>) for editing command output.</p>\n")
             (title . "Any way to make prompts and previous-output uneditable in shell/term-mode?")
             (link . "http://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode/2897#2897")
             (body_markdown . "[comint](http://www.emacswiki.org/emacs/ComintMode)-derived modes (like `shell`, `ielm`, ...) support filter functions, the following snippet makes the output and prompt read-only:\n\n    (defun my-comint-preoutput-turn-buffer-read-only (text)\n      (propertize text &#39;read-only t))\n\n    (add-hook &#39;comint-preoutput-filter-functions &#39;my-comint-preoutput-turn-buffer-read-only)\n\nAs for `ansi-term` (which is not derived from [comint](http://www.emacswiki.org/emacs/ComintMode)), it features two modes of operation, `char mode` which stays as truthful to the bindings of the shell and the software running in it as possible and `line mode` which allows you to edit the buffer as you please. You should generally stay in `char mode` (bound to `C-c C-k`) and occasionally switch to `line mode` (bound to `C-c C-j`) for editing command output.")
             (question_id . 2883)
             (answer_id . 2897)
             (creation_date . 1414748894.0)
             (last_activity_date . 1414748894.0)
             (score . 4)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/10/wasamasa")
              (display_name . "wasamasa")
              (profile_image . "https://www.gravatar.com/avatar/1504319df63de7148c39290d4149f150?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 10)
              (reputation . 860)))]))
 ((body . "<p>As an Emacs newbie, I found IDO and loved it since it made searching for files so much quicker. After spending some time on this site, I've read more and more about Helm and I'm planning on making the switch. Some of my questions are: </p>\n\n<ol>\n<li>What are the biggest differences? </li>\n<li>Specifically, how should my workflow change when finding files, switching buffers, or calling new commands?</li>\n</ol>\n\n<p>I used <a href=\"http://tuhdo.github.io/helm-intro.html\">this post</a> to set up Helm, but my file searches (<code>C-x C-f</code>) and buffer switches (<code>C-x b</code>) look pretty much the same as they did before.</p>\n\n<p>Here is my config:</p>\n\n<pre><code>(require 'helm)\n(require 'helm-config)\n\n;; The default \"C-x c\" is quite close to \"C-x C-c\", which quits Emacs.\n;; Changed to \"C-c h\". Note: We must set \"C-c h\" globally, because we\n;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.\n(global-set-key (kbd \"C-c h\") 'helm-command-prefix)\n(global-unset-key (kbd \"C-x c\"))\n\n(define-key helm-map (kbd \"&lt;tab&gt;\") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action\n(define-key helm-map (kbd \"C-i\") 'helm-execute-persistent-action) ; make TAB works in terminal\n(define-key helm-map (kbd \"C-z\")  'helm-select-action) ; list actions using C-z\n\n(when (executable-find \"curl\")\n  (setq helm-google-suggest-use-curl-p t))\n\n(setq helm-quick-update                     t ; do not display invisible candidates\n      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window\n      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil\n      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.\n      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.\n      helm-scroll-amount                    8 ; scroll 8 lines other window using M-&lt;next&gt;/M-&lt;prior&gt;\n      helm-ff-file-name-history-use-recentf t)\n\n(helm-mode 1)\n</code></pre>\n")
  (title . "How should I change my workflow when moving from IDO to Helm")
  (link . "http://emacs.stackexchange.com/questions/2867/how-should-i-change-my-workflow-when-moving-from-ido-to-helm")
  (body_markdown . "As an Emacs newbie, I found IDO and loved it since it made searching for files so much quicker. After spending some time on this site, I&#39;ve read more and more about Helm and I&#39;m planning on making the switch. Some of my questions are: \n\n 1. What are the biggest differences? \n 2. Specifically, how should my workflow change when finding files, switching buffers, or calling new commands?\n\nI used [this post](http://tuhdo.github.io/helm-intro.html) to set up Helm, but my file searches (`C-x C-f`) and buffer switches (`C-x b`) look pretty much the same as they did before.\n\nHere is my config:\n\n    (require &#39;helm)\n    (require &#39;helm-config)\n    \n    ;; The default &quot;C-x c&quot; is quite close to &quot;C-x C-c&quot;, which quits Emacs.\n    ;; Changed to &quot;C-c h&quot;. Note: We must set &quot;C-c h&quot; globally, because we\n    ;; cannot change `helm-command-prefix-key&#39; once `helm-config&#39; is loaded.\n    (global-set-key (kbd &quot;C-c h&quot;) &#39;helm-command-prefix)\n    (global-unset-key (kbd &quot;C-x c&quot;))\n    \n    (define-key helm-map (kbd &quot;&lt;tab&gt;&quot;) &#39;helm-execute-persistent-action) ; rebihnd tab to do persistent action\n    (define-key helm-map (kbd &quot;C-i&quot;) &#39;helm-execute-persistent-action) ; make TAB works in terminal\n    (define-key helm-map (kbd &quot;C-z&quot;)  &#39;helm-select-action) ; list actions using C-z\n    \n    (when (executable-find &quot;curl&quot;)\n      (setq helm-google-suggest-use-curl-p t))\n    \n    (setq helm-quick-update                     t ; do not display invisible candidates\n          helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window\n          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil\n          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.\n          helm-ff-search-library-in-sexp        t ; search for library in `require&#39; and `declare-function&#39; sexp.\n          helm-scroll-amount                    8 ; scroll 8 lines other window using M-&lt;next&gt;/M-&lt;prior&gt;\n          helm-ff-file-name-history-use-recentf t)\n    \n    (helm-mode 1)")
  (question_id . 2867)
  (last_edit_date . 1414706239.0)
  (creation_date . 1414701322.0)
  (last_activity_date . 1414747262.0)
  (score . 6)
  (answer_count . 1)
  (accepted_answer_id . 2880)
  (favorite_count . 2)
  (view_count . 89)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 1)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/45/king-shimkus")
   (display_name . "King Shimkus")
   (profile_image . "https://www.gravatar.com/avatar/073274c272b84eed27790fc3e5231d46?s=128&d=identicon&r=PG&f=1")
   (accept_rate . 93)
   (user_type . "registered")
   (user_id . 45)
   (reputation . 1217))
  (comments .
            [((body . "As you already realized, you forgot to bind Helm specific commands to replace the stock Emacs commands. If you keep reading the guides for each specific Helm command, you will see that I put key bindings (if possible) and setup in each section. Nevertheless, enjoy Helm :)")
              (link . "http://emacs.stackexchange.com/questions/2867/how-should-i-change-my-workflow-when-moving-from-ido-to-helm#comment3950_2867")
              (body_markdown . "As you already realized, you forgot to bind Helm specific commands to replace the stock Emacs commands. If you keep reading the guides for each specific Helm command, you will see that I put key bindings (if possible) and setup in each section. Nevertheless, enjoy Helm :)")
              (comment_id . 3950)
              (post_id . 2867)
              (creation_date . 1414748959.0)
              (post_type . "question")
              (score . 1)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/35/ryan")
   (display_name . "Ryan")
   (profile_image . "https://www.gravatar.com/avatar/18b80f3021a241d9b85c236dafbcd64b?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 35)
   (reputation . 255))
  (tags .
        ["helm" "ido"])
  (answers .
           [((body . "<p>I switched from IDO to Helm few years ago and I never looked back.</p>\n\n<ul>\n<li>I find the presentation cleaner than let's say ido-vertical-mode for example. </li>\n<li>Helm doesn't have flex matching though.</li>\n<li>You don't need smex, etc. Helm does it all.</li>\n<li>The Tu do article is good as it provides some nice animated screenshots of what Helm can do.</li>\n</ul>\n\n<p>I use Helm projectile, Helm swoop, Helm semantic, Helm ag and some of the Helm interactive commands. Checkout the <a href=\"http://melpa.org/#/\" rel=\"nofollow\">Melpa page</a> for Helm related packages that might interest you.</p>\n\n<p>Here's some of my Helm related setup :</p>\n\n<pre><code>(setq helm-ff-transformer-show-only-basename nil\n      helm-adaptive-history-file             ers-helm-adaptive-history-file\n      helm-boring-file-regexp-list           '(\"\\\\.git$\" \"\\\\.svn$\" \"\\\\.elc$\")\n      helm-yank-symbol-first                 t\n      helm-buffers-fuzzy-matching            t\n      helm-ff-auto-update-initial-value      t\n      helm-input-idle-delay                  0.1\n      helm-idle-delay                        0.1)\n\n(autoload 'helm-descbinds      \"helm-descbinds\" t)\n(autoload 'helm-eshell-history \"helm-eshell\"    t)\n(autoload 'helm-esh-pcomplete  \"helm-eshell\"    t)\n\n(global-set-key (kbd \"C-h a\")    #'helm-apropos)\n(global-set-key (kbd \"C-h i\")    #'helm-info-emacs)\n(global-set-key (kbd \"C-h b\")    #'helm-descbinds)\n\n(add-hook 'eshell-mode-hook\n          #'(lambda ()\n              (define-key eshell-mode-map (kbd \"TAB\")     #'helm-esh-pcomplete)\n              (define-key eshell-mode-map (kbd \"C-c C-l\") #'helm-eshell-history)))\n\n(global-set-key (kbd \"C-x b\")   #'helm-mini)\n(global-set-key (kbd \"C-x C-b\") #'helm-buffers-list)\n(global-set-key (kbd \"C-x C-m\") #'helm-M-x)\n(global-set-key (kbd \"C-x C-f\") #'helm-find-files)\n(global-set-key (kbd \"C-x C-r\") #'helm-recentf)\n(global-set-key (kbd \"C-x r l\") #'helm-filtered-bookmarks)\n(global-set-key (kbd \"M-y\")     #'helm-show-kill-ring)\n(global-set-key (kbd \"M-s o\")   #'helm-swoop)\n(global-set-key (kbd \"M-s /\")   #'helm-multi-swoop)\n\n(require 'helm-config)\n(helm-mode t)\n(helm-adaptative-mode t)\n\n(global-set-key (kbd \"C-x c!\")   #'helm-calcul-expression)\n(global-set-key (kbd \"C-x c:\")   #'helm-eval-expression-with-eldoc)\n(define-key helm-map (kbd \"M-o\") #'helm-previous-source)\n\n(global-set-key (kbd \"M-s s\")   #'helm-ag)\n\n(define-key projectile-mode-map (kbd \"C-c p g\") \n                #'(lambda ()                                                                     \n                      (interactive)                                                                \n                      (helm-ag (projectile-project-root))))  \n\n(define-key org-mode-map (kbd \"C-x c o h\") #'helm-org-headlines)     \n</code></pre>\n")
             (title . "How should I change my workflow when moving from IDO to Helm")
             (link . "http://emacs.stackexchange.com/questions/2867/how-should-i-change-my-workflow-when-moving-from-ido-to-helm/2880#2880")
             (body_markdown . "I switched from IDO to Helm few years ago and I never looked back.\n\n- I find the presentation cleaner than let&#39;s say ido-vertical-mode for example. \n- Helm doesn&#39;t have flex matching though.\n- You don&#39;t need smex, etc. Helm does it all.\n- The Tu do article is good as it provides some nice animated screenshots of what Helm can do.\n\nI use Helm projectile, Helm swoop, Helm semantic, Helm ag and some of the Helm interactive commands. Checkout the [Melpa page][1] for Helm related packages that might interest you.\n\nHere&#39;s some of my Helm related setup :\n\n \n \n    (setq helm-ff-transformer-show-only-basename nil\n          helm-adaptive-history-file             ers-helm-adaptive-history-file\n          helm-boring-file-regexp-list           &#39;(&quot;\\\\.git$&quot; &quot;\\\\.svn$&quot; &quot;\\\\.elc$&quot;)\n          helm-yank-symbol-first                 t\n          helm-buffers-fuzzy-matching            t\n          helm-ff-auto-update-initial-value      t\n          helm-input-idle-delay                  0.1\n          helm-idle-delay                        0.1)\n\n    (autoload &#39;helm-descbinds      &quot;helm-descbinds&quot; t)\n    (autoload &#39;helm-eshell-history &quot;helm-eshell&quot;    t)\n    (autoload &#39;helm-esh-pcomplete  &quot;helm-eshell&quot;    t)\n\n    (global-set-key (kbd &quot;C-h a&quot;)    #&#39;helm-apropos)\n    (global-set-key (kbd &quot;C-h i&quot;)    #&#39;helm-info-emacs)\n    (global-set-key (kbd &quot;C-h b&quot;)    #&#39;helm-descbinds)\n\n    (add-hook &#39;eshell-mode-hook\n              #&#39;(lambda ()\n                  (define-key eshell-mode-map (kbd &quot;TAB&quot;)     #&#39;helm-esh-pcomplete)\n                  (define-key eshell-mode-map (kbd &quot;C-c C-l&quot;) #&#39;helm-eshell-history)))\n\n    (global-set-key (kbd &quot;C-x b&quot;)   #&#39;helm-mini)\n    (global-set-key (kbd &quot;C-x C-b&quot;) #&#39;helm-buffers-list)\n    (global-set-key (kbd &quot;C-x C-m&quot;) #&#39;helm-M-x)\n    (global-set-key (kbd &quot;C-x C-f&quot;) #&#39;helm-find-files)\n    (global-set-key (kbd &quot;C-x C-r&quot;) #&#39;helm-recentf)\n    (global-set-key (kbd &quot;C-x r l&quot;) #&#39;helm-filtered-bookmarks)\n    (global-set-key (kbd &quot;M-y&quot;)     #&#39;helm-show-kill-ring)\n    (global-set-key (kbd &quot;M-s o&quot;)   #&#39;helm-swoop)\n    (global-set-key (kbd &quot;M-s /&quot;)   #&#39;helm-multi-swoop)\n\n    (require &#39;helm-config)\n    (helm-mode t)\n    (helm-adaptative-mode t)\n\n    (global-set-key (kbd &quot;C-x c!&quot;)   #&#39;helm-calcul-expression)\n    (global-set-key (kbd &quot;C-x c:&quot;)   #&#39;helm-eval-expression-with-eldoc)\n    (define-key helm-map (kbd &quot;M-o&quot;) #&#39;helm-previous-source)\n\n    (global-set-key (kbd &quot;M-s s&quot;)   #&#39;helm-ag)\n\n    (define-key projectile-mode-map (kbd &quot;C-c p g&quot;) \n                    #&#39;(lambda ()                                                                     \n                          (interactive)                                                                \n                          (helm-ag (projectile-project-root))))  \n\n    (define-key org-mode-map (kbd &quot;C-x c o h&quot;) #&#39;helm-org-headlines)     \n\n\n  [1]: http://melpa.org/#/")
             (question_id . 2867)
             (answer_id . 2880)
             (creation_date . 1414710893.0)
             (last_edit_date . 1414747262.0)
             (last_activity_date . 1414747262.0)
             (score . 4)
             (is_accepted . t)
             (comment_count . 1)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/943/rimero")
              (display_name . "rimero")
              (profile_image . "https://www.gravatar.com/avatar/0dcea030535e386eee5821f278d74e3e?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 943)
              (reputation . 181))
             (comments .
                       [((body . "@Ryan Notice that the default <code>helm-boring-file-regexp-list</code> contains much more than presented here. If you use remiro&#39;s setup, notice this point. Probably this is his preference. <code>helm-input-idle-delay</code> and <code>helm-idle-delay default</code> were 0.01 months ago, to make the fastest possible response. Overall, the setup is fine.")
                         (link . "http://emacs.stackexchange.com/questions/2867/how-should-i-change-my-workflow-when-moving-from-ido-to-helm/2880#comment3951_2880")
                         (body_markdown . "@Ryan Notice that the default `helm-boring-file-regexp-list` contains much more than presented here. If you use remiro&#39;s setup, notice this point. Probably this is his preference. `helm-input-idle-delay` and `helm-idle-delay default` were 0.01 months ago, to make the fastest possible response. Overall, the setup is fine.")
                         (comment_id . 3951)
                         (post_id . 2880)
                         (creation_date . 1414749291.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/200/tu-do")
                          (display_name . "Tu Do")
                          (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
                          (accept_rate . 75)
                          (user_type . "registered")
                          (user_id . 200)
                          (reputation . 652)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/943/rimero")
              (display_name . "rimero")
              (profile_image . "https://www.gravatar.com/avatar/0dcea030535e386eee5821f278d74e3e?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 943)
              (reputation . 181)))]))
 ((body . "<p>I got </p>\n\n<pre><code>(setq-default fill-column 80)\n(setq-default truncate-lines nil) ;; which is default\n</code></pre>\n\n<p>When using magit in Emacs, I know that I should avoid long commit messages, but sometimes they just cannot be avoid. </p>\n\n<p>The problem is: <code>(setq-default truncate-lines nil)</code> which is default setting does not work in <strong>magit: project</strong> buffer(there is a straight right arrow at the end of line) but work in <strong>COMMIT_EDITMSG</strong> buffer, the <code>(setq-default fill-column 80)</code> does not work in <strong>COMMIT_EDITMSG</strong> buffer, the <code>fill-column</code> value in it is 72 not 80.</p>\n\n<p>But what I really want is: make the <code>truncate-lines</code> to <code>nil</code> in <strong>magit: project</strong> buffer too so I can see long lines in one window but not break it into several lines, <strong>AND</strong> turn all auto-fill-mode in <strong>COMMIT_EDITMSG</strong> buffer, so long commit messages won't be broken into several lines when I'm typing.</p>\n\n<p>NOTE: DO NOT affect other buffers like <strong>magit-log</strong> buffer</p>\n")
  (title . "How to make `truncate-lines` nil and `auto-fill-mode` off in magit buffers")
  (link . "http://emacs.stackexchange.com/questions/2890/how-to-make-truncate-lines-nil-and-auto-fill-mode-off-in-magit-buffers")
  (body_markdown . "I got \n\n    (setq-default fill-column 80)\n    (setq-default truncate-lines nil) ;; which is default\n\n\nWhen using magit in Emacs, I know that I should avoid long commit messages, but sometimes they just cannot be avoid. \n\nThe problem is: `(setq-default truncate-lines nil)` which is default setting does not work in **magit: project** buffer(there is a straight right arrow at the end of line) but work in **COMMIT_EDITMSG** buffer, the `(setq-default fill-column 80)` does not work in **COMMIT_EDITMSG** buffer, the `fill-column` value in it is 72 not 80.\n\nBut what I really want is: make the `truncate-lines` to `nil` in **magit: project** buffer too so I can see long lines in one window but not break it into several lines, **AND** turn all auto-fill-mode in **COMMIT_EDITMSG** buffer, so long commit messages won&#39;t be broken into several lines when I&#39;m typing.\n\nNOTE: DO NOT affect other buffers like **magit-log** buffer")
  (question_id . 2890)
  (creation_date . 1414730361.0)
  (last_activity_date . 1414744957.0)
  (score . 0)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 16)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 0)
  (owner
   (link . "http://emacs.stackexchange.com/users/794/codychan")
   (display_name . "CodyChan")
   (profile_image . "https://www.gravatar.com/avatar/9e0131ea359bc35a9902fc03d8ac7155?s=128&d=identicon&r=PG&f=1")
   (user_type . "registered")
   (user_id . 794)
   (reputation . 123))
  (tags .
        ["magit"])
  (answers .
           [((body . "<p>You can add setting specific to <code>magit-status</code> to <code>magit-status-mode-hook</code>:</p>\n\n<pre><code>(defun turn-on-truncate-lines ()\n   (setq truncate-lines t))\n\n(add-hook 'magit-status-mode-hook 'turn-on-truncate-lines)\n</code></pre>\n\n<p>For <code>COMMIT_EDITMSG</code>, it depend on your version of magit. In version from magit's git next branch, you use text mode with the git-commit-minor-mode on, so you need to add your configuration to <code>git-commit-mode-hook</code></p>\n\n<pre><code>(add-hook 'git-commit-mode-hook 'turn-off-auto-fill)\n</code></pre>\n\n<p>With other version of magit, or with other configuration, you might be using another mode for editing COMMIT_EDITMSG. Use <code>C-h m</code> to find which, and add <code>-hook</code> to its name to find what you hook should be added to.</p>\n\n<p>(turn-off-auto-fill is already define, no need to define it).  </p>\n\n<p>Ps: you didn't want it, but I add it for completness: the magit-mode-hook exist for setting configuration available in most magit's mode (log, show commit, ....)</p>\n")
             (title . "How to make `truncate-lines` nil and `auto-fill-mode` off in magit buffers")
             (link . "http://emacs.stackexchange.com/questions/2890/how-to-make-truncate-lines-nil-and-auto-fill-mode-off-in-magit-buffers/2895#2895")
             (body_markdown . "You can add setting specific to `magit-status` to `magit-status-mode-hook`:\n\n    (defun turn-on-truncate-lines ()\n       (setq truncate-lines t))\n    \n    (add-hook &#39;magit-status-mode-hook &#39;turn-on-truncate-lines)\n\nFor `COMMIT_EDITMSG`, it depend on your version of magit. In version from magit&#39;s git next branch, you use text mode with the git-commit-minor-mode on, so you need to add your configuration to `git-commit-mode-hook`\n\n    (add-hook &#39;git-commit-mode-hook &#39;turn-off-auto-fill)\n\nWith other version of magit, or with other configuration, you might be using another mode for editing COMMIT_EDITMSG. Use `C-h m` to find which, and add `-hook` to its name to find what you hook should be added to.\n\n(turn-off-auto-fill is already define, no need to define it).  \n\nPs: you didn&#39;t want it, but I add it for completness: the magit-mode-hook exist for setting configuration available in most magit&#39;s mode (log, show commit, ....)\n")
             (question_id . 2890)
             (answer_id . 2895)
             (creation_date . 1414744957.0)
             (last_activity_date . 1414744957.0)
             (score . 2)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/2237/r%c3%a9mi")
              (display_name . "R&#233;mi")
              (profile_image . "https://www.gravatar.com/avatar/2b6a8e936e696ab9da826075fc1a07de?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 2237)
              (reputation . 376)))]))
 ((body . "<p>I am having difficulties composing a regexp that will highlight certain <em>folder</em> extensions -- <strong>including the preceding dot</strong> -- with colors that are different than the rest of the folder -- e.g., <code>.app</code> would be colored red; and <code>.git</code> would be colored blue.</p>\n\n<p>Examples:</p>\n\n<ul>\n<li><p>The folder <code>Emacs.app</code> would be colorized with <code>Emacs</code> being a gray color and <code>.app</code> would be a red color.</p></li>\n<li><p>The folder <code>.0.context-menu.git</code> would be colorized with <code>.0.context-menu</code> being a gray color and <code>.git</code> would be a blue color.</p></li>\n</ul>\n\n<p>I have a regexp that gets me from the left margin all the way to the beginning of the folder -- the variable for that regexp is named <code>folder-listing-before-filename-regexp</code>.</p>\n\n<p>The problem I am having is that the regex <code>.*</code> swallows up the folder extensions.  If I add <code>[^.]</code>, that prevents me from using the dot later on with extensions like <code>.app</code> and <code>.git</code> -- I want the extension to include the dot when colored.</p>\n\n<p>The regexp that leads up to the folder looks like this:</p>\n\n<pre><code>(defvar folder-listing-before-filename-regexp\n  (concat\n    \"\\\\(^ +d[slrwxt+-]+\\\\)\"    ;; 1  permissions\n    \"\\\\(\\s\\\\)\"                 ;; 2\n    \"\\\\([0-9]+\\\\|\\s+[0-9]+\\\\)\" ;; 3  some small numbers\n    \"\\\\(\\s\\\\)\"                 ;; 4\n    \"\\\\([a-zA-Z]+\\\\)\"          ;; 5  group\n    \"\\\\(\\s\\\\)\"                 ;; 6\n    \"\\\\([a-zA-Z]+\\\\)\"          ;; 7  user\n    \"\\\\(\\s\\\\)\"                 ;; 8\n    \"\\\\([\\s0-9.kKMGT]+\\\\)\"     ;; 9  size\n    \"\\\\(\\s\\\\)\"                 ;; 10\n    \"\\\\([0-9-]+\\\\)\"            ;; 11 date\n    \"\\\\(\\s\\\\)\"                 ;; 12\n    \"\\\\([0-9:]+\\\\)\"            ;; 13 time\n    \"\\\\(\\s\\\\)\"))               ;; 14\n</code></pre>\n\n<p>Here is some test data:</p>\n\n<pre><code>  drwxr-xr-x  79 HOME staff 2.7K 10-30-2014 16:42:18 .\n  drwxr-xr-x  27 HOME staff  918 10-28-2014 13:08:59 ..\n  drwxr-xr-x 162 HOME staff 5.4K 10-11-2014 11:34:16 .0.apel_flim_simi\n  drwxr-xr-x 432 HOME staff  15K 10-29-2014 15:37:44 .0.backup\n  drwxr-xr-x   5 HOME staff  170 10-19-2014 09:34:37 .0.context-menu.git\n  drwxr-xr-x   5 HOME staff  170 10-19-2014 13:06:36 .0.dired-read-filename.git\n  drwx------   4 HOME staff  136 04-29-2014 17:40:13 .0.eshell\n  drwxr-xr-x   3 HOME staff  102 10-11-2014 10:29:03 .0.gh\n  drwxr-xr-x   5 HOME staff  170 10-09-2014 21:16:35 .0.lorg-calendar.git\n  drwxr-xr-x   5 HOME staff  170 10-09-2014 20:06:09 .0.lorg-search.git\n  drwxr-xr-x  10 HOME staff  340 01-31-2014 19:37:31 .0.mail\n  drwxr-xr-x  25 HOME staff  850 10-14-2014 13:23:31 .0.snippets\n  drwxr-xr-x  23 HOME staff  782 12-03-2013 17:15:50 .0.sound\n  drwxr-xr-x  27 HOME staff  918 10-18-2014 11:53:56 .0.sources\n  drwxr-xr-x  67 HOME staff 2.3K 05-05-2014 23:19:12 .0.w3m\n  drwxr-xr-x 146 HOME staff 4.9K 09-08-2014 10:54:40 .0.wl\n  drwxr-xr-x   3 HOME staff  102 06-03-2014 11:37:02 Emacs.app\n  drwxr-xr-x   3 HOME staff  102 06-01-2014 10:39:02 Emacs_06_01_2014.app\n  drwxr-xr-x   3 HOME staff  102 10-01-2014 07:31:45 Emacs_10_01_2014.app\n  drwxr-xr-x 195 HOME staff 6.5K 10-18-2014 12:06:25 bin\n  drwxr-xr-x   3 HOME staff  102 06-26-2013 07:59:44 etc\n  drwxr-xr-x  10 HOME staff  340 09-30-2014 18:07:42 include\n  drwxr-xr-x   9 HOME staff  306 06-05-2013 22:50:05 info\n  drwxr-xr-x  27 HOME staff  918 10-18-2014 12:06:42 lib\n  drwxr-xr-x  12 HOME staff  408 09-30-2014 18:10:42 libexec\n  drwxr-xr-x   4 root staff  136 09-30-2014 18:07:42 man\n  drwxr-xr-x   4 root staff  136 09-30-2014 18:10:42 sbin\n  drwxr-xr-x   9 HOME staff  306 09-30-2014 18:10:42 share\n</code></pre>\n\n<p><img src=\"http://www.lawlist.com/images/dired_font_lock.png\" alt=\"Example\"></p>\n")
  (title . "Composing a regexp to highlight folder extensions differently")
  (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently")
  (body_markdown . "I am having difficulties composing a regexp that will highlight certain *folder* extensions -- **including the preceding dot** -- with colors that are different than the rest of the folder -- e.g., `.app` would be colored red; and `.git` would be colored blue.\n\nExamples:\n\n* The folder `Emacs.app` would be colorized with `Emacs` being a gray color and `.app` would be a red color.\n\n* The folder `.0.context-menu.git` would be colorized with `.0.context-menu` being a gray color and `.git` would be a blue color.\n\nI have a regexp that gets me from the left margin all the way to the beginning of the folder -- the variable for that regexp is named `folder-listing-before-filename-regexp`.\n\nThe problem I am having is that the regex `.*` swallows up the folder extensions.  If I add `[^.]`, that prevents me from using the dot later on with extensions like `.app` and `.git` -- I want the extension to include the dot when colored.\n\nThe regexp that leads up to the folder looks like this:\n\n    (defvar folder-listing-before-filename-regexp\n      (concat\n        &quot;\\\\(^ +d[slrwxt+-]+\\\\)&quot;    ;; 1  permissions\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 2\n        &quot;\\\\([0-9]+\\\\|\\s+[0-9]+\\\\)&quot; ;; 3  some small numbers\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 4\n        &quot;\\\\([a-zA-Z]+\\\\)&quot;          ;; 5  group\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 6\n        &quot;\\\\([a-zA-Z]+\\\\)&quot;          ;; 7  user\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 8\n        &quot;\\\\([\\s0-9.kKMGT]+\\\\)&quot;     ;; 9  size\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 10\n        &quot;\\\\([0-9-]+\\\\)&quot;            ;; 11 date\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 12\n        &quot;\\\\([0-9:]+\\\\)&quot;            ;; 13 time\n        &quot;\\\\(\\s\\\\)&quot;))               ;; 14\n\nHere is some test data:\n\n      drwxr-xr-x  79 HOME staff 2.7K 10-30-2014 16:42:18 .\n      drwxr-xr-x  27 HOME staff  918 10-28-2014 13:08:59 ..\n      drwxr-xr-x 162 HOME staff 5.4K 10-11-2014 11:34:16 .0.apel_flim_simi\n      drwxr-xr-x 432 HOME staff  15K 10-29-2014 15:37:44 .0.backup\n      drwxr-xr-x   5 HOME staff  170 10-19-2014 09:34:37 .0.context-menu.git\n      drwxr-xr-x   5 HOME staff  170 10-19-2014 13:06:36 .0.dired-read-filename.git\n      drwx------   4 HOME staff  136 04-29-2014 17:40:13 .0.eshell\n      drwxr-xr-x   3 HOME staff  102 10-11-2014 10:29:03 .0.gh\n      drwxr-xr-x   5 HOME staff  170 10-09-2014 21:16:35 .0.lorg-calendar.git\n      drwxr-xr-x   5 HOME staff  170 10-09-2014 20:06:09 .0.lorg-search.git\n      drwxr-xr-x  10 HOME staff  340 01-31-2014 19:37:31 .0.mail\n      drwxr-xr-x  25 HOME staff  850 10-14-2014 13:23:31 .0.snippets\n      drwxr-xr-x  23 HOME staff  782 12-03-2013 17:15:50 .0.sound\n      drwxr-xr-x  27 HOME staff  918 10-18-2014 11:53:56 .0.sources\n      drwxr-xr-x  67 HOME staff 2.3K 05-05-2014 23:19:12 .0.w3m\n      drwxr-xr-x 146 HOME staff 4.9K 09-08-2014 10:54:40 .0.wl\n      drwxr-xr-x   3 HOME staff  102 06-03-2014 11:37:02 Emacs.app\n      drwxr-xr-x   3 HOME staff  102 06-01-2014 10:39:02 Emacs_06_01_2014.app\n      drwxr-xr-x   3 HOME staff  102 10-01-2014 07:31:45 Emacs_10_01_2014.app\n      drwxr-xr-x 195 HOME staff 6.5K 10-18-2014 12:06:25 bin\n      drwxr-xr-x   3 HOME staff  102 06-26-2013 07:59:44 etc\n      drwxr-xr-x  10 HOME staff  340 09-30-2014 18:07:42 include\n      drwxr-xr-x   9 HOME staff  306 06-05-2013 22:50:05 info\n      drwxr-xr-x  27 HOME staff  918 10-18-2014 12:06:42 lib\n      drwxr-xr-x  12 HOME staff  408 09-30-2014 18:10:42 libexec\n      drwxr-xr-x   4 root staff  136 09-30-2014 18:07:42 man\n      drwxr-xr-x   4 root staff  136 09-30-2014 18:10:42 sbin\n      drwxr-xr-x   9 HOME staff  306 09-30-2014 18:10:42 share\n\n\n\n![Example](http://www.lawlist.com/images/dired_font_lock.png)")
  (question_id . 2881)
  (creation_date . 1414714432.0)
  (last_activity_date . 1414742131.0)
  (score . 3)
  (answer_count . 1)
  (favorite_count . 0)
  (view_count . 29)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 9)
  (comments .
            [((body . "Have you tried looking into other options? Such as <a href=\"http://www.emacswiki.org/DiredPlus\" rel=\"nofollow\">Dired+</a>?")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3933_2881")
              (body_markdown . "Have you tried looking into other options? Such as [Dired+](http://www.emacswiki.org/DiredPlus)?")
              (comment_id . 3933)
              (post_id . 2881)
              (creation_date . 1414715005.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/45/king-shimkus")
               (display_name . "King Shimkus")
               (profile_image . "https://www.gravatar.com/avatar/073274c272b84eed27790fc3e5231d46?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 93)
               (user_type . "registered")
               (user_id . 45)
               (reputation . 1217)))
             ((body . "Why would <code>[^.]*</code> prevent you from using the dot? Something like <code>[^.]*\\\\(\\\\..*\\\\)</code> should match your request and group the entire extension including the dot. Show us what you&#39;ve tried.")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3934_2881")
              (body_markdown . "Why would `[^.]*` prevent you from using the dot? Something like `[^.]*\\\\(\\\\..*\\\\)` should match your request and group the entire extension including the dot. Show us what you&#39;ve tried.")
              (comment_id . 3934)
              (post_id . 2881)
              (creation_date . 1414715050.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "@Malabarba -- your suggestion almost does it, with the exception of the folders beginning with a dot -- e.g., <code>.0.context-menu.git</code>  I&#39;ll work on it a little later on this evening using your example as a starting point.")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3935_2881")
              (body_markdown . "@Malabarba -- your suggestion almost does it, with the exception of the folders beginning with a dot -- e.g., `.0.context-menu.git`  I&#39;ll work on it a little later on this evening using your example as a starting point.")
              (comment_id . 3935)
              (post_id . 2881)
              (creation_date . 1414715279.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/2287/lawlist")
               (display_name . "lawlist")
               (profile_image . "https://www.gravatar.com/avatar/5f4ed4b573c4e38444c6fc54b28076c0?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2287)
               (reputation . 144)))
             ((body . "I see. And does <code>.*\\\\(\\\\.[^.]*\\\\)$</code> not work either? (I&#39;m not at the pc right now so I can&#39;t test myself).")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3936_2881")
              (body_markdown . "I see. And does `.*\\\\(\\\\.[^.]*\\\\)$` not work either? (I&#39;m not at the pc right now so I can&#39;t test myself).")
              (comment_id . 3936)
              (post_id . 2881)
              (creation_date . 1414715491.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "@King Shimkus -- yes, thank you -- the regex for the permission colors and file size are from Dired+, but I broke some of the functionality by adding a custom time-style.  My custom time-style is also not compatible with <code>directory-listing-before-filename-regexp</code>.  Dired mode is not designed for a custom time-style like the one I&#39;m using, so I&#39;m essentially creating a custom version of dired-mode.")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3937_2881")
              (body_markdown . "@King Shimkus -- yes, thank you -- the regex for the permission colors and file size are from Dired+, but I broke some of the functionality by adding a custom time-style.  My custom time-style is also not compatible with `directory-listing-before-filename-regexp`.  Dired mode is not designed for a custom time-style like the one I&#39;m using, so I&#39;m essentially creating a custom version of dired-mode.")
              (comment_id . 3937)
              (post_id . 2881)
              (creation_date . 1414715505.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/45/king-shimkus")
               (display_name . "King Shimkus")
               (profile_image . "https://www.gravatar.com/avatar/073274c272b84eed27790fc3e5231d46?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 93)
               (user_type . "registered")
               (user_id . 45)
               (reputation . 1217))
              (owner
               (link . "http://emacs.stackexchange.com/users/2287/lawlist")
               (display_name . "lawlist")
               (profile_image . "https://www.gravatar.com/avatar/5f4ed4b573c4e38444c6fc54b28076c0?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2287)
               (reputation . 144)))
             ((body . "@Malabarba -- Yes, I think that does it!  I&#39;ll report back later this evening when I&#39;ve done a little more testing.  Thank you.")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3938_2881")
              (body_markdown . "@Malabarba -- Yes, I think that does it!  I&#39;ll report back later this evening when I&#39;ve done a little more testing.  Thank you.")
              (comment_id . 3938)
              (post_id . 2881)
              (creation_date . 1414715687.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/2287/lawlist")
               (display_name . "lawlist")
               (profile_image . "https://www.gravatar.com/avatar/5f4ed4b573c4e38444c6fc54b28076c0?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2287)
               (reputation . 144)))
             ((body . "@Malabarba -- I have a working draft of a few categories with slightly different regexp for each category, and a catch-all category that incorporates a custom list consisting of anything not previously handled.  However, I have not yet been able to compose one whopper regexp for all situations -- e.g., <code>\\\\(?:stuff\\\\(more-stuff\\\\)\\\\)?\\\\(?:misc\\\\(more-misc\\\\)\\\\)?$</code>.  I&#39;d like to please leave this thread open for a couple of days to do some more experimenting and then post a working draft as an edit to my question.  Thanks again for putting me on the right track -- greatly appreciated!  :)")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3942_2881")
              (body_markdown . "@Malabarba -- I have a working draft of a few categories with slightly different regexp for each category, and a catch-all category that incorporates a custom list consisting of anything not previously handled.  However, I have not yet been able to compose one whopper regexp for all situations -- e.g., `\\\\(?:stuff\\\\(more-stuff\\\\)\\\\)?\\\\(?:misc\\\\(more-misc\\\\)\\\\)?$`.  I&#39;d like to please leave this thread open for a couple of days to do some more experimenting and then post a working draft as an edit to my question.  Thanks again for putting me on the right track -- greatly appreciated!  :)")
              (comment_id . 3942)
              (post_id . 2881)
              (creation_date . 1414735370.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/2287/lawlist")
               (display_name . "lawlist")
               (profile_image . "https://www.gravatar.com/avatar/5f4ed4b573c4e38444c6fc54b28076c0?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2287)
               (reputation . 144)))
             ((body . "@lawlist I may be wrong, but it looks like you&#39;re using question marks when you actually need a <code>\\\\|</code>.")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3957_2881")
              (body_markdown . "@lawlist I may be wrong, but it looks like you&#39;re using question marks when you actually need a `\\\\|`.")
              (comment_id . 3957)
              (post_id . 2881)
              (creation_date . 1414753681.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "@Malabarba -- I was hoping to do something like 15 (if matched) will be red, 16 (if matched) will be blue, 17 (if matched) will be green, and so on.  If I use <code>\\\\|</code>, then they will all be the same color -- unless it&#39;s somehow possible to use <code>\\\\(something\\\\)</code> in conjunction with <code>\\\\|</code> -- I&#39;ve never seen an example that uses both.  However, my initial attempts at putting possible matches with question marks haven&#39;t worked.")
              (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently#comment3994_2881")
              (body_markdown . "@Malabarba -- I was hoping to do something like 15 (if matched) will be red, 16 (if matched) will be blue, 17 (if matched) will be green, and so on.  If I use `\\\\|`, then they will all be the same color -- unless it&#39;s somehow possible to use `\\\\(something\\\\)` in conjunction with `\\\\|` -- I&#39;ve never seen an example that uses both.  However, my initial attempts at putting possible matches with question marks haven&#39;t worked.")
              (comment_id . 3994)
              (post_id . 2881)
              (creation_date . 1414772949.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/2287/lawlist")
               (display_name . "lawlist")
               (profile_image . "https://www.gravatar.com/avatar/5f4ed4b573c4e38444c6fc54b28076c0?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2287)
               (reputation . 144)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2287/lawlist")
   (display_name . "lawlist")
   (profile_image . "https://www.gravatar.com/avatar/5f4ed4b573c4e38444c6fc54b28076c0?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2287)
   (reputation . 144))
  (tags .
        ["dired" "regular-expressions"])
  (answers .
           [((body . "<p>The following is just a tweak to Dired+ -- adopting the same manner to highlight folder extensions as is already done with file extensions; with the additional tweak to the beginning regexp to get me over to the files / folders.  My custom time-style of <code>ls</code> is not compatible with dired-mode or the corresponding regexp in files.el, so I composed my own regexp.  The section below relating to <code>diredp-font-lock-keywords-1</code> is just the relevant excerpt.</p>\n\n<p>Over the next few days, I'll continue to think about the regexp proposed by @Malabarba <code>.*\\\\(\\\\.[^.]*\\\\)$</code> to see if I can create a whopper regexp that covers the majority of situations.  I ended up with about 5 different regexp strings, which is not as clean as Dired+ presently offers.</p>\n\n  \n\n<pre><code>(defvar folder-listing-before-filename-regexp\n  (concat\n    \"\\\\(^ +d[slrwxt+-]+\\\\)\"    ;; 1  permissions\n    \"\\\\(\\s\\\\)\"                 ;; 2\n    \"\\\\([0-9]+\\\\|\\s+[0-9]+\\\\)\" ;; 3  some small numbers\n    \"\\\\(\\s\\\\)\"                 ;; 4\n    \"\\\\([a-zA-Z]+\\\\)\"          ;; 5  user\n    \"\\\\(\\s\\\\)\"                 ;; 6\n    \"\\\\([a-zA-Z]+\\\\)\"          ;; 7  group\n    \"\\\\(\\s\\\\)\"                 ;; 8\n    \"\\\\([\\s0-9.kKMGT]+\\\\)\"     ;; 9  size\n    \"\\\\(\\s\\\\)\"                 ;; 10\n    \"\\\\([0-9-]+\\\\)\"            ;; 11 date\n    \"\\\\(\\s\\\\)\"                 ;; 12\n    \"\\\\([0-9:]+\\\\)\"            ;; 13 time\n    \"\\\\(\\s\\\\)\"))               ;; 14\n\n(defvar file-listing-before-filename-regexp\n  (concat\n    \"\\\\(^ +[^d][slrwxt+-]+\\\\)\" ;; 1  permissions\n    \"\\\\(\\s\\\\)\"                 ;; 2\n    \"\\\\([0-9]+\\\\|\\s+[0-9]+\\\\)\" ;; 3  some small numbers\n    \"\\\\(\\s\\\\)\"                 ;; 4\n    \"\\\\([a-zA-Z]+\\\\)\"          ;; 5  user\n    \"\\\\(\\s\\\\)\"                 ;; 6\n    \"\\\\([a-zA-Z]+\\\\)\"          ;; 7  group\n    \"\\\\(\\s\\\\)\"                 ;; 8\n    \"\\\\([\\s0-9.kKMGT]+\\\\)\"     ;; 9  size\n    \"\\\\(\\s\\\\)\"                 ;; 10\n    \"\\\\([0-9-]+\\\\)\"            ;; 11 date\n    \"\\\\(\\s\\\\)\"                 ;; 12\n    \"\\\\([0-9:]+\\\\)\"            ;; 13 time\n    \"\\\\(\\s\\\\)\"))               ;; 14\n\n(defvar diredp-font-lock-keywords-1\n  (list\n    '(\"[^ .]\\\\.\\\\(git\\\\)$\" 1 'diredp-git-face t)\n    '(\"[^ .]\\\\.\\\\(app\\\\)$\" 1 'diredp-app-face t)\n\n  (list folder-listing-before-filename-regexp\n    (list \"\\\\(.+\\\\)$\" nil nil (list 0 diredp-dir-priv 'keep t))) ; folder-name\n\n  (list file-listing-before-filename-regexp\n    (list \"\\\\(.+\\\\)$\" nil nil (list 0 diredp-file-name 'keep t))) ; file-name\n  ))\n</code></pre>\n")
             (title . "Composing a regexp to highlight folder extensions differently")
             (link . "http://emacs.stackexchange.com/questions/2881/composing-a-regexp-to-highlight-folder-extensions-differently/2894#2894")
             (body_markdown . "The following is just a tweak to Dired+ -- adopting the same manner to highlight folder extensions as is already done with file extensions; with the additional tweak to the beginning regexp to get me over to the files / folders.  My custom time-style of `ls` is not compatible with dired-mode or the corresponding regexp in files.el, so I composed my own regexp.  The section below relating to `diredp-font-lock-keywords-1` is just the relevant excerpt.\n\nOver the next few days, I&#39;ll continue to think about the regexp proposed by @Malabarba `.*\\\\(\\\\.[^.]*\\\\)$` to see if I can create a whopper regexp that covers the majority of situations.  I ended up with about 5 different regexp strings, which is not as clean as Dired+ presently offers.\n\n\n  &lt;!-- language: lang-lisp --&gt;\n\n    (defvar folder-listing-before-filename-regexp\n      (concat\n        &quot;\\\\(^ +d[slrwxt+-]+\\\\)&quot;    ;; 1  permissions\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 2\n        &quot;\\\\([0-9]+\\\\|\\s+[0-9]+\\\\)&quot; ;; 3  some small numbers\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 4\n        &quot;\\\\([a-zA-Z]+\\\\)&quot;          ;; 5  user\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 6\n        &quot;\\\\([a-zA-Z]+\\\\)&quot;          ;; 7  group\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 8\n        &quot;\\\\([\\s0-9.kKMGT]+\\\\)&quot;     ;; 9  size\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 10\n        &quot;\\\\([0-9-]+\\\\)&quot;            ;; 11 date\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 12\n        &quot;\\\\([0-9:]+\\\\)&quot;            ;; 13 time\n        &quot;\\\\(\\s\\\\)&quot;))               ;; 14\n    \n    (defvar file-listing-before-filename-regexp\n      (concat\n        &quot;\\\\(^ +[^d][slrwxt+-]+\\\\)&quot; ;; 1  permissions\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 2\n        &quot;\\\\([0-9]+\\\\|\\s+[0-9]+\\\\)&quot; ;; 3  some small numbers\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 4\n        &quot;\\\\([a-zA-Z]+\\\\)&quot;          ;; 5  user\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 6\n        &quot;\\\\([a-zA-Z]+\\\\)&quot;          ;; 7  group\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 8\n        &quot;\\\\([\\s0-9.kKMGT]+\\\\)&quot;     ;; 9  size\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 10\n        &quot;\\\\([0-9-]+\\\\)&quot;            ;; 11 date\n        &quot;\\\\(\\s\\\\)&quot;                 ;; 12\n        &quot;\\\\([0-9:]+\\\\)&quot;            ;; 13 time\n        &quot;\\\\(\\s\\\\)&quot;))               ;; 14\n    \n    (defvar diredp-font-lock-keywords-1\n      (list\n        &#39;(&quot;[^ .]\\\\.\\\\(git\\\\)$&quot; 1 &#39;diredp-git-face t)\n        &#39;(&quot;[^ .]\\\\.\\\\(app\\\\)$&quot; 1 &#39;diredp-app-face t)\n    \n      (list folder-listing-before-filename-regexp\n        (list &quot;\\\\(.+\\\\)$&quot; nil nil (list 0 diredp-dir-priv &#39;keep t))) ; folder-name\n    \n      (list file-listing-before-filename-regexp\n        (list &quot;\\\\(.+\\\\)$&quot; nil nil (list 0 diredp-file-name &#39;keep t))) ; file-name\n      ))")
             (question_id . 2881)
             (answer_id . 2894)
             (creation_date . 1414742131.0)
             (last_activity_date . 1414742131.0)
             (score . 1)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/2287/lawlist")
              (display_name . "lawlist")
              (profile_image . "https://www.gravatar.com/avatar/5f4ed4b573c4e38444c6fc54b28076c0?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 2287)
              (reputation . 144)))]))
 ((body . "<p>TL;DR: How can I force evaluation of a <code>setq</code> statement at start-up so that <code>imenu</code> updates appropriately?  (You can skip down to the second code-block below, if you want.)</p>\n\n<p>I love the concept of <code>imenu</code> (esp. when integrated with <code>helm</code>) and I think it has great potential to improve my workflow...assuming I learn how to correctly set definitions for <code>imenu</code> categories.  </p>\n\n<p>As a simplified example, let's say that I'd like to add a few definitions to my <code>init</code> file's <code>imenu</code> which would be useful when making updates - namely, the list of imported packages.</p>\n\n<p>Let's consider the following three lines as the code we want to parse (purely illustrative; my <code>init</code> file doesn't actually have an explicit line for importing <code>dired</code> and <code>electric</code>):</p>\n\n<pre><code>(defconst dc/default-directory \"/path/to/directory\")\n(use-package dired)\n(use-package electric)\n</code></pre>\n\n<p>In order to extract the constants and package names in <code>imenu</code>, I've included the code below in my <code>init.el</code> file.  This code works BUT only after I manually <code>eval</code> the second line, (i.e., the second <code>setq</code> statement below).</p>\n\n<pre><code>(setq dc-additions '((\"Imported packages\" \".*(use-package \\\\(.*\\\\))*\" 1)\n                     (\"Defined constants\" \".*(defconst \\\\(.*\\\\))*\"    1)))\n(setq imenu-generic-expression (copy-sequence `(,@dc-additions ,@imenu-generic-expression)))\n(add-hook 'emacs-lisp-mode 'imenu-generic-expression)\n</code></pre>\n\n<p>If I don't manually evaluate the second step, the <code>imenu</code> list shows only the default definitions.  However, after I <code>eval</code> the second statement above, the <code>imenu</code> list correctly include includes items based on my definition.</p>\n\n<p>How can I force evaluation of the second statement?  In other words, what can I do to to ensure that <code>imenu</code> will automatically consider items based on my definition?</p>\n\n<hr>\n\n<p>Exhibit 1: <code>imenu</code> after start-up but before I manually <code>eval</code> the second <code>setq</code> statement:</p>\n\n<p><img src=\"http://i.stack.imgur.com/Kv78R.png\" alt=\"enter image description here\"></p>\n\n<p>Exhibit 2: <code>imenu</code> after I manually <code>eval</code> the statement:</p>\n\n<p><img src=\"http://i.stack.imgur.com/iKKqe.png\" alt=\"enter image description here\"></p>\n")
  (title . "How to force evaluation of a statement in my `init` file?")
  (link . "http://emacs.stackexchange.com/questions/2860/how-to-force-evaluation-of-a-statement-in-my-init-file")
  (body_markdown . "TL;DR: How can I force evaluation of a `setq` statement at start-up so that `imenu` updates appropriately?  (You can skip down to the second code-block below, if you want.)\n\nI love the concept of `imenu` (esp. when integrated with `helm`) and I think it has great potential to improve my workflow...assuming I learn how to correctly set definitions for `imenu` categories.  \n\nAs a simplified example, let&#39;s say that I&#39;d like to add a few definitions to my `init` file&#39;s `imenu` which would be useful when making updates - namely, the list of imported packages.\n\nLet&#39;s consider the following three lines as the code we want to parse (purely illustrative; my `init` file doesn&#39;t actually have an explicit line for importing `dired` and `electric`):\n\n    (defconst dc/default-directory &quot;/path/to/directory&quot;)\n    (use-package dired)\n    (use-package electric)\n\nIn order to extract the constants and package names in `imenu`, I&#39;ve included the code below in my `init.el` file.  This code works BUT only after I manually `eval` the second line, (i.e., the second `setq` statement below).\n\n    (setq dc-additions &#39;((&quot;Imported packages&quot; &quot;.*(use-package \\\\(.*\\\\))*&quot; 1)\n    		             (&quot;Defined constants&quot; &quot;.*(defconst \\\\(.*\\\\))*&quot;    1)))\n    (setq imenu-generic-expression (copy-sequence `(,@dc-additions ,@imenu-generic-expression)))\n    (add-hook &#39;emacs-lisp-mode &#39;imenu-generic-expression)\n\nIf I don&#39;t manually evaluate the second step, the `imenu` list shows only the default definitions.  However, after I `eval` the second statement above, the `imenu` list correctly include includes items based on my definition.\n\nHow can I force evaluation of the second statement?  In other words, what can I do to to ensure that `imenu` will automatically consider items based on my definition?\n***\nExhibit 1: `imenu` after start-up but before I manually `eval` the second `setq` statement:\n\n![enter image description here][1]\n\nExhibit 2: `imenu` after I manually `eval` the statement:\n\n![enter image description here][2]\n\n\n  [1]: http://i.stack.imgur.com/Kv78R.png\n  [2]: http://i.stack.imgur.com/iKKqe.png")
  (question_id . 2860)
  (last_edit_date . 1414720130.0)
  (creation_date . 1414687221.0)
  (last_activity_date . 1414736153.0)
  (score . 0)
  (answer_count . 1)
  (accepted_answer_id . 2892)
  (favorite_count . 0)
  (view_count . 52)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 6)
  (last_editor
   (link . "http://emacs.stackexchange.com/users/2084/dipak-c")
   (display_name . "Dipak C")
   (profile_image . "https://www.gravatar.com/avatar/ab1a56787ee3fc3602592a1909cbd0ea?s=128&d=identicon&r=PG&f=1")
   (accept_rate . 100)
   (user_type . "registered")
   (user_id . 2084)
   (reputation . 156))
  (comments .
            [((body . "Could you try <code>helm-imenu</code> alone? <code>helm-semantic-or-imenu</code>, as its name suggests, use either semantic <b>or</b> imenu. If the command has candidates from <code>helm-semantic</code>, it won&#39;t display imenu&#39;s candidates, unless the candidates from <code>helm-semantic</code> is <code>nil</code>. There&#39;s also a stand alone <code>helm-semantic</code>. The advantage of <code>helm-semantic</code> is that it provides more information, so you can look thing like function interface and preselect the candidate when run <code>helm-semantic</code>. But Imenu is easier to extend without touching Semantic parser.")
              (link . "http://emacs.stackexchange.com/questions/2860/how-to-force-evaluation-of-a-statement-in-my-init-file#comment3908_2860")
              (body_markdown . "Could you try `helm-imenu` alone? `helm-semantic-or-imenu`, as its name suggests, use either semantic **or** imenu. If the command has candidates from `helm-semantic`, it won&#39;t display imenu&#39;s candidates, unless the candidates from `helm-semantic` is `nil`. There&#39;s also a stand alone `helm-semantic`. The advantage of `helm-semantic` is that it provides more information, so you can look thing like function interface and preselect the candidate when run `helm-semantic`. But Imenu is easier to extend without touching Semantic parser.")
              (comment_id . 3908)
              (post_id . 2860)
              (creation_date . 1414690376.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))
             ((body . "unfortunately, <code>helm-imenu</code> is also blank.  in fact, both <code>helm-imenu</code> and <code>helm-semantic</code> are blank.")
              (link . "http://emacs.stackexchange.com/questions/2860/how-to-force-evaluation-of-a-statement-in-my-init-file#comment3910_2860")
              (body_markdown . "unfortunately, `helm-imenu` is also blank.  in fact, both `helm-imenu` and `helm-semantic` are blank.")
              (comment_id . 3910)
              (post_id . 2860)
              (creation_date . 1414691174.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652))
              (owner
               (link . "http://emacs.stackexchange.com/users/2084/dipak-c")
               (display_name . "Dipak C")
               (profile_image . "https://www.gravatar.com/avatar/ab1a56787ee3fc3602592a1909cbd0ea?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 2084)
               (reputation . 156)))
             ((body . "You use <code>setq</code> which overrides the default <code>imenu-generic-expression</code>, and I think your regex is potentially wrong, so it shows nothing. Try removing your change and test <code>helm-imenu</code> again.")
              (link . "http://emacs.stackexchange.com/questions/2860/how-to-force-evaluation-of-a-statement-in-my-init-file#comment3911_2860")
              (body_markdown . "You use `setq` which overrides the default `imenu-generic-expression`, and I think your regex is potentially wrong, so it shows nothing. Try removing your change and test `helm-imenu` again.")
              (comment_id . 3911)
              (post_id . 2860)
              (creation_date . 1414691529.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652)))
             ((body . "@Tu Do: I&#39;ve updated the methodology so that my new list of definitions is appended to the existing list (using the <code>,@</code> operator).  As described in my (heavily) edited post above, the new code works but only after I manually evaluated part of the code (details above).")
              (link . "http://emacs.stackexchange.com/questions/2860/how-to-force-evaluation-of-a-statement-in-my-init-file#comment3918_2860")
              (body_markdown . "@Tu Do: I&#39;ve updated the methodology so that my new list of definitions is appended to the existing list (using the `,@` operator).  As described in my (heavily) edited post above, the new code works but only after I manually evaluated part of the code (details above).")
              (comment_id . 3918)
              (post_id . 2860)
              (creation_date . 1414702689.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/200/tu-do")
               (display_name . "Tu Do")
               (profile_image . "https://www.gravatar.com/avatar/4468daf477f79462d675b0be10cd6e22?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 75)
               (user_type . "registered")
               (user_id . 200)
               (reputation . 652))
              (owner
               (link . "http://emacs.stackexchange.com/users/2084/dipak-c")
               (display_name . "Dipak C")
               (profile_image . "https://www.gravatar.com/avatar/ab1a56787ee3fc3602592a1909cbd0ea?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 2084)
               (reputation . 156)))
             ((body . "You&#39;re saying it doesn&#39;t take effect during initialization?")
              (link . "http://emacs.stackexchange.com/questions/2860/how-to-force-evaluation-of-a-statement-in-my-init-file#comment3919_2860")
              (body_markdown . "You&#39;re saying it doesn&#39;t take effect during initialization?")
              (comment_id . 3919)
              (post_id . 2860)
              (creation_date . 1414702847.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999)))
             ((body . "@Malabarba That&#39;s right, I&#39;ve added before and after pictures for clarification.")
              (link . "http://emacs.stackexchange.com/questions/2860/how-to-force-evaluation-of-a-statement-in-my-init-file#comment3922_2860")
              (body_markdown . "@Malabarba That&#39;s right, I&#39;ve added before and after pictures for clarification.")
              (comment_id . 3922)
              (post_id . 2860)
              (creation_date . 1414704904.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (reply_to_user
               (link . "http://emacs.stackexchange.com/users/50/malabarba")
               (display_name . "Malabarba")
               (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
               (accept_rate . 89)
               (user_type . "registered")
               (user_id . 50)
               (reputation . 3999))
              (owner
               (link . "http://emacs.stackexchange.com/users/2084/dipak-c")
               (display_name . "Dipak C")
               (profile_image . "https://www.gravatar.com/avatar/ab1a56787ee3fc3602592a1909cbd0ea?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 100)
               (user_type . "registered")
               (user_id . 2084)
               (reputation . 156)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2084/dipak-c")
   (display_name . "Dipak C")
   (profile_image . "https://www.gravatar.com/avatar/ab1a56787ee3fc3602592a1909cbd0ea?s=128&d=identicon&r=PG&f=1")
   (accept_rate . 100)
   (user_type . "registered")
   (user_id . 2084)
   (reputation . 156))
  (tags .
        ["elisp" "helm"])
  (answers .
           [((body . "<p><code>imenu-generic-expression</code> is a buffer local variable. When opening a new buffer, imenu will set imenu-generic-expression local value to something interesting for the current buffer. So setting imenu-generic-expression in init won't be useful, as its default-value is never used.</p>\n\n<p>What you want to set is the variable that it used by imenu for setting imenu-generic-expression in <code>emacs-lisp-mode</code>. It seem that it is the value from <code>lisp-imenu-generic-expression</code>, that is define in <code>lisp-mode</code>. So you just have to:</p>\n\n<pre><code>(with-eval-after-load 'lisp-mode\n    (setq lisp-imenu-generic-expression (copy-sequence `(,@dc-additions ,@lisp-imenu-generic-expression))))\n</code></pre>\n\n<p>Note that you must eval the <code>setq lisp-imenu-generic-expression</code> in a <code>with-eval-after-load</code> because you depend on the default value of <code>lisp-imenu-generic-expression</code> that won't be known to Emacs before lisp-mode is loaded. </p>\n")
             (title . "How to force evaluation of a statement in my `init` file?")
             (link . "http://emacs.stackexchange.com/questions/2860/how-to-force-evaluation-of-a-statement-in-my-init-file/2892#2892")
             (body_markdown . "`imenu-generic-expression` is a buffer local variable. When opening a new buffer, imenu will set imenu-generic-expression local value to something interesting for the current buffer. So setting imenu-generic-expression in init won&#39;t be useful, as its default-value is never used.\n\nWhat you want to set is the variable that it used by imenu for setting imenu-generic-expression in `emacs-lisp-mode`. It seem that it is the value from `lisp-imenu-generic-expression`, that is define in `lisp-mode`. So you just have to:\n\n    (with-eval-after-load &#39;lisp-mode\n        (setq lisp-imenu-generic-expression (copy-sequence `(,@dc-additions ,@lisp-imenu-generic-expression))))\n\nNote that you must eval the `setq lisp-imenu-generic-expression` in a `with-eval-after-load` because you depend on the default value of `lisp-imenu-generic-expression` that won&#39;t be known to Emacs before lisp-mode is loaded. \n")
             (question_id . 2860)
             (answer_id . 2892)
             (creation_date . 1414736153.0)
             (last_activity_date . 1414736153.0)
             (score . 3)
             (is_accepted . t)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/2237/r%c3%a9mi")
              (display_name . "R&#233;mi")
              (profile_image . "https://www.gravatar.com/avatar/2b6a8e936e696ab9da826075fc1a07de?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 2237)
              (reputation . 376)))]))
 ((body . "<p>How can I keep my .org files up to date across several computers, perhaps across multiple platforms(linux/windows)?</p>\n\n<p>I could keep all the .org files in git for example, but that would require me to remember to pull and push to keep the repo updated. Could work with a few scripts to automatically handle the files, as I don't have that many.</p>\n\n<p>I could try using dropbox for this as well. I'd assume dropbox syncs the files often enough to not cause problems.</p>\n\n<p>Does org-mode offer any functionality to help with synchronizing the files across multiple locations?</p>\n")
  (title . "Keeping my .org files in sync across multiple computers")
  (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers")
  (body_markdown . "How can I keep my .org files up to date across several computers, perhaps across multiple platforms(linux/windows)?\n\nI could keep all the .org files in git for example, but that would require me to remember to pull and push to keep the repo updated. Could work with a few scripts to automatically handle the files, as I don&#39;t have that many.\n\nI could try using dropbox for this as well. I&#39;d assume dropbox syncs the files often enough to not cause problems.\n\nDoes org-mode offer any functionality to help with synchronizing the files across multiple locations?")
  (question_id . 2871)
  (creation_date . 1414704876.0)
  (last_activity_date . 1414723518.0)
  (score . 7)
  (answer_count . 5)
  (favorite_count . 0)
  (view_count . 258)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 3)
  (comments .
            [((body . "Does this help? <a href=\"http://orgmode.org/worg/org-tutorials/org-vcs.html\" rel=\"nofollow\">Putting your org...</a>")
              (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers#comment3923_2871")
              (body_markdown . "Does this help? [Putting your org...](http://orgmode.org/worg/org-tutorials/org-vcs.html)")
              (comment_id . 3923)
              (post_id . 2871)
              (creation_date . 1414705534.0)
              (post_type . "question")
              (score . 1)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/45/king-shimkus")
               (display_name . "King Shimkus")
               (profile_image . "https://www.gravatar.com/avatar/073274c272b84eed27790fc3e5231d46?s=128&d=identicon&r=PG&f=1")
               (accept_rate . 93)
               (user_type . "registered")
               (user_id . 45)
               (reputation . 1217)))
             ((body . "Dropbox syncs files as soon as they are written.  Internally, I believe Dropbox uses a Python script to monitor the directory.")
              (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers#comment3925_2871")
              (body_markdown . "Dropbox syncs files as soon as they are written.  Internally, I believe Dropbox uses a Python script to monitor the directory.")
              (comment_id . 3925)
              (post_id . 2871)
              (creation_date . 1414706465.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/2264/sean-allred")
               (display_name . "Sean Allred")
               (profile_image . "https://www.gravatar.com/avatar/9261936847b5a31e15da6e86533d3de3?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2264)
               (reputation . 512)))
             ((body . "If security/privacy is an issue, <a href=\"https://spideroak.com/\" rel=\"nofollow\">SpiderOak</a> is also pretty great.")
              (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers#comment3932_2871")
              (body_markdown . "If security/privacy is an issue, [SpiderOak](https://spideroak.com/) is also pretty great.")
              (comment_id . 3932)
              (post_id . 2871)
              (creation_date . 1414713689.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/2224/jon")
               (display_name . "jon")
               (profile_image . "https://www.gravatar.com/avatar/b4968ea2e03e3781500351ae376316c4?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2224)
               (reputation . 101)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/2042/zavior")
   (display_name . "Zavior")
   (profile_image . "https://www.gravatar.com/avatar/233098e41e5f22e4cdf7ecea04ffb991?s=128&d=identicon&r=PG")
   (user_type . "registered")
   (user_id . 2042)
   (reputation . 136))
  (tags .
        ["org-mode"])
  (answers .
           [((body . "<p>Org files are just plain text files, so any technique that is able to synchronise plain text files will work fine with org files.</p>\n\n<p>My favourite synchronisation tool is <a href=\"http://www.cis.upenn.edu/~bcpierce/unison/\" rel=\"nofollow\">Unison</a>.  With the right configuration file, I type</p>\n\n<pre><code>unison org\n</code></pre>\n\n<p>and Unison will compare files in my <code>~/org/</code> directories on both my machines, let me interactively review its actions, then copy the files that need to be copied and invoke an external merge tool for the files that need merging.</p>\n")
             (title . "Keeping my .org files in sync across multiple computers")
             (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers/2873#2873")
             (body_markdown . "Org files are just plain text files, so any technique that is able to synchronise plain text files will work fine with org files.\n\nMy favourite synchronisation tool is &lt;a href=&quot;http://www.cis.upenn.edu/~bcpierce/unison/&quot;&gt;Unison&lt;/a&gt;.  With the right configuration file, I type\n\n    unison org\n\nand Unison will compare files in my `~/org/` directories on both my machines, let me interactively review its actions, then copy the files that need to be copied and invoke an external merge tool for the files that need merging.")
             (question_id . 2871)
             (answer_id . 2873)
             (creation_date . 1414705578.0)
             (last_edit_date . 1414715009.0)
             (last_activity_date . 1414715009.0)
             (score . 3)
             (is_accepted . :json-false)
             (comment_count . 0)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/2223/jch")
              (display_name . "jch")
              (profile_image . "https://www.gravatar.com/avatar/8b728c7d6c1dce81f5a82249b7617756?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 2223)
              (reputation . 425))
             (owner
              (link . "http://emacs.stackexchange.com/users/2223/jch")
              (display_name . "jch")
              (profile_image . "https://www.gravatar.com/avatar/8b728c7d6c1dce81f5a82249b7617756?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 2223)
              (reputation . 425)))
            ((body . "<p>Org mode does not provide any syncing functionality itself. You would have to rely on a separate application like Dropbox to sync your files. Dropbox syncs not just often, but immediately when files change, so unless you change a file and immediately shut off your computer, you will keep your files in sync.</p>\n")
             (title . "Keeping my .org files in sync across multiple computers")
             (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers/2874#2874")
             (body_markdown . "Org mode does not provide any syncing functionality itself. You would have to rely on a separate application like Dropbox to sync your files. Dropbox syncs not just often, but immediately when files change, so unless you change a file and immediately shut off your computer, you will keep your files in sync.")
             (question_id . 2871)
             (answer_id . 2874)
             (creation_date . 1414706455.0)
             (last_activity_date . 1414706455.0)
             (score . 2)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/35/ryan")
              (display_name . "Ryan")
              (profile_image . "https://www.gravatar.com/avatar/18b80f3021a241d9b85c236dafbcd64b?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 35)
              (reputation . 255)))
            ((body . "<p>Dropbox (or any other cloud storage I guess) is the way to go. In addition, this is really handy to synchronize the Emacs setup in <code>~/.emacs.d</code> (use <code>mklink</code> on Windows).</p>\n")
             (title . "Keeping my .org files in sync across multiple computers")
             (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers/2876#2876")
             (body_markdown . "Dropbox (or any other cloud storage I guess) is the way to go. In addition, this is really handy to synchronize the Emacs setup in `~/.emacs.d` (use `mklink` on Windows).")
             (question_id . 2871)
             (answer_id . 2876)
             (creation_date . 1414707814.0)
             (last_activity_date . 1414707814.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/2317/alain")
              (display_name . "Alain")
              (profile_image . "https://www.gravatar.com/avatar/6b2434a658adfce8c1fea450d6f48773?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 2317)
              (reputation . 51)))
            ((body . "<p>I personally have this problem and have been using copy instead of Dropbox to have my files in sync. (I don't use Dropbox because of privacy issues). Just like Dropbox, Copy offers clients for all the major platforms.</p>\n\n<p>Sometimes I forget to save my org buffers, so I have hacked the following functions to save all org files whenever I save some file. Not a great solution, but it serves my purpose.</p>\n\n<pre><code>(add-hook 'after-save-hook 'org-save-all-org-buffers)\n</code></pre>\n\n<p>Also, if you hold sensitive data in your org files, I recommend you to take a look at <a href=\"http://orgmode.org/worg/org-tutorials/encrypting-files.html\" rel=\"nofollow\">this page</a>. I use org-crypt which is amazing.</p>\n")
             (title . "Keeping my .org files in sync across multiple computers")
             (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers/2882#2882")
             (body_markdown . "I personally have this problem and have been using [copy](www.copy.com) instead of Dropbox to have my files in sync. (I don&#39;t use Dropbox because of privacy issues). Just like Dropbox, Copy offers clients for all the major platforms.\n\nSometimes I forget to save my org buffers, so I have hacked the following functions to save all org files whenever I save some file. Not a great solution, but it serves my purpose.\n\n    (add-hook &#39;after-save-hook &#39;org-save-all-org-buffers)\n\nAlso, if you hold sensitive data in your org files, I recommend you to take a look at [this page](http://orgmode.org/worg/org-tutorials/encrypting-files.html). I use org-crypt which is amazing.")
             (question_id . 2871)
             (answer_id . 2882)
             (creation_date . 1414715968.0)
             (last_activity_date . 1414715968.0)
             (score . 2)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/290/renan-ranelli")
              (display_name . "Renan Ranelli")
              (profile_image . "https://www.gravatar.com/avatar/4569aec00cb223b3fbf484f9e7ba1256?s=128&d=identicon&r=PG")
              (accept_rate . 100)
              (user_type . "registered")
              (user_id . 290)
              (reputation . 360)))
            ((body . "<p>I use git-annex assistant to sync my org files across my computers and even my phone (a jolla so the linux binary works out of the box but there is also an android version).</p>\n\n<p>It works like dropbox in that whenever you save a file it will try to sync it but it is decentralized and free (as in free speech and free beer).</p>\n\n<p>The setup is straightforward if you know git but you need some time to familiarize yourself with all the options.</p>\n\n<p>You can find more at <a href=\"http://git-annex.branchable.com\" rel=\"nofollow\">http://git-annex.branchable.com</a></p>\n")
             (title . "Keeping my .org files in sync across multiple computers")
             (link . "http://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers/2886#2886")
             (body_markdown . "I use git-annex assistant to sync my org files across my computers and even my phone (a jolla so the linux binary works out of the box but there is also an android version).\n\nIt works like dropbox in that whenever you save a file it will try to sync it but it is decentralized and free (as in free speech and free beer).\n\nThe setup is straightforward if you know git but you need some time to familiarize yourself with all the options.\n\nYou can find more at http://git-annex.branchable.com")
             (question_id . 2871)
             (answer_id . 2886)
             (creation_date . 1414723518.0)
             (last_activity_date . 1414723518.0)
             (score . 4)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/2344/razcampagne")
              (display_name . "razcampagne")
              (profile_image . "https://www.gravatar.com/avatar/a17d0aed8c29ac52d5693ff864adba74?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 2344)
              (reputation . 41)))]))
 ((body . "<p>I'm writting a package which dowloads a list of healines, contents,\nand some other properties, which are to be displayed to the user. For\nthe moment, an <code>org-mode</code> buffer seems like a good way of displaying\nthese headlines.</p>\n\n<p>Below is an example of how this list might be structured. It is merely\nillustrative, I can easily convert it to any other structure as\nnecessary.</p>\n\n<pre><code>'((\"One headline\" \"Some much longer content.\"\n   (property1 . value)\n   (property2 . value))\n  (\"Second headline\" \"More much longer content.\"\n   (property1 . value)\n   (property2 . value)))\n</code></pre>\n\n<p><strong>Is there a function or package which prints such a list into an <code>org-mode</code> buffer?</strong></p>\n\n<p>Here's the desired output.</p>\n\n<pre><code>* One headline\n  :PROPERTIES:\n  :property1: value\n  :property2: value\n  :END:\nSome much longer content.\n\n* Second headline\n  :PROPERTIES:\n  :property1: value\n  :property2: value\n  :END:\nMore much longer content.\n</code></pre>\n\n<p>I could do this manually, I'm just wondering if there might be\nsomething out there.</p>\n")
  (title . "Turn a list or data structure into an org document")
  (link . "http://emacs.stackexchange.com/questions/2869/turn-a-list-or-data-structure-into-an-org-document")
  (body_markdown . "I&#39;m writting a package which dowloads a list of healines, contents,\nand some other properties, which are to be displayed to the user. For\nthe moment, an `org-mode` buffer seems like a good way of displaying\nthese headlines.\n\nBelow is an example of how this list might be structured. It is merely\nillustrative, I can easily convert it to any other structure as\nnecessary.\n\n    &#39;((&quot;One headline&quot; &quot;Some much longer content.&quot;\n       (property1 . value)\n       (property2 . value))\n      (&quot;Second headline&quot; &quot;More much longer content.&quot;\n       (property1 . value)\n       (property2 . value)))\n\n**Is there a function or package which prints such a list into an `org-mode` buffer?**\n\nHere&#39;s the desired output.\n\n    * One headline\n      :PROPERTIES:\n      :property1: value\n      :property2: value\n      :END:\n    Some much longer content.\n    \n    * Second headline\n      :PROPERTIES:\n      :property1: value\n      :property2: value\n      :END:\n    More much longer content.\n\nI could do this manually, I&#39;m just wondering if there might be\nsomething out there.\n")
  (question_id . 2869)
  (creation_date . 1414703482.0)
  (last_activity_date . 1414722781.0)
  (score . 3)
  (answer_count . 2)
  (accepted_answer_id . 2885)
  (favorite_count . 0)
  (view_count . 50)
  (is_answered . t)
  (close_vote_count . 0)
  (reopen_vote_count . 0)
  (delete_vote_count . 0)
  (comment_count . 1)
  (comments .
            [((body . "Oh my, this would be very useful. +1.  I would note that even Org does so manually.  See <code>org-insert-drawer</code>.  (That is, I would imagine if such a converter existed, this function would call that one with <code>nil</code>.)")
              (link . "http://emacs.stackexchange.com/questions/2869/turn-a-list-or-data-structure-into-an-org-document#comment3926_2869")
              (body_markdown . "Oh my, this would be very useful. +1.  I would note that even Org does so manually.  See `org-insert-drawer`.  (That is, I would imagine if such a converter existed, this function would call that one with `nil`.)")
              (comment_id . 3926)
              (post_id . 2869)
              (creation_date . 1414706659.0)
              (post_type . "question")
              (score . 0)
              (edited . :json-false)
              (owner
               (link . "http://emacs.stackexchange.com/users/2264/sean-allred")
               (display_name . "Sean Allred")
               (profile_image . "https://www.gravatar.com/avatar/9261936847b5a31e15da6e86533d3de3?s=128&d=identicon&r=PG")
               (user_type . "registered")
               (user_id . 2264)
               (reputation . 512)))])
  (owner
   (link . "http://emacs.stackexchange.com/users/50/malabarba")
   (display_name . "Malabarba")
   (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
   (accept_rate . 89)
   (user_type . "registered")
   (user_id . 50)
   (reputation . 3999))
  (tags .
        ["org-mode"])
  (answers .
           [((body . "<p>I've sort of tangentially looked at this issue. Take a look at the <a href=\"http://orgmode.org/worg/org-contrib/org-protocol.html\" rel=\"nofollow\">org-protocol.el</a>. It's bundled with org-mode. Specifically, the org-protocol-do-capture function converts a list, \"parts\" (which you seem to already have), to org-mode properties using org-store-link-props function and then calls org-capture. This assumes that you have a capture template with placeholders such as %:link. You can define the properties to be whatever you like.</p>\n\n<p>I've done <a href=\"https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-protocol-templates.el\" rel=\"nofollow\">something similar</a> to scrape title, author, date, source, etc. from site APIs. If you end up looking at this code, be sure to also look at capture-templates.el.</p>\n\n<p>If you're working with JSON data, the json.el and / or request.el might be useful.</p>\n")
             (title . "Turn a list or data structure into an org document")
             (link . "http://emacs.stackexchange.com/questions/2869/turn-a-list-or-data-structure-into-an-org-document/2877#2877")
             (body_markdown . "I&#39;ve sort of tangentially looked at this issue. Take a look at the [org-protocol.el][1]. It&#39;s bundled with org-mode. Specifically, the org-protocol-do-capture function converts a list, &quot;parts&quot; (which you seem to already have), to org-mode properties using org-store-link-props function and then calls org-capture. This assumes that you have a capture template with placeholders such as %:link. You can define the properties to be whatever you like.\n\nI&#39;ve done [something similar][2] to scrape title, author, date, source, etc. from site APIs. If you end up looking at this code, be sure to also look at capture-templates.el.\n\n  [1]: http://orgmode.org/worg/org-contrib/org-protocol.html\n  [2]: https://github.com/sk8ingdom/.emacs.d/blob/master/org-mode-config/org-protocol-templates.el\n\nIf you&#39;re working with JSON data, the json.el and / or request.el might be useful.")
             (question_id . 2869)
             (answer_id . 2877)
             (creation_date . 1414710004.0)
             (last_activity_date . 1414710004.0)
             (score . 0)
             (is_accepted . :json-false)
             (comment_count . 0)
             (owner
              (link . "http://emacs.stackexchange.com/users/804/sk8ingdom")
              (display_name . "sk8ingdom")
              (profile_image . "https://www.gravatar.com/avatar/ca8e672ae4fd9972d188f31ca2c41775?s=128&d=identicon&r=PG")
              (user_type . "registered")
              (user_id . 804)
              (reputation . 1)))
            ((body . "<p>This is the job of <a href=\"http://orgmode.org/worg/dev/org-element-api.html\" rel=\"nofollow\"><code>org-element</code></a>, the <em>awesome</em>(!) work of <strong>Nicolas Goaziou</strong>.  If you don't know <code>org-element</code> and you care about org-development this is something you should look into.  It is not only a great tool to work with, it is also increasingly powering <code>org</code>.  Most notably the <code>org-export</code> (<a href=\"http://orgmode.org/org.html#Exporting\" rel=\"nofollow\"><code>ox</code></a>), but also functions in e.g. <code>org.el</code>.</p>\n\n<p>To get the \"lisp representation\" of an element under point use <code>org-element-at-point</code> or <code>org-element-context</code>.  To get the representation of the buffer use <code>org-element-parse-buffer</code>.  While not directly relevant here, be aware of <code>org-element-map</code>.</p>\n\n<p>To go from the \"lisp representation\" of an <code>element</code>, <code>secondary string</code> or <code>parse tree</code> back to the <a href=\"http://orgmode.org/worg/dev/org-syntax.html\" rel=\"nofollow\">\"Org syntax representation\"</a> use <code>org-element-interpret-data</code>.  This is <em>the</em> (only) way to turn a \"lisp representation\" into a \"Org syntax representation\".  You will probably not want to write this representation manually, though.  Here's is a pretty small representation of your first headline</p>\n\n<pre><code>(org-element-interpret-data\n '(headline (:title \"One headline\" :level 1)\n            (property-drawer nil ((node-property (:key \"property1\" :value \"value1\"))\n                                  (node-property (:key \"property2\" :value \"value2\"))))\n            (#(\"Some much longer content.\"))))\n</code></pre>\n\n<p>If you must add both headlines add a <code>parse tree</code></p>\n\n<pre><code>(org-element-interpret-data\n '(org-data nil (headline (:title \"One headline\" :level 1)\n                          (property-drawer nil ((node-property (:key \"property1\" :value \"value1\"))\n                                                (node-property (:key \"property2\" :value \"value2\"))))\n                          (#(\"Some much longer content.\")))\n            (headline (:title \"Second headline\" :level 1)\n                          (property-drawer nil ((node-property (:key \"property1\" :value \"value1\"))\n                                                (node-property (:key \"property2\" :value \"value2\"))))\n                          (#(\"More much longer content.\")))))\n</code></pre>\n\n<p>You may find that <strong>Thorsten Jolitz</strong>'s <a href=\"https://github.com/tj64/org-dp\" rel=\"nofollow\"><code>org-dp</code></a> library will aid you such efforts.  My understanding (I haven't used <code>org-dp</code>) is that <code>org-dp</code> is a meant to make it easer for to use \"lisp representation\" to create new \"Org syntax representation\" (as this it not a goal of <code>org-element</code> per se).</p>\n\n<p>For further clarification <a href=\"http://dir.gmane.org/gmane.emacs.orgmode\" rel=\"nofollow\"><code>gmane.emacs.orgmode</code></a> is really the appropriate forum.</p>\n")
             (title . "Turn a list or data structure into an org document")
             (link . "http://emacs.stackexchange.com/questions/2869/turn-a-list-or-data-structure-into-an-org-document/2885#2885")
             (body_markdown . "This is the job of [`org-element`][1], the *awesome*(!) work of **Nicolas Goaziou**.  If you don&#39;t know `org-element` and you care about org-development this is something you should look into.  It is not only a great tool to work with, it is also increasingly powering `org`.  Most notably the `org-export` ([`ox`][2]), but also functions in e.g. `org.el`.\n\nTo get the &quot;lisp representation&quot; of an element under point use `org-element-at-point` or `org-element-context`.  To get the representation of the buffer use `org-element-parse-buffer`.  While not directly relevant here, be aware of `org-element-map`.\n\nTo go from the &quot;lisp representation&quot; of an `element`, `secondary string` or `parse tree` back to the [&quot;Org syntax representation&quot;][3] use `org-element-interpret-data`.  This is *the* (only) way to turn a &quot;lisp representation&quot; into a &quot;Org syntax representation&quot;.  You will probably not want to write this representation manually, though.  Here&#39;s is a pretty small representation of your first headline\n\n    (org-element-interpret-data\n     &#39;(headline (:title &quot;One headline&quot; :level 1)\n                (property-drawer nil ((node-property (:key &quot;property1&quot; :value &quot;value1&quot;))\n                                      (node-property (:key &quot;property2&quot; :value &quot;value2&quot;))))\n                (#(&quot;Some much longer content.&quot;))))\n\nIf you must add both headlines add a `parse tree`\n\n    (org-element-interpret-data\n     &#39;(org-data nil (headline (:title &quot;One headline&quot; :level 1)\n                              (property-drawer nil ((node-property (:key &quot;property1&quot; :value &quot;value1&quot;))\n                                                    (node-property (:key &quot;property2&quot; :value &quot;value2&quot;))))\n                              (#(&quot;Some much longer content.&quot;)))\n                (headline (:title &quot;Second headline&quot; :level 1)\n                              (property-drawer nil ((node-property (:key &quot;property1&quot; :value &quot;value1&quot;))\n                                                    (node-property (:key &quot;property2&quot; :value &quot;value2&quot;))))\n                              (#(&quot;More much longer content.&quot;)))))\n\n\nYou may find that **Thorsten Jolitz**&#39;s [`org-dp`](https://github.com/tj64/org-dp) library will aid you such efforts.  My understanding (I haven&#39;t used `org-dp`) is that `org-dp` is a meant to make it easer for to use &quot;lisp representation&quot; to create new &quot;Org syntax representation&quot; (as this it not a goal of `org-element` per se).\n\nFor further clarification [`gmane.emacs.orgmode`][4] is really the appropriate forum.\n\n\n  [1]: http://orgmode.org/worg/dev/org-element-api.html\n  [2]: http://orgmode.org/org.html#Exporting\n  [3]: http://orgmode.org/worg/dev/org-syntax.html\n  [4]: http://dir.gmane.org/gmane.emacs.orgmode")
             (question_id . 2869)
             (answer_id . 2885)
             (creation_date . 1414721912.0)
             (last_edit_date . 1414722781.0)
             (last_activity_date . 1414722781.0)
             (score . 4)
             (is_accepted . t)
             (comment_count . 1)
             (last_editor
              (link . "http://emacs.stackexchange.com/users/1974/rasmus")
              (display_name . "rasmus")
              (profile_image . "https://www.gravatar.com/avatar/bd580f3842d43ba9dd42aff4914a38d3?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 1974)
              (reputation . 486))
             (comments .
                       [((body . "Perfect! Thanks a lot.")
                         (link . "http://emacs.stackexchange.com/questions/2869/turn-a-list-or-data-structure-into-an-org-document/2885#comment4006_2885")
                         (body_markdown . "Perfect! Thanks a lot.")
                         (comment_id . 4006)
                         (post_id . 2885)
                         (creation_date . 1414779800.0)
                         (post_type . "answer")
                         (score . 0)
                         (edited . :json-false)
                         (owner
                          (link . "http://emacs.stackexchange.com/users/50/malabarba")
                          (display_name . "Malabarba")
                          (profile_image . "https://www.gravatar.com/avatar/c14505947446e90f2a14fd8f3dd3cf94?s=128&d=identicon&r=PG")
                          (accept_rate . 89)
                          (user_type . "registered")
                          (user_id . 50)
                          (reputation . 3999)))])
             (owner
              (link . "http://emacs.stackexchange.com/users/1974/rasmus")
              (display_name . "rasmus")
              (profile_image . "https://www.gravatar.com/avatar/bd580f3842d43ba9dd42aff4914a38d3?s=128&d=identicon&r=PG&f=1")
              (user_type . "registered")
              (user_id . 1974)
              (reputation . 486)))]))]

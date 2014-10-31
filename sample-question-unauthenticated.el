
(defvar sample-question-unauthenticated
;;; A sample of data offered by the question object, without authentication.
  '((body . "<p>I am getting \"rgb(18, 115, 224)\" from a dom element. Now I want to assign the color(whatever i am getting from this element) to a span element. So I need hexadecimal equivalent of the color I am getting. For this I can use </p>

<pre><code>\"#\" + componentToHex(r) + componentToHex(g) + componentToHex(b)
</code></pre>

<p>but, My question here's how can I get the r, g,b component values from \"rgb(18, 115, 224)\"</p>
")
    (title . "get r, g, b component of a color in rgb() format")
    (link . "http://stackoverflow.com/questions/26679366/get-r-g-b-component-of-a-color-in-rgb-format")
    (body_markdown . "I am getting &quot;rgb(18, 115, 224)&quot; from a dom element. Now I want to assign the color(whatever i am getting from this element) to a span element. So I need hexadecimal equivalent of the color I am getting. For this I can use 
    
    &quot;#&quot; + componentToHex(r) + componentToHex(g) + componentToHex(b)
but, My question here&#39;s how can I get the r, g,b component values from &quot;rgb(18, 115, 224)&quot;")
    (question_id . 26679366)
    (creation_date . 1414773223.0)
    (last_activity_date . 1414773433.0)
    (score . 0)
    (answer_count . 2)
    (view_count . 16)
    (is_answered . :json-false)
    (comment_count . 1)
    (comments . [((comment_id . 41957708) (post_id . 26679366) (creation_date . 1414773413.0) (score . 0) (edited . :json-false) (owner (link . "http://stackoverflow.com/users/14104/epascarello") (display_name . "epascarello") (profile_image . "https://www.gravatar.com/avatar/760b4da77fd54d37c104029ff1f56749?s=128&d=identicon&r=PG") (user_type . "registered") (user_id . 14104) (reputation . 76145)))])
    (owner (link . "http://stackoverflow.com/users/3867678/rasmita-dash") (display_name . "Rasmita Dash") (profile_image . "http://graph.facebook.com/100001985977413/picture?type=large") (accept_rate . 45) (user_type . "registered") (user_id . 3867678) (reputation . 90))
    (tags . ["javascript" "jquery" "html" "css" "colors"])
    (answers . [((body . "<blockquote>
  <p>Now I want to assign the color(whatever i am getting from this element) to a span element.</p>
</blockquote>

<p>No you don't, you can just use <code>rgb(18, 115, 224)</code> directly for a color value in CSS.</p>
") (link . "http://stackoverflow.com/questions/26679366/get-r-g-b-component-of-a-color-in-rgb-format/26679414#26679414") (body_markdown . "&gt; Now I want to assign the color(whatever i am getting from this element) to a span element.

No you don&#39;t, you can just use `rgb(18, 115, 224)` directly for a color value in CSS.") (question_id . 26679366) (answer_id . 26679414) (creation_date . 1414773411.0) (last_activity_date . 1414773411.0) (score . 0) (is_accepted . :json-false) (comment_count . 0) (owner (link . "http://stackoverflow.com/users/157247/t-j-crowder") (display_name . "T.J. Crowder") (profile_image . "https://www.gravatar.com/avatar/ca3e484c121268e4c8302616b2395eb9?s=128&d=identicon&r=PG") (accept_rate . 90) (user_type . "registered") (user_id . 157247) (reputation . 307244))) ((body . "<p>You don't need to convert it to anything.  If you want to assign this value to a span colour, then simply do:</p>

<pre><code>var clr = rgb(18, 115, 224);
$('#myspan').css('color', clr);
</code></pre>
") (link . "http://stackoverflow.com/questions/26679366/get-r-g-b-component-of-a-color-in-rgb-format/26679423#26679423") (body_markdown . "You don&#39;t need to convert it to anything.  If you want to assign this value to a span colour, then simply do:

    var clr = rgb(18, 115, 224);
    $(&#39;#myspan&#39;).css(&#39;color&#39;, clr);") (question_id . 26679366) (answer_id . 26679423) (creation_date . 1414773433.0) (last_activity_date . 1414773433.0) (score . 0) (is_accepted . :json-false) (comment_count . 0) (owner (link . "http://stackoverflow.com/users/717214/aleks-g") (display_name . "Aleks G") (profile_image . "https://www.gravatar.com/avatar/9075315c5f4b734a8f2567be54a4cc45?s=128&d=identicon&r=PG") (accept_rate . 92) (user_type . "registered") (user_id . 717214) (reputation . 26812)))])))

(provide 'sample-question-unauthenticated)

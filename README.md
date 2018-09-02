Shout!
======

A notifications gateway for helping your little robot friends to
be heard.

How Do I Run It?
-----------------

You need `sbcl`, Steel Bank Common Lisp.  Once you have that:

    sbcl --script run.lisp

No Seriously, How Do I Run It, _In Production_?
-----------------------------------------------

Production?  Well why didn't you say so?  I recommend our
[BOSH release][bosh]; it's very popular this time of year.

How Do I Use It?
----------------

Don't get mad.  We use curl.

First, upload the rules:

    curl -X POST http://username:password@localhost:7109/rules \
      --data-binary @path/to/rules-file

After that, you can create events:

    curl -X POST http://username:password@localhost:7109/events --data-binary '{
      "topic"       : "some-pipeline",
      "ok"          : true,
      "message"     : "Pipeline build #367 succeeded",
      "link"        : "https://ci/p/some-pipeline/367"
    }'

Shout! keeps track of the `ok`-ness of each topic you create.
Whenever transitions occur, either a failure (ok -> not ok) or a recovery (the
opposite), a notification is sent.

By default, the `username:password` pair is `shout:shout`, but this can be
overridden by setting the `SHOUT_OPS_AUTH` environment variable for the
operations endpoints (/events, /state, and /states) and `SHOUT_ADMIN_AUTH` for
admin endpoint (/rules) before starting the Shout! process.

No Seriously, How Do I Use It, _For Real_?
------------------------------------------

You sure do like production.

For real-world use, you probably just want to hook this up to your
pipelines via our [Concourse resource][concourse].  It's the
chef's specialty.

It looks a little something like this:

    resource_types:
      - name: shout
        type: docker-image
        source:
          repository: huntprod/shout-resource
          tag:        latest

    resources:
      - name: shout-out
        type: shout
        source:
          url: http://your-shout-ip:7109

and then, whenever you need to notify (i.e. `on_failure`):

    on_failure:
      put: shout-out
      params:
        topic:   $BUILD_PIPELINE_NAME
        message: Pipeline failed.
        link:    https://ci/teams/$BUILD_TEAM_NAME/pipelines/$BUILD_PIPELINE_NAME

And you're off to the races!  Err... pipelines.

Configuring Notification Rules
------------------------------

Shout! isn't terribly useful without some notification rules.  The
`api:run` function takes a keyword argument called `:rules` that
lets you specify the path to a file containing these rules:

    (api:run :rules #p"/path/to/rules")

(That `#p` bit before the string is just Common Lisp's way of
 saying _what follows is a filesystem path_, for handling diverse
 operating systems like Windows and UNIX)

The contents of the rules file is a bunch of Lisp forms that
implement a script in a small domain-specific language.  It looks
like this:

    ; comments start with ';' and continue
    ; until the end of the line.

    ; (blank lines like ^^ are ignored)

    ((set a-variable "some value")
     (set webhook "https://slack.web.hook/...")
     ; SET creates a new variable and sets its value.
     ; you can refer to these values later, in handlers.

     (for "a-topic"    ; the following rules only apply to the
                       ; topic 'a-topic'

       (when ((on weekdays)
              (from 0800 am to 0500 pm))

         ; the rest of this WHEN clause identifies
         ; how to notify, if the FOR and WHEN parameters
         ; are met.

         ;; send a slack notification
         (slack :webhook "$webhook"
                :text    "$topic is $status: $message"
                :color   "#127abd")))

     (for *         ; matches everything
       (when *      ; matches any time

         ;; a fancier slack message
         (slack :webhook "$webhook"
                :text    "$topic is $status"
                :color   (if ok? "good" "danger")
                :attach  (if link "$message <$link>"
                                  "$message")))))

Shout! stops looking for FOR matches once it finds one.
Likewise, it stops evaluating WHEN rules once a match is found.


### Rules Language Reference

#### String Interpolation

Shout! interpolates two embedded forms of variable references in
strings, under certain conditions.

    "$link"

This is called a _builtin variable_, and there are fixed number of
these.  For break/fix notifications, `$message`, `$topic`,
`$status`, and `$link`.

    "$[meta-data]"

This references metadata supplied to an event or announcement by
the end user.

#### Top-Level Forms

- <tt>(for <em>topic-condition</em> <em>when-clause...</em>)</tt>

  Creates a _topic-match_ handler, which will fire if and when
  <tt><em>topic-condition</em></tt> is met.  If the condition is a
  quoted string, the handler matches that topic and that topic only.

  The condition can be a compound expression, using logic operators
  like `(or ...)`, `(and ...)` and `(not ...)`.

- <tt>(set <em>variable-name</em> <em>value</em>)</tt>

  Defines a variable, named <tt><em>variable-name</em></tt>, and
  assigns it the given value.  <tt><em>value</em></tt> can be a
  complex expression, or a _map_ value.

#### FOR Topic Conditions

- `"a literal string"` (top-level only)

  Shorthand for `(is "a literal string")`.  This cannot be combined
  with any other topic condition form.

- `*` (top-level only)

  Matches everything, implicitly.  This cannot be combined with any
  other topic condition form.

- <tt><em>expr</em></tt>

  A _general expression_, which is evaluated.  If the expression returns
  true, this FOR handler will fire, and no other handlers will be
  considered.


#### WHEN Clauses

A FOR handler can have any number of WHEN clauses, which associate a
time-of-day assertion with one or more calls to notification methods,
like Slack or email.

The first argument to a `(when ...)` form is a list of _general expressions_,
implicitly wrapped in an `(and ...)`.

    (for "stuff"
      (when ((on weekdays)
             (from 0800 am to 0500 pm))
        (slack ...)))

More complicated guards are possible:

    (for "stuff"
      (when ((and (or (after 0500 pm)
                      (before 1000 am))
                  (or (on monday)
                      (on wednesday)
                      (on friday))))
        (slack ...)))

#### General Expressions

Inside of the argument list of a notification form, like `(slack ...)`,
you can use _expressions_ to do more than just settle for hard-coded
values.

The following expressions are defined:

- <tt>(if <em>test</em> <em>then-form</em> <em>else-form</em>)</tt>

  Evaluates <tt><em>test</em></tt>; if it evaluates to true, the
  <tt><em>then-form</em></tt> is evaluated; otherwise the
  <tt><em>else-form</em></tt> is evaluated (if present).  The value
  of the `(if ...)` expression is the value of the evaluated sub-form.

  For example:

        (if ok? "good" "bad")

  Will either be `"good"` (if `ok?` is true) or `"bad"`.

- <tt>(not <em>expr</em>)</tt>

  Evaluates the given <tt><em>expr</em></tt> and then negates its value.
  True becomes false; false becomes true.

- <tt>(and <em>expr...</em>)</tt>

  Evaluate each <tt><em>expr</em></tt> in turn, and return true if **all**
  expressions evaluate to true.  Evaluation stops on the first non-true
  expression, and false is returned.

- <tt>(or <em>expr ...</em>)</tt>

  Evaluates each <tt><em>expr</em></tt> in turn, and return true if **any**
  expression evaluates to true.  Evaluation stops on the first true
  expression.  If none of the expressions evaluate to true, false is
  returned.

- <tt>(value <em>variable-name</em>)</tt>

  Evaluates to the value of the variable <tt><em>variable-name</em></tt>.

  An error is signaled if the variable is not `set`.

- <tt>(lookup <em>map-var-name</em> <em>key...</em>)</tt>

  Evaluates each `key` (there can be multiple), in order, and attempts
  to look up the corresponding value in the map variable
  <tt><em>map-var-name</em></tt>.

  The first found value will be returned.

  Errors are signaled if <tt><em>map-var-name</em></tt> isn't a map
  variable, or if no value could be found for any of the given keys.

- <tt>(metadata? <em>var-name</em>)</tt>

  Returns true if the event in question came with the metadata
  variable <tt><em>var-name</em></tt> set.

- <tt>(metadata <em>var-name</em>)</tt>

  Returns the value of the metadata variable
  <tt><em>var-name</em></tt>, or the empty string, if not set.

- <tt>(is <em>quoted-literal</em>)</tt>

  A literal and explicit match against the current topic will be made.
  If the match fails, returns false.

- <tt>(~ <em>quoted-glob</em>)</tt>

  A _UNIX glob_ pattern match will be made against the current topic.
  If the match fails, returns false.

  The glob language is as follows:

    - `?` - Matches any single character.
    - `\*` - Matches zero or more characters.
    - `[abc] - Matches one instance of the characters
      `a`, `b`, or `c`.
    - `[a-z]` - Matches one instance of the characters
      that are greater than or equal to `a`, and less
      than or equal to `z` (per ASCII / Unicode sets).

  Globs are implicitly anchored.  To unanchor them, start
  or end your pattern with a `\*`.

- <tt>(re <em>quoted-regex</em>)</tt>

  A _regular expressions_ match will be made against the current topic.
  If the match fails, return false.

  For the semantics of the language, refer to any reference on
  _Perl-Compatible Regular Expressions_.

- <tt>(on <em>day-spec...</em>)</tt>

  Matches a given subset of the days of the week.  You can list one or
  more days by name, `sunday`, `monday`, `tuesday`, `wednesday`, `thursday`,
  `friday`, or `saturday`.  You can also use `weekdays` and `weekends` as
  shorthand for monday - friday, and saturday + sunday, respectively.

- <tt>(from <em>hour</em> <em>am/pm</em> to <em>hour</em> <em>am/pm</em>)</tt>

  Matches a given time-of-day, using the current timezone (as determined by
  the value of the `$TZ` environment variable).  The <tt><em>hour</em></tt>
  values must be specified in `HHMM` format, i.e. `0830` for 8:30.

- <tt>(after <em>hour</em> <em>am/pm</em>)

  Equivalent to a `(from ...)` expression, if the `to` clause were ignored.

- <tt>(before <em>hour</em> <em>am/pm</em>)

  Equivalent to a `(from ...)` expression, if the `from` clause were ignored.

#### String Interpolation

### Rules Cookbook

Here's a bunch of examples that aim to be both instructive in terms
of theory, and usable via copy-and-paste-and-tweak.

#### Using Maps To Cascade Values

The _map_ variable type creates an association map of names to values,
which can be used by a `(lookup ...)` form to get a final value.

    (set webhooks (map "shield"  "https://slack.web.hook/shield..."
                       "safe"    "https://slack.web.hook/safe..."
                       "default" "https://slack.web.hook/default...")

Later, during a notification handler body, we can refer to the
`webhooks` map and lookup values in it:

    ; yields the SHIELD webhook
    (lookup webhooks "shield")

    ; yields the DEFAULT webhook, since "genesis"
    ; is not in the map
    (lookup webhooks "genesis" "default")

    ; also yields the SHIELD webhook
    (lookup webhooks "shield" "default")

    ; causes an error
    (lookup webhooks "not-defined")


#### Handling Multiple Topics Identically

You will normally have more topics than you have different avenues
of notification, so the default case of one FOR per topic can become
tiresome quickly.

You can use an `(or ...)` form to match multiple topics explicitly:

    (for (or (is "topic-1")
             (is "topic-2"))
      (when ...))

You can also use a `(not ...)` form to match everything _except_ a
specific topic:

    (for (not (is "that-other-topic"))
      (when ...))

The `(not ...)` form makes more sense when coupled with an `(and ...)`
operator to handle everything but a _subset_ of topic:

    (for (and (not (is "this-topic"))
              (not (is "that-other-topic")))
      (when ...))

Normal boolean-logic laws apply, so AND-ing a bunch of NOTs together
is the same as NOT-ing a bunch of ORs:

    (for (not (or (is "this-topic"))
              (or (is "that-other-topic")))
      (when ...))

#### Better Matching Through Regular Expressions

The Shout! FOR evaluator support regular expressions via the
`(~ ...)` and `(re ...)` forms:

    (for (~ "*-pipeline"))
      (when ...))

... or if you prefer Perl-compatible Regular Expression syntax:

    (for (re ".*-pipeline$")
      (when ...))

These can be combined with `(and ...)`, `(or ...)`, and `(not ...)`:

    (for (or (~ "shield*")
             (~ "safe*")
             (~ "genesis*"))
      (when ...))

#### Provide a "Fallback" Handler

If most of your notification needs are uniform, with only a few
exceptions, you can use a _fallback handler_, defined by `(for * ...)`:

    (for "special-topic"
      (when ...))
    (for "other-special-topic"
      (when ...))

    (for *
      (when ...))

The `(for * ...)` will handle anything that falls through the other
FOR definitions.  Shout! stops looking for FOR matches once it finds
one.

#### On-Call Notifications

Often, you will want to send notifications via one method, during
business hours, and a different way the rest of the time.  Here's an
example that uses slack from 9am - 5pm on weekdays, and otherwise
resorts to email.

    (for *
      (when ((on weekdays)
             (from 0900 am to 0500 pm))
         (slack ...))
      (when *
         (email ...)))

Note that the `(when * ...)` clause doesn't trigger during business
hours because Shout! stops evaluating WHEN clauses after the first
match.

How Do I Contribute?
--------------------

  1. Fork this repo
  2. Create your feature branch (`git checkout -b my-new-feature`)
  3. Commit your changes (`git commit -am 'Added some feature'`)
  4. Push to the branch (`git push origin my-new-feature`)
  5. Create a new Pull Request in Github
  6. Profit!


[bosh]:      https://github.com/jhunt/shout-boshrelease
[concourse]: https://github.com/jhunt/shout-resource

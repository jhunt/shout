Shout!
======

A notifications gateway for helping your little robot friends to
be heard.

How Do I Run It?
-----------------

You need `sbcl`, Steel Bank Common Lisp.  Once you have that:

    sbcl --script run.lisp

If you want to hack around:

    sbcl --load shout.lisp --eval '(api:run)'

to get a Lisp REPL (read/eval/print loop).

No Seriously, How Do I Run It, _In Production_?
-----------------------------------------------

Production?  Well why didn't you say so?  I recommend our
[BOSH release][bosh]; it's very popular this time of year.

How Do I Use It?
----------------

Don't get mad.  We use curl.

    curl -X POST http://localhost:7109 --data-binary '{
      "topic"       : "some-pipeline",
      "ok"          : true,
      "message"     : "Pipeline build #367 succeeded",
      "link"        : "https://ci/p/some-pipeline/367"
    }'

Shout! keeps track of the `ok`-ness of each topic you create.
Whenever transitions occur, either a failure (ok -> not ok) or a
recovery (the opposite), a notification is sent.

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

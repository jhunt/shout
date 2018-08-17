# The SHOUT! API

## Break/Fix Events

```
curl -X POST https://shout-ip/events -d '{
  "topic"       : "some-pipeline",
  "ok"          : true,
  "message"     : "Pipeline build #367 succeeded",
  "link"        : "https://ci/p/some-pipeline/367",
  "occurred-at" : 2208990000
}'
```

... which returns:

```
{
  "name"  : "some-pipeline",
  "state" : "working",

  "previous" : {
    "occurred-at" : 2208989999,
    "reported-at" : 2208989999,
    "ok"          : false,
    "message"     : "Pipeline build #366 failed",
    "link"        : "https://ci/p/some-pipeline/366"
  },
  "first" : {
    "occurred-at" : 2208990000,
    "reported-at" : 2208990004,
    "ok"          : true,
    "message"     : "Pipeline build #367 succeeded",
    "link"        : "https://ci/p/some-pipeline/367"
  },
  "last" : {
    "occurred-at" : 2208990000,
    "reported-at" : 2208990004,
    "ok"          : true,
    "message"     : "Pipeline build #367 succeeded",
    "link"        : "https://ci/p/some-pipeline/367"
  }
}
```

This will cause SHOUT! to update its internal state database to
reflect the fact that _some-pipeline_ is now ok.  This may be a
continuation of a previous good state, in which case no
notification will go out, or it could be a recovery from a
previously bad state.

If _occurred-at_ is not given in the submitted payload, it will be
set to the current time.  The event's _reported-at_ is always set
server-side.

## Announcements

```
curl -X POST https://shout-ip/announcements -d '{
  "topic"   : "shout-releases",
  "message" : "Shout v1.2.3 (Mumble) Released!",
  "link"    : "https://github.com/jhunt/shout/releases/1.2.3"
}
```

... which returns:

```
{
  "ok" : "Success!"
}
```
